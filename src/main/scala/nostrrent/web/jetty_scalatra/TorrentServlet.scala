package nostrrent.web.jetty_scalatra

import nostrrent.*
import nostrrent.web.*

import org.scalatra.servlet.{ FileUploadSupport }
import org.eclipse.jetty.ee10.servlet.ServletHolder
import org.{ scalatra => http }
import jakarta.servlet.http.HttpServletRequest
import jakarta.servlet.MultipartConfigElement
import java.io.InputStream
import scala.util.{ Success, Failure }
import java.net.{ URL, URI }
import org.eclipse.jetty.http.MimeTypes

class TorrentServlet(
  fileStorage: FileStorage,
  serverPaths: ServerPaths,
  replaceLocalhost: Option[String])
extends BaseServlet(replaceLocalhost)
with FileUploadSupport:

  // configureMultipartHandling(MultipartConfig())
  def init(holder: ServletHolder): Unit =
    holder.getRegistration.setMultipartConfig(MultipartConfigElement(TempDir.getAbsolutePath))

  private def handleUpload(files: Iterator[(String, InputStream)]): http.ActionResult =
    val (torrentID, btHashHex) = fileStorage.saveFiles(files)
    val location = mkSeedLocation(torrentID)
    contentType = "text/plain"
    http.Accepted(btHashHex, Map("Location" -> location))

  post(s"$UploadPath/:filename"):
    request.getContentLength match
      case -1 => http.LengthRequired("Content-Length missing")
      case 0 => http.BadRequest("No content")
      case _ =>
        val file = Iterator.single(params("filename") -> request.getInputStream)
        handleUpload(file)

  final val UploadPath = "/upload"
  post(s"$UploadPath/?"):
    if ! request.contentType.exists(_.startsWith("multipart/form-data")) then
      http.BadRequest(s"Expected multipart/form-data, was: ${request.contentType || "<N/A>"}")
    else
      val files =
        fileMultiParams.iterator
          .flatMap(_._2)
          .map: item =>
            val filename = item.name
            filename -> item.part.getInputStream
      handleUpload(files)

  private def mkSeedLocation(torrentId: TorrentID)(using request: HttpServletRequest): String =
    val reqURL = requestURL()
    val replacePos = reqURL.indexOf(request.getPathInfo)
    val seedPath = SeedPathQ.replace(TorrentIDParm, torrentId.toString)
    reqURL.take(replacePos) + seedPath

  private final val SeedPath = "/seed"
  private final val TorrentIDParm = ":torrentID"
  private final val SeedPathQ = s"$SeedPath/$TorrentIDParm"

  private def makeURL(pathPrefix: String, leafPath: String)(using request: HttpServletRequest): URL =
    val reqURL = request.getRequestURL()
    val removePos = reqURL.indexOf("/", reqURL.indexOf("//") + 2)
    reqURL.replace(removePos, reqURL.length, "")
    reqURL.append(pathPrefix).append('/').append(leafPath)
    URI(reqURL.toString).toURL

  put(s"$SeedPathQ/?"):
    val torrentID = TorrentID(params(TorrentIDParm.drop(1)))
    val requestType = request.contentType.map(MimeTypes.getBase) match
      case Some(contentType) => contentType
      case None =>
        if request.body.startsWith("{") then MimeType.JSON
        else MimeType.FormData

    val seedOptions = requestType match
      case MimeType.JSON => Right:
        SeedOptions.fromJSON(request.body)
      case MimeType.FormData => Right:
        SeedOptions.fromFormData(params.toMap)
      case other => Left:
        http.UnsupportedMediaType(s"Cannot process: $other", Map("Connection" -> "close"))

    seedOptions match
      case Left(failed) => failed
      case Right(Failure(ex)) => throw ex
      case Right(Success(SeedOptions(nostrSig, exposeHttpSeeding))) =>
        val makeWebSeedURL =
          serverPaths.wsPathPrefix
            .filter(_ => exposeHttpSeeding)
            .map: wsPathPrefix =>
              makeURL(wsPathPrefix, _)
        fileStorage.verifyAndSeed(torrentID, nostrSig, makeWebSeedURL) match
          case Failure(tooBig: TorrentTooBig) =>
            http.RequestEntityTooLarge(errMsg(tooBig))
          case Failure(ise: IllegalStateException) =>
            http.Conflict(errMsg(ise))
          case Failure(th: Throwable) => throw th
          case Success(seedInfo) =>
            val location =
              serverPaths.xsPathPrefix
                .map(makeURL(_, s"$torrentID$TorrentFileExt"))
            val magnet = seedInfo.magnet.copy(dn = None)
            val magnetLink =
              location.filter(_ => exposeHttpSeeding)
                .map(xsURL => magnet.copy(xs = xsURL :: Nil))
                .getOrElse(magnet)
                .toString()
            contentType = MimeType.TorrentFile
            val headers =
              location.map(url => Map("Location" -> url.toString)).getOrElse(Map.empty)
              + ("X-Magnet-Link" -> magnetLink)
            http.Created(seedInfo.torrentBytes, headers)

  get("/*"):
    log(s"Invalid GET: ${request.getRequestURL}")
    http.MethodNotAllowed("N/A", Map("Connection" -> "close"))
