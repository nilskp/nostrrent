package nostrrent.web.jetty_scalatra

import java.io.File

/**
  * Torrent content server.
  * @param path server path prefix
  * @param workDir Root torrent dir
  */
class WebSeedServlet(val path: String, workDir: File)
extends AbstractFileServer(workDir)

  // override def doGet(req: HttpServletRequest, res: HttpServletResponse): Unit =
  //   req.getPathInfo match
  //     case null | "" | "/" =>
  //       res.sendError(HttpServletResponse.SC_FORBIDDEN)
  //     case pathInfo =>
  //       val torrentFolder = File(workDir, pathInfo)
  //       val torrentFile = File(workDir, s"${torrentFolder.getName}.torrent")
  //       Try { TorrentID(torrentFolder.getName) } match
  //         case Success(torrentID) if
  //           torrentFolder.isDirectory &&
  //           torrentFile.isFile =>
  //             val json =
  //               fileSystem.listFiles(torrentID)
  //               .map(name => s"\"$name\"")
  //               .mkString("[", ",", "]")
  //             res.setContentType("application/json")
  //             res.setContentLength(json.length)
  //             res.setCharacterEncoding("UTF-8")
  //             res.getWriter.write(json)
  //             res.setStatus(HttpServletResponse.SC_OK)
  //         case _ =>
  //           super.doGet(req, res)
