package nostrrent.web.jetty_scalatra

import jakarta.servlet.http.{ HttpServletRequest, HttpServletRequestWrapper }

class ReplaceServerNameRequest(
  req: HttpServletRequest,
  fromTo: String => Option[String])
extends HttpServletRequestWrapper(req):

  import ReplaceServerNameRequest.*

  override def getRequestURL(): StringBuffer =
    val reqURL = super.getRequestURL()
    MatchServer.findFirstMatchIn(reqURL)
      .flatMap: m =>
        val serverName = m.group(1)
        fromTo(serverName).map(serverName -> _)
      .foreach: (serverName, replacement) =>
        val replacePos = reqURL.indexOf(serverName)
        reqURL.replace(replacePos, replacePos + serverName.length, replacement)
    reqURL

  override def getServerName(): String =
    fromTo(req.getServerName) match
      case None => super.getServerName
      case Some(replacement) => replacement

object ReplaceServerNameRequest:
  private val MatchServer = "//([^:/]+)".r
