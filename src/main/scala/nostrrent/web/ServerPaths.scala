package nostrrent.web

final case class ServerPaths(
  wsPathPrefix: Option[String],
  xsPathPrefix: Option[String],
)

object ServerPaths:

  def apply(wsPathPrefix: String = null, xsPathPrefix: String = null): ServerPaths =
    new ServerPaths(
      wsPathPrefix = Option(wsPathPrefix),
      xsPathPrefix = Option(xsPathPrefix),
    )
