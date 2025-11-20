package nostrrent.web.jetty_scalatra

import org.eclipse.jetty.ee10.servlet.ResourceServlet
import jakarta.servlet.http.{ HttpServletRequest, HttpServletResponse }
import java.io.File
import org.eclipse.jetty.ee10.servlet.ServletContextHandler

abstract class AbstractFileServer(workDir: File)
extends ResourceServlet:

  def path: String

  def configure(ctx: ServletContextHandler): Unit =
    val holder = ctx.addServlet(this, s"$path/*")
    Map(
      "acceptRanges" -> true,
      "baseResource" -> workDir,
      "cacheControl" -> "public, immutable",
      "cacheValidationTime" -> -1,
      "dirAllowed" -> false, // We *can* override this for the collection dirs and serve JSON
      "maxCachedFiles" -> 0,
      "useFileMappedBuffer" -> false, // Seems memory inefficient to enable, *particularly* for large files, and have GC issues
    ).foreach: (key, value) =>
        holder.setInitParameter(key, value.toString)

  /**
   * Make 404 into 403 to match directory listing attempt.
   * This minimizes information leaking about server state.
   */
  override def doNotFound(req: HttpServletRequest, res: HttpServletResponse, encodedPathInContext: String): Unit =
    res.sendError(HttpServletResponse.SC_FORBIDDEN)
