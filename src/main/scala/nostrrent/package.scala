
import java.io.File

package object nostrrent:

  def Logger(clz: Class[?]) =
    val name =
      clz.getName.substring(clz.getPackage.getName.length+1) match
        case "package$" => clz.getPackage.getName
        case name =>
          if name.endsWith("$") then name.dropRight(1)
          else name
    com.typesafe.scalalogging.Logger(name)

  private val log = Logger(this.getClass)

  final val TorrentFileExt = ".torrent"

  @inline
  def IAE(msg: String, cause: Throwable = null) = new IllegalArgumentException(msg, cause)

  extension[T](opt: Option[T])
    def ||[B >: T](orElse: => B): B = opt.getOrElse(orElse)

  val TempDir = new File(sys.props("java.io.tmpdir"))
  log.info(s"Temp dir: $TempDir")
