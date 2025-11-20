package nostrrent.bittorrent

import nostrrent.*
import nostrrent.nostr.NostrSignature
import com.frostwire.jlibtorrent.{
  SessionManager, SessionParams, SettingsPack,
  TorrentBuilder, TorrentInfo,
  swig
}
import java.io.{ File, FileInputStream, ByteArrayInputStream }
import scala.util.Try
import java.net.URL
import com.frostwire.jlibtorrent.AlertListener
import com.frostwire.jlibtorrent.alerts.{ Alert, AlertType }

class JLibTorrent(rootTorrentDir: File, ioBufferSize: Int)
extends nostrrent.LocalFileSystem(rootTorrentDir, ioBufferSize)
with AutoCloseable:

  import JLibTorrent.*

  private val session = SessionManager() // Configure and start in companion

  def close(): Unit = session.stop()

  private def torrentInfo(torrentID: TorrentID): TorrentInfo =
    val saveDir = File(workDir, torrentID.toString)
    val torrent = TorrentBuilder()
      .path(saveDir)
      .generate()
    TorrentInfo(torrent.entry.bencode)

  def generateBTMHash(torrentID: TorrentID): String =
    torrentInfo(torrentID).infoHashV2.toHex()

  def listFiles(torrentID: TorrentID): IndexedSeq[String] =
    val info = torrentInfo(torrentID)
    val files = info.files()
    (0 until info.numFiles).collect:
      case idx
        if files.fileFlags(idx).notPadding =>
          files.fileName(idx)

  def verifyAndSeed(
    torrentID: TorrentID, sig: NostrSignature,
    webSeedURL: Option[String => URL])
  : Try[SeedInfo] = Try:

    // val canonicalSeedPath = torrentSeedPath(torrentID).getCanonicalFile()
    val saveDir = File(workDir, torrentID.toString).getCanonicalFile()
    val torrentFile = File(rootTorrentDir, s"$torrentID$TorrentFileExt")

    val (info, torrentBytes) =
      if torrentFile.exists() then
        loadTorrentInfo(torrentFile)
      else
        val torrent =
          TorrentBuilder()
            .path(saveDir)
            .creator(Creator)
            .comment(sig.toComment)
            .generate()
        val torrentBytes = torrent.entry.bencode
        TorrentInfo.bdecode(torrentBytes) -> torrentBytes

    log.info(s"info.isValid: ${info.isValid}")
    log.info(s"info.isLoaded: ${info.isLoaded}")
    log.info(s"info.isValid: ${info.isValid}")

    if torrentBytes.length > MaxTorrentFileSize then
      throw TorrentTooBig(torrentBytes.length)
    if info.comment != sig.toComment then
      throw IllegalStateException("Signature mismatch")
    if ! sig.verifySignature(info.infoHashV2.toHex) then
      throw IAE("Signature validation failed")

    webSeedURL.map: makeWebSeedURL =>
      val seedPath = webSeedPath(torrentID)
      val relativeSeedPath =
        if seedPath.isDirectory then seedPath.getName
        else s"$torrentID/${seedPath.getName}"
      val webSeedURL = makeWebSeedURL(relativeSeedPath)
      info.addUrlSeed(webSeedURL.toString)

    if ! torrentFile.exists() then
      this.writeFile(
        ByteArrayInputStream(info.bencode),
        torrentFile)

    // if session.find(info) == null then // Race condition, but no obvious way to avoid
    JLibTorrent.seed(session, saveDir, info)

    val magnet = MagnetLink.parse(info.makeMagnetUri())
    SeedInfo(magnet, torrentBytes)

end JLibTorrent

object JLibTorrent:
  import com.frostwire.jlibtorrent.FileStorage.*

  private val log = Logger(this.getClass)

  final val Creator = "Nostrrent"
  final val MaxTorrentFileSize = 10*1024*1024

  extension[T](fileFlags: swig.file_flags_t)
    def notPadding: Boolean = ! fileFlags.and_(FLAG_PAD_FILE).nonZero()

  private case class SeedFile(torrentFile: File, torrentFolder: File):
    assert(torrentFile.isFile)
    assert(torrentFolder.isDirectory)
    assert(torrentFile.getName.startsWith(torrentFolder.getName))
    assert(torrentFile.getName.endsWith(TorrentFileExt))

  private def deleteDir(dir: File): Unit =
    Option(dir.listFiles()).foreach(_.foreach(_.delete()))
    dir.delete()

  def apply(rootTorrentDir: File, ioBufferSize: Int): JLibTorrent =
    val impl = new JLibTorrent(rootTorrentDir, ioBufferSize)
    val seedFolders =
      rootTorrentDir.listFiles(_.isDirectory)
        .flatMap: folder =>
          val torrentFile = File(rootTorrentDir, s"${folder.getName}$TorrentFileExt")
          if torrentFile.isFile then
            Some(SeedFile(torrentFile, folder))
          else // Cleanup:
            deleteDir(torrentFile)
            deleteDir(folder)
            None
        .toList
    startSession(impl.session, seedFolders)
    impl

  import com.frostwire.jlibtorrent.{ TorrentFlags => tf }
  private final val SeedFlags =
    tf.SEED_MODE or_
    tf.SHARE_MODE or_
    tf.UPLOAD_MODE

  private def seed(session: SessionManager, torrentDir: File, info: TorrentInfo): Unit =
    log.info(s"Seeding `${torrentDir}` | ${info.infoHashV2.toHex}")
    val files = info.files
    val infoNumFiles = info.numFiles()
    val filesNumFiles = files.numFiles()
    val infoName = info.name()
    val filesName = files.name
    log.info(s"\\t info.name: $infoName, files.name: $filesName, info.numFiles: $infoNumFiles, files: $filesNumFiles")
    val paths = files.paths()
    log.info(s"\\t paths = $paths")
    (0 until filesNumFiles).map: i =>
      val filename = files.fileName(i)
      val path = files.filePath(i)
      val size = files.fileSize(i)
      log.info(s"\\t\\t path: $path, filename: $filename, size: $size")
    session.download(info, torrentDir, null, null, null, SeedFlags)

  private def loadTorrentInfo(torrentFile: File): (TorrentInfo, Array[Byte]) =
    val bytes = FileInputStream(torrentFile).readAllBytes()
    TorrentInfo.bdecode(bytes) -> bytes

  private def startSession(session: SessionManager, existing: IterableOnce[SeedFile]): Unit =
    import swig.settings_pack.bool_types.*
    swig.settings_pack.bool_types.no_recheck_incomplete_resume

    val settings = SettingsPack()

    Map( // Boolean settings:
      enable_upnp -> true,
      enable_natpmp -> true,
      enable_incoming_tcp -> true,
      enable_incoming_utp -> true,
      no_recheck_incomplete_resume -> true,
    ).foreach: (name, value) =>
      settings.setBoolean(name.swigValue, value)

    settings.activeDownloads(0)
    settings.activeSeeds(Int.MaxValue)
    settings.setEnableDht(true)
    settings.setEnableLsd(false)
    settings.seedingOutgoingConnections(true)

    session.addListener:
      new AlertListener:
        def alert(alert: Alert[?]): Unit = log.info(s"libtorrent: $alert")
        def types(): Array[Int] =
          AlertType.values()
            .filterNot: at =>
              at == AlertType.UNKNOWN ||
              at == AlertType.SESSION_STATS ||
              at == AlertType.SESSION_STATS_HEADER ||
              at == AlertType.STATE_UPDATE ||
              at.name.startsWith("DHT_") ||
              at.name.startsWith("PEER_") ||
              at.name.startsWith("LISTEN_")
            .map(_.swig)

    session.start(SessionParams(settings))

    // Start seeding existing torrents:
    existing.iterator.foreach:
      case SeedFile(torrentFile, folder) =>
        val (info, _) = loadTorrentInfo(torrentFile)
        seed(session, folder, info)
