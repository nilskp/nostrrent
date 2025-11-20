package nostrrent

import java.io.{ File, InputStream, FileOutputStream }
import scala.annotation.tailrec
import nostrrent.nostr.NostrSignature
import scala.util.Try
import java.net.URL
import nostrrent.bittorrent.MagnetLink

trait FileStorage:

  type BTMHashHex = String

  /**
    * Save file collection.
    * @param files
    * @return the torrent id and hexadecimal representation of the torrent BTM Hash
    */
  def saveFiles(files: IterableOnce[(String, InputStream)]): (TorrentID, BTMHashHex)

  /**
    * List files in torrent.
    * @param torrentID
    * @return torrent filenames
    */
  def listFiles(torrentID: TorrentID): IndexedSeq[String]

  /**
    * Generate Bittorrent multi-hash (v2).
    * @param torrentID The scope to hash
    * @return Hexadecimal BT hash
    */
  def generateBTMHash(torrentID: TorrentID): BTMHashHex

  case class SeedInfo(magnet: MagnetLink, torrentBytes: Array[Byte])

  /**
    * Verify signature and seed collection.
    * @param torrentID identifier
    * @param sig Signature
    * @param webSeedURL Full URL to root of file serving
    * @return Magnet link and torrent info bytes
    */
  def verifyAndSeed(
    torrentID: TorrentID,
    sig: NostrSignature,
    webSeedURL: Option[String => URL]): Try[SeedInfo]


trait LocalFileSystem(val workDir: File, ioBufferSize: Int)
extends FileStorage:

  require(workDir == workDir.getCanonicalFile, s"Working directory must be canonical")

  if ! workDir.exists() then
    if ! workDir.mkdir() then throw SystemException(s"Cannot create dir: $workDir")

  private val WindowsIllegalChars = """[\/:\*\?"|<>]""".r

  def saveFiles(files: IterableOnce[(String, InputStream)]): (TorrentID, BTMHashHex) =
    @tailrec
    def saveFiles(torrentID: TorrentID, files: IterableOnce[(String, InputStream)]): (TorrentID, BTMHashHex) =
      val newTorrentFolder = File(workDir, torrentID.toString)
      if newTorrentFolder.exists then
        saveFiles(TorrentID(), files) // Torrent ID clash, try again (very unlikely)
      else if ! newTorrentFolder.mkdir() then
        throw SystemException(s"Cannot create dir: $newTorrentFolder")
      else
        val filesWritten =
          files.iterator
            .map(writeTorrentFile(newTorrentFolder))
            .size
        if filesWritten == 0 then
          Try { newTorrentFolder.delete() }
          throw IAE("No file(s) provided")
        // Success:
        torrentID -> generateBTMHash(torrentID)

    saveFiles(TorrentID(), files)


  protected def webSeedPath(torrentID: TorrentID): File =
    val torrentDir = File(workDir, torrentID.toString)
    torrentDir.list() match
      case null => throw sys.error(s"Invalid folder: $torrentDir")
      case Array(onlyFilename) => File(torrentDir, onlyFilename)
      case Array() => throw sys.error(s"No files: $torrentDir")
      case _ => torrentDir

  private def writeTorrentFile(
      torrentFolder: File, ioBuffer: Array[Byte] = new Array(ioBufferSize))(
      filename: String, inp: InputStream): Unit =
    val safeFilename = WindowsIllegalChars.replaceAllIn(filename, "_")
    val file = File(torrentFolder, safeFilename)
    writeFile(inp, file, ioBuffer)

  def writeFile(inp: InputStream, file: File, ioBuffer: Array[Byte] = new Array(ioBufferSize)): Unit =
    if ! file.createNewFile() then
      if file.exists() then throw DuplicateFilename(file.getName)
      else throw SystemException(s"Cannot create file: ${file.getName}")
    val out = FileOutputStream(file)

    @tailrec
    def writeNextChunk(): Unit =
      inp.read(ioBuffer) match
        case -1 =>
          out.close()
        case bytesRead =>
          out.write(ioBuffer, 0, bytesRead)
          writeNextChunk()

    try writeNextChunk()
    catch case e: Exception =>
      file.delete()
      throw e
