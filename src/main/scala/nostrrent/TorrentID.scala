package nostrrent

import scala.util.Random
import org.ngengine.bech32.Bech32
import java.nio.ByteBuffer
import org.ngengine.bech32.Bech32Exception

class TorrentID(long: Long):

  import TorrentID.*

  override val toString =
    Bech32.bech32Encode(
      PrefixBytes,
      ByteBuffer.wrap(Toolbox.toByteArray(long))
    ).drop(Prefix.length + 1)


object TorrentID:

  private final val Prefix = "nostrrent"
  private final val PrefixBytes = Prefix.getBytes()

  private def invalidBech32(invalid: String) = IAE(s"Invalid identifier: $invalid")

  def apply(long: Long = Random.nextLong) = new TorrentID(long)

  def apply(unqualifiedBech32: String): TorrentID =
    val bb =
      try Bech32.bech32Decode(s"${Prefix}1$unqualifiedBech32")
      catch case _: Bech32Exception => throw invalidBech32(unqualifiedBech32)

    if bb.capacity != 8 then throw invalidBech32(unqualifiedBech32)

    val array = new Array[Byte](8)
    bb.get(array)
    new TorrentID(BigInt(array).toLong) // TODO: Make custom conversion
