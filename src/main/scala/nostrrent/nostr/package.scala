package nostrrent

import fr.acinq.secp256k1.{ Secp256k1, Secp256k1Exception }
import org.ngengine.bech32.{ Bech32, Bech32Exception }
import nostrrent.Toolbox.hexToByteArray

package object nostr:

  private val secp = Secp256k1.get() // Singleton instance

  private def decodeNpub(npub: String): Array[Byte] =
    val bb = Bech32.bech32Decode(npub)
    val array = new Array[Byte](bb.capacity)
    bb.get(array)
    array

  protected[nostr] def verifySignature(btHexHash: String, nostrSig: NostrSignature): Boolean =
    try
      val npubBytes = decodeNpub(nostrSig.npub)
      val hashBytes = hexToByteArray(btHexHash)
      assert(hashBytes.length == 32)
      val sigBytes = nostrSig.hashSigBytes
      assert(sigBytes.length == 64, s"Signature must be 64 bytes, was ${sigBytes.length}")
      secp.verifySchnorr(sigBytes, hashBytes, npubBytes)
    catch
      case th: (Secp256k1Exception | Bech32Exception) =>
        throw IAE(th.getMessage, th)
