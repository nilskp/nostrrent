package nostrrent.tests

import scodec.bits.ByteVector
import nostrrent.nostr.NostrSignature
import org.bitcoins.crypto.ECPrivateKey

@main def signHash(hexHash: String): Unit =
  require(btmHexHashValidation.matches(hexHash), s"Invalid BTM hex hash: $btmHexHashValidation")
  val keys = nostrrent.NostrKeyPair()
  val btHashBytes = ByteVector.fromHex(hexHash).get
  val signature = keys.sign(btHashBytes)
  assert(signature.bytes.length == 64)
  val nostrSig = NostrSignature(keys.npub, signature.bytes.toHex)
  println(s"Nostr signature: $nostrSig")
