package nostrrent

import org.ngengine.bech32.Bech32
import org.bitcoins.crypto.ECPrivateKey
import java.nio.ByteBuffer
import scodec.bits.ByteVector
import org.bitcoins.crypto.SchnorrDigitalSignature
import fr.acinq.secp256k1.Secp256k1
import java.util.Arrays

class NostrKeyPair(
  privKey: ECPrivateKey = ECPrivateKey()):

  private val secp256k1 = Secp256k1.get

  private val pubkey = privKey.publicKey.compressed.bytes.drop(1).toArray

  val npub = Bech32.bech32Encode(
    "npub".getBytes,
    ByteBuffer.wrap(pubkey),
  )

  if Arrays.equals(pubkey, decodeNpub(npub)) then println("Bech32 decode works!")
  else sys.error("Bech32 decode problem")

  private def decodeNpub(npub: String): Array[Byte] =
    val bb = Bech32.bech32Decode(npub)
    val array = new Array[Byte](bb.capacity)
    bb.get(array)
    array


  def sign(bytes: Array[Byte]): SchnorrDigitalSignature =
    sign(bytes, ByteVector(bytes))
  def sign(bytes: ByteVector): SchnorrDigitalSignature =
    sign(bytes.toArray, bytes)

  private def sign(dataArray: Array[Byte], dataBytes: ByteVector) =
    val signature = privKey.schnorrSign(dataBytes)
    if secp256k1.verifySchnorr(signature.bytes.toArray, dataArray, pubkey) then println("Signature validated!")
    else sys.error("Signature validation failed!")
    signature
