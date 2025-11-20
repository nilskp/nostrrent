package nostrrent.nostr

import nostrrent.Toolbox.hexToByteArray

/**
  * Nostr signature.
  * @param npub Nostr public key
  * @param hashSigHex Nostr signature of BT hash, 128 char hex string
  */
final case class NostrSignature(npub: String, hashSigHex: String):
  import NostrSignature.*

  require(npubValidation.matches(npub), s"Invalid npub: $npub")
  require(sigValidation.matches(hashSigHex), s"Hash signature must be 128 character hexadecimal encoding")

  def hashSigBytes = hexToByteArray(hashSigHex)

  // def toJSON = s"""{"npub":"$npub"},"hashSig":"$hashSigHex"}"""
  def toComment = s"$npub:$hashSigHex"

  def verifySignature(btHexHash: String): Boolean =
    nostrrent.nostr.verifySignature(btHexHash, this)

object NostrSignature:
  private val npubValidation = """^npub1[02-9ac-hj-np-z]{58}$""".r
  private val sigValidation = """^[0-9a-fA-F]{128}$""".r
