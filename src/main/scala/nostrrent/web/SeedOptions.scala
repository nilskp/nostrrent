package nostrrent.web

import nostrrent.nostr.NostrSignature
import scala.util.Try

final case class SeedOptions(
  signature: NostrSignature,
  exposeHttpSeeding: Boolean,
)

object SeedOptions:
  import com.github.plokhotnyuk.jsoniter_scala.macros._
  import com.github.plokhotnyuk.jsoniter_scala.core._

  private final case class JSON(npub: String, hashSig: String, exposeHttpSeeding: Boolean = false)

  private given JsonValueCodec[JSON] = JsonCodecMaker.make

  def fromJSON(json: String): Try[SeedOptions] = Try:
    val JSON(npub, hashSig, exposeHttpSeeding) = readFromString[JSON](json)
    SeedOptions(NostrSignature(npub, hashSig), exposeHttpSeeding)

  def fromFormData(params: Map[String, String]): Try[SeedOptions] = Try:
    val nostrSig = NostrSignature(params("npub"), params("hashSig"))
    val exposeHttpSeeding = params.get("exposeHttpSeeding").map(_.toLowerCase).map:
        case "false" | "off" | "0" => false
        case _ => true
      .getOrElse(false)
    SeedOptions(nostrSig, exposeHttpSeeding)
