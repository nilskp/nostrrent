package nostrrent.web

import nostrrent.nostr.NostrSignature
import scala.util.Try

final case class SeedOptions(
  signature: NostrSignature,
  exposeHttpServer: Boolean,
)

object SeedOptions:
  import com.github.plokhotnyuk.jsoniter_scala.macros._
  import com.github.plokhotnyuk.jsoniter_scala.core._

  private final case class JSON(pubKey: String, hashSig: String, exposeHttpServer: Boolean = false)

  private given JsonValueCodec[JSON] = JsonCodecMaker.make

  def fromJSON(json: String): Try[SeedOptions] = Try:
    val JSON(pubKey, hashSig, exposeHttpServer) = readFromString[JSON](json)
    SeedOptions(NostrSignature(pubKey, hashSig), exposeHttpServer)

  def fromFormData(params: Map[String, String]): Try[SeedOptions] = Try:
    val nostrSig = NostrSignature(params("pubKey"), params("hashSig"))
    val exposeHttpServer = params.get("exposeHttpServer").map(_.toLowerCase).map:
        case "false" => false
        case _ => true
      .getOrElse(false)
    SeedOptions(nostrSig, exposeHttpServer)
