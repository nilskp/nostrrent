package nostrrent

final case class UnknownProperty(name: String)
extends RuntimeException(s"Unknown property: $name")
