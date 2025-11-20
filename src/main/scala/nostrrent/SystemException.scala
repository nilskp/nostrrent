package nostrrent

final case class SystemException(msg: String)
extends RuntimeException(msg)
