package nostrrent

final case class DuplicateFilename(filename: String)
extends IllegalArgumentException(s"Duplicate filename: $filename")
