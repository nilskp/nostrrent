package nostrrent

final case class TorrentTooBig(size: Int)
extends IllegalStateException(s"Torrent file would be $size bytes, that's excessive")
