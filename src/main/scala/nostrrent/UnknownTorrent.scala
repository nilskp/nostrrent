package nostrrent

final case class UnknownTorrent(id: TorrentID)
extends RuntimeException(s"Unknown torrent: $id")
