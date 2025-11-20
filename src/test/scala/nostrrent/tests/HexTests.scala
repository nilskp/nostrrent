package nostrrent.tests

import org.scalatest.funsuite.AnyFunSuite

class HexTests
extends AnyFunSuite:

  test("Hex parsing"):
    assert(1 + 1 == 2)
    assert(nostrrent.Toolbox.hexToByteArray("FFFF") === Array[Byte](-1, -1) )
    assert(nostrrent.Toolbox.hexToByteArray("FF00") === Array[Byte](-1, 0) )
    assert(nostrrent.Toolbox.hexToByteArray("0f0f0F") === Array[Byte](15, 15, 15) )
    assert(nostrrent.Toolbox.hexToByteArray("F0") === Array[Byte](-16) )
