package webapp

import org.scalatest.flatspec.AnyFlatSpec

class ShiftSpec extends AnyFlatSpec {

  "Shifting" should "works" in {
    val row = Seq(Some(2), Some(2), None, Some(4))
    val res = ShiftUtil.shiftLeft(row)
    assert(res === (Seq(4, 4), 4))
  }

  "Shifting" should "works2" in {
    val row = Seq(Some(2), Some(4), Some(2), Some(8))
    val res = ShiftUtil.shiftLeft(row)
    assert(res === (Seq(2,4,2,8), 0))
  }


  "Shifting" should "works3" in {
    val row = Seq(None, None, None, None)
    val res = ShiftUtil.shiftLeft(row)
    assert(res === (Seq.empty, 0))
  }

  "Shifting" should "works5" in {
    val row = Seq(Some(4), None, None, None)
    val res = ShiftUtil.shiftLeft(row)
    assert(res === (Seq(4), 0))
  }

  "Shifting" should "works6" in {
    val row = Seq(None, Some(4), None, None)
    val res = ShiftUtil.shiftLeft(row)
    assert(res === (Seq(4), 0))
  }


  "Shifting" should "works7" in {
    val row = Seq(Some(16), Some(8), None, Some(4))
    val res = ShiftUtil.shiftLeft(row)
    println(res)
    assert(res === (Seq(16, 8, 4), 0))
  }
}