import org.scalatest._
import flatspec._
import matchers._
class d15test extends AnyFlatSpec with should.Matchers {
  "Empty range" should "Single value" in {
    // Empty to new
    d15.mergeRanges(Array(), (1, 2)) should be(Array(Array(1, 2)))
    // Same one
    d15.mergeRanges(Array(Array(1, 2)), (1, 2)) should be(Array(Array(1, 2)))

    // Disjoint
    d15.mergeRanges(Array(Array(1, 2)), (4, 5)) should be(
      Array(Array(1, 2), Array(4, 5))
    )

    d15.mergeRanges(Array(Array(4, 5)), (1, 2)) should be(
      Array(Array(1, 2), Array(4, 5))
    )

    d15.mergeRanges(Array(Array(1, 2)), (3, 4)) should be(
      Array(Array(1, 4))
    )
  }
}
