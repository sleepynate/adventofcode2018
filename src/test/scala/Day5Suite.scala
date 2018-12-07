package adventofcode

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Day5Suite extends FunSuite{

  test("Should react letters correctly") {
    assert(Day5.shouldReactLetters("aA"), "aA should react")
    assert(Day5.shouldReactLetters("Aa"), "aA should react")
    assert(Day5.shouldReactLetters("bB"), "bB should react")
    assert(Day5.shouldReactLetters("Bb"), "Bb should react")
    assert(!Day5.shouldReactLetters("aa"), "aa should not react")
    assert(!Day5.shouldReactLetters("ab"), "ab should not react")
    assert(!Day5.shouldReactLetters("aB"), "aB should not react")
    assert(!Day5.shouldReactLetters("Ba"), "Ba should not react")
  }

  test("Should react string correctly") {
    Day5.reactString("aA") shouldBe ""
    Day5.reactString("abBA") shouldBe ""
    Day5.reactString("abAB") shouldBe "abAB"
    Day5.reactString("aabAAB") shouldBe "aabAAB"
    Day5.reactString("dabAcCaCBAcCcaDA") shouldBe "dabCBAcaDA"
  }

  test("Can solve Day 5 part 1") {
    val input = Day5.getInput.stripLineEnd

    assert(input.forall(_.isLetter))

    val reduced = Day5.reactString(input)

    reduced.length shouldBe 9060
  }

  test("Can solve test data") {
    val input = "dabAcCaCBAcCcaDA"
    val (removed, length) = Day5.findShortestPolymer(input)

    assert(length == 4, s"length was found by removing ${removed}")

  }
  test("Can solve Day 5 part 2") {
    val input = Day5.getInput.stripLineEnd

    assert(input.forall(_.isLetter))

    val (removed, length) = Day5.findShortestPolymer(input)

    assert(length == 6310, s"length was found by removing ${removed}")
  }
}
