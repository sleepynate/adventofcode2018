package adventofcode

import org.scalatest.FunSuite

class Day2Suite extends FunSuite {
  import Day2.string2MappedOutWord

  test("Does not false positive for duplicates") {
    assert(!"abcdef".hasExactlyTwoOfALetter, "abcdef does not contain duplicates")
  }

  test("Can find word with 2 letters") {
    assert("bababc".hasExactlyTwoOfALetter, "bababc has exactly 2 a")
    assert("abbcde".hasExactlyTwoOfALetter, "abbcde has exactly 2 b")
  }

  test("Can find words with 3 letters") {
    assert("bababc".hasExactlyThreeOfALetter, "bababc has 3 bs")
    assert("abcccd".hasExactlyThreeOfALetter, "abcccd has 3 cs")
  }

  test("Can calculate checksum") {
    val boxIDs = List("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
    assert(Day2.getChecksum(boxIDs) == 12)
  }

//  test("Can solve puzzle part 1") {
//    assert(Day2.getChecksum(Day2.getInput) === 0)
//  }

  test("can create index list from string") {
    assert(Day2.word2IndexList("abcde") == List(('e',4), ('d',3), ('c',2), ('b',1), ('a',0)))
  }

  test("Can find most similar box ids") {
    val testList = List( "abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")
    assert(Day2.findCommonBoxIDLetters(testList) == "fgij")
  }

  test("Can find most similar box ids functionally") {
    val testList = List( "abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")
    assert(Day2.findCommonBoxIDLettersFun(testList) == "fgij")
  }

//  test("Can solve puzzle part 2") {
//    assert(Day2.findCommonBoxIDLetters(Day2.getInput) == "")
//  }
}

