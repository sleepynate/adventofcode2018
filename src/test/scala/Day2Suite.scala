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
}

/*
abcdef contains no letters that appear exactly two or three times.
bababc contains two a and three b, so it counts for both.
abbcde contains two b, but no letter appears exactly three times.
abcccd contains three c, but no letter appears exactly two times.
aabcdd contains two a and two d, but it only counts once.
abcdee contains two e.
ababab contains three a and three b, but it only counts once.
*/

