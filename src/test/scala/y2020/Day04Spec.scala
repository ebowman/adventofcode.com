package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day04Spec extends AnyFlatSpec with Matchers with Day04 {

  "Driver" should "do pass the given test case" in {
    val input =
      """
        |ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
        |byr:1937 iyr:2017 cid:147 hgt:183cm
        |
        |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
        |hcl:#cfa07d byr:1929
        |
        |hcl:#ae17e1 iyr:2013
        |eyr:2024
        |ecl:brn pid:760753108 byr:1931
        |hgt:179cm
        |
        |hcl:#cfa07d eyr:2025 pid:166559648
        |iyr:2011 ecl:brn hgt:59in""".stripMargin.trim.linesIterator.toSeq

    var count = 0
    fields.clear()
    for (l <- input) {
      if (l.isEmpty) {
        if (validate(fields.toMap)) count += 1
        fields.clear()
      } else parseAll(line, l)
    }
    count shouldBe 2
  }

  it should "pass the test" in {
    var count = 0
    fields.clear()
    for (l <- Loader(this, "day04.txt")) {
      if (l.isEmpty) {
        if (validate(fields.toMap)) count += 1
        fields.clear()
      } else parseAll(line, l)
    }
    if (fields.nonEmpty) {
      if (validate(fields.toMap)) count += 1
      fields.clear()
    }
    count shouldBe 228
  }

  it should "pass the second test" in {

    extended = true
    var count = 0
    fields.clear()
    for (l <- Loader(this, "day04.txt")) {
      if (l.isEmpty) {
        if (validate(fields.toMap)) count += 1
        fields.clear()
      } else parseAll(line, l)
    }
    if (fields.nonEmpty) {
      if (validate(fields.toMap)) count += 1
      fields.clear()
    }
    count shouldBe 175
  }
}
