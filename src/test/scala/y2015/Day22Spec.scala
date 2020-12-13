package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import y2015.Mana.Recharge

class Day22Spec extends AnyFlatSpec with Matchers with Day22 {
  "First test" should "print something familiar" in {
    var game = Game(Player("me", hit = 10, available = 250), Player("boss", hit = 13, damage = 8), debug = true)

    var mana = Seq("Poison", "", "MagicMissile", "")
    while (!game.isOver) {
      def f(p: Player): Boolean = {
        mana.isEmpty || p.newMana.get.name.startsWith(mana.head)
      }

      game = game.next(f).head
      if (mana.nonEmpty) mana = mana.tail
    }
    game.isOver shouldBe true
    game.time shouldBe 4
    game.player.hit shouldBe 2
    game.player.available shouldBe 24
    game.boss.hit shouldBe 0
    game.player.spent shouldBe 226
  }

  "second test" should "be passing now too" in {
    var game = Game(Player("me", hit = 10, available = 250), Player("boss", hit = 14, damage = 8), debug = true)

    var mana = Seq("Recharge", "", "Shield", "", "Drain", "", "Poison", "", "MagicMissile", "")
    val games = game.playAll(filter = true, mana)

    games.foreach(_.foreach(println))

    game = games.head.head
    game.isOver shouldBe true
    game.time shouldBe 10
    game.player.hit shouldBe 1
    game.player.available shouldBe 114
    game.boss.hit shouldBe -1
    game.player.spent shouldBe 641 // (229 + 113 + 73 + 173 + 53)
  }

  "test 1" should "pass, but it doesn't yet" in {
    val mana = Seq("Poison", "", "Recharge", "", "Poison", "", "Poison", "", "MagicMissile", "", "", "")
    val game = Game(Player("me", hit = 50, available = 500), Player("boss", hit = 58, damage = 9), debug = true)
    val games: Seq[Seq[Game]] = game.playAll(filter = true, mana)
    games.forall(game => game.head.isOver) shouldBe true
    val top10 = games.filter(_.head.playerWins).sortBy(_.head.player.spent).take(10)
    // println(top10.head.foreach(println))
    // 1269, 1309
  }
}

