package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

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
  }

  "second test" should "be passing now too" in {
    var game = Game(Player("me", hit = 10, available = 250), Player("boss", hit = 14, damage = 8), debug = true)

    var mana = Seq("Recharge", "", "Shield", "", "Drain", "", "Poison", "", "MagicMissile", "")
    while (!game.isOver) {
      def f(p: Player): Boolean = {
        mana.isEmpty || p.newMana.get.name.startsWith(mana.head)
      }

      game = game.next(f).head
      if (mana.nonEmpty) mana = mana.tail
    }

    game.isOver shouldBe true
    game.time shouldBe 10
    game.player.hit shouldBe 1
    game.player.available shouldBe 114
    game.boss.hit shouldBe -1

  }

  /*
  "Basics" should "be working soon" in {
    val player = Player("me", hit = 50, available = 500)
    val boss = Player("boss", hit = 58, damage = 9)
    val game = Game(player, boss, debug = false, pruning = true)
    val games: Seq[Seq[Game]] = game.playAll
    games.forall(game => game.head.isOver) shouldBe true
    //println(games.count(_.head.playerWins))
    //println(games.count(!_.head.playerWins))
    //games.foreach(g => println(g.head))
    println(games.filter(_.head.playerWins).minBy(_.head.player.spent).head.player.spent)
  }
   */
}

