package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day21Spec extends AnyFlatSpec with Matchers with Day21 {

  "The game basics" should "be in place" in {
    var player = Player("me", hit = 8, damage = 5, armor = 5)
    var boss = Player("boss", hit = 12, damage = 7, armor = 2)

    boss = player.attack(boss)
    boss.hit shouldBe 9
    player = boss.attack(player)
    player.hit shouldBe 6
    boss = player.attack(boss)
    boss.hit shouldBe 6
    player = boss.attack(player)
    player.hit shouldBe 4
    boss = player.attack(boss)
    boss.hit shouldBe 3

    val game = Game(Player("me", hit = 8, damage = 5, armor = 5), Player("boss", hit = 12, damage = 7, armor = 2))
    game.play.playerWins shouldBe true
    println(game.play)
    game.play.player.hit shouldBe 2
  }

  lazy val boss = Player("Boss", hit = 109, damage = 8, armor = 2)

  "The final engine" should "find the cheapest way to win (part 1)" in {
    val winners = for {
      weapon <- Items.weapons
      armor <- Items.armors
      ring <- Items.rings.combinations(2)
      player = Player("Player").loadUp(ring :+ weapon :+ armor)
      endGame = Game(player, boss).play if endGame.playerWins
    } yield endGame

    winners.minBy(_.player.cost).player.cost shouldBe 111
  }

  it should "find the most expensive way to lose (part 2)" in {
    val losers = for {
      weapon <- Items.weapons
      armor <- Items.armors
      ring <- Items.rings.combinations(2)
      player = Player("Player").loadUp(ring :+ weapon :+ armor)
      endGame = Game(player, boss).play if !endGame.playerWins
    } yield endGame

    losers.maxBy(_.player.cost).player.cost shouldBe 188
  }

}

