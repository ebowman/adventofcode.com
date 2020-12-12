package y2015

import scala.annotation.tailrec

trait Day21 {

  case class Player(name: String, hit: Int = 100, damage: Int = 0, armor: Int = 0, cost: Int = 0, items: Seq[Item] = Seq()) {
    def dead: Boolean = hit <= 0

    override def toString = s"$name(hit=$hit,damage=$damage,armor=$armor,cost=$cost)"

    def attack(defender: Player): Player = {
      val delta = Math.max(1, this.damage - defender.armor)
      defender.copy(hit = defender.hit - delta)
    }

    def loadUp(items: Seq[Item]): Player = {
      @tailrec
      def recurse(p: Player, i: Seq[Item]): Player = {
        if (i.isEmpty) p
        else {
          recurse(p.copy(
            damage = p.damage + i.head.damage,
            armor = p.armor + i.head.armor,
            cost = p.cost + i.head.cost), i.tail)
        }
      }

      recurse(this, items).copy(items = items)
    }
  }

  case class Item(name: String, cost: Int, damage: Int, armor: Int)

  case class Game(player: Player, boss: Player) {
    def playerWins: Boolean = boss.dead && !player.dead

    def play: Game = {
      @tailrec
      def recurse(game: Game): Game = {
        val newBoss = game.player.attack(game.boss)
        if (newBoss.dead) Game(game.player, newBoss)
        else {
          val newPlayer = newBoss.attack(game.player)
          if (newPlayer.dead) Game(newPlayer, newBoss)
          else recurse(Game(newPlayer, newBoss))
        }
      }

      recurse(this)
    }
  }

  object Items {
    val weapons = Seq( // note the rule: you must buy at least one weapon!
      Item("Dagger", 8, 4, 0),
      Item("Shortsword", 10, 5, 0),
      Item("Warhammer", 25, 6, 0),
      Item("Longsword", 40, 7, 0),
      Item("Greataxe", 74, 8, 0),
    )

    val armors = Seq(
      Item("NoArmor", 0, 0, 0),
      Item("Leather", 13, 0, 1),
      Item("Chainmail", 31, 0, 2),
      Item("Splintmail", 53, 0, 3),
      Item("Bandedmail", 75, 0, 4),
      Item("Platemail", 102, 0, 5)
    )

    val rings = Seq(
      Item("No Ring 1", 0, 0, 0),
      Item("No Ring 2", 0, 0, 0),
      Item("Damage +1", 25, 1, 0),
      Item("Damage +2", 50, 2, 0),
      Item("Damage +3", 100, 3, 0),
      Item("Defense +1", 20, 0, 1),
      Item("Defense +2", 40, 0, 2),
      Item("Defense +3", 80, 0, 3)
    )
  }

}