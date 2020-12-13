package y2015

trait Day22 {


  case class Game(player: Player, boss: Player, time: Int = 0, debug: Boolean = false) {

    override def toString: String = s"Game(time=$time,$player,$boss"

    def playerWins: Boolean = boss.dead

    def playAll(filter: Boolean = false, mana: Seq[String] = Seq.empty): Seq[Seq[Game]] = {
      def recurse(gs: Seq[Game], m: Seq[String]): Seq[Seq[Game]] = {
        def f(p: Player): Boolean = {
          m.isEmpty || p.newMana.get.name.startsWith(m.head)
        }
        (for {
          g <- gs
          n <- if (!filter) g.next() else g.next(f)
        } yield {
          if (n.isOver) {
            Seq(Seq(g, n).reverse)
          }
          else {
            recurse(Seq(n), if (filter) m.tail else Seq.empty).map { s => s :+ g }
          }
        }).flatten
      }

      recurse(Seq(this), mana)
    }

    import Game.minSpent

    def next(filter: Player => Boolean = { _ => true }): Seq[Game] = {
      if (debug) println(s"time = $time")
      if ((time % 2) == 0) { // player turn
        val players = player.mananize(minSpent, time).filter(filter)
        if (players.isEmpty) Seq(copy(player = player.copy(outOfMoney = true), time = time + 1))
        for {
          player <- players
        } yield {
          if (debug) println("-- Player turn --")
          if (debug) println(s"- Player has ${player.hit} hit points, ${player.armor} armor, ${player.available + player.newMana.get.cost} mana")
          if (debug) println(s"- Boss has ${boss.hit} hit points")
          // play the effects of existing mana
          val (p1, b1) = player.mana.foldLeft((player, boss)) {
            case ((player, boss), mana) => mana.playerTurn(debug, time, player, boss)
          }

          // turn on any new mana
          val (p2, b3) = p1.newMana.map { nM =>
            val (p3, b2) = nM.turnOn(debug, p1, b1)
            (p3.copy(mana = p3.mana :+ nM, newMana = None), b2)
          }.getOrElse((p1, b1))

          val p5 = p2.expire(debug, time)

          val g = copy(player = p5, boss = b3, time = time + 1)
          if (g.isOver && p5.spent < minSpent) {
            minSpent = p5.spent
            println(s"minSpent = $minSpent")
          }
          g
        }
      } else { // boss turn
        if (debug) println("-- Boss turn --")
        if (debug) println(s"- Player has ${player.hit} hit points, ${player.armor} armor, ${player.available} mana")
        if (debug) println(s"- Boss has ${boss.hit} hit points")

        // play the effects of existing mana
        val (p1, b1) = player.mana.foldLeft((player, boss)) {
          case ((player, boss), mana) => mana.bossTurn(debug, time, player, boss)
        }

        if (!b1.dead) {
          val delta1 = b1.damage - p1.armor
          val p2 = if (p1.hit == player.hit || delta1 >= 1) {
            val damage = Math.max(delta1, 1)
            if (p1.armor > 0) {
              if (debug) println(s"Boss attacks for ${b1.damage} - ${p1.armor} = $damage damage!")
            } else {
              if (debug) println(s"Boss attacks for $damage damage!")
            }
            p1.copy(hit = p1.hit - damage)
          } else p1

          val p5 = p2.expire(debug, time)

          Seq(copy(player = p5, boss = b1, time = time + 1))
        } else {
          Seq(Game(player = p1, boss = b1, time = time + 1))
        }
      }
    }

    def isOver: Boolean = boss.dead || player.dead
  }

  object Game {
    var minSpent: Int = Int.MaxValue
  }
}

case class Player(name: String,
                  hit: Int = 100,
                  damage: Int = 0,
                  armor: Int = 0,
                  spent: Int = 0,
                  available: Int = 0,
                  newMana: Option[Mana] = None,
                  mana: Seq[Mana] = Seq.empty,
                  outOfMoney: Boolean = false) {

  def dead: Boolean = hit <= 0 || outOfMoney

  override def toString = s"$name(hit=$hit,damage=$damage,armor=$armor,spent=$spent,available=$available,mana=$mana,next=${newMana.getOrElse("x")})"

  def expire(debug: Boolean, time: Int): Player = {
    val expired = mana.filter(_.expired(time + 1))
    if (debug) println(s"Expired $expired from $this")
    val player = copy(mana = mana.diff(expired))
    // turn off expired
    expired.foldLeft(player) {
      case (p, m) => m.turnOff(debug, p)
    }
  }

  def mananize(minSpent: Int, time: Int): Seq[Player] = {
    def avail(t: Int): Seq[Mana] = Mana.mana(t).filter(_.cost <= available)

    avail(time).map(m => copy(newMana = Some(m), spent = spent + m.cost, available = available - m.cost)).filter(_.spent < minSpent)
  }
}

trait Mana {

  override def toString: String = s"$name/${born+lifespan}"

  def name: String = getClass.getName.replaceAll(".*\\$", "")

  def cost: Int

  def born: Int

  def lifespan: Int

  def age(time: Int): Int = time - born

  def expired(time: Int): Boolean = (time - born) > lifespan

  def playerTurn(debug: Boolean, time: Int, player: Player, boss: Player): (Player, Player) = (player, boss)

  def bossTurn(debug: Boolean, time: Int, player: Player, boss: Player): (Player, Player) = (player, boss)

  def turnOn(debug: Boolean, player: Player, boss: Player): (Player, Player) = {
    if (debug) println(s"Player casts $name")
    (player, boss)
  }

  def turnOff(debug: Boolean, player: Player): Player = {
    if (debug && lifespan > 1) println(s"$name wears off.")
    player
  }
}

object Mana {

  val minAvailable: Int = MagicMissile(0).cost // cheapest

  def mana(time: Int): Seq[Mana] = Seq(MagicMissile(time), Drain(time), Shield(time), Poison(time), Recharge(time))

  case class MagicMissile(born: Int) extends Mana {
    val cost = 53
    val lifespan = 1

    override def turnOn(debug: Boolean, player: Player, boss: Player): (Player, Player) = {
      if (boss.hit <= 4) {
        if (debug) println(s"Player casts $name, dealing 4 damage. This kills the boss, and the player wins.")
      } else {
        if (debug) println(s"Player casts $name, dealing 4 damage.")
      }
      (player, boss.copy(hit = boss.hit - 4))
    }
  }

  case class Drain(born: Int) extends Mana {
    val cost = 73
    val lifespan = 1

    override def turnOn(debug: Boolean, player: Player, boss: Player): (Player, Player) = {
      if (boss.hit <= 2) {
        if (debug) println(s"Player casts $name, dealing 2 damage, and healing 2 hit points. This kills the boss, and the player wins.")
      } else {
        if (debug) println(s"Player casts $name, dealing 2 damage, and healing 2 hit points.")
      }
      (player.copy(hit = player.hit + 2), boss.copy(hit = boss.hit - 2))
    }
  }

  case class Shield(born: Int) extends Mana {
    val cost = 113
    val lifespan = 6

    override def turnOn(debug: Boolean, player: Player, boss: Player): (Player, Player) = {
      if (debug) println(s"Player casts $name, gets increasing armor by 7")
      (player.copy(armor = player.armor + 7), boss)
    }

    override def playerTurn(debug: Boolean, time: Int, p: Player, b: Player): (Player, Player) = {
      if (debug) println(s"Shield's timer is now ${lifespan - age(time)}")
      (p, b)
    }

    override def bossTurn(debug: Boolean, time: Int, p: Player, b: Player): (Player, Player) = {
      if (debug) println(s"Shield's timer is now ${lifespan - age(time)}")
      (p, b)
    }

    override def turnOff(debug: Boolean, player: Player): Player = {
      if (debug) println(s"$name wears off.")
      player.copy(armor = player.armor - 7)
    }
  }

  case class Poison(born: Int) extends Mana {
    val cost = 173
    val lifespan = 6

    override def playerTurn(debug: Boolean, time: Int, player: Player, boss: Player): (Player, Player) = {
      if (boss.hit <= 3) {
        if (debug) println(s"$name deals 3 damage, its timer is now ${lifespan - age(time)}. This kills the boss, and the player wins.")
      } else {
        if (debug) println(s"$name deals 3 damage, its timer is now ${lifespan - age(time)}")
      }
      (player, boss.copy(hit = boss.hit - 3))
    }

    override def bossTurn(debug: Boolean, time: Int, player: Player, boss: Player): (Player, Player) = {
      if (boss.hit <= 3) {
        if (debug) println(s"$name deals 3 damage, its timer is now ${lifespan - age(time)}. This kills the boss, and the player wins.")
      } else {
        if (debug) println(s"$name deals 3 damage, its timer is now ${lifespan - age(time)}")
      }
      (player, boss.copy(hit = boss.hit - 3))
    }
  }

  case class Recharge(born: Int) extends Mana {
    val cost = 229
    val lifespan = 5

    override def playerTurn(debug: Boolean, time: Int, player: Player, boss: Player): (Player, Player) = {
      if (debug) println(s"$name provides 101 rama, its timer is now ${lifespan - age(time)}")
      (player.copy(available = player.available + 101), boss)
    }

    override def bossTurn(debug: Boolean, time: Int, player: Player, boss: Player): (Player, Player) = {
      if (debug) println(s"$name provides 101 rama, its timer is now ${lifespan - age(time)}")
      (player.copy(available = player.available + 101), boss)
    }
  }

}
