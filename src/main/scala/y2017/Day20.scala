package y2017

import scala.annotation.targetName

trait Day20:
  case class Vec3(x: Int, y: Int, z: Int):
    @targetName("plus")
    def +(other: Vec3): Vec3 = Vec3(x + other.x, y + other.y, z + other.z)

    def manhattanDistance: Int = x.abs + y.abs + z.abs

  case class Particle(id: Int, position: Vec3, velocity: Vec3, acceleration: Vec3):
    def tick: Particle =
      val newVelocity = velocity + acceleration
      val newPosition = position + newVelocity
      Particle(id, newPosition, newVelocity, acceleration)

    def longTermAccelerationMagnitude: Int = acceleration.manhattanDistance

  private object Particle:
    private val ParticleRegex =
      """
      |p=<\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)>, v=<\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)>, a=<\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)>
      """.stripIndent.trim.r

    def parse(input: String, id: Int): Particle =
      input match
        case ParticleRegex(px, py, pz, vx, vy, vz, ax, ay, az) =>
          Particle(
            id,
            Vec3(px.toInt, py.toInt, pz.toInt),
            Vec3(vx.toInt, vy.toInt, vz.toInt),
            Vec3(ax.toInt, ay.toInt, az.toInt)
          )
        case _ => throw IllegalArgumentException(s"Invalid particle format: $input")

  def solvePart1(input: Seq[String]): Int =
    val particles = input.zipWithIndex.map((line, id) => Particle.parse(line, id))

    // In the long term, the particle with the smallest acceleration magnitude
    // will stay closest to the origin. If there are ties, we need to look at
    // initial conditions.
    val minAcc = particles.minBy(_.longTermAccelerationMagnitude)
    val particlesWithMinAcc = particles.filter(_.longTermAccelerationMagnitude == minAcc.longTermAccelerationMagnitude)

    if particlesWithMinAcc.size == 1 then
      minAcc.id
    else
      // For particles with the same acceleration, simulate until we find the closest one
      val numIterations = 1000 // Should be enough to establish a pattern
      val finalStates = (0 until numIterations).foldLeft(particlesWithMinAcc): (particles, _) =>
        particles.map(_.tick)
      finalStates.minBy(_.position.manhattanDistance).id
  end solvePart1

  def solvePart2(input: Seq[String]): Int =
    var particles = input.zipWithIndex.map((line, id) => Particle.parse(line, id))

    // Simulate until no collisions occur for several iterations
    val numIterationsWithoutCollisions = 10
    var stableIterations = 0
    var iterations = 0

    while stableIterations < numIterationsWithoutCollisions && iterations < 1000 do
      particles = particles.map(_.tick)

      // Find particles at the same position
      val positionGroups = particles.groupBy(_.position)
      val collisions = positionGroups.filter(_._2.size > 1).values.flatten.map(_.id).toSet

      if collisions.nonEmpty then
        particles = particles.filterNot(p => collisions.contains(p.id))
        stableIterations = 0
      else
        stableIterations += 1

      iterations += 1
    end while

    particles.size

end Day20