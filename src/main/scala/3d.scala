import scala.math

sealed trait Shape3D extends Located3D with Bounded3D with Movable3D {
  def surfaceArea(): Double
  def volume(): Double
}

case class Point3D(x: Double, y: Double, z: Double)

sealed trait Located3D {
  def location: Point3D
}

// Bound represents shape's minimum enclosing Cuboid
sealed trait Bounded3D {
  def bound: Cuboid
}

sealed trait Movable3D {
  def move(x: Double, y: Double, z: Double): Movable3D
}

// a Sphere is described by its center pint and its radius
final case class Sphere(center: Point3D, radius: Double) extends Shape3D {
  // a Sphere's center is considered to be its location
  override def location: Point3D = center.copy()

  override def bound: Cuboid = Cuboid(
    Point3D(
      center.x - radius,
      center.y - radius,
      center.z - radius
    ),
    Point3D(
      center.x + radius,
      center.y + radius,
      center.z + radius
    )
  )

  override def move(x: Double, y: Double, z: Double): Sphere = {
    val newCenter = Point3D(center.x + x, center.y + y, center.z + z)
    Sphere(newCenter, radius)
  }

  override def surfaceArea(): Double = 4 * math.Pi * math.pow(radius, 2)
  override def volume(): Double = 4 / 3 * math.Pi * math.pow(radius, 3)
}

// Cuboid is described by its two diagonally opposite points
final case class Cuboid(a: Point3D, b: Point3D) extends Shape3D {
  // Cuboid's location is its center point
  override def location: Point3D = Point3D(
    Math.abs(a.x - b.x) / 2,
    Math.abs(a.y - b.y) / 2,
    Math.abs(a.z - b.z) / 2
  )

  // Cuboid's bound is equal to itself
  override def bound: Cuboid = this.copy()

  override def move(x: Double, y: Double, z: Double): Cuboid = {
    val newA = Point3D(a.x + x, a.y + y, a.z + z)
    val newB = Point3D(b.x + x, b.y + y, b.z + z)

    Cuboid(newA, newB)
  }

  override def surfaceArea(): Double = {
    val xy = math.abs(a.x - b.x) * math.abs(a.y - b.y)
    val xz = math.abs(a.x - b.x) * math.abs(a.z - b.z)
    val yz = math.abs(a.y - b.y) * math.abs(a.z - b.z)

    xy * 2 + xz * 2 + yz * 2
  }

  override def volume(): Double = {
    val xy = math.abs(a.x - b.x)
    val xz = math.abs(a.x - b.x)
    val yz = math.abs(a.y - b.y)

    xy * xz * yz
  }
}
