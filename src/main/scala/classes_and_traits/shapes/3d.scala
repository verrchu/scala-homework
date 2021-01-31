package classes_andd_traits.shapes

import scala.math

sealed trait Shape3D extends Located3D with Bounded3D with Movable3D {
  def surfaceArea(): Double
  def volume(): Double
}

case class Point3D(x: Double, y: Double, z: Double) extends Movable3D {
  def move(x: Double, y: Double, z: Double): Point3D =
    Point3D(this.x + x, this.y + y, this.z + z)
}

sealed trait Located3D {
  def location: Point3D
}

// Bound represents shape's minimum enclosing Cuboid
sealed trait Bounded3D {
  def bounds: Cuboid
}

sealed trait Movable3D {
  def move(x: Double, y: Double, z: Double): Movable3D
}

// a Sphere is described by its center pint and its radius
final case class Sphere(center: Point3D, radius: Double) extends Shape3D {
  // a Sphere's center is considered to be its location
  override def location: Point3D = center.copy()

  override def bounds: Cuboid = Cuboid(
    center.copy(),
    x = radius * 2,
    y = radius * 2,
    z = radius * 2
  )

  override def move(x: Double, y: Double, z: Double): Sphere =
    this.copy(center = center.move(x, y, z))

  override def surfaceArea(): Double = 4 * math.Pi * math.pow(radius, 2)
  override def volume(): Double = 4 / 3 * math.Pi * math.pow(radius, 3)
}

// Cuboid is described by its center point and XYZ lengths
final case class Cuboid(center: Point3D, x: Double, y: Double, z: Double)
    extends Shape3D {
  // Cuboid's location is its center point
  override def location: Point3D = center.copy()

  // Cuboid's bounds are equal to itself
  override def bounds: Cuboid = this.copy()

  override def move(x: Double, y: Double, z: Double): Cuboid =
    this.copy(center = center.move(x, y, z))

  override def surfaceArea(): Double = x * y * 2 + x * z * 2 + y * z * 2

  override def volume(): Double = x * y * z
}
