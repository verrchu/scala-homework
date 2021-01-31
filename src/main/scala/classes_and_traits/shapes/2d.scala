package classes_andd_traits.shapes

import scala.math

sealed trait Shape2D extends Located2D with Bounded2D with Movable2D {
  def area(): Double
}

case class Point2D(x: Double, y: Double)

sealed trait Located2D {
  def location(): Point2D
}

sealed trait Bounded2D {
  def bounds(): Rectangle
}

sealed trait Movable2D {
  def move(x: Double, y: Double): Movable2D
}

// Circle is represented by its center point and its radius
final case class Circle(center: Point2D, radius: Double) extends Shape2D {
  // Circle's location is its center point
  override def location(): Point2D = center.copy()

  override def bounds(): Rectangle = Rectangle(
    center.copy(),
    x = radius * 2,
    y = radius * 2
  )

  override def move(x: Double, y: Double): Circle = {
    val newCenter = Point2D(center.x + x, center.y + y)
    Circle(newCenter, radius)
  }
  override def area(): Double = Math.PI * Math.pow(radius, 2)
}

// Rectangle is represented by its center point and XY lengths
final case class Rectangle(center: Point2D, x: Double, y: Double)
    extends Shape2D {
  // Rectangle's location is its center point
  override def location(): Point2D = center.copy()

  // Rectangle's bounds are equal to itself
  override def bounds(): Rectangle = this.copy()

  override def move(x: Double, y: Double): Rectangle = {
    val newCenter = Point2D(center.x + x, center.y + y)

    Rectangle(newCenter, this.x, this.y)
  }
  override def area(): Double = x * y
}
