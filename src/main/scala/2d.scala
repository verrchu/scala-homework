import scala.math

sealed trait Shape2D extends Located2D with Bounded2D with Movable2D {
  def area(): Double
}

case class Point2D(x: Double, y: Double)

sealed trait Located2D {
  def location(): Point2D
}

sealed trait Bounded2D {
  def bound(): Rectangle
}

sealed trait Movable2D {
  def move(x: Double, y: Double): Movable2D
}

// Circle is represented by its center point and its radius
final case class Circle(center: Point2D, radius: Double) extends Shape2D {
  // Circle's location is its center point
  override def location(): Point2D = center.copy()

  override def bound(): Rectangle = Rectangle(
    Point2D(
      center.x - radius,
      center.y - radius
    ),
    Point2D(
      center.x + radius,
      center.y + radius
    )
  )

  override def move(x: Double, y: Double): Circle = {
    val newCenter = Point2D(center.x + x, center.y + y)
    Circle(newCenter, radius)
  }
  override def area(): Double = Math.PI * Math.pow(radius, 2)
}

// Rectangle is represented by its two diagonally opposite points
final case class Rectangle(a: Point2D, b: Point2D) extends Shape2D {
  // Rectangle's location is its center point
  override def location(): Point2D = Point2D(
    math.abs(a.x - b.x),
    math.abs(a.y - b.y)
  )

  // Rectangle's bound is equal to itself
  override def bound(): Rectangle = this.copy()

  override def move(x: Double, y: Double): Rectangle = {
    val newA = Point2D(a.x + x, a.y + y)
    val newB = Point2D(b.x + x, b.y + y)

    Rectangle(newA, newB)
  }
  override def area(): Double = Math.abs(a.x - b.x) * Math.abs(a.y - b.y)
}
