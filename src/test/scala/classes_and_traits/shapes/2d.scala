package classes_andd_traits.shapes

import scala.math

import org.scalatest._
import flatspec._
import matchers._

class CircleSpec extends AnyFlatSpec with should.Matchers {
  "Circle" should "report its location" in {
    val circle = Circle(Point2D(0.0, 0.0), 1.0)

    circle.location should be(Point2D(0.0, 0.0))
  }

  it should "report its bounds" in {
    val circle = Circle(Point2D(0.0, 0.0), 1.0)

    val expectedBounds = Rectangle(Point2D(-1.0, -1.0), Point2D(1.0, 1.0))
    circle.bounds should be(expectedBounds)
  }

  it should "report its area" in {
    val radius = 2.0
    val circle = Circle(Point2D(0.0, 0.0), radius)

    val expectedArea = math.Pi * math.pow(radius, 2)
    circle.area should be(expectedArea)
  }

  it should "move" in {
    val circle = Circle(Point2D(0.0, 0.0), 1.0)
    val movedCircle = circle.move(1.0, 1.0)

    val expectedCircle = Circle(Point2D(1.0, 1.0), 1.0)
    movedCircle should be(expectedCircle)
  }
}

class RectangleSpec extends AnyFlatSpec with should.Matchers {
  "Rectangle" should "have equivalent definitions" in {
    val a = Point2D(1.0, 1.0)
    val b = Point2D(3.0, 3.0)

    Rectangle(a, b) should be(Rectangle(b, a))
  }

  it should "report its location" in {
    val rect = Rectangle(Point2D(1.0, 1.0), Point2D(3.0, 3.0))

    val expectedLocation = Point2D(2.0, 2.0)
    rect.location should be(expectedLocation)
  }

  it should "report its bounds" in {
    val rect = Rectangle(Point2D(1.0, 1.0), Point2D(3.0, 3.0))

    rect.bounds should be(rect)
  }

  it should "report its area" in {
    val rect = Rectangle(Point2D(1.0, 1.0), Point2D(3.0, 3.0))

    rect.area should be(4.0)
  }

  it should "move" in {
    val rect = Rectangle(Point2D(1.0, 1.0), Point2D(3.0, 3.0))
    val movedRect = rect.move(1.0, 1.0)

    val expectedRect = Rectangle(Point2D(2.0, 2.0), Point2D(4.0, 4.0))
    movedRect should be(expectedRect)
  }
}
