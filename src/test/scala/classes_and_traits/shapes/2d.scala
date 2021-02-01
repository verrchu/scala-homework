package classes_andd_traits.shapes

import scala.math

import org.scalatest._
import flatspec._
import matchers._

class Point2DSpec extends AnyFlatSpec with should.Matchers {
  "Point2D" should "move" in {
    val point = Point2D(-1.0, 4.0)
    val movedPoint = point.move(2.0, 3.0)

    movedPoint should be(Point2D(1.0, 7.0))
  }

  it should "report its location" in {
    val point = Point2D(-1.0, 4.0)

    point.location() should be(point)
  }
}

class CircleSpec extends AnyFlatSpec with should.Matchers {
  "Circle" should "report its location" in {
    val circle = Circle(Point2D(0.0, 0.0), 1.0)

    circle.location should be(Point2D(0.0, 0.0))
  }

  it should "report its bounds" in {
    val circle = Circle(Point2D(0.0, 0.0), 1.0)

    val expectedBounds = Rectangle(Point2D(0.0, 0.0), x = 2.0, y = 2.0)
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
  "Rectangle" should "report its location" in {
    val center = Point2D(1.0, 1.0)
    val rect = Rectangle(center, x = 2.0, y = 2.0)

    rect.location should be(center)
  }

  it should "report its bounds" in {
    val rect = Rectangle(Point2D(1.0, 1.0), x = 2.0, y = 2.0)

    rect.bounds should be(rect)
  }

  it should "report its area" in {
    val x = 2.0
    val y = 2.0
    val rect = Rectangle(Point2D(1.0, 1.0), x, y)

    rect.area should be(x * y)
  }

  it should "move" in {
    val rect = Rectangle(Point2D(1.0, 1.0), x = 2.0, y = 2.0)
    val movedRect = rect.move(1.0, 1.0)

    val expectedRect = rect.copy(center = Point2D(2.0, 2.0))
    movedRect should be(expectedRect)
  }
}
