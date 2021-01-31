package classes_andd_traits.shapes

import scala.math

import org.scalatest._
import flatspec._
import matchers._

class Point3DSpec extends AnyFlatSpec with should.Matchers {
  "Point" should "move" in {
    val point = Point3D(-1.0, 4.0, 2.0)
    val movedPoint = point.move(2.0, 3.0, -6.0)

    movedPoint should be(Point3D(1.0, 7.0, -4.0))
  }
}

class SphereSpec extends AnyFlatSpec with should.Matchers {
  "Sphere" should "report its location" in {
    val sphere = Sphere(Point3D(0.0, 0.0, 1.0), 1.0)

    sphere.location should be(Point3D(0.0, 0.0, 1.0))
  }

  it should "report its bounds" in {
    val sphere = Sphere(Point3D(0.0, 0.0, 1.0), 1.0)

    val expectedBounds =
      Cuboid(Point3D(0.0, 0.0, 1.0), x = 2.0, y = 2.0, z = 2.0)
    sphere.bounds should be(expectedBounds)
  }

  it should "report its surface area" in {
    val radius = 2.0
    val sphere = Sphere(Point3D(0.0, 0.0, 0.0), radius)

    val expectedArea = 4 * math.Pi * math.pow(radius, 2)
    sphere.surfaceArea should be(expectedArea)
  }

  it should "report its volume" in {
    val radius = 2.0
    val sphere = Sphere(Point3D(0.0, 0.0, 0.0), radius)

    val expectedVolume = 4 / 3 * math.Pi * math.pow(radius, 3)
    sphere.volume should be(expectedVolume)
  }

  it should "move" in {
    val sphere = Sphere(Point3D(-4.0, 3.0, 0.0), 1.0)
    val movedSphere = sphere.move(1.0, -1.0, 3.0)

    val expectedSphere = Sphere(Point3D(-3.0, 2.0, 3.0), 1.0)
    movedSphere should be(expectedSphere)
  }
}

class CuboidSpec extends AnyFlatSpec with should.Matchers {
  "Cuboid" should "report its location" in {
    val center = Point3D(1.0, 1.0, 1.0)
    val cube = Cuboid(center, x = 2.0, y = 2.0, z = 2.0)

    cube.location should be(center)
  }

  it should "report its bounds" in {
    val center = Point3D(1.0, 1.0, 1.0)
    val cube = Cuboid(center, x = 2.0, y = 2.0, z = 2.0)

    cube.bounds should be(cube)
  }

  it should "report its surface area" in {
    val x = 2.0
    val y = 3.0
    val z = 4.0
    val center = Point3D(1.0, 1.0, 1.0)
    val cube = Cuboid(center, x, y, z)

    cube.surfaceArea should be(2 * x * y + 2 * x * z + 2 * y * z)
  }

  it should "report its volume" in {
    val x = 2.0
    val y = 3.0
    val z = 4.0
    val center = Point3D(1.0, 1.0, 1.0)
    val cube = Cuboid(center, x, y, z)

    cube.volume should be(x * y * z)
  }

  it should "move" in {
    val center = Point3D(1.0, 1.0, 1.0)
    val cube = Cuboid(center, x = 2.0, y = 2.0, z = 2.0)
    val movedCube = cube.move(1.0, -1.0, 0.0)

    val expectedCube = cube.copy(center = Point3D(2.0, 0.0, 1.0))
    movedCube should be(expectedCube)
  }
}
