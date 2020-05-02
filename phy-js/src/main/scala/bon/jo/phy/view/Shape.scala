package bon.jo.phy.view

object Shape {

  case class Circle(r: Double) extends Shape {
    def *(d: Double) = Circle(r * d)

  }

  case class Square(r: Double) extends Shape

  case class Rectangle(w: Double, h: Double) extends Shape

}

sealed trait Shape