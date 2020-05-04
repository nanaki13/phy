package bon.jo.phy

import bon.jo.phy.Phy.{A, P, V}

trait PointDynamic{
  var m: Double

  var p : P
  var v : V
  var a : A
  def addDt(t : Double): Unit
  def x: Double = p.x

  def y: Double = p.y

  def t: Double = p.t
}
