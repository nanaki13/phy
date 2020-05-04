package bon.jo.phy

import bon.jo.phy.Phy.{A, P, V}

 case class PointDynamicImpl(p_ : P,var v : V = V(),var a : A= A(),var m:Double = 1
                           ,var forces : List[A] = Nil
                           ) extends PointDynamic{
  private var _p : P = p_
  def p : P = _p
  def p_=(par : P): Unit = _p = par



}
