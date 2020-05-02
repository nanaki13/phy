package bon.jo.phy

import bon.jo.phy.Phy.{A, P, PointDynamic, V, XYT}

object Phy {
   trait _XYT{
     def x : Double
     def y : Double
     def t : Double
   }
   abstract class XYT[Self <: XYT[_]]( implicit val  fact: Fact[Self]) extends _XYT {



    def +(other : XYT[Self]) : Self = {
      fact.create(x + other.x, y + other.y)
    }
    def -(other : XYT[Self]): Self = {
      fact.create(x - other.x, y - other.y)
    }
  }

  trait Fact[XYZ]{
    def create(x : Double,y : Double): XYZ
  }
  implicit val  pfact: Fact[P] =  P(_,_)
  implicit val  vfact: Fact[V] =  V(_,_)
  implicit val  afact: Fact[A] =  A(_,_)


  case class P(x : Double= 0, y : Double= 0,t : Double = 0) extends XYT[P]
  //case class F(x : Double, y : Double,t : Double) extends XY
  case class V(x : Double =0, y : Double= 0,t : Double = 0) extends XYT[V] with dt[P] {
    override def *(t: Double): P = P(x*t,y*t)
  }
  case class A(x : Double =0, y : Double =0,t : Double = 0) extends XYT[A] with dt[V] {
    override def *(t: Double): V= V(x*t,y*t)
  }

  implicit val  pdfact: Fact[PointDynamic] = (a,b)=>PointDynamic( P(a,b) )
  case class PointDynamic(var p : P,var v : V = V(),var a : A= A()) {
    def addDt(t : Double): Unit ={
      val newV =  a * t + v
      val newP = newV * t + p
      v = newV
      p = newP

    }

     def x: Double = p.x

     def y: Double = p.y

     def t: Double = p.t
  }
}

trait dt[_XY <: XYT[_]]{
  _ : XYT[_] =>


  def *(t : Double):_XY
}



object Test extends App{
 // val a = A(1,1)
  val g = A(0,-10)
  val v_0 = V(100,100)
  val p_0 = P()

  val pd = PointDynamic(p_0,v_0,g)

  for(  _  <- 1 to 1000){
    pd.addDt(0.001F)
    println(pd)
  }
  val t_0 = 0
  val t_1 = 10


}