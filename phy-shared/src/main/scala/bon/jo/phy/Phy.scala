package bon.jo.phy

import bon.jo.phy.Phy.{A, P, PointDynamic, PointDynamicImpl, V, XYT}

object Phy {
   trait _XYT{
     def x : Double
     def y : Double
     def t : Double
   }
   abstract class XYT[Self <: XYT[_]]( implicit val  fact: Fact[Self]) extends _XYT {



     def * (othher : XYT[_]) : Double = {
        this.x+othher.x + this.y*othher.y
     }
     def sameDir (othher : XYT[_]) : Boolean = {
       this * othher > 0
     }
     def rotate(d: Double): Self = fact.create(x*Math.cos(d),y*Math.sin(d))
     def unary_- : Self = {
       fact.create(-y, -x )
     }
     def rotate90: Self = {
       fact.create(-y, x )
     }
    def +(other : XYT[Self]) : Self = {
      fact.create(x + other.x, y + other.y)
    }
     def *(other : Double) : Self = {
       fact.create(x * other, y *other)
     }
     def /(other : Double) : Self = {
       fact.create(x / other, y /other)
     }
    def -(other : XYT[Self]): Self = {
      fact.create(x - other.x, y - other.y)
    }
     def carre: Self = fact.create(x *x, y *y)
     def n : Double = {
       val c = carre
       Math.sqrt(c.x + c.y)
     }
     def n2 : Double = {
       val c = carre
       c.x + c.y
     }
     def n2(other : XYT[Self]): Self ={
        (other - this).carre.asInstanceOf[Self]
     }
     def r2(other : XYT[Self]): Double = {
       val nn = n2(other)
       (nn.x + nn.y)
     }
     def r(other : XYT[Self]): Double ={
       val nn = n2(other)
        Math.sqrt(nn.x + nn.y).toFloat

     }
     def dx(other : XYT[Self]): Double ={
       this.x - other.x
     }
     def dy(other : XYT[Self]): Double ={
       this.y - other.y
     }

  }

  trait Fact[XYZ]{
    def create(x : Double,y : Double): XYZ
  }
  implicit val  pfact: Fact[P] =  P(_,_)
  implicit val  vfact: Fact[V] =  V(_,_)
  implicit val  afact: Fact[A] =  A(_,_)


  case class P(x : Double= 0, y : Double= 0,t : Double = 0) extends XYT[P]{
    def this(p : _XYT){
      this(p.x,p.y,p.t)
    }
  }

  //case class F(x : Double, y : Double,t : Double) extends XY
  case class V(x : Double =0, y : Double= 0,t : Double = 0) extends XYT[V] with dt[P] {
    def this(p : _XYT){
      this(p.x,p.y,p.t)
    }
    override def sum(t: Double): P = P(x*t,y*t)


  }
  case class A(var x : Double =0,var y : Double =0,t : Double = 0) extends XYT[A] with dt[V] {

    def this(p : _XYT){
      this(p.x,p.y,p.t)
    }


    override def sum(t: Double): V= V(x*t,y*t)
  }

  implicit val  pdfact: Fact[PointDynamic] = (a,b)=>PointDynamicImpl( P(a,b),m=1 )
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
  case class PointDynamicImpl(p_ : P,var v : V = V(),var a : A= A(),var m:Double) extends PointDynamic{
    private var _p : P = p_
    def p : P = _p
    def p_=(par : P): Unit = _p = par
    def addDt(t : Double): Unit ={
      val newV =  (a sum t) + v
      val newP = (newV sum t) + p
      v = newV
      _p = newP

    }


  }
}

trait dt[_XY <: XYT[_]]{
  _ : XYT[_] =>
  def sum(t : Double):_XY
}


