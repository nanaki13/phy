package bon.jo.phy

import bon.jo.phy.MainGalaxy.Model
import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.view.DrawerJS.`2PI`
import bon.jo.phy.view.PointDynamicColor
import bon.jo.phy.view.Shape.Circle

import scala.util.Random

case class Calculateur(model: Model) {


  def rds: Seq[PointDynamicColor[Circle]] = model.rds

  def replaceAround(from: Double, step: Double, calculParam: CalculParam): Unit = {

    calculParam.forSun(sun => {
      rds.zipWithIndex.foreach(planeteIndex => {
        val (pl, i) = planeteIndex
        val distToSun = i * step + from
        val angle = Random.nextDouble() * `2PI`
        val newP = P(sun.x + scala.math.cos(angle) * distToSun, sun.y + scala.math.sin(angle) * distToSun)
        val newV = V(-Math.sqrt(2)  * scala.math.sin(angle)*( `2PI`*distToSun/2)/calculParam.soleilMasse, Math.sqrt(2) * `2PI`*distToSun /2* scala.math.cos(angle)/calculParam.soleilMasse)

        pl.p = newP
        pl.v = newV
      })
    })
  }

  var haveToStab = false;
  def stab(haveToStabp : Boolean) = haveToStab = haveToStabp;

  def push = rds.foreach(p => p.v = new V(p.a / p.a.n * p.v.n))

  def pull = rds.foreach(p => p.v = -(new V(p.a / p.a.n * p.v.n)) * 1.2f)

  def applyV(factorV: Double) = {
    rds.foreach(p => p.v = p.v * factorV)
  }

  def turnAroundSun(p: P) = {

    rds.foreach(e => {
      val vect: V = new V(p - e.p)
      val vi = ((vect.rotate90) / vect.n) * e.v.n

      e.v = vi
      e.a = A()
      (e.p.copy(), vi.copy())
    })

  }

  implicit val caculContext: CaculContext = CaculContext()

  def calculnext(e: PointDynamic, s: PointDynamic, calculParam: CalculParam): Unit = {

    val soleil = s.p
    val dir = soleil - e.p
    implicit val a: CalculParam = calculParam
    val dist = dir.n

    caculContext.point = e
    caculContext.pointToSun = new A(dir / dist)
    caculContext.distCarre = dir.n2
    caculContext.dist = Math.sqrt(dir.n2)
    e.a = calculParam.interaction.resultatA
    if(haveToStab){
      e.v = Math.sqrt(dist) * (e.a.rotate90 ).to[V]
    }

  }
}
