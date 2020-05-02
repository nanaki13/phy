package bon.jo.phy

import bon.jo.phy.Phy.{A, V}

abstract sealed case class Interaction(name: String) {
  def calculA(implicit ctx: CaculContext, e: CalculParam): A

  def frt(k: Double)(implicit ctx: CaculContext, e: CalculParam): A = new A(ctx.point.v * k)

  def correction(aSans: A)(implicit ctx: CaculContext, e: CalculParam): A = {
    if (e.correction) {
      val pr = aSans * ctx.point.v
      val abs = Math.abs(pr)

      if (pr != 0) {
        val vect: A = ctx.pointToSun * ctx.point.v.n
        val vi = ((vect.rotate90))

        ctx.point.v = new V(vi)
         A()

      } else {
        aSans
      }
    } else {
      aSans
    }

  }

  def calculAAvecFrotment(k: Double)(implicit ctx: CaculContext, e: CalculParam): A = {
    frt(k)
  }

  def resultatA(implicit ctx: CaculContext, e: CalculParam): A = {
    //  if (ctx.dist > e.rInf) {
    val a = correction((calculA))
    if (e.frt != 0d) {
      a + new A(ctx.point.v * e.frt) //  } else {
    } else {
      a
    }
  }


}

object Interaction {

  object Forte extends Interaction("Forte") {
    override def calculA(implicit ctx: CaculContext, e: CalculParam): A = {

      (ctx.pointToSun * e.soleilMasse / ctx.point.m) / ctx.dist

    }
  }

  object Faible2 extends Interaction("Forte") {
    override def calculA(implicit ctx: CaculContext, e: CalculParam): A = {
      (ctx.pointToSun * e.soleilMasse / ctx.point.m) / ctx.dist
    }
  }

  object Faible extends Interaction("Faible") {
    override def calculA(implicit ctx: CaculContext, e: CalculParam): A = {
      val p = ctx.point
      (ctx.pointToSun * e.soleilMasse / ctx.point.m) / ctx.distCarre
    }
  }

  object Ressort extends Interaction("Ressort") {
    override def calculA(implicit ctx: CaculContext, calculParam: CalculParam): A = {
      val p = ctx.point
      if (ctx.pointToSun.sameDir(p.v)) {
        (ctx.pointToSun) * ctx.dist * calculParam.kRessort - new A(ctx.point.v)
      } else {
        -(ctx.pointToSun) * ctx.dist * calculParam.kRessort
      }

    }
  }

}