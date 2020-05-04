package bon.jo.phy

import bon.jo.phy.Phy.{A, Fact, ToXY, V, XYT,pfact,vfact,afact}


abstract sealed case class Interaction(name: String) {



  def calculA(implicit ctx: CaculContext, e: CalculParam): A

  def frt(k: Double)(implicit ctx: CaculContext, e: CalculParam): A = new A(ctx.point.v * k)

  def correction(aSans: A)(implicit ctx: CaculContext, e: CalculParam): A = {
    if (e.correction) {
      if(ctx.dist > e.rSup){

        val PS = ctx.dist * ctx.pointToSun
        val r = e.rSup/20 * ctx.pointToSun.rotate90
        val cheminV =  PS + r
         (e.rSup/20  * cheminV.unitary) - 0.1 * ctx.point.v.to[A]

      }else if(ctx.dist < e.rInf){
        aSans
      }else{
        aSans
      }
    } else {
      aSans
    }

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

      (ctx.pointToSun * e.soleilMasse * ctx.point.m) / ctx.dist

    }
  }

  object Faible2 extends Interaction("Forte") {
    override def calculA(implicit ctx: CaculContext, e: CalculParam): A = {
      (ctx.pointToSun * e.soleilMasse * ctx.point.m) / ctx.dist
    }
  }

  object Faible extends Interaction("Faible") {
    override def calculA(implicit ctx: CaculContext, e: CalculParam): A = {
      val p = ctx.point
      (ctx.pointToSun* e.G * e.soleilMasse * ctx.point.m) / ctx.distCarre
    }
  }

  object Ressort extends Interaction("Ressort") {
    override def calculA(implicit ctx: CaculContext, calculParam: CalculParam): A = {
      val p = ctx.point
      (ctx.pointToSun) * ctx.dist * calculParam.kRessort
    }
  }

}