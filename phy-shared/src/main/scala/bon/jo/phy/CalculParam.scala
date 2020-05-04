package bon.jo.phy

trait CalculParam extends Param {
  var dt = 0d

}

trait Param {
  //def forSun(doThat :  PointDynamic => Unit): Unit = sun foreach doThat

  var G :Double
  var soleilMasse: Double
  var kRessort: Double
  var speedFactor: Double

  var scleTime: Double
  var frt: Double

  def rSup = Math.sqrt(Math.sqrt(soleilMasse)) * 1000

  def rInf = Math.sqrt(soleilMasse)

  var interaction: Interaction
  var correction: Boolean
  var sun: Option[PointDynamic]
}
object CalculParam {
  def apply(e: Param):CalculParam = {
    CalcuParamImpl(
      e.soleilMasse,
      e.kRessort,
      e.speedFactor,
      e.scleTime,
      e.frt,
      e.interaction,
      e.correction,
      e.sun: Option[PointDynamic],e.G
    )
  }
}