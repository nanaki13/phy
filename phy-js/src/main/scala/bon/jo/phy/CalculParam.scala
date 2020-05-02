package bon.jo.phy

import bon.jo.phy.Phy.PointDynamic
import bon.jo.phy.view.PointDynamicColor
import bon.jo.phy.view.Shape.Circle

trait CalculParam {
  def forSun(doThat :  PointDynamic => Unit): Unit = sun foreach doThat


  var soleilMasse: Double
  var kRessort: Double
  var speedFactor: Double

  var scleTime: Double
  var frt: Double

  def rSup = Math.sqrt(Math.sqrt(soleilMasse)) * 1000

  def rInf = Math.sqrt(Math.sqrt(soleilMasse)) / 10

  var interaction: Interaction
  var correction: Boolean
  var sun: Option[PointDynamic]
}

object CalculParam {
  def apply(e: CalculParam):CalculParam = {
    CalcuParamImpl(
      e.soleilMasse,
      e.kRessort,
      e.speedFactor,
      e.scleTime,
      e.frt,
      e.interaction,
      e.correction,
      e.sun: Option[PointDynamic]
    )
  }
}