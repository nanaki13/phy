package bon.jo.phy

import bon.jo.phy.Phy.PointDynamic
import bon.jo.phy.view.PointDynamicColor
import bon.jo.phy.view.Shape.Circle

case class CalcuParamImpl(var soleilMasse: Double,
                          var kRessort: Double,
                          var speedFactor: Double,

                          var scleTime: Double,
                          var frt: Double,


                          var interaction: Interaction,
                          var correction: Boolean,
                          var sun: Option[PointDynamic]) extends CalculParam
