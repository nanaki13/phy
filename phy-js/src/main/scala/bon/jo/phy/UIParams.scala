package bon.jo.phy

import bon.jo.phy.view.PointDynamicColor
import bon.jo.phy.view.Shape.Circle

case class UIParams(


                     var soleilMasse: Double = 80000,
                     var frt: Double = 0,
                     var kRessort: Double = 1,
                     var speedFactor: Double = 1d,
                     var tracerString: String = "non",
                     var tracer: Boolean = false,
                     var scleTime: Double = 1d,
                     var sizeFactor: Double = 1d,
                     var scale: Double = 1d,
                     var maskColor: String = "#000F56",
                     var switchIneraction: (Interaction, Interaction) = (Interaction.Faible, Interaction.Forte),

                     var correction: Boolean = false,
                     var sunCircle: Option[PointDynamicColor[Circle]] = None,
                     var minViewX: Double = 0D,
                     var minViewY: Double = 0D,
                     var height: Int = 1200,
                     var width: Int = 2400

                   ) extends CalculParam{
  override var interaction: Interaction = _
  override var sun: Option[Phy.PointDynamic] = sunCircle
}
