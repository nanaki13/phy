package bon.jo.phy.view

import bon.jo.phy.view.Shape.Circle
import bon.jo.phy.{Interaction, Param, PointDynamic}

case class UIParams(


                     var soleilMasse: Double = 4000,
                     var frt: Double = 0,
                     var kRessort: Double = 1,
                     var speedFactor: Double = 1d,
                     var tracerString: String = "non",
                     var tracer: Boolean = false,
                     var scleTime: Double = 1d,
                     var sizeFactor: Double = 1d,
                     var scale: Double = 1d,
                     var maskColor: String = "#000F56",
                     var switchIneraction: List[Interaction]= List(Interaction.Faible, Interaction.Forte,Interaction.Ressort),
                     var correction: Boolean = true,
                     private var _sunCircle: Option[PointDynamicColor[Circle]] = None,
                     var minViewX: Double = 0D,
                     var minViewY: Double = 0D,
                     var height: Int = 1200,
                     var width: Int = 2400,
                     var planeteChoice : List[Int] = Nil,
                    var G: Double  = 6.67E-1
                   )  extends Param {
   var interaction: Interaction =Interaction.Faible



}
