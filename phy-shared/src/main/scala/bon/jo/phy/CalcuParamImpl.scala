package bon.jo.phy

case class CalcuParamImpl(
                          var kRessort: Double,
                          var speedFactor: Double,

                          var scleTime: Double,
                          var frt: Double,


                          var interaction: Interaction,
                          var correction: Boolean,
                         var G : Double
                         ) extends CalculParam
