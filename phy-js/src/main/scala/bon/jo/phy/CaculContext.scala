package bon.jo.phy

import bon.jo.phy.Phy.{A, PointDynamic}

case class CaculContext(var point: PointDynamic = null,
                        var pointToSun: A = null,

                        var distCarre: Double = 0,
                        var dist: Double = 0,

                       ) {

}
