package bon.jo.phy

import bon.jo.phy.Phy.A

case class CaculContext(var point: PointDynamic = null,
                        var pointToSource: A = null,
                        var source: PointDynamic = null,
                        var distCarre: Double = 0,
                        var dist: Double = 0,

                       ) {
  def rSup = Math.sqrt(Math.sqrt(source.m)) * 1000

  def rInf = Math.sqrt(Math.sqrt(source.m))

}
