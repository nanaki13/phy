package bon.jo.phy.view

import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.{PointDynamicImpl, WithId}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color

abstract class PointDynamicColor[S](mIni: Double, pIni: P, vIni: V = V(), aIni: A = A(), var c: Color, var shape: S,id : Int)
  extends PointDynamicImpl(pIni, vIni, aIni, mIni,Nil,id) with JS2D with WithId{


  implicit val  drawer: Drawer[CanvasRenderingContext2D, S]

  def mask(implicit ctx: CanvasRenderingContext2D, sizeFactor : Double)


  def draw(implicit ctx: CanvasRenderingContext2D,sizeFactor : Double): Unit = {
    ctx.fillStyle = c.toHex
    super.drawFill( shape, p)
  }
}
