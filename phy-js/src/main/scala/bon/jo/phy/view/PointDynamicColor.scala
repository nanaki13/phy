package bon.jo.phy.view

import bon.jo.phy.Phy.{A, P, PointDynamicImpl, V}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color

abstract class PointDynamicColor[S](m: Double, pIni: P, vIni: V = V(), aIni: A = A(), val c: Color, val shape: S) extends PointDynamicImpl(pIni, vIni, aIni, m) with JS2D {
  def mask(implicit ctx: CanvasRenderingContext2D, sizeFactor : Double)


  def draw(implicit ctx: CanvasRenderingContext2D, drawer: Drawer[CanvasRenderingContext2D, S],sizeFactor : Double): Unit = {
    ctx.fillStyle = c.toHex
    super.drawFill( shape, p)
  }
}
