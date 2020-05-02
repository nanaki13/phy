package bon.jo.phy.view

import bon.jo.phy.Phy.P
import org.scalajs.dom.CanvasRenderingContext2D

trait JS2D extends Draw[CanvasRenderingContext2D] {
  override def drawFill[S](a: S, p: P)(implicit t: CanvasRenderingContext2D, drawer: Drawer[CanvasRenderingContext2D, S], sizeFactor : Double): Unit = {
    drawer.drawFill( a, p)
  }
  override def drawStroke[S](a: S, p: P)(implicit t: CanvasRenderingContext2D, drawer: Drawer[CanvasRenderingContext2D, S], sizeFactor : Double): Unit = {
    drawer.drawStrike( a, p)
  }
}
