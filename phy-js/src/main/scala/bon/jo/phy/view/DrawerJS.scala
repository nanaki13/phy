package bon.jo.phy.view

import bon.jo.phy.Phy.P
import org.scalajs.dom.CanvasRenderingContext2D

object DrawerJS {
  val `2PI`: Double = 2 * Math.PI.toFloat

  trait OpsCtw{
    def fill(implicit t: CanvasRenderingContext2D) ={t.fill()}
    def stroke(implicit t: CanvasRenderingContext2D)={t.stroke()}
    def beginPath(implicit t: CanvasRenderingContext2D)={t.beginPath()}
    def arc(a: Shape.Circle, p: P)(implicit t: CanvasRenderingContext2D, sizeFactor : Double)={
      t.arc(p.x, p.y, a.r*sizeFactor, 0, `2PI`)
    }
  }
  implicit object CircleDraw extends
    Drawer[CanvasRenderingContext2D, Shape.Circle] with OpsCtw {
    override def drawFill( a: Shape.Circle, p: P)(implicit t: CanvasRenderingContext2D, sizeFactor : Double): Unit = {
      beginPath
      arc(a,p)
      fill
    }

    override def drawStrike( a: Shape.Circle, p: P)(implicit t : CanvasRenderingContext2D,  sizeFactor: Double): Unit ={
      beginPath
      arc(a,p)
      stroke
    }
  }

}
