package bon.jo.phy

import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.InDom
import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.view.DrawerJS._
import bon.jo.phy.view.Shape.Circle
import bon.jo.phy.view.{PointDynamicColor, Shape, UIParams, ViewPort}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.{Div, Select}
import org.scalajs.dom.raw.KeyboardEvent

case class UI()(implicit uIParams: UIParams) extends TemplatePhy with TemplateEvent{

  override val params: UIParams = uIParams
  import params._

  var camera: PointDynamicImpl = PointDynamicImpl(P(uIParams.width / 2, uIParams.height / 2), V(), A(), 0)

  def follow(value: PointDynamicColor[_ <: Shape])(implicit ctx: CanvasRenderingContext2D, eventContext: EventContext): Unit = {
    scalajs.js.special.debugger()
    camera = value
    goTo(camera.p)
  }



  var viewPort: ViewPort = view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale))

  def clear(implicit ctx: CanvasRenderingContext2D): Unit = {

    ctx.fillStyle = maskColor
    ctx.fillRect(viewPort.leftBottm.x, viewPort.leftBottm.y, viewPort.w.x, viewPort.h.y)
//    ctx.strokeStyle = "black"
//    ctx.strokeRect(viewPort.leftBottm.x, viewPort.leftBottm.y, (viewPort.w.x - 5), (viewPort.h.y - 5))
  }


  def initView()(implicit ctx: CanvasRenderingContext2D, eventsHandler: EventContext): EventContext = {
    addHtmlAndEvent
  }

  def goTo(dest: P)(implicit ctx: CanvasRenderingContext2D, eventContext: EventContext, eventsHandler: EventContext): Unit = {

    val current = P(minViewX + (width / 2) / scale, minViewY + (height / 2) / scale)
    val p = dest - current
    ctx.translate(-p.x, -p.y)
    minViewX = minViewX + p.x
    minViewY = minViewY + p.y
    eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))

  }

  def getCtx2D(implicit eventContext: EventContext): CanvasRenderingContext2D = {
    val cn = InDom[Div](<div id="cnt-canvas" tabindex="0"></div>)

    org.scalajs.dom.document.body.appendChild(cn.html())

    cn.me.appendChild(canvas.html())
    implicit val ct: CanvasRenderingContext2D = canvas.me.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    cn.me.addEventListener[KeyboardEvent]("keypress", hadleKeyB)
    ct
  }

  var clickBehavhoir: (Purpose, Purpose.What) = (Purpose.Void, Purpose.Void)





  protected var planeteIndex = List[Int]()
  protected var interactionIndex = List[Int]()

  def addForChoice(i: Int, index: List[Int], select: Select): List[Int] = {
    scalajs.js.special.debugger()
    val newI = index :+ i
    val opt_ = optFromStringValue(i)
    select.appendChild(opt_.html())
    newI
  }


  def removePlaneteFromSelection(): Unit = {
    planeteIndex = planeteIndex.slice(0, planeteIndex.size - 1)
    plneteSelection.clear()
    plneteSelection.appendChild(NoneChoix.html())
    planeteIndex.foreach(e => plneteSelection.appendChild(optFromStringValue(e).html()))
  }

  def removeInteractionFromSelection(): Unit = {
    interactionIndex = interactionIndex.slice(0, interactionIndex.size - 1)
    interactionSelection.clear()
    interactionSelection.appendChild(NoneChoix.html())
    interactionIndex.foreach(e => interactionSelection.appendChild(optFromStringValue(e).html()))
  }

  def dw(implicit uIParams: UIParams): Double = -uIParams.width / 4d

  def dh(implicit uIParams: UIParams): Double = -uIParams.height / 4d

  def hadleKeyB(e: KeyboardEvent)(implicit ctx: CanvasRenderingContext2D, eventContext: EventContext, uIParams: UIParams): Unit = {


    import uIParams._
    val keysMapProcess = Map(
      "+" -> (() => {

        ctx.translate((width / 2) * scale, (height / 2) * scale)
        minViewX -= (width / 2) * scale
        minViewY -= (height / 2) * scale
        scale = scale * 1.1
        minViewX /= 1.1
        minViewY /= 1.1
        ctx.scale(1.1, 1.1)
        ctx.translate(-(width / 2) * scale, -(height / 2) * scale)
        minViewX += (width / 2) * scale
        minViewY += (height / 2) * scale
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))

      }), "-" -> (() => {


        ctx.translate((width / 2) * scale, (height / 2) * scale)
        minViewX -= (width / 2) * scale
        minViewY -= (height / 2) * scale
        scale = scale * 0.9
        minViewX /= 0.9
        minViewY /= 0.9
        minViewX += (width / 2) * scale
        minViewY += (height / 2) * scale
        ctx.scale(0.9, 0.9)
        ctx.translate(-(width / 2) * scale, -(height / 2) * scale)
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))

      }), "d" -> (() => {
        val v = dw
        minViewX += v
        ctx.translate(-v, 0)
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))
      }), "q" -> (() => {
        val v = dw
        minViewX -= v
        ctx.translate(v, 0)
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))
      }), "z" -> (() => {
        val v = dh
        minViewY += v
        ctx.translate(0, -v)
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))
      }), "s" -> (() => {
        val v = dh
        minViewY -= v
        ctx.translate(0, v)
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))
      }), "p" -> (() => {
        sizeFactor *= 1.1
        eventContext.sizeFactor.newValue(sizeFactor)
      }), "m" -> (() => {
        sizeFactor *= 0.9
        eventContext.sizeFactor.newValue(sizeFactor)
      })
    )

    keysMapProcess.get(e.key).map(e => {
      clear
      e()
      true
    }).foreach(_ => {

      clear
    })
    eventContext.viewPort.suscribe(v => {
      CircleDraw.drawFill(Circle(50), v.leftBottm + P(v.w.x / 2, v.h.y / 2))(ctx, 1)
      this.viewPort = v
    })

  }


}




  

