package bon.jo.phy

import bon.jo.Logger
import bon.jo.Logger.Conf
import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.InDom
import bon.jo.phy.ImportExport.{ExportedElement, ModelExport, PointExport}
import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.Purpose.What
import bon.jo.phy.view.DrawerJS._
import bon.jo.phy.view.Shape.Circle
import bon.jo.phy.view.{DrawerJS, PointDynamicColor, Shape, UIParams, ViewPort}
import org.scalajs.dom.{CanvasRenderingContext2D, raw}
import org.scalajs.dom.html.{Div, Select}
import org.scalajs.dom.raw.KeyboardEvent
import UI._

object UI {
  type modelInterSel = Selection[PointInteraction[PointDynamicColorCircle]]
  type planeteInterSel = Selection[PointDynamicColorCircle]

//  type InterSel = InteractionSelection[PointDynamicColorCircle, PointInteraction[PointDynamicColorCircle]]
//  type Inter = PointInteraction[PointDynamicColorCircle]
}

case class UI()(implicit uIParams: UIParams) extends TemplatePhy with TemplateEvent {
  def clearIfNoKeepTail(implicit canvasRenderingContext2D: CanvasRenderingContext2D): Unit = if (!uIParams.tracer) clear

  def removeAnimable(): Unit = {
    movable.removeFromView()
  }

  def newPositionModel(): Unit = {
    val P(x, y, _): P = positionDybCala.model.points.head.p

    movable.me.style.top = y.toInt.toString + "px"
    movable.me.style.left = x.toInt.toString + "px"

  }


  override val params: UIParams = uIParams

  import params._

  type ptWithId = PointDynamic with WithId
  val positionDybCala = new Calculateur[ptWithId](new Model[ptWithId](Nil, PointInteraction(PointDynamic(viewPort.middle), Interaction.Ressort) :: Nil))
  positionDybCala.model.points = PointDynamic(viewPort.middle - P(100, 100), V(50)) :: Nil


  def uiCalulateur: Calculateur[ptWithId] = positionDybCala

  var camera: PointDynamic = PointDynamic(viewPort.middle, V(), A(), 0)

  def follow(value: PointDynamicColor[_ <: Shape])(implicit ctx: CanvasRenderingContext2D, eventContext: EventContext[ModelExport, ExportedElement]): Unit = {

    camera = value
    goTo(camera.p)
  }


  def clear(implicit ctx: CanvasRenderingContext2D): Unit = {

    ctx.fillStyle = maskColor
    ctx.fillRect(viewPort.leftBottm.x, viewPort.leftBottm.y, viewPort.w.x, viewPort.h.y)
    ctx.strokeStyle = "black"
    ctx.strokeRect(viewPort.leftBottm.x, viewPort.leftBottm.y, viewPort.w.x - 5, viewPort.h.y - 5)
  }


  def initView()(implicit ctx: CanvasRenderingContext2D, eventsHandler: EventContext[ModelExport, ExportedElement]): EventContext[ModelExport, ExportedElement] = {
    addHtmlAndEvent
  }

  def goTo(dest: P)(implicit ctx: CanvasRenderingContext2D, eventContext: EventContext[ModelExport, ExportedElement], UIParams: UIParams): Unit = {
    val current = viewPort.middle
    val p = dest - current
    if (!uIParams.tracer) {
      clear
    }
    ctx.translate(-p.x, -p.y)
    viewPort = viewPort.copy(leftBottm = viewPort.leftBottm + p)
    sendVP

  }

  var ct: CanvasRenderingContext2D = _

  def getCtx2D(implicit eventContext: EventContext[ModelExport, ExportedElement]): CanvasRenderingContext2D = {
    val cn = InDom[Div](<div id="cnt-canvas" tabindex="0"></div>)

    org.scalajs.dom.document.body.appendChild(cn.html())

    cn.me.appendChild(canvas.html())
    implicit val ct: CanvasRenderingContext2D = canvas.me.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    cn.me.addEventListener[KeyboardEvent]("keypress", hadleKeyB)
    this.ct = ct
    ct
  }

  def drawScreenLeftBottm(): Unit = {
    if (!Conf.prod) {
      ct.fillStyle = "black"
      DrawerJS.CircleDraw.drawFill(Circle(10), viewPort.leftBottm + viewPort.h)(ct, 1)
      ct.fillStyle = "red"
      DrawerJS.CircleDraw.drawFill(Circle(10), viewPort.leftBottm + viewPort.w)(ct, 1)
      ct.fillStyle = "blue"
      DrawerJS.CircleDraw.drawFill(Circle(10), viewPort.leftBottm + viewPort.w + viewPort.h)(ct, 1)
      ct.fillStyle = "green"
      DrawerJS.CircleDraw.drawFill(Circle(10), viewPort.leftBottm)(ct, 1)
      ct.fillStyle = "yellow"
      DrawerJS.CircleDraw.drawFill(Circle(4), viewPort.middle)(ct, 1)
    }


    //  ct.fillRect(viewPort.leftBottm.x + 40, viewPort.leftBottm.y + 40, viewPort.w.x - 40, viewPort.h.y - 40)
    //Logger.log(PointExport(viewPort.leftBottm))
  }

  var clickBehavhoir: (Purpose, Purpose.What) = (Purpose.Void, Purpose.Void)


  protected var planeteSelectionModel: Seq[planeteInterSel] = List[planeteInterSel]()
  protected var interSelModel: Seq[modelInterSel] = List[modelInterSel]()

  def addForChoice[A <: WithId](i: Selection[A], model:  Seq[Selection[A]], select: Select): Seq[Selection[A]] = {
    val newI = model :+ i
    val opt_ = optFromStringValue(i.id)
    select.appendChild(opt_.html())
    newI
  }


  def removePlaneteFromSelection(p: Selection[_]): Unit = {
    planeteSelectionModel = planeteSelectionModel.filter(_.id != p.id)
    plneteSelection.clear()
    plneteSelection.appendChild(NoneChoix.html())
    planeteSelectionModel.foreach(e => plneteSelection.appendChild(optFromStringValue(e).html()))
  }

  def removeInteractionFromSelection(p: Selection[_]): Unit = {
    interSelModel = interSelModel.filter(_.id != p.id)
    interactionSelection.clear()
    interactionSelection.appendChild(NoneChoix.html())
    interSelModel.foreach(e => interactionSelection.appendChild(optFromStringValue(e).html()))
  }

  def dw: Double = -viewPort.w.x / 4d

  type ctxGlb = (CanvasRenderingContext2D, EventContext[ModelExport, ExportedElement])

  def zoom(ctx: CanvasRenderingContext2D, eventContext: EventContext[ModelExport, ExportedElement]): Unit = {
    val midel = viewPort.middle
    val depl = (viewPort.leftBottm - midel) * 0.1
    ctx.scale(1.1, 1.1)
    ctx.translate(depl.x, depl.y)
    viewPort = viewPort.copy(scale = viewPort.scale * 1.1
      , leftBottm = (viewPort.leftBottm / 1.1) - depl
      , w = viewPort.w / 1.1, h = viewPort.h / 1.1)
    sendVP(eventContext)
  }

  def unzoom(ctx: CanvasRenderingContext2D, eventContext: EventContext[ModelExport, ExportedElement]): Unit = {
    val midel = viewPort.middle

    val depl = -(viewPort.leftBottm - midel) * 0.1
    ctx.scale(0.9, 0.9)
    ctx.translate(depl.x, depl.y)
    viewPort = viewPort.copy(scale = viewPort.scale * 0.9
      , leftBottm = (viewPort.leftBottm / 0.9) - depl
      , w = viewPort.w / 0.9, h = viewPort.h / 0.9)
    sendVP(eventContext)
  }

  def right(ctx: CanvasRenderingContext2D, eventContext: EventContext[ModelExport, ExportedElement]): Unit = {
    val v = dw
    viewPort = viewPort.copy(leftBottm = viewPort.leftBottm + P(v))
    ctx.translate(-v, 0)
    sendVP(eventContext)
  }

  def left(ctx: CanvasRenderingContext2D, eventContext: EventContext[ModelExport, ExportedElement]): Unit = {

    val v = dw
    viewPort = viewPort.copy(leftBottm = viewPort.leftBottm - P(v))
    ctx.translate(v, 0)
    sendVP(eventContext)
  }

  def down(ctx: CanvasRenderingContext2D, eventContext: EventContext[ModelExport, ExportedElement]): Unit = {
    val v = dh
    viewPort = viewPort.copy(leftBottm = viewPort.leftBottm - P(0, v))
    ctx.translate(0, +v)
    sendVP(eventContext)
  }

  def up(ctx: CanvasRenderingContext2D, eventContext: EventContext[ModelExport, ExportedElement]): Unit = {
    val v = dh
    viewPort = viewPort.copy(leftBottm = viewPort.leftBottm + P(0, v))
    ctx.translate(0, -v)
    sendVP(eventContext)
  }

  def sizeUp(eventContext: EventContext[ModelExport, ExportedElement]): Unit = {
    sizeFactor *= 1.1
    eventContext.sizeFactor.newValue(sizeFactor)
  }

  def sizeDown(eventContext: EventContext[ModelExport, ExportedElement]): Unit = {
    sizeFactor *= 0.9
    eventContext.sizeFactor.newValue(sizeFactor)
  }

  def clkToVoid(): Unit = {
    clickBehavhoir match {
      case (Purpose.Create, What.Point) => createPoint.me.innerText = "Créer planète"
      case (Purpose.Create, What.Interaction) => createInteraction.me.innerText = "Créer interaction"
      case _ =>
    }
    clickBehavhoir = (Purpose.Void, Purpose.Void)

  }

  def dh: Double = -viewPort.h.y / 4d

  val keysMapProcess: Map[String, ctxGlb => Unit] = Map(
    "+" -> zoom _,
    "-" -> unzoom _,
    "d" -> right _,
    "q" -> left _,
    "s" -> down _,
    "z" -> up _,
    "p" -> ((ctx: _root_.org.scalajs.dom.CanvasRenderingContext2D, eventContext: _root_.bon.jo.phy.EventContext[_root_.bon.jo.phy.ImportExport.ModelExport, _root_.bon.jo.phy.ImportExport.ExportedElement]) => sizeUp(eventContext)),
    "m" -> ((ctx: _root_.org.scalajs.dom.CanvasRenderingContext2D, eventContext: _root_.bon.jo.phy.EventContext[_root_.bon.jo.phy.ImportExport.ModelExport, _root_.bon.jo.phy.ImportExport.ExportedElement]) => sizeDown(eventContext)),
    "²" -> ((_: _root_.org.scalajs.dom.CanvasRenderingContext2D, eventContext: _root_.bon.jo.phy.EventContext[_root_.bon.jo.phy.ImportExport.ModelExport, _root_.bon.jo.phy.ImportExport.ExportedElement]) => clkToVoid())
  ).view.mapValues(e => e.tupled).toMap


  def hadleKeyB(e: KeyboardEvent)(implicit ctx: CanvasRenderingContext2D, eventContext: EventContext[ModelExport, ExportedElement]): Unit = {

    if (!keysMapProcess.contains(e.key)) Logger.log(s"${e.key}  ${e.charCode} ${e.keyCode} ${e.metaKey}")
    keysMapProcess.get(e.key).map(e => {
      clear
      e(ctx, eventContext)
      true
    }).foreach(_ => {
      clear
    })


  }

  def sendVP(implicit eventContext: EventContext[_, _]): Unit = eventContext.viewPort.newValue(EmittedValue(viewPort, Source.UI))

}




  

