package bon.jo.phy

import bon.jo.Logger
import bon.jo.phy.ImportExport.{ExportedElement, ModelExport, PDExport}
import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.Purpose.What
import bon.jo.phy.Purpose.What.Interaction.Attractive
import bon.jo.phy.view.Shape.Circle
import bon.jo.phy.view.{Drawer, DrawerJS, PointDynamicColor, Shape, UIParams, ViewPort}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color

import scala.scalajs.js.JSON
import scala.scalajs.js.timers.SetIntervalHandle
import scala.util.Random


object MainGalaxy extends App {
  // val a = A(1,1)
  val a0 = 0
  val a_0 = A(0, 0)
  val v_0 = V(70, 0)
  val p_0 = P(600, 350)
  val dt = 1F / 50F
  val delataTFrame = 1F / 20F

  var tmp: Option[Seq[(P, V)]] = None

  def draw(e: P, vi: V)(implicit ctx: CanvasRenderingContext2D): Unit = {
    ctx.beginPath()
    ctx.strokeStyle = "black"
    ctx.moveTo(e.x, e.y)
    ctx.lineTo(e.x + vi.x, e.y + vi.y)
    ctx.stroke()
  }


  private var t = 0
  private var initPhase = true

  private var mainAnim: SetIntervalHandle = _

  def stop(): Unit = {
    scalajs.js.timers.clearInterval(mainAnim)
  }


  var keyFrame: Long = 0

  def calculAndDraw(calculParam: CalculParam)(implicit ctx: CanvasRenderingContext2D
                                              , calculateur: Calculateur[PointDynamicColorCircle]
                                              , uiParam: UIParams
                                              , ui: UI
                                              , eventContext: EventContext[ModelExport, ExportedElement]): Unit = {
    implicit val s: Double = uiParam.sizeFactor


    val sgn = if (calculParam.scaleTime > 0) 1 else -1
    val dtt = sgn * dt
    val intPos = sgn * calculParam.scaleTime.toInt
    val pCam = ui.camera.p
    calculParam.dt = dtt
    if (keyFrame < 6001) {
      val b = calculParam.frt
      calculParam.frt = -0.1
      ui.uiCalulateur.model.points.foreach(p => ui.uiCalulateur.calculnextPosition(p, calculParam))
      calculParam.frt = b
      keyFrame += 1
      if (keyFrame == 6000) {
        ui.uiCalulateur.model.interactions = Nil
        ui.removeAnimable()
      } else if (keyFrame == 6000) {
        ui.removeAnimable()
      }
      ui.newPositionModel()
    }

    calculateur.model.interactions.foreach { soleilEl =>
      val centreForce = soleilEl.p
      ctx.fillStyle = uiParam.maskColor
      centreForce.mask
    }
    calculateur.model.points.foreach(e => {
      if (!uiParam.tracer) {
        ctx.fillStyle = uiParam.maskColor
        e.mask
      }


      for (_ <- 0 until intPos) {

        calculateur.calculnextPosition(e, calculParam)

      }


      tmp.foreach(e => e.foreach(draw _ tupled _))
      t += 1
    })

    if (pCam != ui.camera.p) {
      ui.goTo(pCam)
    }
    calculateur.model.points.foreach(_.draw)
    calculateur.model.interactions.foreach(_.p.draw)
    ui.drawScreenLeftBottm()
  }

  def toCalcuParam(UIParams: UIParams)(implicit eventContext: EventContext[_, _]): Unit = {
    eventContext.correction.newValue(UIParams.correction)
    //   eventContext.action.suscribe(calculParam.correction = _)
    eventContext.frotement.newValue(UIParams.frt)
    eventContext.ineraction.newValue(UIParams.interaction)
    //eventContext.newElemtsMasse.suscribe(calculParam.newElemtsMasse = _)
    eventContext.scaleTime.newValue(UIParams.scaleTime)

    eventContext.speedFactor.newValue(UIParams.speedFactor)
    eventContext.sizeFactor.newValue(UIParams.sizeFactor)
  }

  def linkParam(calculParam: CalculParam)(implicit ui: UI, eventContext: EventContext[ModelExport, ExportedElement]) = {
    eventContext.correction.suscribe(calculParam.correction = _)
    //   eventContext.action.suscribe(calculParam.correction = _)
    eventContext.frotement.suscribe(calculParam.frt = _)
    eventContext.ineraction.suscribe(calculParam.interaction = _)
    //eventContext.newElemtsMasse.suscribe(calculParam.newElemtsMasse = _)
    eventContext.scaleTime.suscribe(calculParam.scaleTime = _)
    calculParam.kRessort = 1
    eventContext.speedFactor.suscribe(calculParam.speedFactor = _)
    eventContext.sizeFactor.suscribe(ui.params.sizeFactor = _)
  }

  def stopCamera(oldCamera: PointDynamic): PointDynamic = PointDynamic(oldCamera.p.copy(), V(), A(), 0)

  def stopCamera(UI: UI): Unit = UI.camera = stopCamera(UI.camera)

  def getPointSelection(implicit model: Model[PointDynamicColorCircle]):Option[PointDynamic] = selectedIndexPlanete match {
    case (Purpose.What.Interaction,i) => Option( model.interactions(i).p)
    case (Purpose.What.Point,i) => Option(model.points(i))
    case _ => Option.empty
  }

  def linkAction(calculParam: CalculParam)(implicit calculateur: Calculateur[PointDynamicColorCircle],
                                           ui: UI,
                                           uIParams: UIParams,
                                           ctx: CanvasRenderingContext2D,
                                           eventContext: EventContext[ModelExport, ExportedElement], model: Model[PointDynamicColorCircle]) = {
    implicit val s = uIParams.sizeFactor
    eventContext.action.suscribe {
      case ActionPointDynamicParam(pdy: PointDynamic, Purpose.Create, Purpose.What.Interaction, Some(forceType: What.Interaction.Type)) =>
        pdy match {
          case impl: PointDynamicColorCircle => {
            val p = rdPointDynamic
            p.p = impl.p
            p.m = impl.m
            model.interactions = model.interactions :+ PointInteraction(p, calculParam.interaction, forceType)
            eventContext.opeationOnElementDone.newValue((Purpose.Create, Purpose.What.Interaction, model.interactions.size - 1))
          }
          case _ =>
        }
      case ActionPointDynamicNoParam(pdy, Purpose.Create, Purpose.What.Point) =>
        pdy match {
          case impl: PointDynamicColorCircle => {

            val p = rdPointDynamic
            p.p = impl.p
            p.m = impl.m

            model.points = model.points :+ p
            eventContext.opeationOnElementDone.newValue((Purpose.Create, Purpose.What.Point, model.points.size - 1))
          }
          case _ =>
        }
      case ActionPointDynamicNoParam(pdy: PointDynamic, Purpose.Move, what: What) =>
        what match {
          case Purpose.Void =>
          case _ => getPointSelection.foreach(_.p = pdy.p)
        }
      case a @ _ => Logger.log(s"not handle action : $a")
    }
    //    eventContext.actionPoint.suscribe(action => {
    //      action.what match {
    //        //  case Purpose.PutSun =>
    //        case Purpose.PlanetTarget =>
    //        case Purpose.Move => {
    //          selectedIndexPlanete = (Purpose.Void, -1)
    //          ui.goTo(action.p);
    //          ui.camera = PointDynamic(action.p)
    //        }
    //        case _ =>
    //      }
    //    })
    eventContext.speedFactor.suscribe(calculateur.applyV)
    eventContext.stabilise.suscribe(calculateur.stab)

    eventContext.pushPull.suscribe(e => if (e) calculateur.push else calculateur.pull)
    eventContext.viewPort.suscribe(e => viewPort = e.value)
    eventContext.replaceAround.suscribe(e => {
      ui.clearIfNoKeepTail
      calculateur.replaceAround(model.interactions.head.p.p, viewPort.w.x / 4, 0)
    })
    eventContext.userChoice.suscribe {
      case (Purpose.Void, _) =>
      case e@(what, i) => ui.clearIfNoKeepTail
        eventContext.selectionCtrlToUi.newValue(what match {
          case Purpose.Void => NoneSelection
          case What.Point => val a = model.points(i); ui.follow(a); PlaneteSelectionCust(Some(new PointDynamicColorCircle(a)))
          case What.Interaction => val a = model.interactions(i); ui.camera = stopCamera(ui.camera); ui.goTo(a.p.p); InteractionSelectionCust(Some(a.copy(new PointDynamicColorCircle(a.p), a.interaction)))
        })

        selectedIndexPlanete = e

    }

    def modify(mod: PointDynamicColorCircle, from: PointDynamicColorCircle) = {
      mod.m = from.m
      mod.shape.r = from.shape.r
    }

    eventContext.selectionUpdateUiToCtrl.suscribe {
      case s@InteractionSelectionCust(Some(selected)) => {

        val mod = model.interactions(selectedIndexPlanete._2)
        modify(mod.p, selected.p)
        if (selected.interaction != mod.interaction) {
          mod.interaction = selected.interaction
        }
      }
      case NoneSelection =>
      case PlaneteSelectionCust(Some(selected)) =>
        val mod = model.points(selectedIndexPlanete._2)
        modify(mod, selected)
      case _ =>
    }

    eventContext.clean.suscribe(_ => ui.clear)

    eventContext.userWant.suscribe {
      //case Purpose.PutSun =>
      case Purpose.PlanetTarget =>
      case Purpose.Move =>

      case Purpose.Delete =>
        selectedIndexPlanete match {
          case (Purpose.What.Point, i) =>
            ctx.fillStyle = uIParams.maskColor
            model.points.zipWithIndex.find(_._2 == i).get._1.mask
            ui.camera = stopCamera(ui.camera)
            model.points = model.points.zipWithIndex.filter(_._2 != i).map(_._1)
            eventContext.opeationOnElementDone.newValue((Purpose.Delete, selectedIndexPlanete._1, selectedIndexPlanete._2))

          case (Purpose.What.Interaction, i) =>
            ctx.fillStyle = uIParams.maskColor
            model.interactions.zipWithIndex.find(_._2 == i).get._1.p.mask
            ui.camera = stopCamera(ui.camera)
            model.interactions = model.interactions.zipWithIndex.filter(_._2 != i).map(_._1)
            eventContext.opeationOnElementDone.newValue((Purpose.Delete, selectedIndexPlanete._1, selectedIndexPlanete._2))
          case (Purpose.Void, _) =>

        }
        selectedIndexPlanete = (Purpose.Void, -1)

      case Purpose.DontFollow
        if (selectedIndexPlanete != (Purpose.Void, -1)) =>
        stopCamera(ui)


      case _ =>
    }
    eventContext.saveModel.suscribe { _ =>
      eventContext.modelForSave.newValue {
        ImportExport.exporeModel(model, viewPort, uIParams)
      }
    }

    eventContext.modelImport.suscribe { newModel =>
      val m = newModel

      model.interactions.foreach(e => {
        eventContext.opeationOnElementDone.newValue(Purpose.Delete, Purpose.What.Interaction, 0)
      })


      model.points.foreach(e => {
        eventContext.opeationOnElementDone.newValue(Purpose.Delete, Purpose.What.Point, 0)
      })

      model.interactions = m.model.interactions
      model.points = m.model.points

      eventContext.viewPort.newValue(EmittedValue(m.viewPort, Source.Ctrl))
      eventContext.uiParams.newValue(EmittedValue(m.uiParams, Source.Ctrl))
      this.toCalcuParam(m.uiParams)
      model.interactions.zipWithIndex.foreach(e => {
        eventContext.opeationOnElementDone.newValue(Purpose.Create, Purpose.What.Interaction, e._2)
      })
      model.points.zipWithIndex.foreach(e => {
        eventContext.opeationOnElementDone.newValue(Purpose.Create, Purpose.What.Point, e._2)
      })

    }
    eventContext
  }

  var selectedIndexPlanete: (Purpose.What, Int) = (Purpose.Void, -1)
  var viewPort: ViewPort = _


  def go(): Unit = {

    implicit val uiParams: UIParams = UIParams()
    implicit val ui: UI = UI()
    implicit val eventContext: EventContext[ModelExport, ExportedElement] = EventContext[ModelExport, ExportedElement]()
    implicit val ctx: CanvasRenderingContext2D = ui.getCtx2D

    implicit val sf: Double = 1

    ui.initView()


    implicit val m: Model[PointDynamicColorCircle] = Model[PointDynamicColorCircle](Nil, Nil)
    implicit val calculParam: CalculParam = CalculParam(uiParams)

    implicit val calcul: Calculateur[PointDynamicColorCircle] = Calculateur(m)
    linkParam(calculParam)
    linkAction(calculParam)
    eventContext.viewPort.newValue(EmittedValue(uiParams.viewPort, Source.Ctrl))

    mainAnim = scalajs.js.timers.setInterval(delataTFrame)({


      calculAndDraw(calculParam)
      if (calcul.haveToStab) {
        eventContext.stabilise.newValue(false)
      }


    })


  }

  def rdInt(e: Int) = Random.nextInt(e)


  def rdPointDynamic(implicit uIParams: UIParams): PointDynamicColorCircle = {
    val px = Random.nextInt(Math.round(uIParams.viewPort.w.x.toFloat))

    val py = Random.nextInt(Math.round(uIParams.viewPort.h.y.toFloat))
    val vy = Random.nextInt(3) - 1
    val vx = Random.nextInt(3) - 1
    // val vy = 0
    //    val vx = 0
    val m = 700 + 600 * Random.nextDouble()
    new PointDynamicColorCircle(m, P(px, py), V(vx, vy), A(), Color(rdInt(255), rdInt(255), rdInt(255)), Circle(m * 0.01))
  }


  go()
}

class PointDynamicColorCircle(mIni: Double, pIni: P, vIni: V = V(), aIni: A = A(), val colorIni: Color, val shapeIni: Circle) extends PointDynamicColor[Circle](mIni, pIni, vIni, aIni, colorIni, shapeIni) {
  override def mask(implicit tx: CanvasRenderingContext2D, sizeFactor: Double): Unit = {

    drawFill[Circle](this.shape * 1.2F, p)
  }

  def this(p: PointDynamicColorCircle) {
    this(p.m, p.p.copy(), p.v.copy(), p.a.copy(), p.c, p.shape.copy())
  }

  def toJs = null

  override implicit val drawer: Drawer[CanvasRenderingContext2D, Circle] = DrawerJS.CircleDraw
}

class TextDynamic(m: Double, pIni: P, vIni: V = V(), aIni: A = A(), c: Color, shape: Shape.Text) extends PointDynamicColor[Shape.Text](m, pIni, vIni, aIni, c, shape) {
  override def mask(implicit tx: CanvasRenderingContext2D, sizeFactor: Double): Unit = {

    drawFill[Shape.Text](this.shape, p)
  }

  override implicit val drawer: Drawer[CanvasRenderingContext2D, Shape.Text] = DrawerJS.TextDraw
}


