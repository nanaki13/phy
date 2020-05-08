package bon.jo.phy

import bon.jo.Logger
import bon.jo.phy.ImportExport.ModelExport
import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.Purpose.What
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


  def drawFirst()(implicit ctx: CanvasRenderingContext2D, uIParams: UIParams, model: Model[_]): Unit = {
    import uIParams._
    implicit val s: Double = sizeFactor
    model.interactions.foreach { soleilEl =>
      val centreForce: PointDynamicColor[Shape] = soleilEl._1.asInstanceOf[PointDynamicColor[Shape]]
      ctx.fillStyle = uIParams.maskColor
      centreForce.mask
      centreForce.draw
    }
  }

  var keyFrame: Long = 0

  def calculAndDraw(calculParam: CalculParam)(implicit ctx: CanvasRenderingContext2D
                                              , calculateur: Calculateur[PointDynamicColorCircle]
                                              , uiParam: UIParams
                                              , UI: UI
                                              , eventContext: EventContext[ModelExport]): Unit = {
    implicit val s: Double = uiParam.sizeFactor


    val sgn = if (calculParam.scleTime > 0) 1 else -1
    val dtt = sgn * dt
    val intPos = sgn * calculParam.scleTime.toInt
    val pCam = UI.camera.p
    calculParam.dt = dtt
    if (keyFrame < 6001) {
      val b = calculParam.frt
      calculParam.frt = -0.1
      UI.uiCalulateur.model.points.foreach(p => UI.uiCalulateur.calculnextPosition(p, calculParam))
      calculParam.frt = b
      keyFrame += 1
      if (keyFrame == 6000) {
        UI.uiCalulateur.model.interactions = Nil
        UI.removeAnimable()
      } else if (keyFrame == 6000) {
        UI.removeAnimable()
      }
      UI.newPositionModel()
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

    if (pCam != UI.camera.p) {
      UI.goTo(pCam)
    }
    calculateur.model.points.foreach(e => e.draw)
  }


  def linkParam(calculParam: CalculParam)(implicit ui: UI, eventContext: EventContext[ModelExport]) = {
    eventContext.correction.suscribe(calculParam.correction = _)
    //   eventContext.action.suscribe(calculParam.correction = _)
    eventContext.frotement.suscribe(calculParam.frt = _)
    eventContext.ineraction.suscribe(calculParam.interaction = _)
    //  eventContext.masseSolei.suscribe(calculParam.soleilMasse = _)
    eventContext.scaleTime.suscribe(calculParam.scleTime = _)
    calculParam.kRessort = 1
    eventContext.speedFactor.suscribe(calculParam.speedFactor = _)

  }

  def stopCamera(oldCamera: PointDynamic) = PointDynamic(oldCamera.p.copy(), V(), A(), 0)

  def linkAction(calculParam: CalculParam)(implicit calculateur: Calculateur[PointDynamicColorCircle],
                                           ui: UI,
                                           uIParams: UIParams,
                                           ctx: CanvasRenderingContext2D,
                                           eventContext: EventContext[ModelExport], model: Model[PointDynamicColorCircle]) = {
    implicit val s = uIParams.sizeFactor
    eventContext.action.suscribe {
      case ActionPointDynamic(pdy, Purpose.Create, Purpose.What.Interaction) =>
        pdy match {
          case impl: PointDynamicColorCircle => {
            model.interactions = model.interactions :+ (impl, calculParam.interaction)
            eventContext.opeationOnElementDone.newValue((Purpose.Create, Purpose.What.Interaction, model.interactions.size - 1))
          }
          case _ =>
        }
      case ActionPointDynamic(pdy, Purpose.Create, Purpose.What.Point) =>
        pdy match {
          case impl: PointDynamicColorCircle => {
            val p = rdPointDynamic
            p.p = impl.p
            model.points = model.points :+ p
            eventContext.opeationOnElementDone.newValue((Purpose.Create, Purpose.What.Point, model.points.size - 1))
          }
          case _ =>
        }

      case _ =>
    }
    eventContext.actionPoint.suscribe(action => {
      action.what match {
        //  case Purpose.PutSun =>
        case Purpose.PlanetTarget =>
        case Purpose.Move => {
          selectedIndexPlanete = (Purpose.Void, -1)
          ui.goTo(action.p);
          ui.camera = PointDynamic(action.p)
        }
        case _ =>
      }
    })
    eventContext.speedFactor.suscribe(calculateur.applyV)
    eventContext.stabilise.suscribe(calculateur.stab)

    eventContext.pushPull.suscribe(e => if (e) calculateur.push else calculateur.pull)
    eventContext.viewPort.suscribe(viewPort = _)
    eventContext.replaceAround.suscribe(e => {
      ui.clear
      calculateur.replaceAround(model.interactions.head._1.p, viewPort.w.x / 4, 0)
    })
    eventContext.userChoice.suscribe {
      case (Purpose.Void, _) =>
      case e@(what, i) => ui.clear
        what match {
          case Purpose.Void =>
          case What.Point => ui.follow(model.points(i))
          case What.Interaction => ui.camera = stopCamera(ui.camera); ui.goTo(model.interactions(i)._1.p)
        }
        selectedIndexPlanete = e

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
            model.interactions.zipWithIndex.find(_._2 == i).get._1._1.mask
            ui.camera = stopCamera(ui.camera)
            model.interactions = model.interactions.zipWithIndex.filter(_._2 != i).map(_._1)
            eventContext.opeationOnElementDone.newValue((Purpose.Delete, selectedIndexPlanete._1, selectedIndexPlanete._2))
          case (Purpose.Void, _) =>

        }
        selectedIndexPlanete = (Purpose.Void, -1)


      case _ =>
    }
    eventContext.saveModel.suscribe { _ =>
      eventContext.modelForSave.newValue {
        ImportExport.exporeModel(model)
      }
    }

    eventContext.modelImport.suscribe { newModel =>
      val m = newModel.asInstanceOf[Model[PointDynamicColorCircle]]
      model.interactions.foreach(e => {
        eventContext.opeationOnElementDone.newValue(Purpose.Delete,Purpose.What.Interaction,0)
      })


      model.points.foreach(e => {
        eventContext.opeationOnElementDone.newValue(Purpose.Delete,Purpose.What.Point,0)
      })

      model.interactions = m.interactions
      model.points = m.points

      model.interactions.zipWithIndex.foreach(e => {
        eventContext.opeationOnElementDone.newValue(Purpose.Create,Purpose.What.Interaction,e._2)
      })
      model.points.zipWithIndex.foreach(e => {
        eventContext.opeationOnElementDone.newValue(Purpose.Create,Purpose.What.Point,e._2)
      })


    }
    eventContext
  }

  var selectedIndexPlanete: (Purpose.What, Int) = (Purpose.Void, -1)
  var viewPort: ViewPort = _


  def go(): Unit = {

    implicit val uiParams: UIParams = UIParams()
    implicit val ui: UI = UI()
    implicit val eventContext: EventContext[ModelExport] = EventContext[ModelExport]()
    implicit val ctx: CanvasRenderingContext2D = ui.getCtx2D

    implicit val sf: Double = 1

    ui.initView()
    // physical repère
    ctx.transform(1, 0, 0, -1, 1, 1)
    ctx.translate(0, -uiParams.height)

    implicit val m: Model[PointDynamicColorCircle] = Model[PointDynamicColorCircle](Nil, Nil)
    implicit val calculParam: CalculParam = CalculParam(uiParams)

    implicit val calcul: Calculateur[PointDynamicColorCircle] = Calculateur(m)
    linkParam(calculParam)
    linkAction(calculParam)
    eventContext.viewPort.newValue(ui.viewPort)


    // org.scalajs.dom.window.addEventListener[KeyboardEvent]("keyup", hadleKeyB)
    mainAnim = scalajs.js.timers.setInterval(delataTFrame)({
      drawFirst()

      calculAndDraw(calculParam)
      if (calcul.haveToStab) {
        eventContext.stabilise.newValue(false)
      }
      //      if (m.points.length < 20 && initPhase) {
      //        if (Random.nextDouble() > 0.85) {
      //          m.points = m.points :+ rdPointDynamic
      //          eventContext.opeationOnElementDone.newValue(Purpose.Create,Purpose.What.Point, m.points.size -1)
      //        }
      //      }else{
      //        initPhase = false
      //      }

    })


  }

  def rdInt(e: Int) = Random.nextInt(e)


  def rdPointDynamic(implicit uIParams: UIParams): PointDynamicColorCircle = {
    val px = Random.nextInt(uIParams.width)

    val py = Random.nextInt(uIParams.height)
    val vy = Random.nextInt(3) - 1
    val vx = Random.nextInt(3) - 1
    // val vy = 0
    //    val vx = 0
    val m = 700 + 600 * Random.nextDouble()
    new PointDynamicColorCircle(m, P(px, py), V(vx, vy), A(), Color(rdInt(255), rdInt(255), rdInt(255)), Circle(m * 0.01))
  }


  go()
}

case class PointDynamicColorCircle(mIni: Double, pIni: P, vIni: V = V(), aIni: A = A(), colorIni: Color, shapeIni: Circle) extends PointDynamicColor[Circle](mIni, pIni, vIni, aIni, colorIni, shapeIni) {
  override def mask(implicit tx: CanvasRenderingContext2D, sizeFactor: Double): Unit = {

    drawFill[Circle](this.shape * 1.2F, p)
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


