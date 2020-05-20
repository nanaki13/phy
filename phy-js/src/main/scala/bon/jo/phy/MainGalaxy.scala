package bon.jo.phy

import bon.jo.Logger
import bon.jo.phy.ImportExport.{ExportedElement, ModelExport}
import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.Purpose.What
import bon.jo.phy.view.Shape.Circle
import bon.jo.phy.view.{PointDynamicColor, UIParams, ViewPort}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color

import scala.scalajs.js.timers.SetIntervalHandle
import scala.util.Random


object MainGalaxy extends App {
  // val a = A(1,1)
  val a0 = 0
  val a_0 = A(0)
  val v_0 = V(70)
  val p_0 = P(600, 350)
  val dt = 1F / 50F
  val delataTFrame = 1F / 20F

  var tmp: Option[Seq[(P, V)]] = None


  private var t = 0

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


      t += 1
    })

    if (pCam != ui.camera.p) {
      ui.goTo(pCam)
    }
    calculateur.model.points.foreach(_.draw)
    calculateur.model.interactions.foreach(_.p.draw)
    ui.drawScreenLeftBottm()
  }

  def emitFromUiParam(UIParams: UIParams)(implicit eventContext: EventContext[_, _]): Unit = {
    eventContext.correction.newValue(UIParams.correction)
    //   eventContext.action.suscribe(calculParam.correction = _)
    eventContext.frotement.newValue(UIParams.frt)

    //eventContext.newElemtsMasse.suscribe(calculParam.newElemtsMasse = _)
    eventContext.scaleTime.newValue(UIParams.scaleTime)

    eventContext.speedFactor.newValue(UIParams.speedFactor)
    eventContext.sizeFactor.newValue(UIParams.sizeFactor)
  }

  def linkParam(calculParam: CalculParam)(implicit ui: UI, eventContext: EventContext[ModelExport, ExportedElement]): Unit = {
    eventContext.correction.suscribe(calculParam.correction = _)
    //   eventContext.action.suscribe(calculParam.correction = _)
    eventContext.frotement.suscribe(calculParam.frt = _)

    //eventContext.newElemtsMasse.suscribe(calculParam.newElemtsMasse = _)
    eventContext.scaleTime.suscribe(calculParam.scaleTime = _)
    calculParam.kRessort = 1
    eventContext.speedFactor.suscribe(calculParam.speedFactor = _)
    eventContext.sizeFactor.suscribe(ui.params.sizeFactor = _)
  }

  def stopCamera(oldCamera: PointDynamic): PointDynamic = PointDynamic(oldCamera.p.copy(), V(), A(), 0)

  def stopCamera(UI: UI): Unit = UI.camera = stopCamera(UI.camera)

  def getPointSelection
  [A <: PointDynamicColor[_], B <: PointInteraction[A]](implicit model: Model[A]): Option[A] = selectedIndexPlanete match {
    case selection: PlaneteSelection[_] => selection.selected.flatMap(e => model.points.find(_.id == e.id))
    case selection: InteractionSelection[_, _] => selection.selected.flatMap(e => model.interactions.find(_.id == e.id).map(_.p))
    case NoneSelection => Logger.log("NoneSelection"); None
    case a@_ => Logger.log(s"$a"); None
  }

  def linkAction(implicit calculateur: Calculateur[PointDynamicColorCircle], ui: UI, uIParams: UIParams, ctx: CanvasRenderingContext2D, eventContext: EventContext[ModelExport, ExportedElement], model: Model[PointDynamicColorCircle]): EventContext[ModelExport, ExportedElement] = {
    implicit val s: Double = uIParams.sizeFactor
    implicit val modelP: Model[PointDynamicColor[_]] = model.asInstanceOf[Model[PointDynamicColor[_]]]
    eventContext.action.suscribe {
      case ActionPointDynamicParam(pdy: PointDynamic, Purpose.Create, Purpose.What.Interaction, Some((int: Interaction, forceType: What.Interaction.Type))) =>
        pdy match {
          case impl: PointDynamicColorCircle =>
            val p = rdPointDynamic
            p.p = impl.p
            p.m = impl.m
            p.c = impl.c
            val inter = PointInteraction(p, int, forceType)
            model.interactions = model.interactions :+ inter
            eventContext.opeationOnElementDone.newValue((Purpose.Create, Purpose.What.Interaction, InteractionSelectionCust(Some(inter))))
          case _ =>
        }
      case ActionPointDynamicNoParam(pdy, Purpose.Create, Purpose.What.Point) =>
        pdy match {
          case impl: PointDynamicColorCircle =>

            val p = rdPointDynamic
            p.p = impl.p
            p.m = impl.m
            p.c = impl.c
            model.points = model.points :+ p
            eventContext.opeationOnElementDone.newValue((Purpose.Create, Purpose.What.Point, PlaneteSelectionCust(Some(p))))
          case _ =>
        }
      case ActionPointDynamicNoParam(pdy: PointDynamic, Purpose.Move, what: What) =>
        what match {
          case Purpose.Void => Logger.log("what  match Void")
          case _ => getPointSelection[PointDynamicColor[_], PointInteraction[PointDynamicColor[_]]].map(e => {
            println(e); e
          }).foreach(a => a.p = pdy.p)
        }
      case ActionPointDynamicNoParam(p: PointDynamic, Purpose.Find, _) =>

        findModelObject(p).map(e => {
          selectedIndexPlanete = e
          e
        }).foreach(eventContext.selectionCtrlToUi.newValue)

      case a@_ => Logger.log(s"not handle action : $a, select by default")
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

    eventContext.pushPull.suscribe(e => if (e) calculateur.push() else calculateur.pull())
    eventContext.viewPort.suscribe(e => viewPort = e.value)
    eventContext.replaceAround.suscribe(_ => {
      ui.clearIfNoKeepTail
      calculateur.replaceAround(model.interactions.head.p.p, viewPort.w.x / 4, 0)
    })
    eventContext.userChoice.suscribe {
      case (Purpose.Void, _) =>
      case (what, i) => ui.clearIfNoKeepTail
        val newSelection: Selection[_] = what match {
          case Purpose.Void | Purpose.All => NoneSelection
          case What.Point =>
            val a = model.points.find(_.id == i.asInstanceOf[PlaneteSelection[_ <: WithId]].selected.get.id)
            PlaneteSelectionCust(Some(new PointDynamicColorCircle(a.get)))
          case What.Interaction =>
            i match {
              case selection: InteractionSelectionCust
              => InteractionSelectionCust.apply(selection.selected
                .flatMap { e =>
                  model.interactions.find(el => e.p.id == el.p.id)
                    .map(finded => finded.copy(p = new PointDynamicColorCircle(finded.p)))
                })
              case NoneSelection => NoneSelection
              case _: PlaneteSelection[_] => NoneSelection
              case _ => NoneSelection
            }
        }
        selectedIndexPlanete = newSelection
        eventContext.selectionCtrlToUi.newValue(newSelection)


    }

    def findModelObject(p: PointDynamic): Option[Selection[_]] = {
      val pt = model.points.find { pl =>
        val rV = p.p - pl.p
        rV.n < pl.shape.r
      } map {
        e =>
          PlaneteSelectionCust(Some(new PointDynamicColorCircle(e)))
      }
      if (pt.isEmpty) {
        model.interactions.find { pl =>
          val rV = p.p - pl.p.p
          rV.n < pl.p.shape.r
        } map {
          e =>
            InteractionSelectionCust(Some(e.copy(new PointDynamicColorCircle(e.p), e.interaction)))
        }
      } else {
        pt
      }

    }

    def modify(mod: PointDynamicColorCircle, from: PointDynamicColorCircle): Unit = {
      mod.m = from.m
      mod.shape.r = from.shape.r
      mod.c = from.c
    }

    eventContext.selectionUpdateUiToCtrl.suscribe {
      case InteractionSelectionCust(Some(selected)) =>

        model.interactions.find(_.p.id == selected.p.id).foreach { mod =>
          modify(mod.p, selected.p)
          if (selected.interaction != mod.interaction) {
            mod.interaction = selected.interaction
          }
        }
      case NoneSelection =>
      case PlaneteSelectionCust(Some(selected)) =>
        model.points.find(_.id == selected.id).foreach { mod =>
          modify(mod, selected)
        }
      case _ =>
    }

    eventContext.clean.suscribe(_ => ui.clear)

    eventContext.userWant.suscribe {
      case Purpose.Move =>

      case Purpose.Delete =>
        selectedIndexPlanete match {
          case a: PlaneteSelectionCust =>
            ctx.fillStyle = uIParams.maskColor
            model.points.find(_.id == a.selected.get.id).get.mask
            ui.camera = stopCamera(ui.camera)
            model.points = model.points.filter(_.id != a.id)
            eventContext.opeationOnElementDone.newValue((Purpose.Delete, What.Point, a.copy(Some(new PointDynamicColorCircle(a.selected.get)))))

          case a: InteractionSelection[_, _] =>
            ctx.fillStyle = uIParams.maskColor
            model.interactions.find(_.p.id == a.id).get.p.mask
            ui.camera = stopCamera(ui.camera)
            model.interactions = model.interactions.filter(_.p.id != a.id)
            eventContext.opeationOnElementDone.newValue((Purpose.Delete, What.Interaction, a.cp()))
          case NoneSelection =>

        }
        selectedIndexPlanete = NoneSelection

      case Purpose.DontFollow
        if selectedIndexPlanete != NoneSelection =>
        stopCamera(ui)
      case Purpose.Follow =>
        getPointSelection[PointDynamicColor[_], PointInteraction[PointDynamicColor[_]]].foreach(e => ui.follow(e))
      case _ =>
    }
    eventContext.saveModel.suscribe { _ =>
      eventContext.modelForSave.newValue {
        ImportExport.exporeModel(model, viewPort, uIParams)
      }
    }

    eventContext.modelImport.suscribe { newModel =>
      val m = newModel

      eventContext.resetModelView.newValue(())

      model.interactions = m.model.interactions
      model.points = m.model.points

      eventContext.viewPort.newValue(EmittedValue(m.viewPort, Source.Ctrl))
      eventContext.uiParams.newValue(EmittedValue(m.uiParams, Source.Ctrl))
      this.emitFromUiParam(m.uiParams)
      model.interactions.foreach(e => {
        eventContext.opeationOnElementDone.newValue(Purpose.Create, Purpose.What.Interaction, InteractionSelectionCust(Some(e)))
      })
      model.points.foreach(e => {
        eventContext.opeationOnElementDone.newValue(Purpose.Create, Purpose.What.Point, PlaneteSelectionCust(Some(e)))
      })

    }
    eventContext
  }

  var selectedIndexPlanete: Selection[_] = NoneSelection
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
    linkAction
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
    new PointDynamicColorCircle(m, P(px, py), V(vx, vy), A(), Color(rdInt(255), rdInt(255), rdInt(255)), Circle(m * 0.01), PointDynamicImpl.createId)
  }


  go()
}




