package bon.jo.phy

import bon.jo.Logger
import bon.jo.html.{DomShell, InDom}
import bon.jo.html.DomShell.{$, ExtendedElement}
import bon.jo.html.cpnt.Download
import bon.jo.phy.ImportExport.{ExportedElement, ModelExport, PointExport}
import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.Purpose.{Delete, Move, What}
import bon.jo.phy.Purpose.What.Point
import bon.jo.phy.view.Shape.Circle
import org.scalajs.dom.{CanvasRenderingContext2D, Event}
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html.{Div, Select}
import EventContext._
import bon.jo.phy.UI.modelInterSel
import bon.jo.phy.view.{DrawerJS, PointDynamicColor, UIParams}
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js.JSON
import scala.xml.{Elem, NodeBuffer}

trait TemplateEvent extends TemplatePhy {
  ui: UI =>

  import params._



  def colorXml(implicit selected: PointDynamicColor[_]): NodeBuffer = {
    <div class="in">
      Couleur :
    </div>
      <div id="selected-color" class="in" style={s"height:1em;background-color:${selected.c.toHex}"}>
      </div>
  }

  def massXml(implicit selected: PointDynamicColor[_]): NodeBuffer = {
    <div class="in">
      masse :
    </div>
      <div id="selected-masse" class="in">
        {selected.m}
      </div>
  }

  def rayonXml(implicit selected: PointDynamicColor[Circle]): NodeBuffer = {
    <div class="in">
      rayon :
    </div>
      <div id="selected-rayon" class="in">
        {selected.shape.r}
      </div>
  }

  def ineractionXml(implicit v: UI.modelInterSel): NodeBuffer = {
    <div id="selected-inter" class="in">
      {v.selected.get.interaction.name}
    </div>
      <div id="selected-inter-type" class="in">
        {v.selected.get._type.toString}
      </div>
  }

  def updateInfoInteraction[A <: UI.modelInterSel]
  (selected: PointInteraction[PointDynamicColorCircle],
   interactionSelection: A)(implicit ev: EventContext[_, _]): Unit = {
    implicit val p: PointDynamicColorCircle = selected.p
    implicit val i: UI.modelInterSel = interactionSelection
    val g = Grid("sele-updat", Grid.withLegend, "Information sur l'interaction")
    g :+ ineractionXml :+ massXml :+ rayonXml :+ colorXml

    if (g.isInDom) {
      g.removeFromView()
    }
    selection.me.appendChild(g.html())
    $[Div]("selected-inter").clkOnce().suscribe {
      _ =>
        val nextInter: Interaction = switchIneraction
          .zipWithIndex
          .map(e =>
            (e._1, if (e._2 != switchIneraction.size - 1) {
              switchIneraction(e._2 + 1)
            } else {
              switchIneraction.head
            }, e._2))
          .find(_._1 == selected.interaction).get._2
        $[Div]("selected-inter").innerText = nextInter.name
        selected.interaction = nextInter
        ev.selectionUpdateUiToCtrl.newValue(interactionSelection)
    }
    $[Div]("selected-inter-type").clkOnce().suscribe { _ =>
      selected._type = !selected._type
      ev.selectionUpdateUiToCtrl.newValue(interactionSelection)
    }
    $[Div]("selected-masse").UserCanUpdate().suscribe(e => {

      selected.p.m = e.toDouble
      ev.selectionUpdateUiToCtrl.newValue(interactionSelection)

    })
    $[Div]("selected-rayon").UserCanUpdate().suscribe(e => {
      selected.p.shape.r = e.toDouble
      ev.selectionUpdateUiToCtrl.newValue(interactionSelection)
    })
    val colrElement = $[Div]("selected-color")
    val cChoose = new ColorChooser(selected.p.c)
    colrElement.clkOnce().suscribe(_ => {
      if (!cChoose.isInDom) {
        colrElement.addChild(cChoose.xml())
        cChoose.init(colrElement)
        cChoose.colorObs.suscribe { c =>
          selected.p.c = c
          ev.selectionUpdateUiToCtrl.newValue(interactionSelection)
        }
      }
    })
  }



  def updateInfoPlanete(selected: PointDynamicColorCircle, planeteSelection: UI.planeteInterSel)(implicit ev: EventContext[_, _]): Unit = {
    implicit val p: PointDynamicColorCircle = selected
    val g = Grid("sele-updat", Grid.withLegend, "Information sur la planète")
    g :+ massXml :+ rayonXml :+ colorXml

    if (g.isInDom) {
      g.removeFromView()
    }


    selection.me.appendChild(g.html())

    $[Div]("selected-masse").UserCanUpdate().suscribe(e => {

      selected.m = e.toDouble
      ev.selectionUpdateUiToCtrl.newValue(planeteSelection)

    })
    $[Div]("selected-rayon").UserCanUpdate().suscribe(e => {
      selected.shape.r = e.toDouble
      ev.selectionUpdateUiToCtrl.newValue(planeteSelection)
    })
    val colrElement = $[Div]("selected-color")
    val cChoose = new ColorChooser(selected.c)
    colrElement.clkOnce().suscribe(_ => {
      if (!cChoose.isInDom) {
        colrElement.addChild(cChoose.xml())
        cChoose.init(colrElement)
        cChoose.colorObs.suscribe { c =>
          selected.c = c
          ev.selectionUpdateUiToCtrl.newValue(planeteSelection)
        }
      }
    })
  }


  def addHtmlAndEvent(implicit ctx: CanvasRenderingContext2D, eventsHandler: EventContext[ModelExport, ExportedElement]): EventContext[ModelExport, ExportedElement] = {


    setUpHtml()

    org.scalajs.dom.document.body.appendChild(root.html())
    ctxMessage.init(root.me)

    ctxMessage.close.suscribe(e => {
      clickBehavhoir = (Purpose.Void,Purpose.Void)
    })
    val unserInput = newElementMassseHtml.me.UserCanUpdate()

    unserInput.suscribe(e => {

      eventsHandler.newElemtsMasse.newValue(e.toDouble)
    })
    eventsHandler.newElemtsMasse.suscribe { v =>
      newElemtsMasse = v
    }


    def tr(p: Boolean): Unit = {
      val tracerString = if (p) "oui" else "non"
      keepTail.me.innerText = s"tracter:$tracerString"
    }

    keepTail.me.clkOnce().suscribe(_ => {
      tracer = !tracer
      eventsHandler.tracer.newValue(tracer)
      val tracerString = if (tracer) "oui" else "non"
      keepTail.me.innerText = s"tracter: $tracerString"
    })
    eventsHandler.tracer.suscribe {
      tr
    }
    createPoint.me.clkOnce().suscribe(_ => {
      clickBehavhoir = (Purpose.Create, Purpose.What.Point)
      ctxMessage.message = "Clicker où mettre la planète"
    })
    createInteraction.me.clkOnce().suscribe(_ => {
      clickBehavhoir = (Purpose.Create, Purpose.What.Interaction)
      ctxMessage.message = "Clicker où mettre l'interaction"
    })
    val tUpUpdate = timeup.me.UserCanUpdate()

    tUpUpdate.suscribe(e => {
      eventsHandler.scaleTime.newValue(e.toDouble)
    })
    sizeFactorInput.me.UserCanUpdate().suscribe(e => {
      eventsHandler.sizeFactor.newValue(e.toDouble)
    })


    pMas.clkOnce().suscribe(_ => {
      //TODO   varier la selction ou tous si pas de sel    faire vai
      //      soleilMasse *= 1.1
      //      eventsHandler.soleilMasse.newValue(soleilMasse)
      newElementMassseHtml.me.innerText = newElemtsMasse.toString
    })
    mMas.clkOnce().suscribe(_ => {
      //      soleilMasse *= 0.9
      //      eventsHandler.soleilMasse.newValue(soleilMasse)
      newElementMassseHtml.me.innerText = newElemtsMasse.toString
    })

    speedUp.me.UserCanUpdate().suscribe(e => {
      eventsHandler.speedFactor.newValue(e.toDouble)

    })
    comeBack.me.clkOnce().suscribe(_ => {
      eventsHandler.pushPull.newValue(false)
    })
    partirBack.me.clkOnce().suscribe(_ => {
      eventsHandler.pushPull.newValue(true)
    })
    effacer.me.clkOnce().suscribe(_ => {
      eventsHandler.clean.newValue(())
    })
    interactionType.me.clkOnce().suscribe(_ => {
      switchIneraction = switchIneraction.tail :+ switchIneraction.head
      interactionType.me.innerText = switchIneraction.head.name
      eventsHandler.ineraction.newValue(switchIneraction.head)
    })
    interactionSelectionOppose.me.clkOnce().suscribe(_ => {
      creationForceOppose = !creationForceOppose
      interactionSelectionOppose.me.innerText = creationForceOppose.name
    })

    frtOptionIn.me.UserCanUpdate().suscribe(e => {
      eventsHandler.frotement.newValue(e.trim.toDouble)
    })
    eventsHandler.ineraction.suscribe(creationInteractionElm = _)

    correctionInput.me.clkOnce().suscribe(_ => {
      correction = !correction
      correctionInput.me.innerText = if (correction) "Oui" else "Non"
      eventsHandler.correction.newValue(correction)
    })

    save.me.clkOnce().suscribe(_ => {
      eventsHandler.saveModel.newValue(())

    })
    replacer.me.clkOnce().suscribe(_ => {
      eventsHandler.replaceAround.newValue(())

    })
    var currentSelectionWhat: Purpose.What = Purpose.Void
    plneteSelection.addEventListener[Event]("change", e => {
      val choiXtring = e.target.asInstanceOf[Select].value
      if (choiXtring != noneChoixString) {
        currentSelectionWhat = Purpose.What.Point
        eventsHandler.userChoice.newValue((Purpose.What.Point, planeteSelectionModel.find(_.id == choiXtring.toInt).get))
      } else {
        currentSelectionWhat = Purpose.Void
      }

    })

    interactionSelection.addEventListener[Event]("change", e => {
      val choiXtring = e.target.asInstanceOf[Select].value
      if (choiXtring != noneChoixString) {
        currentSelectionWhat = Purpose.What.Interaction
        eventsHandler.userChoice.newValue((Purpose.What.Interaction, interSelModel.find(_.id == choiXtring.toInt).get))
      } else {
        currentSelectionWhat = Purpose.Void
      }
    })


    var selectedPurpose: Purpose = Delete
    planeteActionRef.addEventListener[Event]("change", _ => {
      Logger.log(planeteActionRef.value)
      selectedPurpose = Purpose(planeteActionRef.value)
      if (selectedPurpose == Move) {
        clickBehavhoir = (Purpose.Move, currentSelectionWhat)
        ctxMessage.message = "Clicker sur la destination"
      } else {
        clickBehavhoir = (Purpose.Void, currentSelectionWhat)
        ctxMessage.message = "--"
      }

    })
    planeteActionSubmit.addEventListener[Event]("click", _ => {
      eventsHandler.userWant.newValue(selectedPurpose)
    })
    importModel.init(root.me)
    importModel.obsInst.suscribe {
      e =>
        eventsHandler.modelImport.newValue(e)
    }
    importExample.me.clkOnce.suscribe { _ => {
      eventsHandler.modelImport.newValue(ImportExport.getExample)
    }
    }
    eventsHandler.uiParams.suscribe {
      case EmittedValue(value, Source.Ctrl) =>
        display(value)
      case _ =>
    }
    trait ObsViewUpdateText[E] {
      val obs: Obs[E]
      val htmlCp: InDom[Div]

      def text(e: E): String

      def link(): Unit = obs.suscribe(e => {
        htmlCp.me.innerText = text(e)
      })
    }
    object ObsViewUpdateText {
      def apply[E](obsp: Obs[E], htmlCpp: InDom[Div], textF: E => String): ObsViewUpdateText[E] = {
        new ObsViewUpdateText[E]() {
          override val obs: Obs[E] = obsp
          override val htmlCp: InDom[Div] = htmlCpp

          override def text(e: E): String = textF(e)
        }
      }
    }
    var stabiliseV = false
    stabilise.me.clkOnce().suscribe(_ => {
      if (!stabiliseV) {
        stabiliseV = true
        eventsHandler.stabilise.newValue(stabiliseV)
      }

    })

    ObsViewUpdateText.apply(eventsHandler.stabilise, stabilise, (b: Boolean) => {
      stabiliseV = b
      "Stabilise" + (if (b) "Oui" else "Non")
    }).link()
    eventsHandler.stabilise.newValue(false)
    canvas.me.clkOnce().suscribe(e => {
      val rect = canvas.me.getBoundingClientRect()
      val xc = rect.left
      val yc = rect.bottom
      val pc = P(xc, yc)

      val x = e.clientX
      val y = -e.clientY
      val clickIn = pc + P(x, y)


      val other = clickIn / viewPort.scale + viewPort.leftBottm


      implicit val s: Double = sizeFactor
      clickBehavhoir match {
        case (Purpose.Void, Purpose.Void) =>
          Logger.log("Void void")
          eventsHandler.action.newValue(ActionPointDynamicNoParam(new PointDynamicImpl(other), purpose = Purpose.Find, Purpose.All))
        case (Purpose.Move, somthong) =>
          Logger.log("Purpose.Move, somthong")
          somthong match {
            case Purpose.Void =>
            case _ => eventsHandler.action.newValue(ActionPointDynamicNoParam(new PointDynamicColorCircle(newElemtsMasse, other, V(), A(), colorChooser.sommeColr.getOrElse(Color.Red), Circle(20f),-1), Purpose.Move, somthong))
          }
        case (Purpose.Create, what) =>
          what match {
            case Purpose.All | Purpose.Void =>
            case What.Point =>
              eventsHandler.action.newValue(ActionPointDynamicNoParam(
                new PointDynamicColorCircle(newElemtsMasse, other, V(), A(),
                  colorChooser.sommeColr.getOrElse(Color.Red), Circle(20f),PointDynamicImpl.createId), Purpose.Create, what))
            case What.Interaction =>
              eventsHandler.action.newValue(ActionPointDynamicParam(
                new PointDynamicColorCircle(newElemtsMasse, other, V(), A(),
                  colorChooser.sommeColr.getOrElse(Color.Red), Circle(20f),PointDynamicImpl.createId), Purpose.Create, what,
                Some((creationInteractionElm, creationForceOppose))))
          }
        case noWay@_ => Logger.log(s"Différent de Void Void !!!: $noWay")
      }

      //  clickBehavhoir = (Purpose.Void, Purpose.Void)

    })


    eventsHandler.opeationOnElementDone.suscribe {
      case (purpose, Point, selection) =>
        purpose match {

          case Purpose.Move =>
          case Purpose.Delete => removePlaneteFromSelection(selection)
          case Purpose.Create =>
            selection match {
              case _: InteractionSelection[_, _] =>
              case NoneSelection =>
              case selection: PlaneteSelection[_] =>
                planeteSelectionModel = addForChoice(selection.asInstanceOf[UI.planeteInterSel], planeteSelectionModel, plneteSelection)
              case _ =>
            }


          case Purpose.Void =>
          case _ =>
        }
      case (purpose, Purpose.What.Interaction, i) =>
        purpose match {

          case Purpose.Move =>
          case Purpose.Delete => removeInteractionFromSelection(i)
          case Purpose.Create =>
            interSelModel = addForChoice[PointInteraction[PointDynamicColorCircle]](i.asInstanceOf[modelInterSel], interSelModel, interactionSelection)
          case Purpose.Void =>
          case _ =>
        }
      case a@((_, Purpose.All, _) | (_, Purpose.Void, _)) => throw new IllegalStateException(a._1.toString)

    }

    eventsHandler.modelForSave.suscribe {
      e: ModelExport =>
        addDownloadLinlk(JSON.stringify(e))
    }

    eventsHandler.selectionCtrlToUi.suscribe { sel =>

      val a = sel match {
        case inte@InteractionSelectionCust(Some(selected)) =>
          if(interactionSelection.value != inte.selected.get.id.toString){
            interactionSelection.value = inte.selected.get.id.toString

            interactionSelection.options.find(_.value == inte.selected.get.id.toString).foreach(opt=>opt.selected = true)
          }
          currentSelectionWhat = What.Interaction
          updateInfoInteraction(selected, inte.asInstanceOf[UI.modelInterSel])
        case NoneSelection => None
        case pl@PlaneteSelectionCust(Some(selected)) =>
          if(plneteSelection.value != pl.selected.get.id.toString){
            plneteSelection.value = pl.selected.get.id.toString

            plneteSelection.options.find(_.value == pl.selected.get.id.toString).foreach(opt=>opt.selected = true)
          }
          currentSelectionWhat = What.Point
          updateInfoPlanete(selected, pl.asInstanceOf[UI.planeteInterSel])
        case _ => None
      }
    }
    eventsHandler.viewPort.suscribe {
      case EmittedValue(_, Source.UI) =>
      case EmittedValue(vp, Source.Ctrl) =>
        //  ui.clear

        ui.drawScreenLeftBottm()
        ctx.setTransform(1, 0, 0, -1, 0, 0)
        ctx.transform(vp.scale, 0, 0, vp.scale, 0, 0)
        ctx.translate(-vp.leftBottm.x, -vp.leftBottm.y - vp.h.y)


        params.viewPort = vp
        ui.drawScreenLeftBottm()
        eventsHandler.viewPort.newValue(EmittedValue(vp, Source.UI))
      //   ctx.setTransform(vp.value.scale,0,0,-vp.value.scale,vp.value.leftBottm.x,vp.value.leftBottm.y)


    }
    colorChooser.init(root.me)
    eventsHandler
  }


  def addDownloadLinlk(string: String): Unit = {
    val dl = Download("model.json", string)

    if (!dl.isInDom) {
      dl.addTo(this.save.me.parentNode.asInstanceOf[HTMLElement])
      dl.me.classList.add("col")
    } else {
      dl.updateLink(string)
    }

  }


}



