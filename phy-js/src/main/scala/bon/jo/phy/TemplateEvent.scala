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
import bon.jo.phy.view.{DrawerJS, PointDynamicColor, UIParams}
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js.JSON
import scala.xml.Elem

trait TemplateEvent extends TemplatePhy {
  ui: UI =>

  import params._


  type InterSel = InteractionSelection[PointDynamicColorCircle, PointInteraction[PointDynamicColorCircle]]
  type Inter = PointInteraction[PointDynamicColorCircle]

  def colorXml(implicit selected: PointDynamicColor[_]) = {
    <div class="in">
      Couleur :
    </div>
      <div id="selected-color" class="in" style={s"height:1em;background-color:${selected.c.toHex}"}>
      </div>
  }

  def massXml(implicit selected: PointDynamicColor[_]) = {
    <div class="in">
      masse :
    </div>
      <div id="selected-masse" class="in">
        {selected.m}
      </div>
  }

  def rayonXml(implicit selected: PointDynamicColor[Circle]) = {
    <div class="in">
      rayon :
    </div>
      <div id="selected-rayon" class="in">
        {selected.shape.r}
      </div>
  }

  def ineractionXml(implicit v: Inter) = {
    <div id="selected-inter" class="in">
      {v.interaction.name}
    </div>
      <div id="selected-inter-type" class="in">
        {v._type.toString}
      </div>
  }

  def updateInfoInteraction[A <: InterSel]
  (selected: Inter,
   interactionSelection: A)(implicit ev: EventContext[_, _]): Unit = {
    implicit val p = selected.p
    implicit val i = selected
    val g = Grid("sele-updat", Grid.withLegend, "Information sur l'interaction")
    g :+ ineractionXml :+ massXml :+ rayonXml :+ colorXml

    if (g.isInDom) {
      g.removeFromView()
    }
    selection.me.appendChild(g.html())
    $[Div]("selected-inter").clkOnce().suscribe {
      e =>
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
    $[Div]("selected-inter-type").clkOnce().suscribe { e =>
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
    val cChoose = new ColorChooser
    colrElement.clkOnce().suscribe(e => {
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

  type PlanSel = PlaneteSelection[PointDynamicColorCircle]

  def updateInfoPlanete(selected: PointDynamicColorCircle, planeteSelection: PlanSel)(implicit ev: EventContext[_, _]) = {
    implicit val p = selected
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
    val cChoose = new ColorChooser
    colrElement.clkOnce().suscribe(e => {
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
      keepTail.me.innerText = s"tracter:$tracerString"
    })
    eventsHandler.tracer.suscribe {
      tr
    }
    createPoint.me.clkOnce().suscribe(_ => {
      clickBehavhoir = (Purpose.Create, Purpose.What.Point)
      createPoint.me.innerText = "Clicker où mettre"
    })
    createInteraction.me.clkOnce().suscribe(_ => {
      clickBehavhoir = (Purpose.Create, Purpose.What.Interaction)
      createInteraction.me.innerText = "Clicker où mettre"
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
        eventsHandler.userChoice.newValue((Purpose.What.Point, choiXtring.toInt))
      } else {
        currentSelectionWhat = Purpose.Void
      }

    })

    interactionSelection.addEventListener[Event]("change", e => {
      val choiXtring = e.target.asInstanceOf[Select].value
      if (choiXtring != noneChoixString) {
        currentSelectionWhat = Purpose.What.Interaction
        eventsHandler.userChoice.newValue((Purpose.What.Interaction, choiXtring.toInt))
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
        planeteActionSubmit.innerText = "Clicker sur la destination"
      } else {
        clickBehavhoir = (Purpose.Void, currentSelectionWhat)
        planeteActionSubmit.innerText = "Appliquer"
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
      if (clickBehavhoir != (Purpose.Void, Purpose.Void)) {
        val rect = canvas.me.getBoundingClientRect()
        val xc = rect.left
        val yc = rect.bottom
        val pc = P(xc, yc)

        val x = e.clientX
        val y = -e.clientY
        val clickIn = pc + P(x, y)
        val other = clickIn / viewPort.scale + viewPort.leftBottm

        Logger.log(PointExport(other))
        Logger.log(PointExport(viewPort.leftBottm))
        implicit val s: Double = sizeFactor
        clickBehavhoir match {
          case (Purpose.Void, Purpose.Void) =>
          case (Purpose.Move, somthong) =>
            somthong match {
              case Purpose.Void =>
              case _ => eventsHandler.action.newValue(ActionPointDynamicNoParam(new PointDynamicColorCircle(newElemtsMasse, other, V(), A(), colorChooser.sommeColr.getOrElse(Color.Red), Circle(20f)), Purpose.Move, somthong))
            }
          case (Purpose.Create, what) =>
            what match {
              case Purpose.Void =>
              case What.Point => eventsHandler.action.newValue(ActionPointDynamicNoParam(new PointDynamicColorCircle(newElemtsMasse, other, V(), A(), colorChooser.sommeColr.getOrElse(Color.Red), Circle(20f)), Purpose.Create, what))
              case What.Interaction => eventsHandler.action.newValue(ActionPointDynamicParam(new PointDynamicColorCircle(newElemtsMasse, other, V(), A(), colorChooser.sommeColr.getOrElse(Color.Red), Circle(20f)), Purpose.Create, what, Some((creationInteractionElm,creationForceOppose))))
            }
          case noWay@_ => Logger.log(s"Différent de Void Void !!!: $noWay")
        }
        clickBehavhoir match {
          case (Purpose.Create, What.Point) => createPoint.me.innerText = "Créer planète"
          case (Purpose.Create, What.Interaction) => createInteraction.me.innerText = "Créer interaction"
          case _ =>
        }
        //  clickBehavhoir = (Purpose.Void, Purpose.Void)
      }
    })


    eventsHandler.opeationOnElementDone.suscribe {
      case (purpose, Point, i) =>
        purpose match {

          case Purpose.Move =>
          case Purpose.Delete => removePlaneteFromSelection()
          case Purpose.Create =>

            planeteIndex = addForChoice(i, planeteIndex, plneteSelection)

          case Purpose.Void =>
          case _ =>
        }
      case (purpose, Purpose.What.Interaction, i) =>
        purpose match {

          case Purpose.Move =>
          case Purpose.Delete => removeInteractionFromSelection()
          case Purpose.Create =>
            interactionIndex = addForChoice(i, interactionIndex, interactionSelection)
          case Purpose.Void =>
          case _ =>
        }
      case a@(_, Purpose.Void, _) => throw new IllegalStateException(a._1.toString)
    }

    eventsHandler.modelForSave.suscribe {
      e: ModelExport =>
        addDownloadLinlk(JSON.stringify(e))
    }

    eventsHandler.selectionCtrlToUi.suscribe { select =>

      val a = select match {
        case inte@InteractionSelectionCust(Some(selected)) =>
          updateInfoInteraction[InteractionSelectionCust](selected, inte)
        case NoneSelection => None
        case pl@PlaneteSelectionCust(Some(selected)) =>
          updateInfoPlanete(selected, pl)
        case _ => None
      }


    }
    eventsHandler.viewPort.suscribe {
      case EmittedValue(vp, Source.UI) =>
      case EmittedValue(vp, Source.Ctrl) => {
        //  ui.clear

        ui.drawScreenLeftBottm()
        ctx.setTransform(1, 0, 0, -1, 0, 0)
        ctx.transform(vp.scale, 0, 0, vp.scale, 0, 0)
        ctx.translate(-vp.leftBottm.x, (-vp.leftBottm.y - vp.h.y))


        params.viewPort = vp
        ui.drawScreenLeftBottm()
        eventsHandler.viewPort.newValue(EmittedValue(vp, Source.UI))
        //   ctx.setTransform(vp.value.scale,0,0,-vp.value.scale,vp.value.leftBottm.x,vp.value.leftBottm.y)
      }


    }
    colorChooser.init(root.me)
    eventsHandler
  }


  def addDownloadLinlk(string: String) = {
    val dl = Download("model.json", string)

    if (!dl.isInDom) {
      dl.addTo(this.save.me.parentNode.asInstanceOf[HTMLElement])
      dl.me.classList.add("col")
    } else {
      dl.updateLink(string)
    }

  }


}



