package bon.jo.phy

import bon.jo.Logger
import bon.jo.html.InDom
import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.cpnt.Download
import bon.jo.phy.ImportExport.ModelExport
import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.Purpose.{Delete, What}
import bon.jo.phy.Purpose.What.Point
import bon.jo.phy.view.Shape.Circle
import org.scalajs.dom.{CanvasRenderingContext2D, Event}
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html.{Div, Select}
import EventContext._
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js.JSON

trait TemplateEvent extends TemplatePhy {
  ui : UI =>
  import params._
  def addHtmlAndEvent(implicit ctx: CanvasRenderingContext2D, eventsHandler: EventContext[ModelExport]): EventContext[ModelExport] = {




    setUpHtml()

    org.scalajs.dom.document.body.appendChild(root.html())
    val unserInput = masseSoleilInput.me.UserCanUpdate()

    unserInput.suscribe(e => {

      eventsHandler.masseSolei.newValue(e.toDouble)
    })


    def tr(p : Boolean): Unit = {val tracerString = if (p) "oui" else "non"
    keepTail.me.innerText = s"tracter:$tracerString"}

    keepTail.me.clkOnce().suscribe(_ => {
      tracer = !tracer
      eventsHandler.tracer.newValue(tracer)
      val tracerString = if (tracer) "oui" else "non"
      keepTail.me.innerText = s"tracter:$tracerString"
    })
    eventsHandler.tracer.suscribe{tr}
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
      soleilMasse *= 1.1
      eventsHandler.soleilMasse.newValue(soleilMasse)
      masseSoleilInput.me.innerText = soleilMasse.toString
    })
    mMas.clkOnce().suscribe(_ => {
      soleilMasse *= 0.9
      eventsHandler.soleilMasse.newValue(soleilMasse)
      masseSoleilInput.me.innerText = soleilMasse.toString
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
      interaction = switchIneraction.head
      interactionType.me.innerText = interaction.name
      eventsHandler.ineraction.newValue(interaction)
    })


    frtOptionIn.me.UserCanUpdate().suscribe(e => {
      eventsHandler.frotement.newValue(e.trim.toDouble)
    })


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
    plneteSelection.addEventListener[Event]("change", e => {
      val choiXtring = e.target.asInstanceOf[Select].value
      if (choiXtring != noneChoixString) {
        eventsHandler.userChoice.newValue((Purpose.What.Point, choiXtring.toInt))
      }

    })

    interactionSelection.addEventListener[Event]("change", e => {
      val choiXtring = e.target.asInstanceOf[Select].value
      if (choiXtring != noneChoixString) {
        eventsHandler.userChoice.newValue((Purpose.What.Interaction, choiXtring.toInt))
      }
    })


    var selectedPurpose: Purpose = Delete
    planeteActionRef.addEventListener[Event]("change", _ => {
      Logger.log(planeteActionRef.value)
      selectedPurpose = Purpose(planeteActionRef.value)
    })
    planeteActionSubmit.addEventListener[Event]("click", _ => {
      eventsHandler.userWant.newValue(selectedPurpose)
    })
    importModel.init(root.me)
    importModel.obsInst.suscribe{
      e =>
        eventsHandler.modelImport.newValue(e)
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
        val other = P(clickIn.x / scale + minViewX, clickIn.y / scale + minViewY)


        implicit val s: Double = sizeFactor
        clickBehavhoir match {
          case (Purpose.Void, Purpose.Void) =>
          case (purpose, what) => eventsHandler.action.newValue(
            ActionPointDynamic(new PointDynamicColorCircle(soleilMasse, other, V(), A(), Color("#FF5F1C"), Circle(20f)), purpose, what))

        }
        clickBehavhoir._2 match {
          case Purpose.Void =>
          case What.Point => createPoint.me.innerText = "Créer planète"
          case What.Interaction => createInteraction.me.innerText = "Créer interaction"
        }
        clickBehavhoir = (Purpose.Void, Purpose.Void)
      }
    })


    eventsHandler.opeationOnElementDone.suscribe {
      case (purpose, Point, i) =>
        purpose match {
          case Purpose.PlanetTarget =>
          case Purpose.Move =>
          case Purpose.Delete => removePlaneteFromSelection()
          case Purpose.Create =>

            planeteIndex = addForChoice(i, planeteIndex, plneteSelection)

          case Purpose.Void =>
          case _ =>
        }
      case (purpose, Purpose.What.Interaction, i) =>
        purpose match {
          case Purpose.PlanetTarget =>
          case Purpose.Move =>
          case Purpose.Delete => removeInteractionFromSelection()
          case Purpose.Create =>
            interactionIndex = addForChoice(i, interactionIndex, interactionSelection)
          case Purpose.Void =>
          case _ =>
        }
      case a@(_, Purpose.Void, _) => throw new IllegalStateException(a._1.toString)
    }

    eventsHandler.modelForSave.suscribe{
      e : ModelExport =>

        addDownloadLinlk(JSON.stringify(e))
    }


    eventsHandler
  }

  def addDownloadLinlk(string: String) ={
    val dl = Download("model.json",string)

    if(!dl.isInDom){
      dl.addTo(this.save.me.parentNode.asInstanceOf[HTMLElement])
      dl.me.classList.add("col")
    }else{
      dl.updateLink(string)
    }

  }
}
