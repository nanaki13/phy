package bon.jo.phy

import bon.jo.phy.Phy.{A, Fact, P, V, _XYT}
import bon.jo.phy.Purpose.What
import bon.jo.phy.view.{Shape, UIParams, ViewPort}
import org.scalajs.dom.ext.Color

import scala.scalajs.js.JSConverters._
import scala.scalajs.js
import scala.scalajs.js.{Dynamic, JSON}

object ImportExport {


  @js.native
  trait ShapeCercleExport extends js.Object {
    val r: Double = js.native

  }

  object ShapeCercleExport {
    def apply(r: Double): ShapeCercleExport = {
      scalajs.js.Dynamic.literal(
        r = r,

      ).asInstanceOf[ShapeCercleExport]
    }
  }

  object PDExport {
    def unapply(exp: PDExport): Option[PointDynamicColorCircle] = {
      Some(new PointDynamicColorCircle(exp.m, PointExport.unapply[P](exp.pIni).get
        , PointExport.unapply[V](exp.vIni).get
        , PointExport.unapply[A](exp.aIni).get
        , Color(exp.color), Shape.Circle(exp.shape.r)
      ))
    }

    def apply(p: PointDynamicColorCircle): PDExport = {
      scalajs.js.Dynamic.literal(
        m = p.m,
        pIni = PointExport(p.p),
        vIni = PointExport(p.v),
        aIni = PointExport(p.a),
        color = p.colorIni.toHex,
        shape = ShapeCercleExport(p.shapeIni.r),
      ).asInstanceOf[PDExport]
    }
  }

  object InterExport {
    def unapply(i: InterExport): Option[PointInteraction[PointDynamicColorCircle]] = {
      Some(PointInteraction(PDExport.unapply(i.point).get, Interaction(i.name).get, What.Interaction.Type(i._type)))
    }

    def apply(p: PointInteraction[PointDynamicColorCircle]): InterExport = {
      scalajs.js.Dynamic.literal(

        point = PDExport(p.p),
        name = p.interaction.name,
        _type = p._type.name
      ).asInstanceOf[InterExport]
    }
  }

  @js.native
  trait InterExport extends scalajs.js.Object {
    val name: String = js.native
    val _type: String = js.native
    val point: PDExport = js.native
  }

  @js.native
  trait PDExport extends scalajs.js.Object {
    val m: Double = js.native
    val pIni: PointExport = js.native
    val vIni: PointExport = js.native
    val aIni: PointExport = js.native
    val color: String = js.native
    val shape: ShapeCercleExport = js.native
  }

  @js.native
  trait UIParamsExport extends scalajs.js.Object {


    var kRessort: Double = js.native
    var speedFactor: Double = js.native
    var tracerString: String = js.native
    var tracer: Boolean = js.native
    var scaleTime: Double = js.native
    var sizeFactor: Double = js.native
    var correction: Boolean = js.native
    var G: Double = js.native

  }

  object UIParamsExport extends JSCompanion[UIParamsExport, UIParams] {

    import Helpers._

    override def apply(v: UIParams): UIParamsExport = {
      JS(
        kRessort = v.kRessort,
        speedFactor = v.speedFactor,
        tracerString = v.tracerString,
        tracer = v.tracer,
        scaleTime = v.scaleTime,
        sizeFactor = v.sizeFactor,
        correction = v.correction,
        G = v.G
      ).toTarget
    }

    override def unnaply(a: UIParamsExport): Option[UIParams] = {
      Some(UIParams(kRessort = a.kRessort,
        speedFactor = a.speedFactor,
        tracerString = a.tracerString,
        tracer = a.tracer,
        scaleTime = a.scaleTime,
        sizeFactor = a.sizeFactor,
        correction = a.correction,
        G = a.G
      ))
    }
  }

  @js.native
  trait ViewPortJS extends scalajs.js.Object {
    val scale: Double = js.native
    val leftBottm: PointExport = js.native
    val w: PointExport = js.native
    val h: PointExport = js.native
  }

  trait JSCompanion[A <: js.Object, B <: Product] {
    def apply(b: B): A

    def unnaply(a: A): Option[B]

    def JS = scalajs.js.Dynamic.literal

    def cv[A](a: js.Any): A = a.asInstanceOf[A]

    object Helpers {

      implicit class ToTarget(a: js.Any) {
        def toTarget[A]: A = cv[A](a)
      }

    }

  }

  object ViewPortJS extends JSCompanion[ViewPortJS, ViewPort] {

    import Helpers._

    override def apply(v: ViewPort): ViewPortJS = {
      JS(
        scale = v.scale,
        leftBottm = PointExport(v.leftBottm),
        w = PointExport(v.w),
        h = PointExport(v.h),
      ).toTarget
    }

    override def unnaply(a: ViewPortJS): Option[ViewPort] = {
      Some(ViewPort(a.scale,
        PointExport.unapply[P](a.leftBottm).get,
        PointExport.unapply[V](a.w).get,
        PointExport.unapply[V](a.h).get))
    }
  }

  @js.native
  trait ModelExport extends scalajs.js.Object {
    val points: js.Array[PDExport] = js.native
    val interactions: js.Array[InterExport] = js.native
    val viewPort: ViewPortJS = js.native
    val uiParams: UIParamsExport = js.native

  }

  case class ExportedElement(model: Model[PointDynamicColorCircle],
                             viewPort: ViewPort, uiParams: UIParams)

  object ModelExport extends JSCompanion[ModelExport,ExportedElement] {
    import Helpers._
    def apply(jsonString: String): ModelExport = JSON.parse(jsonString).asInstanceOf[ModelExport]

    def apply(model: Model[PointDynamicColorCircle],
              viewPort: ViewPort,
              iuParams: UIParams
             ): ModelExport = {
      apply(ExportedElement(model,viewPort,iuParams))
    }

    override def apply(b: ExportedElement): ModelExport = {
      JS(
       points = b.model.points.map(PDExport(_)).toJSArray,
       interactions = b.model.interactions.map(InterExport(_)).toJSArray,
       viewPort = ViewPortJS(b.viewPort),
       uiParams = UIParamsExport(b.uiParams),
      ).toTarget
    }

    override def unnaply(a: ModelExport): Option[ExportedElement] =  Some(ExportedElement(
      Model[PointDynamicColorCircle](a.points.map(PDExport.unapply).map(_.get).toList,
        a.interactions.map(InterExport.unapply).map(_.get).toList),
      ViewPortJS.unnaply(a.viewPort).get,
      UIParamsExport.unnaply( a.uiParams).get))
  }

  @js.native
  trait PointExport extends scalajs.js.Object {
    val x: Double = js.native
    val y: Double = js.native
    val t: Double = js.native
  }

  object PointExport {

    def unapply[Other](arg: PointExport)(implicit fact: Fact[Other]): Option[Other] = {
      Some(fact(arg.x, arg.y))
    }

    def apply(p: _XYT) = scalajs.js.Dynamic.literal(
      x = p.x,
      y = p.y,
      t = p.t
    ).asInstanceOf[PointExport]
  }

  def exporeModel(model: Model[PointDynamicColorCircle], viewPort: ViewPort,iuParams: UIParams): ModelExport = {
    ModelExport(model, viewPort,iuParams)
  }

  def importModel(model: ModelExport): ExportedElement = {
    ModelExport.unnaply(model).get
  }

  def ex = """{"points":[{"m":4000,"pIni":{"x":3689.2837157739505,"y":409.86699832498226,"t":0},"vIni":{"x":-1.631353910560933,"y":-47.12869008468681,"t":0},"aIni":{"x":-0.9468886882273586,"y":0.07896631673232413,"t":0},"color":"#721e21","shape":{"r":70}},{"m":4000,"pIni":{"x":-1149.0972319559928,"y":-848.3126271104194,"t":0},"vIni":{"x":-28.212171942900753,"y":37.786877337852985,"t":0},"aIni":{"x":0.7880946290701143,"y":0.5307925420568592,"t":0},"color":"#759a5f","shape":{"r":70}},{"m":4000,"pIni":{"x":-91.14423241382572,"y":3022.6668474681724,"t":0},"vIni":{"x":41.523361278801595,"y":22.35140292672289,"t":0},"aIni":{"x":0.4091843159443112,"y":-0.857555865619402,"t":0},"color":"#32969e","shape":{"r":70}},{"m":4000,"pIni":{"x":-1442.4928568208063,"y":1532.3666272523599,"t":0},"vIni":{"x":13.853394093570317,"y":45.07613799745249,"t":0},"aIni":{"x":0.8936159342212127,"y":-0.3229309975449102,"t":0},"color":"#849da6","shape":{"r":100}},{"m":4000,"pIni":{"x":3680.842843291772,"y":936.6164420268177,"t":0},"vIni":{"x":7.725270466690376,"y":-46.51983384420395,"t":0},"aIni":{"x":-0.9437944528219909,"y":-0.10993588081668246,"t":0},"color":"#b1b8d7","shape":{"r":70}}],"interactions":[{"point":{"m":10000000,"pIni":{"x":1049,"y":631.000244140625,"t":0},"vIni":{"x":0,"y":1,"t":0},"aIni":{"x":0,"y":0,"t":0},"color":"#df3289","shape":{"r":50}},"name":"Faible","_type":"Attractive"}],"viewPort":{"scale":0.12216860794583104,"leftBottm":{"x":-9130.467112086035,"y":-4505.116944681272,"t":0},"w":{"x":19644.98114821893,"y":0,"t":0},"h":{"x":0,"y":9822.490574109464,"t":0}},"uiParams":{"kRessort":1,"speedFactor":1,"tracerString":"non","tracer":false,"scaleTime":20,"sizeFactor":1,"correction":true,"G":0.667}}"""

  def getExample: ModelExport = ModelExport(ex)

  def importModelExample(): ExportedElement = {
    ModelExport.unnaply(getExample).get
  }
}
