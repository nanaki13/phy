package bon.jo.phy

import bon.jo.phy.Phy.{A, Fact, P, V, _XYT}
import bon.jo.phy.view.{Shape, UIParams}
import org.scalajs.dom.ext.Color
import scala.scalajs.js.JSConverters._
import scala.scalajs.js

object ImportExport {



   @js.native
   trait ShapeCercleExport extends js.Object {
      val r : Double = js.native

   }
   object ShapeCercleExport {
      def apply(r: Double): ShapeCercleExport = {
         scalajs.js.Dynamic.literal(
            r = r,

         ).asInstanceOf[ShapeCercleExport]
      }
   }
   object PDExport{
      def unapply(exp: PDExport) : Option[PointDynamicColorCircle] = {
        Some( PointDynamicColorCircle(exp.m,PointExport.unapply[P](exp.pIni).get
         ,PointExport.unapply[V](exp.vIni).get
            ,PointExport.unapply[A](exp.aIni).get
            ,Color(exp.color),Shape.Circle(exp.shape.r)
         ))
      }

      def apply(p: PointDynamicColorCircle): PDExport = {
         scalajs.js.Dynamic.literal(
            m = p.m,
            pIni =  PointExport(p.p),
            vIni = PointExport(p.v),
            aIni = PointExport(p.a),
            color = p.colorIni.toHex,
            shape = ShapeCercleExport(p.shapeIni.r),
         ).asInstanceOf[PDExport]
      }
   }

   object InterExport{
      def unapply( i : InterExport) : Option[(PointDynamicColorCircle,Interaction)] = {
        Some(PDExport.unapply(i.point).get  , Interaction(i.name).get)
      }

      def apply(p: PointDynamicColorCircle, interaction: Interaction): InterExport = {
         scalajs.js.Dynamic.literal(

            point = PDExport(p),
            name = interaction.name
         ).asInstanceOf[InterExport]
      }
   }
   @js.native
   trait InterExport extends scalajs.js.Object{
      val name : String = js.native
      val point : PDExport = js.native
   }
   @js.native
   trait PDExport extends scalajs.js.Object{
      val m: Double = js.native
      val pIni: PointExport = js.native
      val vIni: PointExport = js.native
      val aIni: PointExport = js.native
      val color: String = js.native
      val shape: ShapeCercleExport = js.native
   }
   @js.native
   trait UIParamsExport extends scalajs.js.Object{

   }
   @js.native
   trait ModelExport extends scalajs.js.Object{
      val points : js.Array[PDExport] = js.native
      val interactions : js.Array[InterExport] = js.native
    //  val UIParams : List[UIParamsExport]
   }
   object ModelExport{
      def apply(points : List[PDExport], interactions : List[InterExport]): ModelExport = {
         scalajs.js.Dynamic.literal(

            points = points.toJSArray,
            interactions = interactions.toJSArray
         ).asInstanceOf[ModelExport]
      }

      def unapply(arg: ModelExport): Option[Model[PointDynamicColorCircle]] = {
     Some(Model[PointDynamicColorCircle] (  arg.points.map(PDExport.unapply).map(_.get).toList,
         arg.interactions.map(InterExport.unapply).map(_.get).toList))
      }
   }
   @js.native
   trait PointExport extends scalajs.js.Object{
      val x : Double = js.native
      val y : Double = js.native
      val t : Double = js.native
   }
   object PointExport{

      def unapply[Other](arg: PointExport)( implicit   fact: Fact[Other]): Option[Other] = {
         Some(fact(arg.x,arg.y))
      }

      def apply( p : _XYT) =  scalajs.js.Dynamic.literal(
      x = p.x,
      y = p.y,
      t = p.t
   ).asInstanceOf[PointExport]
}
   def exporeModel(model: Model[PointDynamicColorCircle]): ModelExport ={
      ModelExport(model.points.map(PDExport.apply),
      model.interactions.map(e => InterExport.apply _ tupled e  ))
   }

   def importModel(model: ModelExport): Model[PointDynamicColorCircle] ={
      val m = ModelExport.unapply(model).get
      Model[PointDynamicColorCircle](m.points,m.interactions)
   }

}
