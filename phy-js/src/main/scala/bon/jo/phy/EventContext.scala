package bon.jo.phy

import bon.jo.html.DomShell.Obs
import bon.jo.phy.EventContext.obs
import bon.jo.phy.Phy.P
case class EventContext(
                         scaleTime: Obs[Double] = obs(),
                         masseSolei: Obs[Double] = obs(),
                         tracer: Obs[Boolean] = obs(),
                         sizeFactor: Obs[Double] = obs(),
                         soleilMasse: Obs[Double] = obs(),
                         goTo: Obs[P] = obs(),
                         speedFactor: Obs[Double] = obs(),
                         pushPull: Obs[Boolean] = obs(),
                         turnAround: Obs[Unit] = obs(),
                         ineraction: Obs[bon.jo.phy.Interaction] = obs(),
                         frotement: Obs[Double] = obs(),
                         correction: Obs[Boolean] = obs(),
                         replaceAround: Obs[Unit] = obs(),
                         stabilise: Obs[Boolean] = obs(),
                         action: Obs[ActionPointDynamic[_]] = obs(),
                         actionPoint: Obs[ActionPoint] = obs(),
                         viewPort: Obs[ViewPort] = obs(),
                         followDynamicPoint : Obs[PointDynamic] = obs(),
                         planeteAdded: Obs[Int] = obs(),
                         userChoicePlanete : Obs[Int] = obs()
                       ) {


}

//UI
object EventContext{
  def obs[T](): Obs[T] = Obs.once[T]().toMany
}