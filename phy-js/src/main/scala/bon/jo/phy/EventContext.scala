package bon.jo.phy

import bon.jo.html.DomShell.Obs
import bon.jo.phy.Phy.P
import EventContext.obs
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
                         stabilise: Obs[Unit] = obs(),
                         action: Obs[ActionPointDynamic[_]] = obs(),
                         actionPoint: Obs[ActionPoint] = obs(),
                         viewPort: Obs[ViewPort] = obs()
                       )

//UI
object EventContext{
  def obs[T](): Obs[T] = Obs.once[T]()
}