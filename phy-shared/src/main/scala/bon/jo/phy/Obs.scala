package bon.jo.phy

import scala.collection.mutable

trait Obs[A] {
  def suscribe(client: A => Unit): Unit

  def newValue(a: A): Unit

  def clearClients: Unit
}

object Obs {
  val alls: mutable.Map[String, Obs[_]] = mutable.Map[String, Obs[_]]()

  def once[A](): OnceObs[A] = {
    val ret = new OnceObs[A]() {}
    ret
  }

  def once[A](client: A => Unit): OnceObs[A] = {
    val ret = new OnceObs[A](client) {}
    ret
  }

  def get[A](id: String): Obs[A] = {

    val ret = alls.get(id)
    if (ret.isEmpty) {
      val n = new StackObs[A]()
      alls(id) = n
      n
    } else {
      ret.get.asInstanceOf[Obs[A]]
    }
  }


}