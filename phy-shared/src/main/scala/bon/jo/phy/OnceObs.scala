package bon.jo.phy

class OnceObs[A](private var client: A => Unit = null) extends Obs[A] {
  def clearClients(): Unit = {
    client = null
  }

  override def suscribe(clientp: A => Unit): Unit = client = clientp

  override def newValue(a: A): Unit = {
    if(client != null) client(a) else throw new IllegalStateException("no client for observed value  : "+a)
  }

  def toMany = new StackObs[A](if (client != null) client :: Nil else Nil)
}
