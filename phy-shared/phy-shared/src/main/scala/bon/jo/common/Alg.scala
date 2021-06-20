package bon.jo.common

trait Alg[A]:
  def +(a: A, b: A): A

  def -(a: A, b: A): A

  def *(a: A, b: A): A

  def /(a: A, b: A): A
object Alg
