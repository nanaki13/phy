package bon.jo.common.log

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

object Macro :
    def inspectCode[T](x: Expr[T])(using Type[T], Quotes): Expr[String] =
        
        '{ s"Value of " + ${Expr(x.show)} + " is " + $x }
        
    inline def inspect[T](inline x: T): String = ${ inspectCode('x) }

