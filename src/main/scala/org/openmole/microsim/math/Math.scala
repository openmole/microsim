package org.openmole.microsim.math

import scala.collection.mutable.ArrayBuffer

object Math {

  def cumsum[T:Numeric](a:Seq[T]): Seq[T] = {
    a.length match {
      case 0 => Seq.empty
      case _ => {
        val res = new ArrayBuffer[T]
        res.append(a(0))
        for (aa <- a.tail) {
          res.append(Numeric[T].plus(res.last, aa))
        }
        res.toSeq
      }
    }
  }

}
