package org.openmole.microsim.math

import org.apache.commons.math3.linear.RealMatrix

class Matrix[T](var values: Array[Array[T]]) {

  implicit def fromRealMatrix(m: RealMatrix): Matrix[Double] = new Matrix[Double](m.getData.clone())


}


object Matrix {



}
