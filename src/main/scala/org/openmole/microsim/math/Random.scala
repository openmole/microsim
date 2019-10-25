package org.openmole.microsim.math

import org.apache.commons.math3.random.SobolSequenceGenerator

object Random {


  /**
    *
    * @param samples
    * @param dimension
    * @return
    */
  def sobolSequence(samples: Int, dimension: Int): Vector[Vector[Double]] = {
    val generator = new SobolSequenceGenerator(dimension)
    Iterator.continually(generator.nextVector().toVector).take(samples).toVector
  }


}
