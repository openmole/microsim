package org.openmole.microsim.test

import scala.util.Random

object Test extends App {

  implicit val rng = new Random


  TestSyntheticPopulation.testQIWS



}


object TestSyntheticPopulation {


  def testQIWS(implicit rng: Random) = {

    // random marginals?
    def randomMarginal(size: Int,totalPop: Int, maxValue: Int = 1000) = {
      val unscaled = Vector.fill(size){rng.nextInt(maxValue)}
      // rescale to totalPop

      // TODO
    }


  }


}
