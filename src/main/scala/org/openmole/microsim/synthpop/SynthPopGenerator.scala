package org.openmole.microsim.synthpop

import org.openmole.microsim._

import scala.util.Random

trait SynthPopGenerator {

  def generatePopulation(implicit rng: Random): Population

}



