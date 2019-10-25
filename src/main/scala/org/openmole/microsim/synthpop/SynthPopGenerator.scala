package org.openmole.microsim.synthpop

import org.openmole.microsim._

import scala.util.Random


/**
  *
  * Genstar project https://github.com/ANRGenstar/genstar
  *
  */
trait SynthPopGenerator {

  def generatePopulation(implicit rng: Random): Population

}



