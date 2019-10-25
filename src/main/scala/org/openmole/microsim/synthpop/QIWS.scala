package org.openmole.microsim.synthpop

import org.openmole.microsim._
import org.openmole.microsim.math.Random
import org.openmole.microsim.math.Math._

import scala.util.Random


case class QIWSPopulationGenerator(

                                  ) extends SynthPopGenerator {

  override def generatePopulation(implicit rng: Random): Population =

}


object QIWS {

  /**
    * Generate a population structure from marginals using the QIWS algorithm
    *    Smith, A., Lovelace, R., & Birkin, M. (2017). Population synthesis with Quasirandom integer sampling. Journal of Artificial Societies and Social Simulation, 20(4).
    *
    * @param marginals
    * @return
    *
    */
  def generatePopulation(marginals: Vector[Vector[Int]]): PopulationStructure = {
    val populations = marginals.map(_.sum)
    val totalpop = populations(0)
    //val cummarginals = marginals.map(cumsum(_))
    // ensure all marginals are consistent
    assert(populations.distinct.length>1,"Inconsistent marginals in QIWS population generation")
    /*
    // incorrect: this is sampling with replacement
    Random.sobolSequence(totalpop,dimension).map{ r =>
      r.zip(cummarginals).map{case (rk,cumm) => cumm.indexWhere(_ > rk*totalpop)}
    }*/
    def sampleWithoutReplacement(quasirandomseq: Vector[Vector[Double]],remainingMarginals: Vector[Vector[Int]],existingPop: PopulationStructure): (PopulationStructure,Vector[Vector[Int]]) = {
      if(remainingMarginals.flatten.sum==0){return(existingPop,remainingMarginals)}
      else {
        val cummarginals = remainingMarginals.map(cumsum(_)) // not optimal to recompute at each step; could use the indexWhere to do so
        val (individual,updatedMarginals) = quasirandomseq.head.zip(cummarginals).zip(remainingMarginals).map{
          case ((rd,cumm),marg) => {
            val i = cumm.indexWhere{_ > rd*totalpop}
            (i,marg.zipWithIndex.map{case(m,j)=>if(j==i) m - 1 else m})
          }
        }.unzip
        sampleWithoutReplacement(quasirandomseq.tail,updatedMarginals,existingPop++Vector(individual))
      }
    }
    sampleWithoutReplacement(Random.sobolSequence(totalpop,marginals.size),marginals,Vector.empty)._1
  }

}
