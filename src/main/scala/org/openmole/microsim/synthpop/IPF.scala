package org.openmole.microsim.synthpop

import org.openmole.microsim
import org.openmole.microsim._
import org.openmole.microsim.math.ContingencyTable

import scala.util.Random


/**
  *
  * see Lovelace, R., Birkin, M., Ballas, D., & van Leeuwen, E. (2015). Evaluating the performance of iterative proportional fitting for spatial microsimulation: new tests for an established technique. Journal of Artificial Societies and Social Simulation, 18(2).
  *   http://jasss.soc.surrey.ac.uk/18/2/21.html
  *
  * @param marginals
  */
case class IPFPopulationGenerator(
                                   marginals: Vector[Vector[Int]],
                                   totalPopulation: Int,
                                   stopping: StoppingCondition
                                 ) extends SynthPopGenerator {

  import IPF._

  override def generatePopulation(implicit rng: Random): Population = new Population{
    def structure: PopulationStructure = integerTableToPopulationStructure(integerizeMaxDiff(generateContingencyTable(marginals,stopping),totalPopulation))
  }

}




object IPF {

  def notStop(stoppingCondition: StoppingCondition) = {
    state: (Int,ContingencyTable[Double],Double) =>
      stoppingCondition match {
        case MaxIterations(m) => state._1 < m
        case ToleranceThreshold(epsilon) => state._3 > epsilon
      }
  }

  def generateContingencyTable(marginals: Vector[Vector[Int]],stopping: StoppingCondition): ContingencyTable[Double] = {
    Iterator.iterate((0.toInt,ContingencyTable(marginals.map{_.length},1.0),Double.PositiveInfinity)) {state =>
      updateTable(state._1,state._2, marginals)
    }.takeWhile(notStop(stopping)).toVector.last._2
  }


  /**
    * One iteration along all dimensions - note: this is super inefficient
    *   [will fail in one dimension - not needed in that case]
    * @param table
    * @param marginals
    * @return
    */
  def updateTable(iter: Int, table: ContingencyTable[Double], marginals: Vector[Vector[Int]]): (Int,ContingencyTable[Double],Double) = {
    val totsum = table.flatten.sum
    val d = marginals.length;val a = marginals.toArray
    val newtable = Iterator.iterate((0.toInt,table)){
      case (k,t) =>
        val multiinds = ContingencyTable.getMultiIndices((marginals.take(k-1)++marginals.takeRight(d-k)).map{_.length}).toArray
        val currentmarginal: Vector[Int] = a(k)
        (k+1,Iterator.iterate((0.toInt,t)) {case (kk,tt)=>
          (kk+1,tt.updateRow(multiinds(k), k, _.zip(currentmarginal).map { case (y, x) => y * x.toDouble / totsum }))
        }.take(multiinds.length).toVector.last._2)
    }.take(d-1).next()._2
    (iter + 1, newtable, table.flatten.zip(newtable.flatten).map{case (p1,p2) => scala.math.pow(p1 - p2,2)}.sum)
  }


  /**
    * Brutal integerization
    *  [should implement different integerization algorithms ]
    * @param table
    * @param totalPopulation
    * @return
    */
  def integerizeMaxDiff(table: ContingencyTable[Double], totalPopulation: Int): ContingencyTable[Int] = {
    val integerTable: ContingencyTable[Int] = table.elementWiseMap(d=> scala.math.floor(d*totalPopulation.toDouble).toInt)
    val diffs = table.zip(integerTable).elementWiseMap{case (d,i) => d - i}
    val (_,maxmultiind) = diffs.flatten.zip(ContingencyTable.getMultiIndices(table.dimensions)).maxBy(_._1) // rq: must check the multiinds are effectively the good ones when flattening
    val popdiff = totalPopulation - integerTable.flatten.sum
    integerTable.set(maxmultiind,popdiff)
  }

  def integerTableToPopulationStructure(table: ContingencyTable[Int]): PopulationStructure =
    table.flatten.zip(ContingencyTable.getMultiIndices(table.dimensions)).map{case (count,characs) => Vector.fill(count)(characs)}.fold(Vector.empty){case (v1,v2)=> v1++v2}


}
