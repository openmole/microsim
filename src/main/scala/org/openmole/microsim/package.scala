package org.openmole

package object microsim {

  trait Individual

  trait IndividualGrouping

  trait Population {
    def structure: PopulationStructure
  }

  /**
    * Description of a population by classes vector for each individual
    */
  type PopulationStructure = Vector[Vector[Int]]


  sealed trait StoppingCondition{
    def iterations: Int
    def tolerance: Double
  }
  final case class MaxIterations(iterations: Int) extends StoppingCondition {def tolerance = 0.0}
  final case class ToleranceThreshold(tolerance: Double) extends StoppingCondition {def iterations = 0}



}
