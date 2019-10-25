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

}
