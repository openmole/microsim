package org.openmole.microsim.math

/**
  * Any dimension contingency table
  * @tparam T
  */
sealed trait ContingencyTable[T] {

  def table: Vector[_]

  def dimensions: Vector[Int]

  def flatten: Vector[T]

  def get(index: Vector[Int]): T

  def set(index: Vector[Int],value: T): ContingencyTable[T]

  def getRow(index: Vector[Int],alongDim: Int): Vector[T]

  def setRow(index: Vector[Int],alongDim: Int, value: Vector[T]): ContingencyTable[T] = updateRow(index,alongDim,_ => value)

  def updateRow(index: Vector[Int],alongDim: Int, update: Vector[T]=> Vector[T]): ContingencyTable[T]

  def elementWiseMap[U](f: T=>U): ContingencyTable[U]

  def zip[U](t2: ContingencyTable[U]): ContingencyTable[(T,U)]

}

final case class Count[T](table: Vector[T]) extends ContingencyTable[T] {
  def dimensions = Vector(table.length)
  def flatten: Vector[T] = table
  def get(index: Vector[Int]) = table(index.head)
  def set(index: Vector[Int], value: T): ContingencyTable[T] = Count[T](table.updated(index.head,value))
  def getRow(index: Vector[Int],alongDim: Int) = table
  def updateRow(index: Vector[Int], alongDim: Int, update: Vector[T]=> Vector[T]): ContingencyTable[T] = this.copy(table=update(table))
  def elementWiseMap[U](f: T=> U) = this.copy(table=table.map(f))
  def zip[U](t2: ContingencyTable[U]) = t2 match {
    case Count(t) => Count(table.zip(t))
    case d: DContingencyTable[U] => Count(table.zip(d.getRow(Vector.fill(d.dimensions.head)(0),0)))
  }
}

final case class DContingencyTable[T](table: Vector[ContingencyTable[T]]) extends ContingencyTable[T]{

  /**
    * note: we assume consistency and do not check it !
    * @return
    */
  def dimensions = Vector(table.length)++table.head.dimensions

  def flatten: Vector[T] = table.flatMap{_.flatten}

  def get(index: Vector[Int]) = table(index.head).get(index.tail)
  // not sure this is optimal
  def set(index: Vector[Int], value: T): ContingencyTable[T] = this.copy(table=table.updated(index.head,table(index.head).set(index.tail, value)))

  def getRow(index: Vector[Int],alongDim: Int): Vector[T] = alongDim match {
    case 0 => table.map{_.get(index)}
    case _ => table(index.head).getRow(index.tail,alongDim - 1)
  }

  def updateRow(index: Vector[Int], alongDim: Int, update: Vector[T]=> Vector[T]): ContingencyTable[T] = alongDim match {
    case 0 => {
      val updatedrow = update(table.map{_.get(index)})
      this.copy(table=table.zip(updatedrow).map{case (t,u) => t.set(index,u)})
    }
    case _ => this.copy(table = table.updated(index.head,table(index.head).updateRow(index.tail,alongDim - 1,update)))
  }

  def elementWiseMap[U](f: T => U): ContingencyTable[U] = this.copy(table=table.map(_.elementWiseMap(f)))

  def zip[U](t2: ContingencyTable[U]): ContingencyTable[(T,U)] = t2 match {
    case Count(t) => Count(getRow(Vector.fill(dimensions.head)(0),0).zip(t))
    case DContingencyTable(t) => this.copy(table = table.zip(t).map{case (d1,d2) => d1.zip(d2)})
  }

}

object ContingencyTable {

  def apply[T](count: Vector[T]) = Count[T](count)

  def apply[T](table: Vector[Vector[_]]): ContingencyTable[T] = DContingencyTable[T](table.map{r =>
    r match {
      case c: Vector[T] => ContingencyTable(c)
      case t: Vector[Vector[_]] => ContingencyTable(t)
    }
  })


  def apply[T](dimensions: Vector[Int], value: T): ContingencyTable[T] = dimensions match {
    case Vector(n) => Count(Vector.fill(n)(value))
    case dims => DContingencyTable[T](Vector.fill(dims.head){ContingencyTable(dims.tail,value)})
  }

  /**
    * get all multiindices for given dimensions
    * @param dimensions
    * @return
    */
  def getMultiIndices(dimensions: Vector[Int]): Vector[Vector[Int]] = dimensions match {
    case Vector(n) => Vector.tabulate(n){i => Vector(i)}
    case dims => {
      val currentinds: Vector[Int] = Vector.tabulate(dims.head){_}
      val tailmultiinds: Vector[Vector[Int]] = getMultiIndices(dims.tail)
      tailmultiinds.map{multiind => currentinds.map{i: Int => Vector(i)++multiind}}.fold(Vector.empty){case (v1,v2)=> v1++v2}
    }
  }


}