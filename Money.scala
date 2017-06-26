package com.hcl.money

trait Money[A]{
  def add( firstParam : A, secondParam : A) : A
}

object Money {
  
  def apply[A]( implicit m : Money[A] ) : Money[A] = m
  
  object ops {

    implicit class MoneyOps[A: Money]( firstParam : A ){
      def add( secondParam : A ) = Money[A].add( firstParam, secondParam )
    }
  }
  
  /**
   * Declaring Add methods for different data types
   */
  def instance[ A ]( funct : (A, A) => A ): Money[A] = new Money[A] {
    def add( firstParam : A, secondParam : A ) = funct( firstParam, secondParam )
  }
  
  implicit val addInt : Money[Int] = instance[Int]( ( firstParam : Int, secondParam : Int ) => { firstParam + secondParam } )
  
  implicit val addBigDecimal : Money[BigDecimal] = instance[BigDecimal]( ( firstParam : BigDecimal, secondParam : BigDecimal ) => { firstParam + secondParam } )
    
  implicit val addDOuble : Money[Double] = instance[Double]( ( firstParam : Double, secondParam : Double ) => { firstParam + secondParam } )
  
}


object TestMoney{
  def main(args: Array[String]): Unit = {
    import Money.ops._
    println(2 add 3)
    println( 2.3 add 3.2 )
    val firstBigDecimalParam : BigDecimal = 1234.12
    val secondBigDecimalParam : BigDecimal = 1234.12
    println( firstBigDecimalParam add secondBigDecimalParam   )
  }
}
