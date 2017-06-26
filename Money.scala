package com.hcl.money

/**
 * Defining Money operations
 */
trait Money[A]{
  def add( firstParam : A, secondParam : A) : A
}

/**
 * Defining implicit class to do operation on value like 2 add 3
 */
object MoneyMakeUp {
 implicit class MoneyOps[A: Money]( firstParam : A )( implicit m : Money[A] ){
   def add( secondParam : A ) = m.add( firstParam, secondParam )
 }
}
/**
 * MoneyImpl class -- Wrapper class to define how add function should be called
 */
class MoneyImpl[A]( m : Money[A]) extends Money[A]{
  override def add( firstParam : A, secondParam : A ) = m.add(firstParam, secondParam)
}
/**
 * LoggerMoneyImpl class -- Wrapper class to define how add function should be called
 */
class LoggerMoneyImpl[A](  m : Money[A]) extends Money[A]{
  private def log[X]( f: => X) = {
    println("LOG INFO:START")
    val x = f
    println(s"LOG INFO:$x")
    println("LOG INFO:END")
    x
  }
  override def add( firstParam : A, secondParam : A ) = {log( m.add(firstParam, secondParam) ) }
}

/**
 * Trait MoneyStructImplicit extending Money trait - defined to help implicit for different data types
 */
trait MoneyStructImplicit[A] extends Money[A]
/**
 * object MoneyStructImplicit - defined to implement implicit which extends MoneyStructImplicit 
 */
object MoneyStructImplicit{
  implicit object MoneyStructImplicitInt extends MoneyStructImplicit[Int] {
    override def add( firstParam: Int, secondParam: Int ): Int = firstParam + secondParam
  }
}
/**
 * Object Money-- define implicit logMoney method
 */
object Money{
  //implicit def logMoney[A]( implicit delegate :MoneyStructImplicit[A]  ) : Money[A] = new MoneyImpl(    delegate    )
  implicit def logMoney[A]( implicit delegate :MoneyStructImplicit[A]  ) : Money[A] = new LoggerMoneyImpl(    delegate    )
}

object TestMoney{
  def main(args: Array[String]): Unit = {
    
   import MoneyMakeUp._
   val a = 2  add 3
   println(a)
    
  }
}