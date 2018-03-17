package homework.l2
import com.sun.org.apache.xpath.internal.functions.Function2Args
/**
  * Вам нужно реализовать функции sumOfSquares и multiplicationOfCubes
  * при помощи ListFunctions.fold, композиции и частичного применения функций, sum, multiply и pow
  * Можно добавлять промежуточные функции.
  * Также вам может понадобится функция Function.uncurry,
  * которая из карированной функции делает функцию с несколькими аргументами
  */
object ListHomework {

  val sum = (a: Int, b: Int) => a + b

  val multiply = (a: Int, b: Int) => a * b

  def pow(a: Int, p: Int): Int = if(p <= 0) 1 else a * pow(a, p - 1)

  /**
    * Сумма квадратов чисел в списке
    */
  lazy val sumOfSquares: List[Int] => Int = { list =>
    val powPart = pow(_:Int,2)
    var amount=0
    val sumPart=sum(_:Int,amount)
    def sumOfSquares = {x:Int=> {
        x match {
          case i=> amount=sumPart(i); amount
        }
      }
    }
    def funcCompose(elem :Int) = sumOfSquares compose powPart
    val funcUncurried =Function.uncurried(funcCompose _)
    ListFunctions.fold(0, list)(funcUncurried)
  }
  /**
    * Сумма кубов чисел в списке
    */
  lazy val multiplicationOfCubes: List[Int] => Int = { list=>
    val powPart = pow(_:Int,3)
    var compositionOfCubes=1
    val multiplyPart=multiply(_:Int, compositionOfCubes)
    def multiplyOfCubes = { x: Int => {
        x match {
          case i => compositionOfCubes = multiplyPart(i); compositionOfCubes
        }
      }
    }
    def funcCompose(elem:Int) = multiplyOfCubes compose powPart
    val funcUncurried=Function.uncurried(funcCompose _ )
    ListFunctions.fold(1, list)(funcUncurried)
  }
}