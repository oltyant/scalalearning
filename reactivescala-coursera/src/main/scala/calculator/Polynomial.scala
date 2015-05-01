package calculator
import scala.math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
    	val aval = a()
    	val bval = b()
    	val cval = c()
    	bval * bval - 4 * aval * cval
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
   	def compute2ndDegreePolynomial(a: Double, b: Double, c: Double, delta: Double): Set[Double] = {
   		val minusb = (-1 * b)
   		if (delta == 0) Set(minusb / (2 * a))
   		else {
   			Set(minusb + sqrt(delta), minusb - sqrt(delta)).map(_ / (2 * a))
   		}
   	}
   	Signal {
   		val deltaval = delta()
   		val aval = a()	
   		val bval = b()
   		val cval = c()
   		if (deltaval < 0) Set[Double]()
   		else {
   			compute2ndDegreePolynomial(aval, bval, cval, deltaval)
   		}
   	}
  }
}
