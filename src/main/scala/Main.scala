import scala.util.Random
import scala.math.abs

object Main {
  // Funcion seleccionable
  //private val f: Punto2D => Double = fitnessSphere
  private val f: Punto2D => Double = fitnessRosenbrock
  private val ERROR = 0.001

  def main(args: Array[String]): Unit = {
    var puntos: List[Punto2D] = List(
      generarPuntoAleatorio(),
      generarPuntoAleatorio(),
      generarPuntoAleatorio()
    )

    do {
      // Ordena usando la función de fitness seleccionada
      puntos = puntos.sortBy(p => f(p))

      val b: Punto2D = puntos.head
      val g: Punto2D = puntos(1)
      val w = puntos(2)

      val m: Punto2D = crearM(b, g)
      val r: Punto2D = crearR(m, w)

      if (f(r) < f(m)) {
        // case 1
        if (f(b) < f(r)) {
          // W <- R
          puntos = puntos.updated(2, r)
        } else {
          val e: Punto2D = crearE(r, m)
          if (f(e) < f(b)) {
            // W <- E
            puntos = puntos.updated(2, e)
          } else {
            // W <- R
            puntos = puntos.updated(2, r)
          }
        }
      } else {
        // case 2
        if (f(r) < f(w)) {
          // W <- R
          puntos = puntos.updated(2, r)
        }
        val c: Punto2D = crearC(w, m)
        if (f(c) < f(w)) {
          // W <- C
          puntos = puntos.updated(2, c)
        } else {
          // w <- s        g <- m
          val s: Punto2D = crearS(b, w)
          puntos = puntos.updated(2, s)
          puntos = puntos.updated(1, m)
        }
      }
    } while (abs(f(puntos.head) - f(puntos(2))) > ERROR)

    println(puntos.head)
    println(f(puntos.head))
  }

  private val random = new Random()

  private def generarPuntoAleatorio(): Punto2D = {
    def randomDoubleInRange(min: Double, max: Double): Double =
      min + (random.nextDouble() * (max - min))

    Punto2D(randomDoubleInRange(-5, 5), randomDoubleInRange(-5, 5))
  }

  private def crearM(b: Punto2D, g: Punto2D): Punto2D = {
    (b + g) / 2.0
  }

  private def crearR(m: Punto2D, w: Punto2D): Punto2D = {
    val term1: Punto2D = m * 2.0
    term1 - w
  }

  private def crearE(r: Punto2D, m: Punto2D): Punto2D = {
    val term1: Punto2D = r * 2.0
    term1 - m
  }

  private def crearC(w: Punto2D, m: Punto2D): Punto2D = {
    (w + m) / 2.0
  }

  private def crearS(b: Punto2D, w: Punto2D): Punto2D = {
    (b + w) / 2.0
  }

  // Función de fitnessSphere que recibe un Punto2D como parámetro
  private def fitnessSphere(ext: Punto2D): Double = {
    math.pow(ext.x, 2) + math.pow(ext.y, 2)
  }

  // Función de fitnessRosenbrock que recibe un Punto2D como parámetro
  private def fitnessRosenbrock(ext: Punto2D): Double = {
    val term1 = math.pow(1.0 - ext.x, 2)
    val term2 = 100.0 * math.pow(ext.y - math.pow(ext.x, 2), 2)
    term1 + term2
  }
}
