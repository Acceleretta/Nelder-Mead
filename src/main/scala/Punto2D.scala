case class Punto2D(x: Double, y: Double) {
  def +(ext: Punto2D): Punto2D = {
    Punto2D(this.x + ext.x, this.y + ext.y)
  }

  def -(ext: Punto2D): Punto2D = {
    Punto2D(this.x - ext.x, this.y - ext.y)
  }

  def /(ext: Double): Punto2D = {
    Punto2D(this.x / ext, this.y / ext)
  }

  def *(ext: Double): Punto2D = {
    Punto2D(this.x * ext, this.y * ext)
  }




}
