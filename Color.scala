
sealed abstract class RGB[T <: RGB[T]](requiredValue: Int) {
  val value:Int = requiredValue
  if(value > 255 || value < 0)
    throw new IllegalArgumentException("RGB value invalid, value: " + value)

  def +(that:T): T = {
    val proposedValue = this.value + that.value
    if(proposedValue < 255)
      create(proposedValue)
    else
      create(255)
  }
  def *(that:T): T = {
    val proposedValue = (this.value) * (that.value) / 255
    if (proposedValue > 0)
      create(proposedValue)
    else
      create(0)
  }
  override def toString(): String
  def create(value:Int): T

}

class R(value:Int) extends RGB[R](value:Int) {
  override def create(value:Int): R = new R(value)
  override def toString(): String = "Red: " + value
}
class G(value:Int) extends RGB[G](value:Int) {
  override def create(value:Int): G = new G(value)
  override def toString(): String = "Green: " + value
}
class B(value:Int) extends RGB[B](value:Int) {
  override def create(value:Int): B = new B(value)
  override def toString(): String = "Blue: " + value
}


class Color(requiredR:R, requiredG:G, requiredB:B) {

  val r = requiredR
  val g = requiredG
  val b = requiredB

  def +(that: Color): Color = {
    val newR = this.r + that.r
    val newG = this.g + that.g
    val newB = this.b + that.b

    new Color(newR, newG, newB)
  }
  def *(that: Color): Color = {
    val newR = this.r * that.r
    val newG = this.g * that.g
    val newB = this.b * that.b

    new Color(newR, newG, newB)
  }
  override def toString(): String = "Color {" + r + " " + g + " " + b + "}"

}

object Color {
  def main(args:Array[String]): Unit = {
    val c1 = new Color(new R(255), new G(0), new B(128))
    val c2 = new Color(new R(0), new G(128), new B(128))
    val c3 = new Color(new R(128), new G(255), new B(128))

    println("original: \n" + c1 + "\n" + c2 + "\n" + c3)

    val cadd1 = c1 + c2
    val cadd2 = c2.+(c3)
    val cadd3 = c1 + c3

    println("add: \n" + cadd1 + "\n" + cadd2 + "\n" + cadd3)

    val cmul1 = c1 * c2
    val cmul2 = c2.*(c3)
    val cmul3 = c1 * c3

    println("multiply: \n" + cmul1 + "\n" + cmul2 + "\n" + cmul3)

  }
}
