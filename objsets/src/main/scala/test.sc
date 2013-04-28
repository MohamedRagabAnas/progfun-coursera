object Rational {
  val x = new Rational(1,3)                       //> x  : Rational = 1/3
  val y = new Rational(5,7)                       //> y  : Rational = 5/7
  val z = new Rational(3,2)                       //> z  : Rational = 3/2

  x.neg                                           //> res0: Rational = 1/-3
  
  x.sub(y).sub(z)                                 //> res1: Rational = -79/42
  y.add(y)                                        //> res2: Rational = 10/7
}

class Rational(x: Int, y: Int) {
  private def gdc(a: Int, b: Int): Int = if (b == 0) a else gdc(b, a % b)
  private val g = gdc(x, y)
  def numer = x / g
  def denom = y / g

  def add(that: Rational) =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom)
  
  def neg = new Rational(-numer, denom)
  
  def sub(that: Rational) = add(that.neg)
      
  override def toString = numer + "/" + denom

}