package example

//noinspection SimplifiableFoldOrReduce
case class Polynomial(coefficients: Seq[Complex])
{
  def derivative = Polynomial (
    coefficients.zipWithIndex.tail map {case (c, i) => i*c }
  )

  def valueAt(z:Complex): Complex =
    coefficients.zipWithIndex map {case (c, i) => c * (z^i) } total

  def gradientAt (z: Complex): Complex =
    derivative valueAt z
}
