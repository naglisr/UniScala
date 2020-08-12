package example

import scala.annotation.tailrec

final case class Complex (real: Double, im: Double = 0) {
  def -(z: Complex) = Complex(real-z.real, im-z.im)

  def +(z: Complex) = Complex(real+z.real, im+z.im)

  def /(q: Double) = Complex(real / q, im / q)

  def /(z: Complex): Complex = {
    val denominator = z.real*z.real + z.im*z.im

    Complex((real*z.real + im*z.im) / denominator, (im*z.real-real*z.im) / denominator)
  }

  def *(q: Double) = Complex(real*q, im*q)

  def *(z: Complex) = Complex(real*z.real-im*z.im, im*z.real+real*z.im)

  @tailrec
  def ^(q: Int, acc: Complex = 1D): Complex =
    if (q == 0) acc
    else ^(q - 1, *(acc))

  def abs: Double = math.sqrt(real*real + im*im)
}
object Complex {
  implicit def double2complex(num: Double): Complex =
    Complex (num)

  implicit class SeqWrapper (nums: Seq[Complex]) {
    def total: Complex = nums.fold (Complex (0)) (_+_)
  }
}