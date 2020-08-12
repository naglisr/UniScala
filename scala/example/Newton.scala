package example

import java.awt.Color
import java.awt.image.BufferedImage
import java.awt.image.BufferedImage.TYPE_INT_RGB
import java.io.File

import javax.imageio.ImageIO

import scala.annotation.tailrec

case class Newton(p: Polynomial, maxIterations: Int=40, tol: Double = 10e-12, res: Int = 800)
{
  def draw(){
    val img = new BufferedImage(res, res, TYPE_INT_RGB)
    val canvas = plot()
    val hues = 0 until (p.coefficients.length - 1) map (_.toFloat / (p.coefficients.length-1))
    println(hues)

    for(x <- 0 until res; y <- 0 until res){
      val (index, iter) = canvas(x)(y)
      val hue = hues(index)
      val brightness = 1- iter/maxIterations.toFloat
      val col = Color.getHSBColor(hue, 1, brightness).getRGB
      img setRGB (x, y, col)
    }
    ImageIO write (img, "png", new File("/home/naglis/Fractal.png"))
  }

  @tailrec
  private def plot(x: Int = 0,points: Seq[Seq[(Int,Int)]] = Seq(), roots: Seq[Complex] = Seq()): Seq[Seq[(Int, Int)]] = {
    @tailrec
    def plotCol(roots: Seq[Complex], col: Seq[(Int,Int)] = Seq(), y: Int = 0):(Seq[(Int,Int)], Seq[Complex]) =
      if (y >= res) (col, roots)
      else {
        val (root, nIter) = calculate(pixelToComplex(x, y))

        val (newRoots, index) =
          if (roots.size < p.coefficients.length-1) {
            val rootOpt =
              roots.zipWithIndex find { case (r, _) => (root - r).abs < tol }
            rootOpt map
              { case (_, i) => (roots, i)} getOrElse
              (roots :+ root, roots.size % (p.coefficients.length - 1))
          }
          else {
            val indexOfClosestRoot =
              roots.zipWithIndex minBy {
                case (r, _) => root - r abs
              } _2

            (roots, indexOfClosestRoot)
          }
        plotCol(newRoots, col :+ (index, nIter), y+1)
      }
    if(x >= res) points
    else {
      val (col, newRoots) = plotCol(roots)
      plot(x+1, points :+ col, newRoots)
    }
  }

  @tailrec
  private def calculate(z: Complex, numIterations: Int = maxIterations): (Complex, Int) = {
    val nextZ = iterate (z)
    if (numIterations <= 0 || (z - nextZ).abs < tol)
      (nextZ, maxIterations-numIterations)
    else
      calculate (nextZ, numIterations - 1)
  }

  private def iterate(z: Complex): Complex =
    z - p.valueAt(z) / (p.derivative valueAt z)


  private def pixelToComplex(x: Int, y: Int) =
    Complex (2D*x/res - 1, 1 - 2D*y/res)
}