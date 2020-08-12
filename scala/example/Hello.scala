package example

object Hello extends Greeting with App
{
  println(greeting)

  val p = Polynomial( Seq (-1D, 0D,0D,1D))
  val n = Newton(p)
  n draw()
}

trait Greeting {
  lazy val greeting: String = "hello"
}
