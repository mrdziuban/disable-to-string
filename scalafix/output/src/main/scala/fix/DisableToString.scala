package fix

import cats.{Show => CatsShow}
import cats.syntax.show._
import scalaz.{Cord, Show => ScalazShow}

case class Foo(s: String)

case class Bar(i: Int)
object Bar {
  implicit val scalazNewShowToString: scalaz.Show[Bar] = new scalaz.Show[Bar] {
    override def shows(b: Bar): String = "Bar(" ++ b.i.toString ++ ")"
  }

  val scalazNewShowInterp: scalaz.Show[Bar] = new scalaz.Show[Bar] {
    override def shows(b: Bar): String = s"Bar(${b.i})"
  }

  val scalazNewShowToStringAliased: ScalazShow[Bar] = new ScalazShow[Bar] {
    override def shows(b: Bar): String = "Bar(" ++ b.i.toString ++ ")"
  }

  val scalazNewShowInterpAliased: ScalazShow[Bar] = new ScalazShow[Bar] {
    override def shows(b: Bar): String = s"Bar(${b.i})"
  }

  val scalazShowFuncToString: scalaz.Show[Bar] = scalaz.Show.show(b => Cord("Bar(" ++ b.i.toString ++ ")"))
  val scalazShowFuncInterp: scalaz.Show[Bar] = scalaz.Show.show(b => Cord(s"Bar(${b.i})"))

  val scalazShowFuncToStringAliased: ScalazShow[Bar] = ScalazShow.show(b => Cord("Bar(" ++ b.i.toString ++ ")"))
  val scalazShowFuncInterpAliased: ScalazShow[Bar] = ScalazShow.show(b => Cord(s"Bar(${b.i})"))

  val scalazShowsFuncToString: scalaz.Show[Bar] = scalaz.Show.shows(b => "Bar(" ++ b.i.toString ++ ")")
  val scalazShowsFuncInterp: scalaz.Show[Bar] = scalaz.Show.shows(b => s"Bar(${b.i})")

  val scalazShowsFuncToStringAliased: ScalazShow[Bar] = ScalazShow.shows(b => "Bar(" ++ b.i.toString ++ ")")
  val scalazShowsFuncInterpAliased: ScalazShow[Bar] = ScalazShow.shows(b => s"Bar(${b.i})")

  implicit val catsNewShowToString: cats.Show[Bar] = new cats.Show[Bar] {
    override def show(b: Bar): String = "Bar(" ++ b.i.toString ++ ")"
  }

  val catsNewShowInterp: cats.Show[Bar] = new cats.Show[Bar] {
    override def show(b: Bar): String = s"Bar(${b.i})"
  }

  val catsNewShowToStringAliased: CatsShow[Bar] = new CatsShow[Bar] {
    override def show(b: Bar): String = "Bar(" ++ b.i.toString ++ ")"
  }

  val catsNewShowInterpAliased: CatsShow[Bar] = new CatsShow[Bar] {
    override def show(b: Bar): String = s"Bar(${b.i})"
  }

  val catsShowFuncToString: cats.Show[Bar] = cats.Show.show(b => "Bar(" ++ b.i.toString ++ ")")
  val catsShowFuncInterp: cats.Show[Bar] = cats.Show.show(b => s"Bar(${b.i})")

  val catsShowFuncToStringAliased: CatsShow[Bar] = CatsShow.show(b => "Bar(" ++ b.i.toString ++ ")")
  val catsShowFuncInterpAliased: CatsShow[Bar] = CatsShow.show(b => s"Bar(${b.i})")
}

object DisableToString {
  val stringLitVal = "1"
  val stringVal = 1.toString
  def stringDef() = "1"
  val interpString = s"a $stringLitVal b $stringVal c ${stringDef()} d"

  val intVal = 1
  def intDef() = 1
  val interpIntLit = s"a ${1} b"
  val interpIntVal = s"a $intVal b"
  val interpIntDef = s"a ${intDef()} b"

  val boolVal = true
  def boolDef() = false
  val interpBoolLit = s"a ${true} b"
  val interpBoolVal = s"a $boolVal b"
  val interpBoolDef = s"a ${boolDef()} b"

  val fooVal = Foo("foo")
  def fooDef() = Foo("bar")
  val interpFooLit = s"a ${Foo("baz")} b"
  val interpFooVal = s"a $fooVal b"
  val interpFooDef = s"a ${fooDef()} b"
  val interpFooValMember = s"a ${fooVal.s} b"
  val interpFooDefMember = s"a ${fooDef().s} b"

  val barVal = Bar(1)
  def barDef() = Bar(2)
  val interpBarLit = s"a ${Bar(3)} b"
  val interpBarVal = s"a $barVal b"
  val interpBarDef = s"a ${barDef()} b"
  val interpBarValMember = s"a ${barVal.i} b"
  val interpBarDefMember = s"a ${barDef().i} b"
  val showInterpBarLit = show"a ${Bar(3)} b"
  val showInterpBarVal = show"a $barVal b"
  val showInterpBarDef = show"a ${barDef()} b"

  // val allowedToString = Foo("foo").toString
  // val allowedInterp = s"a ${Bar(1)} b"

  def badSingleton[S <: Singleton](s: S) = s"a $s b"
  def goodSingleton[S <: Singleton with String](s: S) = s"a $s b"

  val cordToString = Cord("foo").toString
  val shownToString = CatsShow.Shown("bar").toString

  val cordInterp = s"a ${Cord("foo")} b"
  val shownInterp = s"a ${CatsShow.Shown("bar")} b"

  def cordFToString(x: Cord) = s"a $x b"
  def shownFToString(x: CatsShow.Shown) = s"a $x b"

  val showInterpToString = show"a ${Bar(1)} c".toString
  val showInterpInterp = s"${show"a ${Bar(1)} c"}"
}
