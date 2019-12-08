package fix

import cats.Show.Shown
import metaconfig.{Conf, Configured}
import scalafix.v1._
import scala.meta._
import scala.meta.internal.pc.ScalafixGlobal
import scala.meta.internal.proxy.GlobalProxy
import scala.reflect.internal.util.{Position => ScalaPosition}
import scala.util.Properties
import scalaz.Cord

object code {
  def apply(s: String): String = s"`$s`"
}

case class ToString(t: Term.Select) extends Diagnostic {
  override def position: Position = t.pos
  override def message: String = s"Calls to ${code(".toString")} are disabled, use ${code("cats.Show")}"
}

case class Interp(t: Term) extends Diagnostic {
  override def position: Position = t.pos
  override def message: String = s"Only strings can be interpolated. Consider defining a ${code("cats.Show")} " ++
                                 s"instance and using ${code("""show"..."""")} from ${code("cats.syntax.show._")}"
}

class DisableToString(global: ScalafixGlobal) extends SemanticRule("DisableToString") {
  def this() = this(null)

  override def withConfiguration(c: Configuration): Configured[Rule] =
    if (c.scalacClasspath.nonEmpty && c.scalaVersion != Properties.versionNumberString)
      Configured.typeMismatch(s"scalaVersion=${Properties.versionNumberString}", Conf.Obj("scalaVersion" -> Conf.Str(c.scalaVersion)))
    else
      Configured.Ok(new DisableToString(
        if (c.scalacClasspath.isEmpty) null
        else ScalafixGlobal.newCompiler(c.scalacClasspath, c.scalacOptions, Map())))

  private lazy val STRING_TPES = Set(
    global.typeOf[String],
    global.typeOf[Shown],
    global.typeOf[Cord])
  private lazy val SINGLETON_STRING_TPE = global.typeOf[Singleton with String]

  private def pos(term: Term, unit: global.CompilationUnit): ScalaPosition =
    global.rangePos(unit.source, term.pos.start, term.pos.start, term.pos.end)

  private def isStringOrShown(term: Term, unit: global.CompilationUnit): Boolean =
    term match {
      case Lit.String(_) => true
      case _ =>
        val tpe = GlobalProxy.typedTreeAt(global, pos(term, unit)).tpe
        STRING_TPES.exists(t => t =:= tpe || tpe <:< SINGLETON_STRING_TPE)
    }

  private lazy val SHOW_TPES = Set(
    Symbol("cats/Show#"),
    Symbol("scalaz/Show#"))

  private def isShowTpe(tpe: Type)(implicit doc: SemanticDocument): Boolean =
    SHOW_TPES.contains(tpe.symbol)

  private lazy val SHOW_FNS = Set(
    Symbol("cats/Show.show()."),
    Symbol("scalaz/Show.show()."),
    Symbol("scalaz/Show.shows()."))

  private def isShowFn(fn: Term)(implicit doc: SemanticDocument): Boolean =
    SHOW_FNS.contains(fn.symbol)

  private def fixTree(tree: Tree, unit: global.CompilationUnit)(implicit doc: SemanticDocument): Patch = {
    tree match {
      // Disallow calls to `.toString` on anything but String/Shown/Cord
      case t @ Term.Select(term, Term.Name("toString")) if !isStringOrShown(term, unit) =>
        Patch.lint(ToString(t))

      // Disallow string interpolation of anything but String/Shown/Cord
      case Term.Interpolate(Term.Name("s"), _, args) =>
        args.map(a => if (isStringOrShown(a, unit)) Patch.empty else Patch.lint(Interp(a))).asPatch

      // Allow the above when inside a `new Show` block
      case Term.NewAnonymous(Template(_, List(Init(Type.Apply(tpe, _), _, _)), _, _)) if isShowTpe(tpe) =>
        Patch.empty

      // Allow the above when inside a `Show.show` or `Show.shows` call
      case Term.Apply(fn, _) if isShowFn(fn) =>
        Patch.empty

      case _ => tree.children.map(fixTree(_, unit)).asPatch
    }
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    fixTree(doc.tree, global.newCompilationUnit(doc.input.text, doc.input.syntax)).atomic
  }
}
