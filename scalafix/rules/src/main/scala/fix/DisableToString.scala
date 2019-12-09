package fix

import metaconfig.{Conf, Configured}
import scalafix.v1._
import scala.annotation.tailrec
import scala.meta._
import scala.meta.internal.pc.ScalafixGlobal
import scala.meta.internal.proxy.GlobalProxy
import scala.reflect.internal.util.{Position => ScalaPosition}
import scala.util.{Properties, Try}
import scala.util.control.NonFatal

object code {
  def apply(s: String): String = s"`$s`"
}

case class ToString(tree: Tree, tpe: ScalafixGlobal#Type) extends Diagnostic {
  override def position: Position = tree.pos
  override def message: String = s"Don't call ${code(s"$tpe.toString")}, use ${code("cats.Show")}"
}

case class Interp(tree: Tree, tpe: ScalafixGlobal#Type) extends Diagnostic {
  override def position: Position = tree.pos
  override def message: String = s"Only strings can be interpolated. Consider defining a ${code(s"cats.Show[$tpe]")} " ++
                                 s"instance and using ${code("""show"..."""")} from ${code("cats.syntax.show._")}"
}

class DisableToString(global: ScalafixGlobal) extends SemanticRule("DisableToString") {
  def this() = this(null)

  override def afterComplete(): Unit = shutdownCompiler()

  private def shutdownCompiler(): Unit = {
    if (global != null) {
      try {
        global.askShutdown()
        global.close()
      } catch {
        case NonFatal(_) =>
      }
    }
  }

  override def withConfiguration(c: Configuration): Configured[Rule] =
    if (c.scalacClasspath.nonEmpty && c.scalaVersion != Properties.versionNumberString)
      Configured.typeMismatch(s"scalaVersion=${Properties.versionNumberString}", Conf.Obj("scalaVersion" -> Conf.Str(c.scalaVersion)))
    else
      Configured.Ok(new DisableToString(
        if (c.scalacClasspath.isEmpty) null
        else ScalafixGlobal.newCompiler(c.scalacClasspath, c.scalacOptions, Map())))

  private lazy val STRING_TPE = global.typeOf[String]
  private lazy val STRING_TPES = Set(STRING_TPE) ++
    Try(Set(global.rootMirror.staticClass("_root_.scalaz.Cord").toType)).getOrElse(Set()) ++
    Try(Set(global.rootMirror.staticClass("_root_.cats.Show.Shown").toType)).getOrElse(Set())

  private def pos(term: Term, unit: global.CompilationUnit): ScalaPosition =
    global.rangePos(unit.source, term.pos.start, term.pos.start, term.pos.end)

  @tailrec private def dealiasType(t: global.Type): global.Type = t.dealias match {
    case x if x == t => x
    case x => dealiasType(x)
  }

  @tailrec private def widenType(t: global.Type): global.Type = t.widen match {
    case x if x == t => x
    case x => widenType(x)
  }

  private def getType(term: Term, unit: global.CompilationUnit): global.Type =
    widenType(dealiasType(GlobalProxy.typedTreeAt(global, pos(term, unit)).tpe))

  private def isStringOrShown(term: Term, unit: global.CompilationUnit): (Boolean, global.Type) =
    term match {
      case Lit.String(_) => (true, STRING_TPE)
      case _ =>
        val tpe = getType(term, unit)
        (STRING_TPES.exists(t => t =:= tpe || tpe <:< t), tpe)
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
      case t @ Term.Select(term, Term.Name("toString")) =>
        val (isStr, tpe) = isStringOrShown(term, unit)
        if (isStr) Patch.empty else Patch.lint(ToString(t, tpe))

      // Disallow string interpolation of anything but String/Shown/Cord
      case Term.Interpolate(Term.Name("s"), _, args) =>
        args.map { a =>
          val (isStr, tpe) = isStringOrShown(a, unit)
          if (isStr) Patch.empty else Patch.lint(Interp(a, tpe))
        }.asPatch

      // Allow the above when inside a `new Show` block
      case Term.NewAnonymous(Template(_, List(Init(Type.Apply(tpe, _), _, _)), _, _)) if isShowTpe(tpe) =>
        Patch.empty

      // Allow the above when inside a `Show.show` or `Show.shows` call
      case Term.Apply(fn, _) if isShowFn(fn) =>
        Patch.empty

      case _ => tree.children.map(fixTree(_, unit)).asPatch
    }
  }

  override def fix(implicit doc: SemanticDocument): Patch =
    fixTree(doc.tree, global.newCompilationUnit(doc.input.text, doc.input.syntax)).atomic
}

object DisableToString
