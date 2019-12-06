package fix

import scalafix.v1._
import scala.meta._

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

class DisableToString extends SemanticRule("DisableToString") {
  private lazy val SCALA_STRING = Symbol("scala/Predef.String#")
  private lazy val JAVA_STRING = Symbol("java/lang/String#")

  private def withSym[A](tree: Tree)(f: Symbol => A)(implicit doc: SemanticDocument): A =
    f(tree.symbol)

  private def isString(sym: Symbol): Boolean = sym == SCALA_STRING || sym == JAVA_STRING

  private def isString(arg: Term)(implicit doc: SemanticDocument): Boolean =
    (arg, withSym(arg)(_.info.map(_.signature))) match {
      case (Lit.String(_), _) => true
      case (_, Some(ValueSignature(TypeRef(_, sym, _)))) => isString(sym)
      case (_, Some(ValueSignature(SingleType(_, sym)))) => isString(sym)
      case (_, Some(MethodSignature(_, _, TypeRef(_, sym, _)))) => isString(sym)
      case (_, Some(MethodSignature(_, _, SingleType(_, sym)))) => isString(sym)
      case (x, y) => false
    }

  private lazy val CATS_SHOW_TPE = Symbol("cats/Show#")
  private lazy val SCALAZ_SHOW_TPE = Symbol("scalaz/Show#")

  private def isShowTpe(tpe: Type)(implicit doc: SemanticDocument): Boolean =
    withSym(tpe)(s => s == CATS_SHOW_TPE || s == SCALAZ_SHOW_TPE)

  private lazy val CATS_SHOW_FUN = Symbol("cats/Show.show().")
  private lazy val SCALAZ_SHOW_FUN = Symbol("scalaz/Show.show().")
  private lazy val SCALAZ_SHOWS_FUN = Symbol("scalaz/Show.shows().")

  private def isShowFunc(fn: Term)(implicit doc: SemanticDocument): Boolean =
    withSym(fn)(s => s == CATS_SHOW_FUN || s == SCALAZ_SHOW_FUN || s == SCALAZ_SHOWS_FUN)

  private def fixTree(tree: Tree)(implicit doc: SemanticDocument): Patch = {
    tree match {
      // Disallow calls to `.toString`
      case t @ Term.Select(_, Term.Name("toString")) =>
        Patch.lint(ToString(t))

      // Disallow string interpolation of anything but strings
      case t @ Term.Interpolate(Term.Name("s"), _, args) =>
        args.map(a => if (isString(a)) Patch.empty else Patch.lint(Interp(a))).asPatch

      // Allow the above when inside a `new Show` block
      case Term.NewAnonymous(t @ Template(_, List(Init(Type.Apply(tpe, _), _, _)), _, _)) if isShowTpe(tpe) =>
        Patch.empty

      // Allow the above when inside a `Show.show` or `Show.shows` call
      case Term.Apply(fn, _) if isShowFunc(fn) =>
        Patch.empty

      case _ => tree.children.map(fixTree(_)).asPatch
    }
  }

  override def fix(implicit doc: SemanticDocument): Patch = fixTree(doc.tree).atomic
}
