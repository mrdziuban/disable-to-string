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
  private lazy val STRING_TPES = Set(
    Symbol("scala/Predef.String#"),
    Symbol("java/lang/String#"),
    Symbol("cats/Show.Shown."),
    Symbol("cats/Show.Shown#"),
    Symbol("scalaz/Cord."),
    Symbol("scalaz/Cord#"))

  private def withSym[A](tree: Tree)(f: Symbol => A)(implicit doc: SemanticDocument): A =
    f(tree.symbol)

  private def isStringOrShown0(sym: Symbol): Boolean = {
    // println(s"************************\n$sym\n************************")
    STRING_TPES.contains(sym)
  }

  private def isStringOrShown(tpe: SemanticType)(implicit doc: SemanticDocument): Boolean =
    tpe match {
      case TypeRef(_, sym, _) => isStringOrShown(sym)
      case SingleType(_, sym) => isStringOrShown(sym)
      case ThisType(sym) => isStringOrShown(sym)
      case SuperType(_, sym) => isStringOrShown(sym)
      case ConstantType(_: StringConstant) => true
      case IntersectionType(tpes) => tpes.exists(isStringOrShown(_)) // TODO - is this right?
      case UnionType(tpes) => tpes.exists(isStringOrShown(_)) // TODO - is this right?
      case WithType(tpes) => tpes.exists(isStringOrShown(_)) // TODO - is this right?
      case StructuralType(tpe, decls) => isStringOrShown(tpe)
      case AnnotatedType(_, tpe) => isStringOrShown(tpe)
      case UniversalType(_, tpe) => isStringOrShown(tpe)
      case ByNameType(tpe) => isStringOrShown(tpe)
      case RepeatedType(tpe) => isStringOrShown(tpe)
      case _ => false
    }

  private def isStringOrShown(sym: Symbol)(implicit doc: SemanticDocument): Boolean =
    isStringOrShown0(sym) || (sym.info.map(_.signature) match {
      case Some(ValueSignature(tpe)) => isStringOrShown(tpe)
      case Some(MethodSignature(_, _, tpe)) => isStringOrShown(tpe)
      case Some(TypeSignature(_, _, tpe)) => isStringOrShown(tpe)
      case _ => false
    })

  private def isStringOrShown(arg: Term)(implicit doc: SemanticDocument): Boolean =
    arg match {
      case Lit.String(_) => true
      case t => withSym(t)(isStringOrShown(_))
    }

  private lazy val SHOW_TPES = Set(
    Symbol("cats/Show#"),
    Symbol("scalaz/Show#"))

  private def isShowTpe(tpe: Type)(implicit doc: SemanticDocument): Boolean =
    withSym(tpe)(SHOW_TPES.contains(_))

  private lazy val SHOW_FNS = Set(
    Symbol("cats/Show.show()."),
    Symbol("scalaz/Show.show()."),
    Symbol("scalaz/Show.shows()."))

  private def isShowFn(fn: Term)(implicit doc: SemanticDocument): Boolean =
    withSym(fn)(SHOW_FNS.contains(_))

  private def fixTree(tree: Tree)(implicit doc: SemanticDocument): Patch = {
    tree match {
      // Disallow calls to `.toString`
      case t @ Term.Select(term, Term.Name("toString")) if !isStringOrShown(term) =>
        Patch.lint(ToString(t))

      // Disallow string interpolation of anything but strings
      case t @ Term.Interpolate(Term.Name("s"), _, args) =>
        println(s"*****************\n${args.map(_.symbol).mkString("\n")}\n**********************")
        args.map(a => if (isStringOrShown(a)) Patch.empty else Patch.lint(Interp(a))).asPatch

      // Allow the above when inside a `new Show` block
      case Term.NewAnonymous(t @ Template(_, List(Init(Type.Apply(tpe, _), _, _)), _, _)) if isShowTpe(tpe) =>
        Patch.empty

      // Allow the above when inside a `Show.show` or `Show.shows` call
      case Term.Apply(fn, _) if isShowFn(fn) =>
        Patch.empty

      case _ => tree.children.map(fixTree(_)).asPatch
    }
  }

  override def fix(implicit doc: SemanticDocument): Patch =
    fixTree(doc.tree).atomic
}
