# Deprecation notice

Please see [mblink/disable-toString](https://github.com/mblink/disable-toString) for an alternative. It's a compiler plugin that supersedes the functionality of these rules and has been tested and used with real-world code.

# Disable `toString`

This repository contains a scalafix rule to disallow calls to `toString`, both explicit and implicit (i.e. in string
interpolation). Usage of [cats](https://github.com/typelevel/cats) and [scalaz](https://github.com/scalaz/scalaz) `Show`
typeclasses is supported and encouraged.

## What's disallowed

```scala
val i: Int = 1

// Calls to `toString` and interpolation of non String values trigger warnings
i.toString
s"The int is: $i"
```

## What's allowed

```scala
val s: String = "test"

// Calls to `toString` and interpolation of String values are allowed
s.toString
s"The string is: $s"

// Everything is allowed when defining `cats.Show` instances
// Both `new Show { ... }` and `Show.show(...)` definitions are supported
val catsShow1: cats.Show[Int] = new cats.Show[Int] { def show(i: Int) = i.toString }
val catsShow2: cats.Show[Int] = cats.Show.show(_.toString)

// Similarly, everything is allowed when defining `scalaz.Show` instances
// All of `new Show { ... }`, `Show.show(...)`, and `Show.shows(...)` definitions are supported
val scalazShow1: scalaz.Show[Int] = new scalaz.Show[Int] { def shows(i: Int) = i.toString }
val scalazShow2: scalaz.Show[Int] = scalaz.Show.show(i => scalaz.Cord(i.toString))
val scalazShow3: scalaz.Show[Int] = scalaz.Show.shows(_.toString)
```

## Suppression

You can suppress warnings from this rule using `scalafix:ok` or `scalafix:off ... scalafix:on`
[comments](https://scalacenter.github.io/scalafix/docs/users/suppression.html#comments).
