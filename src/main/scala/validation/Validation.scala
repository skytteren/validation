package validation

import scala.NamedTuple.NamedTuple
import scala.compiletime.*
import scala.deriving.Mirror
import scala.math.Ordered.orderingToOrdered

enum Error:
  case Single(message: String, path: List[String])
  case Composite(message: String, path: List[String], errors: List[Error])

  def inPath(in: String): Error = this match
    case Single(message, path)          => Single(message, in +: path)
    case Composite(message, path, list) => Composite(message, in +: path, list.map(_.inPath(in)))

enum Result[+A]:
  case Valid(value: A) extends Result[A]
  case Invalid(error: Error) extends Result[Nothing]

  def inPath(in: String): Result[A] = this match
    case v @ Valid(_)   => v
    case Invalid(error) => Invalid(error.inPath(in))

  def map[B](f: A => B): Result[B] = this match
    case Valid(a)       => Valid(f(a))
    case Invalid(error) => Invalid(error)

  def toEither: Either[Error, A] = this match
    case Invalid(error) => Left(error)
    case Valid(value)   => Right(value)

trait Constraint[V]:
  def validate(value: V): Result[V]
  def coMap[A](f: A => V): Constraint[A] = (a: A) => this.validate(f(a)).map(_ => a)

object Constraint:
  def apply[A: Constraint as const]: Constraint[A] = const

  def check[A](predicate: A => Boolean, errorMessage: A => String): Constraint[A] = (a: A) =>
    if predicate(a) then Result.Valid(a)
    else Result.Invalid(Error.Single(errorMessage(a), List.empty))

  def notNull[A >: Null]: Constraint[A] = check(_ != null, _ => "must not be null")
  def min[N: Numeric](value: N): Constraint[N] = check[N](_ >= value, n => s"must be greater than or equal to $value, but was $n")
  def max[N: Numeric](value: N): Constraint[N] = check(_ <= value, n => s"must be less than or equal to $value, but was $n")

  object string:
    def notEmpty: Constraint[String] = check(_.nonEmpty, _ => "must not be empty")
    def notBlank: Constraint[String] = check(!_.isBlank, _ => "must not be blank")
    def size(min: Int = 0, max: Int = Int.MaxValue): Constraint[String] =
      check(s => s.length >= min && s.length <= max, s => s"size must be between $min and $max, but was ${s.length}")
    def pattern(regex: String): Constraint[String] = check(_.matches(regex), s => s"'$s' must match pattern '$regex'")

  given Constraint[String] = Result.Valid(_)
  given Constraint[Int] = Result.Valid(_)
  given Constraint[Long] = Result.Valid(_)
  given Constraint[Boolean] = Result.Valid(_)
  given Constraint[Double] = Result.Valid(_)
  given Constraint[Short] = Result.Valid(_)
  given Constraint[Float] = Result.Valid(_)

  inline given [P <: Product: Mirror.ProductOf as mirror]: Constraint[P] = product(
    summonAll[Tuple.Map[mirror.MirroredElemTypes, Constraint]].toList.toIndexedSeq.asInstanceOf[IndexedSeq[Constraint[Any]]]
  )
  inline given [T <: Tuple]: Constraint[T] = product(
    summonAll[Tuple.Map[T, Constraint]].toList.toIndexedSeq.asInstanceOf[IndexedSeq[Constraint[Any]]]
  )
  inline given namedTuple[K <: Tuple, V <: Tuple]: Constraint[NamedTuple[K, V]] = ntConst(
    labels = constValueTuple[K].toList.toIndexedSeq.asInstanceOf[IndexedSeq[String]],
    constraints = summonAll[Tuple.Map[V, Constraint]].toList.toIndexedSeq.asInstanceOf[IndexedSeq[Constraint[Any]]]
  )
  inline given [S: Mirror.SumOf as mirror]: Constraint[S] =
    sum(summonAll[Tuple.Map[mirror.MirroredElemTypes, Constraint]].toList.asInstanceOf[List[Constraint[Any]]])
  given [T: Constraint as constraint, S <: Seq[T]]: Constraint[S] = (value: S) =>
    labelsErrors(i => i.toString, _ => constraint.asInstanceOf[Constraint[Any]], value, value)
  given [T: Constraint as constraint]: Constraint[Map[String, T]] = (value: Map[String, T]) =>
    val values = value.toIndexedSeq
    labelsErrors(i => values(i)._1, _ => constraint.asInstanceOf[Constraint[Any]], values.map(_._2).toList, value)

  private def product[P <: Product](constraints: IndexedSeq[Constraint[Any]]): Constraint[P] = (p: P) =>
    labelsErrors(labels = p.productElementNames.toIndexedSeq, constraint = constraints, values = p.productIterator.toList, p)
  private def ntConst[K <: Tuple, V <: Tuple](labels: IndexedSeq[String], constraints: IndexedSeq[Constraint[Any]]): Constraint[NamedTuple[K, V]] =
    (t: NamedTuple[K, V]) => labelsErrors(labels, constraints, t.toList, t)
  private def sum[S: Mirror.SumOf as mirror](constraints: List[Constraint[Any]]): Constraint[S] = (s: S) =>
    constraints(mirror.ordinal(s)).validate(s).asInstanceOf[Result[S]]

  private def labelsErrors[T](labels: Int => String, constraint: Int => Constraint[Any], values: Seq[Any], value: T): Result[T] =
    val errors: Seq[Error] = values.zipWithIndex
      .map { (t: Any, i: Int) => constraint(i).validate(t).inPath(labels(i)) }
      .collect[Error] { case Result.Invalid(error) => error }
    if errors.nonEmpty then Result.Invalid(Error.Composite("", Nil, errors.toList))
    else Result.Valid(value)

object Validation:
  def apply[V: Constraint as constraint](value: V): Result[V] = constraint.validate(value)
  def apply[T <: Tuple](values: T)(using constraint: Constraint[T]): Result[T] = constraint.validate(values)
