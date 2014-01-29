package org.kiji.modeling

import us.bpsm.edn.parser._
import us.bpsm.edn.Tag
import us.bpsm.edn.parser.CollectionBuilder.Factory
import scala.io.Source

object EdnConf {
  def main(args: Array[String]): Unit = {

    val source = Source.fromFile("conf.edn")
    val toPrint = parsing.parse(source.bufferedReader()).toList.first

//    val toPrint = parsing.parse("#org.kiji.modeling.conf/express-job{:name \"foobar\" :job-class \"foo\" :dependencies #{\"foo\" \"bar\"} :options {}}")
//    val toPrint = parsing.parse("#org.kiji.modeling.conf/express-job[1 2 3]").first
//    val toPrint = validation.validateType[Map[String, String]](List(1, 2, 3))

//    val mapValidator = validateMap[Int, CharSequence](
//        required(1 -> validateType[CharSequence]),
//        optional(4 -> validateType[Utf8])) _

//    val toPrint = mapValidator(SortedMap(1 -> "foo", 2 -> "bar", 3 -> "baz", 4 -> "foo"))

    println(toPrint)
  }

  private val TagPrefix = "org.kiji.modeling.conf"
  private def tag(name: String) = us.bpsm.edn.Tag.newTag(TagPrefix, name)
  private def keyword(name: String) = Symbol(":" + name)
  private def keyword(prefix: String, name: String) = Symbol(":" + name)

  /**
   * Indicates a type which can be interpreted as edn.
   */
  trait EdnType {
    def tag: Tag
  }

  /**
   * A marker trait for a task. Tasks have a uniquely identifying name.
   */
  trait Task extends EdnType {
    /** The name of this tasks.  Must be uniquely identifying. */
    def name: String
  }

  /** A reference to a task.  The `name` of this task identifies the referenced task. */
  trait TaskRef extends Task

  /**
   * A reference to an ExpressJob with the given name.
   */
  case class ExpressJobRef(name: String) extends TaskRef {
    def tag: Tag = ExpressJobRef.Tag
  }
  object ExpressJobRef {
    val Tag = tag("express-job-ref")
  }

  /**
   * A configuration for an ExpressJob.
   */
  case class ExpressJob(
    name: String,
    jobClass: String,
    dependencies: Set[String],
    options: Map[Any, Any]
  ) extends Task {
    def tag: Tag = ExpressJob.Tag
  }
  object ExpressJob {
    val Tag = tag("express-job")
  }

  object parsing {
    def parse(text: Readable): Seq[Any] =
    new ParseIterator(Parsers.newParser(parserConfig), Parsers.newParseable(text)).toStream

    def parse(text: CharSequence): Seq[Any] =
      new ParseIterator(Parsers.newParser(parserConfig), Parsers.newParseable(text)).toStream

    private class ParseIterator(parser: Parser, parseable: Parseable) extends Iterator[Any] {
      var next: Any = _
      override def hasNext: Boolean = {
        next = toScala(parser.nextValue(parseable))
        next != Parser.END_OF_INPUT
      }
    }

    private def parserConfig: Parser.Config =
      Parsers
        .newParserConfigBuilder()
        .setListFactory(ListFactory)
        .setVectorFactory(VectorFactory)
        .setMapFactory(MapFactory)
        .setSetFactory(SetFactory)
        .putTagHandler(ExpressJob.Tag, ExpressJobHandler)
        .build()

    private object ExpressJobHandler extends TagHandler {
      def transform(tag: Tag, value: AnyRef): AnyRef = validation.validateExpressJob(tag, value)
    }

    def toScala(obj: Any): Any = obj match {
      case keyword: us.bpsm.edn.Keyword => Symbol(keyword.toString)
      case symbol: us.bpsm.edn.Symbol => Symbol(symbol.toString)
      case tagged: us.bpsm.edn.TaggedValue => (Symbol(tagged.getTag.toString), toScala(tagged.getValue))
      case bool: java.lang.Boolean => bool.booleanValue()
      case byte: java.lang.Byte => byte.byteValue()
      case chars: java.lang.CharSequence => chars.toString
      case char: java.lang.Character => char.charValue()
      case double: java.lang.Double => double.doubleValue()
      case float: java.lang.Float => float.floatValue()
      case int: java.lang.Integer => int.intValue()
      case long: java.lang.Long => long.longValue()
      case short: java.lang.Short => short.shortValue()
      case bigint: java.math.BigInteger => new BigInt(bigint)
      case bigdec: java.math.BigDecimal => new BigDecimal(bigdec)
      // case list: List[Any] => // Handled by ListFactory
      // case vector: Vector[Any] => // Handled by VectorFactory
      // case map: Map[Any, Any] => // Handled by MapFactory
      // case set: Set[Any] => // Handled by SetFactory
      case other => other
    }

    private object ListFactory extends Factory {
      def builder(): CollectionBuilder = new CollectionBuilder {
        private[this] val builder = List.newBuilder[Any]
        def add(elem: Any): Unit = builder += toScala(elem)
        def build(): List[Any] = builder.result()
      }
    }

    private object VectorFactory extends Factory {
      def builder(): CollectionBuilder = new CollectionBuilder {
        private[this] val builder = Vector.newBuilder[Any]
        def add(elem: Any): Unit = builder += toScala(elem)
        def build(): Vector[Any] = builder.result()
      }
    }

    private object MapFactory extends Factory {
      def builder(): CollectionBuilder = new CollectionBuilder {
        private[this] val builder = Map.newBuilder[Any, Any]
        private[this] var key: Option[Any] = None
        // Builder#add will be called successively with the key and then the value
        def add(keyOrValue: Any): Unit = key match {
          case Some(k) => {
            builder += toScala(k) -> toScala(keyOrValue)
            key = None
          }
          case None => key = Some(keyOrValue)
        }
        def build(): Map[Any, Any] = {
          require(key.isEmpty) // Sanity check
          builder.result()
        }
      }
    }

    private object SetFactory extends Factory {
      def builder(): CollectionBuilder = new CollectionBuilder {
        private[this] val builder = Set.newBuilder[Any]
        def add(elem: Any): Unit = builder += toScala(elem)
        def build(): Set[Any] = builder.result()
      }
    }
  }

  object validation {

    // Type class to hold validation errors
    // pseudo-haskell: String | (String, ValidationError) | List[ValidationError]
    /**
     * A type class to hold validation errors.  In pseudo-haskell this would be equivalent to:
     *    String | (String, ValidationError) | List[ValidationError]
     *
     * The print method is provided for pretty printing hierarchical error messages in a yaml-like
     * format.
     *
     * The idea here is that ValidationError is a tree.  Nodes are either an edge node (String),
     * or interior node (Tuple2[String, ValidationError] or List[ValidationError]).
     */
    sealed trait ValidationError {
      def print: List[String]
    }
    implicit def StringValidationError(error: String): ValidationError = new ValidationError {
      override def print: List[String] = List("- " + error)
    }
    implicit def TupleValidationError[T <% ValidationError](tuple: (String, T)): ValidationError = new ValidationError {
      override def print: List[String] = tuple._1.print ++ tuple._2.print.map(s => '\t' + s)
    }
    implicit def ListValidationError[T <% ValidationError](list: List[T]): ValidationError = new ValidationError {
      override def print: List[String] = list.flatMap(_.print)
    }
    class ValidationException(message: String) extends Exception {
      override def getMessage: String = message
    }
    def validate_![T](either: Either[ValidationError, T]): T = either match {
      case Left(error) => throw new ValidationException(error.print.mkString("\n", "\n", "\n"))
      case Right(t) => t
    }

    /**
     * Validates that the object is an instance of the provided type or a subtype of the provided
     * type. Note that because of erasure, this method does not check generics.
     */
    def validateType[T : Manifest](obj: Any): Either[ValidationError, T] = obj match {
      // specializations are necessary because obj.getClass will return an AnyRef class instead of an AnyVal class
      case b: Byte if manifest[T].erasure isAssignableFrom classOf[Byte] => Right(b.asInstanceOf[T])
      case s: Short if manifest[T].erasure isAssignableFrom classOf[Short] => Right(s.asInstanceOf[T])
      case i: Int if manifest[T].erasure isAssignableFrom classOf[Int] => Right(i.asInstanceOf[T])
      case l: Long if manifest[T].erasure isAssignableFrom classOf[Long] => Right(l.asInstanceOf[T])
      case f: Float if manifest[T].erasure isAssignableFrom classOf[Float] => Right(f.asInstanceOf[T])
      case d: Double if manifest[T].erasure isAssignableFrom classOf[Double] => Right(d.asInstanceOf[T])
      case c: Char if manifest[T].erasure isAssignableFrom classOf[Double] => Right(c.asInstanceOf[T])
      case b: Boolean if manifest[T].erasure isAssignableFrom classOf[Double] => Right(b.asInstanceOf[T])
      case t if manifest[T].erasure isAssignableFrom obj.getClass => Right(t.asInstanceOf[T])
      case null => Left("Invalid type: expected a %s, but was instead null."
        .format(manifest[T].erasure.getSimpleName))
      case _ => Left("Invalid type: Expected a %s, but was instead a %s: %s."
        .format(manifest[T].erasure.getSimpleName, obj.getClass.getSimpleName, obj))
    }

    /**
     * Validates that the provided iterable contains elements of the provided type or a subtype of
     * the provided type.  Note that because of erasure, this method does not check the type
     * parameters of generic elements (for instance, this will check that an Iterable[(Int, String)]
     * contains Tuple2 elements, but it will not validate that the contents of the Tuple2's are
     * Ints and Strings).
     */
    def validateElementTypes[E : Manifest](coll: Iterable[Any]): Either[ValidationError, Iterable[E]] =
      flatten[E](coll.map(validateType[E]))

    def validateTrue[T]: T => Either[ValidationError, T] = Right.apply

    def validateList[E : Manifest](validator: (E => Either[ValidationError, E]) = validateTrue[E])(obj: Any): Either[ValidationError, List[E]] = {
      // Check that the object is a List
      validateType[List[_]](obj)
        .right.flatMap { list: List[_] =>
          // Check the element types
          validateElementTypes[E](list)
            .left.map[ValidationError](error => "Invalid element types in list:" -> error)
            .right.flatMap { elements: Iterable[E] =>
              flatten(elements.map(validator), "Invalid elements in list:")
                .right.map(x => obj.asInstanceOf[List[E]])
            }
        }
    }

    def validateVector[E : Manifest](validator: E => Either[ValidationError, E] = validateTrue[E])(obj: Any): Either[ValidationError, Vector[E]] = {
      // Check that the object is a List
      validateType[Vector[_]](obj).right.flatMap { vector: Vector[_] =>
        // Check the element types
        validateElementTypes[E](vector)
          .left.map[ValidationError](error => "Invalid element types in vector: " -> error)
          .right.flatMap { elements: Iterable[E] =>
            flatten(elements.map(validator), "Invalid elements in vector:")
              .right.map(x => obj.asInstanceOf[Vector[E]])
          }
      }
    }

    def validateSet[E : Manifest](validator: E => Either[ValidationError, E] = validateTrue[E])(obj: Any): Either[ValidationError, Set[E]] = {
      // Check that the object is a List
      validateType[Set[_]](obj).right.flatMap { set: Set[_] =>
        // Check the element types
        validateElementTypes[E](set)
          .left.map[ValidationError](error => "Invalid element types in set:" -> error)
          .right.flatMap { elements: Iterable[E] =>
            flatten(elements.map(validator), "Invalid elements in set:")
              .right.map(x => obj.asInstanceOf[Set[E]])
        }
      }
    }

    /**
     * Validates that the provided object is a Map with the specified key and value types, and
     * validates specified entries in the map.
     */
    def validateMap[K : Manifest, V : Manifest]
    (keyValidators: (K, (Option[V] => Either[ValidationError, V]))*)
      (obj: Any): Either[ValidationError, Map[K, V]] = {

      // Check that the object is a Map
      validateType[Map[_, _]](obj).right.flatMap { map: Map[_, _] =>

      // Check that the type of each key and value in the map is well-typed
        val keyTypesValidation: Either[ValidationError, Iterable[K]] =
          validateElementTypes[K](map.keys)
            .left.map(error => "Invalid key types in map:" -> error)
        val valueTypesValidation: Either[ValidationError, Iterable[V]] =
          validateElementTypes[V](map.values)
            .left.map(error => "Invalid value types in map:" -> error)

        flatten(List(keyTypesValidation, valueTypesValidation))
          .right.flatMap { _ =>
            // We know the map is well-typed, so we can cast
            val typedMap: Map[K, V] = map.asInstanceOf[Map[K, V]]
            flatten(keyValidators.map { case (key, validator) => validator(typedMap.get(key)) },
                    "Map contains invalid entries:")
              .right.map(_ => typedMap)
        }
      }
    }

    val NameKey = keyword("name")
    val JobClassKey = keyword("job-class")
    val DependenciesKey = keyword("dependencies")
    val OptionsKey = keyword("options")

    def validateExpressJob(tag: Tag, obj: Any): ExpressJob = {
      require(tag == ExpressJob.Tag) // Sanity check

      val validation = validateMap[Symbol, Any](
          required(NameKey -> validateType[String]),
          required(JobClassKey -> validateType[String]),
          required(DependenciesKey -> validateSet[String]()),
          required(OptionsKey -> validateMap[Any, Any]())
          )(obj)
        .left.map[ValidationError](error => "Invalid express-job: " -> error)
        .right.map { map =>
          ExpressJob(
            map(NameKey).asInstanceOf[String],
            map(JobClassKey).asInstanceOf[String],
            map(DependenciesKey).asInstanceOf[Set[String]],
            map(OptionsKey).asInstanceOf[Map[Any, Any]])
      }
      validate_!(validation)
    }

    def required[K, V](kv: (K, V => Either[ValidationError, V])): (K, (Option[V] => Either[ValidationError, V])) = {
      val (key, validator) = kv
      key -> {
        case Some(v) => validator(v)
        case None => Left("Required key %s missing.".format(key))
      }
    }

    def optional[K, V](kv: (K, V => Either[ValidationError, V])): (K, (Option[V] => Either[ValidationError, V])) = {
      val (key, validator) = kv
      key -> {
        case Some(v) => validator(v)
        case None => Right(null.asInstanceOf[V])
      }
    }

    def flatten[T](validations: Iterable[Either[ValidationError, T]]) = doFlatten(validations, None)
    def flatten[T](validations: Iterable[Either[ValidationError, T]], error: String) = doFlatten(validations, Some(error))

    /**
     * Iterable[Either[ValidationError, T]] => Either[ValidationError, Iterable[T]]
     */
    private def doFlatten[T](
      validations: Iterable[Either[ValidationError, T]],
      error: Option[String]
    ): Either[ValidationError, Iterable[T]] = {
      if (validations.forall(_.isRight)) Right(validations.map(_.right.get))
      else {
        val errors: List[ValidationError] = validations.filter(_.isLeft).map(_.left.get).toList
        Left(error.map[ValidationError](e => e -> errors).getOrElse(errors))
      }
    }
  }
}
