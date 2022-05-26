package io.getquill

import io.getquill.ast.*
import io.getquill.context.ExecutionType
import io.getquill.context.ExecutionType.{ Dynamic, Static }
import io.getquill.context.mirror.Row
import io.getquill.generic.{ GenericColumnResolver, GenericDecoder, GenericRowTyper }
import io.getquill.quat.quatOf
import io.getquill.{ QuotationLot, QuotationVase, Quoted, query, quote }
import org.scalatest.*

import scala.collection.mutable.LinkedHashMap
import scala.compiletime.{ constValue, erasedValue, summonFrom }
import scala.deriving.*
import scala.language.implicitConversions
import scala.quoted.*
import scala.reflect.{ ClassTag, classTag }

class GenericDecoderTest extends Spec {
  import GenericDecoderTest.*

  "domain-model product using row-typer" - {
    val ctx = new MirrorContext[MirrorSqlDialect, Literal](MirrorSqlDialect, Literal) with MirrorColumnResolving[MirrorSqlDialect, Literal]
    import ctx.{ *, given }

    given RowTyper[Shape] with
      def apply(row: Row) =
        row.apply[String]("type") match
          case "square" => classTag[Shape.Square]
          case "circle" => classTag[Shape.Circle]

    "test product type" in {
      val s = io.getquill.MirrorSession.default
      inline def q = quote { query[Shape].filter(s => s.id == 18) }
      val result = ctx.run(q)

      val squareRow = Row("type" -> "square", "id" -> 18, "radius" -> 890, "width" -> 123, "height" -> 456)
      result.extractor(squareRow, s) mustEqual Shape.Square(18, 123, 456)
      val circleRow = Row("type" -> "circle", "id" -> 18, "radius" -> 890, "width" -> 123, "height" -> 456)
      result.extractor(circleRow, s) mustEqual Shape.Circle(18, 890)
    }
  }

  "simple examples" - {
    val s = io.getquill.MirrorSession.default
    val ctx = new MirrorContext[MirrorSqlDialect, Literal](MirrorSqlDialect, Literal) with MirrorColumnResolving[MirrorSqlDialect, Literal]
    import ctx.{ *, given }

    "test tuple type" in {
      inline def q = quote { query[Person].map(p => (p.name, p.age)) }
      val result = ctx.run(q)

      val tupleRow = Row("_1" -> "Joe", "_2" -> 123)
      result.extractor(tupleRow, s) mustEqual ("Joe", 123)
    }

    "test case class type" in {
      inline def q = quote { query[Person] }
      val result = ctx.run(q)

      val tupleRow = Row("name" -> "Joe", "age" -> 123)
      result.extractor(tupleRow, s) mustEqual Person("Joe", 123)
    }
  }

  "joins" - {
    val s = io.getquill.MirrorSession.default
    val ctx = new SqlMirrorContext[MirrorSqlDialect, Literal](MirrorSqlDialect, Literal)
    import ctx.{ *, given }

    "inner join" in {
      inline def q = quote { query[Person].join(query[Address]).on((p, a) => p.name == a.personName)}
      val result = ctx.run(q)

       val tupleRow = Row.fromList("Joe", 123, "Joe", "Database Row 23")
      result.extractor(tupleRow, s) mustEqual (Person("Joe", 123), Address("Joe", "Database Row 23"))
    }

    "left join (some)" in {
      inline def q = quote { query[Person].leftJoin(query[Address]).on((p, a) => p.name == a.personName)}
      val result = ctx.run(q)

       val tupleRow = Row.fromList("Joe", 123, "Joe", "Database Row 23")
      result.extractor(tupleRow, s) mustEqual (Person("Joe", 123), Some(Address("Joe", "Database Row 23")))
    }

    "left join (none)" in {
      inline def q = quote { query[Person].leftJoin(query[Address]).on((p, a) => p.name == a.personName)}
      val result = ctx.run(q)

       val tupleRow = Row.fromList("Joe", 123, null, null)
      result.extractor(tupleRow, s) mustEqual (Person("Joe", 123), None)
    }
  }
}
object GenericDecoderTest {
  case class Person(name: String, age: Int)
  case class Address(personName: String, street: String)

  enum Shape(val id: Int):
    case Square(override val id: Int, width: Int, height: Int) extends Shape(id)
    case Circle(override val id: Int, radius: Int) extends Shape(id)
}
