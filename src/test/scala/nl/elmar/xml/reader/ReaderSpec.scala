package nl.elmar.xml.reader

import cats.data.NonEmptyList
import org.scalatest.WordSpec
import org.scalatest.Matchers._

class ReaderSpec extends WordSpec {

  def provide = afterWord("provide")
  def support = afterWord("support")
  def read = afterWord("read")

  "Reader" should provide {
      "readers for primitive types" that read {
        "string" in {
          stringReader(<node>content</node>) should be (
            valid("content")
          )
        }
        "long" in {
          longReader(<node>4234</node>) should be (
            valid(4234)
          )
        }
        "boolean" in {
          booleanReader(<node>true</node>) should be (
            valid(true)
          )
          booleanReader(<node>false</node>) should be (
            valid(false)
          )
        }
      }
      "readers for List and Option" that read {
        "List[A: Reader]" in {
          val reader = listReader[Long]
          reader(
              <item>1</item>
              <item>2</item>
              <item>3</item>
          ) should be (
            valid(List(1, 2, 3))
          )
        }
        "Option[A: Reader]" in {
          val reader = optionReader[Boolean]
          reader(Nil) shouldBe valid(None)
          reader(<node>true</node>) shouldBe valid(Some(true))
        }
      }
    }

   "Reader" should support {
      "cartesian syntax because it is Applicative" in {
        import XmlPath.__
        import cats.syntax.cartesian._
        val reader: Reader[(String, Int)] =
        (__ \ "name")[String] |@|
        (__ \ "age")[Int] map ((_, _))
        reader(
          <person>
            <name>John</name>
            <age>39</age>
          </person>
        ) shouldBe valid(("John", 39))
      }
      "composition because it is Kleisli" in {
        val reader: Reader[Boolean] = longReader.map( n => if (n > 10) true else false)
        reader(<node>11</node>) should be (valid(true))
        reader(<node>5</node>) should be (valid(false))
      }

     "`orElse` combinator" in {
       import XmlPath.__

       val reader: Reader[String] =
         (__ \ "user" \ "name")[String]
           .orElse((__ \ "name")[String])

       reader(
         <person>
           <name>John</name>
         </person>
       ) shouldBe valid("John")

       reader(
         <person>
           <user>
             <name>Bill</name>
           </user>
         </person>
       ) shouldBe valid("Bill")

       reader(
         <person>
           <user>John</user>
         </person>
       ) shouldBe invalid("there must be one node containing Text node inside", XmlPath(List("name")))
     }

     "`orEmpty` combinator" in {
       import XmlPath.__
       import cats.instances.list._

       val reader: Reader[List[String]] =
         (__ \ "name").list[String].orEmpty((__ \ "item").list[String])

       reader(
         <items>
           <item>1</item>
           <item>2</item>
           <item>3</item>
         </items>
       ) shouldBe valid(List("1", "2", "3"))

       reader(
         <items>
           <name>John</name>
           <name>Bill</name>
         </items>
       ) shouldBe valid(List("John", "Bill"))

       reader(
         <person>
           <user>John</user>
         </person>
       ) shouldBe valid(List.empty)
     }


     "`nel` combinator" in {
       import XmlPath.__

       val reader: Reader[NonEmptyList[String]] = (__ \ "item").nel[String]

       reader.run(
         <items>
           <item>1</item>
           <item>2</item>
           <item>3</item>
         </items>
       ) shouldBe valid(NonEmptyList.of("1", "2", "3"))

       val reader2: Reader[NonEmptyList[String]] = (__ \ "persons" \ "person").nel[String]

       reader2(
         <global>
           <persons>
             <person>Bill</person>
             <person>John</person>
           </persons>
         </global>
       ) shouldBe valid(NonEmptyList.of("Bill", "John"))

       reader.orElse(reader2)(
         <global>
           <persons>
             <person>Bill</person>
             <person>John</person>
           </persons>
         </global>
       ) shouldBe valid(NonEmptyList.of("Bill", "John"))

       reader.orElse(reader2)(
           <items>
             <item>1</item>
             <item>2</item>
             <item>3</item>
           </items>
       ) shouldBe valid(NonEmptyList.of("1", "2", "3"))

       reader(<test></test>) shouldBe invalid("node cannot be empty", XmlPath(List("item")))
     }
   }
}
