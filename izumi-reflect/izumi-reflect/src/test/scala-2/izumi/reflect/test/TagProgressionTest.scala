package izumi.reflect.test

import org.scalatest.exceptions.TestFailedException

class TagProgressionTest extends SharedTagProgressionTest {

  "[progression] Tag (Scala 2)" should {

    "progression test: Scala 2 fails to `support HKTag for higher-kinded type lambdas with inner and outer type bounds`" in {
      val t = intercept[TestFailedException](assertCompiles("""
      import izumi.reflect.{HKTag, Tag}

      trait Trait3[T <: Dep]
      trait TraitK3[T[x <: Dep] <: Trait3[x]]

      type `TagK<:Dep`[K[_ <: Dep]] = HKTag[{ type Arg[A <: Dep] = K[A] }]
      type `TagKT<:Dep`[K[T[x <: Dep] <: Trait3[x]]] = HKTag[{ type Arg[T[x <: Dep] <: Trait3[x]] = K[T] }]

      def t[K[F[x <: Dep] <: Trait3[x]]: `TagKT<:Dep`, T[x <: Dep] <: Trait3[x]: `TagK<:Dep`] = Tag[K[T]]

      assert(t[TraitK3, Trait3].tag == Tag[TraitK3[Trait3]].tag)
      """))
      assert(t.message.get.contains("could not find implicit value"))
      assert(t.message.get.contains("deriving"))
    }

  }

}
