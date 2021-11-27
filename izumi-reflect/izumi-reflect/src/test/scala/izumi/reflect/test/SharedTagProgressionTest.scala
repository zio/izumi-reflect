package izumi.reflect.test

import izumi.reflect.test.TestModel.*
import izumi.reflect.{Tag, TagK3, TagKK}
import org.scalatest.wordspec.AnyWordSpec

abstract class SharedTagProgressionTest extends AnyWordSpec with TagAssertions with TagProgressions {

  "[progression] Tag (all versions)" should {

    "progression test: reported in https://github.com/zio/izumi-reflect/issues/82, fails to soundly convert trifunctor hkt to bifunctor when combining tags" in {
      def tag[F[-_, +_, +_]: TagK3] = Tag[BIO2[F[Any, +*, +*]]]

      doesntWorkYetOnScala2 {
        assertChild(tag[ZIO].tag, Tag[BIO2[IO]].tag)
      }
      doesntWorkYetOnScala2 {
        assertChild(Tag[BIO2[IO]].tag, tag[ZIO].tag)
      }
      doesntWorkYetOnScala2 {
        assertSame(tag[ZIO].tag, Tag[BIO2[IO]].tag)
      }
    }

    "progression test: reported in https://github.com/zio/izumi-reflect/issues/83, fails to convert trifunctor tag to bifunctor tag" in {
      def direct[F[+_, +_]: TagKK] = Tag[BIO2[F]]
      def indirectFrom3[F[-_, +_, +_]: TagK3] = direct[F[Any, +*, +*]]

      doesntWorkYetOnScala2 {
        assertSame(direct[ZIO[Any, +*, +*]].tag, indirectFrom3[ZIO].tag)
      }
    }

  }

}
