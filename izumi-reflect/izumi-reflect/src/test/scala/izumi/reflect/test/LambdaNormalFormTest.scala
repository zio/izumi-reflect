/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package izumi.reflect.test

import izumi.reflect.macrortti._
import izumi.reflect.macrortti.LightTypeTagRef._
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{LambdaParamName, SymTypeName}

class LambdaNormalFormTest extends TagAssertions {

  import TestModel._

  "lambda normal form (#379)" should {

    "produce primary-compatible normal forms for outer and inner nested lambdas" in {
      // Models: λ %0:0/1 -> Applicative3[λ %1:0/3, %1:1/3, %1:2/3 -> 0:0/1[1:1, 1:2]]
      val outerParam = LambdaParamName(0, 0, 1)
      val innerP0 = LambdaParamName(0, 1, 3)
      val innerP1 = LambdaParamName(1, 1, 3)
      val innerP2 = LambdaParamName(2, 1, 3)

      val innerBody = FullReference(
        symName = outerParam,
        parameters = List(
          TypeParam(NameReference(innerP1), Variance.Invariant),
          TypeParam(NameReference(innerP2), Variance.Invariant)
        )
      )
      val innerLambda = Lambda(List(innerP0, innerP1, innerP2), innerBody)
      val outerBody = FullReference(
        symName = SymTypeName("Applicative3"),
        parameters = List(TypeParam(innerLambda, Variance.Invariant))
      )
      val outerLambda = Lambda(List(outerParam), outerBody)

      // 1. Outer normal form retains 0:0/1 and 1:0/3..1:2/3 without fake placeholders.
      assert(outerLambda.normalizedOutput == outerBody)

      // 2. Inner normal form shifts outer parameter to -1:0/1 and inner parameters to 0:0/3..0:2/3.
      val expectedInnerOutput = FullReference(
        symName = LambdaParamName(0, -1, 1),
        parameters = List(
          TypeParam(NameReference(LambdaParamName(1, 0, 3)), Variance.Invariant),
          TypeParam(NameReference(LambdaParamName(2, 0, 3)), Variance.Invariant)
        )
      )
      assert(innerLambda.normalizedOutput == expectedInnerOutput)
      assert(innerLambda.normalized.input == List(LambdaParamName(0, 0, 3), LambdaParamName(1, 0, 3), LambdaParamName(2, 0, 3)))
    }

    "normalize multi-level outer context depths (-2, -1, 0) and IT1[List] inner lambda depth" in {
      val level0Param = LambdaParamName(0, 0, 1)
      val level1Param = LambdaParamName(0, 1, 1)
      val level2Param = LambdaParamName(0, 2, 1)

      val level2Lambda = Lambda(
        List(level2Param),
        FullReference(
          symName = level0Param,
          parameters = List(
            TypeParam(NameReference(level1Param), Variance.Covariant),
            TypeParam(NameReference(level2Param), Variance.Covariant)
          )
        )
      )
      val expectedLevel2 = FullReference(
        symName = LambdaParamName(0, -2, 1),
        parameters = List(
          TypeParam(NameReference(LambdaParamName(0, -1, 1)), Variance.Covariant),
          TypeParam(NameReference(LambdaParamName(0, 0, 1)), Variance.Covariant)
        )
      )
      assert(level2Lambda.normalizedOutput == expectedLevel2)

      val it1ListTag = LTT[IT1[List]]
      val innerListLambda = it1ListTag.ref.asInstanceOf[FullReference].parameters.head.ref.asInstanceOf[Lambda]
      assert(innerListLambda.normalized.input == List(LambdaParamName(0, 0, 1)))
      assert(innerListLambda.normalized.toString == "λ %0 → List[+0]")
    }

  }

}
