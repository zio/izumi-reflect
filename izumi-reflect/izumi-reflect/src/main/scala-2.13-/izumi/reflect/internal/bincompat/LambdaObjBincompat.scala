package izumi.reflect.internal.bincompat

import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, Lambda, SymName}

private[reflect] abstract class LambdaObjBincompat extends scala.runtime.AbstractFunction2[List[SymName.LambdaParamName], AbstractReference, Lambda] {
  @deprecated("Lambda constructor is deprecated, use Lambda.make", "3.1.0")
  override def apply(inputs: List[SymName.LambdaParamName], outputs: AbstractReference): Lambda = new Lambda(inputs, outputs)
}
