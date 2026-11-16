package izumi.reflect.internal.bincompat

import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, Lambda, SymName}

private[reflect] trait LambdaObjBincompat {
  @deprecated("bincompat only", "3.1.0")
  private[internal] def apply(inputs: List[SymName.LambdaParamName], outputs: AbstractReference): Lambda = new Lambda(inputs, outputs)
}
