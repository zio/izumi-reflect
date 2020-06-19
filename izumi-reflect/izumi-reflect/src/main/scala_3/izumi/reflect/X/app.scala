package izumi.reflect.X

import izumi.reflect.Tag

object app extends App {
  izumi.reflect.dottyreflection.Inspect.inspect[(Double, Double)]
}
