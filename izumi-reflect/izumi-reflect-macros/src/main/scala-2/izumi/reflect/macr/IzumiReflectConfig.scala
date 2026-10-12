package izumi.reflect.macr

object IzumiReflectConfig {
  // Global switch for all caches
  final val enableAllCaches: Boolean = sys.props.getOrElse("izumi.reflect.enable.all.caches", "true").toBoolean

  // Macro-level cache (Scala 2 only)
  final val enableMacroLevelCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.macro.cache", "true").toBoolean

  // LightTypeTag-level cache (Scala 2 only)
  final val enableLightTypeTagCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.ltt.cache", "true").toBoolean

  // Serialized LightTypeTag-level cache (Scala 2 only)
  final val enableSerializedLttCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.serialized.ltt.cache", "true").toBoolean

  // AbstractReference-level cache (Scala 2 only)
  final val enableAbstractReferenceCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.abstract.ref.cache", "true").toBoolean

  // BasesDb cache (Scala 2 only)
  final val enableBasesDbCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.bases.db.cache", "true").toBoolean

  // InheritanceDb cache (Scala 2 only)
  final val enableInheritanceDbCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.inheritance.db.cache", "true").toBoolean
}
