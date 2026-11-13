package izumi.reflect.examples.storeandexchangetypetags

import izumi.reflect.macrortti.LightTypeTag

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.util.Base64

final case class StoredType(hash: Int, ref: String, databases: String, version: Int)

object StoredTypes {
  def toBase64(binary: String): String =
    Base64.getEncoder.encodeToString(binary.getBytes(ISO_8859_1))

  def store(tag: LightTypeTag): StoredType = {
    val s = tag.serialize()
    StoredType(s.hash, toBase64(s.ref), toBase64(s.databases), s.version)
  }

  def fromBase64(text: String): String =
    new String(Base64.getDecoder.decode(text), ISO_8859_1)

  def load(stored: StoredType): LightTypeTag =
    LightTypeTag.parse(LightTypeTag.Serialized(stored.hash, fromBase64(stored.ref), fromBase64(stored.databases), stored.version))
}
