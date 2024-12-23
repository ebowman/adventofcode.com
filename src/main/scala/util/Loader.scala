package util

object Loader:
  def is(obj: AnyRef, resource: String): IndexedSeq[String] = apply(obj, resource).toIndexedSeq

  /** Load a file from src/main/resources/[package] into an iterable of strings */
  def apply(obj: AnyRef, resource: String): Iterable[String] =
    import scala.io.Source
    Source.fromInputStream(obj.getClass.getResourceAsStream(resource)).getLines().iterator.to(Iterable)
end Loader

