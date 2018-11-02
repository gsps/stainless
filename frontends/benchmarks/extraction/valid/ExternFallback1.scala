import stainless.lang._
import stainless.annotation._

import scala.annotation.meta.field

object ExternFallback1 {

  import scala.collection.concurrent.TrieMap

  @extern
  def getTrieMap(x: BigInt): TrieMap[BigInt, String] = TrieMap.empty

  @extern
  def setTrieMap(trie: TrieMap[BigInt, String]): Unit = ()

  case class Wrapper[K, V](
    @(extern @field)
    theMap: TrieMap[K, V]
  ) {
    @extern
    def getMap: TrieMap[K, V] = theMap

    @extern
    def setMap(map: TrieMap[K, V]): Unit = ()
  }

  def prop2 = {
    val wrapper = Wrapper(getTrieMap(1))
    wrapper.setMap(wrapper.getMap)
    assert(wrapper.getMap == getTrieMap(1))
  }
}