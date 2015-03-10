package object chapter1 {
  def base36Decode(source: BigInt): String = {
    source.toString(36)
  }

  def base36Encode(from: String): BigInt = {
    if (from.isEmpty) BigInt(0)
    else from.toList.map(x => BigInt("0123456789abcdefghijklmnopqrstuvwxyz".indexOf(x))).reduceLeft(_ * 36 + _)
  }
}
