object Logic {
  def matchLikeliHood(kitten: Kitten, prefs: UserPreferencies): Double = {
    val matches = prefs.attributes.map(kitten.attributes.contains(_)).map(if (_) 1.0 else 0.0)
    matches.sum / matches.length
  }
}
