package org.preownedkittens

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck._

object LogicSpecification extends Properties("Logic") {
	val allAttributes = Array("Harlequin", "Tortoiseshell", "Siamese", "Alien", "Rough", "Tom", "Sad", "Overweight")

	val genKitten: Gen[Kitten] = for {
		attributes <- Gen.containerOf[Set, String](Gen.oneOf(allAttributes))
	} yield Kitten("1", attributes.toSeq)

	val genBuyerPreferences: Gen[UserPreferencies] = (for {
		attributes <- Gen.containerOf[Set, String](Gen.oneOf(allAttributes))
	} yield UserPreferencies(attributes.toSeq))

	def matches(x: String, a: Kitten): Double = if (a.attributes.contains(x)) 1.0 else 0.0

	property("matchLikeliHood") = forAll(genKitten, genBuyerPreferences) {
		(a: Kitten, b: UserPreferencies) => {
			if (b.attributes.size == 0) true
			else {
				val num = b.attributes.map(matches(_, a)).sum
				num / b.attributes.size - Logic.matchLikeliHood(a, b) < 0.001
			}
		}
	}
}