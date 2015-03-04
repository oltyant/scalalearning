package org.preownedkittens;

import org.junit.*;
import scala.collection.immutable.*;

public class LogicJavaTest {
	@Test
	public void testKitte() {
		Kitten kitten = new Kitten("1", (new scala.collection.immutable.HashSet<String>()).toSeq());
		Assert.assertEquals(0, kitten.attributes().size());
	}
}