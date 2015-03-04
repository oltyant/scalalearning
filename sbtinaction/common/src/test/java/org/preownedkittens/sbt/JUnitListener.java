package org.preownedkittens.sbt;

import org.junit.*;
import java.io.*;
import org.junit.runner.*;
import org.junit.runner.notification.*;

public class JUnitListener extends RunListener {
	private PrintWriter pw;
	private boolean testFailed;
	private String outputFile = System.getProperty("junit.output.file");

	@Override
	public void testRunStarted(Description description) throws Exception {
		pw = new PrintWriter(new FileWriter(outputFile));
		pw.println("<html><head><title>JUnit report</title></head><body>");
	}

	@Override
	public void testRunFinished(Result result) throws Exception {
		pw.println("</body></html>");
		pw.close();
	}

	@Override
	public void testStarted(Description description) throws Exception {
		pw.print("<p> Test " + description.getDisplayName() + " ");
		testFailed = false;
	}

	@Override
	public void testFinished(Description description) throws Exception {
		if (!testFailed) {
			pw.print("OK");
		}
		pw.println("</p>");
	}

	@Override
	public void testFailure(Failure failure) throws Exception {
		testFailed = true;
		pw.print("FAILED!");
	}

	@Override
	public void testAssumptionFailure(Failure failure) {
		pw.print("ASSUMPTION FAILURE");
	}

	@Override
	public void testIgnored(Description description) throws Exception {
		pw.print("IGNORED");
	}
}