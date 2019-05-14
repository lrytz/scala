/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt;

import java.util.ArrayList;

import scala.Some;

import xsbti.Logger;

import dotty.tools.dotc.core.Contexts.Context;
import dotty.tools.repl.ReplDriver;
import dotty.tools.repl.State;

public class ConsoleInterface {
  public String[] commandArguments(String[] args, String bootClasspathString, String classpathString, Logger log) {
    return args;
  }

  public void run(
    String[] args,
    String bootClasspathString,
    String classpathString,
    String initialCommands,
    String cleanupCommands,
    ClassLoader loader,
    String[] bindNames,
    Object[] bindValues,
    Logger log
  ) {
    ArrayList<String> completeArgsList = new ArrayList<>();
    for (String arg : args)
      completeArgsList.add(arg);
    if (!bootClasspathString.isEmpty()) {
      completeArgsList.add("-bootclasspath");
      completeArgsList.add(bootClasspathString);
    }
    completeArgsList.add("-classpath");
    completeArgsList.add(classpathString);
    String[] completeArgs = completeArgsList.toArray(args);

    ReplDriver driver = new ReplDriver(completeArgs, System.out, Some.apply(loader));

    State state = driver.initialState();
    assert bindNames.length == bindValues.length;
    for (int i = 0; i < bindNames.length; i++)
      state = driver.bind(bindNames[i], bindValues[i], state);

    state = driver.run(initialCommands, state);
    // TODO handle failure during initialisation
    state = driver.runUntilQuit(state);
    driver.run(cleanupCommands, state);
  }
}
