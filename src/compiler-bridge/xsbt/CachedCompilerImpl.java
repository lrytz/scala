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

import xsbti.AnalysisCallback;
import xsbti.Logger;
import xsbti.Reporter;
import xsbti.Problem;
import xsbti.Severity;
import xsbti.compile.*;

import java.io.File;
import scala.tools.nsc.ZincMain;
import scala.tools.nsc.ZincMainCallback;

public class CachedCompilerImpl implements CachedCompiler {
  private final String[] args;
  private final Output output;
  private final String[] outputArgs;

  public CachedCompilerImpl(String[] args, Output output) {
    super();
    this.args = args;
    this.output = output;

    if (!(output instanceof SingleOutput))
      throw new IllegalArgumentException("output should be a SingleOutput, was a " + output.getClass().getName());

    this.outputArgs =
      new String[] { "-d", ((SingleOutput) output).getOutputDirectory().getAbsolutePath().toString() };
  }

  public String[] commandArguments(File[] sources) {
    String[] sortedSourcesAbsolute = new String[sources.length];
    for (int i = 0; i < sources.length; i++)
      sortedSourcesAbsolute[i] = sources[i].getAbsolutePath();
    java.util.Arrays.sort(sortedSourcesAbsolute);

    // Concatenate outputArgs, args and sortedSourcesAbsolute
    String[] result = new String[outputArgs.length + args.length + sortedSourcesAbsolute.length];
    int j = 0;
    for (int i = 0; i < outputArgs.length; i++, j++)
      result[j] = outputArgs[i];
    for (int i = 0; i < args.length; i++, j++)
      result[j] = args[i];
    for (int i = 0; i < sortedSourcesAbsolute.length; i++, j++)
      result[j] = sortedSourcesAbsolute[i];

    return result;
  }

  synchronized public void run(
    File[] sources,
    DependencyChanges changes,
    AnalysisCallback callback,
    Logger log,
    Reporter delegate,
    CompileProgress progress) {
    log.debug(() -> {
      String msg = "calling Scala compiler with arguments (CachedCompilerImpl):";
      for (String arg : args)
        msg = msg + "\n\t" + arg;
      return msg;
    });
    ZincMainCallback zincCallback = new ZincMainCallback() {
      public void handleErrors(String[] args, Problem[] problems) {
        throw new InterfaceCompileFailed(args, problems);
      }
      public void handleCompilationCancellation(String [] args, Problem[] problems) {
        throw new InterfaceCompileCancelled(args, problems);
      }
    };
    boolean success = ZincMain.process(
      commandArguments(sources),
      changes,
      callback,
      zincCallback,
      log,
      delegate,
      progress,
      output);
    if (!success) {
      throw new InterfaceCompileFailed(args, new Problem[0]);
    }
  }
}
