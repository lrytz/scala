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

import xsbti.Problem;

public class InterfaceCompileCancelled extends xsbti.CompileCancelled {
    private final String[] _arguments;
    private final Problem[] _problems;

    public InterfaceCompileCancelled(String[] arguments, Problem[] problems) {
        super();
        this._arguments = arguments;
        this._problems = problems;
    }

    public String[] arguments() {
        return _arguments;
    }

    public Problem[] problems() {
        return _problems;
    }
}
