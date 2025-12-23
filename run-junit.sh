#!/bin/bash
# Run JUnit tests for Java API

# Compile Java sources first
lein with-profile +test javac

# Get classpath
CP=$(lein with-profile +test classpath 2>/dev/null)

# Run JUnit
java -jar ~/.m2/repository/org/junit/platform/junit-platform-console-standalone/1.10.2/junit-platform-console-standalone-1.10.2.jar \
  execute \
  --class-path "target/classes:$CP" \
  --select-class org.closyr.core.FindFormulaTest \
  --details tree
