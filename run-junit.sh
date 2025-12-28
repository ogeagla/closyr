#!/bin/bash
# Run JUnit tests for Java API

set -e

# Compile main Java sources
lein with-profile +test javac

# Get classpath
CP=$(lein with-profile +test classpath 2>/dev/null)

# Create test-classes directory
mkdir -p target/test-classes

# Compile Java test sources manually
javac -d target/test-classes \
  -cp "target/classes:$CP" \
  test/org/closyr/core/FindFormulaTest.java

# Run JUnit
java -jar ~/.m2/repository/org/junit/platform/junit-platform-console-standalone/6.0.1/junit-platform-console-standalone-6.0.1.jar \
  execute \
  --class-path "target/test-classes:target/classes:$CP" \
  --select-class org.closyr.core.FindFormulaTest \
  --details tree
