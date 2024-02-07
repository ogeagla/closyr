#!/usr/bin/env bash
set -e

mkdir -p releases
rm -rf releases/*.zip
rm -rf releases/*.jar

lein uberjar


cp target/uberjar/closyr-0.1.0-SNAPSHOT-standalone.jar releases/closyr-0.1.0-SNAPSHOT-standalone.jar
zip --junk-paths releases/closyr-0.1.0-SNAPSHOT-standalone.jar.zip releases/closyr-0.1.0-SNAPSHOT-standalone.jar

shasum -a 256 releases/closyr-0.1.0-SNAPSHOT-standalone.jar | cut -d " " -f 1 > releases/closyr-0.1.0-SNAPSHOT-standalone.jar.shasum256.txt
shasum -a 256 releases/closyr-0.1.0-SNAPSHOT-standalone.jar.zip | cut -d " " -f 1 > releases/closyr-0.1.0-SNAPSHOT-standalone.jar.zip.shasum256.txt

echo "JAR SHA256:"
cat releases/closyr-0.1.0-SNAPSHOT-standalone.jar.shasum256.txt

echo "JAR ZIP SHA256:"
cat releases/closyr-0.1.0-SNAPSHOT-standalone.jar.zip.shasum256.txt

echo "Done, check release folder"
