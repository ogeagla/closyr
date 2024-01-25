#!/usr/bin/env bash

export _JAVA_OPTIONS="$_JAVA_OPTIONS \
	-Dawt.useSystemAAFontSettings=on \
	-Dswing.aatext=true \
	-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel \
	-Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"

echo "Running with GTK look and feel...this might look worse..."

lein run
