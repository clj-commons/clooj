---
clooj
---

### the application
clooj is a small, simple IDE (integrated development environment) for the clojure programming language. clooj is written entirely in clojure and uses a swing-based GUI. It is cross-platform (assuming Java 1.6 has been installed on your operating system), and runs as a standalone application or as a clojure editor embedded in another java or clojure application. The standalone version (containing the clojure core) is a single jar file that can be launched by double-clicking its file icon or running
java -jar clooj-XXX-STANDALONE.jar from the command line. To embed in java, call clooj.core.show().

### the layout
The clooj GUI consists of three columns. The left-most column is a tree showing clojure projects and the source files they contain. The middle column is the source file editor. The right column displays inputs and outputs of clojure REPLs (read-evaluate-print loops).

### the source editor
The source code editor offers a few simple things to make writing clojure code easier:
  -   A non-traditional bracket-matching feature highlights in gray those brackets that contain the innermost form you are currently editing.
  -   Mismatched or unmatched brackets are highlighted in pink.
  -   TAB indents, and shift+TAB unindents.
  -   Automatically comment-out (and un-comment out) multiple lines.
  -   When newlines are entered, the next line is automatically indented. 
  -   Press ctrl-ENTER to send either the nearest root form or the selected text to the REPL.
  -   Source files are continuously saved in the background to prevent accidental loss of your work in the event of a crash.

### clojure projects
Each clojure project corresponds to a project directory somewhere in the hard drive, containing a src directory. Inside the src directory is the source code hierarchy, composed of directories and .clj files. Note this directory structure is completely compatible with the lein and cake clojure build programs and you are encouraged to use one of these from the command line in conjunction with the clooj editor. Clicking different source files in the projects tree will automatically change the source file currently being edited, as well as switch the REPL to the corresponding current namespace.

### read-evaluate-print loops
The upper part of clooj's REPL display column shows the REPL history (inputs and outputs) and the lower part is a text area for inputting forms into REPL. Each project gets its own REPL environment: when a project is first selected, a new clojure REPL is created behind the scenes and becomes the REPL in use. By choosing "Restart REPL" you cause a new REPL to be created for the currently selected project and the old one discarded, if possible. If the project directory contains directories named "lib" and/or "jars" and there are any jar files inside, these jars will be included in the classpath whenever the project REPL is launched. You can subsequently add further jar files to the classpath by placing them in the "lib" or "jars" directory and restarting the REPL. These new classes can then be imported.

### more work needed
clooj is a work in progress. Your suggestions, criticisms and code contributions are appreciated.

-- Arthur Edelstein

