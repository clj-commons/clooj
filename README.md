---
clooj, a lightweight IDE for clojure
---

### the application
clooj is a small, simple IDE (integrated development environment) for the [clojure](http://clojure.org) programming language, [available for free download](https://github.com/arthuredelstein/clooj/downloads). clooj is written entirely in clojure and uses a swing-based GUI. It is cross-platform (assuming Java 1.6 has been installed on your operating system), and runs as a standalone application. The application (containing the clojure core, currently version 1.3.0) is a single jar file that can be launched by double-clicking its file icon or by running
java -jar clooj-XXX-STANDALONE.jar from the command line.

### the layout
The clooj window contains three columns. The left-most column is a tree showing clojure projects and the source files they contain. The middle column is the source file editor. The right column displays inputs and outputs of clojure REPLs (read-evaluate-print loops).

### the source editor
The source code editor offers a few simple things to make writing clojure code easier:

 *  A non-traditional bracket-matching feature highlights in gray those brackets that contain the innermost form you are currently editing, and the argument list for the function or macro at the head of the form is displayed.
 *  Mismatched or unmatched brackets are highlighted in pink.
 *  cmd+[ indents, and cmd+] unindents.
 *  cmd+\ cleans up indentation.
 *  Automatically comment-out (and un-comment-out) multiple lines.
 *  When newlines are entered, the next line is automatically indented. 
 *  Press ctrl-ENTER to send either the nearest root form or the selected text to the REPL.
 *  Double-click a paren to highlight a form
 *  Source files are continuously saved in the background to prevent accidental loss of your work in the event of a crash.
 *  Syntax highlighting (using the RSyntaxTextArea library)

### clojure projects
Each clojure project corresponds to a project directory somewhere in the file system, containing a src directory. Inside the src directory is the source code hierarchy, composed of directories and .clj files. Note this directory structure is completely compatible with the lein and cake clojure build programs and you are encouraged to use one of these from the command line in conjunction with the clooj editor. Clicking different source files in the projects tree will automatically change the source file currently being edited, as well as switch the REPL to the appropriate namespace.

### read-evaluate-print loops
The upper part of clooj's REPL display column shows the REPL history (inputs and outputs) and the lower part is a text area for inputting forms into REPL. Each project gets its own REPL environment (in an external process): when a project is first selected, a new clojure REPL is created behind the scenes and becomes the REPL in use. By choosing "Restart REPL" you cause a new REPL to be created for the currently selected project and the old one to be discarded, if possible. If the project directory contains directories named "lib" and/or "jars" and there are any jar files inside, these jars will be included in the classpath whenever the project REPL is launched. You can subsequently add further jar files to the classpath by placing them in the "lib" or "jars" directory and restarting the REPL.

### name search, documentation and auto-completion
clooj can help you search for functions and other names, provide documentation for these names, and auto-complete your code. (This feature is in the early stages of development.) When help is activated, a list of available names similar to the local text is shown at left. At right, documentation and source code is shown for the selected name. Press TAB or shift+TAB to browse through the list, then press ENTER to replace what you have typed with the name you have selected. Press ESC to get out of help mode.

### more work needed
clooj is a work in progress. Please post any suggestions or criticisms to the [clooj Google group](http://groups.google.com/group/clooj) or to the [github issues list](https://github.com/arthuredelstein/clooj/issues).

All feedback is much appreciated. Code contributions in the form of github pull requests are also very welcome!

If you want to start building and working on clooj then please read the [building clooj from source wiki
page](https://github.com/arthuredelstein/clooj/wiki/Building-clooj).

-- Arthur Edelstein

