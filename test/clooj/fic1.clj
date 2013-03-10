(ns clooj.test
  (:refer-clojure :exclude [type]) )

; *** test 1
; evaluate entire file
; with version 0.3.11, you have this warning :
; WARNING: type already refers to: #'clojure.core/type in namespace: clooj.test, being replaced by: #'clooj.test/type
; with this fork, no warning
; 
; *** test 2
; switch to buffer "fic2.clj" then switch back to this buffer and evaluate (type)
; with version 0.3.11, you will have this error :
; ArityException Wrong number of args (0) passed to: core$type  clojure.lang.AFn.throwArity (AFn.java:437)
; with this fork, no error
;
(defn type [] 'ok)


