(ns joy-of-clojure.interlude1.vars_symbols_namespaces)

; Symbols ----------------------------------------------------------------------
; By separation between symbols and vars, Clojure uses two levels of
; indirection ; for mapping symbols to their underlying values.

(def foo "bar")
; => #'joy-of-clojure.interlude1.vars_symbols_namespaces/foo

(class 'foo)
; => clojure.lang.Symbol

(class #'foo)
; => clojure.lang.Var

(class @#'foo)
; => clojure.lang.String

; 'foo (Symbol) ---> #'foo (Var) ---> @#'foo (Value)

; Symbols in Clojure are just pieces of data. From an implementation
; perspective they're instances of clojure.lang.Symbol.
; They can be passed as function arguments or put into a collection just like
; a keyword or a string. Clojure uses them as identifiers for various values
; and references, which gives them a bit of a special status in the
; language—but if we take care to quote them in our code or at the repl,
; they're usable just like any other value.
['+ '- '/]
; => [+ - /]

; Symbols implement the clojure.lang.Named interface, which means they have the
; ability to return a String description of their namespace as well as their
; name using the methods getNamespace() and getName() respectively.
(namespace 'foo) ; => nil
(namespace 'faz/foo) ; => "faz"
(name 'foo) ; => "foo"

; Note, however, that symbols are not functions. When invoking (+ 1 2)
; we are invoking a function which '+ refers to.
(#'+ 1 2)
; => 3
; (+ 1 2) works too because the reader is automatically resolving symbol '+.
(resolve '+) ; => #'clojure.core./+

; It's important to remember that symbols don't belong to any namespace.
; We can't declare a symbol and when qualified a symbol, the namespace
; does not have to exist:
'foo.bar.baz/bir ; => foo.bar.baz/bir

; Differently qualified symbols are not the same when compared:
(= 'foo.bar.baz/bir 'bir) ; => bir
(= 'foo 'foo) ; => true
; However, they are never the same object (they are not static):
(identical? 'foo 'foo) ; => false

; To automatically qualify a symbol use a backtick:
`hello-world
; => joy-of-clojure.interlude1.vars_symbols_namespaces/hello-world

; Vars -------------------------------------------------------------------------
; If Clojure only maintained a static mapping from symbols to values or
; functions (instances of clojure.lang.IFn), it would be severely limiting. We
; wouldn't be able to have more than one function with a given name in our
; entire program, and we wouldn't be able to dynamically change what a symbol
; refers to.

; To achieve this, we need second level of indirection which is implemented
; with Vars. Vars (instances of clojure.lang.Var) are one of four constructs the
; Clojure language gives us to maintain a persistent reference to a changing
; value. In fact, of the four constructs (vars, atoms, refs, and agents),
; they're probably the one you use the most without even realizing it! Every
; time something is defined using the special form def (or the defn macro which
; uses def internally), Clojure creates a new var and places the value or
; function inside:

(def v 7)
(var-get #'v) ; => 7
(defn f [] 777)
(var-get #'f)
; => #object[joy_of_clojure.interlude1.vars_symbols_namespaces$f
;           0x4f77dcbf
;           "joy_of_clojure.interlude1.vars_symbols_namespaces$f@4f77dcbf"]

; Both def and defn create Var and initialize it's root binding.
; Root binding is a default value for each thread of execution.
; This means, that each call to def changes the value of some var for _all_
; threads. To see this, we can spawn multiple threads and see what happens.

(def x nil)

; To naively (there is a better way, we will come to it in later talks)
; start a number of threads we can do:
(dotimes [i 10] (.start (Thread. (fn [] (def x i)))))

x ; => 9 (typically)

; As you can see, var was rebound. What exactly happens when we re-def
; a var?

(def val1 4)
(def var_val1 #'val1)
(def val1 5)
(= var_val1 #'val1) ; => true

; As we can see, def did not create a new var, but assigned it a new
; value. In this way, vars are like locations in memory which are referenced
; by names.

; Curiously, when in let form, Clojure does not create new vars:
(def x 777)
(let [x 888] (deref #'x)) ; => 777

; This happens because let is a macro which simply substitutes values
; for names specified in the binding vector. However, if we were to use defn,
; we would again change the root binding:
(let [x 888] (def x 999) x) ; => 888
x ; => 999

; In order to modify a thread-local version of the var we can use special
; function "binding":
(doc binding)

(binding [x 111] x)
; Execution error (IllegalStateException) at joy-of-clojure.interlude1
; .vars-symbols-namespaces/eval1817 (vars_symbols_namespaces.clj:107).
; Can't dynamically bind non-dynamic var: joy-of-clojure.interlude1
; .vars_symbols_namespaces/x

; This happens because Clojure does not allow to dynamically bind values
; to non-dynamic vars. To enable this, we must first declare x as dynamic.
(def ^:dynamic x 777)
(binding [x 111] x) ; => 111
x ; => 777

; The "^:dynamic" bit, by the way, is adding private flag to the metadata
; of the var. We can check metadata of any var with meta function:
(meta #'x)
; =>
; {:dynamic true,
;  :line 115,
;  :column 1,
;  :file "/home/siegmeyer/Code/joy-of-clojure/src/joy_of_clojure/interlude1
;  /vars_symbols_namespaces.clj",
;  :name x,
;  :ns #object[clojure.lang.Namespace
;             0x32bcb736
;             "joy-of-clojure.interlude1.vars_symbols_namespaces"]}

; When creating functions with defn- we are declaring them private:
(ns super-secret-ns)
(defn- secret-fn [] 1)
; Equivalent of:
(defn ^:private secret-fn [] 1)

; This instructs Clojure reader to block any attempts to resolve symbol
; from other namespaces:
(ns joy-of-clojure.interlude1.vars_symbols_namespaces)
(use 'super-secret-ns)
(super-secret-ns/secret-fn)
; Syntax error (IllegalStateException) compiling super-secret-ns/secret-fn at
; (src/joy_of_clojure/interlude1/vars_symbols_namespaces.clj:154:1).
; var: #'super-secret-ns/secret-fn is not public

; We can bypass this by referring to Var directly:
(#'super-secret-ns/secret-fn) ; => 1

; To declare a var with no bindings (useful for forward declaration),
; we can use declare:
(declare v)
(bound? #'v) ; => false
(def v "Bound!")
(bound? #'v) ; => true

; To check if given var has a thread-local binding we can do:
(thread-bound? #'v) ; => false
(declare ^:dynamic v2)
(binding [v2 "Bound locally!"]
  [(thread-bound? #'v2) (bound? #'v2)])
(bound? #'v2) ; => false

; If we resort to Java, we can create Vars without identifying them with
; symbols (symbolic names):
(import '[clojure.lang Var])

; Root binding
(Var/create 42) ; => #'Var: --unnamed-->
(var-get (Var/create 42)) ; => 42

;; thread-local binding
(let [x (.setDynamic (Var/create 0))]
  (println (var-get x))    ; => 0
  (with-bindings {x 42}
    (println (var-get x))) ; => 42
  (println (var-get x)))   ; => 0

; Namespaces -------------------------------------------------------------------
; Namespaces are Java objects that contain mapping between (unqualified)
; symbols and vars.

*ns*
; =>
; #object[clojure.lang.Namespace
;         0x32bcb736
;         "joy-of-clojure.interlude1.vars_symbols_namespaces"]

; As we can see, *ns* is a special var which refers to clojure.lang.Namespace
; object. No magic here.

; If we want to retrieve all the mappings within the namespace, we can
; use the ns-map function:
(ns-map *ns*) ; => ...

; To check namespace aliases:
(ns foo
  (:require [joy-of-clojure.core :as core]
            [joy-of-clojure.chapter3.dipping-your-toes-in-the-pool :refer
             [whole-name]]))

(ns-aliases *ns*)
; => {core #object[clojure.lang.Namespace 0x17aa0ce3 "joy-of-clojure.core"]}

; whole-name will be contained in (ns-refers *ns*):
(contains? (ns-refers *ns*) 'whole-name) ; => true
