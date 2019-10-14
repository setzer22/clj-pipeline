(ns clj-pipeline.core
  (:require [clojure.spec.alpha :as spec]))

#_"We will define this nice utility function:"

(defn run-pipeline [in & [input-step & rest-steps]]
  (assert (vector? in) "Input must be a vector of arguments")
  (reduce
   (fn [env step]
     (step env))
   (apply input-step in)
   rest-steps))

(comment

  #_"With it, you can chain steps to perform a pipelined computation.
     The value being pipelined is a local environment (i.e: a map)"

  #_"Let's see how it works without the syntactic sugar:"

  #_"This is an input step, takes regular arguments and generates an env"
  (defn --step1 [val]
    {:var1 val})

  #_"This is an intermediate step, takes an env and returns an env"
  (defn --step2 [{:keys [var1] :as in}]
    (merge in {:var2 (inc var1)}))

  #_"These are output steps, they take an env and return an output"
  (defn --step3-1 [{:keys [var1 var2] :as in}]
    (* var1 var2))

  (defn --step3-2 [{:keys [var1 var2] :as in}]
    (/ var1 var2))

  #_"With run-pipeline, you can run the three steps in a pipeline"
  (run-pipeline [25]
                --step1 --step2 --step3-1)

  #_"The motivation is being able to fork the pipeline without replicating code"
  (run-pipeline [25]
                --step1 --step2 --step3-2)

  )

#_"Now, let's define some syntactic sugar:"

(comment

  #_"Example:"

  (defpipe-input --step-1 [val]
    {:var1 val})

  (defpipe-step --step-2 [var1]
    {:var2 (inc var1)})

  (defpipe-output --step3 [var1 var2]
    (* var1 var2))

  #_"These should compile to the above function declarations."

  #_"The maps returned by the steps now have a different meaning, they describe
     the modifications that will be done to the environment by that step. The
     semantics is that the map will be merged to the current environment."

  #_"Also note that we do not need to specify the map destructuring every time,
     that is done by the macro."

  )

#_"Now, the implementation:"

(spec/def ::defpipe-macro-args
  (spec/cat
   :name symbol?
   :docstring (spec/? string?)
   :args :clojure.core.specs.alpha/seq-binding-form
   :body (spec/* any?)))

(defn conform-or-throw
  "Tries to conform val to spec, returns conformed if valid or throws with message if invalid"
  [spec val & [message & _]]
  (let [ret (spec/conform spec val)]
    (if (spec/invalid? ret)
      (throw (Exception. (str "Failed to conform " spec " to " val ","
                              message "\n" (spec/explain-str spec val))))
      ret)))

;;TODO: Static checking

(defmacro defpipe-input [& macro-args]
  (let [{:keys [name args body docstring]} (conform-or-throw ::defpipe-macro-args macro-args)
        arglist (vec (spec/unform :clojure.core.specs.alpha/seq-binding-form args))]
    `(defn ~name ~arglist
       (let [ret# (do ~@body)]
         (assert (map? ret#) "Input step must return an env")
         ret#))))

(defmacro defpipe-step [& macro-args]
  (let [{:keys [name args body docstring]} (conform-or-throw ::defpipe-macro-args macro-args)
        arglist (vec (spec/unform :clojure.core.specs.alpha/seq-binding-form args))]
    `(defn ~name [{:keys ~arglist :as in#}]
       (let [ret# (do ~@body)]
         (assert (map? ret#) (str "Intermediate step " ~name " must return an env"))
         (merge in# ret#)))))

(defmacro defpipe-output [& macro-args]
  (let [{:keys [name args body docstring]} (conform-or-throw ::defpipe-macro-args macro-args)
        arglist (vec (spec/unform :clojure.core.specs.alpha/seq-binding-form args))]
    `(defn ~name [{:keys ~arglist :as in#}]
       (do ~@body))))

#_"This small macro also allows us the boilerplate of putting
   local symbols into a map duplicating names. Anything more
   complicated can be done by creating a map."

(defmacro env
  "(env x y) -> {:x x :y y}"
  [& args]
  (let [kws (map #(-> % name keyword) args)]
    (into {} (map vector kws args))))

#_"Finally, a working example!"

(comment
  (defpipe-input --test-step-1 [x y]
    (env x y))

  (defpipe-step --test-step-2 [x y]
    {:test (+ x y)})

  (defpipe-output --test-step-3 [x test]
    (* x test))


  (--test-step-1 3 2)

  (--test-step-2 {:x 3 :y 2})

  (--test-step-3 {:x 3 :y 2 :test 5})

  (run-pipeline [3 2]
                --test-step-1
                --test-step-2
                --test-step-3)
  )

#_"Useful for conditional pipelines where you want to do something only under certain conditions"
(defpipe-step nop-step []
  {})
