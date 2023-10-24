(import hy.models [Symbol]
        trytond.pool [Pool]
        trytond.modules.hyton.utils [first]
        hyrule [rest]
        cytoolz [second partition])
(require hyrule [->>])

(defn default-func-name [name]
  (+ "default_" (.replace name "-" "_")))

(defmacro default [field args #* body]
  `(defn [classmethod] ~(Symbol (default-func-name (str field)))
         ~(+ [(Symbol "cls")] (list args)) ~@body))

(defmacro default-value [field value]
  `(defn [classmethod] ~(Symbol (default-func-name (str field))) [cls] ~value))

(defmacro default-fn [field function]
  `(defn [classmethod] ~(Symbol (default-func-name (str field))) [cls #* args]
         (~function #* args)))

;; Pool realated
(defn gets [provider keys]
  (map (fn[s] (.get provider s)) keys))

(defn save [model]
  (.save model)
  model)

(defn pool-gets [provider keys]
  (map (fn[s] (.get (Pool) s)) keys))

(defn pool-create [model-name #* args #** kargs]
  (save ((.get (Pool) model-name) #* args #** kargs)))

(defn pool-new [model-name #* args #** kargs]
  ((.get (Pool) model-name) #* args #** kargs))

(defn pool-load [model-name id]
  ((.get (Pool) model-name) id)
  )

(defn pool-search [model-name #* args #** kargs]
  (.search (.get (Pool) model-name) #* args #** kargs))

;;rec-name helpers
(defn is-not-operator [s]
  (or (.startswith s "!") (.startswith s "not ")))

(defn rec-name-and-or [s-operator]
  (if (is-not-operator s-operator) ["AND"] ["OR"]))

(defn rec-name-search-fields [fields clauses]
  (->>
    fields
    (map (fn [field]
           (tuple (+ [field] (list (rest clauses))))))
    list
    (+ (rec-name-and-or (second clauses)))))

;; this macro requires rec-name-search-fields
(defmacro search-rec-name-fields [fields]
  `(defn [classmethod] search-rec-name [cls name clauses]
         (rec-name-search-fields ~fields clauses)))

(defmacro create-fn-values [fn-values]
  `(defn [classmethod] create [cls vlist]
     (setv
       fn-pre ~fn-values
       c-vlist (lfor x vlist (.copy x)))
     (for [values c-vlist] (fn-pre values))
     (.create (super) c-vlist)))

;; it has defensive copy is it really is-needed
(defmacro on-create [fn-values fn-record]
  `(defn [classmethod] create [cls vlist]
     (setv
       fn-pre ~fn-values
       fn-post ~fn-record
       c-vlist (lfor x vlist (.copy x)))
     (for [values c-vlist] (fn-pre values))
     (setv ret (.create (super) c-vlist))
     (for [o ret] (fn-post o))
     ret))

;; should it have defensive is-copy
(defmacro on-write [fn]
  `(defn [classmethod] write [cls records values #* args]
     (setv fn-record-values ~fn)
     (for [o records] (fn-record-values o values))
     (for [o (list (partition 2 args))]
       (for [r (first o)] (fn-record-values r (second o))))
     (.write (super) records values #* args)))
