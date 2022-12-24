(import hy.models [Symbol]
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

;;rec-name helpers
(defn not-operator? [s]
  (or (.startswith s "!") (.startswith s "not ")))

(defn rec-name-and-or [s-operator]
  (if (not-operator? s-operator) ["AND"] ["OR"]))

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

(defmacro create-fn-values [f-values]
  `(defn [classmethod] create [cls vlist]
     (setv c-vlist
           (lfor x vlist (.copy x)))
     (for [values c-vlist] (~f-values values))
     (.create (super) c-vlist)
     ))

;; it has defensive copy is it really needed?
(defmacro on-create [fn-values fn-record]
  `(defn [classmethod] create [cls vlist]
     (setv c-vlist
           (lfor x vlist (.copy x)))
     (for [values c-vlist] (~fn-values values))
     (setv ret (.create (super) c-vlist))
     (for [o ret] (~fn-record o))
     ret))

;; should it have defensive copy?
(defmacro on-write [fn]
  `(defn [classmethod] write [cls records values #* args]
     (for [o records] (~fn o values))
     (for [o (list (partition 2 args))]
       (for [r (first o)] (~fn r (second o))))
     (.write (super) records values #* args)))
