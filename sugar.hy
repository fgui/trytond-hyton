(defn default-func-name [name]
  (+ "default_" (.replace name "-" "_")))

(defmacro default [field args &rest body]
  `(with-decorator classmethod
     (defn ~(HySymbol (default-func-name (name field)))
       ~(+ [(HySymbol "cls")] (list args)) ~@body)))

(defmacro default-value [field value]
  `(with-decorator classmethod
     (defn ~(HySymbol (default-func-name (name field))) [cls] ~value)))

(defmacro default-fn [field function]
  `(with-decorator classmethod
     (defn ~(HySymbol (default-func-name (name field))) [cls &rest args]
       (~function #* args))))

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
  `(with-decorator classmethod
     (defn search-rec-name [cls name clauses]
       (rec-name-search-fields ~fields clauses))))

(defmacro create-fn-values [f-values]
  `(with-decorator classmethod
     (defn create [cls vlist]
       (setv c-vlist
             (lfor x vlist (.copy x))
             )
       (for [values c-vlist] (~f-values values))
       (.create (super) c-vlist)
       )))

;; it has defensive copy is it really needed?
(defmacro on-create [fn-values fn-record]
  `(with-decorator classmethod
     (defn create [cls vlist]
       (setv c-vlist
             (lfor x vlist (.copy x))
             )
       (for [values c-vlist] (~fn-values values))
       (setv ret (.create (super) c-vlist))
       (for [o ret] (~fn-record o))
       ret)))

;; should it have defensive copy?
(defmacro on-write [fn]
`(with-decorator classmethod
     (defn write [cls records values &rest args]
        (for [o records] (~fn o values))
        (for [o (->> (partition args 2) (list))]
          (for [r (first o)] (~fn r (second o))))
        (.write (super) records values #* args))))
