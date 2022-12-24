(import inspect
        trytond.model [Model fields]
        trytond.model.fields [Field]
        trytond.pyson [Equal Eval]
        trytond.modules.hyton.common-fields [add-readonly add-depends]
        cytoolz [second])
(require trytond.modules.hyton.sugar [default-value]
         hyrule [->>])


(setv _close-readonly-statement 
  (Equal (Eval "closed" False) True))

(defn readonly-closed-setup [class]
        (for [field (->> class
                         (inspect.getmembers)
                         (map second)
                         (filter (fn[m] (isinstance m Field))))]
          (add-depends 
            (add-readonly field _close-readonly-statement)
            ["closed"])))

;;TODO maybe rename to CloseableMixin
(defclass Closeable [Model]
  (setv
    closed (.Boolean fields "Closed" :select True :readonly True))

  (defn [classmethod] readonly-closed-setup [cls]
    (for [field (->> cls
                     (inspect.getmembers)
                     (map second)
                     (filter (fn[m] (isinstance m Field))))]
      (add-depends 
        (add-readonly field _close-readonly-statement)
        ["closed"])))
  
  (default-value closed False)

  (defn can-close [self]
    True)
  
  (defn _close [self]
    (when (.can-close self)
      (setv self.closed True)
      True))

  
  (defn can-open [self]
    True)
  
  (defn _open [self]
    (when (.can-open self)
      (setv self.closed False)
      True)))
