(import trytond.model [fields]
        trytond.pyson [Not Equal Eval Or And Bool If Get Greater]
        trytond.modules.hyton.utils [get-or])
(require hyrule [assoc ->])

(defn Company [[name "Company"] #* args #** kwargs]
  (.Many2One fields "company.company" name #* args #** kwargs))

(defn Party [[name "Party"] #* args #** kwargs]
  (.Many2One fields "party.party" name #* args #** kwargs))

(defn add-state-key [field key statement]
  (setv states (or field.states {})
        current-statement (get-or states key None))
  (assoc states key
               (if current-statement
                   (Or statement current-statement)
                   statement))
  (setv field.states states)
  field)

(defn add-readonly [field readonly] (add-state-key field "readonly" readonly))
(defn add-invisible [field invisible] (add-state-key field  "invisible" invisible))


(defn add-depends [field depends]
  "depends maybe a list or set"
  (setv current-depends (or field.depends #{}))
  (setv field.depends (.union current-depends (set depends)))
  field)

(defn readonly-no-company [field]
  (-> field
      (add-readonly (And (Bool (Eval "company"))
                            (Not (Equal (Eval "company")
                                        (Get (Eval "context" {}) "company" "0")))))
      (add-depends ["company"])))

(defn invisible-no-company [field]
  (-> field
      (add-invisible (Not (Equal (Eval "company")
                                      (Get (Eval "context" {}) "company" "0")
                                      )))
      (add-depends ["company"])))

(defn immutable [field]
  (-> field
      (add-readonly (Greater (Eval "id" 0) 0))
      (add-depends ["id"])))

(defn invisible-new [field]
  (-> field
      (add-invisible (bnot (Greater (Eval "id" 0) 0)))
      (add-depends ["id"])
      ))

