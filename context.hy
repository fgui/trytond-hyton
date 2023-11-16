(import
  trytond.transaction [Transaction])

(defn context-get [key]
  (.get (. (Transaction) context) key))


(defn context-language []
  (. (Transaction) language))

;; this could be in company.
(defn context-company []
  (context-get "company"))

;; this could/should be in cargo_container/container.
(defn context-warehouse[]
  (context-get "cargo_warehouse"))

;; this could be in company.
(defn context-locale []
  (context-get "locale"))
