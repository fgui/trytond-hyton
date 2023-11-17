(import
  trytond.pyson [Not Equal Eval Or And Bool If Get Greater])

;; this could be in company.



(defclass NoCompanyStates []
  "State when the company field is not the current users"
  (setv READONLY {"readonly"
                  (And (Bool (Eval "company"))
                            (Not (Equal (Eval "company")
                                        (Get (Eval "context" (dict)) "company" "0")
                                )))}
        INVISIBLE {"invisible"
                   (Not (Equal (Eval "company")
                                      (Get (Eval "context" (dict)) "company" "0")
                              ))}
        DEPENDS ["company"]))

(setv IMMUTABLE {"readonly"
                 (Greater (Eval "id" 0) 0)})



