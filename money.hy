(import trytond.model [fields]
        trytond.modules.hyton.utils [evently-divide evently-divide-portions quantize-euros calculate-percentage]
        decimal [Decimal]
        decimal)

(defn Money [name #* args #** kwargs]
  (.Numeric fields name :digits #(16 2) #* args #** kwargs))

(defn evently-divide-money [money divisor-int]
  (evently-divide money divisor-int (Decimal "0.01")))

(defn evently-divide-portions-money [money portions]
  (evently-divide-portions money portions (Decimal "0.01")))

(defn calculate-percentage-money [money percentatge]
  (quantize-euros (calculate-percentage money percentatge)))
