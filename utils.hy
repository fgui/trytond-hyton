(import decimal
        decimal [Decimal]
        functools [reduce]
        itertools [repeat groupby]
        hy.pyops *)

(defn none? [x]
  "Check if `x` is None"
  (is x None))

(defn empty? [coll]
  "Check if `coll` is empty."
  (= 0 (len coll)))

(defn first [coll]
  "Return first item from `coll`."
  (next (iter coll) None))

(defn some [pred coll]
  "Return the first logical true value of applying `pred` in `coll`, else None."
  (first (filter None (map pred coll))))


(defn quantize-euros[d]
  (when (not (none? d))
    (.quantize d (decimal.Decimal "0.01") decimal.ROUND_HALF_UP)))

(defn calculate-percentage [percentage value]
  (/ (* value percentage) (Decimal "100")))

(defn not-none? [x] (not (none? x)))

(defn get-or [map key value]
  (try (get map key) (except [KeyError] value)))

(defn get-or-none [map key]
  (get-or map key None))

(defn str-empty? [s]
  (or (none? s) (empty? (.strip s))))

(defn str-not-empty? [s]
  (not (str-empty? s)))

(defn less-caps [a-string]
  (.join " " (map (fn [s] (if (= s (.lower s)) s (.capitalize s))) (.split a-string))))

(defn evently-divide [dividend-decimal divisor-int decimal-min-value]
  (setv
    total-amount (.quantize (abs dividend-decimal) decimal-min-value)
    low-amount (.quantize
                 (/ total-amount divisor-int)
                 decimal-min-value decimal.ROUND_DOWN)
    high-amount (+ low-amount decimal-min-value)
    num-highs (%
                (int (* total-amount (/ 1 decimal-min-value)))
                divisor-int
                )
    num-lows (- divisor-int num-highs)
    dividend-decimal-fn (if (< 0 dividend-decimal) + -)
    high-amount (dividend-decimal-fn high-amount)
    low-amount (dividend-decimal-fn low-amount)
    )
  (+ (list (repeat high-amount num-highs))
     (list (repeat low-amount num-lows)))
  )


(defn evently-divide-portions [value portions decimal-min-value]
  (setv total-portions (reduce + portions)
        values (list (map
                      (fn [portion]
                        (.quantize 
                          (/ (* value portion) total-portions)
                          decimal-min-value
                          decimal.ROUND_DOWN)
                        )
                      portions))
        diff (- value (reduce + values))
        )
  (if (= 0 diff)
      values
      (do
        (list
          (map + values
               (evently-divide diff (len values) decimal-min-value)))))
  )

(defn c-group-by [fn iter]
  (groupby
    (sorted iter :key fn)
    :key fn))
