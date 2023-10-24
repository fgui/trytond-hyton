(import decimal
        decimal [Decimal]
        functools [reduce]
        itertools [repeat groupby]
        hy.pyops *)

(defn is-none [x]
  "Check if `x` is None"
  (is x None))

(defn is-empty [coll]
  "Check if `coll` is empty."
  (= 0 (len coll)))

(defn first [coll]
  "Return first item from `coll`."
  (next (iter coll) None))

(defn some [pred coll]
  "Return the first logical true value of applying `pred` in `coll`, else None."
  (first (filter None (map pred coll))))


(defn quantize-euros[d]
  (when (not (is-none d))
    (.quantize d (decimal.Decimal "0.01") decimal.ROUND_HALF_UP)))

(defn calculate-percentage [percentage value]
  (/ (* value percentage) (Decimal "100")))

(defn is-not-none [x] (not (is-none x)))

(defn get-or [map key value]
  (try (get map key) (except [KeyError] value)))

(defn get-or-none [map key]
  (get-or map key None))

(defn is-str-empty [s]
  (or (is-none s) (is-empty (.strip s))))

(defn is-str-not-empty [s]
  (not (is-str-empty s)))

(defn str-as-one-line [s]
  (if s
      (.join " " (.split s))
      s))

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

(defn volume-m3-to-cms [volume-m3]
  (when (and (not (is volume-m3 None))
             (>= volume-m3 0)) 
    (evently-divide
      (Decimal (* 300
                 (** (float volume-m3) (/ 1 3)))) 3 (Decimal "0.01"))))

(defn volume-cms-to-m3 [cms]
  (let [cm3 (reduce * cms)]
    (.quantize
      (if cm3  ;; if 0 return 0 do not divide.
          (/ cm3
             (Decimal "1000000"))
          cm3)
      (Decimal "0.001")
      decimal.ROUND_HALF_UP)))
