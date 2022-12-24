(import trytond.model [ModelSQL ModelView fields]
        trytond.pool [Pool]
        functools [reduce]
        trytond.modules.hyton.utils [none? empty?]
        cytoolz [take]
        hy.pyops *
        random)
(require hyrule [assoc])

(defn repeatedly [func]
  "Yield result of running `func` repeatedly."
  (while True
    (yield (func))))

(defn dec [n]
  "Decrement `n` by 1."
  (- n 1))

(defn create-random-str [letters size]
  (setv l (len letters))
  (reduce +
          (take size
                (repeatedly
                  (fn [] (get letters (dec (random.randint 1 l))))))))

(defn create-id [[size 8] [prefix ""]]
  ;; some the letters/numbers that get confussed removed
  ;; 0 O 1
  (+ prefix
       (create-random-str "23456879ABCDEFGHJKLMNPQRSTUVWXYZ" size)))

(defn get-new-id [model field [size 8] [prefix ""]]
  (setv TheModel (.get (Pool) model)
        new-identifier (create-id size prefix))
  (while (not (empty? (.search TheModel [#(field "=" new-identifier)])))
    (setv new-identifier (create-id size prefix)))
  new-identifier)

(defn add-identifier [values key-identifier [size 8] [prefix ""]]
  (setv identifier (try (get values "identifier") (except [KeyError] None)))
      (when (or (none? identifier) (= "" identifier))
        (assoc values "identifier" (get-new-id key-identifier "identifier" size prefix))))
