(import datetime
        time
        pytz
        datetime [timedelta]
        trytond.model [fields Model]
        trytond.pool [Pool]
        trytond.modules.hyton.utils [first]
        cytoolz [second])
(require hyrule [->])

(setv TIMEZONES  (lfor x pytz.common_timezones #(x x)))

(defn timezones []
  TIMEZONES)

(defn default-timezone []
  ;; TODO maybe attempt to get timezone from user/company if exists
  "Europe/Madrid")

(defn date-today []
  (setv Date (.get (Pool) "ir.date"))
  (.today Date))

(defn datetime-now []
  (datetime.datetime.now))

(defn date-first-day-month [date]
  (.replace date :day 1))

(defn date-last-day-month [date]
  (setv month date.month)
  (if (= 12 date.month)
      (.replace date :day 31);;december
      (->
        date
        (date-first-day-month)
        (.replace :month (+ 1 month))
        (- (timedelta :days 1)))))

(defn date-first-day-year [date]
  (.replace (.replace date :day 1) :month 1)
  )

(defn date-first-day-current-year []
  (.replace (.replace (datetime.date.today) :day 1) :month 1))

(defn plus-days [dt-time days]
  (+ dt-time (timedelta :days days)))

(defn next-day [dt-time]
  (plus-days dt-time 1))

(defn skip-weekend [dt-time]
  (setv wkday (.weekday dt-time))
  (if (or(= wkday 5) (= wkday 6))
      (skip-weekend (next-day dt-time))
      dt-time))

(defn plus-days-weekday [dt-time days]
  (skip-weekend (plus-days dt-time days)))

(defclass DateBoundMixin [Model]
  (setv
    start-date (.Date fields "Start Date" :required True)
    end-date (.Date fields "End Date")))

(defn date-next-weekday [date weekday]
  (setv day-gap (- (int weekday) (date.weekday))
        forward-day-gap (if (> 0 day-gap) (+ 7 day-gap) day-gap))
  (+ date (timedelta :days forward-day-gap)))

(defn date-next-day-of-month [date day-of-month]
  (if (<= date.day day-of-month)
      (.replace date :day day-of-month)
      (date-next-day-of-month (plus-days (date-last-day-month date) 1) day-of-month)
      ))

;; it includes date in locale and unix time up to seconds.
(defn dt-str4bots [[sep "-"]]
  (+ (.strftime (datetime.datetime.now) "%Y%m%H%M%S")
     sep
     (str (int (time.time)))))
