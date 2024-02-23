(import datetime
        time
        pytz
        datetime [timedelta]
        trytond.model [fields Model]
        trytond.pool [Pool]
        trytond.modules.hyton.utils [first]
        trytond.modules.hyton.context [context-company context-locale context-language]
        cytoolz [second])
(require hyrule [->])

(setv TIMEZONES  (lfor x pytz.common_timezones #(x x)))

(defn timezones []
  TIMEZONES)

(defn default-timezone []
  "Europe/Madrid")

(defn default-timezone-company-context []
  (let [company-id (context-company)]
    (if (and company-id (> company-id 0))
        (let [company ((.get (Pool) "company.company") company-id)]
          (if (and company company.timezone)
              company.timezone
              (default-timezone)))
        (default-timezone))))

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
  (.replace (.replace date :day 1) :month 1))

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
      (date-next-day-of-month (plus-days (date-last-day-month date) 1) day-of-month)))

;; it includes date in formated and unix time up to seconds.
(defn dt-str4bots [[sep "-"]]
  (+ (.strftime (datetime.datetime.now) "%Y%m%d%H%M%S")
     sep
     (str (int (time.time)))))

(defn format-dd-mmm-yy-hh-mm-context-tz-lang [dt]
  (let [Lang (.get (Pool) "ir.lang")
        lang (.get Lang (context-language))]
    (lang.strftime
     (.astimezone dt
                  (pytz.timezone
                   (default-timezone-company-context)))
     :format "%d %b %y %H:%M")))

(defclass DTMix [Model]
  (setv
    dt (.Function fields (.Char fields "Datetime")
                  "get_dt" :searcher "search_dt"))

  (defn get-dt [self [name None]]
    (format-dd-mmm-yy-hh-mm-context-tz-lang self.create-date))

  (defn [classmethod] search-dt [cls name clause]
    (when (get clause 2)
      (let [cl2 (get clause 2)
            cl2 (.replace  cl2 "%" "")]
        (when (.isdigit cl2)
          [#( "create_date"
               "<"
               (plus-days (datetime-now) (- (int cl2))))]))))

  (defn order_dt [tables]
    (let [table (first (get tables None))]
      [table.create-date])))
