(ns timeoff.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]
      [cljs-time.core :as time]
      [cljs-time.format :as time-format]))

;; -------------------------
;; Logic

(def anchor-payday (time/local-date 2020 7 20))

(defn payday-seq
  ([]
   (payday-seq anchor-payday))
  ([date]
   (lazy-seq (cons date (payday-seq (time/plus date (time/weeks 2)))))))

(def paydays (payday-seq))

(defn most-recent-payday []
  (last (take-while #(time/before? % (time/today)) paydays)))

(defn paydays-after [date]
  (drop-while #(or (time/equal? % date) (time/before? % date)) paydays))

(defn hours-per-period [days-per-year] (/ (* 8 days-per-year) 26))

(defn days->hours [days] (* days 8))
(defn hours->days [hours] (Math/floor (/ hours 8)))

(defn accrued-by-payday [accrued-as-of starting-hours hours-per-period]
  (map-indexed (fn [idx date]
                 (let [accrued-hours (+ starting-hours (* (+ 1 idx) hours-per-period))]
                   {:date date
                    :accrued-hours accrued-hours
                    :accrued-days (hours->days accrued-hours)}))
               (paydays-after accrued-as-of)))


;; -------------------------
;; State

(defonce hours-accrued (r/atom 0))
(defonce future-days-used (r/atom 0))
(defonce accrued-as-of (r/atom (most-recent-payday)))
(defonce vac-days-per-year (r/atom 15))

;; -------------------------
;; Views

(defn input-element [id type value-fn on-change]
  [:input {:id id
           :class "form-control"
           :type type
           :value (value-fn)
           :on-change on-change}])

(defn basic-input
  ([id label input-type atom]
   (basic-input id label input-type atom #(reset! atom (-> % .-target .-value))))
  ([id label input-type atom on-change]
   [:div.form-group
    [:label {:for id} label]
    [input-element id input-type #(deref atom) on-change]]))

(defn number-input [id label atom]
  (basic-input id label "number" atom))

(def iso-date-formatter (time-format/formatters :date))

(defn date-input [id label atom]
  [:div.form-group
   [:label {:for id} label]
   [input-element id "date"
    #(time-format/unparse iso-date-formatter @atom)
    #(reset! atom (time-format/parse iso-date-formatter (-> % .-target .-value)))]])

(defn hours-accrued-input []
  (number-input "hours-accrued" "Hours accrued" hours-accrued))

(defn future-days-used-input []
  (number-input "future-used" "Future days used" future-days-used))

(defn vac-days-per-year-input []
  (number-input "vac-days-per-year" "Vacation days per year" vac-days-per-year))

(defn accrued-as-of-input []
  (date-input "accrued-as-of" "Hours accrued current as of" accrued-as-of))

(defn table [header rows]
  [:table.table.table-striped
   [:thead [:tr (map (fn [cell] [:th cell]) header)]]
   [:tbody (map (fn [row]
              [:tr (map (fn [cell] [:td cell]) row)])
            rows)]])

(def human-formatter (time-format/formatter "M/d/yy"))

(defn accruals-table []
  (let [starting-hours (- @hours-accrued (days->hours @future-days-used))
        accruals (take 26 (accrued-by-payday
                           @accrued-as-of
                           starting-hours
                           (hours-per-period @vac-days-per-year)))]
    (table
     ["Date" "Unused Accrued Hours" "Unused Accrued Days"]
     (map (fn [r] [(time-format/unparse human-formatter (:date r))
                   (.toFixed (:accrued-hours r) 2)
                   (:accrued-days r)])
          accruals))))

(defn home-page []
  [:div
   [:div [:h2 "Time off calculator"]]
   [:form
    [hours-accrued-input]
    [future-days-used-input]
    [vac-days-per-year-input]
    [accrued-as-of-input]]
   [accruals-table]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
