(ns timeoff.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]
      [cljs-time.core :as time]
      [cljs-time.format :as time-format]))


;; -------------------------
;; Logic

(def anchor-payday (time/local-date 2020 7 20))

(defn paydays-from [date]
   (lazy-seq (cons date (paydays-from (time/plus date (time/weeks 2))))))

(def paydays (paydays-from anchor-payday))

(defn most-recent-payday []
  (let [today (time/today)]
    (last (take-while #(time/before? % today) paydays))))

(defn paydays-after [date]
  (drop-while #(or (time/before? % date) (time/equal? % date)) paydays))

(defn hours-per-period [days-per-year] (/ (* 8 days-per-year) 26))

(defn days->hours [days] (* days 8))
(defn hours->days [hours] (Math/floor (.toFixed (/ hours 8) 3)))

(defn accrued-by-payday [accrued-as-of starting-hours hours-per-period]
  (map-indexed (fn [idx date]
                 (let [accrued-hours (+ starting-hours (* (+ 1 idx) hours-per-period))]
                   {:date date
                    :accrued-hours accrued-hours
                    :accrued-days (hours->days accrued-hours)}))
               (paydays-after accrued-as-of)))

(defn insert-at-new-year [accruals el-fn]
  (reduce (fn [accruals next]
            (let [last-acc (last accruals)
                  last-year (and last-acc (time/year (:date last-acc)))
                  next-year (time/year (:date next))]
              (if (and last-year (not= last-year next-year))
                (conj accruals (el-fn (time/year (:date next))) next)
                (conj accruals next))))
          []
          accruals))


;; -------------------------
;; Generic Views

(def iso-date-formatter (time-format/formatters :date))
(def human-formatter (time-format/formatter "M/d/yy"))
(defn human-format-date [date] (time-format/unparse human-formatter date))

(defn input-element [id type value-fn on-change]
  [:input {:id id
           :class "form-control"
           :type type
           :value (value-fn)
           :on-change on-change}])

(defn form-group [id label helper-text input-el]
  [:div.form-group
   [:label {:for id} label]
   input-el
   (into [:small.form-text.text-muted] helper-text)])

(defn basic-input
  ([id atom input-type label helper-text]
   (basic-input id atom input-type label helper-text #(reset! atom (-> % .-target .-value))))
  ([id atom input-type label helper-text on-change]
   (form-group id label helper-text
               [input-element id input-type #(deref atom) on-change])))

(defn number-input [id atom label helper-text]
  (basic-input id atom "number" label helper-text))

(defn date-input [id atom label helper-text]
  (form-group id label helper-text
              [input-element id "date"
               #(time-format/unparse iso-date-formatter @atom)
               #(reset! atom (time-format/parse iso-date-formatter (-> % .-target .-value)))]))

(defn tr
  ([key cells]
   (tr key cells :td))
  ([key cells cell-elem]
   [:tr {:key key} (map-indexed (fn [idx cell] [cell-elem {:key (str key idx)} cell]) cells)]))

(defn table [header-cells & body]
  [:table.table.table-sm.table-striped
   [:thead (tr "header" header-cells :th)]
   [:tbody (apply concat body)]])

(defn infobox [body]
  [:div.card.bg-light
   (into [:div.card-body] body)])


;; -------------------------
;; State

(defonce hours-accrued (r/atom 0))
(defonce future-days-used (r/atom 0))
(defonce accrued-as-of (r/atom (most-recent-payday)))
(defonce vac-days-per-year (r/atom 15))


;; -------------------------
;; App Components

(defn hours-accrued-input []
  (number-input "hours-accrued" hours-accrued  "Hours accrued"
                ["How many hours you currently have accrued. You can get this by running " [:code "/accruals list"]
                 " in Slack and finding " [:strong "Remaining Balance"] " under " [:strong "VAC Hours"] "."]))

(defn future-days-used-input []
  (number-input "future-used" future-days-used "Future days used"
                ["How many VAC days you already have planned in the future. You can see leave you have requested "
                 "by messaging " [:code "summary @yourname"] " to " [:strong "@AttendanceBot"] " on Slack."]))

(defn vac-days-per-year-input []
  (number-input "vac-days-per-year" vac-days-per-year "Vacation days per year"
                ["How many vacation days you earn per year"]))

(defn accrued-as-of-input []
  (date-input "accrued-as-of" accrued-as-of "Hours accrued current as of"
              ["When the accruals balance was last updated. This is the " [:strong "Balance As Of"]
              " date in " [:code "/accruals list"] "."]))

(defn style-hours [hours]
  (let [rounded (.toFixed hours 2)]
    (if (neg? hours)
      [:span.text-danger rounded]
      rounded)))

(defn accruals-row [row]
  (tr (str (:date row)) [(human-format-date (:date row))
                         (style-hours (:accrued-hours row))
                         (:accrued-days row)]))

(defn new-year-per-row [new-year]
  [:tr.table-success {:key (str "new-year-per" new-year)}
   [:td (human-format-date (time/local-date new-year 1 1))]
   [:td {:col-span 2}
    [:small "Make sure you have used your personal days, as they do not roll over."
     [:br] "You will get " [:strong "3"] " new personal days, which are not reflected here."]]])

(defn accruals-table []
  (let [starting-hours (- @hours-accrued (days->hours @future-days-used))
        accruals (take 26 (accrued-by-payday
                           @accrued-as-of
                           starting-hours
                           (hours-per-period @vac-days-per-year)))
        accs-with-new-year (insert-at-new-year accruals (fn [year] {:new-year year}))]
    (table
     ["Date" "Unused Accrued Hours" "Unused Accrued Days"]
     (map #(if (:new-year %)
             (new-year-per-row (:new-year %))
             (accruals-row %))
          accs-with-new-year))))

(defn general-info []
  (infobox ["This is a calculator to help you plan your future PTO. "
            "It tells you how much vacation (VAC) time you will have, minus any time that you already have planned in the future."
            [:br] [:br]
            "Note that this does not take personal days (PER) into account."]))

(defn feedback []
  (infobox ["Questions? Suggestions? Holler at " [:strong "@Micah"] " on Slack. Or raise an issue on "
            [:a {:href "https://github.com/micahbf/timeoff"} "GitHub"] "."]))

(defn really-big-emoji [emoji]
  [:div {:style {:font-size "30em"}} emoji])

(defn home-page []
  [:div.container
   [:div.row [:div.col [:h2 "Time Off Planner"]]]
   [:div.row
    [:div.col-md
     [general-info]
     [:br]
     [:form
      [hours-accrued-input]
      [accrued-as-of-input]
      [future-days-used-input]
      [vac-days-per-year-input]]
     [feedback]
     [really-big-emoji "🌴"]]
    [:div.col-md [accruals-table]]]])


;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
