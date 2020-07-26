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
  ([id atom input-type label helper-text]
   (basic-input id atom input-type label helper-text #(reset! atom (-> % .-target .-value))))
  ([id atom input-type label helper-text on-change]
   [:div.form-group
    [:label {:for id} label]
    [input-element id input-type #(deref atom) on-change]
    (into [:small.form-text.text-muted] helper-text)]))

(defn number-input [id atom label helper-text]
  (basic-input id atom "number" label helper-text))

(def iso-date-formatter (time-format/formatters :date))

(defn date-input [id atom label helper-text]
  [:div.form-group
   [:label {:for id} label]
   [input-element id "date"
    #(time-format/unparse iso-date-formatter @atom)
    #(reset! atom (time-format/parse iso-date-formatter (-> % .-target .-value)))]
   (into [:small.form-text.text-muted] (vec helper-text))])

(defn tr
  ([key cells]
   (tr key cells :td))
  ([key cells cell-elem]
   [:tr {:key key} (map-indexed (fn [idx cell] [cell-elem {:key (str key idx)} cell]) cells)]))

(defn table [header rows]
  [:table.table.table-striped
   [:thead (tr "header" header :th)]
   [:tbody (map #(tr (first %) %) rows)]])

(defn infobox [body]
  [:div.card
   (into [:div.card-body] body)])

(defn hours-accrued-input []
  (number-input "hours-accrued" hours-accrued  "Hours accrued"
                ["How many hours you currently have accrued. You can get this by running " [:code "/accruals list"]
                 " in Slack and finding " [:strong "Remaining Balance"] " under " [:strong "VAC Hours"] "."]))

(defn future-days-used-input []
  (number-input "future-used" future-days-used "Future days used"
                ["How many VAC days you already have planned in the future. You can see leave you have requested "
                 "by messaging " [:code "summary @yourname"] " to " [:strong "attendancebot"] " on Slack."]))

(defn vac-days-per-year-input []
  (number-input "vac-days-per-year" vac-days-per-year "Vacation days per year"
                ["How many vacation days you earn per year"]))

(defn accrued-as-of-input []
  (date-input "accrued-as-of" accrued-as-of "Hours accrued current as of"
              ["When the accruals balance was last updated. This is the " [:strong "Balance As Of"]
              " date in " [:code "/accruals list"] "."]))

(def human-formatter (time-format/formatter "M/d/yy"))

(defn style-hours [hours]
  (let [rounded (.toFixed hours 2)]
    (if (neg? hours)
      [:span {:style {:color "red"}} rounded]
      rounded)))

(defn accruals-table []
  (let [starting-hours (- @hours-accrued (days->hours @future-days-used))
        accruals (take 26 (accrued-by-payday
                           @accrued-as-of
                           starting-hours
                           (hours-per-period @vac-days-per-year)))]
    (table
     ["Date" "Unused Accrued Hours" "Unused Accrued Days"]
     (map (fn [r] [(time-format/unparse human-formatter (:date r))
                   (style-hours (:accrued-hours r))
                   (:accrued-days r)])
          accruals))))

(defn general-info []
  (infobox ["This is a calculator to help you plan how much time off you can take in the future. "
            "It lets you know how much time you can take, when."
            [:br] "Note that this does not take personal days (PER) into account."]))

(defn feedback []
  (infobox ["Questions? Suggestions? Holler at " [:strong "@Micah"] " on Slack. Or raise an issue on "
            [:a {:href "https://github.com/micahbf/timeoff"} "GitHub"] "."]))

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
     [feedback]]
    [:div.col-md [accruals-table]]]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
