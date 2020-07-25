(ns timeoff.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]
      [cljs-time.core :as time]))

;; -------------------------
;; Views

(defonce hours-accrued (r/atom 0))
(defonce future-days-used (r/atom 0))
(defonce accrued-as-of (r/atom (time/today)))

(defn input-element [id type value-fn on-change]
  [:input {:id id
           :class "form-control"
           :type type
           :value (value-fn atom)
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

(defn date-input [id label atom]
  [:div.form-group
   [:label {:for id} label]
   [input-element id "date" atom]])

(defn hours-accrued-input []
  (number-input "hours-accrued" "Hours accrued" hours-accrued))

(defn future-days-used-input []
  (number-input "future-used" "Future days used" future-days-used))

(defn home-page []
  [:div [:h2 "Time off calculator"]]
  [:form [hours-accrued-input] [future-days-used-input]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
