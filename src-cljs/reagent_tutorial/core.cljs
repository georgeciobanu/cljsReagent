(ns reagent-tutorial.core
  (:require [clojure.string :as string]
            [reagent.core :as r]))

(enable-console-print!)

(def selected-component (atom ""))


;; The "database" of your client side UI.
(def app-state
  (r/atom
   {:components
    [{:id "b1" :caption "Button 1"}
     {:id "b2" :caption "Button 2"}]}))

(defn get-next-component-id [component-type]
  (inc (count (filter #( = (:type %) component-type)))))

(defn update-components! [f & args]
  (do
    (.log js/console (str args))
    (apply swap! app-state update-in [:components] f args)))

(defn add-component! [c x y]
  (case c
    "Button" (update-components! conj {:id
                                       (str "Button " (get-next-component-id "button")) :caption "Button 3" :x x :y y})
    "Label" (update-components! conj {:id "l3" :caption "Label 3" :x x :y y})
    ""))

;; TODO fix it so it works
(defn remove-component! [cid]
  (update-components! (fn [cs]
                      (remove #(= (:id %) cid) cs))
                    cid))

(defn get-component-by-id [id]
  (first (filter #(= id (:id %)) (:components @app-state))))


(defn update-helper [c cid new-key-vals]
  (if (= cid (:id c))
      (merge c new-key-vals)
      c))

(defn update-component! [cid new-key-vals]
  (update-components! (fn [cs]
                          (map #(update-helper % cid new-key-vals) cs))))


(def start-move (atom {}))

;; UI components
(defn component [c]
  [:div {:class "button-component" :style {:top (:y c) :left (:x c)}
         :on-mouse-down #(do
                           (reset! start-move {:x (- (.-clientX %) (:x c)) :y (- (.-clientY %) (:y c))})
                           (.log js/console (:x @start-move)))
         :on-mouse-move #( if (seq @start-move)
                           (update-component! (:id c) {:x (- (.-clientX %) (:x @start-move)) :y (- (.-clientY %) (:y @start-move))}))
         :on-mouse-up #(reset! start-move {})}
   [:span (:caption c)]])



(defn main-component []
  [:div {:class "main-component"}
   [:div {:class "component-sidebar"}
    [:div {:class "button-component-sidebar" :on-click #(do
                                                  (.log js/console "Selected button")
                                                  (reset! selected-component "Button"))}]
    [:div {:class "label-component-sidebar"}]]
   [:div {:id "editor-canvas" :on-click #(do
                                            (.log js/console  (str "Created " @selected-component (.-clientX %) (.-clientY %)))
                                            (add-component! @selected-component (.-clientX %) (.-clientY %))
                                            (reset! selected-component ""))}
    (for [c (:components @app-state)]
      [component c])
    ]])



;; Render the root component
(defn start []
  (r/render-component
   [main-component]
   (.getElementById js/document "root")))
