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

(defn update-components! [f & args]
  (do
    (.log js/console (str args))
    (apply swap! app-state update-in [:components] f args)))

(defn add-component! [c x y]
  (case c
    "Button" (update-components! conj {:id "b3" :caption "Button 3" :x x :y y})
    "Label" (update-components! conj {:id "l3" :caption "Label 3" :x x :y y})))

(defn remove-component! [c]
  (update-components! (fn [cs]
                      (remove #(= % c) cs))
                    c))

(defn get-component-by-id [id]
  (get-in @app-state [:components id]))

(defn update-component! [cid new-key-vals]
  (update-components! (fn [cs]
                          (update-in cs [cid] merge new-key-vals))))

;; UI components
(defn component [c]
  [:div {:class "button-component" :style {:top (:y c) :left (:x c)}}
   [:span (:caption c)]])



(defn main-component []
  [:div {:class "main-component"}
   [:div {:class "component-sidebar"}
    [:div {:class "button-component" :on-click #(do
                                                  (.log js/console "Selected button")
                                                  (reset! selected-component "Button"))}]
    [:div {:class "label-component"}]]
   [:div {:class "editor-canvas" :on-click #(do
                                            (.log js/console  (str "Created " @selected-component (.-clientX %) (.-clientY %)))
                                            (add-component! @selected-component (.-clientX %) (.-clientY %) ))}
    (for [c (:components @app-state)]
      [component c])
    ]])



;; Render the root component
(defn start []
  (r/render-component
   [main-component]
   (.getElementById js/document "root")))
