(ns reagent-tutorial.core
  (:require [clojure.string :as string]
            [reagent.core :as r]))

(enable-console-print!)

(def sel-palette-comp (atom ""))


;; The "database" of your client side UI.
(def app-state
  (r/atom
   {:components
    [{:id "b1" :caption "Button 1" :type "button" :x 180 :y 50 :width 100 :height 50}
     {:id "b2" :caption "Button 2" :type "button" :x 180 :y 150 :width 100 :height 50}]}))

(defn get-next-component-id [component-type]
  (inc (count (filter #( = (:type %) component-type) (@app-state :components)))))

(defn update-components! [f & args]
  (do
    (.log js/console (str args))
    (apply swap! app-state update-in [:components] f args)))

(defn add-component! [c x y]
  (case c
    "button" (update-components! conj {:id (str "b" (get-next-component-id "button"))
                                       :caption (str "Button " (get-next-component-id "button"))
                                       :x x
                                       :y y
                                       :type "button"
                                       :width 100
                                       :height 50})
    "label" (update-components! conj {:id (str "L" (get-next-component-id "label"))
                                      :caption (str "Label " (get-next-component-id "label"))
                                      :x x
                                      :y y
                                      :type "label"})
    ""))

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



;; UI components


(defn resize-handler-mid-left [c]
  [:div {:class "resize-handler"
         :style {:top (/ (:height c) 2)
                 :left -3}}])

(defn resize-handler-mid-right [c]
  (let [start-resize (atom {})]
    (fn [c]
      [:div {:class "resize-handler"
             :style {:top (/ (:height c) 2)
                     :left (- (:width c) 3)
                     :cursor "ew-resize"}
             :on-mouse-down #(reset! start-resize {:x (.-clientX %)})
             :on-mouse-move #(if (seq @start-resize)
                               (update-component! (:id c)
                                                  {:width (+ (:width c) (- (.-clientX %) (:x @start-resize)))}))
             :on-mouse-up #(reset! start-resize {})
                                                   }])))

(defn resize-handler-mid-top [c]
  [:div {:class "resize-handler"
         :style {:top -3
                 :left (/ (:width c) 2)}}])

(defn resize-handler-mid-bottom [c]
  [:div {:class "resize-handler"
         :style {:top (- (:height c) 3)
                 :left (/ (:width c) 2)}}])



(defn component [c]
  (let [start-move (atom {})]
    (fn [c]
      [:div {:class "button-component" :style {:top (:y c)
                                               :left (:x c)
                                               :width (str (:width c) "px")
                                               :height (str (:height c) "px")
                                               }
              :on-mouse-down #(do
                                (reset! start-move {:x (- (.-clientX %) (:x c))
                                                    :y (- (.-clientY %) (:y c))})
                                (.log js/console (:x @start-move)))
         :on-mouse-move #( if (seq @start-move)
                           (update-component! (:id c)
                                              {:x (- (.-clientX %) (:x @start-move))
                                               :y (- (.-clientY %) (:y @start-move))}))
         :on-mouse-up #(reset! start-move {})
         :on-mouse-out #(reset! start-move {})}
       [resize-handler-mid-left c]
       [resize-handler-mid-top c]
       [resize-handler-mid-right c]
       [resize-handler-mid-bottom c]
       ;[:span (:caption c)]
       ])))



(defn main-component []
  [:div {:class "main-component"}
   [:div {:class "component-sidebar"}
    [:div {:class "button-component-sidebar" :on-click #(do
                                                  (.log js/console "Selected button")
                                                  (reset! sel-palette-comp "button"))}]
    [:div {:class "label-component-sidebar"  :on-click #(do
                                                  (.log js/console "Selected label")
                                                  (reset! sel-palette-comp "label"))}]]
   [:div {:id "editor-canvas"
          :on-mouse-down #(cond ;if the user selected a component from the comp palette, place it on the canvas
                                (seq @sel-palette-comp) (do
                                                            (.log js/console  (str "Created " @sel-palette-comp (.-clientX %) (.-clientY %)))
                                                            (add-component! @sel-palette-comp (.-clientX %) (.-clientY %))
                                                            (reset! sel-palette-comp ""))
                                ;if there are selected component(s) on the canvas, move it/them
                                 (seq @selected-component) (do
                                                             (.log js/console ()}
    (for [c (:components @app-state)]
      [component c])
    ]])



;; Render the root component
(defn start []
  (r/render-component
   [main-component]
   (.getElementById js/document "root")))
