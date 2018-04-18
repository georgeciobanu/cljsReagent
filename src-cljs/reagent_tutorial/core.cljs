(ns reagent-tutorial.core
  (:require [clojure.string :as string]
            [reagent.core :as r]))

(enable-console-print!)

(def sel-palette-comp (atom ""))

(def selecting (atom false))

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

(defn get-component-under-cursor [evt]
  "returns the topmost component under cursor"
  (let [curX (.-clientX evt)
        curY (.-clientY evt)]
    (first (filter #(and
              (and (>= curX (:x %))
                   (>= curY (:y %)))
              (and (<= curX (+ (:x %) (:width %)))
                   (<= curY (+ (:y %) (:height %)))))
          (:components @app-state)))))

(defn get-components-in-rect [x1 y1 x2 y2]
  (map (fn [c] ({:id (:id c) :x-offset 0 :y-offset 0}))
       (filter #(and
             (and (>= (:x %) x1)
                  (>= (:y %) y1))
             (and (<= (+ (:x %) (:width %)) x2)
                  (<= (+ (:y %) (:height %)) y2)))
          (:components @app-state)))

(def selection-div-size (r/atom {}))

(def sel-canvas-comp (r/atom []))

(def current-operation (r/atom ""))

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

(def start-move (atom {}))

(defn save-cursor-offset [evt]
  (reset! start-move {:x (.-clientX evt)
                      :y (.-clientY evt)}))

(defn component [c]
      [:div {:class "button-component" :style {:top (:y c)
                                               :left (:x c)
                                               :width (str (:width c) "px")
                                               :height (str (:height c) "px")
                                               }
;;               :on-mouse-down #(do
;;                                 (save-cursor-offset %)
;;                                 (.log js/console (:x @start-move)))
;;          :on-mouse-move #( if (seq @start-move)
;;                            (update-component! (:id c)
;;                                               {:x (- (.-clientX %) (:x @start-move))
;;                                                :y (- (.-clientY %) (:y @start-move))}))
;;          :on-mouse-up #(reset! start-move {})
;;          :on-mouse-out #(reset! start-move {})
             }
       [resize-handler-mid-left c]
       [resize-handler-mid-top c]
       [resize-handler-mid-right c]
       [resize-handler-mid-bottom c]
       ;[:span (:caption c)]
       ])

(defn selection-rect []
  [:div {:style {:top (:y @start-move)
               :left (:x @start-move)
               :width (:width @selection-div-size)
               :height (:height @selection-div-size)
               :hidden (empty? @selection-div-size)}
   :class "selection-rect"}])


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
          :on-mouse-down #(let [comp-under-cursor (get-component-under-cursor %)]
                            (cond
                              ;; user selected a component to insert
                              (= current-operation "insert") (do
                                                               (.log js/console  (str "Created " @sel-palette-comp (.-clientX %) (.-clientY %)))
                                                               (add-component! @sel-palette-comp (.-clientX %) (.-clientY %))
                                                               (reset! sel-palette-comp "")
                                                               (reset! current-operation ""))
                              ;; user clicked on empty area
                              (empty? comp-under-cursor) (do
                                                           (save-cursor-pos %)
                                                           (reset! current-operation "select")
                                                           (reset! sel-canvas-comp [])
                                                           (doseq [c @sel-canvas-comp]
                                                             (update-component! c {:selected false})))
                              ;; user clicked on a component
                              (seq comp-under-cursor) (do
                                                        ;; save all selected components' offset to cursor
                                                        (

                              (do ;; otherwise save the current cursor and each component's offset to it
                                (save-

                            ;; otherwise save the offset (cursor, selected components)
                            ;; for moving components or drawing a selection rectangle
                            :else (do
                                    (save-cursor-offset %)
                                    (.log js/console (str "Comp under cursor: " (get-component-under-cursor %)))
                                    (if (empty? (get-component-under-cursor %))
                                      ;; we are selecting
                                      (do
                                        (reset! sel-canvas-comp [])
                                        (reset! selecting true)
                                        (.log js/console (str "Selecting: " @selecting)))
                                      ;; otherwise we are moving component(s)
                                      (do
                                        (reset! selecting false)
                                        (reset! sel-canvas-comp [(get-component-under-cursor %)])))))

          :on-mouse-move #(cond ;; if components are selected and not selecting, move them
                            (and (seq @sel-canvas-comp) (not @selecting)) (do
                                                                            (.log js/console (str "Starting to move " @sel-canvas-comp))
                                                     (doseq [c @sel-canvas-comp] (update-component! (:id c)
                                                                                    {:x (- (.-clientX %) (:x @start-move))
                                                                                     :y (- (.-clientY %) (:y @start-move))})))
                            ;; otherwise we are selecting so draw a selection rectangle
                            (seq @start-move) (do
                                    (reset! selection-div-size {:width (- (.-clientX %) (:x @start-move))
                                                                :height (- (.-clientY %) (:y @start-move))})
                                    (reset! sel-canvas-comp [(get-components-in-rect (:x @start-move)
                                                                                     (:y @start-move)
                                                                                     (.-clientX %)
                                                                                     (.-clientY %))]))
                                    ;; set the selected flag on components
                                    (map (fn [c] (update-component! (:id c) {:selected (contains? @sel-canvas-comp c)} @app-state)))
                                    (.log js/console (str "Start: " @start-move " end " (.-clientX %) " " (.-clientY %)))
                                    (.log js/console (str "comps: " @sel-canvas-comp))
                                    ;; (.log js/console (str @app-state))
                                                ))
          :on-mouse-up #(do
                          (reset! start-move {})
                          (reset! selection-div-size {})
                          (reset! selecting false))}
    [selection-rect]

    (for [c (:components @app-state)]
      [component c])
    ]])



;; Render the root component
(defn start []
  (r/render-component
   [main-component]
   (.getElementById js/document "root")))
