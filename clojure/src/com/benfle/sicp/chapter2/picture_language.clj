(ns com.benfle.sicp.chapter2.picture-language
  (:import [javafx.embed.swing JFXPanel]
           [javafx.application Platform]
           [javafx.stage StageBuilder]
           [javafx.scene Group]
           [javafx.scene Scene]
           [javafx.scene.canvas Canvas]
           [javafx.scene.canvas GraphicsContext]))

(defn add-vec
  [{ux :x uy :y} {vx :x vy :y}]
  {:x (+ ux vx)
   :y (+ uy vy)})

(defn scale-vec
  [s {:keys [x y]}]
  {:x (* s x)
   :y (* s y)})

(defn sub-vec
  [{ux :x uy :y} {vx :x vy :y}]
  {:x (- ux vx)
   :y (- uy vy)})

(defn origin-frame
  [{:keys [width height]}]
  {:origin {:x 0 :y 0}
   :edge1 {:x width :y 0}
   :edge2 {:x 0 :y height}})

(defn frame-coord-map
  "To transform coordinates from the unit square to the frame's coordinate system."
  [{:keys [origin edge1 edge2]}]
  (fn [{:keys [x y]}]
    (add-vec origin
             (add-vec (scale-vec x edge1)
                      (scale-vec y edge2)))))

;; force initialization of the FX toolkit
(JFXPanel.)

;; to ensure the code runs on the FX application thread
(defn run-later
  [f]
  (Platform/runLater f))

(def ^:dynamic *gc* nil)

(defn draw
  "Render the painter on a Java FX canvas."
  [painter]
  (run-later
   (fn []
     (let [width 1000
           height 1000
           frame (origin-frame {:width width :height height})
           root (Group.)
           canvas (Canvas. width height)
           _ (.add (.getChildren root) canvas)
           scene (Scene. root)
           stage (-> (StageBuilder/create)
                     (.title "Picture Language")
                     (.scene scene)
                     (.build))]
       (binding [*gc* (.getGraphicsContext2D canvas)]
         (painter frame))
       (.show stage)))))

(defn draw-line
  [{sx :x sy :y} {ex :x ey :y}]
  (.strokeLine *gc* sx sy ex ey))

(defn segments->painter
  [segments]
  (fn [frame]
    (let [m (frame-coord-map frame)]
      (doseq [{:keys [start end]} segments]
        (draw-line (m start) (m end))))))

(defn segments
  [coords]
  (map (fn [[sx sy ex ey]]
         {:start {:x sx :y sy}
          :end {:x ex :y ey}})
       coords))

(def outline
  (segments->painter
   (segments
    [[0 0 0 1]
     [0 1 1 1]
     [1 1 1 0]
     [1 0 0 0]])))

(def cross
  (segments->painter
   (segments
    [[0 0 1 1]
     [0 1 1 0]])))

(def diamond
  (segments->painter
   (segments
    [[0 0.5 0.5 1]
     [0.5 1 1 0.5]
     [1 0.5 0.5 0]
     [0.5 0 0 0.5]])))

(def triangle
  (segments->painter
   (segments
    [[0 0 0.5 1]
     [0.5 1 1 0]
     [1 0 0 0]])))

(defn transform-painter
  [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter
       {:origin new-origin
        :edge1 (sub-vec (m corner1) new-origin)
        :edge2 (sub-vec (m corner2) new-origin)}))))

(defn flip-vert
  [painter]
  (transform-painter
   painter
   {:x 0 :y 1}
   {:x 1 :y 1}
   {:x 0 :y 0}))

(defn shrink-to-upper-right
  [painter]
  (transform-painter
   painter
   {:x 0.5 :y 0.5}
   {:x 1 :y 0.5}
   {:x 0.5 :y 1}))

(defn rotate90
  [painter]
  (transform-painter
   painter
   {:x 1 :y 0}
   {:x 1 :y 1}
   {:x 0 :y 0}))

(defn rotate180
  [painter]
  (transform-painter
   painter
   {:x 1 :y 1}
   {:x 0 :y 1}
   {:x 1 :y 0}))

(defn squash-inwards
  [painter]
  (transform-painter
   painter
   {:x 0 :y 0}
   {:x 0.65 :y 0.35}
   {:x 0.35 :y 0.65}))

(defn beside
  [painter1 painter2]
  (let [split-point {:x 0.5 :y 0}
        paint-left (transform-painter painter1
                                      {:x 0 :y 0}
                                      split-point
                                      {:x 0 :y 1})
        paint-right (transform-painter painter2
                                       split-point
                                       {:x 1 :y 0}
                                       {:x 0.5 :y 1})]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))

(defn below
  [painter1 painter2]
  (let [split-point {:x 0 :y 0.5}
        paint-top (transform-painter painter1
                                     split-point
                                     {:x 1 :y 0.5}
                                     {:x 0 :y 1})
        paint-bottom (transform-painter painter2
                                        {:x 0 :y 0}
                                        {:x 1 :y 0}
                                        split-point)]
    (fn [frame]
      (paint-top frame)
      (paint-bottom frame))))

(defn flip-vert
  [painter]
  (transform-painter
   painter
   {:x 0 :y 1}
   {:x 1 :y 1}
   {:x 0 :y 0}))

(defn flip-horiz
  [painter]
  (transform-painter
   painter
   {:x 1 :y 0}
   {:x 0 :y 0}
   {:x 1 :y 1}))

(defn split
  [t1 t2]
  (fn s [painter n]
    (if (zero? n)
      painter
      (let [p (s painter (dec n))]
        (t1 painter (t2 p p))))))

(def right-split (split beside below))
(def up-split (split below beside))

(defn corner-split
  [painter n]
  (if (zero? n)
    painter
    (let [up (up-split painter (dec n))
          right (right-split painter (dec n))
          top-left (beside up up)
          bottom-right (below right right)
          corner (corner-split painter (dec n))]
      (beside (below painter top-left)
              (below bottom-right corner)))))

(defn square-of-four
  [top-left top-right bottom-left bottom-right]
  (fn [painter]
    (below (beside (bottom-left painter)
                   (bottom-right painter))
           (beside (top-left painter)
                   (top-right painter)))))

(defn square-limit
  [painter n]
  ((square-of-four flip-horiz identity rotate180 flip-vert)
   (corner-split painter n)))
