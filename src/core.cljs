(ns gameoflife.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as st]))

(enable-console-print!)

(println "This text is printed from src/gameoflife/core.cljs. Go ahead and edit it and see reloading in action.")

;  clj -R:repl build.clj figwheel

;<g transform="translate(0,200)" data-reactid=".0.1.0.$0,4"><g data-reactid=".0.1.0.$0,4.0"><rect style="fill:#D7DBDD;" width="50" height="50" data-reactid=".0.1.0.$0,4.0.0"></rect><polygon style="fill:#E5E7E9;" points="1,49 49,1 1,1" data-reactid=".0.1.0.$0,4.0.1"></polygon><polygon style="fill:#979A9A;" points="49,1 1,49 49,49" data-reactid=".0.1.0.$0,4.0.2"></polygon><rect style="fill:#D0D3D4;cursor:pointer;" width="44" height="44" x="3" y="3" data-reactid=".0.1.0.$0,4.0.3"></rect></g></g>

(defn n-randoms [n]
  "produces n random numbers picked from 0 to 1"
  (mapv #(rand 1) (range n)))

(defn probability-pick [n p1 [c1 c2]]
  "Makes a collection of length n that contains c1 with probability p1 and c2 otherwise."
  (let [randos (n-randoms n)
        selector (fn [r p [el1 el2]]
                   (if (< r p) el1 el2))]
    (mapv #(selector % p1 [c1 c2]) randos)))

(defn index-to-label [xnum ind]
  [(rem ind xnum) (quot ind xnum)])

(defn label-to-index [xnum [x y]]
  (+ (* y xnum) x))

(defn index-to-coords [xnum ind cell-size]
  [(* cell-size (rem ind xnum))
   (* cell-size (quot ind xnum))])

(defn in-window [num [lower upper]]
  (and (>= num lower) (<= num upper)))

(defn label-in-bounds [[xlabel ylabel] xnum ynum]
  (and (in-window xlabel [0 (dec xnum)])
       (in-window ylabel [0 (dec ynum)])))

(defn square [cell-size xnum ind colour]
  (let [box-side (str cell-size "px")
        [x y] (index-to-coords xnum ind cell-size)]
    [:g {:transform (str "translate(" x "," y ")")}
     {:style {:background  colour}}
     [:rect ^{:key ind}
         {:style
          {:border "1px solid black"
           :fill  colour
           :width box-side
           :height box-side}}]]))

(defn live-square [cell-size xnum ind]
  (square cell-size xnum ind "black"))

(defn dead-square [cell-size xnum ind]
  (square cell-size xnum ind "white"))

(defn count-live-neighbours [conts nb]
  (reduce + (mapv #((conts %) 1) nb)))

(defn count-board-neighbours [curr-board]
  (let [conts (:contents curr-board)
        nbs (:nb-indices curr-board)]
    (mapv #(count-live-neighbours conts %) nbs)))

(defn cell-next-state [cellcurr livenbs]
  (let [keep-alive (if (#{2 3} livenbs) 1 0)
        make-alive (if (= 3 livenbs) 1 0)
        keep-make {0 make-alive 1 keep-alive}]
    [(cellcurr 0) (keep-make (cellcurr 1))]))

(defn board-next-state [curr-board]
  (let [num-nbs (count-board-neighbours curr-board)
        next-conts (mapv #(cell-next-state %1 %2) (:contents curr-board) num-nbs)]
    (assoc curr-board :contents next-conts)))

(defn print-board [b]
  (let [cb (:contents b)
        brows (partition (:xnumber b) cb)
        brow-entries (mapv (fn [r] (mapv #(% 1) r)) brows)]
    brow-entries))

(defn print-neighbours [b]
  (let [nb (count-board-neighbours b)
        brows (into [] (partition (:xnumber b) nb))]
    brows))

(defn flip-square [[ind sqstate]]
  [[ind ({0 1 1 0} sqstate)]])

(defn neighbour-indices [index xnum ynum]
  (let [nb-adds  [-1 1 (- xnum) (- 1 xnum) (- 0 1 xnum)
                  xnum (inc xnum) (dec xnum)]
        cand-inds (mapv #(+ index %) nb-adds)
        cand-points (mapv #(index-to-label xnum %) cand-inds)
        neighbour-labels (filterv (fn [label] (label-in-bounds label xnum ynum)) cand-points)]
    (mapv #(label-to-index xnum %) neighbour-labels)))

(defn glider-state [x y]
  {[x y] 0
   [(+ x 1) y] 1
   [(+ x 2) y] 0
   [x (+ y 1)] 0
   [(+ x 1) (+ y 1)] 0
   [(+ x 2) (+ y 1)] 1
   [x (+ y 2)] 1
   [(+ x 1) (+ y 2)] 1
   [(+ x 2) (+ y 2)] 1})

(defn blinker-state [x y]
  {[x y] 1 [(inc x) y] 1 [(+ 2 x) y] 1})

(defn board-with-glider  [cell-size xnum ynum xglider yglider]
  (let [glider (glider-state xglider yglider)
        conts (mapv #(vector % (get glider (index-to-label xnum %) 0) )(range (* xnum ynum)))]
   {:xnumber xnum
   :ynumber ynum
   :nb-indices (mapv #(neighbour-indices % xnum ynum) (range (* xnum ynum)))
   :xsize (* xnum cell-size)
   :ysize (* ynum cell-size)
   :cell-size cell-size
   :contents conts}))

(defn board-with-blinker  [cell-size xnum ynum xblinker yblinker]
  (let [glider (blinker-state xblinker yblinker)
        conts (mapv #(vector % (get glider (index-to-label xnum %) 0) )(range (* xnum ynum)))]
    {:xnumber xnum
     :ynumber ynum
     :nb-indices (mapv #(neighbour-indices % xnum ynum) (range (* xnum ynum)))
     :xsize (* xnum cell-size)
     :ysize (* ynum cell-size)
     :cell-size cell-size
     :contents conts}))

(defn random-board [cell-size xnum ynum]
  {:xnumber xnum
   :ynumber ynum
   :nb-indices (mapv #(neighbour-indices % xnum ynum) (range (* xnum ynum)))
   :xsize (* xnum cell-size)
   :ysize (* ynum cell-size)
   :cell-size cell-size
   :contents (vec (map-indexed (fn [ind n] [ind n]) (probability-pick (* xnum ynum) 0.5 [1 0])))})

(def test-board
  (board-setup 20 10 10))

(defn square-render [s cell-size xnum ind]
  (if (zero? s)
    (dead-square cell-size xnum ind)
    (live-square cell-size xnum ind)))

(defn board-maker [b]
  (for [[ind n] (:contents b)]
    (square-render n (:cell-size b) (:xnumber b) ind)))

(defn board-render [b]
  (into [:svg
         {:style
          {:border "1px solid"
           :width (:xsize b)
           :height (:ysize b)}}]
        (board-maker b)))

(def board-state (atom (board-with-blinker 20 10 10 1 1)))

(defn next-button []
  [:input {:type "button" :value "Next board"
           :on-click #(swap! board-state board-next-state)}])

(defn reset-button []
  [:input {:type "button" :value "Next board"
           :on-click #(reset! board-state (board-with-blinker 20 10 10 1 1))}])

(defn board []
   [:div
    [:h1 "Game of life"]
    [:div [:input {:type "button" :value "Next board"
                   :on-click #(swap! board-state board-next-state)}]]
    [:div  [:input {:type "button" :value "Reset board"
                    :on-click #(reset! board-state
                                      (board-with-blinker 20 10 10 1 1))}]]
    [:div (board-render @board-state)]])

(reagent/render-component
 [board]
 (. js/document (getElementById "app")))

