(ns gameoflife.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as st]))

(enable-console-print!)

;  clj -R:repl build.clj figwheel

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

(defn flip-cell [[ind sqstate]]
  [ind ({0 1 1 0} sqstate)])

(defn square-click [ind curr-board]
  (update-in curr-board [:contents ind] flip-cell))

(defn square [cell-size xnum ind colour curr-board]
  (let [box-side (str cell-size "px")
        [x y] (index-to-coords xnum ind cell-size)]
    [:g
     {:transform (str "translate(" x "," y ")")}
     {:style {:background  colour}}
     [:rect ^{:key ind}
      {:on-click (fn [] (swap! curr-board #(square-click ind @curr-board)))
       :style
       {:border "1px solid black"
        :fill  colour
        :width box-side
        :height box-side}}]]))

(defn live-square [cell-size xnum ind curr-board]
  (square cell-size xnum ind "black" curr-board))

(defn dead-square [cell-size xnum ind curr-board]
  (square cell-size xnum ind "white" curr-board))

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

(defn blank-board [cell-size xnum ynum]
  (let [conts (mapv #(vector % 0) (range (* xnum ynum)))]
   {:xnumber xnum
     :ynumber ynum
     :nb-indices (mapv #(neighbour-indices % xnum ynum) (range (* xnum ynum)))
     :xsize (* xnum cell-size)
     :ysize (* ynum cell-size)
     :next-xnumber  xnum
     :next-ynumber ynum
     :cell-size cell-size
     :contents conts}))

(defn board-with-glider  [cell-size xnum ynum xglider yglider]
  (let [glider (glider-state xglider yglider)
        conts (mapv #(vector % (get glider (index-to-label xnum %) 0) )(range (* xnum ynum)))]
   {:xnumber xnum
    :ynumber ynum
    :nb-indices (mapv #(neighbour-indices % xnum ynum) (range (* xnum ynum)))
    :xsize (* xnum cell-size)
    :ysize (* ynum cell-size)
    :next-xnumber  xnum
    :next-ynumber  ynum
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
     :next-xnumber  xnum
     :next-ynumber  ynum
     :cell-size cell-size
     :contents conts}))

(defn random-board [cell-size xnum ynum]
  {:xnumber xnum
   :ynumber ynum
   :nb-indices (mapv #(neighbour-indices % xnum ynum) (range (* xnum ynum)))
   :xsize (* xnum cell-size)
   :ysize (* ynum cell-size)
   :next-xnumber  xnum
   :next-ynumber ynum
   :cell-size cell-size
   :contents (vec (map-indexed (fn [ind n] [ind n]) (probability-pick (* xnum ynum) 0.5 [1 0])))})

(defn square-render [s cell-size xnum ind curr-board]
  (if (zero? s)
    (dead-square cell-size xnum ind curr-board)
    (live-square cell-size xnum ind curr-board)))

(defn board-maker [b]
  (for [[ind n] (:contents @b)]
    (square-render n (:cell-size @b) (:xnumber @b) ind b)))

(defn board-render [b]
  (let [title [:h1.head "Game of life"]
        next [:div [:input {:type "button" :value "Next board" :class "button"
                      :on-click #(swap! b board-next-state)}]]
        xset [:div [:h3.head "Next x size"]
              [:input.textything {:type "text" :default-value (:next-xnumber @b)
                       :on-input (fn [e] (swap! b assoc :next-xnumber
                                                (int (-> e .-target .-value))))}]]
        yset  [:div [:h3.head "Next y size"]
               [:input.textything {:type "text" :default-value (:next-ynumber @b)
                        :on-input (fn [e] (swap! b assoc :next-ynumber
                                                 (int (-> e .-target .-value))))}]]
        newboard [:div  [:input {:type "button" :value "New board" :class "button"
                                 :on-click #(reset! b 
                                                    (blank-board 20 (:next-xnumber @b)
                                                                 (:next-ynumber @b)))}]]
        life-display (into [:svg
                            {:style
                             {:border "1px solid black"
                              :width (:xsize @b)
                              :height (:ysize @b)}}]
                           (board-maker b))]
    [:div title next xset yset newboard life-display]))

(def board-state (atom (board-with-glider 20 50 50 1 1)))

(defn board []
   [:div 
    [:div (board-render board-state)]])

(reagent/render-component
 [board]
 (. js/document (getElementById "app")))

