(ns kojo-no-tsuki.score)

(def factor 1250)
(def score-1-1 [[64 1] [64 1] [69 1] [71 1] [72 1] [71 1] [69 2]])
(def score-1-2 [[65 1] [65 1] [64 1] [62 1] [64 3] [0  1]])
(def score-1-3 [[65 1] [62 1] [64 1.5] [64 0.5] [57 3] [0 1]])
(def score-1-4 [[60 1.5] [60 0.5] [59 1] [57 1] [65 1] [65 1] [64 2]
                [62 1] [64 1] [65 1.5] [65 0.5] [64 3] [0 1]])

(def score-2-1 [[64 1] [64 1] [60 1] [64 1] [69 1] [62 0.5] [64 0.5] [65 2]])
(def score-2-2 [[57 1] [59 1] [60 1] [60 1] [59 3] [0  1]])
(def score-2-3 [[57 1] [59 1] [60 1] [59 1] [57 3] [0  1]])
(def score-2-4 [[57 1] [57 1] [57 1] [57 1] [57 1] [59 1] [60 2]
                [59 1] [60 0.5] [59 0.5] [57 1] [57 1] [59 3] [0 1]])

(def score-1 (->> (mapcat identity
                          [score-1-1 score-1-2
                           score-1-1 score-1-3
                           score-1-4
                           score-1-1 score-1-3])
                  (butlast)))
(def score-2 (->> (mapcat identity
                          [score-2-1 score-2-2
                           score-2-1 score-2-3
                           score-2-4
                           score-2-1 score-2-3])
                  (butlast)))

(defn slow [ratio score]
  (map (fn [[note dur] ratio] [note (* dur ratio)])
       score
       (cons 1.8 (repeat ratio))))

(def all-score-1 (mapcat identity [score-1 [[0 7]] (butlast score-1) [[0 1]]
                                   (drop-last 5 score-1) [[0 6]] (slow 1.5 score-1-3)]))
(def all-score-2 (mapcat identity [score-2 [[0 7]] (butlast score-2) [[0 1]]
                                   (drop-last 5 score-2) [[0 6]] (slow 1.5 score-2-3)]))
