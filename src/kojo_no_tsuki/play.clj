(ns kojo-no-tsuki.play
  (:use [overtone.core]
        [music.helper]
        [kojo-no-tsuki.score]))

(defn schedule-control [now node [{:keys [t param value]} & rest]]
  (when (not (nil? t))
    (let [next-t (+ now (* t 1000))]
      (at next-t (ctl node param value))
      (apply-by next-t schedule-control [next-t node rest]))))

(defmacro play-at [now dur body & schedules]
  `(let [now# ~now
         node# (at now# ~body)]
     (schedule-control now# node# ~(first schedules))
     (apply-by (+ now# ~(* factor dur)) kill [node#])))

(defn play [current start bus [pulse-t & rest]]
  (let [current (+ current (* factor start))]
    (when (not (nil? pulse-t))      
      (apply-by (+ current 500) control-bus-set! [bus 0])
      (apply-by (+ current 550) control-bus-set! [bus 1])
      (let [next-t (+ current pulse-t)]
        (apply-by next-t play [next-t 0 bus rest])))))


(defn make-some-arrays [music-seq]
  (let [[note-arr rythm-arr] (->> music-seq
                                  (filter (fn [[x y]] (not (= x 0))))
                                  (transpose))
        play-arr (->> (map list music-seq (conj (vec (rest music-seq)) [0 0]))
                      (reduce (fn [acc [[x1 y1] [x2 y2]]]
                                (cond
                                  (= x1 0) acc
                                  (= x2 0) (conj acc (+ y1 y2))
                                  :t (conj acc y1)))
                              []))
        start (->> music-seq
                   (take-while (fn [[x y]] (= x 0)))
                   (reduce (fn [acc [x y]] (+ acc y)) 0))]
    [note-arr rythm-arr play-arr start]))

(defn play-melody [now part-seq insts]
  (let [pulse-bus (control-bus)
        [note-arr rythm-arr play-arr start] (make-some-arrays part-seq)]
    (play now start pulse-bus (map #(* factor %) play-arr))
    (for [inst insts]
      (let [vol-bus (control-bus)]
        (demo 300 (let [gate (in:kr pulse-bus)
                        gate (- gate (delay1:kr gate))
                        dur (demand gate 0 (dseq rythm-arr))
                        freq (midicps (demand gate 0 (dseq note-arr)))]
                    (inst freq dur gate (in:kr vol-bus))))
        vol-bus))))

(defmacro play-once [t  body]
  `(let [f# (synth (str "audition-synth-" ~t) (out 0 ~body))]
     (apply-by ~t f# [])))
