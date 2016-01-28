(ns music.helper
  (:use [overtone.core]))

(defn lower-bound [in]
  (case (first in)
    latch:ar (lower-bound (second in))
    sin-osc -1
    sin-osc:kr -1
    lf-tri -1
    lf-tri:kr -1
    lf-saw -1
    lf-saw:kr -1
    white-noise -1
    lf-noise0 -1
    lf-noise1 -1
    lf-noise2 -1
    lf-noise0:kr -1
    lf-noise1:kr -1
    lf-noise2:kr -1
    lf-pulse 0
    lf-pulse:kr 0))

(defmacro rg-lin [in lo hi]
  `(lin-lin ~in ~(lower-bound in) 1 ~lo ~hi))
(defmacro rg-exp [in lo hi]
  `(lin-exp ~in ~(lower-bound in) 1 ~lo ~hi))

(defmacro dq [trig arr] `(demand ~trig 0 (dseq ~arr INF)))
(defmacro dq:kr [trig arr] `(demand:kr ~trig 0 (dseq ~arr INF)))
(defmacro dt [dur arr]
  (if (coll? dur)
    `(duty:ar (dseq ~dur INF) 0 (dseq ~arr INF))
    `(duty:ar ~dur 0 (dseq ~arr INF))))

(defmacro dt:kr [dur arr]
  (if (coll? dur)
    `(duty:kr (dseq ~dur INF) 0 (dseq ~arr INF))
    `(duty:kr ~dur 0 (dseq ~arr INF))))

(defn transpose [xss]
  (apply map list xss))

(defn step
  ([t] (step t 0))
  ([t dur]
   (env-gen (envelope [0 0 1] [t dur]))))

(defmacro switch [trig a b]
  `(let [t# ~trig]
     (~'+
      (~'* t# ~a)
      (~'* (~'- 1 t#) ~b))))

(defn m-map [f & xs]
  (let [result (apply map f xs)
        is-single (map? (first result))]
    (if is-single
      (mix result)
      (map mix result))))

(defn n-range [min max num]
  (range min max (/ (- max min) num)))


(defmacro v0 [lower upper]
  `(lin-lin (lf-noise0 8) 0 1 ~lower ~upper))

(defn poll-mouse-x [min max]
  (let [snd (mouse-x min max)]
    (poll (impulse 8) snd "mouse-x")
    snd))
(defn poll-mouse-y [min max]
  (let [snd (mouse-x min max)]
    (poll (impulse 8) snd "mouse-y")
    snd))
(defn poll-ex-mouse-x [min max]
  (let [snd (mouse-x min max EXP)]
    (poll (impulse 8) snd "mouse-x")
    snd))
(defn poll-ex-mouse-y [min max]
  (let [snd (mouse-x min max EXP)]
    (poll (impulse 8) snd "mouse-y")
    snd))

