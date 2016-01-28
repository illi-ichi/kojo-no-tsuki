(ns kojo-no-tsuki.inst
  (:use [overtone.core]
        [music.helper]))

(def main-vol 1/2)

(defsynth vol [bus 0 s-dur 0 e-dur 0 s-val 1 e-val 1 max-vol 1]
  (out bus (env-gen (envelope [s-val max-vol e-val] [s-dur e-dur]) :action FREE)))

(defcgen koto [freq {:default 440}
                dur {:default 0.5}
                gate {:default 1}
                 vol {:default 1}]
  (:ar (pan2 (m-map (fn [detune]
                      (let [freq (+ freq detune)
                            env (env-gen (envelope [0 1 1 0] [0 dur 0.1]) gate)]
                        (* main-vol vol env (comb-l (* (lf-noise2 3000)
                                              (decay2 gate 0.008 0.04))
                                           (/ 1 freq) (/ 1 freq) dur))))
                    [0 -0.5 0.4]))))

(defcgen broken [freq {:default 440}
                 dur {:default 0.5}
                 gate {:default 1}
                 vol {:default 1}]
  (:ar (let [delay (rg-lin (lf-tri:kr (/ 1 4) 1) 0 0.05)
             snd (pulse (/ freq 4) (rg-lin (lf-noise0 32) 0 1))
             env (env-gen (env-perc 0.05 dur) gate)]
         (pan2 (* main-vol vol env 1/16 (ringz snd freq delay))))))

(defcgen fue [freq {:default 440}
              dur {:default 0.5}
              gate {:default 1}
              vol {:default 1}]
  (:ar (pan2 (let [freq (lag-ud (* freq (rg-lin (sin-osc 2.1) 0.99 1)) 0.3 0.05)
              freq [(* (+ 1 0.01) freq)
                    (* (- 1 0.01) freq)]             
                   s-env (lag (env-gen (envelope [0 0 0.8] [0 0.8]) gate) 0.1)
              snd (* 2 main-vol vol (switch s-env
                                 (+ (sin-osc freq)
                                    (* 1/12 (lf-tri (* freq 8))))
                                 (moog-ff (white-noise) freq 3.95)))]
               snd))))

(defcgen fue-5 [freq {:default 440}
              dur {:default 0.5}
              gate {:default 1}
              vol {:default 1}]
  (:ar (pan2 (let [freq (lag-ud (* 2 3/2 freq (rg-lin (sin-osc 2.1) 0.99 1)) 0.3 0.05)
                   freq [(* (+ 1 0.01) freq)
                         (* (- 1 0.01) freq)]             
                   s-env (lag (env-gen (envelope [0 0 0.8] [0 0.3]) gate) 0.1)
                   snd (* 2 main-vol vol (switch s-env
                                        (+ (sin-osc freq)
                                           (* 1/12 (lf-tri (* freq 8))))
                                        (moog-ff (white-noise) freq 3.95)))]
               snd))))

(defcgen chin [freq {:default 440}
               dur {:default 0.5}
               gate {:default 1}
               vol {:default 1}]
  (:ar (pan2 (apply m-map #(* 3 main-vol vol %2 (sin-osc (* % (* 2 freq)))
                              (env-gen (env-perc %3 %4) gate))
                    (transpose [[4.1  1/2   0.001 0.1]
                                [4    1/16   0.015 0.5]
                                [2.2  1/8 0.05 0.3]
                                [2    1/2 0.01 0.1]
                                [1    1/8 0.1 0.4]])))))

(defcgen yoru [freq {:default 440}
                 dur {:default 0.5}
                 gate {:default 1}
                 vol {:default 1}]
  (:ar (out 0 (let [num 3
                    freq (lag-ud freq 0.1 0.05)
                    freq (+ (* freq 2) (* 10 (sin-osc (repeatedly num #(ranged-rand -5 5)) (/ Math/PI 2))))
                    sound (pm-osc freq (/ freq 2) (rg-lin (sin-osc 1/5) 3 8))]
                (* 1/16  main-vol
                   [(mix (+ sound
                            (* 0.9 (comb-l sound 0.3 (repeatedly num #(ranged-rand 0.1 0.3))))))
                    (mix (+ sound
                            (* 0.9 (comb-l sound 0.3 (repeatedly num #(ranged-rand 0.1 0.3))))))]
                   vol)))))

(defsynth kaze [vol 1]
  (out 0 (pan2 (let [snd (rlpf (white-noise) (rg-lin (sin-osc 1/4) 2000 4000))]
                 (* 1/2 vol
                    (switch (lf-pulse 1/4 0 0.7) (* 4 (squared snd)) snd)
                    (- 1 (* 10000 (lag (dust (rg-lin (lf-tri 0.52 -1) 1 10)) 0.8)))
                    (lf-pulse (a2k (dt 4 [1/2 4])) 0 0.8)))
               (lin-lin (m-map lf-pulse [1/3 2.3 1/5]) 0 1 -1 1))))


(defsynth base [freq 50 vol 1]
  (out 0 (let [f-freq 1/4
               env (sin-osc 1/8)]
           (* 1/2 vol env (rlpf (saw [(* 0.99 freq) (* 1.01 freq)])
                            (rg-lin (sin-osc f-freq) 20 1000))))))

(defsynth muramasa [min-freq 3000
                max-freq 5000
                period 10
                change-freq 4]
  (out 0 (let [base-freq (rg-lin (lf-noise0 change-freq) min-freq max-freq)
               trig (impulse (/ 1 period))
          env #(env-gen:ar (env-perc 0.05 (/ period %)) trig)
          snd (map #(* (sin-osc %) (env %2))
                   (take 20 (iterate #(* (+ 1 (rand 0.1)) %) base-freq))
                   (iterate inc 1))]
           (* 20 (splay snd)))))

(defn moon [start]
  (kojo-no-tsuki.play/play-once start
             (hold (splay (let [freqs (range 400 2000 312)]
                            (map (fn [freq phase]
                                   (let [env (min (sin-osc 0.1 phase) 0)]
                                     (* 1/2 (sin-osc (dt 10 [freq (* 3/2 freq)])) env)))
                                 freqs
                                 (range 0 Math/PI (/ Math/PI (count freqs))))))
                   10 0 FREE)))
(defsynth star []
  (out 0 (let [snd (* (saw (v0 4000 4050))
                (env-gen (env-perc 0.05 0.5)))
         snd2 (comb-c snd 1/5 1/5 2)
         snd3 (comb-c snd2 1/3 1/3 3)]
           [(+ snd (* 1/3 snd3))
            (+ snd (* 2/3 snd2))])))

(defsynth rain []
  (out 0 (tanh (let [period 16
                 gate (impulse (lin-exp (m-map lf-pulse [1 1/3 2.3 1/4]) 0 1 0.1 20) [0 1/4])
                 gate2 (impulse (/ 1 period))]
             (* (+ (* 10 (rlpf (white-noise)
                               (env-gen (envelope [100 100 750 750] [0.02 0 1])
                                        gate)
                               0.4))
                   (* 1/5 (hpf (white-noise)
                               (env-gen (envelope [12000 0] [0.01])
                                        gate))))
                (env-gen (env-perc 0.01 0.15) gate)
                (env-gen (envelope [0 0 1] [0 period]) gate2))))))

(defsynth makibishi-1 [dur 10 min-density 0.1]
  (out 0 (hold (* 2 (pan2 (dust (lin-exp (m-map lf-pulse:kr [1.2 3.4 1/4]) 0 1 min-density 50))
                          (lin-lin (m-map lf-pulse [1/3 1/8]) 0 1 -1 1)))
               (- dur 1) 1 FREE)))



(defsynth makibishi-2 [dur 10]
  (out 0 (hold (pan2 (latch:ar (sin-osc 440) (dust (lin-exp (m-map lf-pulse [1/9 1.3 1/7 1/4]) 0 1 0.05 50)))
                     (m-map lf-saw [1.1 6 1/3]))
               (- dur 1) 1 FREE)))

(defsynth pressure [freq 4000 dur 10 vol 1]
  (let [env (env-gen (envelope [0 1 0.8 0] [8 (- dur 9) 1]) :action FREE)
        fenv (latch (env-gen (envelope [1 1 1.2] [0 dur])) (impulse 4))]
    (out 0 (pan2 (* vol env (sin-osc (* freq fenv)) (lin-lin (* (sin-osc 1/4) (sin-osc 1/9) -1 1 0.5 1)))
                 (lin-lin (m-map lf-tri [1/4 1/11]) 0 1 -1 1)))))


(defsynth energy-flow [dur 5 vol 1 freq-l 200 freq-r 350]
  (out 0 (hold (map (fn [freq1 freq2]
                      (m-map #(* vol (bpf (saw freq1) (rg-exp (sin-osc:kr 1/5 %) 1000 8000) 0.1)
                                 (rg-lin (sin-osc freq2) 2 8))
                             (range 0 3 1)))
                    [freq-l freq-r]
                    [12 1/10])
               (- dur 0.5) 0.5 FREE)))

(defsynth tsubame [dur 10 period 10 freq 1760]
  (out 0 (hold (* 1/2
                (sin-osc (rg-lin (sin-osc 1) 30 64))
                (sin-osc [(* 1.01 freq)
                          (* 0.99 freq)])
                (sin-osc (rg-lin (sin-osc 1/5) 100 200))
                (env-gen (env-perc 0 period) (impulse (/ 1 period))))
               (- dur 1) 1 FREE)))

(defsynth ten []
  (out 0 (let [time [7.5 2.25 1.875 2.8125 0.9375 3.75]
               freq (env-gen (envelope [400 1020 1820 1508 1508 702 702] time) :action FREE)
               pos-freq (env-gen (envelope [1 10 1/2 1/4 1/4 2 1/8] time) :action FREE)]
           (pan2 (* 1/12 (+ (sin-osc (* (sin-osc (rg-lin (sin-osc 1/10) 20 50)) freq))
                            (* 1/4 (sin-osc (* 3 (sin-osc (rg-lin (sin-osc 1/9) 600 800)) freq)))
                            (* 1/12 (sin-osc (* 5 (sin-osc (rg-lin (sin-osc 1/8) 30 50)) freq))))
                    (env-gen (envelope [0.5 1 1 0.5] [2 2 2]) (impulse 1/3))
                    (env-gen (envelope [0   1   1   0   0   1     1   0]
                                       [0.8 5.7 0.7 0.3 0.8 7.835 3.75]) :action FREE))
                 (sin-osc pos-freq)))))

(defn once [dur]
  (env-gen (envelope [0 1 0 0] [0.001 0.001 dur]) :action FREE))

(defsynth atsu [freq 440 dur 30 bus 0 max-ratio 1.2]
  (out bus
       (let [freq-env (env-gen (envelope [1 1 max-ratio] [5 (- dur 5)]) :action FREE)
             freq (* freq freq-env)
             filter-env (rg-exp (sin-osc 0.22 1) 3 20)
             env (env-gen (envelope [0 1 1 0] [5 (- dur 8) 3]) :action FREE)]
         (* 1/4 env (lpf (m-map #(* (sin-osc 1/21 %1)
                                (pulse (* %2 freq) 0.5))
                            (n-range 0 6 4)
                            [1 3/2 7 3])
                     (* filter-env freq))))))
