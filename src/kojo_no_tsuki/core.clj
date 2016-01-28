(ns kojo-no-tsuki.core
  (:use [overtone.core]
        [kojo-no-tsuki.score]))

(connect-external-server "localhost" 57110)

(use 'music.helper)
(use 'kojo-no-tsuki.play)
(use 'kojo-no-tsuki.inst)

(defn tsubame-taigun [now]
  (doseq [[t freq] [[500 800] [1000 1600] [2000 1200] [4000 1800] [4800 900]
                    [6800 3900] [7500 2800] [8000 500] [8300 1200] [9500 2800]]]
    (play-at (+ now t) 8 (tsubame :freq freq))))

(defn play-intro [start]
  (let [start (+ start   6000)] (play-once start (koto :freq (line 30 5 80) :dur 1.5 :vol 1 :gate (impulse 1/4))))
  (let [start (+ start   8000)] (play-at start 16 (base)))
  (let [start (+ start  12500)] (play-at     start 8 (kaze))))

(defn play-part-1 [start fue-vol chin-vol]
  (doseq [i (range 4)]
    (let [start (+ start  (* i 20000))]       (play-at start 11 (vol fue-vol 6.25 3.75 0 0.4)))
    (let [start (+ start (* i 20000) 10000)
          long (if (= i 3) 10 7.5)] (play-at start 11 (vol fue-vol 2.5 long 0.4 0))))
  (let [start (+ start     0)] (play-at     start 10 (vol chin-vol 5 0 1/64 1)))
  (let [start (+ start  2500)] (play-at     start 60 (kaze))) 
  (let [start (+ start  7500)] (play-at start 30 (base :vol 0.5)))
  (let [start (+ start 20000)] (play-at start 60 (makibishi-2 60)))
  (let [start (+ start 37500)] (play-at start 30 (rain)))
  (let [start (+ start 46500)] (play-at start 5 (star)))
  (let [start (+ start 77000)] (play-at start 5 (star)))
  (let [start (+ start 72000)] (play-at start 10 (tsubame :freq 2200)))
  (let [start (+ start 79000)] (play-at start 10 (tsubame :freq 1850)))
  
  (let [now (+ start 76000)]
    (let [start (+ now 0)] (play-at start 6 (muramasa)))
    (let [start (+ now 0)] (play-at start 10 (makibishi-1 10) [{:t 4, :param :min-density, :value 2}
                                                               {:t 2, :param :min-density, :value 40}]))
    (let [start (+ now 6000)] (tsubame-taigun start))
    (let [start (+ now 6000)] (play-at start 12 (makibishi-2 12)))    
    (let [start (+ now 0)] (play-at start 5 (kaze 0.1))))  )

(defn play-part-2 [start broken-vol chin-vol]
  (play-at start 1 (vol broken-vol 1 0 1 0.8))
  (play-at start 1 (vol chin-vol 1 0 1 0.8))
  (doseq [t [4 12 22 28 36 49 58 66]] (tsubame-taigun (+ start (* t 1000))))
  
  (let [start (+ start 20000)] (play-at start 40 (makibishi-1 40)))
  (let [start (+ start 22500)] (play-at start 40 (kaze :vol 0.5)))
  (let [start (+ start 18000)] (play-at start 50 (muramasa) [{:t 20, :param :max-freq, :value 8000}
                                                             {:t 10, :param :period, :value 15}
                                                             {:t 10, :param :change-freq, :value 8}
                                                             {:t 10, :param :period, :value 20}]))
  (let [start (+ start 34000)]
    (play-once start (yoru :freq (env-gen (envelope [88 88 90 128] [0 2 7]) (impulse 1/10))
                           :vol (env-gen (envelope [0 0.8 1 0] [5 30 10]) :action FREE))))

  (let [start (+ start 46250)] (play-at start 30 (energy-flow 30 0.1)))
  (let [start (+ start 75800)]
    (play-at (- start 500) 10 (muramasa))
    (play-at (- start 500) 15 (makibishi-2 15))
    (play-once start (chin :freq (midicps 69) :dur 10 :vol 0.5 :gate (once 10)))    
    (play-once start (broken :freq (midicps 69) :dur 10 :vol 0.8 :gate (once 10)))
    (play-once start (koto :freq (midicps 59) :dur 10 :vol 0.3 :gate (once 10)))))

(defn play-part-3 [start broken-vol chin-vol fue-vol fue-5-vol koto-vol yoru-vol]
  (play-at start 1 (vol broken-vol 3 0 0 1))
  (play-at start 1 (vol chin-vol 3 0 0 0.5))
  (doseq [i (range 4)
          vol-bus [fue-vol fue-5-vol yoru-vol]]
    (let [start (+ start  (* i 20000))]
      (play-at start 11 (vol vol-bus 6.25 3.75 0 0.2 0.8)))
    (let [start (+ start (* i 20000) 10000)]
      (if (not (= i 3)) (play-at start 11 (vol vol-bus 2.5 7.5 0 0 0.8)))))
  (let [start (+ start 7500)] (play-at start 55 (base :vol 0.5)))
  (let [start (+ start 10000)] (play-at start 60 (tsubame 60) [{:t 10 :param :freq :value 1581}
                                                               {:t 10 :param :freq :value 1407}]))
  (doseq [[freq t] [[4000 0] [6300 3] [8900 10] [12900 20]]]
    (let [start (+ start 48000 (* t 1000))] (play-at start 30 (pressure freq (- 30 t) 0.1))))

  (doseq [[dur freq bus ratio] [[80 440 0 1.2]
                                [60 660 1 1.2]
                                [30 220 1 3]]]
    (play-at (- (+ start 78000) (* dur 1000)) 80 (atsu freq dur bus ratio)))
  
  (let [start (+ start 70000)] (play-at start 20 (ten)))
  (let [start (+ start 78000)] (play-at start 5 (star)))
  (let [start (+ start (* factor (+ 64 -8 6)))]
    (play-at start 3 (doseq [vol-bus [koto-vol chin-vol broken-vol fue-vol fue-5-vol yoru-vol]]
                       (vol vol-bus 10 8 0.2 0 0.8)))))

(let [now (now)
      part-1 25000
      part-2 (+ part-1 (* factor (+ 64 -1 7)))
      part-3 (+ part-2 (* factor (+ 64 -4 1)))
      [broken-vol chin-vol fue-vol fue-5-vol] (play-melody (+ now part-1) all-score-1 [broken chin fue fue-5])
      [koto-vol yoru-vol] (play-melody (+ now part-1) all-score-2 [koto yoru])]
  (doseq [start [0 15000
                 (+ part-1 6000) (+ part-1 18000)
                 (+ part-1 30000) (+ part-1 70000)
                 (+ part-2 8000) (+ part-2 18000)
                 (+ part-2 70000)
                 (+ part-3 6000) (+ part-3 17000)
                 (+ part-3 37000) (+ part-3 70000)
                 (+ part-3 87500)]]
    (moon (+ now start)))
  (vol koto-vol)
  (play-intro now)
  (play-part-1 (+ now part-1) fue-vol chin-vol)
  (play-part-2 (+ now part-2) broken-vol chin-vol)
  (play-part-3 (+ now part-3) broken-vol chin-vol fue-vol fue-5-vol koto-vol yoru-vol))





