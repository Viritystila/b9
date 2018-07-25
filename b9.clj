(ns b9 (:use [overtone.live]))

(do
  (defonce root-trg-bus (control-bus)) ;; global metronome pulse
  (defonce root-cnt-bus (control-bus)) ;; global metronome count
  (defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
  (defonce beat-cnt-bus (control-bus)) ;; beat count
  (def BEAT-FRACTION "Number of global pulses per beat" 30)
  )

(do
  (defsynth root-trg [rate 100]
    (out:kr root-trg-bus (impulse:kr rate)))

  (defsynth root-cnt []
    (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))

  (defsynth beat-trg [div BEAT-FRACTION]
    (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div)))

  (defsynth beat-cnt []
    (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus)))))

(do
  (def r-trg (root-trg))
  (def r-cnt (root-cnt [:after r-trg]))
  (def b-trg (beat-trg [:after r-trg]))
  (def b-cnt (beat-cnt [:after b-trg]))
  (ctl r-trg :rate 10))





(def BEAT-FRACTION "Number of global pulses per beat" 30)


(defonce bus1 (control-bus 1))

(defonce bus2 (audio-bus))

(defonce beatbuffer (buffer 32))

(defsynth sn [freq 22 out-bus 0]
  (let [f_in (in:kr freq)
        src (sin-osc f_in)]
    (out out-bus (pan2 src))))

(def snf (sn :freq  bus1 :out-bus bus2))

(control-bus-set! bus1 22)

(defsynth sn2 [in-bus 0 amp 1]
  (let [src (in in-bus)]
    (out 0 (pan2 (* amp src)))))

(def sn2f (sn2 :in-bus bus2))

(defsynth sn3 [out-bus 0 value 22]
  (let [ov (sin-osc:kr value)]
    (out:kr out-bus value)))

(def sn3f (sn3 :out-bus bus1 :value 10))





(ctl sn3f :value 32)

(ctl sn2f :amp 0.9)

(kill sn2f)

(kill snf)

(pp-node-tree)

(show-graphviz-synth sn2)

(stop)
