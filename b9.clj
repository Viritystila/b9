(ns b9 (:use [overtone.live]))

(do
  ;Gloabl pulses
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

 ;Buses
  (do
    ;Control
    (defonce cbus1 (control-bus 1))
    ;Audio
    (defonce abus1 (audio-bus))

    (defonce main-g (group "main bus"))
    (defonce early-g (group "early bus" :head main-g))
    (defonce later-g (group "late bus" :after early-g))
    )

  )



(defsynth sn [freq 22 out-bus 0]
  (let [f_in (in:kr freq)
        src (sin-osc f_in)]
    (out out-bus (pan2 src))))

(def snf (sn [:tail early-g] :freq cbus1  :out-bus abus1))

(control-bus-set! cbus1 22)

(defsynth sn2 [in-bus 0 amp 1]
  (let [src (in in-bus)]
    (out 0 (pan2 (* amp src)))))

(def sn2f (sn2 [:tail early-g] :in-bus abus1))

(defsynth sn3 [out-bus 0 value 22]
  (let [ov (sin-osc:kr value)]
    (out:kr out-bus value)))

(def sn3f (sn3 [:tail early-g] :out-bus cbus1 :value 10))


(ctl sn3f :value 32)

(ctl sn2f :amp 0.9 :in-bus abus1)

(kill sn2f)

(kill snf)

(pp-node-tree)

(show-graphviz-synth sn2)

(stop)
