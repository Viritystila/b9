(ns b9 (:use [overtone.live]))


(defonce bus1 (control-bus 1))

(defonce bus2 (audio-bus))


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

(ctl sn3f :value 22)

(ctl sn2f :amp 0.15)

(kill sn2f)

(kill snf)

(pp-node-tree)

(show-graphviz-synth sn2)

(stop)
