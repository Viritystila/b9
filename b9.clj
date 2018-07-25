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
    (ctl r-trg :rate 10)
    )

 ;Buses
  (do
    ;Control
    (defonce cbus1 (control-bus 1))
    (defonce cbus2 (control-bus 1))
    (defonce cbus3 (control-bus 1))
    ;Audio
    (defonce abus1 (audio-bus))
                                        ;in bus


    (defonce main-g (group "main bus"))
    (defonce early-g (group "early bus" :head main-g))
    (defonce later-g (group "late bus" :after early-g))
    )
                                        ;Buffers
  (defonce buffer1 (buffer 32))
  (defonce buffer2 (buffer 32))
  (defonce buffer3 (buffer 16))
  (defonce buffer4 (buffer 32))
  )



(defsynth sn [freq 22 out-bus 0]
  (let [f_in (in:kr freq)
        src (sin-osc f_in)]
    (out out-bus (pan2 src))))

(def snf (sn [:tail early-g] :freq cbus1  :out-bus abus1))

(control-bus-set! cbus1 32)

(defsynth sn2 [in-bus 0 amp 1]
  (let [src (in in-bus)]
    (out 0 (pan2 (* amp src)))))

(def sn2f (sn2 [:tail early-g] :in-bus abus1 :amp 0.01))

(defsynth sn3 [out-bus 0 value 22 amp 5]
  (let [ov (sin-osc:kr value)]
    (out:kr out-bus (* amp ov))))

(def sn3f (sn3 [:tail early-g] :out-bus cbus1 :value 3 :amp 60))

(defsynth sn4 [out-bus 0 value 22 amp 5]
  (let [ov (lf-saw:kr value)]
    (out:kr out-bus (* amp ov))))

(def sn4f (sn4 [:tail early-g] :out-bus cbus2 :value 1 :amp 60))

(defsynth sn5 [out-bus 0 fraction 2]
  (let [tr_in ( pulse-divider (in:kr root-trg-bus) fraction)
        env (linen (impulse 0 0.1) 0.1 0.1 0.1 :gate tr_in)
        rnd_src (lf-saw:kr (pink-noise:kr))
        ]
    (out:kr out-bus (* env rnd_src  100000))))

(def sn5f (sn5 [:tail early-g] cbus3))

(kill sn5f)


(ctl sn3f :value (/ 10 16) :amp 200)

(ctl sn2f :amp 0.1 :in-bus abus1)

(ctl snf :freq cbus1)

(ctl sn4f :value 10 :amp 1000)

(kill sn2f)

(kill snf)

(kill sn3f)

(pp-node-tree)

(show-graphviz-synth sn2)

(def buffer1_pattern [20 30 20 30 20 90 20 100 40 20 50 60 20 40 20 40
                      20 40 20 30 40 50 60 70  80 30 40 20 40 20 40 20])

(def buffer1_pattern [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])

(def buffer1_pattern [20 0 20 0 20 0 20 0 20 0 20 0 20 0 20 0
                      30 40 50 60 30 40 50 60 60 50 40 30 20 50 60 20 ])

(def buffer1_pattern [0 20 0 0 0 30 0 0 0 20 0 0 0 40 0 0
                      0 20 0 0 0 50 0 0 0 60 0 0 0 30 0 0])

(buffer-write! buffer1 buffer1_pattern)

(defsynth noisInput [ amp 0.1  fraction BEAT-FRACTION in-bus 0 dec 0.1 attack 0.1 release 0.1]
  (let [src1 (in in-bus)
        tr_in (pulse-divider (in:kr root-trg-bus) fraction)
        indexes (dseq (range (buffer-size buffer1)) INF)
        freqs (dbufrd buffer1 indexes)
        note-gen (demand:kr tr_in 0 freqs)
        note-gen (midicps note-gen)
        env (env-gen (perc attack release) :gate tr_in)
        sawsrc (saw note-gen)
        src2 (+ src1 (decay sawsrc dec) amp)]
    (out 0 (pan2 (* src2 amp env)))))

(def noisInputf (noisInput 0.02 2 abus1 0.1 0.1 0.1))

(ctl noisInputf :in-bus abus1 :amp 0.0025)

(kill noisInputf)

(def buffer2_pattern [1 0 0 0 1 0 1 0 1 0 1 0 1 0 0 0
                      1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0])

(def buffer2_pattern [1 0 0 0 1 0 1 0 1 0 0 0 1 0 0 0
                      1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0])

(def buffer2_pattern [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])

(buffer-write! buffer2 buffer2_pattern)

(defsynth dualPulse [note 22 amp 1 fraction 1 attack 0.1 decay 0.1 sustain 0.2 release 1 del 0.0]
  (let [tr_in (pulse-divider (in:kr root-trg-bus) fraction)
        indexes (dseq (range (buffer-size buffer2)) INF)
        pulses (dbufrd buffer2 indexes)
        pulse_trig (demand:kr tr_in 0 pulses)
        pulse_trig (t-delay:kr pulse_trig del)
                                        ;env (env-gen (asr attack sustain release)  :gate pulse_trig)
        env2 (env-gen (perc attack release) :gate pulse_trig)
        sp1 (sin-osc note)
        sp2 (sin-osc (* note 2))
        sp3 (+ env2 (* sp1 sp2))
        sp4 (clip2 sp3 1)]
    (out 0 (pan2 (* amp sp4 env2)))))

(def dualPulsef (dualPulse :note 15 :amp 1 :fraction 2 :attack 0.01 :sustain 0.05 :release 0.01))

(ctl dualPulsef :amp 1 :note 15 :attack 0.01 :sustain 0.05 :release 0.001)

(kill dualPulsef)

(def buffer4_pattern [0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0
                      0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0])

(buffer-write! buffer4 buffer4_pattern)

(defsynth snare [amp 1 fraction 2 del 0]
  (let [tr_in (pulse-divider (in:kr root-trg-bus) fraction)
        indexes (dseq (range (buffer-size buffer4)) INF)
        pulses (dbufrd buffer4 indexes)
        pulse_trig (demand:kr tr_in 0 pulses)
        pulse_trig (t-delay:kr pulse_trig del)
        env (linen (impulse 0 0.05) 0.01 0.01 0.05 :gate pulse_trig)
        snare (* 3 (pink-noise) (apply + (* (decay env [0.05 0.01]) [1 0.05])))
        snare (+ snare (bpf (* 4 snare) 2000))
        snare (clip2 snare 1)]
    (out 0 (* amp snare))))

(def snaref (snare :amp 1))

(ctl snaref :amp 0.5 :del 0 :fraction 2)

(kill snaref)



(defsynth synther [amp 1 freq 22 in-abus 0 in-cbus 0 dec 0.1 attack 0.1 sustain 0.1 release 0.1 ]
  (let [ in_src (in in-abus)
        in_ctr (in:kr in-cbus)
        src1 (sin-osc freq in_ctr)
        src2 (* src1 in_src)
        src3 (saw freq)
        src4 (+ src2 (decay src3 dec))]
    (out 0 (pan2 (* amp src4)))))

(def syntherf (synther :amp 0.0015 :freq 2 :in-abus abus1 :in-cbus cbus1))

(ctl syntherf :amp 0.02 :freq 2 :in-cbus 0 :dec 0.1)

(kill syntherf)

(def buffer3_pattern [60 58 62 60 58 62 60 58 62 60 58 62 60 58 62 60])

(buffer-write! buffer3 buffer3_pattern)

(defsynth overpad [note 60 amp 0.7 attack 0.001 release 2 fraction 2]
  (let [freq (midicps note)
        tr_in (pulse-divider (in:kr root-trg-bus) fraction)
        indexes (dseq (range (buffer-size buffer3)) INF)
        freqs (dbufrd buffer3 indexes)
        note-gen (demand:kr tr_in 0 freqs)
        freq (midicps note-gen)
        env (env-gen (perc attack release) :gate tr_in :action FREE)
        f-env (+ freq (* 10 freq (env-gen (perc 0.012 (- release 0.01)))))
        bfreq (/ freq 2)
        sig (apply - (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)])) (lpf (saw [freq (* freq 1.01)]) f-env)))
        _ (tap "sig" 60 (a2k sig))]
    (out 0 (pan2 (* amp env sig)))))

(def overpadf (overpad :note 52 :attack 5 :release 5 :amp 0.1 :fraction 40))

(ctl overpadf :attack 5 :release 5 :note 60 :fraction 40)

(kill overpadf)

(defsynth noise [freq 44 amp 1 freq2 44]
  (let [noiseV (pink-noise)
        src1 (sin-osc noiseV)
        src2 (sin-osc (* noiseV 0.9 (in:kr freq2)))
        src3 (lf-saw freq)]
    (out 0 (pan2 (*  amp (+ src1 src2) src3)))))

(def noisef (noise [:tail later-g] :freq2 cbus1 :amp 0.2))

(ctl noisef :freq2 cbus3 :amp 0.1 :freq cbus1)

(kill noisef)

(defsynth in-bus-synth [in-bus 0 gain 10 cutoff 10]
  (let [src (sound-in in-bus)
        srci (lpf src cutoff)
        srco (* gain srci (pink-noise))]
    (out 0 (pan2 (+ srco (sin-osc srco) )))))

(def ibs (in-bus-synth :cutoff 440 :gain 1))

(ctl ibs :gain 1 :cutoff 100)

(kill ibs)
