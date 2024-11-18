#lang stacker/smol/hof
#:no-trace

(defvar x (mpair 76 18))
(set-right! x x)
x



