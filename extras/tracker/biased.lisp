#|
This file contains a model with no experiment.

The model consists of just one production which creates 
two trackers showing the options for creating a bias in
the initial distribution.  That can be done by providing
either a list of arbitrary x,y values to initialize the
quadratic or by just providing the y values in which case
the x values used will be the possible choices (in order).

After running the model to fire the production the stats
of the trackers can be inspected with print-tracker-stats
to see the distrubitions.  There is an example run provided
at the end of this file.

|#

(clear-all)
(require-extra "tracker")


(define-model bias-initial
    
  ;; set a seed to generate the specific trace below
  (sgp :seed (1045 1))
         
  (sgp :v t :trace-detail low)
  
  ;; create a chunk-type to provide the slot names
  ;; used in the example.
  (chunk-type dummy x-y-values only-y-values)
  
  ;; create chunks for tracker names and good slot to avoid warnings
  (define-chunks x-y-values only-y-values color)
  
  (p create
     ?goal>
       buffer empty
    ==>
     +goal>
       
     ;; First tracker generated with specific x,y values
     ;; to create the initial quadratic
     +tracker>
       isa track-outcome
       control-slot x-y-values
       good-slot color
       min 9 max 18
       x-bias (2 5 20 20)
       y-bias (-10 -1 2 2)
       
     ;; second tracker, specify values for
     ;; the possible choices
     +tracker>
       isa track-outcome
       control-slot only-y-values
       good-slot color
       min 9 max 18
       y-bias (-10 -10 -10 -10 0 0 0 0 0 0 2 2 2 2 2 2 2 2 2 2 2)))

#| Example run
CG-USER(140): (run .05)
     0.050   PROCEDURAL             PRODUCTION-FIRED CREATE
     0.050   GOAL                   SET-BUFFER-CHUNK-FROM-SPEC GOAL 
     0.050   ------                 Stopped because time limit reached
0.05
19
NIL
CG-USER(141): (print-tracker-stats)
Tracker named TRACKER1 for slot ONLY-Y-VALUES (active)
 Good slot is COLOR in buffer IMAGINAL with weight 1
  Current temperature is: 1.0
  Current equation is: -234022/3059 + 345700/33649x + -302000/908523x^2
  Choices:  9.000  9.450  9.900 10.350 10.800 11.250 11.700 12.150 12.600 13.050 13.500 13.950 14.400 14.850 15.300 15.750 16.200 16.650 17.100 17.550 18.000
  Probs:    0.000  0.000  0.000  0.000  0.000  0.000  0.001  0.004  0.010  0.022  0.042  0.070  0.102  0.131  0.147  0.144  0.123  0.092  0.060  0.034  0.017
  Current setting is: 15.3
Tracker named TRACKER0 for slot X-Y-VALUES (active)
 Good slot is COLOR in buffer IMAGINAL with weight 1
  Current temperature is: 1.0
  Current equation is: -158/9 + 184/45x + -7/45x^2
  Choices:  9.000  9.450  9.900 10.350 10.800 11.250 11.700 12.150 12.600 13.050 13.500 13.950 14.400 14.850 15.300 15.750 16.200 16.650 17.100 17.550 18.000
  Probs:    0.007  0.012  0.020  0.030  0.043  0.058  0.073  0.087  0.097  0.101  0.099  0.091  0.079  0.064  0.049  0.035  0.024  0.015  0.009  0.005  0.003
  Current setting is: 12.6
((TRACKER1 ONLY-Y-VALUES 1 (-234022/3059 345700/33649 -302000/908523)) (TRACKER0 X-Y-VALUES 1 (-158/9 184/45 -7/45)))
|#


