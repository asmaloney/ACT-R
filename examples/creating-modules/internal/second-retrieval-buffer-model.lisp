;;; This model is a test of the module that adds a
;;; second retrieval buffer called r2.  It simply
;;; adds 2 chunks to DM and has one production which 
;;; makes requests to retrieve those chunks using both
;;; the retrieval and r2 buffers.

(clear-all)

(define-model test-r2-buffer
    (sgp :v t :esc t)
  
  (add-dm (value 1)
          (b1 color blue))
  
  (set-base-levels (b1 1))
  
  (p retrieve
     ?goal>
       buffer empty
     ==>
     +goal>
     +retrieval>
     value 1
     +r2>
     color blue))

  
#|
> (run 2)
     0.000   PROCEDURAL              CONFLICT-RESOLUTION
     0.050   PROCEDURAL              PRODUCTION-FIRED RETRIEVE
     0.050   PROCEDURAL              CLEAR-BUFFER GOAL
     0.050   PROCEDURAL              CLEAR-BUFFER RETRIEVAL
     0.050   PROCEDURAL              CLEAR-BUFFER R2
     0.050   GOAL                    SET-BUFFER-CHUNK GOAL CHUNK1
     0.050   DECLARATIVE             start-retrieval
     0.050   PROCEDURAL              CONFLICT-RESOLUTION
     0.418   SECOND-RETRIEVAL-BUFFER SECOND-RETRIEVAL-BUFFER-RETRIEVED-CHUNK B1
     0.418   SECOND-RETRIEVAL-BUFFER SET-BUFFER-CHUNK R2 B1
     0.418   PROCEDURAL              CONFLICT-RESOLUTION
     1.050   DECLARATIVE             RETRIEVED-CHUNK CHUNK0
     1.050   DECLARATIVE             SET-BUFFER-CHUNK RETRIEVAL CHUNK0
     1.050   PROCEDURAL              CONFLICT-RESOLUTION
     1.050   ------                  Stopped because no events left to process
1.05
23
NIL

> (buffer-chunk retrieval r2)
RETRIEVAL: CHUNK0-0 [CHUNK0]
CHUNK0-0
   VALUE  1

R2: B1-0 [B1]
B1-0
   COLOR  BLUE

(CHUNK0-0 B1-0)

|#