(clear-all)

(require-extra "WNLexicalModule")

(define-model find-definition
  
  (sgp :wnl-chunks wnl)

  (chunk-type goal word-str state)
  
  (define-chunks 
   (find-word-definition isa chunk)
   (find-word-gloss isa chunk)
   (say-definition isa chunk)
   (g1 isa goal word-str "wordnet" state find-word-definition))
  
  (p find-word-definition
     =goal> 
     isa goal
     state find-word-definition
     word-str =word
     
     ?wn-lexical>
     state free
     state success
     
     ==>
     
     +wn-lexical>
     isa wnl-request
     wn-operator s
     word =word
     
     !output! (Looking for sense of =word)
     
     =goal>
     state find-word-gloss
     )
  
  (p do-not-know-word
     =goal> 
     isa goal
     state find-word-gloss
     word-str =word
     
     ?wn-lexical>
     state free
     state error
     
     ==>
     
     !output! (I know nothing about =word)
     
     -goal>
     
     )
  
  (p find-word-gloss
     =goal> 
     isa goal
     state find-word-gloss
     word-str =word
     
     ?wn-lexical>
     state free
     state success
     
     =wn-lexical>
     isa s
     word =word
     synset-id =synset-id
     
     ==>
     
     +wn-lexical>
     isa wnl-request
     wn-operator g
     synset-id =synset-id
     
     !output! (Looking for definition of =word)
     
     =goal>
     state say-definition
     )
  
  (p say-definition
     =goal> 
     isa goal
     state say-definition
     word-str =word
     
     ?wn-lexical>
     state free
     state success
     
     =wn-lexical>
     isa g
     gloss =gloss
     
     ==>
     
     !output! (the definition of =word is =gloss)
     -goal>
     )
  

  (goal-focus g1)
  )


#|  Sample run
 
? (run 5)
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL G1 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.050   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION
LOOKING FOR SENSE OF "wordnet"
     0.050   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL
     0.050   WN-LEXICAL             RETRIEVE-WN-CHUNKS
     0.050   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 CRITERION IS RANDOM FROM RETRIEVED-CHUNKS (S-106639428-1)
     0.050   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-106639428-1
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.100   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS
LOOKING FOR DEFINITION OF "wordnet"
     0.100   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL
     0.100   WN-LEXICAL             RETRIEVE-WN-CHUNKS
     0.100   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 CRITERION IS RANDOM FROM RETRIEVED-CHUNKS (G-106639428-1)
     0.100   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-106639428-1
     0.100   PROCEDURAL             CONFLICT-RESOLUTION
     0.150   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION
THE DEFINITION OF "wordnet" IS "any of the machine-readable lexical databases modeled after the Princeton WordNet"
     0.150   PROCEDURAL             CLEAR-BUFFER GOAL
     0.150   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL
     0.150   PROCEDURAL             CONFLICT-RESOLUTION
     0.150   ------                 Stopped because no events left to process
0.15
41
NIL
|#

