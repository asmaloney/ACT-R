(clear-all)

(require-extra "WNLexicalModule")

(define-model find-synonyms
  
  (sgp :wnl-chunks wnl)

  (chunk-type goal word-str synset-id state)
  
  (define-chunks 
   (find-word-definition isa chunk)
   (find-word-gloss isa chunk)
   (find-synonyms isa chunk)
   (say-definition isa chunk)
   (g1 isa goal word-str "wiener" state find-word-definition)
   (g2 isa goal word-str "dog" state find-word-definition))
  
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
     synset-id =synset-id
     
     ==>
     
     !output! (the definition of =word is =gloss)
     =goal>
     state find-synonyms
     synset-id =synset-id
     
     +wn-lexical>
     isa wnl-request
     wn-operator s
     synset-id =synset-id
     
     )

  (p find-synonyms
     =goal> 
     isa goal
     state find-synonyms
     synset-id =synset-id
     
     ?wn-lexical>
     state free
     state success
     
     =wn-lexical>
     isa s
     word =word
     synset-id =synset-id
     
     ==>
     
     !output! (=word is a synonyms)
    
     +wn-lexical>
     isa wnl-request
     wn-operator s
     synset-id =synset-id
     context-criterion set-difference
     
     )
  

  (goal-focus g2)
  )



#|  Sample run
 
? (run 1)
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL G2 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.050   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION
LOOKING FOR SENSE OF "dog"
     0.050   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL
     0.050   WN-LEXICAL             RETRIEVE-WN-CHUNKS
     0.050   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 8 CRITERION IS RANDOM FROM RETRIEVED-CHUNKS (S-202001858-7 S-110114209-2 S-110023039-1 S-109886220-4 S-107676602-5 S-103901548-4 S-102710044-3 S-102084071-1)
     0.050   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-110114209-2
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.100   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS
LOOKING FOR DEFINITION OF "dog"
     0.100   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL
     0.100   WN-LEXICAL             RETRIEVE-WN-CHUNKS
     0.100   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 CRITERION IS RANDOM FROM RETRIEVED-CHUNKS (G-110114209-1)
     0.100   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-110114209-1
     0.100   PROCEDURAL             CONFLICT-RESOLUTION
     0.150   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION
THE DEFINITION OF "dog" IS "a dull unattractive unpleasant girl or woman; \"she got a reputation as a frump\"; \"she''s a real dog\""
     0.150   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL
     0.150   WN-LEXICAL             RETRIEVE-WN-CHUNKS
     0.150   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 2 CRITERION IS RANDOM FROM RETRIEVED-CHUNKS (S-110114209-2 S-110114209-1)
     0.150   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-110114209-1
     0.150   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   PROCEDURAL             PRODUCTION-FIRED FIND-SYNONYMS
"frump" IS A SYNONYMS
     0.200   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL
     0.200   WN-LEXICAL             RETRIEVE-WN-CHUNKS
     0.200   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 2 CRITERION IS SET-DIFFERENCE BETWEEN RETRIEVED-CHUNKS (S-110114209-2 S-110114209-1) AND CONTEXT (G-110114209-1 S-110114209-2 S-110114209-1)
     0.200   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   ------                 Stopped because no events left to process
0.2
57
NIL
|#

