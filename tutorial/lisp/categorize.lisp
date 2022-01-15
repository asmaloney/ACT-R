; ACT-R tutorial unit8 categorization experiment.
; This task sequentially presents the model with 
; features which the model must classify as
; being small, medium, or large given a numeric
; description, and then after those features have
; been encoded it must make a choice as to which
; category of items it belongs based on the 
; examples that it has pre-encoded in declarative
; memory.  It is an abstraction and simplification
; of a face categorizing task:
;
; Nosofsky, R. M. (1991). Tests of an exemplar model for relating 
; perceptual classification and recognition memory. Journal of Experimental 
; Psychology: Human Perception and Performance. 17, 3-27.
;

; These are the feature sets for the categories (based on
; the general values).

(defvar *cat1* '((small   large   medium  small)
                 (medium  small   large   medium)
                 (small   medium  medium  medium)
                 (small   medium  large   medium)
                 (small   medium  large   large)))
  
(defvar *cat2* '((large   small   small  small)
                 (medium  medium  small  large)
                 (large   small   large  small)
                 (large   small   large  large)
                 (large   small   small  large)))


; This is the data indicating the category 1 choice proportions 
; for the set of stims below (represented by their underlying
; normalized numeric values)

(defparameter *cat-data* '(0.975 0.85 0.987 1.0 0.963 0.075 0.138 0.087 0.05 0.025 0.937 0.544 0.988 0.087))

(defparameter *stims* '((-1.025  0.493  0.048  -0.666)
                        (-0.172  -0.557  0.337  0.163)
                        (-0.98  0.275  -0.005  -0.067)
                        (-0.951  0.259  0.399  0.093)
                        (-0.96  0.198  0.38  0.527)
                        (0.665  -0.441  -0.508  -0.396)
                        (-0.059  0.243  -0.602  0.624)
                        (0.586  -0.511  0.381  -0.507)
                        (0.823  -0.539  0.332  0.633)
                        (0.823  -0.504  -0.487  0.776)
                        (-1.114  -0.52  0.636  -0.028)
                        (-0.154  -0.562  -0.043  0.057)
                        (-0.856  0.197  0.241  0.007)
                        (0.704  -0.287  -0.164  0.178)))


; Global values for the similarity distribution, the mapping of
; sizes to an anchor point, the default slot names, and the
; offset to use for the presented stimulus values.

(defparameter *sigma2* .15)
(defparameter *size-mappings* (list (cons "small" -.9) (cons "medium" 0) (cons "large" .9)))
(defparameter *slots* '(eh es nl mh))
(defvar *attribute-offset* 0)

; Functions for computing the similarity values using a
; normal distribution around the anchor point for a value
; and then scaling them from -1 to 0.

(defun scale-sim (x max)
  (- (/ x max) 1.0))

(defun normal (x sigma2 m)
  (* (/ 1 (sqrt (* 2 pi sigma2))) (exp (- (/ (* (- x m) (- x m)) (* 2 sigma2))))))

(defparameter *max-norm* (normal 0 *sigma2* 0))

(defun size-similarities (a b)
  (let ((number (if (numberp b) (- b *attribute-offset*) nil))
        (size (if (find a '("small" "medium" "large") :test 'string-equal) a nil)))
    
    (when (and number size)
      (scale-sim (normal number *sigma2* (cdr (assoc size *size-mappings* :test 'string-equal))) *max-norm*))))

(add-act-r-command "size-similarities" 'size-similarities "Categorize model's similarity hook function.")

; categorize-stimulus resets the model and then presents the four
; feature values provided (which should be numbers from -2 to 2) to
; the model with the default slot names for features and then 
; creates a goal chunk with state categorize for the model to 
; determine a category for the features it encoded.  If the 
; model provides a category of 1 or 2 (by setting the category slot
; of the chunk in the imaginal buffer) then that value is
; returned, otherwise it returns nil.

(defun categorize-stimulus (a b c d)
  (setf *attribute-offset* 0)
  (setf *slots* '(EH ES NL MH))
  (let ((result (trial (list a b c d))))
    (unless (zerop (cdr result))
      (if (zerop (car result))
          2 1))))

; categorize-attribute takes two values which represent the
; name and value for an attribute in the task.  it presents
; the attribute to the model by setting slots of the chunk in
; the goal buffer and then running the model.  The state slot
; is set to add-attribute, the name slot is set to the name
; provided, and the value slot is set to the value provided.
; The model should encode that value into a general description
; (small, medium, or large) and store that into a slot of the
; chunk in the imaginal buffer with the provided name.
; It does not reset the model.

(defmacro categorize-attribute (name value)
  `(present-one-attribute-fct ',name ,value))

; Create a new chunk for the goal buffer with the values provided
; and run the model.

(defun present-one-attribute-fct (name value)
  (schedule-set-buffer-chunk 'goal `(state add-attribute name ,name value ,(+ *attribute-offset* value)) 0)
  (run 20))

; categorize-experiment takes one required value which
; is how many times to run the whole experiment (one presentation
; of each of the 14 testing stims).  It has one optional parameter
; which indicates an offset to add to the values that are presented
; if it is provided, and accepts 4 additional parameters which
; specify the names of the attributes to present to the model (the
; default names will be used if none are provided).  It runs the
; experiments, determines the proportion of category choices for 
; each item, reports the fit to the experimental data, and prints
; out the proportion of choices for category 1.

(defmacro categorize-experiment (n &optional offset &rest slots)
  `(categorize-fct ,n ,offset ',slots))

(defun categorize-fct (n offset slots)
  
  (if (numberp offset)
      (setf *attribute-offset* offset)
    (setf *attribute-offset* 0))
  
  (cond ((null slots)
         (setf *slots* '(EH ES NL MH)))
        ((not (= (length slots) 4))
         (print-warning "Must specify exactly 4 slot names.  Using defaults instead of ~S" slots)
         (setf *slots* '(EH ES NL MH)))
        ((not (every 'symbolp slots))
         (print-warning "Slot names must be symbols.  Using defaults instead of ~S" slots)
         (setf *slots* '(EH ES NL MH)))
        ((some (lambda (x) (> (count x slots) 1)) slots)
         (print-warning "Slot names must be unique. Using defaults instead of ~S" slots)
         (setf *slots* '(EH ES NL MH)))
        ((find 'category slots)
         (print-warning "Examples already have a category slot. Using defaults instead of ~S" slots)
         (setf *slots* '(EH ES NL MH)))
        (t (setf *slots* slots)))
  
  (let ((results (make-list (length *cat-data*) :initial-element 0))
        (counts (make-list (length *cat-data*) :initial-element 0)))
    (dotimes (i n)
      (let ((data (do-experiment)))
        (setf results (mapcar '+ results (car data)))
        (setf counts (mapcar '+ counts (cdr data)))))
    (setf results (mapcar (lambda (x) (/ x n)) results))
    
    (setf *attribute-offset* 0)
    
    (correlation results *cat-data*)
    (mean-deviation results *cat-data*)
    
    (format t "P(C=1)~%")
    (format t "        ~{(~4d) ~}~%" counts)
    (format t "data  ~{~7,3f~}~%" *cat-data*)
    (format t "model ~{~7,3f~}~%" results)))

(defun do-experiment ()
  (let ((result nil)
        (counts nil))
    (dolist (stim *stims*)
      (let ((data (trial stim)))
        (push (car data) result)
        (push (cdr data) counts)))
    (cons (reverse result) (reverse counts))))

(defun trial (stim)
  (reset)
  
  (dolist (x (permute-list (mapcar 'cons *slots* stim)))
    (present-one-attribute-fct (car x) (cdr x)))
  
  (schedule-set-buffer-chunk 'goal '(state categorize) 0)
  (run 20)
  (let ((category (chunk-slot-value-fct (buffer-read 'imaginal) 'category)))
    
    (cond ((not (numberp category))
           (model-output "Model did not provide a category.")
           (cons 0 0))
          ((= 1 category)
           (cons 1 1))
          ((= 2 category)
           (cons 0 1))
          (t 
           (model-output "Model provided invalid category.")
           (cons 0 0)))))

; create-example-memories is called in the model 
; definition to add chunks for the training examples
; to the model's declarative memory.  The chunks are
; created with the appropriate slots for the features
; based on the values provided by the modeler to
; run the experiment or the default slots if not 
; running the experiment or alternate names were
; not provided.

(defun create-example-memories ()
  (dolist (x *slots*)
    (extend-possible-slots x nil)
    (define-chunks-fct `((,x isa chunk))))
  (dolist (x *cat1*)
    (add-dm-fct `((isa example category 1 ,@(mapcan 'list *slots* x)))))
  (dolist (x *cat2*)
    (add-dm-fct `((isa example category 2 ,@(mapcan 'list *slots* x))))))


(add-act-r-command "create-example-memories" 'create-example-memories "Categorize task function to add the initial example chunks to simulate the training process.")

; Need to load the model after the "create-example-memories" command
; has been added since that command is called during the model
; creation.

(load-act-r-model "ACT-R:tutorial;unit8;categorize-model.lisp")
