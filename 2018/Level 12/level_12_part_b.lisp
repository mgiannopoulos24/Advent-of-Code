(defun split-string-on-substring (string substring)
  (let ((pos (search substring string)))
    (if pos
        (values (subseq string 0 pos)
                (subseq string (+ pos (length substring))))
        (values string nil))))

(defun parse-input (filename)
  (with-open-file (stream filename :direction :input)
    (let ((lines '()))
      ;; Read all lines from the file
      (loop for line = (read-line stream nil nil)
            while line
            do (push line lines))
      (setq lines (nreverse lines))
      ;; Extract the initial state
      (let* ((initial-state-line (first lines))
             (colon-pos (position #\: initial-state-line))
             (initial-state (subseq initial-state-line (+ colon-pos 2)))
             (rules-lines (nthcdr 2 lines))
             (rules (make-hash-table :test 'equal)))
        ;; Parse the rules
        (dolist (line rules-lines)
          (multiple-value-bind (pattern result) (split-string-on-substring line " => ")
            (setf (gethash pattern rules) result)))
        ;; Return initial state and rules
        (values initial-state rules)))))

(defun initial-pots (initial-state)
  (let ((pots '()))
    (loop for i from 0 below (length initial-state)
          do (when (char= (char initial-state i) #\#)
               (push i pots)))
    pots))

(defun simulate-generations-efficiently (initial-state rules total-generations)
  (let ((pots (initial-pots initial-state))
        (previous-state nil)
        (previous-offset nil)
        (stabilization-step nil)
        (stabilization-offset nil))
    (let ((min-pot (reduce #'min pots))
          (max-pot (reduce #'max pots)))
      (loop for generation from 1 to total-generations
            do (let ((new-pots '()))
                 ;; Expand the range we check because plants might grow in new pots
                 (loop for i from (- min-pot 2) to (+ max-pot 2)
                       do (let ((pattern (coerce
                                          (loop for j from (- i 2) to (+ i 2) collect
                                                (if (member j pots) #\# #\.))
                                          'string)))
                            (when (string= (gethash pattern rules) "#")
                              (push i new-pots))))
                 (setq pots new-pots)
                 (setq min-pot (reduce #'min pots))
                 (setq max-pot (reduce #'max pots))
                 ;; Calculate the current state as a string of pots
                 (let ((current-state (coerce
                                       (loop for i from min-pot to max-pot collect
                                             (if (member i pots) #\# #\.))
                                       'string))
                       (current-offset min-pot))
                   ;; Check for stabilization
                   (if (and previous-state (string= current-state previous-state))
                       (progn
                         (setq stabilization-step generation)
                         (setq stabilization-offset (- current-offset previous-offset))
                         (format t "Stabilized at generation ~A.~%" generation)
                         (return))  ; Exit the loop
                       (progn
                         (setq previous-state current-state)
                         (setq previous-offset current-offset))))
                 ;; Optionally print progress
                 (when (or (= (mod generation 100000) 0) (= generation 1))
                   (format t "Progress: Generation ~A~%" generation))))
      ;; After loop
      (if stabilization-step
          (let* ((remaining-generations (- total-generations stabilization-step))
                 (pots (mapcar (lambda (i) (+ i (* remaining-generations stabilization-offset))) pots)))
            pots)
          pots))))

(defun calculate-sum-of-plants (pots)
  (reduce #'+ pots))

(defun main ()
  (multiple-value-bind (initial-state rules) (parse-input "input_level_12.txt")
    (let ((total-generations 50000000000))  ; 50 billion generations
      (let ((pots-with-plants (simulate-generations-efficiently initial-state rules total-generations)))
        (let ((result (calculate-sum-of-plants pots-with-plants)))
          (format t "The sum of the numbers of all pots which contain a plant after ~A generations is: ~A~%"
                  total-generations result))))))

(main)
