(defun split-string-on-substring (string substring)
  (let ((pos (search substring string)))
    (if pos
        (values (subseq string 0 pos)
                (subseq string (+ pos (length substring))))
        (values string nil))))

(defun parse-input (filename)
  (with-open-file (stream filename :direction :input)
    (let ((lines '()))
      (loop for line = (read-line stream nil nil)
            while line
            do (push line lines))
      (setq lines (nreverse lines))
      ;; Extract the initial state
      (let* ((initial-state-line (first lines))
             (initial-state (subseq initial-state-line (+ (position #\: initial-state-line) 2)))
             (rules-lines (nthcdr 2 lines))
             (rules (make-hash-table :test 'equal)))
        ;; Parse the rules
        (dolist (line rules-lines)
          (multiple-value-bind (pattern result) (split-string-on-substring line " => ")
            (setf (gethash pattern rules) result)))
        (values initial-state rules)))))

(defun initial-pots (initial-state)
  (let ((pots '()))
    (loop for i from 0 below (length initial-state)
          do (when (char= (char initial-state i) #\#)
               (push i pots)))
    pots))

(defun simulate-generations (initial-state rules generations)
  (let ((pots (initial-pots initial-state)))
    (loop repeat generations
          do (let ((new-pots '()))
               (let* ((min-pot (reduce #'min pots))
                      (max-pot (reduce #'max pots)))
                 (loop for i from (- min-pot 2) to (+ max-pot 2)
                       do (let ((pattern (coerce
                                          (loop for j from (- i 2) to (+ i 2) collect
                                                (if (member j pots) #\# #\.))
                                          'string)))
                            (when (string= (gethash pattern rules) "#")
                              (push i new-pots)))))
               (setq pots new-pots)))
    pots))

(defun calculate-sum-of-plants (pots)
  (reduce #'+ pots))

(defun main ()
  (multiple-value-bind (initial-state rules) (parse-input "input_level_12.txt")
    (let ((generations 20))
      (let ((pots-with-plants (simulate-generations initial-state rules generations)))
        (let ((result (calculate-sum-of-plants pots-with-plants)))
          (format t "The sum of the numbers of all pots which contain a plant after ~A generations is: ~A~%"
                  generations result))))))

(main)
