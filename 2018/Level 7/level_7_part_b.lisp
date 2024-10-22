;;; Define a function to split a string based on a single character delimiter.
(defun split-string (delimiter string)
  "Split STRING into a list of substrings divided by DELIMITER.
DELIMITER is a single character."
  (let ((result '())
        (current '()))
    (loop for char across string do
          (if (char= char delimiter)
              (progn
                (when current
                  (push (coerce (reverse current) 'string) result))
                (setf current '()))
              (push char current)))
    (when current
      (push (coerce (reverse current) 'string) result))
    (nreverse result)))

;;; Define a function to parse a single instruction line into prerequisite and dependent steps.
(defun parse-instruction (line)
  "Parse a line of instruction into a cons cell (Prerequisite . Dependent).
Example: 'Step C must be finished before step A can begin.' -> (#\C . #\A)"
  (let* ((parts (split-string #\Space line))
         ;; The prerequisite step is the 2nd word (index 1).
         (prereq-char (nth 1 parts))
         ;; The dependent step is the 8th word (index 7).
         (dependent-char (nth 7 parts)))
    ;; Ensure that prerequisite and dependent steps are single characters.
    (unless (and (= (length prereq-char) 1)
                 (= (length dependent-char) 1))
      (error "Invalid instruction format: ~A" line))
    (cons (char prereq-char 0) (char dependent-char 0))))

;;; Define a function to read and parse all instructions from the input file.
(defun read-instructions (filepath)
  "Read instructions from FILEPATH and return a list of (Prerequisite . Dependent) cons cells."
  (with-open-file (stream filepath :direction :input)
    (loop for line = (read-line stream nil)
          while line
          unless (string= line "")
          collect (parse-instruction line))))

;;; Define a function to build the dependency graph.
(defun build-dependency-graph (instructions)
  "Build a hash table mapping each step to its set of prerequisite steps.
Also, collect all unique steps involved.
Returns a list (dependency-graph all-steps)."
  (let ((dependency-graph (make-hash-table :test 'equal))
        (all-steps (make-hash-table :test 'equal)))
    ;; Iterate over each instruction to populate the dependency graph and all-steps set.
    (dolist (instr instructions)
      (let ((prereq (car instr))
            (dependent (cdr instr)))
        ;; Add the prerequisite to the dependent step's list.
        (push prereq (gethash dependent dependency-graph))
        ;; Ensure both prerequisite and dependent steps are in the all-steps set.
        (setf (gethash prereq all-steps) t)
        (setf (gethash dependent all-steps) t)))
    (list dependency-graph all-steps)))

;;; Define a function to copy a hash table.
(defun copy-hash-table (original)
  "Create and return a copy of the ORIGINAL hash table."
  (let ((copy (make-hash-table :test (hash-table-test original))))
    (maphash (lambda (k v)
               (setf (gethash k copy) (copy-list v)))
             original)
    copy))

;;; Define a function to compute the duration of a step.
(defun step-duration (char)
  "Compute the duration of the step for CHAR."
  (+ 60 (1+ (- (char-code char) (char-code #\A)))))

;;; Define a function to simulate the work with multiple workers and step durations.
(defun simulate-work (dependency-graph all-steps)
  "Simulate the work with multiple workers and step durations.
Returns the total time taken to complete all steps."
  (let* ((time 0)
         (num-workers 5)
         (workers (make-array num-workers :initial-element nil)) ; each worker is (current-step . time-remaining)
         (graph-copy (copy-hash-table dependency-graph))
         (completed-steps '())
         (in-progress-steps '()))
    ;; Main loop
    (loop
      ;; Update workers
      (loop for i from 0 below num-workers do
            (let ((worker (aref workers i)))
              (when worker
                (decf (cdr worker))
                (when (<= (cdr worker) 0)
                  ;; Step completed
                  (let ((completed-step (car worker)))
                    (push completed-step completed-steps)
                    ;; Remove step from prerequisites of other steps
                    (maphash (lambda (step prereqs)
                               (setf (gethash step graph-copy)
                                     (remove completed-step prereqs)))
                             graph-copy)
                    ;; Remove from in-progress steps
                    (setf in-progress-steps (remove completed-step in-progress-steps))
                    ;; Set worker to idle
                    (setf (aref workers i) nil))))))
      ;; Check if all steps are completed
      (when (= (length completed-steps) (hash-table-count all-steps))
        (return time))
      ;; Assign available steps to idle workers
      ;; Find available steps
      (let ((available-steps
             (loop for step being the hash-keys of all-steps
                   unless (or (member step completed-steps)
                              (member step in-progress-steps)
                              (gethash step graph-copy))
                   collect step)))
        ;; Sort available steps alphabetically
        (setf available-steps (sort available-steps #'char<))
        ;; Find idle workers
        (let ((idle-workers
               (loop for i from 0 below num-workers
                     when (null (aref workers i))
                     collect i)))
          ;; Assign steps to idle workers
          (loop for i in idle-workers
                for step in available-steps do
                (let ((duration (step-duration step)))
                  (setf (aref workers i) (cons step duration))
                  (push step in-progress-steps)))))
      ;; Advance time
      (incf time))))

;;; Define the main function to execute the strategy.
(defun main (filepath)
  "Main function to process the input file and compute the total time."
  (let* ((instructions (read-instructions filepath))
         (values (build-dependency-graph instructions))
         (dependency-graph (first values))
         (all-steps (second values))
         (total-time (simulate-work dependency-graph all-steps)))
    (format t "Total time to complete all steps: ~A~%" total-time)))

(main "input_level_7.txt")
