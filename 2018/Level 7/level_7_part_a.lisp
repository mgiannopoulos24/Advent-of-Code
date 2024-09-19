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
        ;; Add the dependent step to the dependency graph with its prerequisite.
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
               (setf (gethash k copy) v))
             original)
    copy))

;;; Define a function to determine the order of steps.
(defun determine-step-order (dependency-graph all-steps)
  "Determine the order of steps based on DEPENDENCY-GRAPH and ALL-STEPS.
Returns a string representing the ordered steps."
  (let ((ordered-steps '())
        ;; Create a copy of dependency-graph to modify during processing.
        (graph-copy (copy-hash-table dependency-graph)))
    (loop
      ;; Find all steps that have no prerequisites.
      (let ((available-steps
             (loop for step being the hash-keys of all-steps
                   unless (gethash step graph-copy)
                   collect step)))
        (if (null available-steps)
            (return) ; No available steps left; exit the loop.
            ;; Select the step that comes first alphabetically.
            (let ((next-step (car (sort available-steps #'char<))))
              (push next-step ordered-steps)
              ;; Debugging Statement: Show selected step
              (format t "Selected next step: ~C~%" next-step)
              ;; Remove this step from the all-steps set.
              (remhash next-step all-steps)
              ;; Remove this step from the prerequisites of other steps.
              (maphash (lambda (step prereqs)
                         (setf (gethash step graph-copy)
                               (remove next-step prereqs)))
                       graph-copy)))))
    ;; Since steps were pushed onto the list, reverse it to get the correct order.
    (coerce (reverse ordered-steps) 'string)))

;;; Define the main function to execute the strategy.
(defun main (filepath)
  "Main function to process the input file and compute the order of steps."
  (let* ((instructions (read-instructions filepath))
         (values (build-dependency-graph instructions))
         (dependency-graph (first values))
         (all-steps (second values))
         (step-order (determine-step-order dependency-graph all-steps)))
    (format t "The order of steps is: ~A~%" step-order)))

(main "input_level_7.txt")
