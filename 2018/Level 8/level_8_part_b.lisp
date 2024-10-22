;; Function to read all numbers from the input file
(defun read-input (filename)
  "Reads a file containing space-separated numbers and returns them as a list of integers."
  (with-open-file (stream filename)
    (let ((numbers '()))
      (loop for line = (read-line stream nil)
            while line do
              ;; Use WITH-INPUT-FROM-STRING and READ to parse numbers from the line
              (with-input-from-string (line-stream line)
                (loop for num = (read line-stream nil nil)
                      while num
                      do (push num numbers))))
      (nreverse numbers))))

;; Recursive function to parse the tree and compute node values
(defun parse-tree (numbers)
  "Parses the tree from the list of numbers.
   Returns a cons of (value . remaining-numbers)."
  (let ((child-count (first numbers))
        (metadata-count (second numbers)))
    (let ((rest (cddr numbers)))  ; Skip the first two numbers (header)
      ;; Parse all child nodes
      (let ((child-values '()))
        (dotimes (i child-count)
          (let ((result (parse-tree rest)))
            (push (car result) child-values) ; collect child values
            (setf rest (cdr result))))
        (setf child-values (nreverse child-values))
        ;; After parsing children, rest contains the metadata entries followed by the rest
        (let ((metadata (subseq rest 0 metadata-count))
              (remaining (subseq rest metadata-count)))
          ;; Compute the value of the node
          (let ((value
                  (if (= child-count 0)
                      ;; If no child nodes, value is sum of metadata entries
                      (reduce #'+ metadata :initial-value 0)
                      ;; If there are child nodes, metadata entries are indices into child-values
                      (let ((sum 0))
                        (dolist (idx metadata sum)
                          (when (and (> idx 0) (<= idx (length child-values)))
                            (incf sum (nth (1- idx) child-values))))))))
            (cons value remaining)))))))

;; Main function to compute the value of the root node
(defun compute-root-value (filename)
  "Reads the input file and computes the value of the root node."
  (let ((numbers (read-input filename)))
    (car (parse-tree numbers))))

;; Helper function to print the root node value
(defun print-root-value (filename)
  "Computes and prints the value of the root node from the input file."
  (let ((value (compute-root-value filename)))
    (format t "The value of the root node is: ~a~%" value)))

(defun main ()
  "Main entry point for the Memory Maneuver script."
  (print-root-value "input_level_8.txt"))

(main)
