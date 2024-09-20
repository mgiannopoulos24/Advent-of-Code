;; Function to read all numbers from the input file
(defun read-input (filename)
  "Reads a file containing space-separated numbers and returns them as a list of integers."
  (with-open-file (stream filename)
    (loop for num = (read stream nil nil)
          while num
          collect num)))

;; Recursive function to parse the tree and sum metadata entries
(defun parse-tree (numbers)
  "Parses the tree from the list of numbers.
   Returns a cons of (sum . remaining-numbers)."
  (let ((child-count (first numbers))
        (metadata-count (second numbers)))
    (let ((rest (cddr numbers)))  ; Skip the first two numbers (header)
      (labels ((parse-children (n lst acc)
                 "Recursively parse N child nodes, accumulating the metadata sum."
                 (if (= n 0)
                     ;; Base case: no more children to parse
                     (cons acc lst)
                     ;; Recursive case: parse the next child
                     (let ((result (parse-tree lst)))
                       (parse-children (1- n)
                                       (cdr result)  
                                       (+ acc (car result)))))))
        ;; Parse all child nodes
        (let* ((children-result (parse-children child-count rest 0))
               (children-sum (car children-result))
               (remaining-after-children (cdr children-result))
               (metadata (subseq remaining-after-children 0 metadata-count))
               (metadata-sum (reduce #'+ metadata))
               (total-sum (+ children-sum metadata-sum))
               (remaining-list (subseq remaining-after-children metadata-count)))

          (cons total-sum remaining-list))))))

;; Main function to compute the sum of all metadata entries
(defun sum-metadata (filename)
  "Reads the input file and computes the sum of all metadata entries."
  (let ((numbers (read-input filename)))
    (car (parse-tree numbers))))

;; Helper function to print the sum
(defun print-sum (filename)
  "Computes and prints the sum of all metadata entries from the input file."
  (let ((sum (sum-metadata filename)))
    (format t "The sum of all metadata entries is: ~a~%" sum)))

(defun main ()
  "Main entry point for the Memory Maneuver script."
  (print-sum "input_level_8.txt"))

(main)
