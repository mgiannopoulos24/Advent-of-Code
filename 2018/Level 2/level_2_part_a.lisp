(defun count-letters (id)
  "Count the frequency of each letter in the given ID."
  (let ((freqs (make-hash-table :test 'equal)))
    (dolist (ch (coerce id 'list))
      (incf (gethash ch freqs 0)))
    freqs))

(defun has-count (freqs count)
  "Check if any letter in the frequency table appears exactly 'count' times."
  (let ((values (loop for key being the hash-keys of freqs
                      collect (gethash key freqs))))
    (some (lambda (freq) (= freq count)) values)))

(defun process-id (id)
  "Process a box ID to determine if it contains letters with exactly two or three occurrences."
  (let ((freqs (count-letters id)))
    (values (if (has-count freqs 2) 1 0)
            (if (has-count freqs 3) 1 0))))

(defun compute-checksum (filename)
  "Compute the checksum for the list of box IDs in the given file."
  (let ((two-count 0)
        (three-count 0))
    (with-open-file (stream filename)
      (dolist (line (read-lines stream))
        (multiple-value-bind (has-two has-three) (process-id line)
          (incf two-count has-two)
          (incf three-count has-three))))
    (* two-count three-count)))

(defun read-lines (stream)
  "Read all lines from a stream into a list of strings."
  (let ((lines '()))
    (loop for line = (read-line stream nil 'eof)
          while (not (eql line 'eof))
          do (push line lines))
    (nreverse lines)))

(format t "Checksum: ~d~%" (compute-checksum "input_level_2.txt"))
