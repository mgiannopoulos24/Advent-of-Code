(defun find-common-letters (id1 id2)
  "Find common letters between two IDs that differ by exactly one character."
  (let ((common '()))
    (dotimes (i (length id1))
      (when (char= (elt id1 i) (elt id2 i))
        (push (elt id1 i) common)))
    (coerce (nreverse common) 'string)))

(defun find-correct-boxes (ids)
  "Find the two box IDs that differ by exactly one character."
  (dolist (id1 ids)
    (dolist (id2 ids)
      (unless (equal id1 id2)
        (let ((common (find-common-letters id1 id2)))
          (when (= (length common) (- (length id1) 1))
            (return-from find-correct-boxes common)))))))

(defun read-lines (stream)
  "Read all lines from a stream into a list of strings."
  (let ((lines '()))
    (loop for line = (read-line stream nil 'eof)
          while (not (eql line 'eof))
          do (push line lines))
    (nreverse lines)))

(defun find-common-letters-in-file (filename)
  "Find common letters between the two correct box IDs in the given file."
  (with-open-file (stream filename)
    (let ((ids (read-lines stream)))
      (find-correct-boxes ids))))

(format t "Common letters: ~a~%" (find-common-letters-in-file "input_level_2.txt"))
