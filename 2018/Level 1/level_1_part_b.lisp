(defun read-frequency-changes (filename)
  "Read frequency changes from the specified file."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil 'eof)
          while (not (eq line 'eof))
          collect (parse-integer line))))

(defun find-first-repeated-frequency (changes)
  "Find the first frequency that is reached twice."
  (let ((current-frequency 0)
        (seen-frequencies (make-hash-table :test 'equal))
        (index 0))
    (setf (gethash current-frequency seen-frequencies) t)
    (loop
      while t ;; Continue indefinitely
      do
        (let ((change (nth index changes)))
          (incf current-frequency change)
          (when (gethash current-frequency seen-frequencies)
            (return current-frequency))
          (setf (gethash current-frequency seen-frequencies) t))
        (setq index (mod (1+ index) (length changes))))))

(defun main (filename)
  "Read frequency changes from the file and print the first frequency reached twice."
  (let* ((changes (read-frequency-changes filename))
         (first-repeated-frequency (find-first-repeated-frequency changes)))
    (format t "The first frequency reached twice is: ~a~%" first-repeated-frequency)))

(main "input_level_1.txt")
