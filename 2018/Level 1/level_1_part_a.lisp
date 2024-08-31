(defun read-frequency-changes (filename)
  "Read frequency changes from the specified file."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil 'eof)
          while (not (eq line 'eof))
          collect (parse-integer line))))

(defun calculate-final-frequency (changes)
  "Calculate the final frequency after applying all the changes."
  (reduce #'+ changes :initial-value 0))

(defun main (filename)
  "Read frequency changes from the file and print the resulting frequency."
  (let* ((changes (read-frequency-changes filename))
         (final-frequency (calculate-final-frequency changes)))
    (format t "The resulting frequency is: ~a~%" final-frequency)))

(main "input_level_1.txt")
