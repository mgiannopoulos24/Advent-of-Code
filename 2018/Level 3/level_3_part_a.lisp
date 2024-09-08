(defun parse-claim (claim)
  (let* ((parts (split-string claim))
         (left (parse-integer (nth 1 parts)))
         (top (parse-integer (nth 2 parts)))
         (width (parse-integer (nth 3 parts)))
         (height (parse-integer (nth 4 parts)))
         )
    (list left top width height)))

(defun split-string (string)
  (let ((result '())
        (start 0))
    (loop for i from 0 to (length string)
          do (if (or (= i (length string))
                     (member (char string i) '(#\Space #\@ #\: #\x #\,)))
                 (progn
                   (when (/= start i)
                     (push (subseq string start i) result))
                   (setq start (1+ i)))))
    (nreverse result)))

(defun process-claims (filename)
  (let ((fabric (make-hash-table :test 'equal)))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
            while line
            do (let ((claim (parse-claim line)))
                 (when claim
                   (let* ((left (nth 0 claim))
                          (top (nth 1 claim))
                          (width (nth 2 claim))
                          (height (nth 3 claim)))
                     (dotimes (i width)
                       (dotimes (j height)
                         (let ((coordinate (list (+ left i) (+ top j))))
                           (incf (gethash coordinate fabric 0)))))))))

    ;; Count the number of square inches with two or more claims
    (let ((overlap-count 0))
      (maphash (lambda (key value)
                 (when (>= value 2)
                   (incf overlap-count)))
               fabric)
      overlap-count))))


(let ((filename "input_level_3.txt")) 
  (format t "Number of overlapping square inches: ~d~%" (process-claims filename)))
