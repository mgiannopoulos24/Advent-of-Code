(defun manhattan-distance (p1 p2)
  (reduce #'+ (mapcar #'(lambda (a b) (abs (- a b))) p1 p2)))

(defun dfs (point points visited constellation)
  (setf (gethash point visited) t)
  (push point constellation)
  (dolist (other-point points)
    (when (and (not (gethash other-point visited))
               (<= (manhattan-distance point other-point) 3))
      (dfs other-point points visited constellation))))

(defun count-constellations (points)
  (let ((visited (make-hash-table :test 'equal))
        (constellations '()))
    (dolist (point points)
      (unless (gethash point visited)
        (let ((constellation '()))
          (dfs point points visited constellation)
          (push constellation constellations))))
    (length constellations)))

(defun read-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (map 'list #'parse-integer
                       (split-sequence #\, line)))))

(defun split-sequence (delimiter string)
  (let ((start 0)
        (parts '()))
    (loop for i from 0 to (length string)
          do (if (or (eql i (length string))
                     (char= (char string i) delimiter))
                 (progn
                   (push (subseq string start i) parts)
                   (setf start (1+ i)))))
    (nreverse parts)))

(defun safe-parse-integer (str)
  (ignore-errors (parse-integer str)))

(defun read-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (map 'list #'safe-parse-integer
                       (split-sequence #\, line)))))

(let ((points (read-input "input_level_25.txt")))
  (format t "Number of constellations: ~a~%" (count-constellations points)))
