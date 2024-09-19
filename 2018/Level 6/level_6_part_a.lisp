(defun split-string (delimiter string)
  "Split STRING into a list of substrings divided by DELIMITER."
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

(defun parse-coordinate (line)
  "Parse a line into a coordinate (x . y)."
  (let* ((parts (split-string #\, line))
         (x (parse-integer (string-trim '(#\Space) (first parts))))
         (y (parse-integer (string-trim '(#\Space) (second parts)))))
    (cons x y)))

(defun read-coordinates (filepath)
  "Read coordinates from FILEPATH and return a list of (x . y)."
  (with-open-file (stream filepath :direction :input)
    (loop for line = (read-line stream nil)
          while line
          if (not (string= line ""))
          collect (parse-coordinate line))))

(defun manhattan-distance (point1 point2)
  "Calculate the Manhattan distance between POINT1 and POINT2."
  (+ (abs (- (car point1) (car point2)))
     (abs (- (cdr point1) (cdr point2)))))

(defun get-grid-bounds (coordinates)
  "Determine the minimum and maximum x and y values from COORDINATES."
  (let ((min-x (reduce #'min (mapcar #'car coordinates)))
        (max-x (reduce #'max (mapcar #'car coordinates)))
        (min-y (reduce #'min (mapcar #'cdr coordinates)))
        (max-y (reduce #'max (mapcar #'cdr coordinates))))
    (list min-x max-x min-y max-y)))

(defun assign-points-to-coordinates (coordinates bounds)
  "Assign each grid point within BOUNDS to the closest coordinate.
   Return a list containing area counts and infinite coordinates."
  (let* ((min-x (first bounds))
         (max-x (second bounds))
         (min-y (third bounds))
         (max-y (fourth bounds))
         ;; Assign an index to each coordinate for easy referencing
         (coord-table (make-hash-table :test 'equal))
         (infinite-coords (make-hash-table :test 'equal))
         (area-counts (make-hash-table :test 'equal)))
    ;; Initialize area counts
    (loop for i from 0 below (length coordinates) do
          (setf (gethash i area-counts) 0))
    ;; Iterate over each point in the grid
    (loop for x from min-x to max-x do
          (loop for y from min-y to max-y do
                (let ((current-point (cons x y))
                      (min-distance most-positive-fixnum)
                      (closest-coord nil)
                      (tie-p nil))
                  ;; Find the closest coordinate
                  (loop for coord in coordinates
                        for i from 0
                        do (let ((dist (manhattan-distance current-point coord)))
                             (cond
                               ((< dist min-distance)
                                (setf min-distance dist)
                                (setf closest-coord i)
                                (setf tie-p nil))
                               ((= dist min-distance)
                                (setf tie-p t)))))
                  ;; Assign the point if no tie
                  (when (and closest-coord (not tie-p))
                    (incf (gethash closest-coord area-counts))
                    ;; If the point is on the boundary, mark the coordinate as infinite
                    (when (or (= x min-x) (= x max-x)
                              (= y min-y) (= y max-y))
                      (setf (gethash closest-coord infinite-coords) t))))))
    ;; Return area counts and infinite coordinates
    (list area-counts infinite-coords)))

(defun find-largest-finite-area (area-counts infinite-coords)
  "Find the size of the largest finite area."
  (let ((max-area 0))
    (maphash
     (lambda (coord area)
       (unless (gethash coord infinite-coords)
         (when (> area max-area)
           (setf max-area area))))
     area-counts)
    max-area))

(defun main (filepath)
  "Main function to process the input file and compute the largest finite area."
  (let* ((coordinates (read-coordinates filepath))
         (bounds (get-grid-bounds coordinates))
         (assignments (assign-points-to-coordinates coordinates bounds))
         (area-counts (first assignments))
         (infinite-coords (second assignments))
         (largest-area (find-largest-finite-area area-counts infinite-coords)))
    (format t "The size of the largest finite area is: ~D~%" largest-area)))

(main "input_level_6.txt")
