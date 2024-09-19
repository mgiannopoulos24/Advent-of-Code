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
  (if (null coordinates)
      (error "No coordinates provided.")
      (let ((x-values (mapcar #'car coordinates))
            (y-values (mapcar #'cdr coordinates)))
        (if (or (null x-values) (null y-values))
            (error "Coordinates must have both x and y values.")
            (let ((min-x (reduce #'min x-values))
                  (max-x (reduce #'max x-values))
                  (min-y (reduce #'min y-values))
                  (max-y (reduce #'max y-values)))
              (format t "Grid bounds: min-x=~D, max-x=~D, min-y=~D, max-y=~D~%"
                      min-x max-x min-y max-y)
              (list min-x max-x min-y max-y))))))

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
                        for i from 0 do
                          (let ((dist (manhattan-distance current-point coord)))
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

(defun calculate-safe-region (coordinates bounds threshold)
  "Calculate the size of the safe region where the sum of distances to all coordinates is less than THRESHOLD.
   COORDINATES is a list of (x . y).
   BOUNDS is a list (min-x max-x min-y max-y).
   THRESHOLD is the distance threshold (e.g., 10000)."
  (let* ((min-x (first bounds))
         (max-x (second bounds))
         (min-y (third bounds))
         (max-y (fourth bounds))
         ;; Expand the bounds by a margin to ensure safe region is fully captured
         (margin 100)
         (safe-count 0))
    (format t "Calculating safe region with threshold ~D~%" threshold)
    (loop for x from (- min-x margin) to (+ max-x margin) do
          (loop for y from (- min-y margin) to (+ max-y margin) do
                (let ((current-point (cons x y))
                      (total-distance 0))
                  ;; Calculate the sum of distances to all coordinates
                  (loop for coord in coordinates do
                        (setf total-distance (+ total-distance (manhattan-distance current-point coord))))
                  ;; If the total distance is less than the threshold, count the point
                  (when (< total-distance threshold)
                    (incf safe-count)))))
    safe-count))

(defun main (filepath)
  "Main function to process the input file and compute both parts of the challenge."
  (let* ((coordinates (read-coordinates filepath)))
    (if (null coordinates)
        (error "No coordinates found in the input file.")
        (progn
          (format t "Number of coordinates read: ~D~%" (length coordinates))
          (let ((bounds (get-grid-bounds coordinates))
                (assignments))
            (setq assignments (assign-points-to-coordinates coordinates bounds))
            (let ((area-counts (first assignments))
                  (infinite-coords (second assignments)))
              ;; Part One: Find the largest finite area
              (let ((largest-area (find-largest-finite-area area-counts infinite-coords)))
                (format t "Part One: The size of the largest finite area is: ~D~%" largest-area))
              ;; Part Two: Calculate the size of the safe region
              (let ((threshold 10000)
                    (safe-region-size (calculate-safe-region coordinates bounds 10000)))
                (format t "Part Two: The size of the safe region is: ~D~%" safe-region-size))))))))


(main "input_level_6.txt")
