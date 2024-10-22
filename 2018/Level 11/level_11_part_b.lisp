(defun calculate-power-level (x y serial-number)
  "Calculate the power level of a fuel cell at (x, y) with a given serial number."
  (let* ((rack-id (+ x 10))
         (power-level (* rack-id y)))
    (setf power-level (+ power-level serial-number))
    (setf power-level (* power-level rack-id))
    (setf power-level (mod (floor power-level 100) 10)) ; Get the hundreds digit
    (- power-level 5)))

(defun build-summed-area-table (grid grid-size)
  "Build a summed-area table for fast square sum calculation."
  (let ((summed-area (make-array (list grid-size grid-size) :initial-element 0)))
    (dotimes (x grid-size)
      (dotimes (y grid-size)
        (setf (aref summed-area x y) (aref grid x y))
        (when (> x 0)
          (incf (aref summed-area x y) (aref summed-area (- x 1) y)))
        (when (> y 0)
          (incf (aref summed-area x y) (aref summed-area x (- y 1))))
        (when (and (> x 0) (> y 0))
          (decf (aref summed-area x y) (aref summed-area (- x 1) (- y 1))))))
    summed-area))

(defun get-square-sum (summed-area x y size)
  "Calculate the sum of a square using the summed-area table."
  (let ((total (aref summed-area (+ x size -1) (+ y size -1))))
    (when (> x 0)
      (decf total (aref summed-area (- x 1) (+ y size -1))))
    (when (> y 0)
      (decf total (aref summed-area (+ x size -1) (- y 1))))
    (when (and (> x 0) (> y 0))
      (incf total (aref summed-area (- x 1) (- y 1))))
    total))

(defun find-largest-power-square-any-size (serial-number)
  "Find the square of any size with the largest total power using a summed-area table."
  (let* ((grid-size 300)
         (grid (make-array (list grid-size grid-size) :initial-element 0))
         (summed-area nil)
         (max-power most-negative-fixnum)
         (top-left-coord '(0 0))
         (best-size 0))
    
    ;; Populate the grid with power levels
    (dotimes (x grid-size)
      (dotimes (y grid-size)
        (setf (aref grid x y) (calculate-power-level (1+ x) (1+ y) serial-number))))

    ;; Build the summed-area table
    (setf summed-area (build-summed-area-table grid grid-size))

    ;; Check every square of every possible size
    (dotimes (size grid-size)
      (let ((current-size (1+ size)))
        (dotimes (x (- grid-size size))
          (dotimes (y (- grid-size size))
            (let ((total-power (get-square-sum summed-area x y current-size)))
              (when (> total-power max-power)
                (setf max-power total-power)
                (setf top-left-coord (list (1+ x) (1+ y)))
                (setf best-size current-size)))))
        (format t "Finished checks for square size ~d. Max power so far: ~d at ~d,~d with size ~d~%" current-size max-power (first top-left-coord) (second top-left-coord) best-size)))

    (list top-left-coord best-size)))

(defun main ()
  "Main function to execute the solution with the serial number 9810."
  (let* ((serial-number 9810)
         (result (find-largest-power-square-any-size serial-number)))
    (format t "The top-left coordinate of the largest square is: ~d,~d,~d~%"
            (first (first result)) (second (first result)) (second result))))

;; Execute the main function
(main)
