(defun extract-hundreds-digit (number)
  "Extracts the hundreds digit from a number. Returns 0 if there is no hundreds digit."
  (let ((abs-number (abs number)))
    (if (< abs-number 100)
        0
        (mod (floor abs-number 100) 10))))

(defun power-level (x y serial)
  "Calculates the power level of a fuel cell at (x, y) with the given grid serial number."
  (let* ((rack-id (+ x 10))
         (initial (* rack-id y))
         (after-serial (+ initial serial))
         (multiplied (* after-serial rack-id))
         (hundreds (extract-hundreds-digit multiplied))
         (power (- hundreds 5)))
    power))

(defun create-grid (size serial)
  "Creates a SIZE x SIZE grid of power levels based on the grid serial number."
  ;; Initialize a (size+1) x (size+1) grid to use 1-based indexing
  (let ((grid (make-array (list (1+ size) (1+ size)) :initial-element 0)))
    (dotimes (y size)
      (dotimes (x size)
        (setf (aref grid (1+ x) (1+ y))
              (power-level (1+ x) (1+ y) serial))))
    grid))

(defun find-max-3x3 (grid size)
  "Finds the top-left coordinate of the 3x3 square with the largest total power.
   Returns a list (x y total)."
  (let ((max-sum -1000000)
        (max-x 1)
        (max-y 1))
    (dotimes (y (- size 2))
      (dotimes (x (- size 2))
        (let ((current-sum 0))
          (dotimes (dy 3)
            (dotimes (dx 3)
              (incf current-sum (aref grid (+ x dx 1) (+ y dy 1)))))
          (when (> current-sum max-sum)
            (setf max-sum current-sum
                  max-x (1+ x)
                  max-y (1+ y))))))
    (list max-x max-y max-sum)))

(defun main ()
  "Main function to find the 3x3 square with the largest total power."
  (let* ((serial 9810)
         (size 300)
         (grid (create-grid size serial)))
    (destructuring-bind (x y total) (find-max-3x3 grid size)
      (format t "The top-left coordinate of the 3x3 square with the largest total power is ~d,~d with a total power of ~d.~%"
              x y total))))

;; Execute the main function
(main)
