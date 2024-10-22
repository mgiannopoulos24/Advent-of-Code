;; Define a point structure with x, y positions and velocities vx, vy
(defstruct point
  x
  y
  vx
  vy)

(defun point-update (p)
  "Update the position of the point based on its velocity."
  (incf (point-x p) (point-vx p))
  (incf (point-y p) (point-vy p)))

(defun extract-integers (string)
  "Extract all integers from a given string."
  (let ((numbers '())
        (length (length string))
        (i 0))
    (loop while (< i length) do
          (if (or (digit-char-p (char string i))
                  (and (char= (char string i) #\-)
                       (< i (1- length))
                       (digit-char-p (char string (1+ i)))))
              (let ((start i))
                (incf i)
                (loop while (and (< i length)
                                 (digit-char-p (char string i))) do
                      (incf i))
                (let ((substr (subseq string start i)))
                  (push (parse-integer substr) numbers)))
              (incf i)))
    (nreverse numbers)))

(defun parse-line (line)
  "Parse a line from the input file and create a point object."
  (let ((numbers (extract-integers line)))
    (when (= (length numbers) 4)
      (make-point :x (nth 0 numbers)
                  :y (nth 1 numbers)
                  :vx (nth 2 numbers)
                  :vy (nth 3 numbers)))))

(defun parse-input (filename)
  "Parse the input file and return a list of points."
  (let ((points '()))
    (handler-case
        (with-open-file (stream filename)
          (loop for line = (read-line stream nil)
                while line do
                (let ((p (parse-line line)))
                  (when p
                    (push p points)))))
      (error (e)
        (format t "Error: The file '~A' was not found or could not be opened.~%" filename)
        ;; Exit the function early by returning NIL
        (return-from parse-input nil)))
    (nreverse points)))

(defun get-bounds (points)
  "Get the minimum and maximum x and y values from the list of points."
  (let ((min-x (point-x (first points)))
        (max-x (point-x (first points)))
        (min-y (point-y (first points)))
        (max-y (point-y (first points))))
    (dolist (p points)
      (let ((x (point-x p))
            (y (point-y p)))
        (when (< x min-x) (setf min-x x))
        (when (> x max-x) (setf max-x x))
        (when (< y min-y) (setf min-y y))
        (when (> y max-y) (setf max-y y))))
    (values min-x max-x min-y max-y)))

(defun copy-point (p)
  "Create and return a copy of point p."
  (make-point :x (point-x p)
              :y (point-y p)
              :vx (point-vx p)
              :vy (point-vy p)))

(defun find-message-time (points)
  "Find the time when the message appears and return it along with the points."
  (let ((previous-area most-positive-fixnum)
        (time-sec 0)
        (min-area most-positive-fixnum)
        (min-time 0)
        (min-points nil))
    (loop
      ;; Update positions
      (dolist (p points)
        (point-update p))
      (incf time-sec)
      ;; Calculate area
      (multiple-value-bind (min-x max-x min-y max-y)
          (get-bounds points)
        (let ((area (* (- max-x min-x) (- max-y min-y))))
          (if (< area previous-area)
              (progn
                (setf previous-area area)
                ;; Keep track of the minimum area and corresponding time and points
                (when (< area min-area)
                  (setf min-area area)
                  (setf min-time time-sec)
                  (setf min-points (mapcar #'copy-point points))))
              ;; Area started increasing, so the message was at previous step
              (return (values min-time min-points))))))
    ;; If the loop exits without returning, return the minimum found
    (values min-time min-points)))

(defun print-points (points)
  "Print the grid formed by the points at their current positions."
  (multiple-value-bind (min-x max-x min-y max-y)
      (get-bounds points)
    (let ((width (+ 1 (- max-x min-x)))
          (height (+ 1 (- max-y min-y))))
      ;; To prevent printing extremely large grids
      (if (or (> width 100) (> height 100))
          (format t "Grid too large to display.~%")
          (let ((grid (make-array (list height width) :initial-element #\Space)))
            (dolist (p points)
              (let ((x (- (point-x p) min-x))
                    (y (- (point-y p) min-y)))
                (when (and (>= x 0) (< x width) (>= y 0) (< y height))
                  (setf (aref grid y x) #\#))))
            (loop for y from 0 below height do
                  (loop for x from 0 below width do
                        (write-char (aref grid y x)))
                  (terpri)))))))

(defun main (&optional (filename "input_level_10.txt"))
  "Main function to parse the input, find the message time, and print the points."
  (let ((points (parse-input filename)))
    (if (null points)
        (format t "No points to process. Exiting.~%")
        (multiple-value-bind (message-time message-points)
            (find-message-time points)
          (format t "Message appears at ~A seconds.~%" message-time)
          ;; Optionally, print the message if the grid is not too large
          ;; Uncomment the following line if you wish to see the message
          ;; (print-points message-points)
          ))))

;; Run the main function if this script is executed
(main)
