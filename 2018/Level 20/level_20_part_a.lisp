;; Read input from file
(defun read-input (filename)
  (with-open-file (stream filename)
    (string-trim '(#\Space #\Newline) (read-line stream))))

;; Directions and their corresponding movements
(defparameter *directions*
  '(("N" . (0 . -1))
    ("S" . (0 . 1))
    ("E" . (1 . 0))
    ("W" . (-1 . 0))))

;; Helper function to add coordinates
(defun add-coords (x y dx dy)
  (cons (+ x dx) (+ y dy)))

;; Helper function to get movement for direction
(defun get-direction-movement (char)
  (cdr (assoc (string char) *directions* :test #'string=)))

;; Main exploration function
(defun explore (regex)
  (let ((visited (make-hash-table :test 'equal))
        (stack nil)
        (x 0) (y 0))
    ;; Set initial position distance to 0
    (setf (gethash (cons 0 0) visited) 0)
    
    (loop for char across regex do
      (cond
        ;; Handle movement directions
        ((find char "NSEW") 
         (let* ((movement (get-direction-movement char))
                (new-pos (add-coords x y (car movement) (cdr movement)))
                (current-dist (gethash (cons x y) visited))
                (new-dist (1+ current-dist)))
           (when (or (not (gethash new-pos visited))
                     (> (gethash new-pos visited) new-dist))
             (setf (gethash new-pos visited) new-dist))
           (setf x (car new-pos)
                 y (cdr new-pos))))
        ;; Handle branching
        ((char= char #\()
         (push (cons x y) stack))
        ((char= char #\|)
         (let ((pos (car stack)))
           (setf x (car pos)
                 y (cdr pos))))
        ((char= char #\))
         (let ((pos (pop stack)))
           (setf x (car pos)
                 y (cdr pos))))))
    visited))

;; Main program
(let* ((regex (read-input "input_level_20.txt"))
       ;; Remove first and last characters (^ and $)
       (cleaned-regex (subseq regex 1 (1- (length regex))))
       (visited-rooms (explore cleaned-regex))
       (furthest-room (loop for dist being the hash-values of visited-rooms
                           maximize dist)))
  (format t "The largest number of doors required to reach a room is: ~A~%" 
          furthest-room))