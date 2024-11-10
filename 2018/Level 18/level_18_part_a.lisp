(defun read-input (file-name)
  (with-open-file (stream file-name)
    (loop for line = (read-line stream nil)
          while line
          collect (coerce (string-trim '(#\Space #\Newline) line) 'list))))

(defun count-adjacent (area x y)
  (let ((adjacent (loop for i from (1- x) to (1+ x)
                       append (loop for j from (1- y) to (1+ y)
                                  when (and (or (/= i x) (/= j y))
                                          (>= i 0) (>= j 0)
                                          (< i (length area))
                                          (< j (length (car area))))
                                  collect (list i j)))))
    (values
     (count-if #'(lambda (pos) (char= (nth (cadr pos) (nth (car pos) area)) #\|)) adjacent)
     (count-if #'(lambda (pos) (char= (nth (cadr pos) (nth (car pos) area)) #\#)) adjacent))))

(defun next-minute (area)
  (let ((new-area (mapcar #'copy-list area)))
    (loop for i from 0 below (length area) do
      (loop for j from 0 below (length (car area)) do
        (multiple-value-bind (trees lumberyards) (count-adjacent area i j)
          (let ((current (nth j (nth i area))))
            (setf (nth j (nth i new-area))
                  (cond ((and (char= current #\.) (>= trees 3)) #\|)
                        ((and (char= current #\|) (>= lumberyards 3)) #\#)
                        ((and (char= current #\#) 
                              (or (zerop lumberyards) (zerop trees))) #\.)
                        (t current)))))))
    new-area))

(defun simulate (area minutes)
  (loop repeat minutes
        do (setf area (next-minute area))
        finally (return area)))

(defun resource-value (area)
  (* (loop for row in area sum (count #\| row))
     (loop for row in area sum (count #\# row))))

(defun main ()
  (let* ((area (read-input "input_level_18.txt"))
         (final-area (simulate area 10))
         (value (resource-value final-area)))
    (format t "Total resource value after 10 minutes: ~A~%" value)))

(main)