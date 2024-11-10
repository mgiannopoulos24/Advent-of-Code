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

(defun area-to-key (area)
  "Convert area to a string for hash table key"
  (format nil "~{~{~A~}~}" area))

(defun simulate-with-cycle-detection (area target-minutes)
  (let ((seen-states (make-hash-table :test #'equal))
        (minute 0))
    (loop
      (let ((area-key (area-to-key area)))
        ;; Check if we've seen this state before
        (let ((previous-minute (gethash area-key seen-states)))
          (when previous-minute
            ;; Cycle detected
            (let* ((cycle-length (- minute previous-minute))
                   (remaining-minutes (mod (- target-minutes previous-minute) 
                                         cycle-length)))
              ;; Simulate remaining minutes after cycle
              (return (loop repeat remaining-minutes
                           do (setf area (next-minute area))
                           finally (return area)))))
        
        ;; Record current state
        (setf (gethash area-key seen-states) minute)
        (setf area (next-minute area))
        (incf minute))))))

(defun resource-value (area)
  (* (loop for row in area sum (count #\| row))
     (loop for row in area sum (count #\# row))))

(defun main ()
  (let* ((area (read-input "input_level_18.txt"))
         (target-minutes 1000000000)
         (final-area (simulate-with-cycle-detection area target-minutes))
         (value (resource-value final-area)))
    (format t "Total resource value after ~A minutes: ~A~%" 
            target-minutes value)))

(main)