(defun read-lines (file-name)
  "Reads all lines from the given file and returns them as a list."
  (with-open-file (stream file-name)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-record (line)
  "Parses a single record line into a timestamp and action."
  (let ((timestamp-str (subseq line 1 17))
        (action (subseq line 19)))
    (values timestamp-str action)))

(defun parse-timestamp (timestamp-str)
  "Parses the timestamp string into a list of integers: (year month day hour minute)."
  (list (parse-integer (subseq timestamp-str 0 4))
        (parse-integer (subseq timestamp-str 5 7))
        (parse-integer (subseq timestamp-str 8 10))
        (parse-integer (subseq timestamp-str 11 13))
        (parse-integer (subseq timestamp-str 14 16))))

(defun compare-timestamps (a b)
  "Compares two timestamp lists. Returns T if A is earlier than B."
  (loop for x in a
        for y in b
        do (cond ((< x y) (return t))
                 ((> x y) (return nil)))
        finally (return nil)))

(defun sort-records (records)
  "Sorts records based on their timestamps."
  (sort records #'(lambda (a b)
                    (compare-timestamps (first a) (first b)))))

(defun extract-guard-id (action)
  "Extracts the guard ID from the action string."
  (let ((start (position #\# action)))
    (when start
      (let ((end (position-if-not #'digit-char-p action :start (1+ start))))
        (parse-integer (subseq action (1+ start) (or end (length action))))))))

(defun find-guard-most-asleep (file-name)
  "Processes the guard records and returns the result."
  (let* ((lines (read-lines file-name))
         (parsed-records (mapcar #'(lambda (line)
                                     (multiple-value-bind (timestamp-str action)
                                         (parse-record line)
                                       (list (parse-timestamp timestamp-str)
                                             (string-trim '(#\Space #\Tab #\Newline #\Return) action))))
                                   lines))
         (sorted-records (sort-records parsed-records))
         (guards-sleep-schedule (make-hash-table))
         (guards-total-sleep (make-hash-table))
         (current-guard nil)
         (sleep-start nil))
    (dolist (record sorted-records)
      (let* ((timestamp (first record))
             (action (second record))
             (minute (nth 4 timestamp)))
        (cond
          ((search "Guard" action)
           (setf current-guard (extract-guard-id action)))
          ((string= action "falls asleep")
           (setf sleep-start minute))
          ((string= action "wakes up")
           (let ((sleep-end minute))
             ;; Update the guard's sleep schedule and total sleep time
             (let ((schedule (gethash current-guard guards-sleep-schedule)))
               (unless schedule
                 (setf schedule (make-array 60 :initial-element 0))
                 (setf (gethash current-guard guards-sleep-schedule) schedule))
               (loop for m from sleep-start below sleep-end do
                     (incf (aref schedule m))))
             (incf (gethash current-guard guards-total-sleep 0)
                   (- sleep-end sleep-start)))))))
    ;; Find the guard with the most total sleep time
    (let ((sleepiest-guard (let ((max-sleep 0)
                                 (guard-id nil))
                             (maphash #'(lambda (guard total-sleep)
                                          (when (> total-sleep max-sleep)
                                            (setf max-sleep total-sleep)
                                            (setf guard-id guard)))
                                      guards-total-sleep)
                             guard-id)))
      ;; Find the minute the sleepiest guard was asleep the most
      (let* ((schedule (gethash sleepiest-guard guards-sleep-schedule))
             (max-minute 0)
             (max-count 0))
        (loop for m from 0 below 60 do
              (when (> (aref schedule m) max-count)
                (setf max-count (aref schedule m))
                (setf max-minute m)))
        ;; Return the guard ID multiplied by the minute they were most asleep
        (* sleepiest-guard max-minute)))))

;; Example usage:
(let ((result (find-guard-most-asleep "input_level_4.txt")))
  (format t "~a~%" result))
