;; Define a structure for the nodes in the circle
(defstruct node
  value
  prev
  next)

(defun play-marble-game (num-players last-marble)
  "Simulates the marble game and returns the highest score."
  (let* ((circle (make-node :value 0))
         (scores (make-array num-players :initial-element 0))
         (current-player 0))
    ;; Initialize the circle to point to itself
    (setf (node-prev circle) circle)
    (setf (node-next circle) circle)
    ;; Set the current node
    (let ((current-node circle))
      ;; Loop over marbles
      (loop for marble from 1 to last-marble do
            (if (zerop (mod marble 23))
                ;; Special case when marble % 23 == 0
                (progn
                  ;; Move current node 7 steps backward
                  (loop repeat 7 do
                        (setf current-node (node-prev current-node)))
                  ;; Remove current node and update pointers
                  (let ((removed-node current-node))
                    ;; Add marble and value of removed node to player's score
                    (incf (aref scores current-player) (+ marble (node-value removed-node)))
                    ;; Remove removed-node from circle
                    (let ((prev-node (node-prev removed-node))
                          (next-node (node-next removed-node)))
                      (setf (node-next prev-node) next-node)
                      (setf (node-prev next-node) prev-node)
                      ;; Set current node to next-node
                      (setf current-node next-node))))
                ;; Regular case
                (progn
                  ;; Move current node forward 1 step
                  (setf current-node (node-next current-node))
                  ;; Insert new node after current node
                  (let* ((new-node (make-node :value marble))
                         (next-node (node-next current-node)))
                    ;; Update pointers
                    (setf (node-prev new-node) current-node)
                    (setf (node-next new-node) next-node)
                    (setf (node-next current-node) new-node)
                    (setf (node-prev next-node) new-node)
                    ;; Set current node to new node
                    (setf current-node new-node))))
            ;; Update current player
            (setf current-player (mod (1+ current-player) num-players)))
      ;; Return the maximum score
      (reduce #'max scores))))

(defun extract-numbers (string)
  "Extracts all integer numbers from a given string."
  (let ((numbers '())
        (current-number ""))
    (loop for c across string do
          (if (digit-char-p c)
              (setf current-number (concatenate 'string current-number (string c)))
              (when (> (length current-number) 0)
                (push (parse-integer current-number) numbers)
                (setf current-number ""))))
    (when (> (length current-number) 0)
      (push (parse-integer current-number) numbers))
    (nreverse numbers)))

(defun main ()
  "Main function to read input and calculate the winning score for part two."
  (with-open-file (stream "input_level_9.txt")
    (let ((line (read-line stream nil)))
      (multiple-value-bind (num-players last-marble)
          (let ((numbers (extract-numbers line)))
            (values (first numbers) (second numbers)))
        ;; Part two: Set last-marble to 100 times the original value
        (let ((last-marble (* last-marble 100)))
          (let ((high-score (play-marble-game num-players last-marble)))
            (format t "The new winning Elf's score is: ~A~%" high-score)))))))

(main)
