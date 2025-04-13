(defun addr (reg a b c)
  (setf (nth c reg) (+ (nth a reg) (nth b reg)))
  reg)

(defun addi (reg a b c)
  (setf (nth c reg) (+ (nth a reg) b))
  reg)

(defun mulr (reg a b c)
  (setf (nth c reg) (* (nth a reg) (nth b reg)))
  reg)

(defun muli (reg a b c)
  (setf (nth c reg) (* (nth a reg) b))
  reg)

(defun banr (reg a b c)
  (setf (nth c reg) (logand (nth a reg) (nth b reg)))
  reg)

(defun bani (reg a b c)
  (setf (nth c reg) (logand (nth a reg) b))
  reg)

(defun borr (reg a b c)
  (setf (nth c reg) (logior (nth a reg) (nth b reg)))
  reg)

(defun bori (reg a b c)
  (setf (nth c reg) (logior (nth a reg) b))
  reg)

(defun setr (reg a b c)
  (setf (nth c reg) (nth a reg))
  reg)

(defun seti (reg a b c)
  (setf (nth c reg) a)
  reg)

(defun gtir (reg a b c)
  (setf (nth c reg) (if (> a (nth b reg)) 1 0))
  reg)

(defun gtri (reg a b c)
  (setf (nth c reg) (if (> (nth a reg) b) 1 0))
  reg)

(defun gtrr (reg a b c)
  (setf (nth c reg) (if (> (nth a reg) (nth b reg)) 1 0))
  reg)

(defun eqir (reg a b c)
  (setf (nth c reg) (if (= a (nth b reg)) 1 0))
  reg)

(defun eqri (reg a b c)
  (setf (nth c reg) (if (= (nth a reg) b) 1 0))
  reg)

(defun eqrr (reg a b c)
  (setf (nth c reg) (if (= (nth a reg) (nth b reg)) 1 0))
  reg)

;; Define all opcodes in a list for easy iteration
(defparameter *opcodes* 
  (list #'addr #'addi #'mulr #'muli #'banr #'bani 
        #'borr #'bori #'setr #'seti #'gtir #'gtri 
        #'gtrr #'eqir #'eqri #'eqrr))

(defun parse-digits (str)
  "Extract digits from a string and return them as a list of integers"
  (let ((result nil)
        (current-number "")
        (in-number nil))
    (loop for char across str do
      (if (digit-char-p char)
          (progn
            (setf in-number t)
            (setf current-number (concatenate 'string current-number (string char))))
          (when in-number
            (push (parse-integer current-number) result)
            (setf current-number "")
            (setf in-number nil))))
    ;; Check if we ended with a number
    (when (not (string= current-number ""))
      (push (parse-integer current-number) result))
    (nreverse result)))

(defun parse-samples (file-path)
  "Parse the input file and return a list of samples."
  (with-open-file (stream file-path)
    (let ((samples nil)
          (line nil)
          (line-count 0)
          (before nil)
          (instruction nil)
          (after nil))
      (loop
        (setf line (read-line stream nil nil))
        (when (null line) (return-from parse-samples (nreverse samples)))
        
        (when (> (length line) 7)  ;; Skip blank lines
          ;; Check if line starts with "Before"
          (when (string= (subseq line 0 (min 7 (length line))) "Before:")
            (setf before (parse-digits line))
            ;; Read instruction
            (setf instruction (parse-digits (read-line stream nil nil)))
            ;; Read "After" line
            (setf after (parse-digits (read-line stream nil nil)))
            ;; Skip blank line if present
            (read-line stream nil nil)
            ;; Add sample
            (push (list before instruction after) samples)))))))

(defun count-matching-opcodes (before instruction after)
  "Count how many opcodes match the sample."
  (destructuring-bind (opcode a b c) instruction
    (declare (ignore opcode))
    (let ((matches 0))
      (dolist (func *opcodes* matches)
        (let ((registers (copy-list before)))
          (funcall func registers a b c)
          (when (equal registers after)
            (incf matches)))))))

(defun solve (file-path)
  "Solve the problem."
  (let ((samples (parse-samples file-path))
        (count 0))
    (dolist (sample samples count)
      (destructuring-bind (before instruction after) sample
        (when (>= (count-matching-opcodes before instruction after) 3)
          (incf count))))))

;; Run the solution
(defun run-solution (file-path)
  (format t "Number of samples behaving like three or more opcodes: ~a~%"
          (solve file-path)))

;; Usage:
(run-solution "input_level_16.txt")