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

;; Define all opcodes in a list and with named indices for later lookup
(defparameter *opcodes-list* 
  (list #'addr #'addi #'mulr #'muli #'banr #'bani 
        #'borr #'bori #'setr #'seti #'gtir #'gtri 
        #'gtrr #'eqir #'eqri #'eqrr))

(defparameter *opcodes-names*
  (list "addr" "addi" "mulr" "muli" "banr" "bani" 
        "borr" "bori" "setr" "seti" "gtir" "gtri" 
        "gtrr" "eqir" "eqri" "eqrr"))

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

(defun parse-samples-and-program (file-path)
  "Parse the input file and return samples and program instructions."
  (with-open-file (stream file-path)
    (let ((samples nil)
          (program nil)
          (line nil)
          (section 1)  ;; 1 for samples, 2 for program
          (before nil)
          (instruction nil)
          (after nil))
      (loop
        (setf line (read-line stream nil nil))
        (when (null line) 
          (return-from parse-samples-and-program 
                      (values (nreverse samples) (nreverse program))))
        
        ;; Skip blank lines but check for section transition
        (when (string= line "")
          ;; Check if we need to switch to program section
          (setf line (read-line stream nil nil))
          (when (and line (not (string= (subseq line 0 (min 7 (length line))) "Before:")))
            (setf section 2)
            (when (not (string= line ""))
              (push (parse-digits line) program))))
          
        ;; Process based on current section
        (cond
          ;; Samples section
          ((= section 1)
           (when (and line (> (length line) 7))
             (when (string= (subseq line 0 (min 7 (length line))) "Before:")
               (setf before (parse-digits line))
               ;; Read instruction
               (setf instruction (parse-digits (read-line stream nil nil)))
               ;; Read "After" line
               (setf after (parse-digits (read-line stream nil nil)))
               ;; Add sample
               (push (list before instruction after) samples))))
          
          ;; Program section
          ((= section 2)
           (when (and line (not (string= line "")))
             (push (parse-digits line) program))))))))

(defun get-matching-opcodes (before instruction after)
  "Return list of indices of opcodes that match the sample."
  (destructuring-bind (opcode a b c) instruction
    (declare (ignore opcode))
    (let ((matches nil))
      (dotimes (i (length *opcodes-list*))
        (let ((func (nth i *opcodes-list*))
              (registers (copy-list before)))
          (funcall func registers a b c)
          (when (equal registers after)
            (push i matches))))
      (nreverse matches))))

(defun determine-opcode-mapping (samples)
  "Determine which opcode number maps to which function."
  (let ((possible-mappings (make-array 16 :initial-element nil))
        (final-mapping (make-array 16 :initial-element nil)))
    
    ;; Initialize possible mappings for each opcode number
    (dotimes (i 16)
      (setf (aref possible-mappings i) (loop for j from 0 to 15 collect j)))
    
    ;; Update possible mappings based on samples
    (dolist (sample samples)
      (destructuring-bind (before instruction after) sample
        (let ((opcode-num (first instruction))
              (matching-opcodes (get-matching-opcodes before instruction after)))
          ;; Intersect current possibilities with new matching opcodes
          (setf (aref possible-mappings opcode-num)
                (intersection (aref possible-mappings opcode-num) matching-opcodes)))))
    
    ;; Iteratively resolve opcodes with only one possibility
    (loop
      (let ((resolved nil))
        (dotimes (i 16)
          (when (and (= (length (aref possible-mappings i)) 1)
                     (null (aref final-mapping i)))
            (let ((opcode-func-idx (first (aref possible-mappings i))))
              (setf (aref final-mapping i) opcode-func-idx)
              (setf resolved t)
              ;; Remove this opcode function from all other possibilities
              (dotimes (j 16)
                (when (/= i j)
                  (setf (aref possible-mappings j)
                        (remove opcode-func-idx (aref possible-mappings j))))))))
        (unless resolved (return))))
    
    final-mapping))

(defun execute-program (program opcode-mapping)
  "Execute the program with the given opcode mapping."
  (let ((registers (list 0 0 0 0)))
    (dolist (instruction program)
      (destructuring-bind (opcode-num a b c) instruction
        (let ((opcode-func (nth (aref opcode-mapping opcode-num) *opcodes-list*)))
          (funcall opcode-func registers a b c))))
    registers))

(defun solve-part-two (file-path)
  "Solve Part Two of the problem."
  (multiple-value-bind (samples program) (parse-samples-and-program file-path)
    (let ((opcode-mapping (determine-opcode-mapping samples)))
      ;; Print the opcode mapping for debug
      (format t "Opcode mapping:~%")
      (dotimes (i 16)
        (format t "Opcode ~2d -> ~a~%" i (nth (aref opcode-mapping i) *opcodes-names*)))
      
      ;; Execute the program
      (let ((final-registers (execute-program program opcode-mapping)))
        (format t "Final registers: ~a~%" final-registers)
        (first final-registers)))))

;; Run the solution
(format t "Value in register 0 after executing the test program: ~a~%"
        (solve-part-two "input_level_16.txt"))