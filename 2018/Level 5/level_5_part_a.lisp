(defun reacts-p (unit1 unit2)
  "Check if two units react: same type but opposite polarity."
  (and (not (char= unit1 unit2)) 
       (char= (char-upcase unit1) (char-upcase unit2))))

(defun fully-react (polymer)
  "Process the polymer and return the length after full reaction."
  (let ((stack '()))
    (dolist (unit (coerce polymer 'list))
      (if (and stack (reacts-p unit (car stack)))
          (pop stack)        
          (push unit stack))) 
    (length stack)))

(defun read-polymer-from-file (filepath)
  "Read the polymer string from a given file.
   Assumes the polymer is on a single line."
  (with-open-file (stream filepath :direction :input)
    (let ((line (read-line stream nil)))
      (when line
        line)))) 

(defun process-polymer-file (filepath)
  "Read the polymer from FILEPATH, process it, and print the result."
  (let ((polymer (read-polymer-from-file filepath)))
    (if polymer
        (let ((result (fully-react polymer)))
          (format t "After fully reacting, the polymer has ~D units.~%" result))
        (format t "No polymer found in the file.~%"))))


(process-polymer-file "input_level_5.txt")

