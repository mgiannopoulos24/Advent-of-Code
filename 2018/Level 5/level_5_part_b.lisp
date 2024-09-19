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
  "Read the polymer from FILEPATH, process it for both Part One and Part Two, and print the results."
  (let ((polymer (read-polymer-from-file filepath)))
    (if polymer
        (progn
        
          (let ((part-one-result (fully-react polymer)))
            (format t "Part One: After fully reacting, the polymer has ~D units.~%" part-one-result))
          
         
          (let* ((unit-types (remove-duplicates 
                              (map 'list #'char-upcase (coerce polymer 'list))))
                 (min-length
                   (apply #'min
                          (mapcar
                            (lambda (unit-type)
                              (let* ((filtered-list (remove-if 
                                                     (lambda (c) 
                                                       (char= (char-upcase c) unit-type)) 
                                                     (coerce polymer 'list)))
                                     (filtered-polymer (coerce filtered-list 'string)))
                                (fully-react filtered-polymer)))
                            unit-types))))
            (format t "Part Two: The shortest possible polymer has ~D units.~%" min-length)))
        (format t "No polymer found in the file.~%"))))

(process-polymer-file "input_level_5.txt")
