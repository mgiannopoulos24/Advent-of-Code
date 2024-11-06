;; Define the Cart structure
(defstruct cart
  x y direction turns crashed)

(defun create-cart (x y direction)
  (make-cart :x x :y y :direction direction :turns 0 :crashed nil))

(defun move-cart (cart)
  (setf (cart-x cart)
        (case (cart-direction cart)
          (#\> (+ (cart-x cart) 1))
          (#\< (- (cart-x cart) 1))
          (otherwise (cart-x cart))))
  (setf (cart-y cart)
        (case (cart-direction cart)
          (#\^ (- (cart-y cart) 1))
          (#\v (+ (cart-y cart) 1))
          (otherwise (cart-y cart)))))

(defun turn-cart (cart track-piece)
  (let ((direction (cart-direction cart))
        (turns (cart-turns cart)))
    (setf (cart-direction cart)
          (case track-piece
            (#\/ (if (member direction '(#\^ #\v))
                     (if (char= direction #\^) #\> #\<)
                     (if (char= direction #\>) #\^ #\v)))
            (#\\ (if (member direction '(#\^ #\v))
                     (if (char= direction #\^) #\< #\>)
                     (if (char= direction #\>) #\v #\^)))
            (#\+ (case (mod turns 3)
                   (0 (case direction
                        (#\^ #\<)
                        (#\v #\>)
                        (#\< #\v)
                        (#\> #\^)))
                   (2 (case direction
                        (#\^ #\>)
                        (#\v #\<)
                        (#\< #\^)
                        (#\> #\v)))
                   (otherwise direction)))
            (otherwise direction)))
    (when (char= track-piece #\+)
      (setf (cart-turns cart) (+ turns 1)))))

(defun parse-input (filename)
  (let* ((lines (with-open-file (stream filename :direction :input)
                  ;; Collect each line as a vector of characters
                  (loop for line = (read-line stream nil)
                        while line
                        collect (coerce line 'vector))))
         (height (length lines))
         (width (length (elt lines 0)))
         ;; Create a 2D array (height x width) initialized with the contents of lines
         (tracks (make-array (list height width) :initial-contents lines))
         (carts nil))
    ;; Locate carts and replace their positions in tracks with track pieces
    (loop for y from 0 below height do
          (loop for x from 0 below width do
                (let ((char (aref tracks y x)))
                  (when (member char '(#\^ #\v #\< #\>))
                    (push (create-cart x y char) carts)
                    (setf (aref tracks y x) (if (member char '(#\^ #\v)) #\| #\-))))))
    (values tracks (reverse carts))))

(defun find-last-cart (tracks carts)
  (loop while (> (count-if-not #'cart-crashed carts) 1) do
        ;; Sort carts based on their position (top-left to bottom-right)
        (setf carts (sort carts (lambda (a b)
                                  (or (< (cart-y a) (cart-y b))
                                      (and (= (cart-y a) (cart-y b))
                                           (< (cart-x a) (cart-x b)))))))
        ;; Process each cart
        (loop for cart in carts do
              (unless (cart-crashed cart)
                (move-cart cart)
                ;; Check for collisions after the cart has moved
                (dolist (other carts)
                  (when (and (not (eq cart other))
                             (not (cart-crashed other))
                             (= (cart-x cart) (cart-x other))
                             (= (cart-y cart) (cart-y other)))
                    (setf (cart-crashed cart) t)
                    (setf (cart-crashed other) t))))
              ;; Turn the cart based on the track piece if it didn’t crash
              (unless (cart-crashed cart)
                (let ((track-piece (aref tracks (cart-y cart) (cart-x cart))))
                  (turn-cart cart track-piece))))
        ;; After all carts have moved, remove the crashed ones
        (setf carts (remove-if #'cart-crashed carts)))
  ;; Return the last remaining cart's position
  (let ((last-cart (find-if-not #'cart-crashed carts)))
    (values (cart-x last-cart) (cart-y last-cart))))

;; Main execution
(let ((tracks nil)
      (carts nil))
  (multiple-value-setq (tracks carts) (parse-input "input_level_13.txt"))
  (multiple-value-bind (x y) (find-last-cart tracks carts)
    (format t "Last cart's final location is: (~A, ~A)~%" x y)))
