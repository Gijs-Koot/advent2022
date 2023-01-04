
(cl-defstruct instruction direction length)

(cl-defstruct point x y)

(defun parse-instructions (fn)
  (let ((raw (with-temp-buffer
               (insert-file-contents fn)
               (buffer-string))))
    (mapcar #'parse-instruction (split-string raw "\n" t))))

(defun parse-instruction (str)
  (let ((pair (split-string str " ")))
    (make-instruction :direction (nth 0 pair) :length (string-to-number (nth 1 pair)))))

(defun apply-instruction (instruction point)
  (let ((x (point-x point))
        (y (point-y point))
        (d (instruction-length instruction)))
  (pcase (instruction-direction instruction)
    ("U" (make-point :x (+ x d) :y y))
    ("D" (make-point :x (- x d) :y y))
    ("L" (make-point :x x :y (- y d)))
    ("R" (make-point :x x :y (+ y d)))
    )))

(setq origin (make-point :x 0 :y 0))

(apply-instruction (make-instruction :direction "L" :length 4) origin)

(defun single-step (start end)
  (make-point
   :x (+ (point-x start) (signum (- (point-x end) (point-x start))))
   :y (+ (point-y start) (signum (- (point-y end) (point-y start))))
   ))

(defun trace (start end)
  (if (equal start end) (cons end nil)
    (cons start (trace (single-step start end) end))))

(trace (make-point :x 4 :y 4) (make-point :x 2 :y 3))

(setq instructions (parse-instructions "test"))

(defun follow-instructions (instructions point)
  (if instructions
      (let ((next (apply-instruction (car instructions) point)))
        (cons
         (trace point next)
         (follow-instructions (cdr instructions) next))) nil))

(defun full-trace (instructions)
  (apply #'append (follow-instructions instructions origin)))


(defun follow (points)
  (let ((current (car points)))
    (loop for next in (cdr points) collect
          (if (touches-p next current) current (setf current (single-step current next))))))

(defun touches-p (a b)
  (and (within-1 (point-x a) (point-x b)) (within-1 (point-y a) (point-y b))))

(defun within-1 (a b) (> 2 (abs (- a b))))

(defun solve (fn)
  (length (seq-uniq (follow (full-trace (parse-instructions fn))))))

(setq max-specpdl-size 100000)
(setq max-lisp-eval-depth 100000)


(solve "test")
(solve "input")

;; part B

(defun repeat-value (value n)
  (mapcar (lambda (x) value) (number-sequence 0 (1- n))))

(length (seq-uniq
(funcall (apply #'-compose (repeat-value #'follow 9)) (full-trace (parse-instructions "input")))))

