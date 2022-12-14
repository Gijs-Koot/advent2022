(defun parse-instructions (fn)
  (split-string (with-temp-buffer (insert-file-contents fn) (buffer-string)) "\n"))

(setq instructions (parse-instructions "test"))

(cl-defstruct state cycles value)

(defun apply-instruction (instruction state)
  (let*
      ((tokens (split-string instruction " "))
       (cycles (state-cycles state))
       (value (state-value state))
       (noop  (make-state :cycles (1+ cycles) :value value)))
    (if (> (length tokens) 1)
        ;; addx
        (list noop (make-state :cycles (+ cycles 2) :value (+ value (string-to-number (nth 1 tokens)))))
        ;; noop
        (list noop)
       )))

(setq start (make-state :cycles 0 :value 1))

(apply-instruction "noop" start)

(defun state-signal-strength (state) (* (1+ (state-cycles state)) (state-value state)))

(defun state-score (state)
  (if (member (state-cycles state) '(19 59 99 139 179 219)) (state-signal-strength state) 0))

(defun collect-states-lst (instructions)
  (let ((state (list start)))
    (loop for instruction in instructions collect
          (setf state (apply-instruction instruction (car (last state)))))
    ))

(defun collect-states (instructions)
  (apply #'append (collect-states-lst instructions)))

(mapcar (lambda (state) (message "%s" state)) (collect-states instructions))

(defun solve-1 (fn)
  (reduce #'+ (mapcar #'state-score (collect-states (parse-instructions fn)))))

(solve-1 "test")

;; part A

(solve-1 "input")

;; part B


(setq states (collect-states instructions))

(defun draw-position (value position)
;;  (message "value %d position %d" value position)
  (if (>= 1 (abs (- (mod position 40) value))) ?# ?.))

(defun output-characters (states)
  (loop for position in
        (number-sequence 0 (- (length states) 1)) collect
        (draw-position (state-value (nth position (cons start states))) position)))

(defun formatted-output (states)
  (let ((characters (output-characters states)))
    (dotimes (row (/ (length characters) 40))
      (message (concat (-slice characters (* row 40) (* (1+ row) 40))))
      )))

(formatted-output (collect-states (parse-instructions "test")))





