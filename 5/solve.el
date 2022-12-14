;; find correct datatype

(defun move (from to stacks)
  (push (pop (nth from stacks)) (nth to stacks)))

(defun read-lines (data)
  (seq-filter (-compose #'not #'string-empty-p) (split-string data "\n")))

(defun number-of-stacks (stackdata)
  (reduce #'max (mapcar (-compose #'length #'split-string) (read-lines stackdata))))

(defun load-lines (stackdata)
  (let (
        (n (number-of-stacks stackdata))
        (lines (reverse (-drop-last 1 (read-lines stackdata))))
        (stacks nil)
        )
  ;; construct the stacks
  (dotimes (i n) (push nil stacks))

  ;; push lines onto stacks
  (dolist (line lines)
    (setq i 1)
    (while (< i (length line))
      (let ((char (aref line i)))
        (if (not (= char 32)) (push char (nth (/ (- i 1) 4) stacks)))
        )
      (setq i (+ i 4)))
  )
  stacks
  ))

(if t (message "hi"))

(defun make-word (stacks)
  (apply #'string (mapcar #'first stacks)))

(cl-defstruct instruction n from to)

(defun parse-instruction (line)
  (save-match-data
    (string-match "move \\([[:digit:]]+\\) from \\([[:digit:]]+\\) to \\([[:digit:]]+\\)" line)
    (make-instruction :n (string-to-number (match-string 1 line)) :from (string-to-number (match-string 2 line)) :to (string-to-number (match-string 3 line)))))

;; test

(cl-defstruct problem stacks instructions)

(defun parse-problem (fn)
  (with-temp-buffer (insert-file-contents fn)
     (let ((parts (split-string (buffer-string) "\n\n")))
       (make-problem
        :stacks (load-lines (nth 0 parts))
        :instructions 
        (mapcar #'parse-instruction (read-lines (nth 1 parts)))
        ))))

(parse-problem "test")


(defun apply-instruction (instruction stacks)
  (dotimes
      (ix (instruction-n instruction))
    (move (1- (instruction-from instruction)) (1- (instruction-to instruction)) stacks)))

(defun solve-problem (problem)
    (dolist (instruction (problem-instructions problem))
      (apply-instruction instruction (problem-stacks problem)))
    (make-word (problem-stacks problem)))

(solve-problem (parse-problem "test"))
(solve-problem (parse-problem "input"))

;; part II


(defun apply-instruction (instruction stacks)
  (dolist (item (reverse (mapcar (lambda (x) (pop (nth
                                 (1- (instruction-from instruction)) stacks)))
                                 (number-sequence 1 (instruction-n instruction)))))
    (push item (nth (1- (instruction-to instruction)) stacks))))

(solve-problem (parse-problem "test"))
(solve-problem (parse-problem "input"))
