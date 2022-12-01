(defun read-input ()
  (with-temp-buffer (insert-file-contents "input") (buffer-string)))

(setq input (read-input))

(setq numbers (mapcar (lambda (x) (mapcar #'string-to-number (split-string x "\n")))(split-string input "\n\n")))

(defun sum (lst) (apply #'+ lst))

;; solution to 1
(apply #'max (mapcar #'sum numbers))

(setq totals (mapcar #'sum numbers))

;; solution to 2
(seq-reduce #'+ (subseq (seq-sort #'> totals) 0 3) 0)
