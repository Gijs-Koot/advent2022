(cl-defstruct monkey id inventory operation divisible target-true target-false)

(setq monkey-regex "Monkey \\([[:digit:]]\\):\n  Starting items: \\([[:digit:], ]*\\)\n  Operation: new = \\(.*\\)\n  Test: divisible by \\([[:digit:]]+\\)\n    If true: throw to monkey \\([[:digit:]]+\\)\n    If false: throw to monkey \\([[:digit:]]+\\)")

(defun convert-to-sexpr (expr)
  (let ((split (split-string expr " " t)))
    (concat "(" (nth 1 split) " " (nth 0 split) " " (nth 2 split) ")")))

(convert-to-sexpr "old * old")

(defun eval-operation (expr old)
  (let ((expr (convert-to-sexpr expr)))
    (eval (car (read-from-string expr)))))

(eval-operation "old + 123" 3)

(defun parse-monkey (monkey-raw)
  (save-match-data
    (string-match monkey-regex monkey-raw)
    (let (
          (id (match-string 1 monkey-raw))
          (inventory (match-string 2 monkey-raw))
          (operation (match-string 3 monkey-raw))
          (divisible (match-string 4 monkey-raw))
          (target-true (match-string 5 monkey-raw))
          (target-false (match-string 6 monkey-raw)))
      (make-monkey
       :id (string-to-number id)
       :inventory (mapcar #'string-to-number (split-string inventory ", " t))
       :operation operation
       :divisible (string-to-number divisible)
       :target-true (string-to-number target-true)
       :target-false (string-to-number target-false)))))

(defun parse-monkeys (fn)
  (mapcar #'parse-monkey
          (split-string (with-temp-buffer (insert-file-contents fn) (buffer-string)) "\n\n" t)))

(setq monkeys (parse-monkeys "test"))



