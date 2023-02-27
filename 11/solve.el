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

(defun reverse-inventory (monkey)
  (setf (monkey-inventory monkey) (reverse (monkey-inventory monkey)))
  monkey
  )

(defun parse-monkeys (fn)
  (mapcar #'reverse-inventory (mapcar #'parse-monkey
          (split-string (with-temp-buffer (insert-file-contents fn) (buffer-string)) "\n\n" t))))


(defun apply-monkey-op (monkey level)
  (/ (eval-operation (monkey-operation monkey) level) 3))

(ert-deftest apply-test () (should (= (apply-monkey-op (first monkeys) 79) 500)))

(cl-defstruct inspection-result id level)

(defun divisible-p (num div) (= (* (/ num div) div) num))

(defun monkey-inspect (monkey level) "return monkey id and worry level"
       (let ((new (apply-monkey-op monkey level)))
         (if (divisible-p new (monkey-divisible monkey))
             (make-inspection-result :id (monkey-target-true monkey) :level new)
           (make-inspection-result :id (monkey-target-false monkey) :level new))))

(ert-deftest first-inspection () (should (equal (monkey-inspect (first monkeys) 79) (make-inspection-result :id 3 :level 500))))

(ert-deftest div-yes () (should (divisible-p 500 2)))
(ert-deftest div-no () (should (not (divisible-p 500 3))))

(defun tosses (monkey)
  (reverse (mapcar (lambda (level) (monkey-inspect monkey level)) (monkey-inventory monkey))))

(defun toss-round (monkeys)
  (dolist (monkey monkeys)
    (dolist (toss (tosses monkey))
      (push (inspection-result-level toss) (monkey-inventory (nth (inspection-result-id toss) monkeys)))
    ;; empty inventory
    (setf (monkey-inventory monkey) nil)
    )))

;; part A


(defun monkey-count-inc (ix)
  (push (cons ix (1+ (alist-get ix monkey-counts 0))) monkey-counts))

(defun count-inspect (monkey level)
  (monkey-count-inc (monkey-id monkey)))

(advice-add 'monkey-inspect :after #'count-inspect)

(defun assoc-values (monkey-counts)
  (mapcar #'cdr (mapcar (lambda (ix) (assoc ix monkey-counts))(delete-dups (mapcar #'car monkey-counts)))))

(defun multiply-max-two (assoc-list)
  (apply #'* (seq-subseq (seq-sort #'> (assoc-values assoc-list)) 0 2)))

(defun solve-problem (fn times)
  (setq monkey-counts nil)
  (setq monkeys (parse-monkeys fn))
  (dotimes (i times) (toss-round monkeys))
  (multiply-max-two monkey-counts))

;; part B

;; instead of dividing by three, we do operations mod product of monkey tests
;; this keeps the numbers manageable, and keeps the old calculations working


(defun apply-monkey-op (monkey level)
  (mod (eval-operation (monkey-operation monkey) level) monkey-prod))

(defun solve-problem (fn times)
  (setq monkey-counts nil)
  (setq monkeys (parse-monkeys fn))
  (setq monkey-prod (apply #'* (mapcar #'monkey-divisible monkeys)))
  (dotimes (i times) (toss-round monkeys))
  (multiply-max-two monkey-counts))

(solve-problem "input" 10000)
