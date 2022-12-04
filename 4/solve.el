(with-temp-buffer (insert-file-contents "input") (setq input (buffer-string)))

(setq pairs (mapcar (lambda (x) (split-string x ",")) (seq-filter (-compose #'not #'string-empty-p) (split-string input))))

(cl-defstruct range start end)

(defun parse (x) "create range from '3-7'"
       (pcase-let ((`(,start ,end) (mapcar #'string-to-number (split-string x "-"))))
         (make-range :start start :end end)))

(defun contains (a b)
  "check if a fully contains b"
  (and (<= (range-start a) (range-start b)) (>= (range-end a) (range-end b))))

(setq ranges (mapcar (lambda (x) (mapcar #'parse x)) pairs))

(defun contains-or-is-contained (pair)
  (pcase-let ((`(,left ,right) pair)) (or (contains left right) (contains right left)))
  )

;; part I
(length (seq-filter #'contains-or-is-contained ranges))

(defun do-overlap (pair)
  (pcase-let ((`(,left ,right) pair))
    (or
     (<= (range-start left) (range-start right) (range-end left))
     (<= (range-start right) (range-start left) (range-end right))
     (<= (range-start left) (range-end right) (range-end left))
     (<= (range-start right) (range-end left) (range-end right))
     (contains-or-is-contained pair)
     )
    ))

;; part II
(length (seq-filter #'do-overlap ranges))

