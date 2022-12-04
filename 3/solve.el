(with-temp-buffer (insert-file-contents "input") (setq input (buffer-string)))

(setq rucksacks (seq-filter (-compose #'not #'string-empty-p) (split-string input "\n")))

(set-difference '(?a ?b) '(?A ?b))

(defun upcase-p (c) (= c (upcase c)))

(defun item-score (c) (if (upcase-p c) (- c 38) (- c 96))) 

(ert-deftest upcase-L () (should (upcase-p ?L)))
(ert-deftest item-scoring () (should (= (item-score ?p) 16)))
(ert-deftest item-scoring-uppercase () (should (= (item-score ?L) 38)))



(defun chars (s) (mapcar #'identity s))

(defun overlap (r)
  (let ((halfway (/ (length r) 2)))
    (let 
        ((left (substring r 0 halfway))
         (right (substring r halfway)))
         (-intersection (chars left) (chars right)))))

(ert-deftest overlap-test () (should (equal (overlap "vJrwpWtwJgWrhcsFMMfFFhFp") '(?p))))

(defun total-score (rucksacks) (reduce #'+ (mapcar #'item-score (reduce #'append (mapcar #'overlap rucksacks)))))

(total-score '(
"vJrwpWtwJgWrhcsFMMfFFhFp"
"jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
"PmmdzqPrVvPwwTWBwg"
"wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
"ttgJtRGJQctTZtZT"
"CrZsJsPPZsGzwwsLwLmpwMDw"
))

;; part I
(total-score rucksacks)

(setq groups (mapcar (lambda (x) (-slice rucksacks x (+ x 3))) (number-sequence 0 (- (length rucksacks) 3) 3)))

(defun badge (group) (reduce #'-intersection (mapcar #'chars group)))

(setq test-group-1 '("vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg"))

(badge test-group-1)

;; part 2
(reduce #'+ (mapcar #'item-score (reduce #'append (mapcar #'badge groups))))
