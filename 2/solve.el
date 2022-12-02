(require 'dash)

(with-temp-buffer (insert-file-contents "input") (setq input (buffer-string)))

(setq matches (seq-filter (-compose #'not #'string-blank-p) (split-string input "\n")))

;; A Rock B Paper C Scissors
;; X Rock Y Paper Z Scissors

(defun adv/score (match) (cond
                          ((equal match "A X") (+ 3 1))
                          ((equal match "A Y") (+ 6 2))
                          ((equal match "A Z") (+ 0 3))
                          ((equal match "B X") (+ 0 1))
                          ((equal match "B Y") (+ 3 2))
                          ((equal match "B Z") (+ 6 3))
                          ((equal match "C X") (+ 6 1))
                          ((equal match "C Y") (+ 0 2))
                          ((equal match "C Z") (+ 3 3))))

(defun adv/scores (matches) (seq-reduce #'+ (mapcar #'adv/score matches) 0))

(ert-deftest simple () (should (= (adv/score "A Y") 8)))
(ert-deftest match () (should (= (adv/scores '("A Y" "B X" "C Z")) 15)))

;; A Rock B Papers C Scissors
;; X lose Y draw Z win

(defun adv/alt-score (match) (cond
                          ((equal match "A X") (+ 0 3))
                          ((equal match "A Y") (+ 3 1))
                          ((equal match "A Z") (+ 6 2))
                          ((equal match "B X") (+ 0 1))
                          ((equal match "B Y") (+ 3 2))
                          ((equal match "B Z") (+ 6 3))
                          ((equal match "C X") (+ 0 2))
                          ((equal match "C Y") (+ 3 3))
                          ((equal match "C Z") (+ 6 1))))

(defun adv/alt-scores (matches) (seq-reduce #'+ (mapcar #'adv/alt-score matches) 0))

(ert-deftest simple-alt () (should (= (adv/alt-score "A Y") 4)))
(ert-deftest alt-match () (should (= (adv/alt-scores '("A Y" "B X" "C Z")) 12)))

(adv/alt-scores matches)

