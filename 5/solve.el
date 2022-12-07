;; find correct datatype
(setq stacks '((?Z ?C ?D) (?A ?B ?C)))

(defun move (from to stacks)
  (push (pop (nth from stacks)) (nth to stacks)))

(move 1 0 1 stacks)

;; parse stackdata
(setq stackdata "
[A] [B]
[C] [D] [E] [Z]
 1   2   3   4
")

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
      (push (aref line i) (nth (/ (- i 1) 4) stacks))
      (setq i (+ i 4)))
  )
  stacks
  ))

(defun make-word (stacks)
  (apply #'string (mapcar #'first stacks)))

;; parse movedata

(setq movedata "
move 3 from 12 to 11
move 1 from 7 to 10
")

(read-lines movedata)

(setq line (first (read-lines movedata)))

(save-match-data
  (string-match "move \\([[:digit:]]+\\) from \\([[:digit:]]+\\) to \\([[:digit:]]+\\)" line)
  (setq n (match-string 1 line) from (match-string 2 line) to (match-string 3 line)))

n



(setq line (nth 1 (read-lines stack-data)))

(setq stacks nil)
(dotimes (i 3) (push nil stacks))

stacks







(push ?A (nth 2 stacks))

stacks

(pop x)

x
