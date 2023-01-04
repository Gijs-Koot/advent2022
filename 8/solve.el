(defun read-he pfights (fn)
  (let ((input (with-temp-buffer (insert-file-contents fn) (buffer-string))))
  (mapcar (lambda (x) (mapcar #'string-to-number (split-string x "" t))) (split-string input "\n"))))


(read-heights "test")

(defun get-col (index mat)
  (mapcar (lambda (x) (nth index x)) mat))

(get-col 2 (read-heights "test"))

(defun visible-p (row index)
  (or
   (every (lambda (x) (> (nth index row) x)) (subseq row (1+ index)))
   (every (lambda (x) (> (nth index row) x)) (subseq row 0 index))))

(ert-deftest visible-simple ()
  (should (eql t (visible-p '(1 2 3 2 1) 2))))

(ert-deftest visible-start ()
  (should (eql t (visible-p '(1 2 3 4 3) 1))))

(ert-deftest visible-end ()
  (should (eql t (visible-p '(1 2 3 4 1) 4))))

(ert-deftest invisible ()
  (should (eql nil (visible-p '(3 2 1 4) 2))))

(defun visible-indices (row)
  (seq-filter (lambda (index) (visible-p row index)) (number-sequence 0 (1- (length row)))))

(ert-deftest seeing ()
  (should (equal (visible-indices '(1 2 1 3)) '(0 1 3))))


(defun visible-indices-mat (mat)
  (let* (
        (cols (number-sequence 0 (1- (length (nth 0 mat)))))
        (rows (number-sequence 0 (1- (length mat))))
        (ncols (length cols))
        (nrows (length (nth 0 mat))))
    (apply #'append (append
     ;; rows
     (mapcar (lambda (row) (mapcar (lambda (index) (+ (* row ncols) index)) (visible-indices (nth row mat)))) rows)
     ;; columns
     (mapcar
      (lambda (column)
        (mapcar (lambda (index) (+ (* index nrows) column)) (visible-indices (get-col column mat))))
     cols)
  ))))

(visible-indices (nth 11 mat))

(prin1 mat)

(setq mat (read-heights "input"))

(length (delete-duplicates (visible-indices-mat mat)))

(defun solve (fn)
  (length (delete-dups (visible-indices-mat (read-heights fn)))))

(solve "test")

;;

(solve "input")

;; PART II

(defun view-up (row column mat)
  (reverse (subseq (get-col column mat) 0 (1+ row))))

(defun view-down (row column mat)
  (subseq (get-col column mat) row))

(defun view-left (row column mat)
  (reverse (subseq (nth row mat) 0 (1+ column))))

(defun view-right (row column mat)
  (subseq (nth row mat) column))

(ert-deftest look-up () (should (equal '(3 1 7) (view-up 3 3 mat))))

(ert-deftest look-down () (should (equal '(5 5 3 5) (view-down 0 1 mat))))

(defun all-views (row column mat)
  (list
   (view-up row column mat)
   (view-left row column mat)
   (view-right row column mat)
   (view-down row column mat)
   ))

(defun viewing-distance (view)
  (if (> (length view) 1)
      (let (
            (counter 1)
            (val (nth 0 view))
            (max (1- (length view))))
        (while (and (< counter max) (> val (nth counter view))) (incf counter))
        counter)
    0))

(defun scenic-score (row column mat)
  (reduce #'* (mapcar #'viewing-distance (all-views row column mat))))

(defun map-positions (func mat)
  (loop for row in (number-sequence 0 (1- (length mat))) nconc
    (loop for column in (number-sequence 0 (1- (length (nth 0 mat))))
          collect (funcall func row column mat))))

;; solve PART II
(apply #'max (map-positions #'scenic-score (read-heights "input")))

