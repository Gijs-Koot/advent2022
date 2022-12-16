(defun count-until-marker (stream b-size)
  (let ((counter 1))
    (while (not (last-distinct-p stream counter b-size)) (incf counter))
    (1- counter)))

(defun last-distinct-p (stream index b-size)
  (let ((start (- index (1+ b-size))))
  (if (and (>= (length stream) (1- index)) (<= 0 start))
    (= b-size (length (-distinct (split-string (substring stream start (1- index)) "" t))))
    nil
    )))

;; test
(count-until-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4)

;; part I
(with-temp-buffer (insert-file-contents "input") (count-until-marker (buffer-string) 4))

;; part II
(with-temp-buffer (insert-file-contents "input") (count-until-marker (buffer-string) 14)) 
