(cl-defstruct file size name)

(cl-defstruct dir subdirs files name parent)


(defun read-file (fn)
  (with-temp-buffer
    (insert-file-contents fn)
    (split-string (buffer-string) "\n" t)))

(setq test-lines (read-file "test"))

(defun root-dir (dir)
  (let ((parent (dir-parent dir))) (if parent (root-dir parent) dir)))

(defun parse (lines)
  (let ((current nil))
    (dolist (line lines)
      (pcase (split-string line " ")
        (`("$" "cd" "..") (setf current (dir-parent current)))
        (`("$" "cd" ,dir) (let ((new (make-dir :subdirs nil :files nil :name dir :parent current)))
                            (if current (push new (dir-subdirs current))) (setf current new)))
        (`("$" "ls") (message "LISTING"))
        (`("dir" ,name) (message "SUBLIST %s" name))
        (`(,size ,name) (push (make-file :size (string-to-number size) :name name) (dir-files current)))
        (_ (message "WHAT"))
        ))
    (root-dir current)))


(defun map-files (func dir)
  (append (flatten-tree (mapcar (lambda (subdir) (map-files func subdir)) (dir-subdirs dir)))
          (mapcar (lambda (file) (funcall func file)) (dir-files dir))))

(defun map-subdirs (func dir)
  (append (list (funcall func dir))
          (flatten-tree (mapcar (lambda (subdir) (map-subdirs func subdir)) (dir-subdirs dir)))))

(defun dir-size (dir)
  (reduce #'+ (map-files #'file-size dir)))

(setq test-dir (parse test-lines))

(defun solve (fn)
  (apply #'+ (seq-filter (lambda (x) (< x 100000)) (map-subdirs #'dir-size (parse (read-file fn))))))

(solve "test")

;; part I

(solve "input")

;; part II

(setq root (parse (read-file "input")))
(setq current-space (- 70000000 (dir-size root)))

(dir-size root)

(apply #'min (seq-filter (lambda (size) (> (+ current-space size) 30000000)) (map-subdirs #'dir-size root)))

