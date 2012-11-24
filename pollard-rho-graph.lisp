
(defun sethash (key value table)
  (setf (gethash key table) value))

;; using member and list is dumb, rewrite with hashes
(defun pollard-walk (polynomial modulus &key (initial (random modulus)))
  (let ((observed (make-hash-table))
        (count 0))
    (while (not (gethash (setf initial (mod (polyeval polynomial initial) modulus))
                        observed))
      (incf count)
      (sethash initial count observed))
        (values 
         (hash-table-count observed)
         observed
         initial
         (- count (gethash initial observed)))))


(defun pollard-walk-steps (polynomial modulus steps &key (initial (random
                                                                   modulus)))
    (loop for i upto steps
         collect (setf initial (mod (polyeval polynomial initial) modulus)))))

(defun random-polynomial (modulus &key (degree (random modulus)))
    (loop for i from 1 to degree collect (random modulus)))

(let ((comp (random-composite)))
  (pollard-walk (random-polynomial comp :degree 3) comp))

(defun print-dot-for-walk (polynomial modulus &key (initial (random modulus)))
  (multiple-value-bind (count table last length)
      (pollard-walk polynomial modulus :initial initial)
  (declare (ignore length count))
  (format t  "digraph{~%")
  (loop for k being the hash-keys of table
       for i = 1 then (mod (1+ i) 2)
       do (if (oddp i) (format t "\"~a\"->" k) (format t "\"~a\";~%\"~a\"->" k k)))
  (format t "\"~a\"" (mod (polyeval '(1 0 3) last) modulus))
  (format t ";~%}~%~%")))


(defun print-dot-for-walk (polynomial modulus &key (initial (random modulus)))
  (format t "digraph{~%")
  (let ((observed (make-hash-table))
        last)
    (until (gethash 
            (progn (setf last initial) (setf initial (mod (polyeval polynomial initial)
                                                          modulus))) observed)
      (sethash initial initial observed)
      (format t "\"~a\"->\"~a\";~%" last initial))
    (format t "\"~a\"->\"~a\";~%" last (mod (polyeval polynomial last) modulus))
    (format t "}~%")))

(log (fermat 7) 10)
