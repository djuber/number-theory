;; catalogue should be a quick summary of all functions in the file.
;; I think defpackage is probably a better long term solution

(defparameter *number-theory-catalogue*
  '(((sieve5 n) "Returns a list of all primes from 2 to n")
    (*small-primes* "a list of primes less than 10M")
    (*size-of-small-primes* "length of *small-primes*, for bounds checking")
    ((primep n) "true if n is prime, calls factor")
    ((prime-at n) "the nth prime")
    ((prime-index p) "the index of the prime p, or nil")
    ((next-prime p) "the prime after p")
    ((prime-after n) "the smallest prime greater than or equal to n")
    ((prime-before n) "The largest prime not greater than n")
    ((previous-prime p) "The prime before p")
    ((forward-prime-gap p) "the difference between (next-prime p) and p")
    ((reverse-prime-gap p) "the differnece between p and (previous-prime p)")
    ((goldbach n) "find linear combinations of primes adding to n")
    ((bertrand n) "The smallest p between n and 2n")
    ((triplets n :offset1 2 :offset2 6 :upper-bound 1000) "find the first n prime triplets p, p+offset1, p+offset2")
    ((random-prime) "return a random element of *small-primes*")
    ((random-composite factors) "product of factors random primes")
    ((factor n) "a prime factorization of n")
    ((is-square n) "is n a perfect square?")
    ((square n) "multiply n by itself")
    ((fermat-factor n) "yield a fermat factorization of n")
    ((linear-diophantine-equation a b c) "find a solution of ax+by=c")
    ((invertible-p number modulus) "Is number invertible mod modulus?")
    ((modular-inverses number modulus) "the inverse of number mod modulus, or nil")
    ((egcd a b) "extended euclidean algorithm, yields (list (gcd a b) x y), where ax + by = (a,b)")
    ((modinv a m) "inverse of a mod m")
    ((pairwise-relatively-prime list) "true if every number in the list has no common factors with any other")
    ((chinese-remainder residues moduli :print nil) "solve a simultaneous single variable system of congruences")
    ((transform-pair one two :print nil) "given two (residue modulus) pairs, produce an equivalent pair")
    ((pairs list1 list2) "zip lists into dotted pairs.")
    ((unpair list-of-pairs) "undo pairs operation. values list1 list2")
    ((transform-system-if-possible residues moduli :print nil) 
     "solve first two congruences, then pass system to chinese-remainder")
    )
  "all functions defined in this file")


;; Roger Corman's Sieve function from Corman Lisp examples
;; this causes a heap exhaustion for large enough n
(defun sieve5 (n)
  "Returns a list of all primes from 2 to n"
  (declare (fixnum n) (optimize (speed 3) (safety 0)))
  (let* ((a (make-array n :element-type 'bit :initial-element 0))
	 (result (list 2))
	 (root (isqrt n)))
    (declare (fixnum root))
    (do ((i 3 (the fixnum (+ i 2))))
	((>= i n) (nreverse result))
      (declare (fixnum i))
      (progn (when (= (sbit a i) 0)
	       (push i result)
	       (if (< i root)
		   (do* ((inc (+ i i))
			 (j (* i i) (the fixnum (+ j inc))))
			((>= j n))
		     (declare (fixnum j inc))
		     (setf (sbit a j) 1))))))))

;; let's save the easy ones, less than 10M
;; since sieve5 tries to allocate a bit-array, if you have less memory, you should make the exponent 
;; below smaller
(defparameter *small-primes* (sieve5 (expt 10 8)))
;; and let's not count these over and over again
(defparameter *size-of-small-primes* (length *small-primes*))

(defun primep (n)
  (= 1 (length (factor n) )))

;; now we can include an index function
(defun prime-at (n)
  "return the nth prime number"
  (when 
      (and 
       (< n *size-of-small-primes*)
       (integerp n)
       (plusp n))
    (elt *small-primes* (1- n))))

(defun prime-index (p)
  (1+ (position p *small-primes*)))

(defun next-prime (p)
  (let ((list (member p *small-primes*)))
    (when list
      (second list))))

(defun prime-after (n)
  (car (remove-if (lambda (x) (< x n)) *small-primes*)))

(defun prime-before (n)
  (car (last (remove-if (lambda (x) (> x n)) *small-primes*))))


(defun previous-prime (p)
  (let ((idx (prime-index p)))
    (when idx
      (prime-at (1- idx)))))

(defun forward-prime-gap (p)
  (let ((idx (prime-index p)))
    (when idx
      (- (next-prime p) p))))

(defun reverse-prime-gap (p)
  (let ((idx (prime-index p)))
    (when idx
      (- p (previous-prime p)))))


;; goldbach conjecture : every even number at least 4 is the sum of two primes
(defun goldbach (n)
  "find prime combinations yielding n"
  (check-type n integer)
  (when (and (evenp n) (> n 3))
    (let ((primes (sieve5 n))) ;sieve5 yields a list of primes upto n
      (loop for i in primes 
	 when (and (member (- n i) primes) 
		   (<= i (/ n 2))) 
	   collect (list i '+ (- n i))))))

;; bertrand's conjecture has been proven several times
(defun bertrand (n)
  "find the smallest p between n and 2n"
  (let ((primes (remove-if-not (lambda (x) (> x n)) (sieve5 (* 2 n)))))
    (first primes)))


;; find prime triplets of a given form
(defun triplets (n &key (offset1 2) (offset2 6) (upper-bound 1000))
  "find the first n prime triplets p, p+offset1, p+offset2"
  (let ((primes (sieve5 upper-bound)))
    (subseq 
     (loop for p in primes
	when 
	  (and 
	   (member (+ p offset1) primes)
	   (member (+ p offset2) primes))
	collect (list p (+ p offset1) (+ p offset2)))
     0 n)))

;; could have used random-elt from academy?
;; in any case, select some prime
(defun random-prime ()
  "select a prime from our *small-primes* list at random"
  (elt *small-primes* (random *size-of-small-primes*)))

(defun random-composite (&optional (factors 2))
  "produce a composite number with factors prime factors"
  (apply #'* (loop for i from 1 to factors collecting (random-prime))))

(defun factor (n)
  (multiple-value-bind (smalls n)
      (small-factors n)
    (append smalls 
	    (if (> n 1) 
		(factor% n) 
		nil))))

(defun small-factors (n)
  "strip small prime factors from n. 
Returns small divisors and unfactored remainder."
  (let (result (n n))
    (loop for s in *small-primes* do (do () ((not (zerop (mod n s)))) (push s result) (setf n (/ n s))) when (> s n) return nil)
    (values result n)))


;; right now factor% has an identity crisis, whether it is a general purpose trial division,
;; or only a fall-back from factor. I should stream-line this for the latter
(defun factor% (n)
  "return a list of factors of n"
  (let ((count (if (= 1 (mod (last1 *small-primes*) 6)) 1 0))
	(factors nil)
	(limit (+ 1 (isqrt n))))
    (cond
      ((= n 1) nil)
      ((evenp n) (cons 2 (factor% (/ n 2))))
      ((zerop (mod n 3)) (cons 3 (factor% (/ n 3))))
      (t 
       (labels ((inc (test) 
		  (incf count) 
		  (if (evenp count) 
		      (+ test 4) 
		      (+ test 2))))
	 (do
	  ((test (last1 *small-primes*) (inc test)))
	  ((> test limit)(append (list n) factors))
	   (while (zerop (mod n test)) (push test factors) (setf n (/ n test)))))))))


(defun is-square (n)
  "if n is a perfect square, n's integer square root squared is equal to n"
  (= (* (isqrt n) (isqrt n)) n))

(defun square (n)
  "multiply n by itself"
  (* n n))

(defun fermat-factor (n)
  "yield a fermat factorization of n, printing results"
  ;; find an s,t pair such that n = s^2 - t^2, or s^2 - n = t^2 
  ;; then n = (+ s t) * (- s t)
  (loop for i from (1+ (isqrt n)) 
     to (floor (/ n 2)) ; floor excludes the 1 factor for primes
       when (is-square (-   (square i) n))
       do (let ((s i)
		(r (isqrt (- (square i) n))))
	    (format t "~a = ~a - ~a~%" n (square s) (square r))
	    (format t "~a = ~a * ~a~%" n (- s r) (+ s r))
	    (fermat-factor (- s r))
	    (fermat-factor (+ s r)))))

(defun linear-diophantine-equation (a b c)
  "find a general solution for ax + by = c, where a,b,c,x, and y are integers"
  (let ((gcd (gcd a b)))
    (if (not (integerp (/ c gcd)))
	"no integer solutions"
	;; find an x0,y0 pair knowing that one exists
	(let ((x0y0
	       (do* 
		((x0 0 (1+ x0))
		 (y0 (/ (- c (* a x0)) b) (/ (- c (* a x0)) b)))
		((integerp y0) (list x0 y0)))))
	  (format t "x = ~a + ~an ~%y = ~a - ~an~%"
		  (first x0y0)
		  (/ b gcd)
		  (second x0y0)
		  (/ a gcd))
	  ;; return something reusable
	  (list (first x0y0) (/ b gcd) (second x0y0) (- (/ a gcd)))))))


(defun invertible-p (number modulus)
  "Is number invertible mod modulus?"
  (= (gcd number modulus) 1))    
    
;; it's so wrong to call this inverses... there can be only one
(defun modular-inverses (number modulus)
  (when (invertible-p number modulus)
    (modinv number modulus)))
#|
http://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
returns g x y s.t. ax + by = g = gcd(a,b)
Recursive algorithm
def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return (g, x - (b // a) * y, y)
|#
(defun egcd (a b)
  "extended euclidean algorithm, solves ax+by = (a,b), 
returning a list ((gcd a b) x y)"
  (if (zerop a)
      (list b 0 1)
      (let* ((e (egcd (mod b a) a))
	     (g (first e))
	     (y (second e))
	     (x (third e)))
	(list g (- x (* y (floor (/ b a)))) y))))

#|
http://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
Modular inverse
An application of extended GCD algorithm to finding modular inverses:
def modinv(a, m):
    g, x, y = egcd(a, m)
    if g != 1:
        return None  # modular inverse does not exist
    else:
        return x % m
|#
(defun modinv (a m)
  (let* ((e (egcd a m))
	 (g (first e))
	 (x (second e)))
    (when (= 1 g)
      (mod x m))))

(defun modinv% (a m)
  (mod (first (linear-diophantine-equation a m 1)) m))

(defun pairwise-relatively-prime (list)
  "True if every number in the list shares no common divisors with any other"
  (let ((divisibles 
	 (loop for i in list when
	      (loop for j in (rest (member i list)) 
		 when (not (= 1 (gcd i j))) collect j)
	      collect i)))
    (if divisibles
	(values nil divisibles)
	T)))

(defun chinese-remainder (residues moduli &key print)
  "given a pair of lists of the form (a1 a2 a3 ... an) (m1 m2 m3 ... mn)
produce the solution to the simultaneous one variable congruence system and its modulus"
  (if (pairwise-relatively-prime moduli)
    (let* 
	((M (apply #'* moduli))
	 (mis (loop for i in moduli collect (/ M i)))
	 (mi-mod-m 
	  (loop for i in mis 
	     for j in moduli 
	     collect (mod i j)))
	 (inverses 
	  (loop for i in mi-mod-m 
	     for j in moduli 
	     collect (modular-inverses i j)))
	 (solution 
	  (mod 
	   (loop for residue in residues 
	      for mi in mis for inverse in inverses 
	      summing (* residue mi inverse)) 
	   M)))
      (values 
       (list solution 'mod M) 
       (list 'M_i mis) 
       (list 'M_i-mod-m mi-mod-m) 
       (list 'M_i-inverses inverses) 
       (list 'residues residues) 
       (list 'moduli moduli)))
    (transform-system-if-possible residues moduli :print print)))

  
(defun transform-pair (one two &key print)
	    "given two (res.mod) pairs, find a (res.mod) pair equivalent to them mod (lcm mod1 mod2)"
	    (let ((m1 (cdr one))
		  (m2 (cdr two))
		  (r1 (car one))
		  (r2 (car two)))
	      (when (> m2 m1)
		  (rotatef m1 m2)
		  (rotatef r1 r2))
	      (let ((lcm (lcm m1 m2))
		    (diff (- r1 r2))
		    (gcd (gcd m1 m2)))
		(if (zerop (mod diff gcd))
		    ;; stupid linear search!
		    (progn 
		      (setf r1 (mod r1 m1))
		      (setf r2 (mod r2 m2))
		      (let ((r3 (loop for i from r1 to lcm by m1 when (= r2 (mod i m2)) return i)))
			(when print (format t "~amod~a and ~amod~a ==>~a mod~a~%"
					    r1 m1 r2 m2 r3 lcm))
			(cons r3 lcm)))
		    (progn 
		      (format t "~a mod ~a and ~a mod ~a are incompatible.~%"
			      r1 m1 r2 m2)
		      (cons 0 1))))))

(defun pairs (list1 list2)
  "zip list into dotted pairs.
example: (pairs '(1 2 3 4) '(a b c d)) => ((1 . a) (2 . b) (3 . c) (4 . d))"
  (loop for i in list1 
       for j in list2 
       collect (cons i j)))

(defun unpair (list-of-pairs)
  "undo pairs operation. list1 is first value, list2 is second value."
  (loop for i in list-of-pairs
       collect (car i) into list1
       collect (cdr i) into list2
       finally (return (values list1 list2))))

(defun transform-system-if-possible (residues moduli &key print)
  "solve first two congruences, then pass resulting system back to chinese-remainder"
      (let ((p (transform-pair (cons (first residues) (first moduli)) 
			       (cons (second residues) (second moduli)) :print print)))
	(chinese-remainder (cons (car p) (rest (rest residues)))
			   (cons (cdr p) (rest (rest moduli))) :print print)))

(defun last1 (list)
  (first (last list)))

(defun pairup (list1 list2)
  "this was a first attempt to get values together. I wrote collate more generally."
  (collate list1 list2))

(defun collate (&rest lists)
  "zip lists into a list of their elements one at a time each. 
Example (collate (list 'a 'b 'c) (list 1 2 3)) => '(a 1 b 2 c 3)"
  (do ((lists lists (mapcar #'rest lists))
       (result nil))
      ((every #'null lists) result)
    (setf result (append result (mapcar #'first lists)))))

(defun chinese-remainder-print-result (residues moduli)
  "pretty print results from the chinese-remainder calculator
This doesn't capture effects of transform-system-if-possible, which changes the residues, moduli, and system whenever a gcd is not 1. This doesn't happen in the smartest way, since the transformation takes whichever are the first two equations and gives a new equation in their place.
"
  (multiple-value-bind (solution m-i m-i-mod-m m-i-inverses residue-final moduli-final)
      (chinese-remainder residues moduli :print t)
    (if solution
	;; if there  was, report some useful facts
      (let ((s (first solution))
	    (m (last1 solution))
	    (m-i (last1 m-i))
	    (m-i-mod-m (last1 m-i-mod-m))
	    (m-i-inverses (last1 m-i-inverses))
	    (r-f (last1 residue-final))
	    (m-f (last1 moduli-final)))
	(format t "M_i's were ~{~a ~}~%" m-i)
	(format t "M_i's were ~{~amod~a ~}~%" (collate m-i-mod-m m-f))
	(format t "y_i's were ~{~amod~a ~}~%" (collate m-i-inverses m-f))
	(format t "equation was x = [~{(~a)(~a)(~a)~^+~}]mod~a~%" (collate r-f m-i m-i-inverses) m)
	(format t "x = ~a mod ~a~%" s m)
	s) ; finally, answer the solution
      ;; otherwise get the offending modulus and report
      (multiple-value-bind (n d)
	  (pairwise-relatively-prime moduli)
	(format t "~{~a ~} had a divisor in common with another modulus~%and no solution exists~%" d) 
	n))))


#|
function modular_pow(base, exponent, modulus)
    c := 1
    for e_prime = 1 to exponent 
        c := (c * base) mod modulus
    return c
|#
(defun modular-expt (base expt mod)
  "calculate base to the expt mod mod using single multiplications"
  (let ((c 1))
    (loop for i from 1 to expt
	 do (setf c (mod (* base c) mod)))
c))

#| using exponention by squaring
function modular_pow(base, exponent, modulus)
    result := 1
    while exponent > 0
        if (exponent mod 2 == 1):
           result := (result * base) mod modulus
        exponent := exponent >> 1
        base = (base * base) mod modulus
    return result
|#

;; funny I resisted having this macro until now
(defmacro while (test &body body)
  `(do ()
      ((not ,test))
    ,@body))

;; since these seem to run in a pack, let's just write the second one now
(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defun modpow (base expt mod)
  "calculate base to the expt mod mod using squaring"
  (let ((result 1)
	(e expt))
    (while (> e 0)
      (when (= 1 (mod e 2))
	  (setf result (mod (* result base) mod)))
      (setf e (ash e -1))
      (setf base (mod (* base base) mod)))
    result))

;;; section on hensels lemma 
;;; hensel is broken, but lift works like a champ
(defun modpoly (coeffs modulus)
  "find zeroes of polynomial mod modulus"
  (loop for i upto (1- modulus) when (= 0 (mod (polyeval coeffs i) modulus))
       collect i))

(defun polyeval (coeffs x)
  "evaluate polynomial (a list of coefficients in decreasing order) at x
example (polyeval '(1) 0) => 1
        (polyeval '(1 1) 1) => 2
        (polyeval '(1 0 0 0 1) 2) => 17)"
  ;; use horner's method to collect partial results
  (do* ((e 0 (1+ e))
       (s 0 (+ (car coeffs) (* s x)))
       (coeffs coeffs (cdr coeffs)))
      ((null coeffs) s)))

(defun deriveval (coeffs x)
  "evaluate derivative of polynomial at x"
  (polyeval (polyderiv coeffs) x))

;; this seems to work
(defun polyderiv (coeffs)
  "yield a derivative of a polynomial"
  (labels ((rec (c d)
	     (if (null (cdr c))
		 d
		 (rec (cdr c) (append d (list (* (car c) (1- (length c)))))))))
    (rec coeffs nil)))

(defun polyprint (coefficients &optional (stream t))
  "print a traditional representation of the coefficient list"
  (do ((pwr (1- (length coefficients)) (1- pwr))
       (coefficients coefficients (cdr coefficients)))
      ((null coefficients) (format stream "~%"))
    (unless (zerop (car coefficients)) ; suppress zero terms
      (if (> pwr 0) ; no x term in the constant
	  (format stream "~ax^~a" (car coefficients) pwr)
	  (format stream "~a" (car coefficients))))
    (when (and (cdr coefficients) (not (zerop (cadr coefficients)))) 
      (format stream " + "))))

(defun prime-powers (n)
  "take the output of factor and consolidate into a list of p exp pairs
result will be a list (p1 e1 p2 e2 ... pn en)"
  (labels ((collect-pairs (list)
	     (if (null list)
		 nil
		 (let ((p (first list)))
		   (append (list p (count p list)) (collect-pairs (remove p list)))))))
    (collect-pairs (factor n))))

(defun latexify-powers (n)
  (labels ((two-at-a-time (list)
	     (loop for k on list
		  ; cheating the system, I guess a large enough list would break this
		  for i from 1 to most-positive-fixnum
		  when (oddp i) collect (list (car k) (cadr k)))))
    (let ((powers (two-at-a-time (prime-powers n))))
      (format nil "$~a = ~{(~{~a^{~a}~})~}$" n powers))))
    

(defun cross (list1 list2)
  "make pairs of elements from list1 and list2"
  (let ((result nil))
    (dolist (i list1 result)
      (dolist (j list2)
	(push (list i j) result)))))

(defun unique (list)
  "collect unique elements of list"
  ; this is not horribly efficient, but it stems naturally enough
  ; from a definition as the collection of things which aren't in the tail
  (loop for i in list when (not (member i result)) collect i into result
       finally (return result)))

(defun hensel-crosses (answer)
  "make a list of args for chinese-remainder theorem from the answer accumulated in hensel"
    (reduce #'cross (mapcar #'car answer)))

(defun hensel (coeffs modulus)
  "find all solutions of the polynomial mod modulus"
  (polyprint coeffs)
   (let ((pp (prime-powers modulus))
	(answer nil))
     (format t "~a = ~{~a^~a ~^+~}~%" modulus pp)
     (do* 
      ((pp pp (cddr pp)) ;; our remaining list of prime powers
       (p (first pp) (first pp))
       (e (second pp) (second pp)))
      ((null pp)
       (let ((moduli (mapcar #'cadr answer)))
	 (unique 
	  (loop for i in (hensel-crosses answer) 
	     collect 
	       (progn 
		 (format t "~%c-r-t ~a ~a~%" 
			 i moduli) 
		 (chinese-remainder-print-result i moduli))))))
       (push (list (hensel-polynomial coeffs p e) (expt p e))  answer))))

(defun hensel-polynomial (coeffs prime exponent)
  "use hensels lemma to lift a solution up to modulus, 
where modulus is a power of a prime"
  (let ((candidates (modpoly coeffs prime))) ; just search for the first one
    (format t "~a mod ~a~%" candidates prime) 
    (loop for i from 2 upto exponent
       do (setf candidates (lift candidates coeffs prime i)))
    (format t "(~{~a~^ ~})mod~a~%" candidates (expt prime exponent))
    candidates))

(defun lift (candidates coeffs prime expt)
  "attempt to lift known solutions for coeffs congruent to 0 mod prime^{expt-1}
to congruence mod p^expt"
  (let ((candidates (unique (mapcar (lambda (x) (mod x (expt prime (1- expt)))) candidates))))
    (let ((values (mapcar (lambda (cand) (polyeval coeffs cand)) candidates))
	  (derivs (mapcar (lambda (cand) (deriveval coeffs cand)) candidates))
	  (pe (expt prime expt))
	  (pe1 (expt prime (1- expt)))
	  (answer nil))
      (loop for y in values 
	 for yprime in derivs 
	 for r in candidates do
	   (cond 
	     ((not (zerop (mod yprime prime))) ;; case 1, unique solution
	      (format t "~amod~a has a unique solution mod ~a~%" r pe1 pe)
	      (push 
	       (+
		r
		(* 
		 pe1 
		 (mod 
		  (* 
		   -1 
		   (modinv yprime prime) 
		   (/ y pe1))
		  prime)))
	       answer))
	     ((zerop (mod y pe)) ;; case 2a, there are p lifts
	      (format t "~a mod~a has ~a solutions mod ~a~%" r pe1 prime pe)
	      (loop for i from r 
		 to pe
		 by pe1
		 do (push (mod i pe) answer)))
	     (t (format t "~amod~a has no solutions mod ~a~%" r pe1 pe))))
      ;; for case 2b, nothing to do, since there are no solutions to add
      (unique answer))))
				    

; make a list of digits 
(defun digits (n &key (list ()) (base 10))
  "break an integer into a list of its digits, optionally in base other than 10"
  (if (zerop n)
      list
      (digits (floor (/ n base)) :list (cons (mod n base) list) :base base)))

(defun sum-of-digits (n &key (base 10))
  "add all digits of the argument, optionally in base other than 10"
  (reduce #'+ (digits n :base base)))

(defun alternating-sum (list)
	   "add then subtract items in the list alternately"
	   (labels ((add (list direction sum)
		      (if (null list) 
			  (abs sum) ; we don't care about the sign
			  (add (rest list) (- direction) (+ sum (* direction (first list)))))))
	     (add list -1 0)))

(defun alternating-sum-of-digits (n &key (base 10))
  "return the alternating sum of the digits, optionally in base other than 10"
  (alternating-sum (digits n :base base)))

;; this is an illustration, use (modinv a p) instead
(defun fermat-little-theorem-inverse (a p)
  "calculate a inverse mod p for prime p using modular exponentiation"
  (and (primep p)
       (not (zerop (mod a p)))
       (modpow a (- p 2) p)))

(defun ! (n)
  "return n factorial"
  (do* 
   ((i 1 (1+ i))
    (f i (* i f)))
      ((= i n) f)))

(defun factorial (n)
  (! n))

(defun primorial (n)
  "calculate the nth primorial"
  (reduce #'* (subseq *small-primes* 0  n)))

(defun euler-phi (n)
  "number of relatively prime integers to n less than or equal to n"
  ;; using the definition of the phi function, naively
  (loop for i from 1 to n when (= 1 (gcd i n)) count i))

(defun phi (n &key (print nil))
  "use multiplicativeness of the phi function"
  ;; this is faster, don't take gcd's, only factor n
  (let ((phi 1))
  (labels ((expt-minus (base expt)
	     (if (or (null base) (null expt)) 
		 1
		 (- (expt base expt) (expt base (1- expt))))))
    (do* ((factors (prime-powers n) (cddr factors))
	  (prime (first factors) (first factors))
	  (expt (second factors) (second factors)))
	 ((null factors) phi)
      (when print
	(format t "phi : ~a prime : ~a expt ~a~%" phi prime expt))
      (setf phi (* phi (expt-minus prime expt)))))))

(defun reduced-residue-system (n)
  "yield a list of relatively prime integers to n"
  (loop for i from 1 to n when (= 1 (gcd i n)) collect i))


;; simple naive version
(defun divisors (n)
  "all integers in [1,n] which divide n"
  (loop for i from 1 to n when (zerop (mod n i)) collect i))

(defun powers-upto (base expt)
  (loop for i upto expt collect (expt base i)))

(defun cross-multiply (list1 list2 &rest lists)
  (if (null list2)
      list1
      (loop for i in list1 append (mapcar #'(lambda (x) (* x i)) (cross-multiply list2 (car lists) (rest lists))))))

(defun two-at-a-time (list)
  (loop for k on list
     for i from 1
     when (oddp i) collect (list (car k) (cadr k))))
  
(defun map-two-at-a-time (fn list)
  (map 'list #'(lambda (k) (apply fn k)) (two-at-a-time list)))

(defun all-divisors (n)
    (reduce #'cross-multiply 
	    (map-two-at-a-time #'powers-upto (prime-powers n))))

(defun sum-of-divisors2 (n)
  "use all-divisors"
  (reduce #'+ (all-divisors n)))

;; based on our naive version
(defun number-of-divisors (n)
  "number of integers less than or equal to n which divide n"
  (length (divisors n)))

;; naive
(defun sum-of-divisors (n)
  "sum of the divisors of n"
  (apply #'+ (divisors n)))

(defun tau (n)
  "number of divisors of n, using multiplicativity"
  (do ((factors (prime-powers n) (cddr factors))
       (tau 1))
      ((null factors) tau)
    (setf tau (* tau (1+ (second factors))))))

(defun sigma (n)
  "sum of the divisors of n, using multiplicativity"
  (do* ((factors (prime-powers n) (cddr factors))
	(sigma 1))
       ((null factors) sigma)
    (setf sigma (* sigma (/ (1- (expt (first factors) (1+ (second factors)))) (1- (first factors)))))))


(defun mersenne-prime (n)
  (and 
   (= n (1- (Expt 2 (floor (log (1+ n) 2)))))
   (primep n)))

(defun mersenne-primes (limit)
  (loop for i in *small-primes* 
       while (< i (log limit 2))
       when (primep (1- (expt 2 i)))
       collect (1- (expt 2 i))))


(defun perfect-numbers (limit)
  (let ((mersenne (mersenne-primes limit)))
    (let ((twos (loop for m in mersenne collect (/ (1+ m) 2))))
      (loop for m in mersenne for two in twos collect (* two m)))))

(defun perfect (n)
  (when (member n (perfect-numbers n))
      T))

;;;; Encryption/Decryption methods follow

;; Rosen uses a case insensitive letters only base 26 encoding for these 
;; for exponentiation ciphers, he chunks the letters into groups of digits, in
;; a mixed base 26*100, so ZZ encodes to 2525 (+ (* 100 25) 25)
;; rather than (+ (* 25 26) 25)
;; this is mainly to ease manual encoding and is a rather poor choice in general


(defun to-numbers (plaintext)
  "map a string to a list of numbers mod 26"
  ;; strips all punctuation and spaces, leaving only letters
  ;; this is a defect of the base26 system, really want to do this in bytes
  (map 'list 
       (lambda (ch) 
	 (- (char-code ch) 
	    (if (upper-case-p ch) 65 97))) 
       (remove-if-not #'alpha-char-p plaintext)))

(defun to-string (number-list)
  "return an upcased string from the list of numbers"
  (map 'string 
       (lambda (n) (code-char (+ n 65))) 
       number-list))

(defun shift-cipher (n plaintext)
  "shift plaintext by n, -n is the decryption key"
    (to-string
     (mapcar 
      (lambda (c) (mod (+ c n) 26))
      (to-numbers plaintext))))

(defun shift-decipher (n ciphertext)
  "undo shift cipher operation"
  (shift-cipher (mod (- n) 26) ciphertext))

(defun affine-cipher (a b plaintext)
  "c = ap + b, and p = (* (modinv a 26) (- c b))"
  (to-string 
   (mapcar 
    (lambda (c) (mod (+ b (* a c)) 26))
    (to-numbers plaintext))))

(defun affine-decipher (a b ciphertext)
  "reverse the affine-transform"
  (to-string
   (mapcar 
    (lambda (c) (mod (* (modinv a 26) c) 26)) 
    (mapcar 
     (lambda (n) (mod (- n b) 26)) 
     (to-numbers ciphertext)))))


(defun chunk-length (mod base)
  "determine appropriate chunking length for numbers mod mod"
  (do ((count 0 (1+ count))
       (test 25 (+ 25 (* test base))))
      ((> test mod) count)))

(defun pad-with-xs (list number)
  "add X codes to list to make an even number"
  (let ((length (length list)))
    (append list (loop for i from 1 upto (mod length number) collect (- (char-code #\X) 65)))))

(defun chunk-letters (list mod &key (base 100))
  "group adjacent letters into larger numbers all less than mod"
  (let ((number (chunk-length mod base)))
    (let ((list (pad-with-xs list number)))
      (let ((length (length list)))
	(loop 
	   for i upto (- length number)
	   by number 
	   collect 
	     (do ((j 0 (1+ j))
		  (res 0 (+ (elt list (+ j i)) (* res base))))
		 ((= j number) res)))))))
		      
(defun unchunk-letters (list &key (base 100))
  "restore a chunked number list to a number list"
  (let ((result nil)
	(min-width (ceiling (log (loop for i in list maximizing i) base)))) ; make a good guess on length
    (dolist (x list)
      (setf result (append result (ensure-width min-width (digits x :base base)))))
    result))

(defun ensure-width (length list)
  "pad a list with zeros until length is length"
  (if (= length (length list))
      list
      (ensure-width length (cons 0 list))))


;; change into blocks of letters
(defun exponentiation-cipher (expt mod plaintext)
  "raise chunks of plaintext to expt modulo mod"
     (mapcar (lambda (chunk) (modpow chunk expt mod)) 
	     (chunk-letters (to-numbers plaintext) mod)))


(defun exponentiation-decipher (expt mod ciphertext)
  "return to chunked-letter format, then unchunk"
  (if (= 1 (gcd expt (phi mod)))
      (to-string
       (unchunk-letters 
	(mapcar 
	 #'(lambda (n) (modpow n (modinv expt (phi mod)) mod))
	 ciphertext)))
      (format t "exponent and phi(mod) not relatively prime. Not invertible!~%")))

(defun reverse-phi (phi n)
  "given phi(n), provide two factors of n"
  (values 
   (/ (+ n 1 (- phi) (sqrt (- (square (+ n 1 (- phi))) (* 4 n)))) 2)
   (/ (+ n 1 (- phi) (- (sqrt (- (square (+ n 1 (- phi))) (* 4 n)))) 2))))


(defun pollard-rho-full-factorization (n)
    (if (evenp n)
	(cons 2 (pollard-rho-full-factorization (/ n 2)))
	(multiple-value-bind (d r)
	    (pollard-rho n)
	  (if d
	      (cons d (pollard-rho-full-factorization r))
	      (list r)))))
    

;; non-cycle-detecting version
(defun pollard-rho (n &key (debug nil))
  "mote-carlo method for finding non-trivial divisors"
  (if (< 1 n) ; are there going to be non-trivial divisors?
      (do* ((i 1 (1+ i)) ; counter
	   (k 2 (if (= i k) (* 2 k) k)) ; period counter
	    ;; pick a random x, then square it repeatedly
	   (x (random (1- n)) (mod (1- (square x)) n)) 
	    ;; keep a previous value of x every 2^n steps
	   (y x (if (= (1+ i) k) x y))
	    ;; find the gcd of the difference and n
	   (d 1 (gcd (- y x) n)))
	   ;; if we found a divisor, give it and the remaining (unfactored) part
	   ;; if we wasted too much time, give no divisor and n
	   ((or (and (/= 1 d) (/= n d))
		(> i n)) (if (> i n) (values nil n) (values d (/ n d))))
	;; if we want to see the steps, call with (pollard-rho n :debug t)
	(when debug (format t "~a ~a ~a ~a ~a ~a~%"
		i k x y d n)))))

