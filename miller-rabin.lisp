#|
Wikipedia : Miller-Rabin test


When the number n to be tested is small, trying all a < 2(ln n)2 is not necessary, as much smaller sets of potential witnesses are known to suffice. For example, Pomerance, Selfridge and Wagstaff[6] and Jaeschke[7] have verified that

    if n < 1,373,653, it is enough to test a = 2 and 3;
    if n < 9,080,191, it is enough to test a = 31 and 73;
    if n < 4,759,123,141, it is enough to test a = 2, 7, and 61;
    if n < 2,152,302,898,747, it is enough to test a = 2, 3, 5, 7, and 11;
    if n < 3,474,749,660,383, it is enough to test a = 2, 3, 5, 7, 11, and 13;
    if n < 341,550,071,728,321, it is enough to test a = 2, 3, 5, 7, 11, 13, and 17.

|#

(defun m-r-witnesses (n)
  "which witnesses are sufficient for n?"
  (cond 
    ((< n 1373653) (list 2 3))
    ((< n 9080191) (list 31 73))
    ((< n 4759123141) (list 2 7 61))
    ((< n 2152302898747) (list 2 3 5 7 11))
    ((< n 3474749660383) (list 2 3 5 7 11 13))
    ((< n 341550071728321) (list 2 3 5 7 11 13 17))
    (t (subseq *small-primes* 0 (ceiling (* 2 (log (log n))))))))



(defun powers-of-two-in (n)
  "return the power of two, and the odd remainder"
  (do ((n n (/ n 2))
       (s 0 (1+ s)))
      ((oddp n) (values s n))))

#|

Input: n > 3, an odd integer to be tested for primality;
Input: k, a parameter that determines the accuracy of the test
Output: composite if n is composite, otherwise probably prime
write n − 1 as 2^s·d with d odd by factoring powers of 2 from n − 1
LOOP: repeat k times:
   pick a random integer a in the range [2, n − 2]
   x ← a^d mod n
   if x = 1 or x = n − 1 then do next LOOP
   for r = 1 .. s − 1
      x ← x^2 mod n
      if x = 1 then return composite
      if x = n − 1 then do next LOOP
   return composite
return probably prime
|#
    

(defun m-r-check (n)
  "return true when n is probably prime"
  (let ((witnesses (m-r-witnesses n)))
    (multiple-value-bind (s d)
	(powers-of-two-in (1- n))
      ;; don't want even numbers!
      (when (zerop s)
	(unless (= 1 d)
	  (return-from m-r-check nil)))
      (dolist (a witnesses) ;; loop for a in witness list
	(tagbody :start
	   (let ((x (modpow a d n)))
	     (when (< 1 x (1- n))
	       (dotimes (r (1- s))
		 (setf x (modpow x 2 n))
		 (when (= 1 x)
		   (return-from m-r-check nil))
		 (when (= x (1- n))
		   (go :next-loop)))
	       (return-from m-r-check nil)))
	   :next-loop))))
  T)


	
