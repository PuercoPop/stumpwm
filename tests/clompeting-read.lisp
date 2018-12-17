(fiasco:define-test-package #:ido-tests
  (:use #:cl
        #:ido))
(in-package #:ido-tests)

(deftest test-insert-char ()
  (let ((line (make-instance 'input-line)))
    (is (string= "" (input-contents line)))
    (insert-char #\l line)
    (is (string= "l" (input-contents line)))
    (insert-char #\i line)
    (is (string= "li" (input-contents line)))
    (setf (input-point line)
          0)
    (insert-char #\h line)
    (is (string= "hli" (input-contents line)))))
