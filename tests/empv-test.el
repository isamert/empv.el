;;; empv-test.el --- Tests for empv -*- lexical-binding: t; -*-

(require 'ert)
(require 'empv)

(ert-deftest empv-start-starts-ivjs-before-mpv ()
  (let ((empv-invidious-instance 'ivjs)
        (calls nil))
    (cl-letf (((symbol-function 'empv--running?) (lambda () nil))
              ((symbol-function 'empv--ensure-ivjs-started)
               (lambda () (push 'ivjs calls)))
              ((symbol-function 'empv--make-process)
               (lambda (&rest _) (push 'mpv calls)))
              ((symbol-function 'empv--make-network-process) #'ignore)
              ((symbol-function 'empv-observe) #'ignore)
              ((symbol-function 'run-hooks) #'ignore))
      (empv-start)
      (should (equal (nreverse calls) '(ivjs mpv))))))
