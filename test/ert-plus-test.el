;;; -*- lexical-binding: t; -*-

(defun with-foo ()
  (let ((foo t)) cb))

(defun with-bar ()
  (let ((bar t)) cb))

(defun with-baz ()
  (let ((baz t)) cb))

(defun with-bam ()
  (let ((bam t)) cb))


(describe "ert-plus around" (with-foo)
  (it "should have access to own around"
      (should foo)
      (should-error bar)
      (should-error baz))

  (describe "with-bar" (with-bar)
    (it "should have access to own and parent arounds"
        (should foo)
        (should bar)
        (should-error baz))

    (describe "with-baz and with-bam" (with-baz with-bam)
      (it "should have access to all own arounds"
          (should foo)
          (should bar)
          (should baz)
          (should bam)))))


(defun around-without-cb ())
(defun around-with-incorrect-cb () (cb))

(describe "ert-plus validation" ()
  (it "should catch around without cb"
      (let* ((form '(describe "" (around-without-cb) (it "")))
             (error (should-error (macroexpand form))))
        (should (string-match-p "Around must contain bare symbol cb (around-without-cb)"
                                (nth 1 error)))))

  (it "should catch around with (cb) rather than cb"
      (let* ((form '(describe "" (around-with-incorrect-cb) (it "")))
             (error (should-error (macroexpand form))))
        (should (string-match-p "Around must contain bare symbol cb (around-with-incorrect-cb)"
                                (nth 1 error))))))


(describe "ert-plus fixture" ()
  (it "foobar" "top-level-it")

  (describe "nested" ()
    (it "foobar" "nested-it")))


(describe "ert-plus find test" ()
  (it "should find the definition of ertp tests"
      (save-window-excursion
        (cl-letf* ((symbol (intern "ert-plus fixture :: foobar"))
                   ((symbol-function 'ert-test-at-point) (lambda () symbol)))
          (ert-results-find-test-at-point-other-window)
          (should (string-match "top-level-it" (thing-at-point 'line)))))

      (save-window-excursion
        (cl-letf* ((symbol (intern "ert-plus fixture :: nested :: foobar"))
                   ((symbol-function 'ert-test-at-point) (lambda () symbol)))
          (ert-results-find-test-at-point-other-window)
          (should (string-match "nested-it" (thing-at-point 'line)))))))
