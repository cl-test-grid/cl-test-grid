;;;; Trivial templating utility.
;;;; May be replaced by cl-closure-templates or html-template.

(in-package #:test-grid-reporting)

(defun replace-str (template placeholder value)
  "Differs from CL:REPLACE in that placeholder and value may be of different length."
  (let* ((pos (or (search placeholder template)
                  (error "Can't find the placeholder ~A in the template." placeholder))))
    (concatenate 'string
                 (subseq template 0 pos)
                 value
                 (subseq template (+ pos (length placeholder))))))

(defun fmt-template (file substitutions-alist)
  (let ((template (test-grid-utils::file-string file)))
    (dolist (subst substitutions-alist)
      (setf template (replace-str template (car subst) (cdr subst))))
    template))
