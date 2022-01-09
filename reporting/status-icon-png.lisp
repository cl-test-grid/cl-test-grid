(defpackage #:test-grid-reporting/status-icon-png
  (:nicknames #:tg-reporting/status-icon-png #:tg-rep/status-icon-png)
  ;; dependency for package inferred system
  (:import-from :vecto)) 

(in-package :tg-reporting/status-icon-png)

(defun status-rgb (status-code)
  (ecase status-code
    (:ok '(0 1 0))
    (:fail '(1 0 0))
    (:none '(0.5 0.5 0.5))))

(defun status-png-impl (input x y cell-width cell-height)
  ;; go from top to bottom
  (incf y (* cell-height (length input)))
  (dolist (lisp-results input)
    (setf x 0)    
    (decf y cell-height)
    (dolist (lib-world-result lisp-results)
      (apply #'vecto:set-rgb-fill (status-rgb lib-world-result))
      (vecto:rectangle x y cell-width cell-height)
      (vecto:fill-path)
      (incf x cell-width))))

(defun status-png (input file &key (cell-width 50) (cell-height 1))
  (vecto:with-canvas (:width (* cell-width (length (first input)))
                      :height (* cell-height (length input)))
    (status-png-impl input 0 0 cell-width cell-height)    
    (vecto:save-png file)))

(defun statuses-png (input-by-lib libnames lisp-count lib-world-count file
                     &key (cell-width 50) (cell-height 1))
  (let* ((status-height (* cell-height lisp-count))
         (height (* status-height (length libnames))))
    (vecto:with-canvas (:width (* cell-width lib-world-count)
                        :height height)
      (let ((y height)) ; go from top to bottom
        (dolist (lib libnames)
          (decf y status-height)
          (status-png-impl (gethash lib input-by-lib)
                           0 y cell-width cell-height)))
      (ensure-directories-exist file)
      (vecto:save-png file))))
