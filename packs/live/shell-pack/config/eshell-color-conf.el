(defun eshell-ls-decorated-name (file)
  "Return FILE, possibly decorated."
  (if eshell-ls-use-colors
      (let ((face
	     (cond
	      ((not (cdr file))
	       'eshell-ls-missing)

	      ((stringp (cadr file))
	       'eshell-ls-symlink)

	      ((eq (cadr file) t)
	       'eshell-ls-directory)

	      ((not (eshell-ls-filetype-p (cdr file) ?-))
	       'eshell-ls-special)

	      ((and (/= (user-uid) 0) ; root can execute anything
		    (eshell-ls-applicable (cdr file) 3
					  'file-executable-p (car file)))
	       'eshell-ls-executable)

	      ((not (eshell-ls-applicable (cdr file) 1
					  'file-readable-p (car file)))
	       'eshell-ls-unreadable)

	      ((string-match eshell-ls-archive-regexp (car file))
	       'eshell-ls-archive)

	      ((string-match eshell-ls-backup-regexp (car file))
	       'eshell-ls-backup)

	      ((string-match eshell-ls-product-regexp (car file))
	       'eshell-ls-product)

	      ((string-match eshell-ls-clutter-regexp (car file))
	       'eshell-ls-clutter)

	      ((not (eshell-ls-applicable (cdr file) 2
					  'file-writable-p (car file)))
	       'eshell-ls-readonly)
	      (eshell-ls-highlight-alist
	       (let ((tests eshell-ls-highlight-alist)
		     value)
		 (while tests
		   (if (funcall (caar tests) (car file) (cdr file))
		       (setq value (cdar tests) tests nil)
		     (setq tests (cdr tests))))
		 value)))))
	(if face
	    (add-text-properties 0 (length (car file))
				 (list 'face face)
				 (car file))
          ;; normal face
          (add-text-properties 0 (length (car file))
                               (list 'face 'eshell-ls-normal)
                               (car file)))))
  (car file))
