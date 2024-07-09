(defun kw-bitmap-font-p (font-spec)
  (when (memq (font-get font-spec :spacing) '(100 M 110 C)) t))

(defun kw-bitmap-font-sizes (font-spec)
  "Return list of available sizes for FONT-SPEC.
If FONT-SPEC is a proportional font, return nil."
  (when (kw-bitmap-font-p font-spec)
    (let ((family (font-get font-spec :family)))
      (delete-dups
       (--map
        (font-get it :size)
        (list-fonts (font-spec :family family)))))))

(defun mt-bitmap-font-sizes-old (&optional font-spec)
  (let* ((fs (or font-spec (mt-current-font-spec)))
	 (bitmap-sizes (kw-bitmap-font-sizes fs)))
    (or bitmap-sizes '(12 14 18 22 24))))

(defun mt-current-font-spec ()
  (let* ((old-font-string (cdr (assoc 'font (frame-parameters))))
	 (spec (font-spec :name old-font-string)))
    spec))

(defun mt-current-font-family ()
  (let* ((spec (mt-current-font-spec)))
    (font-get spec :family)))

(defun mt-current-font-size ()
  (let* ((spec (mt-current-font-spec)))
    (font-get spec :size)))

;; TODO: this needs two versions for truetype and bitmap, like
;; mt-change-font-family
(defun mt-change-font-size (newsize)
  (let* ((current-spec (mt-current-font-spec))
	 (_ (font-put current-spec :size newsize))
	 (new-font-string (font-xlfd-name current-spec)))
    (set-face-font 'default new-font-string)))

(defun mt-change-font-family-truetype (newfamily)
  (let* ((current-spec (mt-current-font-spec))
	 (_ (font-put current-spec :family newfamily))
	 (_ (font-put current-spec :foundry nil))
	 (_ (font-put current-spec :size nil))
	 (new-font-string (font-xlfd-name current-spec)))
    (set-face-font 'default new-font-string)))

(defun mt-change-font-family-bitmap (family)
  (when-let* ((font-strings (x-list-fonts family))
	      (head (car font-strings))
	      (old-spec (font-spec :name head)))
    (font-put old-spec :weight nil)
    (set-face-font 'default (font-xlfd-name old-spec))))

(defun mt-interactive-change-font-size (newsize)
  (interactive
   (list
    (completing-read "Font size: "
		     (mapcar
		      #'number-to-string
		      (mt-bitmap-font-sizes
		       (mt-current-font-spec))))))
  (mt-change-font-size (string-to-number newsize)))

(defun mt-xlfd-to-spec (xlfd)
  (font-spec :name xlfd))

(defun mt-font-is-medium (spec)
  (eq (font-get spec :weight) 'medium))

(defun mt-list-all-fonts (font-string)
  (let* ((xlfd-strings (x-list-fonts font-string))
	 (specs (mapcar #'mt-xlfd-to-spec xlfd-strings))
	 (only-mediums (seq-filter #'mt-font-is-medium specs)))
    only-mediums))

(defun mt-spec-to-size (spec)
  (font-get spec :size))

(defun mt-bitmap-font-sizes (&optional font-string)
  (let* ((current-family (symbol-name (mt-current-font-family)))
	 (search-term (or font-string current-family))
	 (specs (mt-list-all-fonts search-term))
	 (sizes (mapcar #'mt-spec-to-size specs)))
    (when (seq-every-p #'identity sizes)
      (-sort '< (seq-uniq sizes)))))

(defun mt-all-font-families ()
  (let* ((strings (x-list-fonts (font-xlfd-name (font-spec nil :name))))
	 (specs (-map #'mt-xlfd-to-spec strings))
	 (families (--map (symbol-name (font-get it :family)) specs)))
    (-sort 'string< (seq-uniq families))))

(defun mt-change-font-family (font-string)
  (interactive
   (list
    (completing-read "New font: "
		     (mt-all-font-families))))
  (set-frame-font font-string))
