(require 'dash)

(defun kw-bitmap-font-p (font-spec)
  (when (memq (font-get font-spec :spacing) '(100 M 110 C)) t))

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

(defun mt-xlfd-to-spec (xlfd)
  (font-spec :name xlfd))

(defun mt-suitable-font-size-p (spec)
  (and (eq (font-get spec :width) 'normal)
	   (eq (font-get spec :weight) 'regular)))

(defun mt-list-all-fonts (font-string)
  (let* ((xlfd-strings (x-list-fonts font-string))
	 (specs (mapcar #'mt-xlfd-to-spec xlfd-strings))
	 (only-mediums (seq-filter #'mt-suitable-font-size-p specs)))
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

(defvar default-sizes '(8 9 10 11 12 14 18 20 22 24 30 36 48))

(defvar default-size-strings
  (mapcar #'number-to-string default-sizes))

(defun mt-current-font-sizes ()
  (let* ((family (mt-current-font-family))
	 (family-string (symbol-name family))
	 (sizes (mt-bitmap-font-sizes family-string))
	 (size-strings (mapcar #'number-to-string sizes)))
    (or size-strings default-size-strings)))

(defun mt-change-font-family (font-string)
  (interactive
   (list
    (completing-read "New font: "
		     (mt-all-font-families))))
  (set-face-font 'default font-string))

(defun mt-change-font-size-truetype (font-string)
  (set-face-font 'default font-string))

(defun mt-change-font-size-bitmap (newsize)
  (let* ((current-spec (mt-current-font-spec))
	 (_ (font-put current-spec :spacing nil))
	 (_ (font-put current-spec :avgwidth nil))
	 (_ (font-put current-spec :size newsize))
	 (new-font-string (font-xlfd-name current-spec)))
    (set-face-font 'default new-font-string)))

(defun mt-change-font-size (font-string)
  (interactive
   (list
    (completing-read "New size: "
		     (mt-current-font-sizes))))
  (let* ((current-spec (mt-current-font-spec))
	 (is-bitmap (kw-bitmap-font-p current-spec))
	 (size-int (string-to-number font-string)))
    (if is-bitmap
	(mt-change-font-size-bitmap size-int)
      (mt-change-font-size-truetype font-string))))

