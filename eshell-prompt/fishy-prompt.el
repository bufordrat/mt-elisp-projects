;; MATT's custom eshell prompt
(defun mt-display-fishy-local-prompt (path)
  (require 'cl-lib)
  (cl-flet ((fish-abbreviate (path)
	      (if (= (length path) 0) path (substring path 0 1))))
    (let* ((stripped (directory-file-name path))
	   (expanded (expand-file-name stripped))
	   (abbreviated-path (abbreviate-file-name expanded))
	   (dirs (file-name-split abbreviated-path))
	   (final (list (file-name-nondirectory abbreviated-path)))
	   (shrunken-dirs (mapcar #'fish-abbreviate (butlast dirs)))
	   (fishy-path (string-join (append shrunken-dirs final) "/"))
	   (path-plus-bracket (concat fishy-path " > ")))
      (if (equal path "/")
	  "/ > "
	path-plus-bracket))))

(defun mt-display-fishy-prompt (path)
  (require 'tramp)
  (require 'cl-lib)
  (cl-flet ((mt-tramp-fn-p (filename)
	      (string-match-p tramp-file-name-regexp filename)))
  (let* ((abbreviated-path (abbreviate-file-name path))
	 (fishy-local (mt-display-fishy-local-prompt abbreviated-path)))
    (if (mt-tramp-fn-p abbreviated-path)
	(let* ((path-record (tramp-dissect-file-name abbreviated-path))
	       (host (tramp-file-name-host path-record))
	       (local-path (tramp-file-name-localname path-record))
	       (fishy-remote (mt-display-fishy-local-prompt local-path)))
	  (concat host " in " fishy-remote))
      fishy-local))))

(defconst fishy-tests
  '("~" "~/foo" "~/foo/bar/zap"
    "~/" "~/foo/" "~/foo/bar/zap/"
    "~teichman" "~teichman/foo" "~teichman/foo/zap"
    "~teichman/" "~teichman/foo/" "~teichman/foo/zap/"
    "~utop" "~utop/foo/bar/baz"
    "/home/teichman" "/home/teichman/dude" "/home/teichman/dude/bro"
    "/" "/usr" "~/usr/local" "/usr/local/bin"))
