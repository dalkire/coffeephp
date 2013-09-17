(defconst root-regexp "^\\( *\\)\\(\\$*[a-zA-Z0-9_-]+\\) *= *$")
(defconst branch-regexp "^\\( *\\)\\(\\$*[a-zA-Z0-9_-]+\\): *$")
(defconst leaf-regexp "^\\( *\\)\\(\\$*[a-zA-Z0-9_-]+\\): *\\([a-zA-Z0-9_-\\\"\\\'\s]+\\)$")
 
(defun cfphp-array ()
  (interactive)
  (let ((str "")
        (ws-stack '()))
    (dolist (ln (cfphp-list))
      (setq ws (cfphp-ws ln))
      (when (not (cfphp-leafp ln))
        (while (and (car ws-stack) (<= (length ws) (length (car ws-stack))))
          (setq str (format "%s%s),\n" str (pop ws-stack))))
        (push ws ws-stack))
      (setq str (format "%s%s\n" str (cfphp-replace ln))))
    (dolist (n (butlast ws-stack))
      (setq str (format "%s%s)\n" str n)))
    (setq str (format "%s%s);" str (car (last ws-stack))))
    (insert str)))

(defun cfphp-replace (ln)
  (when (cfphp-rootp ln)
    (message "root")
    (setq ln (replace-regexp-in-string root-regexp "\\1$\\2 = array(" ln)))
  (when (cfphp-branchp ln)
    (message "branch")
    (setq ln (replace-regexp-in-string branch-regexp "\\1'\\2' => array(" ln)))
  (when (cfphp-leafp ln)
    (message "leaf")
    (setq ln (replace-regexp-in-string leaf-regexp "\\1'\\2' => \\3" ln)))
  ln)

(defun cfphp-rootp (ln)
  (equal "root" (cfphp-line-type ln)))

(defun cfphp-branchp (ln)
  (equal "branch" (cfphp-line-type ln)))

(defun cfphp-leafp (ln)
  (equal "leaf" (cfphp-line-type ln)))

(defun cfphp-ws (line)
  (string-match "^ *" line)
  (substring line (match-beginning 0) (match-end 0)))

(defun cfphp-line-type (line)
  "Given a string, return the type of coffeephp line. One of either \"root\", \"branch\", \"leaf\", or nil"
  (let ((type))
    (when (string-match root-regexp line)
      (setq type "root"))
    (when (string-match branch-regexp line)
      (setq type "branch"))
    (when (string-match leaf-regexp line)
      (setq type "leaf"))
    type))

(defun cfphp-list ()
  "Searches backward from point to fine match for root regexp, returns list of lines."
  (let ((str "")
        (end (nth 5 (posn-at-point)))
        (beg (re-search-backward root-regexp)))
    (setq str (split-string (buffer-substring-no-properties beg end) "\n"))
    (delete-region beg end)
    str))

(provide 'coffeephp)
