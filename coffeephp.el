(defconst initial-regexp "^\\( *\\)\\(\\$*[a-zA-Z0-9_-]+\\) *= *$")
(defconst branch-regexp "^\\( *\\)\\(\\$*[a-zA-Z0-9_-]+\\): *$")
(defconst leaf-regexp "^\\( *\\)\\(\\$*[a-zA-Z0-9_-]+\\): *\\([a-zA-Z0-9_-\"\']+\\)$")

kids =            ;; initial
  brother:        ;; branch
    name: 'Max'   ;; leaf
    age:  11      ;; leaf
  sister:         ;; branch
    name: 'Ida'   ;; leaf
      age:  9     ;; leaf

    kids =
      brother:
        name: 'Max'
        age:  11
      sister:
        name: 'Ida'
        age:  9

(defun cfphp-c ()
  (interactive)
  (let ((str "")
        (depth 0)
        (prev-ws 0)
        (ws 0))
    (loop for line in (cfphp-l) do
      (message "depth: %d" depth)
      (string-match "^ *" line)
      (setq ws (length (match-string 0 line)))
      ;; (message "ws < prev-ws: %d < %d" ws prev-ws)
      (when (< ws prev-ws)
        (setq depth (- depth 1))        
        (setq str (concat str (concat (make-string ws ?\s) "),\n"))))
      (when (> ws prev-ws)
        (setq depth (+ depth 1)))
      (setq str (concat str (concat line "\n")))
      (setq prev-ws ws))
    (while (> depth 1)
      (setq str (concat str "),\n"))
      (setq depth (- depth 1)))
    (insert str)))


(defun cfphp-l ()
  (interactive)
  (let ((repl '()))
    (loop for line in (cfphp-list) do
      (when (equal "initial" (cfphp-line-type line))
        ;; replace with php form
        (setq repl (append repl (cons (replace-regexp-in-string initial-regexp "\\1$\\2 = array(" line) nil))))
      ;; else not initial: find out if branch or leaf
      (when (equal "branch" (cfphp-line-type line))
        (setq repl (append repl (cons (replace-regexp-in-string branch-regexp "\\1\\2 => array(" line) nil))))
      (when (equal "leaf" (cfphp-line-type line))
        (setq repl (append repl (cons (replace-regexp-in-string leaf-regexp "\\1\\2 => \\3" line) nil)))))
    repl))

(defun cfphp-line-type (line)
  "Given a string, return the type of coffeephp line. One of either \"initial\", \"branch\", \"leaf\", or nil"
  (interactive)
  (let ((type))
    (if (string-match initial-regexp line)
      (setq type "initial"))
    (if (string-match branch-regexp line)
      (setq type "branch"))
    (if (string-match leaf-regexp line)
      (setq type "leaf"))
    type))

(cfphp-line-type "  brother:")


(defun cfphp-n ()
  (interactive)
  (let ((fst (nth 0 (cfphp-list))))
    (setq fst (replace-regexp-in-string "\\(^ *[a-zA-Z0-9_-]+\\) *= *$" "$\\1 = array(" fst))
    (message "%s" fst)))

(defun cfphp-list ()
  (interactive)
  (let ((end (nth 5 (posn-at-point)))
        (beg (re-search-backward initial-regexp)))
    (split-string (buffer-substring-no-properties beg end) "\n")))

;; Get absolute position of current point
(nth 5 (posn-at-point))
(buffer-substring-no-properties beg end)

(setq tst '())
(append tst "there")

;; use indentation given
;; keep track of indentation level of region being evaluated
;; so that closing out arrays properly is possible
;; for now, assume no leading '$'

;; $kids = array(
;;   'brother' => array(
;;     'name' => "Max"
;;     'age' => 11
;;   ),
;;   'sister' => array(
;;     'name' => "Ida"
;;     'age' => 9
;;   )
;; );

;; Initial sketch of some rules:
;; From point, look back to assignment:
;; \w = \n
;; if no $, add it
;; --becomes--
;; $\w = array(\n

;; \w : \n
;; if in quotes, leave it
;; if number, leave it
;; if alpha no quotes, add quotes
;; if begins with $, leave it
;; --becomes--
;; '\w' => array(\n

;; \w : \w
;; if in quotes, leave it
;; if number, leave it
;; if alpha no quotes, add quotes
;; if begins with $, leave it
;; --becomes--
;; '\w' => '\w'

;; Does closing parens need to depend on indentation?

