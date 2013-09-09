(defconst initial-regexp "^ *\\(\\$*[a-zA-Z0-9_-]+\\) *?= *$")

kids =            ;; initial
  brother:        ;; branch
    name: "Max"   ;; leaf
    age:  11      ;; leaf
  sister:         ;; branch
    name: "Ida"   ;; leaf
      age:  9     ;; leaf

kids =
  brother:
    name: "Max"
    age:  11
  sister:
    name: "Ida"
      age:  9


(defun cfphp-l ()
  (interactive)
  (let ((depth 0) (n 0) (repl '()) (whitespace 0))
    (loop for line in (cfphp-list) do
      (string-match "^\\([ |\\t]*\\)" line)
      (setq whitespace (- (match-end 1) (match-beginning 1)))
      (message "--%d--" whitespace)
      (if (equal n 0)
        (setq repl (append repl (replace-regexp-in-string initial-regexp "$\\1 = array(" line)))
        ;; (if (string-match 
      )
      (setq n (+ n 1)))
    (message "%S" repl)))

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

