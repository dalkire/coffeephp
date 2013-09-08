(defconst fmatch "^ *\\(\\$*[a-zA-Z0-9_-]+\\) *?= *?\n")

kids =
  brother:
    name: "Max"
    age:  11
  sister:
    name: "Ida"
      age:  9

(defun cfphp-n ()
  (interactive)
  (let ((fst (nth 0 (cfphp-list))))
    (setq fst (replace-regexp-in-string " *= *$" " = array(" fst))
    (message "%s" fst)))

(defun cfphp-list ()
  (interactive)
  (let ((end (nth 5 (posn-at-point)))
        (beg (re-search-backward fmatch)))
    (split-string (buffer-substring-no-properties beg end) "\n")))

;; Get absolute position of current point
(nth 5 (posn-at-point))
(buffer-substring-no-properties beg end)
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

