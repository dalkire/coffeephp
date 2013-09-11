;; find backward from point the 'initial'
;; create full tree
;; write initial w/ closing, step inside
;;

;; breadth-first search
;; car = array(cdr)
        ;; (("brother" ("name" ("Max")) ("age" (11))) ("sister" ("name" ("Ida")) ("age" (9))))

;; dolist cdr 
;; car = array(
;;   car => array(cdr)
;;   car => array(cdr)
;; );

(setq kids '("kids"
               ("brother"
                  ("name" ("Max"))
                  ("age"  (11)))
               ("sister"
                  ("name" ("Ida"))
                  ("age"  (9)))))

(setq brother '("brother"
                  ("name" ("Max"))
                  ("age"  (11))))

(setq name '("name" ("Max")))
(setq tree kids)
(length (cadr kids))    ;; 3
(length (cadr brother)) ;; 2
(length (cadr name))    ;; 1

(length (cdr '()))

(defun mycadr ()
  (interactive)
  (while (not (null tree))
    (if (> (length (cadr tree)) 1)
      (message "%S array{{ %S }}" (car tree) (cdr tree))
      (message "%S" (cadr tree)))
    (setq tree (cadr tree))))

(cadr (cadr (cadr (cadr tree))))

(message "%s = %S" (car tree) (cdr tree))

(dolist (mycar tree)
  (message "%S" mycar))

(car (cdr (cdr (car (cdr tree)))))

(dolist (term tree)
  (message "%S" term))



(setq tr '("kids" ("brother" ("name" ("max"))
                             ("age"  (11)))))

kids brother name max
kids brother age 11
kids sister name ida
kids sister age 9

    kids =
      brother:
        name: 'Max'
        age:  11
      sister:
        name: 'Ida'
        age:  9

    ;; step 1 -- initial with closing
    $kids = array(
      'brother' => array(
        'name' => 'Max'
      )
    );

    ;; step 2 -- 2nd level
    $kids = array(
      'brother' => array(

      ),
      'sister' => array(

      )
    );

    ;; 
    $kids = array(
      'brother' => array(
        'name' => 'Max',
        'age' => 11
      ),
      'sister' => array(
        'name' => 'Ida',
        'age' => 9
      )
    );



(defconst initial-regexp "^\\( *\\)\\(\\$*[a-zA-Z0-9_-]+\\) *= *$")
(defconst branch-regexp "^\\( *\\)\\(\\$*[a-zA-Z0-9_-]+\\): *$")
(defconst leaf-regexp "^\\( *\\)\\(\\$*[a-zA-Z0-9_-]+\\): *\\([a-zA-Z0-9_-\"\']+\\)$")


(defconst kidslist '("    kids =" "      brother:" "        name: 'Max'" "        age:  11" "      sister:" "        name: 'Ida'" "        age:  9"))

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
  "Loops through the list of lines, does replacements based on type of match."
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

(mapcar
 (lambda (l) (message ":\n: %s" l))
 kidslist)


(defun cfphp-indent-level (line)
  (string-match "^ *" line)
  (length (match-string 0 line)))

(defun cfphp-print-indent-level ()
  (interactive)
  (loop for line in (cfphp-list) do
        (message "indent-level: %d" (cfphp-indent-level line))))

(defun cfphp-line-type (line)
  "Given a string, return the type of coffeephp line. One of either \"initial\", \"branch\", \"leaf\", or nil"
  (let ((type))
    (if (string-match initial-regexp line)
      (setq type "initial"))
    (if (string-match branch-regexp line)
      (setq type "branch"))
    (if (string-match leaf-regexp line)
      (setq type "leaf"))
    type))

(defun cfphp-list ()
  "Searches backward from point to fine match for initial regexp, returns list of lines."
  (let ((end (nth 5 (posn-at-point)))
        (beg (re-search-backward initial-regexp)))
    (split-string (buffer-substring-no-properties beg end) "\n")))

(defun cfphp-list-print ()
  (interactive)
  (message "%S" (cfphp-list)))

;; Get absolute position of current point
(nth 5 (posn-at-point))
(buffer-substring-no-properties beg end)

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

