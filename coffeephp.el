;; find backward from point the 'root'
;; create full tree
;; write root w/ closing, step inside
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



    kids =
      brother:
        name: 'Max'
        age:  11
      sister:
        name: 'Ida'
        age:  9

    ;; step 1 -- root with closing
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



(defconst root-regexp "^\\( *\\)\\(\\$*[a-zA-Z0-9_-]+\\) *= *$")
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

  (dolist (ln kidslist)
    (message "--> %s" (cfphp-replace ln)))


;; This is right, but could use some cleanup and usage flexibility
(let ((str "\n\n\n")
      (ws-stack '()))
  (dolist (ln kidslist)
    (setq ws (cfphp-ws ln))
    (when (not (cfphp-leafp ln))
      ;; while < car level, close parens and pop
      (while (and (car ws-stack) (<= (length ws) (length (car ws-stack))))
        (setq str (format "%s%s),\n" str (pop ws-stack))))
      (push ws ws-stack))
    (setq str (format "%s%s\n" str (cfphp-replace ln))))
  (dolist (n (butlast ws-stack))
    (setq str (format "%s%s)\n" str n)))
  (setq str (format "%s%s);" str (car (last ws-stack))))
  (insert str))



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

(defun cfphp-c ()
  (interactive)
  (let ((str "")
        (ws-stack '())
        (depth 0)
        (prev-ws 0)
        (ws 0))
    (loop for line in (cfphp-l) do
      (message "%s" line)
      (message "%s" (equal "leaf" (cfphp-line-type line)))
      (string-match "^ *" line)
      (setq ws (length (match-string 0 line)))
      (when (< ws prev-ws)
        (pop ws-stack)
        (setq depth (- depth 1))        
        (setq str (concat str (concat (make-string ws ?\s) "),\n"))))
      (when (> ws prev-ws)
        (when (not (cfphp-leafp line))
          (push ws ws-stack))
        (setq depth (+ depth 1)))
      (setq str (concat str (concat line "\n")))
      (setq prev-ws ws))
    (while (setq ws (pop ws-stack))
      (setq str (concat str (concat (make-string ws ?\s) "),\n")))
      (setq depth (- depth 1)))
    (insert str)))

(defun cfphp-l ()
  "Loops through the list of lines, does replacements based on type of match."
  (let ((repl '()))
    (loop for line in (cfphp-list) do
      (when (equal "root" (cfphp-line-type line))
        ;; replace with php form
        (setq repl (append repl (cons (replace-regexp-in-string root-regexp "\\1$\\2 = array(" line) nil))))
      ;; else not root: find out if branch or leaf
      (when (equal "branch" (cfphp-line-type line))
        (setq repl (append repl (cons (replace-regexp-in-string branch-regexp "\\1\\2 => array(" line) nil))))
      (when (equal "leaf" (cfphp-line-type line))
        (setq repl (append repl (cons (replace-regexp-in-string leaf-regexp "\\1\\2 => \\3" line) nil)))))
    repl))

(mapcar
 (lambda (l) (message ":\n: %s" l))
 kidslist)

(cons 0 (append '(1 2 3) (list (nth 1 kidslist))))
(append '(b c d) '(a))

(let ((n 0)
      (ws-stack '()))
  (loop for line in kidslist do
    (setq curr-indent (cfphp-indent-level line))
    ;; (message "Curr-indent: %d\nCurr-stack: %S" curr-indent ws-stack)
    (when (cfphp-leafp line)
      (message "leafT: %s" line)
      (message "leafF: %s" line))        
    (if (= n 0)
      (push curr-indent ws-stack)
      (when (> curr-indent (car ws-stack))
        (push curr-indent ws-stack)
        (message "indent me: %S" ws-stack))
      (when (< curr-indent (car ws-stack))
        (setq popped (pop ws-stack)))
        (message "outdent me(popped-%d): %S" popped ws-stack)))
    (incf n)))

(let ((n 0)
      (prev-ln))
  (loop for ln in kidslist do
    (when prev-ln
      ;; (when (indent> ln prev-ln)
        ;; (message "more indent"))
      (when (indent< ln prev-ln)
        (message ")")))
      ;; (when (indent= ln prev-ln)
        ;; (message "equal indent")))
    (message ln)
    (incf n)
    (setq prev-ln ln)))

(defun indent< (a b)
  "Returns t if indent of a < indent of b, nil otherwise"
  (< (cfphp-indent-level a) (cfphp-indent-level b)))

(defun indent> (a b)
  "Returns t if indent of a > indent of b, nil otherwise"
  (> (cfphp-indent-level a) (cfphp-indent-level b)))

(defun indent= (a b)
  "Returns t if indent of a = indent of b, nil otherwise"
  (= (cfphp-indent-level a) (cfphp-indent-level b)))


(defun cfphp-rootp (ln)
  (equal "root" (cfphp-line-type ln)))

(defun cfphp-branchp (ln)
  (equal "branch" (cfphp-line-type ln)))

(defun cfphp-leafp (ln)
  (equal "leaf" (cfphp-line-type ln)))

(defun cfphp-ws (line)
  (string-match "^ *" line)
  (substring line (match-beginning 0) (match-end 0)))

(defun cfphp-indent-level (line)
  (string-match "^ *" line)
  (length (match-string 0 line)))

(defun cfphp-print-indent-level ()
  (interactive)
  (loop for line in (cfphp-list) do
        (message "indent-level: %d" (cfphp-indent-level line))))

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
  (let ((end (nth 5 (posn-at-point)))
        (beg (re-search-backward root-regexp)))
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

