

;;; ############################################################################

(defun lfedoc-sexp-autocompletion-at-point ()
  "Auto complete sexp at point."
  (let ((se (sexp-at-point)))
    ;; at the moment we print the result, in future we will pass it to future
    ;; completion UI
    (pp
     (lfedoc-sexp-autocompletion se))))

;;; autocompletion for various sexp forms
(defun lfedoc-sexp-autocompletion (sexp-str)
  "Read SEXP-STR and get autocompletion."
  (let ((rs (lfedoc-read-sexp sexp-str)))
    (let  ((split-symbol (when (car rs)
                           (split-string (symbol-name (car rs)) ":"))))
        (cond ((equal rs nil)               ; ()
               (lfedoc-find-symbol-autocompletions nil))
              ((equal rs '(:))              ; (:)
               (lfedoc-data-loaded-modules))
              ((and (equal (car rs)         ; (: m) or (: mod)
                           :)
                    (equal (length rs)
                           2))
               (lfedoc-module-or-module-functions-autocompletions (cadr rs)))
              ((and (equal (car rs)         ; (: mod f)
                           :)
                    (equal (length rs)
                           3))
               (lfedoc-module-functions-2 (symbol-name (cadr rs))
                                          (symbol-name (caddr rs))))
              ((and (equal (length split-symbol)
                           2)               ; (mod:) or (mod:f)
                    (equal (length rs)
                           1))
               (lfedoc-module-functions-2 (car split-symbol)
                                          (cadr split-symbol)))
              ((equal (length rs)           ; (a) any
                      1)
               (lfedoc-find-symbol-autocompletions (car rs)))))))

(defun lfedoc-find-symbol-autocompletions (symb)
  "Find symbol SYMB in known symbols and return the function names that return it."
  ;; example (lfedoc-find-symbol-functions  (quote car))
  ;; when symb is nil return everything
  (-filter (lambda (x) (not (null (cl-second x))))
           (-map (lambda (f)
                   (list f
                         (-filter (lambda (sf)
                                    (if symb
                                        (lfedoc-string/starts-with
                                         (symbol-name sf)
                                         (symbol-name symb))
                                      t))
                                  (funcall f))))
                 (lfedoc-get-symbol-functions))))

(defun lfedoc-module-or-module-functions-autocompletions (s)
  "Get auto-completions for modules starting with symbol S,
or functions of module S."
  (let ((found-modules (lfedoc-modules-2 s)))
    (if (-any (lambda (x)
                (equal x s))
              found-modules)
        (list (list 'modules
                    found-modules)
              (list 'module-functions
                    (lfedoc-module-functions-2 (symbol-name s) "")))
      (list (list
             'modules found-modules)))))

;;; -------------- higher ---------------------

(defun lfedoc-modules ()
  "Get list of loaded modules that start with given character(s)."
  (interactive)
  (let ((call-struct (lfedoc-sexp (sexp-at-point))))
    (pp (if (nth 0 call-struct)
            (-filter (lambda (x)
                       (lfedoc-string/starts-with
                         x
                        (format "%s" (nth 0 call-struct))))
                     (lfedoc-query-loaded-modules))
          (lfedoc-query-loaded-modules)))))

(defun lfedoc-modules-2 (symb)
  "Get modules that start with SYMB."
  ;; all modules if symb is nil
  (if symb
      (-map 'intern
            (-filter (lambda (x)
                       (lfedoc-string/starts-with x (symbol-name symb)))
                     (lfedoc-query-loaded-modules)))
    (lfedoc-query-loaded-modules)))

(defun lfedoc-functions ()
  "Get list of known user guide functions that start with given character(s)."
  (interactive)
  ;; we get the character from the call struct
  (let ((call-struct (lfedoc-sexp (sexp-at-point))))
    (when (nth 1 call-struct)
      (princ
       (-distinct
        (-sort 'string<
               (-flatten
                (-map 'cdr
                      (lfedoc-find-symbol-autocompletions
                       (nth 1 call-struct))))))))))

(defun lfedoc-module-functions ()
  "Get a list of module exported functions that start with given character(s)
or all functions if no function characters are given."
  (interactive)
  (let ((call-struct (lfedoc-sexp (sexp-at-point))))
    (if (nth 0 call-struct)
        (pp (-filter
             (lambda (x)
               (if (nth 1 call-struct)
                   ;; if any character of the function given show possible completions
                   ;; otherwise show all available functions
                   (lfedoc-string/starts-with
                    x
                    (format "%s"  (nth 1 call-struct)))
                 t))
             (lfedoc-query-module-functions (nth 0 call-struct)))))))

(defun lfedoc-autocomplete-function ()
  "Autocomplete the function divided into user guide sections."
  (interactive)
  (let ((call-struct (lfedoc-sexp (sexp-at-point))))
    (if (nth 1 call-struct)
        (princ (list 'autocompleting 'function (nth 1 call-struct)
                     call-struct
                     (lfedoc-find-symbol-autocompletions (nth 1 call-struct)))))))

;;; ------------ backend ----------------

(defun lfedoc-query-module-functions (module)
  "Get Exports information about loaded MODULE."
  (let ((exports-seen))
    (-sort 'string<
           (-flatten
            (-map 'lfedoc-split-string-on-spaces
                  (cdr
                   (-reject 'null ; reject everything before the "Exports: " line
                            (-map
                             (lambda (x)
                               (when (equal "Exports: " x)
                                 (setq exports-seen t))
                               (when exports-seen
                                 x))
                             (lfedoc-execute-on-backend
                              (format "lfe -e \"(m (quote %s))\"" module)
                              'lfedoc-string-to-lines)))))))))


(defun lfedoc-read-sexp (str)
  "Convert STR to sexp."
  (read (lfedoc-sanitise str)))

(defun lfedoc-sexp (str)
  "Convert STR to sexp."
  (lfedoc-call-struct (read (lfedoc-sanitise str))))

(defun lfedoc-call-struct (my-sexp)
  "Examine MY-SEXP and return a structure representing module function and arity."
  (cond ((lfedoc-new-erlang-callp my-sexp)
         (lfedoc-new-erlang-call-args my-sexp))
        ((lfedoc-old-erlang-callp my-sexp)
         (lfedoc-old-erlang-call-args my-sexp))
        (t
         (lfedoc-unknown-code my-sexp))))

;;; -------------- doc ---------------------

(defun lfedoc-inspect ()
  "Print sexp."
  (interactive)
  (let ((sexp-str (sexp-at-point)))
    ;; show read source and the sanitised version used for reading by Emacs
    (princ (list sexp-str
                 'sanitised-version (lfedoc-sanitise sexp-str)
                 'macroexpanded
                 (format "lfe -e \"(io:format (macroexpand-all (quote %s )))\""
                         sexp-str)))))

(defun lfedoc-helpme ()
  "Go to Erlang website for help."
  (interactive)
  ;; Read sanitised sexp and extract model and function for browse-url look-up
  (let ((call-struct (lfedoc-sexp (sexp-at-point))))
    (if (car call-struct)
        (progn
          (if (or (cl-equalp "cl" (car call-struct)) ; different forms are read differently
                  (cl-equalp 'cl (car call-struct)))
              ;; browse Hyperspec
              (browse-url
               (format "http://clhs.lisp.se/Body/f_%s.htm" (nth 1 call-struct)))
            ;; browse Erlang documentation
            (browse-url
             (apply 'format
                    (cons "http://erlang.org/doc/man/%s.html#%s-%d"
                          call-struct)))))
      (princ (list "search"
                   (lfedoc-find-symbol-functions (nth 1 call-struct))
                   "for"
                   (nth 1 call-struct)
                   'arity (nth 2 call-struct))))))

(defun lfedoc-cl-function-callp (sl)
  "Check if the SL is a supplemental Lisp function.")

(defun lfedoc-cl-function-call-args (sl)
  "Try to loop up for SL using the Hyperspec equivalent.")

(defun lfedoc-new-erlang-callp (sl)
  "Check id the SL is the new Erlang call syntax."
  (eql (car sl) :))

(defun lfedoc-new-erlang-call-args (sl)
  "Get new Erlang call info for the documentation look-up list SL."
  (cond ((and (nth 1 sl)
              (nth 2 sl))
         (list (nth 1 sl) (nth 2 sl) (- (length sl) 3)))
        ((nth 1 sl)
         (list (nth 1 sl) nil nil))
        (t
         (list nil nil nil))))

(defun lfedoc-old-erlang-callp (sl)
  "Check id the SL is the old Erlang call syntax."
  (eql (length (split-string (symbol-name (car sl))
                             ":"))
       2))

(defun lfedoc-old-erlang-call-args (sl)
  "Get old Erlang call info for the documentation look-up list SL."
  (let ((call-str (split-string (symbol-name (car sl)) ":")))
    (list (nth 0 call-str)
          (nth 1 call-str)
          (- (length sl) 1))))

(defun lfedoc-unknown-code (sl)
  "Provide unrecognised module information from SL."
  ;; because it's not a module:function of : module function
  ;; we returm nil as module but still return the function and arity
  (list nil (car sl) (- (length sl) 1)))

(defun lfedoc-find-symbol-functions (symb)
  "Find symbol SYMB in known symbols and return the function names that return it."
  ;; example (lfedoc-find-symbol-functions  (quote car))
  (-reject 'null
           (-map (lambda (f) (when (-contains? (funcall f) symb) f))
                 (lfedoc-get-symbol-functions))))

;; Trying another set of correct values. for which we should have working
;; auto-completion

;; using (lfedoc-find-symbol-autocompletions nil)
;; () all modules and user_guide functions in groups
;; () alternatively all modules or all functions in groups
;; (: ) all modules

;; (: m) all modules starting with m, m is the incomplete name
;; (: mod) all mod functions, mod is the full name of the module
;; if mod is a full name of the module but at the same time is a
;; prefix/incomplete name of other modules we might have show both possible
;; module name completions and module functions

;; (: mod f) all mod functions starting with f
;; (a) all modules and user_guide functions starting with a - (pp (lfedoc-find-symbol-autocompletions 'a))
;; (mod:) all mod functions
;; (mod:f) all mod functions starting with f
;; otherwise nothing

;;; in scratch buffer evaluate (lfedoc-test-all)
(defun lfedoc-test-all ()
  "Test all test cases."
  (let ((error-count 0))
    (let ((test-case (lambda (tc)
                       (if tc
                           (princ "Y")
                         (progn
                           (cl-incf error-count)
                           (princ "E"))))))
      (lfedoc-query-loaded-modules)
      (princ (format "%ctesting%c" 10 10))
      ;; my test cases
      (funcall test-case (not (>= 1 2)))
      ;; all modules
      (message "loaded modules: %i" (length (lfedoc-query-loaded-modules)))
      (funcall test-case (equal 101 (length (lfedoc-query-loaded-modules))))
      (funcall test-case (equal "application" (car (lfedoc-query-loaded-modules))))
      (funcall test-case (equal "zlib" (car (last (lfedoc-query-loaded-modules)))))
      (funcall test-case (equal 'application (car (lfedoc-data-loaded-modules))))
      (funcall test-case (equal 'zlib (car (last (lfedoc-data-loaded-modules)))))
      ;; all modules
      (funcall test-case (equal 101 (length (lfedoc-modules-2 nil))))
      ;; all modules starting with c
      (funcall test-case (equal '(c code code_server) (lfedoc-modules-2 'c)))
      ;; all functions in module io
      (funcall test-case (equal 22 (length (lfedoc-module-functions-2 "io" ""))))
      ;; all functions in module io that start with p
      (funcall test-case (equal '(parse_erl_exprs parse_erl_form printable_range put_chars)
                                (lfedoc-module-functions-2 "io" "p")))
      ;; test reading string representations of sexps and resulting lengths
      ;; note that (:), (a), (mod:) and (mod:f) all have length 1, so we will
      ;; have to check if it is a colon, contains a colon (hopefully only 1),
      ;; or is a list containing a symbol
      ;; $ lfe -e "(pp (macroexpand-all '(mo:d:fu:n)))"
      ;; $ lfe -e "(pp (macroexpand-all '(mod:fun)))"

      ;; test parsing and reading
      (funcall test-case (equal  '(("()" nil 0) ("(: )" (:) 1) ("(: a)" (: a) 2)
                                   ("(: mod)" (: mod) 2) ("(: mod f)" (: mod f) 3)
                                   ("(: mod f 1)"  (: mod f 1) 4)
                                   ("(a)" (a) 1) ("(mod:)" (mod:) 1)
                                   ("(mod:f)" (mod:f) 1) ("(mod:f 1)" (mod:f 1) 2))
                                 (-map (lambda (x) (list x
                                                    (lfedoc-read-sexp x)
                                                    (length (lfedoc-read-sexp x))))
                                       (list "()" "(: )" "(: a)" "(: mod)"
                                             "(: mod f)" "(: mod f 1)" "(a)"
                                             "(mod:)" "(mod:f)" "(mod:f 1)"))))
      ;; test back-end function invocation
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "()")
                          (lfedoc-find-symbol-autocompletions nil)))
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "(:)")
                          (lfedoc-data-loaded-modules)))
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "(io:)")
                          (lfedoc-module-functions-2 "io" "")))
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "(io:f)")
                          (lfedoc-module-functions-2 "io" "f")))
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "(a)")
                          (lfedoc-find-symbol-autocompletions 'a)))
      ;; conclusion

      (princ (format "%cerror count %s%c" 10 error-count 10))
      nil)))
