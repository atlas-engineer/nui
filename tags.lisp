(in-package :spinneret)

;; TODO: Accessibility and ARIA roles.

(defun collect-n-args (maybe-n-args)
  (loop for (key value) on maybe-n-args by #'cddr
        when (uiop:string-prefix-p "N-" (symbol-name key))
          collect key
          and collect value))

(defmacro with-n-args ((n-args-var &optional no-n-args-var) maybe-n-args &body body)
  (alexandria:with-gensyms (maybe-var)
    `(let* ((,maybe-var ,maybe-n-args)
            (,n-args-var (collect-n-args ,maybe-var))
            ,@(when no-n-args-var
                `((,no-n-args-var (set-difference ,maybe-var ,n-args-var :test #'equal)))))
       ,@body)))

;;; Preprocessed tags.

(deftag :nstyle (body attrs &rest keys &key &allow-other-keys)
  "Regular <style>, but with BODY preprocessed by `nui:nstyle-body'.

N-args (when present) may be useful to pass additional data to
`nui:nstyle-body'."
  (let ((attrs attrs))
    (declare (ignore attrs))
    (with-n-args (n-args non-n-args) keys
      `(:style.nstyle ,@non-n-args (:raw (nui:nstyle-body (list ,@body) ,@n-args))))))

(deftag :nscript (body attrs &rest keys &key &allow-other-keys)
  "Regular <script>, but with BODY preprocessed by `nui:nscript-body'.

N-args (when present) may be useful to pass additional data to
`nui:nscript-body'."
  (let ((attrs attrs))
    (declare (ignore attrs))
    (with-n-args (n-args non-n-args) keys
      `(:script.nscript ,@non-n-args (:raw (nui:nscript-body (list ,@body) ,@n-args))))))

;;; Interactive tags.

(deftag :nselect (body attrs &rest keys &key default (id (alexandria:required-argument 'id)) &allow-other-keys)
  "Generate <select> tag from the BODY resembling cond clauses.

BODY can be:
- Multiple forms, each following/producing one of the formats:
  - (VALUE . FORMS) -- creates <option value=\"value\">value</option> and runs
    FORMS when it's selected.
  - ((VALUE DISPLAY TITLE) . FORMS) -- creates an <option value=\"value\"
    title=\"title\">display</option> and runs FORMS when it's selected. DISPLAY
    and TITLE are optional literal strings.
- A single form, expected to produce a list of forms like above.

In both cases, VALUEs should be literal (and `nui:nprint'-able) atoms. For
instance, symbol, number, string, or keyword.

DEFAULT is a string or a literal atom that is used as the default
option. This option will be both disabled and selected initially.

Options are generated with `nui:nselect-options', onchange event is
generated with `nui:nselect-onchange'.

N-args (when present) may be useful to pass additional data to
`nui:nselect-onchange' and `nui:nselect-options'.

Example for the case BODY compiles to Parenscript:
\(:nselect :id \"number-guessing\"
  :default \"Guess the number\"
  '(1 (ps:chain console (log \"Too low!\")))
  (list 2 `(ps:chain console (log \"Correct!\")))
  `(3 (ps:chain console (log (ps:lisp ,(format nil \"Too ~:@(high~)!\"))))))"
  (once-only (id)
    (with-gensyms (body-var)
      (let ((attrs attrs))
        (declare (ignorable attrs))
        (with-n-args (n-args not-n-args) keys
          `(let ((,body-var ,(if (serapeum:single body)
                                 (first body)
                                 `(list ,@body))))
             (:select.nselect
              ,@not-n-args
              :id ,id
              :onchange (nui:nselect-onchange ,id ,body-var ,@n-args)
              ,@(when default
                  `((:option :selected t :disabled t (nui:nprint ,default))))
              (:raw (nui:nselect-options ,id ,body-var ,@n-args)))))))))

(deftag :nbutton (body attrs &rest keys &key (text (alexandria:required-argument 'text))
                       &allow-other-keys)
  "A button with TEXT text and BODY action.

BODY can consist of quoted lists or forms producing them. produced
forms will be compiled (via `nui:nbutton-onclick').

N-args (when present) may be useful to pass additional data to
`nui:nbutton-onclick'.

Example for the case BODY compiles to Parenscript:
\(:nbutton
  :buffer buffer
  :text \"Do something\"
  '(ps:chain console (log \"Hello!\"))
  (list 'alert))"
  (let ((attrs attrs))
    (declare (ignorable attrs))
    (with-n-args (n-args not-n-args) keys
      `(:button.nbutton
        :onclick (nui:nbutton-onclick (list ,@body) ,@n-args)
        ,@not-n-args
        ,text))))

(deftag :ninput (body attrs &rest keys &key rows cols onfocus onchange &allow-other-keys)
  "Convenient <textarea> with a reasonable number of ROWS/COLS to accommodate the BODY.
Calls Lisp forms in ONFOCUS and ONCHANGE when one focuses and edits the input (respectively).

BODY should be a string or an implicit progn producing a string.

ONFOCUS, and ONCHANGE should each be a quoted Lisp forms or form
producing it. The provided form will be compiled by
`nui:ninput-onfocus' and `nui:ninput-onchange' respectively.

N-args (when present) may be useful to pass additional data to
`nui:ninput-onfocus' and `nui:ninput-onchange'."
  (let ((attrs attrs))
    (declare (ignorable attrs))
    (with-n-args (n-args not-n-args) keys
      (with-gensyms (input)
        ;; NOTE: It's unlikely that BODY will have multiple forms, but better
        ;; prepare for it, just in case someone goes stateful.
        `(let ((,input (progn ,@body)))
           (:textarea.ninput
            :rows (or ,rows (1+ (count #\Newline ,input)) 1)
            :cols (or ,cols
                      (ignore-errors
                       (reduce #'max (serapeum:lines ,input :eol-style :unicode) :key #'length))
                      80)
            ;; More events here.
            ,@(when onfocus
                `(:onfocus (nui:ninput-onfocus ,onfocus ,@n-args)))
            ,@(when onchange
                `(:onkeydown (nui:ninput-onchange ,onchange ,@n-args)))
            ,@not-n-args
            ;; Maybe add ninput-body? Should be unnecessary,
            ;; though---ninput works with mere strings.
            (:raw (the string ,input))))))))

;;; Lisp-enriched tags.

(deftag :nxref (body attrs &rest keys &key (n-package *package*) &allow-other-keys)
  "Create a link to the documentation of BODY symbol.

N-args (when present) may be useful to pass additional data to
`nui:nxref-link', `nui:nxref-doc', and `nui:nxref-display'.

N-PACKAGE, as a pre-set N-arg, mandates which package to print BODY
symbol in."
  (let ((attrs attrs)
        (n-package n-package))
    (declare (ignorable attrs n-package))
    (with-n-args (n-args not-n-args) keys
      `(:a.nxref
        ,@not-n-args
        :href (nui:nxref-link ,(first body) ,@n-args)
        :title (nui:nxref-doc ,(first body) ,@n-args)
        (:code
         (nui:nxref-display ,(first body) ,@n-args))))))

(deftag :ncode (body attrs &rest keys &key n-package (n-inline-p nil n-inline-provided-p)
                     &allow-other-keys)
  "Generate the <pre>/<code> listing from the provided Lisp BODY.

Forms in BODY should be quoted.

N-args are passed to `nui:ncode-body' and
`nui:ncode-inline-p'. Predefined N-args are:
- N-PACKAGE: package to `nui:nprint' the BODY in.
- N-INLINE-P: omitting newlines and <pre> tags---basically a <code>
tag with syntax highlighting. If not provided, is determined
automatically based on `nui:nprint'-ed BODY size."
  (let ((attrs attrs)
        (n-package n-package))
    (declare (ignore attrs n-package))
    (once-only (body)
      (with-n-args (n-args non-n-args) keys
        (alexandria:with-gensyms (body-var inline-var htmlized-var)
          `(let* ((,body-var (list ,@body))
                  (,inline-var ,(if n-inline-provided-p
                                    n-inline-p
                                    `(nui:ncode-inline-p ,body-var ,@n-args)))
                  (,htmlized-var (nui:ncode-body ,body-var ,@n-args)))
             (if ,inline-var
                 (:span.ncode ,@non-n-args (:raw ,htmlized-var))
                 (:pre.ncode ,@non-n-args (:raw ,htmlized-var)))))))))

;;; Structural tags.

(deftag :nsection (body attrs &rest keys
                        &key (title (alexandria:required-argument 'title))
                        level
                        (open-p t)
                        (id (nui:nsection-id title))
                        &allow-other-keys)
  "Collapsible and reference-able <section> with a header.

TITLE should be a human-readable title for a section, or the form producing one.
ID is the string identifier with which to reference the section
elsewhere. If not provided, auto-generated with `nui:nsection-id'.
Header is generated from TITLE and ID with `nui:nsection-header'.

LEVEL (if provided), is the level of heading for the section. If it's 2, the
heading is <h2>, if it's 3, then <h3> etc. If not provided, uses <h*> Spinneret
tag to intelligently guess the current heading level.
OPEN-P mandates whether the section is collapsed or not. True (= not
collapsed) by default.

N-args (when present) may be useful to pass additional data to
`nui:nsection-header'."
  (check-type level (or null (integer 2 6)))
  (let ((attrs attrs))
    (declare (ignore attrs))
    (with-gensyms (id-var)
      (with-n-args (n-args) keys
        `(let ((spinneret::*html-path*
                 ;; Push as many :section tags into the path, as necessary to imply
                 ;; LEVEL for the sections inside this one. A trick on Spinneret to
                 ;; make it think it's deeply nested already.
                 (append
                  spinneret::*html-path*
                  (make-list ,(if level
                                  `(1- (- ,level (spinneret::heading-depth)))
                                  0)
                             :initial-element :section)))
               (,id-var ,id))
           (:section.nsection
            :id ,id-var
            (:details
             :open ,open-p
             (:summary (:raw (nui:nsection-header ,title ,id-var ,@n-args)))
             ,@body)))))))

(deftag :ntoc (body attrs &rest keys &key (title "Table of contents") (depth 3) &allow-other-keys)
  "Generate table of contents for BODY up to DEPTH.
Looks for section tags with ID-s to link to.
:nsection sections are perfectly suitable for that."
  (let ((attrs attrs))
    (declare (ignorable attrs))
    (with-n-args (n-args non-n-args) keys
      (with-gensyms (body-var)
        `(let ((,body-var (with-html-string ,@body)))
           (:nav.toc#toc
            ,@non-n-args
            (:nsection
              :title ,title
              (:raw (nui:ntoc-body ,depth ,body-var ,@n-args))))
           (:raw ,body-var))))))
