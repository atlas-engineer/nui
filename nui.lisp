;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:nui)

(defgeneric nprint (object &optional package)
  (:documentation "Print OBJECT (to string) aesthetically for NUI tags.
PACKAGE should be a valid package designator in which to print OBJECT."))

(defgeneric nstyle-body (body &rest n-args &key &allow-other-keys)
  (:method ((body list) &rest n-args &key &allow-other-keys)
    (declare (ignore n-args))
    (reduce #'serapeum:concat body))
  (:documentation "Print BODY for :nstyle contents.
Default method simply concatenates strings in BODY."))

(defgeneric nscript-body (body &rest n-args &key &allow-other-keys)
  (:method ((body list) &rest n-args &key &allow-other-keys)
    (declare (ignore n-args))
    (reduce #'serapeum:concat body))
  (:documentation "Print BODY for :nscript contents.
Default method simply concatenates strings in BODY."))

(defgeneric nselect-onchange (id clauses &rest n-args &key &allow-other-keys)
  (:documentation "Generic function to specify on option choice of :nselect.
Should return a string suitable for HTML onchange attribute.
Additional keyword arguments are allowed as NUI N-args."))

(defgeneric nselect-option (id clause &rest n-args &key &allow-other-keys)
  (:method (id (clause list) &rest n-args &key &allow-other-keys)
    (declare (ignore n-args))
    (spinneret:with-html-string
      (destructuring-bind (value &optional display title)
          (uiop:ensure-list (first clause))
        (:option
         :title title
         (or display (nprint value))))))
  (:documentation "Generic function to specify for every option in :nselect.
Should return an HTML string for an <option> matching CLAUSE.
Additional keyword arguments are allowed as NUI N-args.

By default generates an <option> based on the VALUE, DISPLAY, and
TITLE (see `:nselect' docstring for the format of CLAUSE)."))

(defgeneric nselect-options (id clauses &rest n-args &key &allow-other-keys)
  (:method ((id t) (clauses list) &rest n-args &key &allow-other-keys)
    (spinneret:with-html-string
      (dolist (clause clauses)
        (apply #'nselect-option id clause n-args))))
  (:documentation "Generic function to specify for :nselect.
Should return a set of <option>s as HTML string based on CLAUSES.
Additional keyword arguments are allowed as NUI N-args.

By default calls `nselect-option' on every form in CLAUSES."))

(defgeneric nbutton-onclick (clauses &rest n-args &key &allow-other-keys)
  (:documentation "Generic function to specify on click of :nbutton.
Should return a string suitable for HTML onchange attribute.
Additional keyword arguments are allowed as NUI N-args."))

(defgeneric ninput-onfocus (clauses &rest n-args &key &allow-other-keys)
  (:documentation "Generic function to specify on focus of :ninput.
Should return a string suitable for HTML onfocus attribute.
Additional keyword arguments are allowed as NUI N-args."))

(defgeneric ninput-onchange (clauses &rest n-args &key &allow-other-keys)
  (:documentation "Generic function to specify on input to :ninput.
Should return a string suitable for HTML onchange attribute.
Additional keyword arguments are allowed as NUI N-args."))

(defgeneric nxref-doc (body &rest n-args &key n-package &allow-other-keys)
  (:method ((body symbol) &rest n-args &key &allow-other-keys)
    (declare (ignore n-args))
    (cond
      ((member body *types*)
       (documentation body 'type))
      ((and (member body *variables*)
            (boundp body))
       (documentation body 'variable))
      ((and (fboundp body)
            (or (member body *functions*)
                (member body *macros*)))
       (documentation body 'function))
      (t (documentation body 'function))))
  (:documentation "Function to specify hoverable documentation for :nxref.

By default relies on code walker variables
(`*variables*',`*functions*', `*macros*', and `*types*') to fetch the
respective `documentation' for BODY symbol.

In case no list contains the BODY symbol, implies a function
documentation.

Additional keyword arguments are allowed as NUI N-args."))

(defgeneric nxref-link (body &rest n-args &key n-package &allow-other-keys)
  (:method ((body t) &rest n-args &key (n-package *package*) &allow-other-keys)
    "")
  (:documentation "Function to specify href link for :nxref.
Additional keyword arguments are allowed as NUI N-args.
Default method returns an empty string."))

(defgeneric nxref-display (body &rest n-args &key package &allow-other-keys)
  (:method ((body t) &rest n-args &key (n-package *package*) &allow-other-keys)
    (declare (ignore n-args))
    (nprint body n-package))
  (:documentation "Function to specify display for :nxref.
By default calls `nprint' on BODY.
Additional keyword arguments are allowed as NUI N-args."))

(defgeneric ncode-format-symbol (symbol package)
  (:method ((symbol symbol) package)
    (spinneret:with-html-string
      (:nxref :n-package package symbol)))
  (:documentation "Format the SYMBOL as HTML to substitute instead of its `nprint'-ed version."))

(defgeneric ncode-display (form &rest n-args &key n-package &allow-other-keys)
  (:method ((form string) &rest n-args &key &allow-other-keys)
    (declare (ignore n-args))
    form)
  (:method ((form list) &rest n-args &key (n-package *package*) &allow-other-keys)
    (declare (ignore n-args))
    (resolve form)
    (flet ((sort-by-printed-length (collection)
             (sort collection #'>
                   :key (lambda (s) (length (nprint s))))))
      ;; Important: sort these so that shorter ones are less likely to
      ;; break longer ones. Some possibility is still present,
      ;; though---we're working with mere string substitutions and
      ;; incomplete parsed data (because no SWANK/MOP).
      (setf *functions* (sort-by-printed-length *functions*)
            *macros* (sort-by-printed-length *macros*)
            *types* (sort-by-printed-length *types*)
            *variables* (sort-by-printed-length *variables*)))
    (let ((printed (nprint form)))
      (flet ((frob-from-list (list)
               (setf printed
                     ;; OPTIMIZE: Potential bottleneck: costly
                     ;; search/replace N^2 over LIST in a tight loop.
                     (uiop:frob-substrings
                      printed (mapcar #'nprint list)
                      (lambda (match frob)
                        (funcall
                         frob (ncode-format-symbol
                               (find match list :key #'nprint :test #'string=)
                               n-package)))))))
        ;; NOTE: Order matters: classes < vars < macros < functions in
        ;; quantity. Subjectively.
        (frob-from-list *types*)
        (frob-from-list *variables*)
        (frob-from-list *macros*)
        (frob-from-list *functions*))))
  (:documentation "Print FORM (from :ncode body) as HTML string, preferably syntax-highlighted.

Defaults with replacing all the symbols from walker
variables (`*variables*',`*functions*', `*macros*', and `*types*')
with their :nxref code.

Uses `ncode-format-symbol' for the actual symbol HTML.

Additional keyword arguments are allowed as NUI N-args."))

(defgeneric ncode-inline-p (body &rest n-args &key n-package &allow-other-keys)
  (:method ((body list) &rest n-args &key (n-package *package*) &allow-other-keys)
    (declare (ignorable n-args))
    (and (serapeum:single body)
         (zerop (count #\newline
                       (if (stringp (first body))
                           (first body)
                           (nprint (first body) n-package))))))
  (:documentation "Decide on whether to print the BODY as inline <code> tag, or as a <pre> listing.
Default method uses `nprint' on body and searches for newlines.
Additional keyword arguments are allowed as NUI N-args."))

(defgeneric ncode-body (body &rest n-args &key n-package &allow-other-keys)
  (:method ((body list) &rest n-args &key &allow-other-keys)
    (spinneret:with-html-string
      (:code
       (:raw (if (serapeum:single body)
                 (apply #'ncode-display (first body) n-args)
                 (serapeum:string-join
                  (mapcar (lambda (form) (apply #'ncode-display form n-args)) body)
                  (make-string 2 :initial-element #\newline)))))))
  (:documentation "Decide on whether to print the BODY as inline <code> tag, or as a <pre> listing.
Default method uses `nprint' on body and searches for newlines.
Additional keyword arguments are allowed as NUI N-args."))

(defgeneric nsection-id (title)
  (:method ((title null))
    (declare (ignore title))
    (alexandria:required-argument 'id))
  (:method ((title string))
    (reduce (lambda (string char)
              (if (and (plusp (length string))
                       (eql (elt string (1- (length string))) #\-)
                       (eql char #\-))
                  string
                  (uiop:strcat string char)))
            (substitute-if-not
             #\- #'alphanumericp
             (string-trim serapeum:whitespace (string-downcase title)))
            :initial-value ""))
  (:documentation "Generate the reference-able ID from TITLE.
By default downcases TITLE and replaces all non-alphanumeric chars
with hyphens."))

(defgeneric nsection-header (title id &rest n-args &key &allow-other-keys)
  (:method (title id &rest n-args &key &allow-other-keys)
    (declare (ignore n-args))
    (spinneret:with-html-string
      (:header
       :style "display: inline"
       (:a.link
        :href (uiop:strcat "#" id)
        (:h* :style "display: inline" title))
       " " (:a.link :href (uiop:strcat "#" id) "#"))))
  (:documentation "Generate the header code for :nsection.
Default method generates a linked heading.
Additional keyword arguments are allowed as NUI N-args."))

(defgeneric ntoc-body (depth body &rest n-args &key &allow-other-keys)
  (:method ((depth integer) (body string) &rest n-args &key &allow-other-keys)
    (declare (ignore n-args))
    (labels ((parents (elem)
               (let ((parent (plump:parent elem)))
                 (when (plump:element-p parent)
                   (cons parent (parents parent)))))
             (parent-section (elem)
               (find "section" (parents elem) :key #'plump:tag-name :test #'string-equal))
             (format-section (heading level)
               (spinneret:with-html-string
                 (let ((parent-section (parent-section heading)))
                   (:li.ntoc (:a :href (format nil "#~a" (plump:attribute parent-section "id"))
                                 (plump:text heading)))
                   (serapeum:and-let* ((_ (< level depth))
                                       (inner-level (1+ level))
                                       (inner-headers
                                        (clss:ordered-select (format nil "h~a" inner-level) parent-section)))
                     (:ul (loop for inner-header across inner-headers
                                collect (:raw (format-section inner-header inner-level)))))))))
      (let* ((dom (plump:parse body))
             (h2s (clss:ordered-select "h2" dom)))
        (spinneret:with-html-string
          (loop for h2 across h2s
                collect (:ul (:raw (format-section h2 2))))))))
  (:documentation "Generate the HTML for the :ntoc based on string BODY.
Default method parses the BODY into nested <ul> with relative links.
Additional keyword arguments are allowed as NUI N-args."))
