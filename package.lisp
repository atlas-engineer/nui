;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package :nui
  (:use :common-lisp)
  (:export
   ;; General utils.
   #:nprint #:nattrs
   ;; Preprocessed tag generics.
   #:nstyle-body #:nscript-body
   ;; Interactive tag generics.
   #:nselect-onchange #:nselect-options
   #:nbutton-onclick
   #:ninput-onfocus #:ninput-onchange
   ;; Lisp-enriched tag generics.
   #:nkey-title #:nkey-display
   #:nxref-doc #:nxref-link #:nxref-display
   #:ncode-format-symbol #:ncode-display #:ncode-inline-p #:ncode-body
   ;; Structural tag generics.
   #:nsection-id #:nsection-header
   #:ntoc-body
   ;; Code walker variables:
   #:*specials* #:*variables* #:*functions* #:*macros* #:*classes*)
  (:documentation "NUI provides a set of Spinneret tags for documentation and text-based websites.
There is an extensible protocol to customize tag behavior.

The tags NUI adds on top of default Spinneret ones are:
- Preprocessing tags:
  - `:nstyle' for <style> generation with customizable rules.
  - `:nscript' is the same, but for <script>.
- Interactive tags:
  - `:nselect' for <select> drop-downs generated from the cond-like
    clauses.
  - `:nbutton' is a button with custom compiled actions.
  - `:ninput' is the input area that's tracking input and focus.
- Tags, based on the data from Lisp image:
  - `:nxref' to link to Lisp functions/classes/variables.
  - `:ncode' to generate syntax-highlighted code snippets.
- Structural/semantic tags:
  - `:nsection' as a document section with linkable heading.
  - `:ntoc' generating the Table of Contents out of its body.

Every single of these has a set of methods to customize the generated
code. Starting from the required methods to the almost unnecessary
ones:

Required methods (respective tags won't work without the specialized
methods):
- `nprint'
- `:nbutton':
  - `nbutton-onclick'.
- `:ninput':
  - `ninput-onchange'.
  - `ninput-onfocus'.
- `:nselect':
  - `nselect-onchange'.
  - (optional) `nselect-options'.

Recommended methods (respective tags would work, but likely won't be
useful enough):
- `:nxref':
  - `nxref-link'.
  - `nxref-doc'.
- `:ncode':
  - `ncode-body'.
  - `ncode-inline-p'.

Optional methods (defaults are intuitive and useful enough):
- `:nsection':
  - `nsection-id'.
  - `nsection-header'.
- `:ntoc'
  - `ntoc-body'.
- `nattrs' to generate the attributes for a certain NUI tag."))
