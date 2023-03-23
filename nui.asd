;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(defsystem "nui"
  :description "Describe nui here"
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nui"
  :license  "BSD-3 Clause"
  :version "0.0.0"
  :depends-on ("spinneret" "serapeum" "plump" "clss")
  :serial t
  :components ((:file "package")
               (:file "walker")
               (:file "tags")
               (:file "nui"))
  :in-order-to ((test-op (test-op "nui/tests")
                         (test-op "nui/tests/compilation"))))

(defsystem "nui/submodules"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-submodule-system)

(defsystem "nui/tests"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-test-system
  :depends-on ("nui" "parenscript")
  :targets (:package :nui/tests)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests")))

(defsystem "nui/tests/compilation"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-compilation-test-system
  :depends-on ("nui")
  :packages (:nui))
