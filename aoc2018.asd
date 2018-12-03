;;;; aoc2018.asd

(asdf:defsystem #:aoc2018
  :description "Describe cmgr here"
  :author "Kristian Müllenholz <k.muellenholz@mailbox.org>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:alexandria)
  :components ((:file "aoc2018")))
