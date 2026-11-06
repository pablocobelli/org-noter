;;; -*- lexical-binding: t; -*-
(add-to-list 'load-path "modules")
(require 'with-simulated-input)
(load-file "org-noter-test-utils.el")

(defvar mock-contents-fringe-notes-file
  ":PROPERTIES:
:ID:       FAKE_5
:END:
#+TITLE: Test book notes (fringe)
* solove-nothing-to-hide
:PROPERTIES:
:NOTER_DOCUMENT: pubs/solove-nothing-to-hide.pdf
:END:
")

(describe "org-noter notes buffer fringe indicators"
  (before-each
    (create-org-noter-test-session)
    (spy-on 'org-noter-core-test-document-property
            :and-call-fake (lambda (&optional doc-prop) (or doc-prop org-noter-test-file))))

  (it "hides truncation arrows and keeps visual-line-mode's hidden continuation arrows"
    (with-mock-contents
     mock-contents-fringe-notes-file
     (lambda ()
       (visual-line-mode 1)
       (goto-char (point-min))
       (re-search-forward "^\\* solove-nothing-to-hide")
       (org-noter-core-test-create-session)
       (with-current-buffer (org-noter--session-notes-buffer org-noter--session)
         ;; the first entry for each indicator is the one Emacs uses
         (expect (cdr (assq 'truncation fringe-indicator-alist)) :to-equal nil)
         (expect (cdr (assq 'continuation fringe-indicator-alist)) :to-equal '(nil nil)))))))
