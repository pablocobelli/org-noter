;;; -*- lexical-binding: t; -*-
(add-to-list 'load-path "modules")
(require 'with-simulated-input)
(load-file "org-noter-test-utils.el")

(defvar mock-contents-continue-in-notes-file
  ":PROPERTIES:
:ID:       FAKE_3
:END:
#+TITLE: Test book notes (continue in notes)
* solove-nothing-to-hide
:PROPERTIES:
:NOTER_DOCUMENT: pubs/solove-nothing-to-hide.pdf
:END:
** Later note
:PROPERTIES:
:NOTER_PAGE: (99 0.9 . 0.1)
:END:

Comment on the later note.
")

(defun org-noter-continue-test-insert (precise-info)
  "Start a session on the document heading and insert a note at PRECISE-INFO.
Return the text of the notes buffer split at point, as (BEFORE . AFTER)."
  (goto-char (point-min))
  (re-search-forward "^\\* solove-nothing-to-hide")
  (org-noter-core-test-create-session)
  (let ((org-noter-insert-note-no-questions t))
    (org-noter-insert-note nil precise-info))
  ;; read point in the notes window: with the option off, org-noter selects
  ;; the document window again
  (let ((window (seq-find (lambda (w) (string-prefix-p "Notes of " (buffer-name (window-buffer w))))
                          (window-list))))
    (with-current-buffer (window-buffer window)
      (let ((p (window-point window)))
        (cons (buffer-substring-no-properties (point-min) p)
              (buffer-substring-no-properties p (point-max)))))))

(describe "org-noter continue in notes"
  (before-each
    (create-org-noter-test-session)
    (spy-on 'org-noter-core-test-get-current-view :and-return-value (vector 'paged 99))
    (spy-on 'org-noter-core-test-document-property
            :and-call-fake (lambda (&optional doc-prop) (or doc-prop org-noter-test-file))))

  (it "leaves point below the quote, with empty lines before the next note"
    (with-mock-contents
     mock-contents-continue-in-notes-file
     (lambda ()
       (let ((split (org-noter-continue-test-insert '(0.2 . 0.1))))
         (expect (car split) :to-match "#\\+END_QUOTE\n\n\\'")
         (expect (cdr split) :to-match "\\`\n\n\\*\\* Later note")))))

  (it "leaves point below the quote at the end of the notes"
    (with-mock-contents
     mock-contents-continue-in-notes-file
     (lambda ()
       (let ((split (org-noter-continue-test-insert '(0.95 . 0.1))))
         (expect (car split) :to-match "#\\+END_QUOTE\n\n\\'")
         (expect (cdr split) :to-match "\\`[ \t\n]*\\'")))))

  (it "still highlights the selected text in the document"
    (with-mock-contents
     mock-contents-continue-in-notes-file
     (lambda ()
       (org-noter-continue-test-insert '(0.2 . 0.1))
       (expect 'org-noter-core-test-add-highlight :to-have-been-called))))

  (it "runs `org-noter-after-insert-note-hook' with point below the quote"
    (let (text-before-point)
      (with-mock-contents
       mock-contents-continue-in-notes-file
       (lambda ()
         (let ((org-noter-after-insert-note-hook
                (list (lambda ()
                        (setq text-before-point
                              (buffer-substring-no-properties
                               (max (point-min) (- (point) 13)) (point)))))))
           (org-noter-continue-test-insert '(0.2 . 0.1)))))
      (expect text-before-point :to-equal "#+END_QUOTE\n\n")))

  (it "leaves point at the start of the quote when the option is off"
    (with-mock-contents
     mock-contents-continue-in-notes-file
     (lambda ()
       (let* ((org-noter-insert-note-continue-in-notes nil)
              (hook-ran nil)
              (org-noter-after-insert-note-hook (list (lambda () (setq hook-ran t))))
              (split (org-noter-continue-test-insert '(0.2 . 0.1))))
         (expect (cdr split) :to-match "\\`#\\+BEGIN_QUOTE")
         (expect hook-ran :not :to-be-truthy))))))
