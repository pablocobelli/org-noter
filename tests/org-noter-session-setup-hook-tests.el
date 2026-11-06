;;; -*- lexical-binding: t; -*-
(add-to-list 'load-path "modules")
(require 'with-simulated-input)
(load-file "org-noter-test-utils.el")

(defvar mock-contents-session-setup-notes-file
  ":PROPERTIES:
:ID:       FAKE_4
:END:
#+TITLE: Test book notes (session setup)
* solove-nothing-to-hide
:PROPERTIES:
:NOTER_DOCUMENT: pubs/solove-nothing-to-hide.pdf
:END:
")

(defun org-noter-session-setup-test-run-timers ()
  "Run the idle timers org-noter schedules while starting a session."
  (dolist (timer (copy-sequence timer-idle-list))
    (timer-event-handler timer)))

(describe "org-noter-after-session-setup-hook"
  (before-each
    (create-org-noter-test-session)
    (spy-on 'org-noter-core-test-get-current-view :and-return-value (vector 'paged 99))
    (spy-on 'org-noter-core-test-document-property
            :and-call-fake (lambda (&optional doc-prop) (or doc-prop org-noter-test-file))))

  (it "runs once, in the notes buffer, when the session is ready"
    (let (runs)
      (with-mock-contents
       mock-contents-session-setup-notes-file
       (lambda ()
         (let ((org-noter-after-session-setup-hook
                (list (lambda ()
                        (push (list (buffer-name) (org-noter--valid-session org-noter--session))
                              runs)))))
           (goto-char (point-min))
           (re-search-forward "^\\* solove-nothing-to-hide")
           (org-noter-core-test-create-session)
           (org-noter-session-setup-test-run-timers))))
      (expect (length runs) :to-equal 1)
      (expect (car (car runs)) :to-match "\\`Notes of ")
      (expect (cadr (car runs)) :to-be-truthy)))

  (it "waits until the windows are set up when the document is another buffer"
    ;; with the document in its own buffer the session is not `single', so the
    ;; windows are set up from an idle timer and the hook must run after that
    (let* ((doc-buffer (get-buffer-create " *org-noter test document*"))
           runs
           (record-windows (lambda (&rest _) (push 'windows runs))))
      (spy-on 'org-noter-core-test-open-document-functions :and-return-value doc-buffer)
      (advice-add 'org-noter--setup-windows :after record-windows)
      (unwind-protect
          (with-mock-contents
           mock-contents-session-setup-notes-file
           (lambda ()
             (let ((org-noter-after-session-setup-hook
                    (list (lambda () (push (buffer-name) runs)))))
               (goto-char (point-min))
               (re-search-forward "^\\* solove-nothing-to-hide")
               (org-noter-core-test-create-session)
               (expect runs :to-equal nil)
               (org-noter-session-setup-test-run-timers))))
        (advice-remove 'org-noter--setup-windows record-windows)
        (kill-buffer doc-buffer))
      (expect (length runs) :to-equal 2)
      (expect (cadr runs) :to-equal 'windows)
      (expect (car runs) :to-match "\\`Notes of "))))
