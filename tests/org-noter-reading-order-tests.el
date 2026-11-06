;;; -*- lexical-binding: t; -*-
(add-to-list 'load-path "modules")
(require 'with-simulated-input)
(load-file "org-noter-test-utils.el")
(require 'org-noter-pdf)

;; Same document as the one `org-noter-core-test-create-session' opens, so the
;; notes are recognized as part of the session when looking for where to
;; insert a new one.
(defvar mock-contents-reading-order-notes-file
  ":PROPERTIES:
:ID:       FAKE_1
:END:
#+TITLE: Test book notes (reading order)
* solove-nothing-to-hide
:PROPERTIES:
:NOTER_DOCUMENT: pubs/solove-nothing-to-hide.pdf
:END:
")

(defvar mock-contents-two-column-notes-file
  ":PROPERTIES:
:ID:       FAKE_2
:END:
#+TITLE: Test book notes (two columns)
* solove-nothing-to-hide
:PROPERTIES:
:NOTER_DOCUMENT: pubs/solove-nothing-to-hide.pdf
:COLUMN_EDGES: (0.49 1)
:END:
")

(defun org-noter-reading-order-test-create-session ()
  "Start the mock session on the document heading, as `org-noter' does.
From the top of the file the session spans the whole file and new
notes become siblings of the document heading instead of children."
  (goto-char (point-min))
  (re-search-forward "^\\* solove-nothing-to-hide")
  (org-noter-core-test-create-session))

(defun org-noter-reading-order-test-locations ()
  "Return the NOTER_PAGE locations of the notes in buffer order."
  (save-excursion
    (goto-char (point-min))
    (let (locations)
      (while (re-search-forward "^[ \t]*:NOTER_PAGE:[ \t]*\\(.+\\)$" nil t)
        (push (car (read-from-string (match-string 1))) locations))
      (nreverse locations))))

(defun org-noter-reading-order-test-insert (precise-infos)
  "Insert one precise note for each element of PRECISE-INFOS, in order."
  (let ((org-noter-insert-note-no-questions t))
    (dolist (info precise-infos)
      (org-noter-insert-note nil info))))

(defun org-noter-reading-order-test-selection (_mode _window)
  "Pretend the selection in the document starts at (0.5 . 0.25)."
  '(0.5 . 0.25))

(describe "org-noter reading order"
  (before-each
    (create-org-noter-test-session)
    ;; a paged view of page 99, the page returned by the mock approx-location
    (spy-on 'org-noter-core-test-get-current-view :and-return-value (vector 'paged 99))
    ;; the mock returns the notes file for every heading, which makes each
    ;; existing note look like it belongs to another document; keep the
    ;; NOTER_DOCUMENT value instead
    (spy-on 'org-noter-core-test-document-property
            :and-call-fake (lambda (&optional doc-prop) (or doc-prop org-noter-test-file))))

  (describe "comparing locations"
    (it "orders notes on the same line from left to right, even if their tops differ slightly"
      (expect (org-noter--compare-locations '< '(4 0.6 . 0.339) '(4 0.5981 . 0.717)) :to-be-truthy)
      (expect (org-noter--compare-locations '< '(4 0.5981 . 0.717) '(4 0.6 . 0.339)) :not :to-be-truthy))

    (it "keeps the left coordinate of the second location"
      (expect (org-noter--compare-locations '<= '(4 0.6 . 0.717) '(4 0.6 . 0.339)) :not :to-be-truthy)
      (expect (org-noter--compare-locations '<= '(4 0.6 . 0.339) '(4 0.6 . 0.717)) :to-be-truthy))

    (it "orders lines from top to bottom"
      (expect (org-noter--compare-locations '< '(4 0.40 . 0.88) '(4 0.60 . 0.10)) :to-be-truthy))

    (it "uses strict top-to-bottom order when the tolerance is 0"
      (let ((org-noter-same-line-tolerance 0))
        (expect (org-noter--compare-locations '< '(4 0.5981 . 0.717) '(4 0.6 . 0.339)) :to-be-truthy)))

    (it "places page notes before the precise notes of their page"
      (expect (org-noter--compare-locations '< '(4 . 0) '(4 0.01 . 0.1)) :to-be-truthy)
      (expect (org-noter--compare-locations '< '(3 0.9 . 0.9) '(4 . 0)) :to-be-truthy))

    (it "reads a whole column before the next one when COLUMN_EDGES is set"
      (with-temp-buffer
        (org-mode)
        (insert "* doc\n:PROPERTIES:\n:COLUMN_EDGES: (0.49 1)\n:END:\n")
        ;; bottom of the left column, then top of the right column
        (expect (org-noter--compare-locations '< '(1 0.51 . 0.46) '(1 0.41 . 0.64)) :to-be-truthy)
        ;; same line of the same column, left to right
        (expect (org-noter--compare-locations '< '(1 0.6 . 0.30) '(1 0.598 . 0.40)) :to-be-truthy))))

  (describe "inserting precise notes"
    (dolist (order '((0 1 2 3 4 5) (5 4 3 2 1 0) (2 5 0 3 1 4)))
      (it (format "keeps reading order when notes are taken in order %S" order)
        (with-mock-contents
         mock-contents-reading-order-notes-file
         (lambda ()
           (org-noter-reading-order-test-create-session)
           (let ((infos '((0.40 . 0.88)                               ; line 1
                          (0.598 . 0.274) (0.6 . 0.339) (0.6 . 0.717) ; line 2
                          (0.80 . 0.10) (0.7984 . 0.19))))            ; line 3
             (org-noter-reading-order-test-insert
              (mapcar (lambda (i) (nth i infos)) order))
             (expect (org-noter-reading-order-test-locations)
                     :to-equal (mapcar (lambda (info) (cons 99 info)) infos)))))))

    (it "keeps reading order across two columns"
      (with-mock-contents
       mock-contents-two-column-notes-file
       (lambda ()
         (org-noter-reading-order-test-create-session)
         ;; right column top, left column bottom, left column top
         (org-noter-reading-order-test-insert '((0.41 . 0.64) (0.51 . 0.46) (0.2 . 0.1)))
         (expect (org-noter-reading-order-test-locations)
                 :to-equal '((99 0.2 . 0.1) (99 0.51 . 0.46) (99 0.41 . 0.64)))))))

  (describe "inserting notes on a selection"
    (before-each
      (add-hook 'org-noter--get-selection-precise-info-hook
                #'org-noter-reading-order-test-selection))
    (after-each
      (remove-hook 'org-noter--get-selection-precise-info-hook
                   #'org-noter-reading-order-test-selection))

    (it "locates the note at the selection"
      (with-mock-contents
       mock-contents-reading-order-notes-file
       (lambda ()
         (org-noter-reading-order-test-create-session)
         (let ((org-noter-insert-note-no-questions t))
           (org-noter-insert-note))
         (expect (org-noter-reading-order-test-locations) :to-equal '((99 0.5 . 0.25))))))

    (it "keeps the page location when nothing is selected"
      (spy-on 'org-noter-test-get-selected-text :and-return-value nil)
      (with-mock-contents
       mock-contents-reading-order-notes-file
       (lambda ()
         (org-noter-reading-order-test-create-session)
         (let ((org-noter-insert-note-no-questions t))
           (org-noter-insert-note))
         (expect (org-noter-reading-order-test-locations) :not :to-equal '((99 0.5 . 0.25))))))

    (it "keeps the page location when the option is off"
      (with-mock-contents
       mock-contents-reading-order-notes-file
       (lambda ()
         (org-noter-reading-order-test-create-session)
         (let ((org-noter-insert-note-no-questions t)
               (org-noter-insert-note-use-selection-location nil))
           (org-noter-insert-note))
         (expect (org-noter-reading-order-test-locations) :not :to-equal '((99 0.5 . 0.25))))))))
