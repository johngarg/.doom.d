;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

(load! "gcal")

(defconst org-directory "~/Dropbox/org/")
(setq org-gcal-file (concat org-directory "google-calendar.org"))
(setq org-capture-file (concat org-directory "refile.org"))

;; org babel
(setq org-babel-python-command *python*)

;; archive directory
(setq org-archive-location
      (concat "archive/archive-"
              (format-time-string "%Y%m" (current-time))
              ".org_archive::"))

;; org capture
(setq org-default-notes-file org-capture-file)
(setq org-refile-targets
      '((nil :maxlevel . 5)
        (org-agenda-files :maxlevel . 5)))

;; google calendar
;; information imported from gcal.el
(setq org-gcal-client-id *gcal-client-id*
      org-gcal-client-secret *gcal-client-secret*)

(setq org-gcal-file-alist
      (backquote (("johngargalionis@gmail.com" . ,org-gcal-file))))

(org-gcal-fetch) ; fetch google calendar events into org-agenda

;; allow whitespace after headings
(setq org-cycle-separator-lines 1)

;; place tags after headings with only one space in between
(setq org-tags-column 0)

;; org-ref and latex export
(setq org-ref-default-bibliography
      '("~/Dropbox/latex/jhep-template/main.bib"))
(setq org-latex-caption-above nil)
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
(setq org-latex-prefer-user-labels 1)
(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t ("pdflatex"))
        ("T1" "fontenc" t ("pdflatex"))
        ("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)
        ))

;; html export style
(setq org-agenda-export-html-style "")

;; Change how fontification is shown in buffers
(setq org-emphasis-alist
  '(("*" nil)
    ("/" nil)
    ("_" nil)
    ("=" nil)
    ("~" nil)
    ("+" nil)))
