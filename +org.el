;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

(load! "gcal")
(load! "ob-mathematica")

(defconst org-directory "~/Dropbox/org/")
(setq org-gcal-file (concat org-directory "google-calendar.org"))
;; (setq org-mac-link-mail-account "johngargalionis@gmail.com")

;; add custom workflow for meetings
(after! org
  (let ((refile-file (concat org-directory "refile.org")))

    ;; agenda
    (setq org-agenda-timegrid-use-ampm 1)

    ;; allow whitespace after headings
    (setq org-cycle-separator-lines 1)

    ;; capture
    (setq org-capture-file refile-file)
    (setq org-default-notes-file refile-file)
    (setq +org-capture-notes-file refile-file)
    (setq +org-capture-todo-file refile-file)
    (setq +org-capture-projects-file refile-file)
    (setq +org-capture-changelog-file refile-file)
    (setq org-refile-targets
          '((nil :maxlevel . 5)
            (org-agenda-files :maxlevel . 5)))

    ;; todo workflow
    (setq org-todo-keywords
          (append
           org-todo-keywords
           '((sequence "MEET(m)" "TUTE(u)" "DEBT(b)" "|" "PAID(P)" "CANC(c)"))))
    (setq org-todo-keyword-faces
          (append org-todo-keyword-faces '(("DEBT" . +org-todo-active))))))

;; disable smart-parens in org mode (fixes slow delete char and insert *)
(add-hook 'org-mode-hook #'turn-off-smartparens-mode)

;; org babel
(setq org-babel-python-command *python*)

;; Load mathematica from contrib
(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
         '((mathematica . t))))

;; Sanitize output and deal with paths
(setq org-babel-mathematica-command "/Users/johngargalionis/.local/bin/mash")
;; Font-locking
(add-to-list 'org-src-lang-modes '("mathematica" . wolfram))
;; For wolfram-mode
(setq mathematica-command-line "/Users/johngargalionis/.local/bin/mash")

;; archive directory
(setq org-archive-location
      (concat "archive/archive-"
              (format-time-string "%Y%m" (current-time))
              ".org_archive::"))

;; google calendar
;; information imported from gcal.el
(setq org-gcal-client-id *gcal-client-id*
      org-gcal-client-secret *gcal-client-secret*)

(setq org-gcal-file-alist
      (backquote (("johngargalionis@gmail.com" . ,org-gcal-file))))

;; (org-gcal-fetch) ; fetch google calendar events into org-agenda

;; place tags after headings with only one space in between
(setq org-tags-column 0)

;; org-ref and latex export
(setq org-ref-default-bibliography
      '("~/Dropbox/latex/jhep-template/main.bib"))
(setq org-latex-caption-above nil)
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
(setq org-latex-prefer-user-labels t)
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
;; (setq org-emphasis-alist
;;   '(("*" nil)
;;     ("/" nil)
;;     ("_" nil)
;;     ("=" nil)
;;     ("~" nil)
;;     ("+" nil)))

;; subfigure
;; (require 'ox-latex-subfigure)

;; remove holidays from cfw calendar
(setq cfw:display-calendar-holidays nil)

;; toggling emphasis markers
(setq org-hide-emphasis-markers t)
(defun my-org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t))
  (org-mode-restart))

(defun my-org-clear-latex-preview-cache ()
  (interactive)
  (shell-command "rm -f ~/.emacs.d/.local/cache/org-latex/*"))

(setq org-reverse-note-order t)
(setq org-ellipsis " ↓")

;; Agenda
(setq org-agenda-tags-column 5)
(setq org-agenda-custom-commands
      '(("D" "John's agenda"
         ((tags-todo "*"
                ((org-agenda-overriding-header "📬 Important unscheduled tasks\n")
                 (org-agenda-skip-function
                  `(org-agenda-skip-entry-if
                    'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                 ))
          (agenda ""
                  ((org-agenda-block-separator nil)
                   (org-agenda-start-day "0d")
                   (org-agenda-span 1)
                   (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                   (org-deadline-warning-days 0)
                   (org-agenda-overriding-header "\n\n🗓 Today\n")))
          (agenda ""
                  ((org-agenda-block-separator nil)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "+1d")
                   (org-agenda-block-separator nil)
                   (org-agenda-span 3)
                   (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                   (org-deadline-warning-days 0)
                   (org-agenda-overriding-header "\n\n👀 Next three days\n")))
          (agenda ""
                  ((org-agenda-time-grid nil)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "+3d")
                   (org-agenda-span 14)
                   (org-agenda-show-all-dates nil)
                   (org-agenda-time-grid nil)
                   (org-deadline-warning-days 0)
                   (org-agenda-block-separator nil)
                   (org-agenda-entry-types '(:deadline :sexp :scheduled))
                   (org-agenda-skip-function `(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "\n\n🚀 Next two weeks\n")))
          ))
        ))
