;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

(load! "gcal")

(defconst org-directory "~/Dropbox/org/")
(setq org-agenda-files (list org-directory (concat org-directory "daily/")))
(setq org-gcal-file (concat org-directory "google-calendar.org"))
(setq org-capture-file (concat org-directory "refile.org"))

;; org babel
(setq org-babel-python-command *python*)
;; languages to run in source blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; todo workflow
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
              (sequence "QUESTION(q)" "|" "ANSWERED(a@/!)")
              (sequence "PROBLEM(p)" "|" "SOLVED(s@/!)")
              (sequence "SOMEDAY(o)"))))

  ;; have `t' mark todos
  (setq evil-org-key-theme '(textobjects navigation additional insert todo))


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

(defconst daily-orgfile
  (concat org-directory "daily/" (today) ".org"))

;; Capture templates for: todos, responds, notes, meetings
(setq org-capture-templates
      (backquote (("t" "todo" entry (file ,org-capture-file)
                   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)

                  ;; ("e" "event" entry (file ,org-gcal-file)
                  ;;  "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

                  ("r" "respond" entry (file ,org-capture-file)
                   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
                   :clock-in t :clock-resume t :immediate-finish t)

                  ;; ("d" "my day" plain (file ,daily-orgfile)
                  ;;  ,(concat "#+TITLE: My day\n#+DATE: "
                  ;;           (today)
                  ;;           "\n\n* Tasks\n** TODO \n\n* Thoughts")
                  ;;  :clock-in t :clock-resume t)

                  ("n" "note" entry (file ,org-capture-file)
                   "* %? :NOTE:\n%U\n%a\n"
                   :clock-in t :clock-resume t)

                  ("m" "meeting" entry (file ,org-capture-file)
                   "* MEETING with %? :MEETING:\n%U"
                   :clock-in t :clock-resume t))))


;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; google calendar
;; information imported from gcal.el
(setq org-gcal-client-id *gcal-client-id*
      org-gcal-client-secret *gcal-client-secret*)

(setq org-gcal-file-alist
      (backquote (("johngargalionis@gmail.com" . ,org-gcal-file))))

(org-gcal-fetch)                        ; fetch google calendar events into org-agenda

;; allow whitespace after headings
(setq org-cycle-separator-lines 1)

;; place tags after headings with only one space in between
(setq org-tags-column 0)
