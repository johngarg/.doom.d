;;; ~/.doom.d/+theming.el -*- lexical-binding: t; -*-

;; remove annoying title bar
(setq-default frame-title-format '(""))

;; datetime constants to toggle theme for day and night
(defconst *datetime* (current-time-string))
(defconst *time* (nth 3 (split-string *datetime*)))
(defconst *hour* (string-to-number (car (split-string *time* ":"))))
(defconst *eveningp* (or (>= *hour* (+ 6 12)) (< *hour* 7)))

(defvar *dark-theme* 'doom-tomorrow-night)
(defvar *light-theme* 'doom-homage-white)
(defvar *theme-shade* (if *eveningp* "dark" "light"))

(defconst *themes*
  `(("light" . ,*light-theme*)
    ("dark" . ,*dark-theme*)))
(defvar *theme* (cdr (assoc *theme-shade* *themes*)))

(setq doom-theme *theme*)

;; set background of pdf-tools' midnight mode to match doom theme colours
(setq pdf-view-midnight-colors '("#2E3440" . "#2E3440" ))

;; Modus
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-fringes nil)
(setq modus-themes-org-agenda
        (quote ((header-block . (variable-pitch 1 semibold))
                (header-date . (grayscale bold-all 1))
                (event . (accented varied))
                (scheduled . uniform)
                (habit . traffic-light))))

(setq modus-themes-lang-checkers '(straight-underline faint))
(setq modus-themes-syntax '(faint alt-syntax))
(setq flycheck-indication-mode nil)
