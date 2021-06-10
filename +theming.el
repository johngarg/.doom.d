;;; ~/.doom.d/+theming.el -*- lexical-binding: t; -*-

;; remove annoying title bar
(setq-default frame-title-format '(""))

;; datetime constants to toggle theme for day and night
(defconst *datetime* (current-time-string))
(defconst *time* (nth 3 (split-string *datetime*)))
(defconst *hour* (string-to-number (car (split-string *time* ":"))))
(defconst *eveningp* (or (>= *hour* (+ 6 12)) (< *hour* 7)))

(defvar *dark-theme* 'doom-nord)
(defvar *light-theme* 'doom-one-light)
(defvar *theme-shade* (if *eveningp* "dark" "light"))

(defconst *themes*
  `(("light" . ,*light-theme*)
    ("dark" . ,*dark-theme*)))
(defvar *theme* (cdr (assoc *theme-shade* *themes*)))

(setq doom-theme *theme*)

;; set background of pdf-tools' midnight mode to match doom theme colours
(setq pdf-view-midnight-colors '("#2E3440" . "#2E3440" ))
