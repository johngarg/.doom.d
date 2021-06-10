;;; ~/.doom.d/+typesetting.el -*- lexical-binding: t; -*-

(setq-default line-spacing 5)
(setq-default display-line-numbers-type 'relative)
(defvar *fontsize* 14)

;; (defvar *font* "Monaco")
(defvar *font* "Roboto Mono Light")
;; (defvar *font* "Fira Code")
(defconst *fonts*
  '((doom-unicode-font . "Fira Sans")
    (doom-variable-pitch-font . "DejaVu Sans Mono")
    (doom-big-font . "Fira Code")))

(setq doom-font (font-spec :family *font* :size *fontsize*)
      doom-variable-pitch-font (font-spec :family *font*)
      doom-unicode-font (font-spec :family (cdr (assoc 'doom-variable-pitch-font *fonts*)))
      doom-big-font (font-spec :family (cdr (assoc 'doom-big-font *fonts*)) :size (+ 7 *fontsize*)))
