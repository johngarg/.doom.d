;;; ~/.doom.d/+typesetting.el -*- lexical-binding: t; -*-

(setq-default line-spacing 5)
(setq-default display-line-numbers-type 'relative)
(defvar *fontsize* 12)
(defconst *fonts*
  '(("Fira Code" . ((doom-variable-pitch-font . "Fira Sans")
                    (doom-unicode-font . "DejaVu Sans Mono")
                    (doom-big-font . "Fira Code")))))
(defvar *font* (caar *fonts*))

(setq doom-font (font-spec :family *font* :size *fontsize*)
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Fira Code" :size (+ 7 *fontsize*)))
