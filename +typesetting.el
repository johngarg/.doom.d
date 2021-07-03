;;; ~/.doom.d/+typesetting.el -*- lexical-binding: t; -*-

(setq-default line-spacing 5)
(setq-default display-line-numbers-type 'relative)
(defvar *fontsize* 14)

(defvar *font* "Iosevka SS07")
(defconst *fonts*
  `((doom-variable-pitch-font . ,*font*)
    (doom-unicode-font . ,*font*)
    (doom-big-font . ,*font*)))

(setq doom-font
      (font-spec :family *font* :size *fontsize*)

      doom-variable-pitch-font
      (font-spec :family (cdr (assoc 'doom-variable-pitch-font *fonts*)))

      doom-unicode-font
      (font-spec :family (cdr (assoc 'doom-unicode-font *fonts*))))
