;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; feeds
(package! elfeed)

;; org
(package! org-gcal)
(package! ox-twbs)
(package! ox-ipynb :recipe (:fetcher github :repo "jkitchin/ox-ipynb"))

;; Python
(package! anaconda-mode)
(package! blacken)
(package! importmagic)
(package! py-isort)
(package! pyimport)
(package! python-docstring)

;; Mathematica
(package! wolfram-mode)

;; zen
(package! focus)
(package! writeroom-mode)
(package! centered-cursor-mode)

;; path
(package! exec-path-from-shell)

;;; Examples
;; (package! auto-highlight-symbol)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)