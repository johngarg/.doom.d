;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; org
;; (package! org-gcal)
(package! org-ref)
(package! org-mime)
;; (package! ox-latex-subfigure)
(package! ox-twbs)

;; note taking
(package!
  denote
  :recipe (:repo "protesilaos/denote" :host github :files ("*.el")))

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
;; (package! olivetti)

;; latex
(package! latex-extra)

;; path
(package! exec-path-from-shell)

;; unfill-paragraph
(package! unfill)

;; prot's themes
(package! modus-themes)
;; (package! ef-themes)

;; mac
(package! applescript-mode)

;; google translate
(package! google-translate)

;;; Examples
;; (package! auto-highlight-symbol)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
