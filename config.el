;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; constants
(defconst *python* "python3")
(defconst *terminal* "/Applications/iTerm2.app")

(load! "+functions")
(load! "+theming")                      ; defines datetime and theming info
(load! "+typesetting")                  ; fonts and ligatures
(load! "+bindings")
(load! "+org")

(setq default-directory "/Users/johngargalionis/")

;; remove line numbers
(setq display-line-numbers-type nil)

;; dired
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Python
(setq python-shell-interpreter *python*)
(setq python-shell-prompt-detect-failure-warning nil)
(custom-set-variables
 '(flycheck-python-flake8-executable *python*)
 '(flycheck-python-pycompile-executable *python*)
 '(flycheck-python-pylint-executable *python*)
 '(pyimport-pyflakes-path "/usr/local/bin/pyflakes"))
(setq blacken-allow-py36 1)
(add-hook
 'python-mode-hook
 (lambda ()
   (anaconda-mode)
   (anaconda-eldoc-mode)
   (importmagic-mode)
   ;; (python-docstring-mode)
   ;; (add-hook 'before-save-hook 'pyimport-remove-unused)
   ;; (add-hook 'before-save-hook 'importmagic-fix-imports)
   ;; (add-hook 'before-save-hook 'py-isort-before-save)
   (add-hook 'before-save-hook 'blacken-buffer)
   (set (make-local-variable 'compile-command)
        (concat *python* " " (buffer-name)))))

;; Mathematica (Wolfram Language)
(autoload 'wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)
(add-to-mode 'wolfram-mode '("\.m$" "\.wl$" "\.fr$" "\.mod$" "\.wls$"))
(setq wolfram-program "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
(setq wolfram-path "/Applications/Mathematica.app/Contents/AddOns/Applications")
(setq wolfram-indent 2)

;; spelling
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "en_GB")

;; work around for slow ivy switch buffer
;; place in ~/.doom.d/config.el
(remove-hook 'ivy-mode-hook #'ivy-rich-mode)

;; exec-path-from-shell config
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; clojure
(setq cider-show-error-buffer 'only-in-repl)

;; langtool
(setq langtool-bin "/usr/local/bin/languagetool")
(setq langtool-default-language "en-GB")

;; latex
(setq +format-on-save-enabled-modes
      (append +format-on-save-enabled-modes '(latex-mode)))

;; scheme and racket
(setq geiser-chez-binary "/usr/local/bin/chez")
(setq geiser-racket-binary "/usr/local/bin/racket")
(setq geiser-active-implementations '(chez))
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-hook
 'scheme-mode-hook
 'geiser-mode)
(setq geiser-mode-start-repl-p t)

;; change doom splash
;; https://github.com/zaiste/.doom.d/issues/1
(setq +doom-dashboard-banner-file "~/.doom.d/logo.png")

;; denote
(setq denote-directory (expand-file-name "~/Documents/notes/"))
(setq denote-known-keywords
      '("one-loop-b-anomalies" "jets" "bviolation" "ps-proton-decay" "uv-models"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
