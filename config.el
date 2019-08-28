;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; constants
(defconst *python* "python3")
(defconst *terminal* "~/Applications/iTerm2.app")

(load! "+functions")
(load! "+theming")                      ; defines datetime and theming info
(load! "+typesetting")                  ; fonts and ligatures
(load! "+bindings")
(load! "+org")

;; Python
(setq python-shell-interpreter *python*)
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
   (add-hook 'before-save-hook 'py-isort-before-save)
   (add-hook 'before-save-hook 'blacken-buffer)
   (set (make-local-variable 'compile-command)
        (concat *python* " " (buffer-name)))))

;; Mathematica (Wolfram Language)
(autoload 'wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)
(add-to-mode 'wolfram-mode '("\.m$" "\.wl$" "\.fr$" "\.mod$"))
(setq wolfram-program "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
(setq wolfram-path "/Applications/Mathematica.app/Contents/AddOns/Applications")
(setq wolfram-indent 2)

(setq ispell-program-name "/usr/local/bin/aspell")

;; work around for slow ivy switch buffer
;; place in ~/.doom.d/config.el
(remove-hook 'ivy-mode-hook #'ivy-rich-mode)

;; disable smart-parens in org mode (fixes slow delete char and insert *)
(add-hook 'org-mode-hook #'turn-off-smartparens-mode)

;; exec-path-from-shell config
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; clojure
(setq cider-show-error-buffer 'only-in-repl)
