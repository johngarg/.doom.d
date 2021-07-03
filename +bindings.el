;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map! :desc "Toggle emacs theme"
      :leader :prefix "t"
      "t" (cmd! (toggle-doom-theme *themes*)))

;; Emacs muscle memory functions for insert mode
(map!
 :i "C-n" 'evil-next-line
 :i "C-p" 'evil-previous-line
 :i "C-f" 'forward-char
 :i "C-b" 'backward-char)

;; distraction-free editing
(defvar my-zen-map (make-sparse-keymap)
  "Keymap for zen-related commands.")

(map! :desc "zen"
      :leader
      :n "z" my-zen-map)

(defvar my-lisp-map (make-sparse-keymap)
  "Keymap for personalised lisp-related commands.")

(map! :desc "lisp"
      :leader
      :n "k" my-lisp-map)

(map! :map my-zen-map
      "z" 'writeroom-mode
      "h" #'writeroom-increase-width
      "l" #'writeroom-decrease-width
      "c" 'centered-cursor-mode
      "f" 'focus-mode)

(map! :leader "v" 'er/expand-region)

;; my open commands
(map! :leader :prefix "o"
      "i" (cmd! (open-dir-in-term *terminal*)))
(map! :leader :prefix "o"
      "c" (cmd! (cfw:open-org-calendar)))

(map! :leader "/" nil)
(map! :leader :prefix "/" "c" 'evil-ex-nohighlight)

(map! :map python-mode-map
      :localleader
      :n "r" #'+python/open-ipython-repl
      :n "R" #'ipython-send-and-eval-buffer)

(map! :map racket-repl-mode-map
      :n "C-w" nil
      "C-w" nil)

(map! :map my-lisp-map
      :leader :prefix "k"
      "h" #'beginning-of-defun
      "k" (cmd! (sp-beginning-of-previous-sexp) (evil-backward-char))
      "j" #'sp-next-sexp
      "e" (cmd!
           (evil-jump-item)
           (evil-append 1)
           (cond
            ((eq major-mode 'emacs-lisp-mode) (eval-last-sexp nil))
            ((eq major-mode 'racket-mode) (geiser-eval-last-sexp nil))
            ((eq major-mode 'clojure-mode) (geiser-eval-last-sexp nil)))
           (evil-normal-state)
           (evil-jump-item))
      "b" #'sp-forward-barf-sexp
      "B" #'sp-backward-barf-sexp
      "s" #'sp-forward-slurp-sexp
      "S" #'sp-backward-slurp-sexp
      "J" #'sp-join-sexp
      "u" #'sp-backward-unwrap-sexp
      "w" #'sp-wrap-round)

;; org insert items and headings
(map! :map org-mode-map
      :localleader
      :n "i" #'org-insert-item
      :n "h" #'org-insert-heading
      :prefix "L"
      :n "e" #'LaTeX-environment
      :n "m" #'my-export-to-latex-and-make
      :n "i" #'org-ref-helm-insert-cite-link
      :n "r" #'org-ref-helm-insert-ref-link)

(map! :map org-mode-map
      :leader :prefix "t"
      :n "e" #'my-org-toggle-emphasis)

(map! :map LaTeX-mode-map
      :localleader
      :n "c" #'org-latex-preview
      :n "h" #'latex/hide-show)

(map! :desc "Toggle git gutter"
      :leader :prefix "t"
      "r" 'git-gutter:toggle)

(map! :map cfw:calendar-mode-map
      "a" #'cfw:org-open-agenda-day
      "d" #'cfw:change-view-two-weeks)

(map! :map haskell-mode-map
      :localleader
      :n "r" #'+haskell/open-repl)

(map! :map haskell-mode-map
      :localleader
      :n "r" #'+haskell/open-repl)

(map! :map haskell-interactive-mode-map
      :nvi "C-c C-r" #'haskell-process-restart)
