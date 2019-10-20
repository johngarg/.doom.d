;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map! :desc "Toggle emacs theme"
      :leader :prefix "t"
      "t" (lambda! (toggle-doom-theme *themes*)))

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

(map! :map my-zen-map
      "z" 'writeroom-mode
      "h" #'writeroom-increase-width
      "l" #'writeroom-decrease-width
      "c" 'centered-cursor-mode
      "f" 'focus-mode)

(map! :leader "v" 'er/expand-region)
(map! :leader :prefix "o"
      "t" (lambda! (open-dir-in-term *terminal*)))
(map! :leader :prefix "/"
      "c" 'evil-ex-nohighlight)
(map! :leader :prefix "/"
      "w" 'wordnut-lookup-current-word)

(map! :map python-mode-map
      :localleader
      :n "r" #'+python/open-ipython-repl
      :n "R" #'ipython-send-and-eval-buffer)

;; org insert items and headings
(map! :map org-mode-map
      :localleader
      :n "i" #'org-insert-item
      :n "h" #'org-insert-heading)

(map! :desc "Toggle git gutter"
      :leader :prefix "t"
      "r" 'git-gutter:toggle)
