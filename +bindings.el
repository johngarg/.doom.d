;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map! :desc "Toggle emacs theme"
      :leader :prefix "t"
      "t" (lambda! (toggle-doom-theme *themes*)))

;; Emacs muscle memory functions
(map!
 :nvi "C-n" 'evil-next-line
 :nvi "C-p" 'evil-previous-line
 :nvi "C-f" 'forward-char
 :nvi "C-b" 'backward-char
 :nvi "M-S-y" 'evil-past-pop-next)

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

;; org insert items and headings
(map! :map org-mode-map
      :localleader
      :n "i" #'org-insert-item
      :n "h" #'org-insert-heading)

(map! :desc "Toggle git gutter"
      :leader :prefix "t"
      "r" 'git-gutter:toggle)
