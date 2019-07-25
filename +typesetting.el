;;; ~/.doom.d/+typesetting.el -*- lexical-binding: t; -*-

(setq-default line-spacing 5)
(setq-default display-line-numbers-type 'relative)
(defvar *ligaturesp* nil)
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



;; Fira code ligatures
(when (and *ligaturesp* (string= *font* "Fira Code"))
  (defun setup-firacode-ligatures ()
    "Solutions from https://github.com/tonsky/FiraCode/wiki/Emacs-instructions"
    (defun fira-code-mode--make-alist (list)
      "Generate prettify-symbols alist from LIST."
      (let ((idx -1))
        (mapcar
         (lambda (s)
           (setq idx (1+ idx))
           (let* ((code (+ #Xe100 idx))
                  (width (string-width s))
                  (prefix ())
                  (suffix '(?\s (Br . Br)))
                  (n 1))
             (while (< n width)
               (setq prefix (append prefix '(?\s (Br . Bl))))
               (setq n (1+ n)))
             (cons s (append prefix suffix (list (decode-char 'ucs code))))))
         list)))
    (defconst fira-code-mode--ligatures
      '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
        "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
        "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
        "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
        ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
        "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
        "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
        "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
        ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
        "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
        "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
        "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
        "x" ":" "+" "+" "*"))
    (defvar fira-code-mode--old-prettify-alist)
    (defun fira-code-mode--enable ()
      "Enable Fira Code ligatures in current buffer."
      (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
      (setq-local prettify-symbols-alist
                  (append
                   (fira-code-mode--make-alist fira-code-mode--ligatures)
                   fira-code-mode--old-prettify-alist))
      (prettify-symbols-mode t))
    (defun fira-code-mode--disable ()
      "Disable Fira Code ligatures in current buffer."
      (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
      (prettify-symbols-mode -1))
    (define-minor-mode fira-code-mode
      "Fira Code ligatures minor mode"
      :lighter " Fira Code"
      (setq-local prettify-symbols-unprettify-at-point 'right-edge)
      (if fira-code-mode
          (fira-code-mode--enable)
        (fira-code-mode--disable)))
    (defun fira-code-mode--setup ()
      "Setup Fira Code Symbols"
      (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))
    (provide 'fira-code-mode))

  (setup-firacode-ligatures)
  (add-hook 'python-mode-hook 'fira-code-mode)
  (add-hook 'clojure-mode-hook 'fira-code-mode)
  (add-hook 'elm-mode-hook 'fira-code-mode)
  (add-hook 'haskell-mode-hook 'fira-code-mode))
