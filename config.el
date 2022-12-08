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
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Email


(after! notmuch

  (setq +notmuch-sync-backend 'offlineimap)
  (setq notmuch-always-prompt-for-sender t)

  (setq mail-specify-envelope-from t)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)

  (setq sendmail-program "/usr/local/bin/msmtp")
  (setq send-mail-function #'sendmail-send-it)

  (setq notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                                 (:name "flagged" :query "tag:flagged" :key "f")
                                 (:name "archive" :query "tag:archive" :key "a")
                                 (:name "sent" :query "tag:sent" :key "s")
                                 (:name "work" :query "tag:work" :key "w")
                                 (:name "meeting" :query "tag:meeting" :key "m")
                                 (:name "trash" :query "tag:trash" :key "t")))

  (setq notmuch-show-log nil
        notmuch-hello-sections `(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags))

  (setq notmuch-fcc-dirs
      '(("johgar@uv.es" . "johgar@uv.es/INBOX.Sent -unread -new +sent +uv +work")
        ("johngarg@ific.uv.es" . "johngarg@ific.uv.es/sent-mail -unread -new +sent +ific +work")
        ("johngargalionis@gmail.com" . "gmail-local-sent -unread -new +sent +gmail")
        (".*" . "fallback-sent"))))

(after! org-mime
  (setq org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil)))

(setq message-signature-file "~/.mailsignature")
(setq message-fill-column nil)

(add-to-list '+doom-dashboard-menu-sections
             '("Open mail"
               :icon (all-the-icons-octicon "mail" :face 'doom-dashboard-menu-title)
               :when (featurep! :email notmuch +afew)
               :face (:inherit (doom-dashboard-menu-title bold))
               :action notmuch-jump-search))
