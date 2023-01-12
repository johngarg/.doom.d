;;; ~/.doom.d/+functions.el -*- lexical-binding: t; -*-

(defun assoc-complement (key alist)
  "Return the complement of an association list"
  (if (string= (caar alist) key)
      (cdr alist)
    (cons (car alist) (assoc-complement key (cdr alist)))))

(defun toggle-doom-theme (themes)
  "Toggles between light/dark themes."
  (interactive)
  (let ((theme-pair (rassoc doom-theme themes)))
    ;; reload timed theme in case changed by user
    (when theme-pair (doom/reload-theme))
    (setq complement-theme
          (assoc-complement
           (car theme-pair)
           themes)))
  (setq doom-theme (cdar complement-theme))
  (doom/reload-theme))

(defun add-to-mode (mode lst)
  "Add multiple file extensions to auto-mode-alist at once."
  (dolist (file lst)
    (add-to-list 'auto-mode-alist
                 (cons file mode))))

(defun open-dir-in-term (terminal)
  "Open the current directory of the buffer in external terminal
   app."
  (interactive)
  (let* ((iterm-app-path terminal))
    (shell-command (concat "open -a " iterm-app-path " ."))))

(defun today ()
  "Return string of today's date nicely formatted."
  (interactive)
  (format-time-string "%Y-%m-%d"))

(defun insert-todays-date ()
  "Insert string for today's date nicely formatted."
  (interactive)
  (insert (today)))

(defun ipython-send-and-eval-buffer ()
  "Opens an ipython repl and sends contents of current buffer."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (+python/open-ipython-repl)
    (python-shell-send-file file)))

(defun my-org-export-agenda (path)
  "Writes agenda to path"
  ;; (load-file "~/.doom.d/+org.el") ; update org-gcal first
  ;; (org-gcal-fetch)
  (org-agenda-list 1 (org-today))
  (org-agenda-write path))

(defun my-export-to-latex-and-make ()
  (interactive)
  "Exports org file to latex and calls make"
  (org-latex-export-to-latex)
  (+ivy/compile))

(defun my-org-scale-latex-fragments (n)
  (interactive "n")
  (setq
   org-format-latex-options
   (plist-put org-format-latex-options :scale n)))

(defun my-force-kill-buffer ()
  (interactive)
  (with-current-buffer (read-string "Buffer to kill: ")
    (let (kill-buffer-hook kill-buffer-query-functions)
      (kill-buffer))))

(defun clerk-show ()
  (interactive)
  (save-buffer)
  (let
      ((filename
        (buffer-file-name)))
    (when filename
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")")))))

(defun clerk-browse ()
  (interactive)
  (save-buffer)
  (let
      ((filename
        (buffer-file-name)))
    (when filename
      (cider-interactive-eval
       (concat "(nextjournal.clerk/serve! {:browse? true})")))))

(defun show-buffer-file-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun +notmuch/search-archive ()
  (interactive)
  (notmuch-search-add-tag '("+archive" "-inbox" "-unread"))
  (notmuch-tree-next-message))

(defun +notmuch/search-flag ()
  (interactive)
  (notmuch-search-add-tag '("+flagged"))
  (notmuch-tree-next-message))

(defun +notmuch/search-unflag ()
  (interactive)
  (notmuch-search-remove-tag '("-flagged"))
  (notmuch-tree-next-message))

(defun safe-mail-send-and-exit ()
  (interactive)
  (message-mode)
  (if (string-equal (read-from-minibuffer "Really send? ") "yes")
      (notmuch-mua-send-and-exit)
    (message "Not sent!")))

(defun toggle-dired-denote-fontification ()
  (interactive)
  (diredfl-mode -1)
  (denote-dired-mode 1))
