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
  (org-gcal-fetch)
  (org-agenda-list 1 (org-today))
  (org-agenda-write path))
