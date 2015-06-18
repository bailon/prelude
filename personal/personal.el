;; i've read magit instructions...
(setq magit-last-seen-setup-instructions "1.4.0")


;; personal information
(setq user-mail-address "merino.bailon@gmail.com")
(setq user-full-name "Bruno Merino-Bailón")


;; small tweaks on the ui
(scroll-bar-mode -1)
(global-hl-line-mode -1)
(load-theme 'wombat)



;; Hack for setting a fixed wrap column in visual-line-mode.
;; Copied from ohai-emacs
(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  if (not visual-wrap-column)
  (set-window-margins nil nil))

;; ...and make it available for org-mode
(add-hook
 'org-mode-hook
 (lambda ()
   (visual-line-mode 1)
   (whitespace-mode -1)
;;   (set-visual-wrap-column 80)
   ))


;; disable flycheck on org-mode
;;(setq-default flycheck-disabled-checkers
;;              (append flycheck-disabled-checkers 'org-mode))




;; line numbers
;; (require 'linum)
;; (global-linum-mode 1)
;; (setq linum-format " %3d ")


;; start in fullscreen
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))



;; org-bullets
(prelude-require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; snippets
(prelude-require-package 'yasnippet)
(yas-global-mode 1)


;; date translation name
(setq system-time-locale (getenv "LANG"))


(provide 'personal)
