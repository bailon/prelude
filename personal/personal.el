;;; personal.el --- Personal configurations for Emacs Prelude
;;
;; Author: Bruno Merino-Bailón <merino.bailon@gmail.com>
;;

;;; Commentary:
;;
;; A lot of small tweaks to make my experience on Emacs more pleasant.
;; I'm still getting the gist of using Emacs and learning the basics
;; of Lisp. I'm still doing a lot of copy/paste of functions I found
;; on the internet.
;;

;; Magit -------------------------------------------------------------
(setq magit-last-seen-setup-instructions "1.4.0")


;; Personal Information ----------------------------------------------
(setq user-mail-address "merino.bailon@gmail.com")
(setq user-full-name "Bruno Merino-Bailón")


;; UI Tweaks ---------------------------------------------------------
(scroll-bar-mode -1)
(global-hl-line-mode -1)
(load-theme 'wombat)



;; Hack for setting a fixed wrap column in visual-line-mode.
;; Source: ohai-emacs
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


;; disable flycheck on org-mode --------------------------------------
;;(setq-default flycheck-disabled-checkers
;;              (append flycheck-disabled-checkers 'org-mode))




;; line numbers ------------------------------------------------------
;; (require 'linum)
;; (global-linum-mode 1)
;; (setq linum-format " %3d ")


;; fullscreen --------------------------------------------------------
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))



;; org-bullets -------------------------------------------------------
(prelude-require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; snippets ----------------------------------------------------------
(prelude-require-package 'yasnippet)
(yas-global-mode 1)


;; date translation --------------------------------------------------
(setq system-time-locale (getenv "LANG"))


;; org-babel ---------------------------------------------------------
(custom-set-variables
 '(org-babel-load-languages (quote ((emacs-lisp . t)
                                    (python . t)
                                    (clojure . t)
                                    (sql . t)
                                    (sh . t))))
 '(org-confirm-babel-evaluate nil))


;; org-mode - code block color ---------------------------------------
(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#FFFFFF" :background "#1b1b1b")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#666666")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#FFFFFF" :background "#1b1b1b")))
  "Face used for the line delimiting the end of source blocks.")


;; technical artifacts -----------------------------------------------
(provide 'personal)
