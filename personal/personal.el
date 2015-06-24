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


;; Keybindings -------------------------------------------------------
(global-set-key (kbd "C-x w") 'ace-window)


;; UI Tweaks ---------------------------------------------------------
(scroll-bar-mode -1)
(global-hl-line-mode -1)
(load-theme 'wombat)


;; Org-Mode ----------------------------------------------------------
(prelude-require-package 'org-bullets)

(add-hook
 'org-mode-hook
 (lambda ()
   (visual-line-mode 1)
   (whitespace-mode -1)
   (turn-on-auto-fill)
   (set-fill-column 80)
   (org-bullets-mode 1)
   ))


;; Line Numbers ------------------------------------------------------
;; (require 'linum)
;; (global-linum-mode 1)
;; (setq linum-format " %3d ")



;; Full Screen -------------------------------------------------------
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))



;; Snippets ----------------------------------------------------------
(prelude-require-package 'yasnippet)
(yas-global-mode 1)


;; Date Translation --------------------------------------------------
(setq system-time-locale (getenv "LANG"))


;; Org-Babel ---------------------------------------------------------
(custom-set-variables
 '(org-babel-load-languages (quote ((emacs-lisp . t)
                                    (python . t)
                                    (clojure . t)
                                    (sql . t)
                                    (sh . t))))
 '(org-confirm-babel-evaluate nil))


;; Org-Mode - Code block color ---------------------------------------
(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#FFFFFF" :background "#1b1b1b")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#666666")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#FFFFFF" :background "#1b1b1b")))
  "Face used for the line delimiting the end of source blocks.")


;; Technical Artifacts -----------------------------------------------
(provide 'personal)
