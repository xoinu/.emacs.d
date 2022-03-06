;; -*- no-byte-compile: t -*-
;;-----------------------------------------------------------------------------
;; Recommended packages
;; - auto-complete
;; - go-mode
;; - lsp-mode
;; - lsp-ui
;; - php-mode
;; - powershell
;; - yaml-mode
;;-----------------------------------------------------------------------------
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;-----------------------------------------------------------------------------
;; General settings
;;-----------------------------------------------------------------------------
(setq-default cursor-type '(bar . 2)
              indent-tabs-mode nil
              indicate-empty-lines t
              make-backup-files nil
              scroll-step 1
              show-trailing-whitespace t
              tab-width 4
              time-stamp-active t
              truncate-lines t
              vc-make-backup-files nil
              visible-bell t)

(line-number-mode t)
(column-number-mode t)

;; Disable show-trailing-whitespace as it is found useless
(let ((dont-show-trailing-whitespace (lambda () (setq show-trailing-whitespace nil))))
  (add-hook 'calendar-mode-hook dont-show-trailing-whitespace)
  (add-hook 'buffer-menu-mode-hook dont-show-trailing-whitespace))

(use-package auto-complete
  :config
  (ac-config-default)
  (global-auto-complete-mode))



(when (require 'lsp-mode)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook #'lsp-format-buffer t t)
                            (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(if (fboundp #'time-stamp)
    (progn
      (add-hook 'write-file-hooks #'time-stamp)
      (setq-default time-stamp-active t)))

;;-----------------------------------------------------------------------------
;; Window system specific settings
;;-----------------------------------------------------------------------------
(when window-system
  (global-set-key (kbd "C-z") 'undo)
  (setq initial-frame-alist '((top . 0) (left . 0) (width . 164) (height . 56)))
  (set-background-color "#1E1E1E")
  (set-foreground-color "#D4D4D4")
  (set-cursor-color "white")
  ;; NTEmacs
  (when (eq window-system 'w32)
    (add-to-list 'default-frame-alist
                 '(font . "Consolas-11"))))
;; '(font . "Cascadia Code-10"))))
;;-----------------------------------------------------------------------------
;; org-mode
;;-----------------------------------------------------------------------------
(setq org-agenda-files '("~/todo.org"))
(setq org-log-done 'time)
(global-set-key (kbd "C-c a") 'org-agenda)

;;-----------------------------------------------------------------------------
;; C/C++
;;-----------------------------------------------------------------------------
(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode   "cc-mode" "C Editing Mode" t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "K&R")
            (setq indent-tabs-mode nil
                  c-basic-offset tab-width)
            (c-set-offset 'arglist-intro tab-width)
            (c-set-offset 'arglist-close 0)
            (c-set-offset 'inline-open 0)
            (c-set-offset 'member-init-intro tab-width)
            (c-set-offset 'member-init-cont -2)
            (c-set-offset 'statement-cont tab-width)
            (c-set-offset 'topmost-intro 0)
            (c-set-offset 'topmost-intro-cont 0)
            (c-set-offset 'innamespace tab-width)))

(add-to-list 'auto-mode-alist '("\\.\\(C\\|c\\|cc\\|cpp\\|cxx\\|e\\|h\\|hh\\|hpp\\|hxx\\)$" . c++-mode))
(defun etolisp-c++-mode ()
  "C++ mode with adjusted defaults for use with the EtoLisp development."
  (interactive)
  (c++-mode)
  (let ((etolisp-indent-level 2))
    (setq indent-tabs-mode nil
          tab-width etolisp-indent-level
          c-indent-level etolisp-indent-level
          c-basic-offset etolisp-indent-level)
    (c-set-offset 'innamespace 0)
    (c-set-offset 'arglist-intro etolisp-indent-level)
    (c-set-offset 'arglist-close 0)
    (c-set-offset 'inline-open 0)
    (c-set-offset 'statement-cont 0)
    (c-set-offset 'topmost-intro 0)
    (c-set-offset 'topmost-intro-cont 0)
    (c-set-offset 'innamespace etolisp-indent-level)))

;;-----------------------------------------------------------------------------
;; Shell Script
;;-----------------------------------------------------------------------------
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))
(add-hook 'go-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode t)
            (setq-local tab-width 4)))

;;-----------------------------------------------------------------------------
;; CSS
;;-----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.css\\'" . scss-mode))
(add-hook 'scss-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

;;-----------------------------------------------------------------------------
;; JavaScript
;;-----------------------------------------------------------------------------
(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))

;;-----------------------------------------------------------------------------
;; XML
;;-----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|cfx\\|cdx\\)$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\(qax\\|enx\\)$" . xml-mode))

;;-----------------------------------------------------------------------------
;; Ruby
;;-----------------------------------------------------------------------------
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|rbw\\|gemspec\\)$" . ruby-mode))

;;----------------------------------------------------------------------------
;; M-x list-faces-display
;;----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(use-package lsp-mode typescript-mode powershell scss-mode php-mode go-mode yaml-mode auto-complete))
 '(safe-local-variable-values '((encoding . utf-8)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(FONT-lock-type-face ((t (:foreground "orchid"))))
 '(bold ((t nil)))
 '(bold-italic ((t nil)))
 '(buffer-menu-buffer ((t nil)))
 '(comint-highlight-input ((t nil)))
 '(compilation-error ((t (:inherit error :weight normal))))
 '(compilation-info ((((class color) (min-colors 88) (background dark)) (:foreground "YellowGreen"))))
 '(compilation-warning ((((class color) (min-colors 16)) (:foreground "Orange"))))
 '(cursor ((t (:background "PaleTurquoise3"))))
 '(custom-group-tag ((((class color) (background dark)) (:foreground "SteelBlue" :height 1.2))))
 '(custom-variable-button ((t (:underline t))))
 '(error ((t (:foreground "Pink"))))
 '(font-lock-comment-face ((t (:foreground "OliveDrab1"))))
 '(font-lock-keyword-face ((t (:foreground "#6495ED"))))
 '(font-lock-preprocessor-face ((t (:foreground "#FFA07A"))))
 '(font-lock-string-face ((t (:foreground "#8B7E66"))))
 '(font-lock-type-face ((t (:foreground "violet"))))
 '(font-lock-variable-name-face ((t nil)))
 '(italic ((((supports :slant italic)) nil)))
 '(mode-line ((t (:background "gray25" :foreground "gray70"))))
 '(mode-line-buffer-id ((t nil)))
 '(mode-line-inactive ((t (:background "gray15" :foreground "gray25"))))
 '(region ((t (:background "midnight blue"))))
 '(warning ((t (:foreground "DarkOrange"))))
 '(widget-button ((t nil))))
