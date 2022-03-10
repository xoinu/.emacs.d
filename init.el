;; -*- no-byte-compile: t -*-
;;-----------------------------------------------------------------------------
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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
              truncate-lines t
              vc-make-backup-files nil
              visible-bell t)

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(line-number-mode t)
(column-number-mode t)

;;
;; Disable show-trailing-whitespace as it is found useless
;;
(let ((dont-show-trailing-whitespace (lambda () (setq show-trailing-whitespace nil))))
  (add-hook 'calendar-mode-hook dont-show-trailing-whitespace)
  (add-hook 'buffer-menu-mode-hook dont-show-trailing-whitespace))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (global-auto-complete-mode))
;; (use-package company
;;   :ensure t
;;   :config
;;   (global-company-mode))

(use-package lsp-mode
  :ensure t
  :custom (lsp-rust-server 'rls)
  :hook
  (rust-mode . lsp)
  (rust-mode . (lambda () (lsp-format-buffer)))
  (go-mode . lsp-deferred)
  (go-mode . (lambda ()
               (add-hook 'before-save-hook #'lsp-format-buffer t t)
               (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(use-package lsp-ui :ensure t)
(use-package php-mode :ensure t)
(use-package powershell :ensure t)
(use-package yaml-mode :ensure t)
(use-package go-mode
  :ensure t
  :init
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(use-package rust-mode
  :ensure t
  :custom rust-format-on-save t)

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package scss-mode
  :ensure t
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2))

(use-package sh-script
  :config
  (setq sh-basic-offset 2
        sh-indentation 2))

(use-package electric
  :hook
  (js-mode . electric-indent-mode)
  (js-mode . electric-layout-mode))

(use-package js
  :custom (js-indent-level 2))

(use-package cc-mode
  :mode "\\.\\(C\\|c\\|cc\\|cpp\\|cxx\\|e\\|h\\|hh\\|hpp\\|hxx\\)\\'"
  :hook
  (c++-mode . cc-mode)
  (c++-mode . (lambda ()
  (c-set-style "K&R")
  (c-set-offset 'arglist-intro tab-width)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'member-init-intro tab-width)
  (c-set-offset 'member-init-cont -2)
  (c-set-offset 'statement-cont tab-width)
  (c-set-offset 'topmost-intro 0)
  (c-set-offset 'topmost-intro-cont 0)
  (c-set-offset 'innamespace tab-width)))
  :config
  (setq indent-tabs-mode nil
        c-basic-offset tab-width))

(use-package ruby-mode
  :mode "\\.\\(rb\\|rbw\\|gemspec\\)\\'"
  :interpreter "ruby")

(use-package loaddefs
  :mode
  ("\\.\\(xml\\|cfx\\|cdx\\)\\'" . xml-mode)
  ("\\(qax\\|enx\\)\\'" . xml-mode))

(use-package org-agenda
  :config
  (setq org-agenda-files '("~/todo.org"))
  (setq org-log-done 'time)
  (global-set-key (kbd "C-c a") 'org-agenda))

;;-----------------------------------------------------------------------------
;; Window system specific settings
;;-----------------------------------------------------------------------------
(when window-system
  (global-set-key (kbd "C-z") 'undo)
  (when (= (x-display-pixel-height) 1080)
    (setq initial-frame-alist '((top . 0) (left . 0) (width . 164) (height . 51))))
  (set-background-color "#1E1E1E")
  (set-foreground-color "#D4D4D4")
  ;; NTEmacs
  (when (eq window-system 'w32)
    (add-to-list 'default-frame-alist
                 ;;'(font . "Consolas-11"))))
                 '(font . "Cascadia Code SemiLight-11"))))

;;-----------------------------------------------------------------------------
;; C/C++
;;-----------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------
;; M-x list-faces-display
;;----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode 'dark)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(auto-complete lsp-ui cargo rust-mode use-package lsp-mode typescript-mode powershell scss-mode php-mode go-mode yaml-mode))
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
 '(fixed-pitch ((t (:family "Consolas"))))
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
