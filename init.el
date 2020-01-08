;; -*- no-byte-compile: t -*-
;;-----------------------------------------------------------------------------
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;-----------------------------------------------------------------------------
(package-initialize)

;;-----------------------------------------------------------------------------
;; Recommended packages
;; - auto-complete
;; - csharp-mode
;; - go-mode
;; - php-mode
;; - yaml-mode
;;-----------------------------------------------------------------------------
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;-----------------------------------------------------------------------------
;; Load path
;;-----------------------------------------------------------------------------
(let ((my-site-lisp (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path my-site-lisp)
  (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (cd my-site-lisp)
    (normal-top-level-add-subdirs-to-load-path)
    (cd "~")
    (byte-recompile-directory my-site-lisp)))

;;-----------------------------------------------------------------------------
;; General settings
;;-----------------------------------------------------------------------------
(setq-default make-backup-files nil)
(setq-default vc-make-backup-files nil)
(setq-default visible-bell t)
(setq-default scroll-step 1)
(setq-default truncate-lines t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(line-number-mode 1)
(column-number-mode 1)

(when (require 'time-stamp nil t)
  (add-hook 'write-file-hooks 'time-stamp)
  (setq-default time-stamp-active t))

(if (fboundp 'ac-config-default) (ac-config-default))

;;-----------------------------------------------------------------------------
;; Window system specific settings
;;-----------------------------------------------------------------------------
(when window-system
  (setq initial-frame-alist
        '((top . 0) (left . 0) (width . 164) (height . 55)))
  (set-background-color "#1E1E1E")
  (set-foreground-color "#D4D4D4")
  (set-cursor-color "white")
  ;; NTEmacs
  (when (eq window-system 'w32)
    (add-to-list 'default-frame-alist
                 '(font . "Consolas-11"))))

;;-----------------------------------------------------------------------------
;; C/C++
;;-----------------------------------------------------------------------------
(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode   "cc-mode" "C Editing Mode" t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "K&R")
            (setq indent-tabs-mode nil)
            (setq c-basic-offset tab-width)
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
    (c-set-offset 'arglist-intro 0)
    (c-set-offset 'arglist-close 0)
    (c-set-offset 'inline-open 0)
    (c-set-offset 'statement-cont 0)
    (c-set-offset 'topmost-intro 0)
    (c-set-offset 'topmost-intro-cont etolisp-indent-level)
    (c-set-offset 'innamespace etolisp-indent-level)))

;;-----------------------------------------------------------------------------
;; Shell Script
;;-----------------------------------------------------------------------------
(add-hook 'sh-mode-hook
          (lambda () (setq sh-basic-offset 2 sh-indentation 2)))

;;-----------------------------------------------------------------------------
;; C#
;;-----------------------------------------------------------------------------
(autoload 'csharp-mode "csharp-mode")
(add-hook 'css-mode-hook
          (lambda ()
                                        ;(electric-pair-local-mode 1)
            ))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;;-----------------------------------------------------------------------------
;; CSS
;;-----------------------------------------------------------------------------
(autoload 'css-mode "css-mode")
(add-hook 'css-mode-hook
          (lambda ()
            (setq cssm-indent-level 2)
            (setq cssm-indent-function #'cssm-c-style-indenter)))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;;-----------------------------------------------------------------------------
;; PHP
;;-----------------------------------------------------------------------------
(autoload 'php-mode "php-mode" "Mode for editing PHP source files" t)
(add-hook 'php-mode-user-hook 'turn-on-font-lock)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;;-----------------------------------------------------------------------------
;; JavaScript
;;-----------------------------------------------------------------------------
(autoload 'javascript-mode "JavaScript mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))

;;-----------------------------------------------------------------------------
;; YAML
;;-----------------------------------------------------------------------------
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;-----------------------------------------------------------------------------
;; XML
;;-----------------------------------------------------------------------------
(autoload 'xml-mode "xml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|cfx\\|cdx\\)$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\(cdx\\|cfx\\|qax\\|enx\\)$" . xml-mode))

;;-----------------------------------------------------------------------------
;; Ruby
;;-----------------------------------------------------------------------------
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|rbw\\|gemspec\\)$" . ruby-mode))

;;-----------------------------------------------------------------------------
;; Go
;;-----------------------------------------------------------------------------
(autoload 'go-mode "go-mode" "Mode for editing Go source files" t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

;;----------------------------------------------------------------------------
;; Ispell
;;----------------------------------------------------------------------------
;; (eval-after-load "ispell"
;;   '(setq ispell-skip-region-alist (cons '("[^\000-\377]")
;;                                         ispell-skip-region-alist)))
;; (setq ispell-dictionary "US-xlg")
;; (setq ispell-local-dictionary-alist
;;       '((nil                            ; default (english.aff)
;;          "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)
;;         ("UK-xlg"                       ; english large version
;;          "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B" "-d" "UK-xlg") nil iso-8859-1)
;;         ("US-xlg"                       ; american large version
;;          "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B" "-d" "US-xlg") nil iso-8859-1)
;;         )
;;       )

;;----------------------------------------------------------------------------
;; M-x list-faces-display
;;----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (php-mode go-mode csharp-mode yaml-mode auto-complete)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
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
 '(compilation-info ((((class color) (min-colors 88) (background dark)) (:foreground "GreenYellow"))))
 '(compilation-warning ((((class color) (min-colors 16)) (:foreground "Orange"))))
 '(custom-group-tag ((((class color) (background dark)) (:foreground "SteelBlue" :height 1.2))))
 '(custom-variable-button ((t (:underline t))))
 '(error ((t (:foreground "Pink"))))
 '(font-lock-comment-face ((t (:foreground "lawn green"))))
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
