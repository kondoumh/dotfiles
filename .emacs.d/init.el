;; add load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf")

(setq byte-compile-warnings '(not free-vars))

;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; disable startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; disable toolbar
(if (display-graphic-p)
  (tool-bar-mode 0))

;; show file path on title 
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; scroll-bar-mode
(if (display-graphic-p)
  (scroll-bar-mode -1))

;; share clipboard
(setq x-select-enable-clipboard t)

;;dired
(use-package ffap)
(ffap-bindings)

;; key bindings
;;; switch buffer 
(windmove-default-keybindings 'super)
;;; cursor scroll
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 1)))
;;; C-h to Backspace
(global-set-key "\C-h" 'delete-backward-char)

(add-hook 'term-setup-hook
  '(lambda ()
    (define-key function-key-map "\e[1;9A" [M-up])
    (define-key function-key-map "\e[1;9B" [M-down])
    (define-key function-key-map "\e[1;9C" [M-right])
    (define-key function-key-map "\e[1;9D" [M-left])))

;; language
(set-language-environment 'Japanese)

;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; sytem-time-locale
(setq system-time-locale "C")

;; file name encoding
(use-package ucs-normalize)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; path environments
(dolist
  (dir
    (list
      "/sbin"
      "/usr/sbin"
      "/bin"
      "/usr/bin"
      "/usr/local/bin"
       (expand-file-name "~/bin")
       (expand-file-name "~/.emacs.d/bin")
    )
  )
  (when (and (file-exists-p dir) (not (member dir exec-path)))
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (setq exec-path (append (list dir) exec-path))))

;; current dir
(cd "~/")

;; no backup files
(setq make-backup-files nil)

;; emphasize between brackets
(show-paren-mode 1)
(setq show-paren-delay 0)

;; color of selected
(set-face-background 'region "#555")

;; auto indent on newline
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

;; no confirmation on kill buffers
(global-set-key [(control x) (k)] 'kill-this-buffer)

;; recentf-ext
(use-package recentf-ext)
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
(recentf-mode 1)
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 20)
;;; recent files
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;;; show recent files list on startup
(add-hook 'after-init-hook (lambda()
  (recentf-open-files)
))

;; auto-save-buffers
(use-package auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1)
(auto-save-buffers-enhanced t)

;; default-tab-width
(setq default-tab-width 4)

;; company
(use-package company)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; org-mode
(setq org-src-fontify-natively t)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(setq org-todo-keywords '((type "TODO(t)" "DOING(i)" "|" "DONE(d)" "SOMEDAY(s)" "WON'T(w)")))

;; rust-mode
(use-package rust-mode)
(add-to-list 'exec-path(expand-file-name "~/.cargo/bin/"))
(add-to-list 'auto-mode-alist '("\\.rs\\'"  . rust-mode))
(add-hook 'rust-mode-hook (lambda()
			    (racer-mode)))
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook (lambda()
			     (company-mode)
			     (set (make-variable-buffer-local 'company-idle-delay) 0.1)
			     (set (make-variable-buffer-local 'company-minimum-prefix-length) 0)))

;; dockerfile-mode
(use-package dockerfile-mode)

;; backslash
(define-key global-map [?\M-¥] "\\")

;; autopair
(use-package autopair)
(autopair-global-mode)

(ivy-mode 1)
(counsel-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dockerfile-mode yaml-mode racer company use-package lsp-mode rustic ivy swiper go-mode rust-mode markdown-mode auto-save-buffers-enhanced counsel web-mode typescript-mode recentf-ext org autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

