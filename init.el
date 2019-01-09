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

;; (install-elisp "http://www.emacswiki.org/emacs/download/auto-install.el")
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  ;; proxy settings if needed
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; enable install-elisp functions
  (auto-install-compatibility-setup))

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
(require 'ffap)
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
(setq default-file-name-coding-system 'shift_jis) ;dired文字化け対策

;; sytem-time-locale
(setq system-time-locale "C")

;; file name encoding
(require 'ucs-normalize)
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

;; font settings
(if (display-graphic-p)
(set-face-attribute 'default nil :family "monaco" :height 120))
(if (display-graphic-p)
(set-fontset-font
  (frame-parameter nil 'font)
    'japanese-jisx0208
    '("Hiragino Kaku Gothic ProN" . "iso10646-1")))
(if (display-graphic-p)
(set-fontset-font
  (frame-parameter nil 'font)
    'japanese-jisx0212
    '("Hiragino Kaku Gothic ProN" . "iso10646-1")))
(if (display-graphic-p)
(set-fontset-font
  (frame-parameter nil 'font)
    'mule-unicode-0100-24ff
    '("monaco" . "iso10646-1")))
(if (display-graphic-p)
(setq face-font-rescale-alist
    '(("^-apple-hiragino.*" . 1.2)
      (".*osaka-bold.*" . 1.2)
      (".*osaka-medium.*" . 1.2)
      (".*courier-bold-.*-mac-roman" . 1.0)
      (".*monaco cy-bold-.*-mac-cyrillic" . 0.9) (".*monaco-bold-.*-mac-roman" . 0.9)
      ("-cdac$" . 1.3))))

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
(require 'recentf-ext)
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
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1)
(auto-save-buffers-enhanced t)

;; default-tab-width
(setq default-tab-width 4)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

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

;; web-mode
(require 'web-mode)
;;; file types
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$"       . web-mode))
;;; indent
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
(add-hook 'web-mode-hook 'web-mode-hook))

;; go-mode
(require 'go-mode)
(add-hook 'go-mode-hook
	  '(lambda ()
	     (setq tab-width 2)))

;; rust-mode
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'"  . rust-mode))

;; typescript-mode
(require 'typescript-mode)

;; backslash
(define-key global-map [?\M-¥] "\\")

;; autopair
(require 'autopair)
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
    (ivy swiper go-mode rust-mode markdown-mode auto-save-buffers-enhanced counsel web-mode typescript-mode recentf-ext org color-theme autopair auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

