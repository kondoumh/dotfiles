;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path))))))

;; elisp と conf ディレクトリをサブディレクトリごと load-path に追加
(add-to-load-path "elisp" "conf")

(setq byte-compile-warnings '(not free-vars))

;; (install-elisp "http://www.emacswiki.org/emacs/download/auto-install.el")
(when (require 'auto-install nil t)
  ;; インストールディレクトリの設定 初期値は ~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWiki に登録されている elisp の名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup))

;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; スタートアップ非表示
(setq inhibit-startup-screen t)
;; 起動画面削除
(setq inhibit-startup-message t)

;; ツールバー非表示
(if (display-graphic-p)
  (tool-bar-mode 0))

;; ファイルのフルパスをタイトルバーに表示
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; scroll-bar-mode
(if (display-graphic-p)
  (scroll-bar-mode -1))

;; クリップボード共有
(setq x-select-enable-clipboard t)

;;dired
(require 'ffap)
(ffap-bindings)

;; キーバインド
;;; バッファ移動
(windmove-default-keybindings 'super)
;;; カーソルキー固定スクロール
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 1)))
;;; C-h を Backspace に
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

;; ファイル名文字化け対応
(require 'ucs-normalize)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; path 環境変数
(dolist
  (dir
    (list
      "/sbin"
      "/usr/sbin"
      "/bin"
      "/usr/bin"
      "/usr/local/bin"
      "/Library/Java/scala-2.10.0/bin"
       (expand-file-name "~/bin")
       (expand-file-name "~/.emacs.d/bin")
    )
  )
  (when (and (file-exists-p dir) (not (member dir exec-path)))
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (setq exec-path (append (list dir) exec-path))))

;; フォント
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

;; カレントディレクトリをホームディレクトリに設定
(cd "~/")

;; バックアップを残さない
(setq make-backup-files nil)

;; 括弧の範囲内を強調表示
(setq show-paren-delay 0.125)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; 括弧の範囲色
;; (set-face-background 'show-paren-match-face "#800")

;; 選択領域の色
(set-face-background 'region "#555")

;; 改行で auto indent
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

;; バッファの Kill で確認しない
(global-set-key [(control x) (k)] 'kill-this-buffer)

;; カレントウィンドウの透明度を変更する
(set-frame-parameter nil 'alpha 0.80)

;;ディスプレイ関係
;;(if window-system
;;  (progn
    ;; 文字色
;;    (add-to-list 'default-frame-alist '(foreground-color . "white"))
    ;; 背景色
;;    (add-to-list 'default-frame-alist '(background-color . "navy"))
    ;; カーソル色
;;    (add-to-list 'default-frame-alist '(cursor-color . "yellow"))
    ;; マウスポインタ色
;;    (add-to-list 'default-frame-alist '(mouse-color . "SlateBlue2"))
    ;; 選択中のリージョン色
;;    (set-face-background 'region "deeppink1")
;;  )
;;)

;; (load-theme 'misterioso t)

;; htmlize
(autoload 'htmlize-buffer "htmlize" "Convert BUFFER to HTML, preserving colors and decorations." t)
(autoload 'htmlize-region "htmlize" "Convert the region to HTML, preserving colors and decorations." t)
(autoload 'htmlize-file "htmlize" "Load FILE, fontify it, convert it to HTML, and save the result." t)

(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)

;; recentf-ext
(require 'recentf-ext)
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
(recentf-mode 1)
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 20)
;;; 最近編集したファイルリスト呼び出し
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;;; 起動画面で recentf を開く
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
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org/todo.org"))
(setq org-mobile-inbox-for-pull "~/Dropbox/mobileorg.org")
(setq org-mobile-directory "~/Dropbox/アプリ/MobileOrg")

(setq org-capture-templates
   '(("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "Tasks") "* TODO %?n %in %a")
     ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org") "* %?n %Un %in %a")
     ("n" "Note" entry (file+headline "~/Dropbox/org/notes.org" "Notes") "* %?n %Un %i")
    )
)

;; web-mode
(require 'web-mode)
;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$"       . web-mode))
;;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2))
(add-hook 'web-mode-hook 'web-mode-hook)

;; typescript-mode
(require 'typescript-mode)

;; cofee-mode
(require 'coffee-mode)
(autoload 'coffee-mode "coffee-mode" "Major mode for editing CoffeeScript." t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (and (set (make-local-variable 'tab-width) 2)
       (set (make-local-variable 'coffee-tab-width) 2))
  )
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; go-mode
;(require 'go-mode-load)
;(add-hook 'go-mode-hook
;  '(lambda()
;    (setq c-basic-offset 4)
;    (setq indent-tabs-mode t)
;	(setq tab-width 2)
;    (local-set-key (kbd "M-.") 'godef-jump)
;    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
;    (local-set-key (kbd "C-c i") 'go-goto-imports)
;    (local-set-key (kbd "C-c d") 'godoc)))
;(add-hook 'before-save-hook 'gofmt-before-save)

;; backslash
(define-key global-map [?\M-¥] "\\")

;; revert-buffer without asking
(defun revert-buffer-force()
  (interactive)
  (revert-buffer nil t)
)

;; バッファリロード
(global-set-key "\C-c\C-r" 'revert-buffer-force)
 
;; folding
;;; C coding style
(add-hook 'c-mode-hook
  '(lambda ()
    (hs-minor-mode 1)))
;;; Scheme coding style
(add-hook 'scheme-mode-hook
  '(lambda ()
    (hs-minor-mode 1)))
;;; Elisp coding style
(add-hook 'emacs-lisp-mode-hook
  '(lambda ()
    (hs-minor-mode 1)))
;;; Lisp coding style
(add-hook 'lisp-mode-hook
  '(lambda ()
    (hs-minor-mode 1)))
;;; Python coding style
(add-hook 'python-mode-hook
  '(lambda ()
    (hs-minor-mode 1)))
;;; CoffeeScript coding style
(add-hook 'coffee-mode-hook
  '(lambda ()
    (hs-minor-mode 1)))
(define-key
  global-map
  (kbd "C-#") 'hs-toggle-hiding)

;; フレーム
;;(setq default-frame-alist
;;  (append (list
;;    '(left . 500)
;;    '(top  . 10)
;;    '(width  .  110)
;;    '(height .  55)
;;  )
;;  default-frame-alist))

;; Window 分割ユーティリティ
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

(defun other-window-or-split()
  (interactive)
  (when (one-window-p)
	(if (>= (window-body-width) 270)
	  (split-window-horizontally-n 3)
	(split-window-horizontally)))
  (other-window 1))

(global-set-key (kbd "C-t") 'other-window-or-split)

;; autopair
(require 'autopair)
(autopair-global-mode)

;; 起動時のウィンドウ分割
;;(add-hook 'after-init-hook (lambda()
;;  (setq w1 (selected-window))
;;  (setq w2 (split-window-vertically))
;;  (select-window w2)
;;  (eshell)
;;  (select-window w1))
;;)


 ;; scroll one line at a time (less "jumpy" than defaults) 
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)

;; tabbar
(require 'tabbar)
(tabbar-mode 1)
(tabbar-mwheel-mode -1)
(setq tabbar-buffer-groups-function nil)

(defun my-tabbar-buffer-list ()
  (remove-if
  (lambda (buffer)
  (find (aref (buffer-name buffer) 0) " *"))
  (buffer-list)))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
(setq tabbar-separator '(1.5))
(set-face-attribute
 'tabbar-default nil
 :family "Monaco"
 :background "black"
 :foreground "gray72"
 :height 1.0)
(set-face-attribute
 'tabbar-unselected nil
 :background "black"
 :foreground "grey72"
 :box nil)
(set-face-attribute
 'tabbar-selected nil
 :background "black"
 :foreground "yellow"
 :box nil)
(set-face-attribute
 'tabbar-button nil
 :box nil)
(set-face-attribute
 'tabbar-separator nil
 :height 1.5)

;; 日付
(defun insert-date ()
   (interactive)
   (let ((format "\n%Y-%m-%d %a  <kondoh@local>\n")
         (system-time-locale "ja_JP"))
   (insert (format-time-string format))))
(define-key global-map "\C-cd" `insert-date)

;; ChangeLog テンプレ
(defun insert-changelog-template()
  (interactive)
  (insert "\n\t* weight:\n\t* breakfast:\n\t* weather:\n\t* ssi:\n\t- 9:00in\n\t-\n\t* lunch:\n\t-\n\t- 19:30out\n\t* dinner:\n"))
(define-key global-map "\C-cl" `insert-changelog-template)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(web-mode typescript-mode tabbar rust-mode recentf-ext org markdown-mode htmlize haskell-mode go-mode color-theme coffee-mode autopair auto-save-buffers-enhanced auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
