;;; init.el --- my settings

;; Emacs25系の日本語入力のちらつきを改善する
(setq redisplay-dont-pause nil)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;;; Code:
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

;; ステータスバー？に時間を表示
;; (display-time)

;; カーソルの形
(setq-default cursor-type 'bar)

;; ラインナンバー表示
(line-number-mode 1)

;; コラムナンバー表示
(column-number-mode 1)

(setq echo-keystrokes 0.1)

(setq backup-inhibited t)

;; ツールバーをなんとかする
;; (tool-bar-mode 0)
(if window-system
    (tool-bar-mode -1)
  (menu-bar-mode -1))

;; スクロールバーをなんとかする
(when window-system
  (set-scroll-bar-mode nil))

;; オートセーブファイル削除
(setq delete-auto-save-files t)

;; バックアップファイル作成
(setq make-backup-files nil)

;; オートセーブファイル作成
(setq auto-save-default nil)

;; インデントにタブを使わない
(setq-default tab-width 4 indent-tabs-mode nil)

;; css-modeでのインデント
(defvar css-indent-offset 2)

;; タイトルバーに文言を表示
(setq frame-title-format (format "%%f - Emacs@%s" (system-name))) ; フルパス

;; 行番号表示
(global-linum-mode (if (display-graphic-p)
                       t
                     nil))

;; 行番号フォーマット
(defvar linum-format
      (if (display-graphic-p)
          "%5d"
        "%5d "))

;; 括弧の範囲内を強調表示
(show-paren-mode 1)
(defvar show-paren-delay 0)

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

(setq comment-style 'multi-line)

(transient-mark-mode 1)

;; 複数ウィンドウを開かないようにする
(setq ns-pop-up-frames nil)

;; クリップボードを使う
(setq select-enable-clipboard t)

;; バッファを自動で更新する
(global-auto-revert-mode 1)

;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(defvar dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(defvar dired-recursive-copies 'always)
;; diredバッファでC-sした時にファイル名だけにマッチさせる
(defvar dired-isearch-filenames t)

;; ビープ音を消す
(setq ring-bell-function 'ignore)

;; ;; grep-edit
;; (require 'grep-edit)

;; POファイルに関する設定
(load "start-po" t)
(defvar po-auto-update-file-header nil)      ; PO-Revision-Dateが無いためヘッダーは更新しない

;; pathの引き継ぎ
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$"
                                                   ""
                                                   (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(add-to-list 'exec-path (expand-file-name "/usr/local/bin"))
(add-to-list 'exec-path (expand-file-name "/Users/syanuma/.go/bin"))

;;------------------------------------------------------------------------------
;; C-aでインデントを飛ばした行頭に移動
;;------------------------------------------------------------------------------
(defun beginning-of-indented-line (current-point)
  (interactive "d")
  (if (string-match "^[ \t]+$"
                    (save-excursion
                      (buffer-substring-no-properties
                       (progn
                         (beginning-of-line)
                         (point))
                       current-point)))
      (begging-of-line)
    (back-to-indentation)))

(defun beginning-of-visual-indented-line (current-point)
  (interactive "d")
  (let ((vhead-pos (save-excursion (progn
                                     (beginning-of-visual-line)
                                     (point))))
        (head-pos (save-excursion (progn
                                    (beginning-of-line)
                                    (point)))))
    (cond
     ((eq vhead-pos head-pos)
      (if (string-match "^[ \t]+$"
                        (buffer-substring-no-properties vhead-pos current-point))
          (beginning-of-visual-line)
        (back-to-indentation)))
     ((eq vhead-pos current-point)
      (backward-char)
      (beginning-of-visual-indented-line (point)))
     (t
      (beginning-of-visual-line)))))

(global-set-key "\C-a" 'beginning-of-visual-indented-line)
(global-set-key "\C-e" 'end-of-visual-line)

;; emacsclient
;; シェルから現在のEmacsにアクセスする
;; (server-start)
;; (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
;; (setq server-socket-dir (format "/private/tmp/emacs%d" (user-uid)))

;; dictionary
(defun dictionary ()
  "dictionary.app"
  (interactive)
  (let ((editable (not buffer-read-only))
        (pt (save-excursion (mouse-set-point last-nonmenu-event)))
        beg end)
    (if (and mark-active
             (<= (region-beginning) pt)
             (<= pt (region-end)))
        (setq beg (region-beginning)
              end (region-end))
      (save-excursion
        (goto-char pt)
        (setq end (progn
                    (forward-word)
                    (point)))
        (setq beg (progn
                    (backward-word)
                    (point)))))
    (browse-url
     (concat "dict:///"
             (url-hexify-string (buffer-substring-no-properties beg end))))))
(define-key global-map (kbd "C-c d") 'dictionary)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "0fb6369323495c40b31820ec59167ac4c40773c3b952c264dd8651a3b704f6b5" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(magit-diff-options nil)
 '(package-selected-packages
   (quote
    (go-errcheck go-rename company flycheck tide ivy ag emmet-mode yasnippet ace-jump-mode use-package yaml-mode web-mode vimrc-mode undo-tree tss smooth-scroll smex smartparens smart-newline smart-mode-line-powerline-theme scss-mode rustfmt rust-mode redo+ recentf-ext rainbow-delimiters npm-mode nginx-mode ng2-mode neotree multiple-cursors monokai-theme mode-icons mo-git-blame migemo markdown-mode magit lorem-ipsum less-css-mode js2-mode ivy-hydra helm-themes helm-swoop helm-git-grep helm-fuzzy-find helm-emmet helm-descbinds helm-company google-translate go-snippets go-scratch go-eldoc go-direx go-complete go-autocomplete gitignore-mode git-gutter-fringe+ fuzzy flymake-go expand-region editorconfig dockerfile-mode counsel company-web company-statistics company-quickhelp company-go comment-dwim-2 color-theme anzu)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; font
(when window-system
  (progn
    (set-face-attribute 'default nil
                        :family "Ricty Diminished"
                        :height 140)
    (set-fontset-font nil
                      'japanese-jisx0208
                      (font-spec :family
                                 "Ricty Diminished"))))
    ;; (set-face-attribute 'default nil
    ;;                     :family "Source Code Pro for Powerline"
    ;;                     :height 120)
    ;; (set-fontset-font nil
    ;;                   'japanese-jisx0208
    ;;                   (font-spec :family
    ;;                              "Source Code Pro for Powerline"))))

;; mouse
(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key "\C-xm" 'xterm-mouse-mode)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1))))
(defun scroll-down-with-lines () (interactive) (scroll-down 3))
(defun scroll-up-with-lines () (interactive) (scroll-up 3))
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)
(global-set-key [double-wheel-up] 'scroll-down-with-lines)
(global-set-key [double-wheel-down] 'scroll-up-with-lines)
(global-set-key [triple-wheel-up] 'scroll-down-with-lines)
(global-set-key [triple-wheel-down] 'scroll-up-with-lines)

;; gulp lint
(defun gulp-lint ()
  (interactive)
  (princ "exec gulp lint")
  (shell-command-to-string "gulp lint"))

;; 背景を透過させる
(when window-system
  (set-frame-parameter nil 'alpha 95))

;; 背景を透過させるコマンド M-x set-alpha
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))


;;------------------------------------------------------------------------------
;; eshell
;;------------------------------------------------------------------------------
(defvar eshell-command-aliases-list
      (append
       (list
        (list "ll" "ls -l")
        (list "la" "ls -a")
        (list "emacs" "find-file $1")
        (list "m" "find-file $1")
        (list "mc" "find-fiel $1")
        (list "d" "dired .")
        (list "gb" "git branch")
        (list "gba" "git branch -a")
        (list "gco" "git checkout")
        (list "gci" "git commit")
        (list "gg" "git graph")
        (list "gs" "git status")
        (list "gf" "git fetch --prune")
        (list "gd" "git diff"))))



;;------------------------------------------------------------------------------
;; Modules
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; anzu
;;------------------------------------------------------------------------------
(global-anzu-mode +1)

;;------------------------------------------------------------------------------
;; company
;;------------------------------------------------------------------------------
(require 'company)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(defvar company-dabbrev-downcase nil)

(company-quickhelp-mode +1)

(require 'company-statistics)
(company-statistics-mode)
(setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))

(global-company-mode +1)

;;------------------------------------------------------------------------------
;; editorconfig
;;------------------------------------------------------------------------------
(editorconfig-mode +1)


;;------------------------------------------------------------------------------
;; emmet-mode
;;------------------------------------------------------------------------------
(require 'emmet-mode)

;;------------------------------------------------------------------------------
;; expand-region
;;------------------------------------------------------------------------------
(require 'expand-region)

;;------------------------------------------------------------------------------
;; flycheck
;;------------------------------------------------------------------------------
(require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
             (append flycheck-disabled-checkers
                     '(javascript-jshint)))

;; ng2-modeへのflycheckの適用
(flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)
;; (flycheck-add-mode 'typescript-tide 'ng2-ts-mode)


;;------------------------------------------------------------------------------
;; flycheck-pos-tip
;;------------------------------------------------------------------------------
;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))
;; (eval-after-load 'flycheck
;;   '(custom-set-variables
;;     '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))


;;------------------------------------------------------------------------------
;; magit
;;------------------------------------------------------------------------------
(require 'magit)
(add-hook 'magit-blame-mode-hook
          '(lambda ()
             (define-key magit-blame-mode-map (kbd "<return>") 'magit-show-commit)
             (define-key magit-blame-mode-map (kbd "q") 'magit-blame-quit)))
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;;------------------------------------------------------------------------------
;; git-gutter+
;;------------------------------------------------------------------------------
(require 'git-gutter+)
(global-git-gutter+-mode t)

;;------------------------------------------------------------------------------
;; git-gutter-fringe+
;;------------------------------------------------------------------------------
(require 'git-gutter-fringe+)

;;------------------------------------------------------------------------------
;; go-mode
;;------------------------------------------------------------------------------
(require 'go-mode)
(require 'company-go)
(require 'go-eldoc)
;; (setenv "GOPATH" (concat (getenv "HOME") "/.go"))
(setenv "GOROOT" (concat (getenv "HOME") "/.go"))
(setenv "GOPATH" (concat (getenv "HOME") "/projects/jtb-agent-account-system/server")) ; for jtb
(add-to-list 'exec-path (expand-file-name "/usr/local/bin"))
(add-to-list 'exec-path (expand-file-name "/usr/local/.go/bin"))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(defun file-name-sans-extension-underbar (filename)
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename)))
	  directory)
      (if (and (string-match "_[^._]*\\'" file)
	       (not (eq 0 (match-beginning 0))))
	  (if (setq directory (file-name-directory filename))
	      (concat directory (substring file 0 (match-beginning 0)))
	    (substring file 0 (match-beginning 0)))
	filename))))

(defun file-name-extension-underbar (filename &optional period)
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename))))
      (if (and (string-match "_[^._]*\\'" file)
	       (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) (if period 0 1)))
        (if period
            "")))))

(defun go--counterpart-name (name)
  (let ((ext (file-name-extension name))
        (base (file-name-sans-extension name)))
    (if (equal ext "go")
        (let ((ext2 (file-name-extension-underbar base))
              (base2 (file-name-sans-extension-underbar base)))
          (if (equal ext2 "test")
              (concat base2 ".go")
            (concat base "_test.go")))
      (concat base ".go"))))

(defun go-open-testfile ()
  (interactive)
  (find-file (go--counterpart-name (buffer-file-name))))

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go install -v && go test -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (local-set-key (kbd "C-c C-c") 'go-open-testfile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (smart-newline-mode 1))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t
                    :foreground "green"
                    :weight 'bold)


;;------------------------------------------------------------------------------
;; go-direx
;;------------------------------------------------------------------------------
;; (require 'go-direx)

;;------------------------------------------------------------------------------
;; google-translate
;;------------------------------------------------------------------------------
(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key "\C-ct" 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist
      '(("en" . "ja") ("ja" . "en")))

;;------------------------------------------------------------------------------
;; helm
;;------------------------------------------------------------------------------
(require 'helm)
(require 'helm-fuzzy-find)
;; ;; (global-set-key (kbd "C-s") 'helm-swoop)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c f") 'helm-find-files)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x f") 'helm-recentf)
;; (global-set-key (kbd "C-x _") 'helm-google-suggest)
(global-set-key (kbd "C-c C-j") 'helm-git-grep)
;; (global-set-key (kbd "C-c C-j") 'helm-git-grep-at-point)
;; (global-set-key (kbd "s-p") 'helm-buffers-list)


;;------------------------------------------------------------------------------
;; ivy
;;------------------------------------------------------------------------------
(ivy-mode +1)
(defvar ivy-use-virtual-buffers t)
(global-set-key (kbd "C-M-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "s-p") 'ivy-switch-buffer)
(global-set-key (kbd "C-x f") 'counsel-recentf)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)


;;------------------------------------------------------------------------------
;; js2-mode
;;------------------------------------------------------------------------------
(require 'js2-mode)
(add-to-list 'auto-mode-alist (cons  "\\.\\(js\\|as\\|jsn\\|jsx\\)\\'" 'js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js-indent-level 2)
             (define-key js2-mode-map (kbd "C-c C-c C-c") 'gulp-lint)))
(setq-default js2-strict-trailing-comma-warning nil) ; eslintでtrailing commaを許可しているため

;;------------------------------------------------------------------------------
;; java-mode
;;------------------------------------------------------------------------------
(add-hook 'java-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (smart-newline-mode 1)))

;;------------------------------------------------------------------------------
;; json-mode
;;------------------------------------------------------------------------------
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-hook 'json-mode-hook
          '(lambda ()
             (setq js-indent-level 2)))


;;------------------------------------------------------------------------------
;; less-css-mode
;;------------------------------------------------------------------------------
(require 'less-css-mode)
;; (setq less-css-compile-at-save t)

(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(add-hook 'less-css-mode-hook
          '(lambda()
             (setq css-indent-ooffset 2)))

;;------------------------------------------------------------------------------
;; markdown-mode
;;------------------------------------------------------------------------------
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;------------------------------------------------------------------------------
;; migemo
;;------------------------------------------------------------------------------
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))

(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)


;;------------------------------------------------------------------------------
;; mode-icons-mode
;;------------------------------------------------------------------------------
;; (mode-icons-mode)

;;------------------------------------------------------------------------------
;; multiple-cursors
;;------------------------------------------------------------------------------
(require 'multiple-cursors)

;;------------------------------------------------------------------------------
;; neotree
;;------------------------------------------------------------------------------
(require 'neotree)

;;------------------------------------------------------------------------------
;; ng2-mode
;;------------------------------------------------------------------------------
(require 'ng2-mode)

;; ng2-modeからコピー
(defun ng2--is-component (name)
  (equal (file-name-extension (file-name-sans-extension name)) "component"))

(defun ng2--is-service (name)
  (equal (file-name-extension (file-name-sans-extension name)) "service"))

(defun ng2--is-pipe (name)
  (equal (file-name-extension (file-name-sans-extension name)) "pipe"))

(defun ng2--is-directive (name)
  (equal (file-name-extension (file-name-sans-extension name)) "directive"))

(defun ng2--is-file (name)
  (cond ((ng2--is-component name) t)
        ((ng2--is-service name) t)
        ((ng2--is-pipe name) t)
        ((ng2--is-directive name) t)))

(defun ng2--counterpart-name (name)
  (if (ng2--is-component name)
      (let ((ext (file-name-extension name))
            (base (file-name-sans-extension name)))
        (if (equal ext "ts")
            (concat base ".html")
          (concat base ".ts")))
    name))

(defun ng2-open-counterpart ()
  (interactive)
  (find-file (ng2--counterpart-name (buffer-file-name))))


(defun ng2--counterpart-spec-name (name)
  (when (not (ng2--is-file name)) name)
  (let ((ext (file-name-extension name))
        (base (file-name-sans-extension name)))
    (if (equal ext "ts")
        (let ((ext2 (file-name-extension base))
              (base2 (file-name-sans-extension base)))
          (if (equal ext2 "spec")
              (concat base2 ".ts")
            (concat base ".spec.ts")))
      (concat base ".ts"))))

(defun ng2-open-counterpart-spec ()
  (interactive)
  (find-file (ng2--counterpart-spec-name (buffer-file-name))))

(defun set-up-ng2-ts-mode ()
  (interactive)
  (local-set-key (kbd "C-c C-f") 'tide-format)
  (local-set-key (kbd "C-c C-c") 'ng2-open-counterpart-spec)
  (eldoc-mode +1)
  (company-mode +1)
  (smart-newline-mode +1)
  ;; tide
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(add-hook 'ng2-ts-mode-hook #'set-up-ng2-ts-mode)

(defun set-up-ng2-html-mode ()
  (interactive)
  (local-set-key (kbd "C-c z") 'emmet-expand-yas))

(add-hook 'ng2-html-mode-hook #'ste-up-ng2-html-mode)

;;------------------------------------------------------------------------------
;; nginx-mode
;;------------------------------------------------------------------------------
(require 'nginx-mode)
(add-to-list 'auto-mode-alist '("nginx.conf" . nginx-mode))
(add-hook 'nginx-mode-hook
          '(lambda ()
             (setq nginx-indent-level 4)))

;;------------------------------------------------------------------------------
;; recentf-ext
;;------------------------------------------------------------------------------
(require 'recentf-ext)
(setq recentf-max-saved-items 500)
(defvar recentf-exculde '("/TAGS$" "/var/tmp/"))

;;------------------------------------------------------------------------------
;; reqdo+.el
;;------------------------------------------------------------------------------
(require 'redo+)
(setq undo-no-redo t)
(setq undo-limit 60000)
(setq undo-strong-limit 900000)

;;------------------------------------------------------------------------------
;; smart-newline
;;------------------------------------------------------------------------------
(require 'smart-newline)

;;------------------------------------------------------------------------------
;; smartparens
;;------------------------------------------------------------------------------
(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
;; smartparens.elの中でハイライトを消す設定をしている

;;------------------------------------------------------------------------------
;; sql-mode
;;------------------------------------------------------------------------------
;; mysqlのキーワードにハイライトを当てる
(add-hook 'sql-mode-hook 'sql-highlight-mysql-keywords)

;;------------------------------------------------------------------------------
;; typescript-mode
;;------------------------------------------------------------------------------
(require 'typescript-mode)
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(add-hook 'typescript-mode-hook
          '(lambda ()
             (define-key typescript-mode-map (kbd "C-c c") 'ng2-open-counterpart)
             (define-key typescript-mode-map (kbd "C-c C-c") 'ng2-open-counterpart-spec)
             (font-lock-add-keywords nil ng2-ts-font-lock-keywords)
             (define-key typescript-mode-map (kbd "C-c i") 'company-yasnippet)))


;;------------------------------------------------------------------------------
;; tide (typescript)
;;------------------------------------------------------------------------------
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1)
  (smart-newline-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(define-key typescript-mode-map (kbd "C-c C-f") 'tide-format)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
;; (add-hook 'ng2-ts-mode-hook #'setup-tide-mode)

;; format options
(defvar tide-format-options
      '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
        :placeOpenBraceOnNewLineForFunctions nil
        :tabSize 2
        :indentSize 2
        :indentStyle 2
        :convertTabsToSpaces t))

;; TODO tsserverを各プロジェクトごとに読み込む方法を調べる
;; (defvar tide-tsserver-executable "/Users/syanuma/projects/jtb-agent-account-system/client/node_modules/typescript/bin/tsserver")

;;------------------------------------------------------------------------------
;; undo-tree-mode
;;------------------------------------------------------------------------------
(global-undo-tree-mode)

;;------------------------------------------------------------------------------
;; web-mode
;;------------------------------------------------------------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.\\(html\\|jsx\\|erb\\)\\'" . web-mode))

(defun web-mode-hook-func ()
  (setq web-mode-markup-indent-offset 2)
  (company-mode +1)
  (smartparens-mode -1)
  (web-mode-offsets))
(add-hook 'web-mode-hook 'web-mode-hook-func)

;;------------------------------------------------------------------------------
;; yaml-mode
;;------------------------------------------------------------------------------
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;------------------------------------------------------------------------------
;; yasnippet.el
;;------------------------------------------------------------------------------
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs "~/.emacs.d/snippets")
(setq yas-prompt-functions
      '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt))


;;------------------------------------------------------------------------------
;; html-mode
;;------------------------------------------------------------------------------
(add-hook 'html-mode-hook
          '(lambda()
             (setq indent-tabs-mode nil)
             (company-mode +1)
             ))



;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (rainbow-delimiters-mode)))

;; Common Lisp
(add-hook 'lisp-mode-hook
          '(lambda()
             (rainbow-delimiters-mode)))

(smart-newline-mode 1)

;; 設定しても大丈夫そうなら設定する
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)


;; 優先
;; ng2-mode
(add-to-list 'auto-mode-alist '("\\.component.html\\'" . ng2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . ng2-mode))

;;------------------------------------------------------------------------------
;; key bind
;;------------------------------------------------------------------------------
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "M-?") 'help-for-help)
(define-key global-map (kbd "C-o") 'ace-jump-mode)
(define-key global-map (kbd "M-C-g") 'grep)
;; (define-key global-map (kbd "C-x l") 'goto-line)
(define-key global-map (kbd "C-x C-l") 'toggle-truncate-lines)
(define-key global-map (kbd "C-i") 'indent-for-tab-command) ;yasnippetをよぶと上書きされるため
(define-key global-map (kbd "M-q") 'keyboard-quit)
(define-key global-map (kbd "C-j") 'indent-new-comment-line)
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "M-;") 'comment-dwim-2)
(define-key global-map (kbd "C-c z") 'emmet-expand-yas)

;; magit
(if window-system
    (define-key global-map (kbd "C-x m") 'magit-status)
  (define-key global-map (kbd "M-m") 'magit-status))
(define-key global-map (kbd "C-c C-b") 'magit-blame)

;; redo+
(define-key global-map (kbd "C-/") 'undo)
(define-key global-map (kbd "C-M-/") 'redo)
(define-key global-map (kbd "M-/") 'redo)

;; expand-region
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)

;; multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; wind move
;; Shift + arrow keysでwindow移動
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; MacOSのdefaultの動作に近づける（タブじゃない）
(define-key global-map (kbd "s-{") 'windmove-left)
(define-key global-map (kbd "s-}") 'windmove-right)

;;------------------------------------------------------------------------------
;; color-theme
;;------------------------------------------------------------------------------
;; (load-theme 'monokai t)
(load-theme 'deeper-blue t)
;; (load-theme 'wonbat t)
;; (load-theme 'misterioso t)

;; smart-mode-line
(defvar sml/theme 'dark)
(sml/setup)

;; カーソルの色
(set-cursor-color "green")

;; スクリーン最大化
(set-frame-parameter nil 'fullscreen 'maximized)
;; ;; フルスクリーン
;; (set-frame-parameter nil 'fullscreen 'fullboth)

(provide 'init)
;;; init.el ends here
