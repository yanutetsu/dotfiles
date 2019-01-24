;;; init.el --- my settings

;; Emacs25系の日本語入力のちらつきを改善する
(setq redisplay-dont-pause nil)

;; (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

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
                     -1))

;; font size
(set-face-attribute 'default nil :height 110)

;; 行番号フォーマット
(defvar linum-format
      (if (display-graphic-p)
          "%5d"
        "%5d "))

;; 括弧の範囲内を強調表示
(show-paren-mode 1)
(defvar show-paren-delay 0)

;; 行末の空白を強調表示
;; Golangを書いているぶんには必要ないかもしれない
(setq-default show-trailing-whitespace nil)
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

;; ファイル名補完で大小文字を区別しない
(setq read-file-name-completion-ignore-case t)

;; バッファ名補完で大小文字を区別しない
(setq read-buffer-completion-ignore-case t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(go-add-tags-style (quote lower-camel-case))
 '(magit-diff-options nil)
 '(package-selected-packages
   (quote
    (go-tag elfeed go-add-tags magit go-guru exec-path-from-shell flycheck-gometalinter twittering-mode markdown-toc go-errcheck go-rename company flycheck tide ivy ag emmet-mode yasnippet ace-jump-mode use-package yaml-mode web-mode vimrc-mode undo-tree tss smooth-scroll smex smartparens smart-newline smart-mode-line-powerline-theme scss-mode rustfmt rust-mode redo+ recentf-ext rainbow-delimiters npm-mode nginx-mode ng2-mode neotree multiple-cursors monokai-theme mode-icons mo-git-blame markdown-mode lorem-ipsum less-css-mode js2-mode ivy-hydra google-translate go-snippets go-scratch go-eldoc go-direx go-complete go-autocomplete gitignore-mode git-gutter-fringe+ fuzzy flymake-go expand-region editorconfig dockerfile-mode counsel company-web company-statistics company-quickhelp company-go comment-dwim-2 color-theme anzu)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(setq company-idle-delay .3)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))
(defvar company-dabbrev-downcase nil)

(company-quickhelp-mode +1)

(require 'company-statistics)
(company-statistics-mode)
(setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
;; (setq company-transformers '(company-sort-by-backend-importance))

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
;; elfeed
;;------------------------------------------------------------------------------
(setq elfeed-feeds
      '("http://cloudplatform.googleblog.com/atom.xml"))

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

(require 'flycheck-gometalinter)
(eval-after-load 'flycheck
  '(add-hook 'flyckec-mode-hook #'flycheck-gometalinter-setup))
(setq flycheck-gometalinter-vendor t)
;; (setq flycheck-gometalinter-errors-only t)
(setq flycheck-gometalinter-fast t)
(setq flycheck-gometalinter-tests t)
(setq flycheck-gometalinter-disable-linters '("gotype" "gocyclo"))
(setq flycheck-gometalinter-disable-all t)
(setq flycheck-gometalinter-enable-linters '("golint"))
(setq flycheck-gometalinter-deadline "10s")

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
(when window-system
  (require 'git-gutter-fringe+))

;;------------------------------------------------------------------------------
;; go-mode
;;------------------------------------------------------------------------------
(require 'go-mode)
(require 'company-go)
(require 'go-eldoc)
(setenv "GOPATH" "/Users/syanuma/go")
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; go-add-tags

;; golint
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

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
  (local-set-key (kbd "C-c C-l") 'go-direx-switch-to-buffer)
  (local-set-key (kbd "M-o") 'company-go)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (smart-newline-mode 1)
  (set (make-local-variable 'company-backends)
       ;; '((company-go company-dabbrev-code company-yasnippet))))
       '((company-dabbrev-code company-yasnippet))))

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

;; Fix error of "Failed to search TKK"
(defun google-translate--get-b-d1 ()
  ;; TKK='427110.1469889687'
  (list 427110 1469889687))

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
;; (global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
(setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))
(setq magit-completing-read-function 'ivy-completing-read)
(setq projectile-completion-system 'ivy)

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
;; 調子良くないから外した
;; (require 'migemo)
;; (setq migemo-command "cmigemo")
;; (setq migemo-options '("-q" "--emacs"))

;; (setq migemo-command "/usr/local/bin/cmigemo")
;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

;; (setq migemo-user-dictionary nil)
;; (setq migemo-regex-dictionary nil)
;; (setq migemo-coding-system 'utf-8-unix)
;; (load-library "migemo")
;; (migemo-init)


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
(define-key global-map (kbd "C-j") 'ace-jump-mode)
(define-key global-map (kbd "M-C-g") 'grep)
(define-key global-map (kbd "C-x l") 'goto-line)
(define-key global-map (kbd "C-x C-l") 'toggle-truncate-lines)
(define-key global-map (kbd "C-i") 'indent-for-tab-command) ;yasnippetをよぶと上書きされるため
(define-key global-map (kbd "M-q") 'keyboard-quit)
;; (define-key global-map (kbd "C-j") 'indent-new-comment-line)
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-M-m") 'indent-new-comment-line)
(define-key global-map (kbd "M-;") 'comment-dwim-2)
(define-key global-map (kbd "C-c z") 'emmet-expand-yas)

;; magit
(define-key global-map (kbd "M-m") 'magit-status)
(define-key global-map (kbd "C-c C-b") 'magit-blame)

;; redo+
(define-key global-map (kbd "C-/") 'undo)
(define-key global-map (kbd "C-M-/") 'redo)
(define-key global-map (kbd "M-/") 'redo)

;; expand-region
(global-set-key (kbd "C-o") 'er/expand-region)
(global-set-key (kbd "C-M-o") 'er/contract-region)

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
(if window-system
    (load-theme 'deeper-blue t)
  (load-theme ' monokai t))
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

;; PATHの引き継ぎ
(exec-path-from-shell-initialize)
(put 'upcase-region 'disabled nil)
