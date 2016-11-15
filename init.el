;;; init.el --- my settings

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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
(tool-bar-mode 0)
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
;; 行番号にも色がついてしまうから使わない
;; (global-linum-mode t)
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

;; ;; highlight-chars.el
;; (require 'highlight-chars)
;; (hc-toggle-highlight-tabs 1)

(setq comment-style 'multi-line)

(transient-mark-mode 1)

;; 複数ウィンドウを開かないようにする
(setq ns-pop-up-frames nil)

;; クリップボードを使う
(setq x-select-enable-clipboard t)

;; バッファを自動で更新する
(global-auto-revert-mode 1)

;; 現在行をハイライト
;; (global-hl-line-mode t)
;; (custom-set-faces
;;  '(hl-line ((t (:background "dark-gray")))))

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

;; ;; When opened from Desktop entry, PATH won't be set to shell's value.
;; (let ((path-str
;;        (replace-regexp-in-string
;;         "\n+$" "" (shell-command-to-string "echo $PATH"))))
;;   (setenv "PATH" path-str)
;;   (setq exec-path (nconc (split-string path-str ":") exec-path)))

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
    ("0fb6369323495c40b31820ec59167ac4c40773c3b952c264dd8651a3b704f6b5"
     "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6"
     default)))
 '(magit-diff-options nil)
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
                                 "Ricty Diminished"))
    ;; (setq face-font-rescale-alist
    ;;       '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)))
    ))

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
(defun scroll-down-with-lines ()
  "" (interactive) (scroll-down 3))
(defun scroll-up-with-lines ()
  "" (interactive) (scroll-up 3))
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
;; auto-complete
;;------------------------------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(ac-set-trigger-key "TAB")
;; (setq ac-disable-faces t)
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)
(setq ac-delay 0.1)
(setq ac-menu-show-map 0.2)
;; (global-auto-complete-mode t)

;; ;;------------------------------------------------------------------------------
;; ;; auto-java-complete
;; ;;------------------------------------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/auto-java-complete")
;; (require 'ajc-java-complete-config)
;; (add-hook 'java-mode-hook 'ajc-java-complete-mode)

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
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)


;; ;;------------------------------------------------------------------------------
;; ;; edit-server for Chrome extention
;; ;;------------------------------------------------------------------------------
;; (require 'edit-server)
;; (edit-server-start)
;; (setq edit-server-new-frame nil)        ; 新しいフレーム


;; ↓なんかイマイチ動かない
;; ;;------------------------------------------------------------------------------
;; ;; flymake
;; ;;------------------------------------------------------------------------------
;; ;; (eval-after-load "go-mode"
;; ;;   '(require 'flymake-go))

;; ;;------------------------------------------------------------------------------
;; ;; go-flymake
;; ;;------------------------------------------------------------------------------
;; (setenv "GOPATH" "/Users/syanuma/.go")
;; ;; (setenv "PATH" (concat (getenv "PATH") ":" "/extra/path/element"))
;; ;; (setq exec-path (append exec-path (list (expand-file-name "/another/thing"))))
;; (add-to-list 'load-path "~/.go/src/github.com/dougm/goflaymake")
;; (require 'go-flycheck)

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
;; キーバインドはこのファイルの最後で設定する
;; (global-set-key (kbd "C-@") 'er/expand-region)
;; (global-set-key (kbd "C-M-@") 'er/contract-region)

;; transient-mark-modeがnilでは動作しないらしい
;; (transient-mark-mode t)

;;------------------------------------------------------------------------------
;; flycheck
;;------------------------------------------------------------------------------
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
             (append flycheck-disabled-checkers
                     '(javascript-jshint)))

;; (setq flycheck-eslintrc "~/.emacs.d/.eslintrc") ; copy ~/bacchus/bacchus/.eslint
;; (add-to-list 'load-path "~/.go/src/github.com/dougm/goflymake")
;; (require 'go-flycheck)

;;------------------------------------------------------------------------------
;; flycheck-pos-tip
;;------------------------------------------------------------------------------
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))


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
;; (require 'go-mode)
;; (require 'go-autocomplete)
;; (require 'go-eldoc)
;; (setenv "GOROOT" "/usr/local/opt/go/libexec")
;; (setenv "GOPATH" (concat (getenv "HOME") "/.go"))
;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
;; (add-hook 'go-mode-hook
;;           '(lambda ()
;;              (setq gofmt-command "goimports")
;;              (add-hook 'before-save-hook 'gofmt-before-save)
;;              (set (make-local-variable 'compile-command)
;;                   "go build -v && go test -v && go vet")
;;              (define-key go-mode-map (kbd "M-.") 'godef-jump)
;;              (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
;;              (go-eldoc-setup)
;;              (smart-newline-mode 1)))


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
;; キーバインドはこのファイルの最後で設定する
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;------------------------------------------------------------------------------
;; neotree
;;------------------------------------------------------------------------------
;; (add-to-list 'load-path "/directory/containing/neotree/")
(require 'neotree)

;;------------------------------------------------------------------------------
;; ng2-mode
;;------------------------------------------------------------------------------
(require 'ng2-mode)

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
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

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
;; (add-hook 'before-save-hook 'tide-format-before-save)
;; 引数の改行を深くインデントしてくれないので、hookを外す
(define-key typescript-mode-map (kbd "C-c C-f") 'tide-format)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))


;;------------------------------------------------------------------------------
;; web-mode
;;------------------------------------------------------------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.\\(html\\|jsx\\|erb\\)\\'" . web-mode))

(defun web-mode-offsets ()
  (defvar web-mode-html-offset   2)
  (defvar web-mode-css-offset    2)
  (defvar web-mode-script-offset 2)
  (defvar web-mode-php-offset    2)
  (defvar web-mode-java-offset   2)
  (defvar web-mode-asp-offset    2)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

(add-hook 'web-mode-hook
          '(lambda()
             (auto-complete-mode)
             ;; (company-mode +1)
             (smartparens-mode -1)
             (web-mode-offsets)))

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
;; (setq yas-snippet-dirs "~/.emacs.d/elpa/yasnippet-20160517.1628/snippets")
(setq yas-prompt-functions
      '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt))


;;------------------------------------------------------------------------------
;; html-mode
;;------------------------------------------------------------------------------
(add-hook 'html-mode-hook
          '(lambda()
             (setq indent-tabs-mode nil)
             ;; (auto-complete-mode)
             (company-mode +1)
             ))



;; ;; ;; Emacs Lisp
;; ;; (add-hook 'emacs-lisp-mode-hook
;; ;;           '(lambda()
;; ;;              (rainbow-delimiters-mode)))

;; ;; ;; Common Lisp
;; ;; (add-hook 'lisp-mode-hook
;; ;;           '(lambda()
;; ;;              (rainbow-delimiters-mode)))

(smart-newline-mode 1)

;; 設定しても大丈夫そうなら設定する
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)


;; 優先
(add-to-list 'auto-mode-alist '("\\.component.html\\'" . ng2-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . ng2-mode))

;;------------------------------------------------------------------------------
;; key bind
;;------------------------------------------------------------------------------
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "M-?") 'help-for-help)
(define-key global-map (kbd "C-o") 'ace-jump-mode)
(define-key global-map (kbd "M-C-g") 'grep)
(define-key global-map (kbd "C-x l") 'goto-line)
;; (define-key global-map (kbd "C-x l") 'linum-mode)
(define-key global-map (kbd "C-x C-l") 'toggle-truncate-lines)
(define-key global-map (kbd "C-i") 'indent-for-tab-command) ;yasnippetをよぶと上書きされるため
(define-key global-map (kbd "M-q") 'keyboard-quit)
(define-key global-map (kbd "C-j") 'indent-new-comment-line)
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; magit
(if window-system
    (define-key global-map (kbd "C-x m") 'magit-status)
  (define-key global-map (kbd "M-m") 'magit-status))
(define-key global-map (kbd "C-c C-b") 'magit-blame)

;; helm
(defvar *helm-key-binding* t)
(defun switch-helm-key-binding ()
  (interactive)
  (if *helm-key-binding*
      (progn
        (define-key global-map (kbd "M-x") 'helm-M-x)
        (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
        (define-key global-map (kbd "C-x C-f") 'helm-find-files)
        (define-key global-map (kbd "C-x b") 'helm-buffers-list)
        (define-key global-map (kbd "C-x f") 'helm-recentf)
        (define-key global-map (kbd "C-x _") 'helm-google-suggest)
        (define-key global-map (kbd "C-c g") 'helm-git-grep)
        (define-key global-map (kbd "C-c C-g") 'helm-git-grep-at-point)
        (define-key global-map (kbd "s-p") 'helm-buffers-list))
    (progn
        (define-key global-map (kbd "M-x") 'execute-extended-command)
        (define-key global-map (kbd "M-y") 'yank-pop)
        (define-key global-map (kbd "C-x C-f") 'find-file)
        (define-key global-map (kbd "C-x b") 'switch-to-buffer)
        (define-key global-map (kbd "C-x f") 'set-fill-column)
        (define-key global-map (kbd "C-x _") nil))))
(defun toggle-helm-key-binding ()
  (interactive)
  (setq *helm-key-binding* (not *helm-key-binding*))
  (switch-helm-key-binding))
(switch-helm-key-binding)

;; redo+
(define-key global-map (kbd "C-/") 'undo)
(define-key global-map (kbd "C-M-/") 'redo)
(define-key global-map (kbd "M-/") 'redo)

;; ;; zencoding
;; (define-key global-map (kbd "C-c z") 'zencoding-expand-yas)

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

;;------------------------------------------------------------------------------
;; color-theme
;;------------------------------------------------------------------------------
;; (load-theme 'monokai t)
(load-theme 'deeper-blue t)
;; (load-theme 'wonbat t)
;; (load-theme 'misterioso t)

;; カーソルの色
(set-cursor-color "green")

;; スクリーン最大化
(set-frame-parameter nil 'fullscreen 'maximized)
;; ;; フルスクリーン
;; (set-frame-parameter nil 'fullscreen 'fullboth)

(provide 'init)
;;; init.el ends here
