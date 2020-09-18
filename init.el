;;; init.el --- my settings

;;; Commentary:

;;; Code:
(setq delete-auto-save-files t)
(setq make-backup-files nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-quickhelp-terminal elpy exec-path-from-shell python-black yasnippet flycheck flycheck-pycheckers company-lsp lsp-mode ivy magit company ace-jump-mode counsel markdown-mode markdown-preview-mode markdownfmt monokai-theme smart-newline smartparens swiper undo-tree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(exec-path-from-shell-initialize)

(ivy-mode 1)
(defvar ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-recentf)
(global-set-key (kbd "C-x d") 'counsel-dired)
(global-set-key (kbd "C-c f") 'counsel-describe-function)
(global-set-key (kbd "C-c v") 'counsel-describe-variable)
(global-set-key (kbd "C-c o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(smartparens-global-mode)
(add-hook 'after-init-hook 'global-company-mode)

(global-undo-tree-mode)
(global-set-key (kbd "C--") 'undo-tree-undo) ; C-/ dose not work
(global-set-key (kbd "M--") 'undo-tree-redo) ; M-/ does not work
(global-set-key (kbd "C-u") 'undo-tree-undo) ; C-/ dose not work
(global-set-key (kbd "C-M-u") 'undo-tree-redo) ; M-/ does not work

(global-set-key (kbd "C-x j") 'ace-jump-mode)

(global-set-key (kbd "C-h") 'delete-backward-char)

(require 'company-lsp)
(push 'company-lsp company-backends)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(setq completion-ignore-case t)
(defvar  company-dabbrev-downcase nil)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)

(company-quickhelp-mode)

(global-flycheck-mode)
(yas-global-mode t)

(defvar python-black-on-save-mode t)
(add-hook 'python-mode-hook
	  (lambda ()
	    (elpy-enable)
	    (define-key python-mode-map (kbd "C-c f") 'python-black-buffer)))

(load-theme 'monokai t)
(menu-bar-mode -1)
;;; init.el ends here
