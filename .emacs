;; General Load Path for my stuff
(setq load-path (cons "/home/richard/emacs" load-path))

(setq-default indent-tabs-mode nil)

;; MELPA Package Manager
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; Theme
(if window-system
    (load-theme 'solarized-light t))

;; APL
(when (>= emacs-major-version 24)
  (add-to-list 'load-path "~/emacs/gnu-apl-mode")
  (require 'gnu-apl-mode))

;; C
(setq c-default-style "k&r" c-basic-offset 2)
(defun my-c-mode-hook ()
  (c-set-offset 'case-label '+))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; Coffeescript
(autoload 'coffee-mode "coffee-mode"
  "Major mode for editing CoffeeScript files" t)
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(require 'coffee-mode)

;; DisARMster
(setq load-path (cons "~/emacs/disarmster" load-path))
(require 'disarmster)
(global-set-key (kbd "C-c d") 'disarmster)

;; Erlang
(setq load-path (cons "/usr/lib/erlang/lib/tools-2.6.13/emacs" load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)
(require 'erlang-flymake)

;; GAS
(require 'gas-mode)
(add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))
(add-to-list 'auto-mode-alist '("\\.lst\\'" . gas-mode))

;; Indent Region
(global-set-key (kbd "C-c i") 'indent-region)

;; Javscript
(setq js-indent-level 2)

;; Magit
(require 'magit)
(global-set-key (kbd "C-c s") 'magit-status)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; LaTeX
(require 'tex-site)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq LaTeX-eqnarray-label "eq"
LaTeX-equation-label "eq"
LaTeX-figure-label "fig"
LaTeX-table-label "tab"
LaTeX-myChapter-label "chap"
TeX-auto-save t
TeX-newline-function 'reindent-then-newline-and-indent
TeX-parse-self t
TeX-style-path
'("style/" "auto/"
"/usr/share/emacs21/site-lisp/auctex/style/"
"/var/lib/auctex/emacs21/"
"/usr/local/share/emacs/site-lisp/auctex/style/")
LaTeX-section-hook
'(LaTeX-section-heading
LaTeX-section-title
LaTeX-section-toc
LaTeX-section-section
LaTeX-section-label))

;; Python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Window naviagtion
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(setq windmove-wrap-around t)

;; Whitespace
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing newline))
(global-whitespace-mode t)
(add-hook 'c-mode-hook (lambda () (whitespace-mode 1)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Tags
(add-hook 'c-mode-common-hook
  (lambda ()
    (define-key c-mode-map [(ctrl tab)] 'complete-tag)))
(setq tags-revert-without-query 1)

;; Write-good mode-line

(add-to-list 'load-path "writegood-mode.el")
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)
(global-set-key "\C-c\C-gg" 'writegood-grade-level)
(global-set-key "\C-c\C-ge" 'writegood-reading-ease)

;; -------------------- Backups ------------------------------------------------

(setq backup-directory-alist
      `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.saves" t)))

;; -------------------- License Text -------------------------------------------

(load-library "xlicense.el")

(define-abbrev-table 'global-abbrev-table
  '(
    ("$$license" "" license-skeleton)
    ))

(add-hook 'c-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'c++-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'ld-script-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'makefile-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'conf-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'emacs-lisp-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'text-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'js-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'ruby-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'fundamental-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'python-mode-hook (function (lambda nil (abbrev-mode 1))))

;; -------------------- Compilation --------------------------------------------

(global-set-key (kbd "C-c r") 'compile)
(global-set-key (kbd "C-c l") 'gdb)

;; Always save when files need compiling
(setq compilation-ask-about-save nil)

;; Follow compilation output
(setq compilation-scroll-output 'first-error)

;; -------------------- GDB ----------------------------------------------------

;; GDB Window navigation (C-c C-g ..)
(load "gdb-select-window")

;; Always use gdb-many-windows
(setq gdb-many-windows t)

;; -------------------- Company Mode -------------------------------------------
(autoload 'company-mode "company" nil t)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company)

;; Create a function that allows the use of tab to trigger company
(defun indent-or-expand (arg)
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (company-complete-common)

    (indent-according-to-mode)))
(defun company-tabbing ()
  (local-set-key "\t" 'indent-or-expand))

;; Hook the company tabbing onto minor modes
(add-hook 'c-mode-hook  'company-tabbing)
(add-hook 'js-mode-hook 'company-tabbing)
(add-hook 'ruby-mode-hook       'company-tabbing)
(add-hook 'markdown-mode-hook   'company-tabbing)
;(add-hook 'python-mode-hook     'company-tabbing)

;; Some extra keybindings for when company is active
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)

;; -------------------- Full Screen --------------------------------------------

;; F11 = Full Screen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
      (if (equal 'fullboth current-value)
        (if (boundp 'old-fullscreen) old-fullscreen nil)
        (progn (setq old-fullscreen current-value)
          'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

;; Disable tool-bar
(tool-bar-mode -1)

;; Disable Menu Bar
(menu-bar-mode -1)

;; -------------------- Locked Buffer mode -------------------------------------

(global-set-key (kbd "C-c k") 'locked-buffer-mode)
(define-minor-mode locked-buffer-mode
  "Make the current window always display this buffer."
  nil " locked" nil
  (set-window-dedicated-p (selected-window) locked-buffer-mode))

;; -------------------- Custom Variables ---------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(safe-local-variable-values (quote ((gud-gdb-command-name . "arm-none-eabi-gdb --annotate=3") (eval setq default-directory (locate-dominating-file buffer-file-name ".dir-locals.el")) (gud-gdb-command-name . "arm-none-eabi-gdb -i=mi")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
