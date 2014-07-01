;; General Load Path for my stuff
(setq load-path (cons "/home/richard/emacs" load-path))

;; C
(setq c-default-style "k&r" c-basic-offset 2)
(defun my-c-mode-hook ()
  (c-set-offset 'case-label '+))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; Erlang
(setq load-path (cons "/usr/lib/erlang/lib/tools-2.6.11/emacs" load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)
(require 'erlang-flymake)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Javscript
(setq js-indent-level 2)

;; Coffeescript
(autoload 'coffee-mode "coffee-mode"
  "Major mode for editing CoffeeScript files" t)
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(require 'coffee-mode)

;; Magit
(require 'magit)
(global-set-key (kbd "C-c s") 'magit-status)

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

;; -------------------- Backups -------------------------------

(setq backup-directory-alist
      `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.saves" t)))

;; -------------------- License Text -------------------------------

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

;; -------------------- Compilation -------------------------------

(global-set-key (kbd "C-c r") 'compile)
(global-set-key (kbd "C-c l") 'gdb)

;; Always save when files need compiling
(setq compilation-ask-about-save nil)

;; Follow compilation output
(setq compilation-scroll-output 'first-error)

;; -------------------- GDB -------------------------------

;; GDB Window navigation
(load "gdb-select-window")

;; Always use gdb-many-windows
(setq gdb-many-windows t)

;; -------------------- Company Mode -------------------------------
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

;; Some extra keybindings for when company is active
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)

;; -------------------- Custom Variables -------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((setq c-basic-offset 8) (gud-gdb-command-name . "arm-none-eabi-gdb --annotate=3") (gud-gdb-command-name . "arm-none-eabi-gdb --annotate=3 --command=gdbscript") (eval setq default-directory (locate-dominating-file buffer-file-name ".dir-locals.el")) (gud-gdb-command-name . "arm-none-eabi-gdb --annotate=3 --command=.gdbscript")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
