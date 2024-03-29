;; General Load Path for my stuff
(add-to-list 'load-path "~/emacs")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(lsp-rust-analyzer-cargo-all-targets nil)
 '(package-selected-packages
   (quote
    (rustic flycheck visual-regexp use-package python-black lsp-ui lsp-mode json-mode gnu-elpa-keyring-update company browse-kill-ring blacken astyle magit string-inflection)))
 '(safe-local-variable-values
   (quote
    ((eval highlight-regexp "^ *")
     (setq-default c-basic-offset 4 tab-width 4 indent-tabs-mode t)
     (gud-gdb-command-name . "arm-none-eabi-gdb --annotate=3")
     (eval setq default-directory
           (locate-dominating-file buffer-file-name ".dir-locals.el"))
     (gud-gdb-command-name . "arm-none-eabi-gdb -i=mi"))))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -------------------- Setup ---------------------------------------

(setq-default fill-column 80) ;; 80 character width
(setq-default indent-tabs-mode nil)

;; Preserve tall windows
(setq split-height-threshold 250)
(setq split-width-threshold 150)

;; use-package
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


;; Theme
(if window-system
    (load-theme 'solarized-light t))

;; APL
;; (when (>= emacs-major-version 24)
;;   (add-to-list 'load-path "~/emacs/gnu-apl-mode")
;;   (require 'gnu-apl-mode))

;; C
(setq c-default-style "k&r" c-basic-offset 2)
(defun my-c-mode-hook ()
  (c-set-offset 'case-label '+))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; (require 'clang-format)
;; (setq clang-format-style-option "llvm")

;; (require 'clang-format)
;; (global-set-key (kbd "C-c i") 'clang-format-region)
;; (global-set-key (kbd "C-c u") 'clang-format-buffer)

;; (setq clang-format-style-option "llvm")

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
(setq load-path (cons "/usr/lib/erlang/lib/tools-2.11.1/emacs" load-path))
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
(global-set-key (kbd "C-c g") 'magit-file-dispatch)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; LaTeX
;; (when (>= emacs-major-version 24)
;;   (require 'tex-site)
;;   (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
;;   (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
;;   (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
;;   (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
;;   (add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;;   ;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
;;   (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;   (setq LaTeX-eqnarray-label "eq"
;;         LaTeX-equation-label "eq"
;;         LaTeX-figure-label "fig"
;;         LaTeX-table-label "tab"
;;         LaTeX-myChapter-label "chap"
;;         TeX-auto-save t
;;         TeX-newline-function 'reindent-then-newline-and-indent
;;         TeX-parse-self t
;;         TeX-style-path
;;         '("style/" "auto/"
;;           "/usr/share/emacs21/site-lisp/auctex/style/"
;;           "/var/lib/auctex/emacs21/"
;;           "/usr/local/share/emacs/site-lisp/auctex/style/")
;;         LaTeX-section-hook
;;         '(LaTeX-section-heading
;;           LaTeX-section-title
;;           LaTeX-section-toc
;;           LaTeX-section-section
;;           LaTeX-section-label))
;;   )

;; Python
(setq python-indent 4)
(add-hook 'python-mode-hook 'jedi:setup)

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'blacken-buffer)
          ))
(setq jedi:complete-on-dot t)

;; LSP
(use-package lsp-mode
  :hook ((rust-mode          ;
          ) . lsp-deferred)
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0.05))

(setq gc-cons-threshold 100000000)

;; rust
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("M-:" . rustic-docstring-dwim)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c t" . lsp-rust-analyzer-open-cargo-toml)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; smerge mode
(setq smerge-command-prefix "\C-cv")

;; String Inflection
(use-package string-inflection)

(global-set-key (kbd "C-c u") 'string-inflection-underscore)
(global-set-key (kbd "C-c L") 'my-string-inflection-cycle-auto)

(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   (t
    ;; default
    (string-inflection-all-cycle))))

;; VHDL Mode
(setq load-path (cons (expand-file-name "~/.emacs.d/vhdl-mode-3.38.1/") load-path))
(autoload 'vhdl-mode "vhdl-mode" "VHDL Mode" t)
(setq auto-mode-alist (cons '("\\.vhdl?\\'" . vhdl-mode) auto-mode-alist))

;; Window naviagtion
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(setq windmove-wrap-around t)

;; query-replace-regexp
(global-set-key "\M-%" 'vr/query-replace)

;; revert buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

;; Whitespace
(require 'whitespace)
(setq whitespace-style '(face empty trailing newline lines-tail))
(global-whitespace-mode t)
(add-hook 'c-mode-hook (lambda () (whitespace-mode 1)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-line-column 99)

(defun preserve-trailing-whitespace ()
  (interactive)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace))

(global-set-key (kbd "C-x w") 'preserve-trailing-whitespace)

(defun unpreserve-trailing-whitespace ()
  (interactive)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(global-set-key (kbd "C-x M-w") 'unpreserve-trailing-whitespace)

;; Tags
(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-map [(ctrl tab)] 'complete-tag)))
(setq tags-revert-without-query 1)

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
(add-hook 'rust-mode-hook (function (lambda nil (abbrev-mode 1))))

;; -------------------- Compilation --------------------------------------------

(global-set-key (kbd "C-c r") 'compile)

;; Always save when files need compiling
(setq compilation-ask-about-save nil)

;; Follow compilation output
(setq compilation-scroll-output 'first-error)

;; -------------------- GDB ----------------------------------------------------

(require 'gdb-mi)

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
(add-hook 'rust-mode-hook  'company-tabbing)

;; Some extra keybindings for when company is active
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)

(setq company-tooltip-align-annotations t)

;; -------------------- Irony --------------------------------------------------

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Company-irony
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

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

;; -------------------- Increment at point -------------------------------------

(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun decrement-number-decimal (&optional arg)
  (interactive "p*")
  (increment-number-decimal (if arg (- arg) -1)))

(global-set-key (kbd "C-c +") 'increment-number-decimal)
(global-set-key (kbd "C-c -") 'decrement-number-decimal)



(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
