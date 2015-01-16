;;; disarmster.el --- Disassemble ARM C/C++ code under cursor in Emacs


;; Original: disaster.el
;; Copyright (C) 2013 Justine Tunney.

;; Author: Justine Tunney <jtunney@gmail.com>
;; Created: 2013-03-02
;; Version: 20130509.1055
;; X-Original-Version: 0.1
;; Keywords: tools
;; URL: https://github.com/jart/disaster

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ![Screenshot](http://i.imgur.com/kMoN1m6.png)
;;
;; Disarmster lets you press `C-c d` to see the compiled assembly code for the
;; C/C++ file you're currently editing. It even jumps to and highlights the
;; line of assembly corresponding to the line beneath your cursor.
;;
;; It works by creating a `.o` file using make (if you have a Makefile) or the
;; default system compiler. It then runs that file through objdump to generate
;; the human-readable assembly.

;;; Installation:

;; Make sure to place `disarmster.el` somewhere in the load-path and add the
;; following lines to your `.emacs` file to enable the `C-c d` shortcut to
;; invoke `disarmster':
;;
;;     (add-to-list 'load-path "/PATH/TO/DISARMSTER")
;;     (require 'disarmster)
;;     (define-key c-mode-base-map (kbd "C-c d") 'disarmster)
;;

;;; Code:

(defgroup disarmster nil
  "Disassemble C/C++ under cursor (Works best with Clang)."
  :prefix "disarmster-"
  :group 'tools)

(defcustom disarmster-make-flags "-k"
  "Command line options to pass to make if a Makefile is found."
  :group 'disarmster
  :type 'string)

(defcustom disarmster-cc (or (getenv "CC") "cc")
  "The command for your C compiler."
  :group 'disarmster
  :type 'string)

(defcustom disarmster-cxx (or (getenv "CXX") "c++")
  "The command for your C++ compiler."
  :group 'disarmster
  :type 'string)

(defcustom disarmster-cflags (or (getenv "CFLAGS")
                               "-march=native")
  "Command line options to use when compiling C."
  :group 'disarmster
  :type 'string)

(defcustom disarmster-cxxflags (or (getenv "CXXFLAGS")
                                 "-march=native")
  "Command line options to use when compiling C++.!"
  :group 'disarmster
  :type 'string)

(defcustom disarmster-objdump "arm-none-eabi-objdump -d -M att -Sl"
  "The command name and flags for running objdump."
  :group 'disarmster
  :type 'string)

(defcustom disarmster-buffer-compiler "*compilation*"
  "Buffer name to use for assembler output."
  :group 'disarmster
  :type 'string)

(defcustom disarmster-buffer-assembly "*assembly*"
  "Buffer name to use for objdump assembly output."
  :group 'disarmster
  :type 'string)

(defcustom disarmster-project-root-files
  (list (list ".git/")         ;; Git directory is almost always project root.
        (list "setup.py"       ;; Python apps.
              "package.json")  ;; node.js apps.
        (list "Makefile"))     ;; Makefiles are sometimes in subdirectories.
  "List of lists of files that may indicate software project root
   directory. Sublist are ordered from highest to lowest
   precedence."
  :group 'disarmster
  :type '(repeat (repeat string)))

(defvar save-place)

;;;###autoload
(defun disarmster (&optional file line)
  "Shows assembly code for current line of C/C++ file.

Here's the logic path it follows:

- Is there a Makefile in this directory? Run `make bufname.o`.
- Or is there a Makefile in a parent directory? Run `make -C .. bufname.o`.
- Or is this a C file? Run `cc -g -O3 -c -o bufname.o bufname.c`
- Or is this a C++ file? Run `c++ -g -O3 -c -o bufname.o bufname.c`
- If build failed, display errors in compile-mode.
- Run objdump inside a new window while maintaining focus.
- Jump to line matching current line.

If FILE and LINE are not specified, the current editing location
is used."
  (interactive)
  (save-buffer)
  (let* ((file (or file (file-name-nondirectory (buffer-file-name))))
         (line (or line (line-number-at-pos)))
         (file-line (format "%s:%d" file line))
         (makebuf (get-buffer-create disarmster-buffer-compiler))
         (asmbuf (get-buffer-create disarmster-buffer-assembly)))
    (if (not (string-match "\\.c[cp]?p?$" file))
        (message "Not C/C++ non-header file")
      (let* ((cwd default-directory)
             (obj-file-abs (concat (file-name-sans-extension (buffer-file-name)) ".o"))
             (make-root (disarmster-find-project-root "Makefile" file))
	     (obj-file-make (concat "out/" (file-relative-name obj-file-abs make-root)))
	     (obj-file-cwd (concat (file-relative-name make-root cwd) obj-file-make))
             (cc (if make-root
                     (if (equal cwd make-root)
                         (format "make %s %s" disarmster-make-flags
				 (file-relative-name obj-file-make make-root))
                       (format "make %s -C %s %s"
                               disarmster-make-flags make-root obj-file-make))
                   (if (string-match "\\.c[cp]p?$" file)
                       (format "%s %s -g -c -o %s %s"
                               disarmster-cxx disarmster-cxxflags
                               obj-file-make file)
                     (format "%s %s -g -c -o %s %s"
                             disarmster-cc disarmster-cflags
                             obj-file-make file))))
             (dump (format "%s %s" disarmster-objdump obj-file-cwd))
             (line-text (save-excursion
                          (buffer-substring-no-properties
                           (progn (beginning-of-line) (point))
                           (progn (end-of-line) (point))))))
        (if (and (eq 0 (progn
                         (message (format "Running: %s" cc))
                         (shell-command cc makebuf)))
                 (file-exists-p obj-file-cwd))
            (when (eq 0 (progn
                          (message (format "Running: %s" dump))
                          (shell-command dump asmbuf)))
              (kill-buffer makebuf)
              (with-current-buffer asmbuf
                ;; saveplace.el will prevent us from hopping to a line.
                (set (make-local-variable 'save-place) nil)
                (asm-mode)
                (disarmster--shadow-non-assembly-code))
              (let ((oldbuf (current-buffer)))
                (switch-to-buffer-other-window asmbuf)
                (goto-char 0)
                (if (or (search-forward line-text nil t)
                        (search-forward file-line nil t))
                    (progn
                      (recenter)
                      (overlay-put (make-overlay (save-excursion
                                                   (beginning-of-line)
                                                   (point))
                                                 (save-excursion
                                                   (forward-line)
                                                   (beginning-of-line)
                                                   (point)))
                                   'face 'region))
                    (message "Couldn't find corresponding assembly line."))
                (switch-to-buffer-other-window oldbuf)))
          (with-current-buffer makebuf
            (save-excursion
              (goto-char 0)
              (insert (concat cc "\n")))
            (compilation-mode)
            (display-buffer makebuf)))))))

(defun disarmster--shadow-non-assembly-code ()
  "Scans current buffer, which should be in asm-mode, and uses
the standard `shadow' face for lines that don't appear to contain
assembly code."
  (remove-overlays)
  (save-excursion
    (goto-char 0)
    (while (not (eobp))
      (beginning-of-line)
      (if (not (looking-at "[ \t]+[a-f0-9]+:[ \t]+"))
          (let ((eol (save-excursion (end-of-line) (point))))
            (overlay-put (make-overlay (point) eol)
                         'face 'shadow)))
      (forward-line))))

(defun disarmster--find-parent-dirs (&optional file)
  "Returns a list of parent directories with trailing slashes.

For example:

    (disarmster--find-parent-dirs \"/home/jart/disarmster-disarmster.el\")
    => (\"/home/jart/disarmster-\" \"/home/jart/\" \"/home/\" \"/\")

FILE defaults to `buffer-file-name'."
  (let ((res nil)
        (dir (file-name-directory
              (expand-file-name (or file (buffer-file-name))))))
    (while dir
      (setq res (cons dir res)
            dir (if (string-match "/[^/]+/$" dir)
                    (substring dir 0 (+ 1 (match-beginning 0))))))
    (reverse res)))

(defun disarmster--dir-has-file (dir file)
  "Returns t if DIR contains FILE (or any file if FILE is a list).

For example:

    (disarmster--dir-has-file \"/home/jart/\" \".bashrc\")
    (disarmster--dir-has-file \"/home/jart/\" (list \".bashrc\" \".screenrc\"))"
  (let ((res nil)
        (dir (file-name-as-directory dir))
        (files (if (listp file)
                   file
                 (list file))))
    (while (and files (not res))
      (setq res (file-exists-p (concat dir (car files)))
            files (cdr files)))
    res))

(defun disarmster-find-project-root (&optional looks file)
  "General-purpose Heuristic to detect bottom directory of project.

This works by scanning parent directories of FILE (using
`disarmster--find-parent-dirs') for certain types of files like a
`.git/` directory or a `Makefile` (which is less preferred).

The canonical structure of LOOKS is a list of lists of files
to look for in each parent directory where sublists are ordered
from highest precedence to lowest.  However you may specify
LOOKS as a single string or a list of strings for your
convenience. If LOOKS is not specified, it'll default to
`disarmster-project-root-files'."
  (let ((res nil)
        (looks (if looks
                   (if (listp looks)
                       (if (listp (car looks))
                           looks
                         (list looks))
                     (list (list looks)))
                 disarmster-project-root-files))
        (parents (disarmster--find-parent-dirs file)))
    (while (and looks (null res))
      (while (and parents (null res))
        (setq res (if (disarmster--dir-has-file
                       (car parents) (car looks))
                      (car parents))
              parents (cdr parents)))
      (setq looks (cdr looks)))
    res))

(provide 'disarmster)

;;; disarmster.el ends here
