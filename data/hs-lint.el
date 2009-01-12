;;; hs-lint.el --- minor mode for HLint code checking

;; Copyright 2009 (C) Alex Ott
;;
;; Author: Alex Ott <alexott@gmail.com>
;; Keywords: haskell, lint, HLint
;; Requirements: 
;; Status: distributed under terms of GPL2 or above

(require 'compile)

(defgroup hs-lint nil
  "Run HLint as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'haskell)

(defcustom hs-lint-command "hlint"
  "The default hs-lint command for \\[hlint]."
  :type 'string
  :group 'hs-lint)

(defcustom hs-lint-save-files t
  "Save modified files when run HLint or no (ask user)"
  :type 'boolean
  :group 'hs-lint)

(defcustom hs-lint-setup-hook nil
  "List of hook functions run by `hs-lint-process-setup' (see `run-hooks')."
  :type 'hook
  :group 'hs-lint)

(defvar hs-lint-regexp-alist
  '(("^\\(.+?\\)\\(:[ \t]*\\)\\([0-9]+\\)\\2" 1 3))
  "Regexp used to match hlint hits.  See `compilation-error-regexp-alist'.")

(defvar hs-lint-menu-map
  (let ((map (make-sparse-keymap "hs-hints")))
    (define-key map [stop-subjob]
      '("Stop HLint" . kill-compilation))
    (define-key map [hs-lint-mode-separator2]
      '("----" . nil))
    (define-key map [hs-lint-first-error]
      '("First hint" . first-error))
    (define-key map [hs-lint-previous-error]
      '("Previous hint" . previous-error))
    (define-key map [hs-lint-next-error]
      '("Next hint" . next-error))
    map))

(defvar hs-lint-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Don't inherit from compilation-minor-mode-map,
    ;; because that introduces a menu bar item we don't want.
    ;; That confuses C-down-mouse-3.
    (define-key map [mouse-2] 'compile-goto-error)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-c\C-c" 'compile-goto-error)
    (define-key map "\C-m" 'compile-goto-error)
    (define-key map "\C-c\C-k" 'kill-compilation)
    (define-key map "\M-n" 'compilation-next-error)
    (define-key map "\M-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)
		(define-key map "q" 'bury-buffer)

    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)

    ;; Set up the menu-bar
    (let ((submap (make-sparse-keymap "HLint")))
      (define-key map [menu-bar hs-lint]
				(cons "HLint" submap))
      (set-keymap-parent submap hs-lint-menu-map))
    (define-key map [menu-bar hs-lint hs-lint-separator2]
      '("----" . nil))
    (define-key map [menu-bar hs-lint hs-lint-recompile]
      '("Re-run HLint" . recompile))
    map)
  "Keymap for HLint log buffers.
`compilation-minor-mode-map' is a parent of this.")

(defun hs-lint-process-setup ()
  "Setup compilation variables and buffer for `hlint'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
				 (if (eq status 'exit)
						 (cond ((zerop code)
										'("finished (code is checked)\n" . "matched"))
									 ((= code 1)
										'("Errors during checking of code\n" . "no match"))
									 (t
										(cons msg code)))
					 (cons msg code))))
  (run-hooks 'hs-lint-setup-hook))

(defun hs-lint-finish-hook (buf msg)
	"Function, that is executed at the end of HLint execution"
		(next-error 1 t)
		)

(define-compilation-mode hs-lint-mode "HLint"
  "Mode for check Haskell source code."
  (set (make-local-variable 'compilation-process-setup-function)
       'hs-lint-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil)
	(set (make-local-variable 'compilation-finish-functions) (list 'hs-lint-finish-hook))
	)

(defun hs-lint ()
	"Run HLint for current buffer with haskell source"
	(interactive)
	(save-some-buffers hs-lint-save-files)
  (compilation-start (concat hs-lint-command " "
														 buffer-file-name) 'hs-lint-mode))

(provide 'hs-lint)
;;; hs-lint.el ends here
