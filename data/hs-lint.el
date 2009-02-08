;;; hs-lint.el --- minor mode for HLint code checking

;; Copyright 2009 (C) Alex Ott
;;
;; Author: Alex Ott <alexott@gmail.com>
;; Keywords: haskell, lint, HLint
;; Requirements:
;; Status: distributed under terms of GPL2 or above

;; Typical message from HLint looks like:
;;
;; /Users/ott/projects/lang-exp/haskell/test.hs:52:1: Eta reduce
;; Found:
;;   count1 p l = length (filter p l)
;; Why not:
;;   count1 p = length . filter p


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

(defcustom hs-lint-replace-with-suggestions nil
  "Replace user's code with suggested replacements"
  :type 'boolean
  :group 'hs-lint)

(defcustom hs-lint-replace-without-ask nil
  "Replace user's code with suggested replacements automatically"
  :type 'boolean
  :group 'hs-lint)

(defun hs-lint-process-setup ()
  "Setup compilation variables and buffer for `hlint'."
  (run-hooks 'hs-lint-setup-hook))

;; regex for replace suggestions
;;
;; ^\(.*?\):\([0-9]+\):\([0-9]+\): .*
;; Found:
;; \s +\(.*\)
;; Why not:
;; \s +\(.*\)

(defvar hs-lint-regex
  "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): .*[\n\C-m]Found:[\n\C-m]\\s +\\(.*\\)[\n\C-m]Why not:[\n\C-m]\\s +\\(.*\\)[\n\C-m]"
  "Regex for HLint messages")

(defun make-short-string (str maxlen)
  (if (< (length str) maxlen)
      str
    (concat (substring str 0 (- maxlen 3)) "...")))

(defun hs-lint-replace-suggestions ()
  "Perform actual replacement of suggestions"
  (goto-char (point-min))
  (while (re-search-forward hs-lint-regex nil t)
    (let* ((fname (match-string 1))
          (fline (string-to-number (match-string 2)))
          (old-code (match-string 4))
          (new-code (match-string 5))
          (msg (concat "Replace '" (make-short-string old-code 30)
                       "' with '" (make-short-string new-code 30) "'"))
          (bline 0)
          (eline 0)
          (spos 0)
          (new-old-code ""))
      (save-excursion
        (switch-to-buffer (get-file-buffer fname))
        (goto-line fline)
        (beginning-of-line)
        (setf bline (point))
        (when (or hs-lint-replace-without-ask
                  (yes-or-no-p msg))
          (end-of-line)
          (setf eline (point))
          (beginning-of-line)
          (setf old-code (regexp-quote old-code))
          (while (string-match "\\\\ " old-code spos)
            (setf new-old-code (concat new-old-code
                                 (substring old-code spos (match-beginning 0))
                                 "\\ *"))
            (setf spos (match-end 0)))
          (setf new-old-code (concat new-old-code (substring old-code spos)))
          (remove-text-properties bline eline '(composition nil))
          (when (re-search-forward new-old-code eline t)
            (replace-match new-code nil t)))))))

(defun hs-lint-finish-hook (buf msg)
  "Function, that is executed at the end of HLint execution"
  (if hs-lint-replace-with-suggestions
      (hs-lint-replace-suggestions)
      (next-error 1 t)))

(define-compilation-mode hs-lint-mode "HLint"
  "Mode for check Haskell source code."
  (set (make-local-variable 'compilation-process-setup-function)
       'hs-lint-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-finish-functions)
       (list 'hs-lint-finish-hook))
  )

(defun hs-lint ()
  "Run HLint for current buffer with haskell source"
  (interactive)
  (save-some-buffers hs-lint-save-files)
  (compilation-start (concat hs-lint-command " " buffer-file-name)
                     'hs-lint-mode))

(provide 'hs-lint)
;;; hs-lint.el ends here
