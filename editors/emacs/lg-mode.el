;;; lg-mode.el --- Major mode and Eglot setup for lg -*- lexical-binding: t; no-native-compile: t; -*-

;; Copyright (C) 2026 Tienson Qin

;; Author: Tienson Qin
;; Keywords: languages, lisp
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1

;;; Commentary:

;; `lg-mode' provides local editing defaults for lg source files and registers
;; the lg language server with Eglot.  It is intentionally self-contained so a
;; fresh Emacs can edit this repository without requiring `clojure-mode'.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'lisp-mode)
(require 'project)
(require 'subr-x)

(when (and (boundp 'native-comp-jit-compilation-deny-list)
           (or load-file-name buffer-file-name))
  (add-to-list 'native-comp-jit-compilation-deny-list
               (regexp-quote (file-truename (or load-file-name
                                                buffer-file-name)))))

(defvar eglot-server-programs)
(defvar eglot-managed-mode)

(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function eglot-current-server "eglot")
(declare-function jsonrpc-request "jsonrpc")
(declare-function neat "neat")

(cl-defmethod project-root ((project (head lg)))
  "Return the root directory for an lg PROJECT."
  (cdr project))

(defgroup lg nil
  "Editing support for the lg language."
  :group 'languages
  :prefix "lg-")

(defcustom lg-lsp-command '("lg" "--lsp")
  "Command used by Eglot to start an installed lg language server."
  :type '(repeat string)
  :group 'lg)

(defcustom lg-use-repository-lsp t
  "Prefer `dune exec lg -- --lsp' inside an lg source checkout.

The repository command keeps editor integration pointed at the currently
checked-out compiler while developing lg itself.  Installed downstream use
falls back to `lg-lsp-command'."
  :type 'boolean
  :group 'lg)

(defcustom lg-project-state-relative-paths
  '("_build/install/default/lib/signal-lg/states/signal_native.state"
    "_build/default/duniverse/signal-lg/lg/signal/signal_native.state"
    "_build/install/default/lib/lui/states/lui_native_backends.state"
    "_build/default/duniverse/lui/lg/lui/lui_native_backends.state"
    "_build/install/default/lib/lg/stdlib/lg_stdlib_native.state")
  "Fallback project-local compiler states tried before the default LSP state.

`lg-mode' first discovers Dune `%{lib:...:*.state}' references from the project.
When those do not resolve to readable states, these paths are tried below the
project root."
  :type '(repeat string)
  :group 'lg)

(defcustom lg-eval-command '("lg" "--run")
  "Command used to run an installed lg program.

The edited file path is appended to this command."
  :type '(repeat string)
  :group 'lg)

(defcustom lg-use-repository-eval t
  "Prefer repository stdlib state when evaluating inside an lg checkout."
  :type 'boolean
  :group 'lg)

(defcustom lg-repl-command '("lg" "repl")
  "Command used to start an installed lg terminal REPL."
  :type '(repeat string)
  :group 'lg)

(defcustom lg-nrepl-command '("lg" "repl" "--nrepl-listen" "127.0.0.1:0")
  "Command used to start an installed lg nREPL server.

`lg-start-nrepl' appends `--port-file' and the generated port file path."
  :type '(repeat string)
  :group 'lg)

(defcustom lg-use-repository-repl t
  "Prefer repository stdlib state when starting REPLs inside an lg checkout."
  :type 'boolean
  :group 'lg)

(defcustom lg-cljc-fallback-mode 'clojure-mode
  "Major mode used for `.cljc' files outside lg projects.

When this mode is unavailable, `lg-cljc-mode' falls back to `lisp-mode'."
  :type 'function
  :group 'lg)

(defvar-keymap lg-mode-map
  :doc "Keymap for `lg-mode'."
  "C-c C-b" #'lg-eval-buffer
  "C-c C-r" #'lg-eval-region
  "C-c C-t" #'lg-show-type-at-point
  "C-c C-z" #'lg-repl
  "C-c C-n" #'lg-connect-nrepl)

(defvar lg-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `lg-mode'.")

(defconst lg-font-lock-keywords
  `((,(regexp-opt
       '("def" "defn" "defn-" "fn" "let" "loop" "recur" "if" "if-not"
         "when" "cond" "do" "match" "require" "module" "module-signature"
         "module-alias" "module-functor" "module-apply" "open" "include"
         "type-alias" "type-record" "type-variant" "protocol" "extend-type"
         "extend-protocol" "deftype" "defrecord" "ffi")
       'symbols)
     . font-lock-keyword-face)
    ("\\_<:[[:alnum:]_.*+!?'/$%&=<>-]+\\_>" . font-lock-constant-face)
    ("\\_<\\^:[[:alnum:]_.*+!?'/$%&=<>-]+\\_>" . font-lock-type-face))
  "Font-lock rules for `lg-mode'.")

(defun lg--project-root ()
  "Return the current project root or `default-directory'."
  (let ((default-directory (lg--file-directory)))
    (if-let ((project (project-current nil)))
        (file-name-as-directory (project-root project))
      (file-name-as-directory default-directory))))

(defun lg--file-directory ()
  "Return the current buffer file directory or `default-directory'."
  (file-name-as-directory
   (or (and buffer-file-name (file-name-directory buffer-file-name))
       default-directory)))

(defun lg--nearest-marker-root (directory)
  "Return the nearest lg marker root above DIRECTORY."
  (or (when-let ((root (locate-dominating-file directory ".lg-project")))
        (file-name-as-directory root))
      (when-let ((root (locate-dominating-file directory "dune-project")))
        (let ((root (file-name-as-directory root)))
	      (when (or (lg--source-checkout-p root)
	                    (lg--dune-project-depends-on-lg-p root)
	                    (lg--opam-project-depends-on-lg-p root))
	            root)))))

(defun lg--nearest-project-root ()
  "Return the nearest project root for mode detection."
  (or (lg--nearest-marker-root (lg--file-directory))
      (let ((default-directory (lg--file-directory)))
        (when-let ((project (project-current nil)))
          (file-name-as-directory (project-root project))))
      (lg--file-directory)))

(defun lg--source-checkout-p (root)
  "Return non-nil when ROOT looks like the lg compiler checkout."
  (and (file-exists-p (expand-file-name "dune-project" root))
       (file-exists-p (expand-file-name "bin/lg_lsp.ml" root))
       (file-exists-p (expand-file-name "src/language_service.ml" root))))

(defun lg--dune-project-depends-on-lg-p (root)
  "Return non-nil when ROOT has a Dune project depending on lg."
  (let ((dune-project (expand-file-name "dune-project" root)))
    (and (file-readable-p dune-project)
         (with-temp-buffer
           (insert-file-contents dune-project)
           (goto-char (point-min))
           (re-search-forward
	            "(depends\\(?:.\\|\n\\)*\\(?:^\\|[[:space:]()]\\)lg\\(?:$\\|[[:space:]()]\\)"
	            nil t)))))

(defun lg--opam-file-depends-on-lg-p (opam-file)
  "Return non-nil when OPAM-FILE declares a dependency on lg."
  (and (file-readable-p opam-file)
       (with-temp-buffer
         (insert-file-contents opam-file)
         (goto-char (point-min))
         (and (re-search-forward
               "^depends:[[:space:]\n]*\\[\\(?:.\\|\n\\)*\"lg\"\\(?:[[:space:]\n{}]\\|$\\)"
               nil t)
              t))))

(defun lg--opam-project-depends-on-lg-p (root)
  "Return non-nil when an opam package under ROOT depends on lg."
  (cl-some #'lg--opam-file-depends-on-lg-p
           (directory-files root t "\\.opam\\'")))

(defun lg-project-p (&optional root)
  "Return non-nil when ROOT should use `lg-mode' for `.cljc' files."
  (let ((root (file-name-as-directory (or root (lg--nearest-project-root)))))
    (or (lg--source-checkout-p root)
        (file-exists-p (expand-file-name ".lg-project" root))
        (lg--dune-project-depends-on-lg-p root)
        (lg--opam-project-depends-on-lg-p root))))

;;;###autoload
(defun lg-project-try (directory)
  "Return an lg project for DIRECTORY when it belongs to one."
  (when-let ((root (lg--nearest-marker-root directory)))
    (cons 'lg root)))

;;;###autoload
(add-hook 'project-find-functions #'lg-project-try)

;;;###autoload
(defun lg-cljc-mode ()
  "Select the right major mode for a `.cljc' file.

Use `lg-mode' in lg projects.  Outside lg projects, preserve the user's usual
Clojure editing setup by dispatching to `lg-cljc-fallback-mode'."
  (interactive)
  (if (lg-project-p)
      (lg-mode)
    (let ((fallback (if (fboundp lg-cljc-fallback-mode)
	                        lg-cljc-fallback-mode
	                      'lisp-mode)))
      (funcall fallback))))

;;;###autoload
(defun lg-lgi-mode ()
  "Select the right major mode for a `.lgi' file.

Use `lg-mode' in lg projects.  Outside lg projects, use `lisp-mode' as a
conservative fallback for the Lisp-like syntax."
  (interactive)
  (if (lg-project-p)
      (lg-mode)
    (lisp-mode)))

(defun lg-eglot-server-command (&optional root)
  "Return the preferred Eglot command for ROOT.

Inside this repository the command runs through Dune so Emacs uses the local
checkout.  Outside it, the installed `lg --lsp' command is used."
  (let* ((root (or root (lg--project-root)))
         (command (if (and lg-use-repository-lsp (lg--source-checkout-p root))
                      '("dune" "exec" "lg" "--" "--lsp")
                    lg-lsp-command))
         (state (lg-project-state-path root))
         (include-path (lg-project-include-path root))
         (command (if state
                      (append command (list "--state" state))
                    command)))
    (if include-path
        (append (list "env"
                      "LG_OCAML_INCLUDE_PATH_AUTHORITATIVE=1"
                      (concat "LG_OCAML_INCLUDE_PATH=" include-path)
                      (concat "OCAMLPATH=" include-path))
                command)
      command)))

(defun lg-project-state-path (&optional root)
  "Return the first readable project-local lg compiler state under ROOT."
  (let* ((root (file-name-as-directory (or root (lg--project-root))))
         (repository-state
          (when (lg--source-checkout-p root)
            (expand-file-name "_build/default/stdlib/lg_stdlib_native.state" root)))
         (candidates (append (delq nil (list repository-state))
                             (lg--discovered-project-state-paths root)
                             (mapcar (lambda (relative)
                                       (expand-file-name relative root))
                                     lg-project-state-relative-paths))))
    (cl-some (lambda (path)
               (when (file-readable-p path) path))
             (delete-dups candidates))))

(defun lg--project-dune-files (directory)
  "Return project Dune files below DIRECTORY, excluding build/vendor trees."
  (let ((result nil))
    (condition-case nil
        (dolist (name (directory-files directory t "\\`[^.]"))
          (cond
           ((file-directory-p name)
            (unless (member (file-name-nondirectory name)
                            '("_build" "_opam" "duniverse" "node_modules"))
              (setq result (append result (lg--project-dune-files name)))))
           ((string= (file-name-nondirectory name) "dune")
            (push name result))))
      (file-error nil))
    (nreverse result)))

(defun lg--dune-state-references (file)
  "Return Dune `%{lib:LIB:STATE}' references from FILE."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((references nil))
        (goto-char (point-min))
        (while (re-search-forward "%{lib:\\([^:}]+\\):\\([^}]+\\.state\\)}" nil t)
          (push (cons (match-string 1) (match-string 2)) references))
        (nreverse references)))))

(defun lg--discovered-project-state-paths (root)
  "Return readable saved compiler states discovered from Dune files below ROOT."
  (let ((paths nil))
    (dolist (dune-file (lg--project-dune-files root))
      (dolist (reference (lg--dune-state-references dune-file))
        (let* ((library (replace-regexp-in-string "\\." "/" (car reference) t t))
               (state-file (cdr reference))
               (path (expand-file-name
                      (concat "_build/install/default/lib/" library "/"
                              state-file)
                      root)))
          (when (file-readable-p path)
            (push path paths)))))
    (delete-dups (nreverse paths))))

(defun lg-project-include-path (&optional root)
  "Return the project OCaml include path for lg tooling below ROOT."
  (let* ((root (file-name-as-directory (or root (lg--project-root))))
         (path (or (cl-some (lambda (relative)
                              (let ((path (expand-file-name relative root)))
                                (when (file-readable-p path) path)))
                            '("_build/default/core/lg_native_include_path"
                              "core/lg_native_include_path"))
                   nil)))
    (when path
      (let* ((build-directory (expand-file-name "_build/default/core" root))
             (base-directory (if (file-directory-p build-directory)
                                 build-directory
                               (file-name-directory path))))
        (mapconcat (lambda (entry)
                     (if (file-name-absolute-p entry)
                         entry
                       (expand-file-name entry base-directory)))
                   (split-string
                    (with-temp-buffer
                      (insert-file-contents path)
                      (string-trim (buffer-string)))
                    ":" t)
                   ":")))))

(defun lg--repository-eval-artifacts (root)
  "Return repository eval artifacts below ROOT."
  (list (expand-file-name "_build/default/stdlib/lg_stdlib_native.state" root)
        (expand-file-name "_build/default/stdlib/lg_stdlib_native.ml" root)))

(defun lg-eval-server-command (&optional root)
  "Return the preferred eval command prefix for ROOT.

The caller appends the source file path."
  (let* ((root (or root (lg--project-root)))
         (artifacts (lg--repository-eval-artifacts root))
         (state (car artifacts))
         (implementation (cadr artifacts)))
    (if (and lg-use-repository-eval (lg--source-checkout-p root))
        (list "dune" "exec" "lg" "--" "--run-from" state implementation)
      lg-eval-command)))

(defun lg-repl-server-command (&optional root)
  "Return the preferred terminal REPL command for ROOT."
  (let* ((root (or root (lg--project-root)))
         (state (car (lg--repository-eval-artifacts root))))
    (if (and lg-use-repository-repl (lg--source-checkout-p root))
        (list "dune" "exec" "lg" "--" "repl" "--state" state)
      lg-repl-command)))

(defun lg-nrepl-server-command (&optional root port-file)
  "Return the preferred nREPL server command for ROOT and PORT-FILE."
  (let* ((root (or root (lg--project-root)))
         (state (car (lg--repository-eval-artifacts root)))
         (base (if (and lg-use-repository-repl (lg--source-checkout-p root))
                   (list "dune" "exec" "lg" "--" "repl" "--nrepl-listen"
                         "127.0.0.1:0" "--state" state)
                 lg-nrepl-command)))
    (append base (when port-file (list "--port-file" port-file)))))

(defun lg-eglot-contact (_interactive project)
  "Return an Eglot server contact for PROJECT."
  (lg-eglot-server-command
   (when project
     (file-name-as-directory (project-root project)))))

(defun lg-eglot-setup ()
  "Register `lg-mode' with Eglot.

The function is safe to call repeatedly from init files."
  (interactive)
  (require 'eglot)
  (let ((entry '((lg-mode :language-id "lg") . lg-eglot-contact)))
    (setq eglot-server-programs
          (cl-remove-if
           (lambda (program)
             (let ((mode (car program)))
               (or (eq mode 'lg-mode)
                   (and (consp mode) (eq (car mode) 'lg-mode)))))
           eglot-server-programs))
    (add-to-list 'eglot-server-programs entry)))

(defun lg--ensure-repository-eval-artifacts (root buffer)
  "Build repository stdlib eval artifacts for ROOT into BUFFER."
  (when (and lg-use-repository-eval (lg--source-checkout-p root))
    (let* ((artifacts (lg--repository-eval-artifacts root))
           (state (car artifacts))
           (implementation (cadr artifacts)))
      (unless (and (file-exists-p state) (file-exists-p implementation))
        (let ((default-directory root))
          (unless
              (zerop
               (call-process "dune" nil buffer t "build"
                             "stdlib/lg_stdlib_native.state"
                             "stdlib/lg_stdlib_native.ml"))
            (error "lg eval failed while building stdlib artifacts")))))))

(defun lg--eval-source (source label)
  "Evaluate SOURCE with lg and return its output.

LABEL is used in the temporary file name and output buffer heading."
  (let* ((root (lg--project-root))
         (default-directory root)
         (buffer (get-buffer-create "*lg eval*"))
         (temp-file (make-temp-file (concat "lg-" label "-") nil ".cljc"))
         (command (lg-eval-server-command root))
         (program (car command))
         (arguments (append (cdr command) (list temp-file))))
    (unwind-protect
        (progn
          (write-region source nil temp-file nil 'silent)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (lg--ensure-repository-eval-artifacts root buffer)
              (insert "$ " (mapconcat #'identity (append command (list temp-file)) " ")
                      "\n\n")
              (let ((exit-code (apply #'call-process program nil buffer t arguments)))
                (if (zerop exit-code)
                    (progn
                      (display-buffer buffer)
                      (buffer-substring-no-properties (point-min) (point-max)))
                  (display-buffer buffer)
                  (error "lg eval failed with exit code %s" exit-code))))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun lg-eval-region (start end)
  "Evaluate the active region as an lg program."
  (interactive "r")
  (lg--eval-source (buffer-substring-no-properties start end) "region"))

(defun lg-eval-buffer ()
  "Evaluate the current buffer as an lg program."
  (interactive)
  (lg--eval-source (buffer-substring-no-properties (point-min) (point-max))
                   "buffer"))

(defun lg-type-at-point ()
  "Return the Eglot hover type string at point, or nil when unavailable."
  (when (fboundp 'eglot-current-server)
    (when-let ((server (eglot-current-server)))
      (require 'jsonrpc)
      (let* ((hover (jsonrpc-request server :textDocument/hover
                                     (eglot--TextDocumentPositionParams)
                                     :timeout 30))
             (contents (plist-get hover :contents)))
        (cond
         ((stringp contents) contents)
         ((and (listp contents) (plist-member contents :value))
          (plist-get contents :value))
         (t nil))))))

(defun lg-show-type-at-point ()
  "Show the Eglot hover type at point."
  (interactive)
  (if-let ((type (lg-type-at-point)))
      (message "%s" type)
    (user-error "No lg type available at point")))

(defun lg--comint (name command)
  "Start COMMAND in a comint buffer named NAME."
  (let* ((buffer-name (format "*%s*" name))
         (buffer (get-buffer-create buffer-name))
         (program (car command))
         (arguments (cdr command)))
    (unless program
      (user-error "Empty lg command"))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (apply #'make-comint-in-buffer name buffer program nil arguments)
        (comint-mode)))
    (pop-to-buffer-same-window buffer)))

(defun lg-repl ()
  "Start or switch to an lg terminal REPL."
  (interactive)
  (lg--comint "lg-repl" (lg-repl-server-command)))

(defun lg-start-nrepl ()
  "Start an lg nREPL server and return its port file path."
  (interactive)
  (let* ((root (lg--project-root))
         (buffer (get-buffer-create "*lg nREPL*"))
         (port-file (make-temp-file "lg-nrepl-port-"))
         (command (lg-nrepl-server-command root port-file))
         (program (car command))
         (arguments (cdr command)))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (erase-buffer)
        (apply #'make-comint-in-buffer "lg-nrepl" buffer program nil arguments)
        (comint-mode)))
    (message "lg nREPL starting; port file: %s" port-file)
    port-file))

(defun lg--wait-for-port-file (port-file)
  "Wait for PORT-FILE to contain an nREPL port."
  (let ((deadline (+ (float-time) 15))
        port)
    (while (and (not port) (< (float-time) deadline))
      (when (and (file-exists-p port-file)
                 (> (file-attribute-size (file-attributes port-file)) 0))
        (with-temp-buffer
          (insert-file-contents port-file)
          (setq port (string-to-number (string-trim (buffer-string))))))
      (unless port
        (accept-process-output nil 0.1)))
    (or port (user-error "Timed out waiting for lg nREPL port"))))

(defun lg-connect-nrepl (&optional port-file)
  "Connect to an lg nREPL server with the optional neat client.

When PORT-FILE is nil, start a local lg nREPL server first."
  (interactive)
  (unless (require 'neat nil t)
    (user-error "Install neat to use lg nREPL from Emacs"))
  (let* ((port-file (or port-file (lg-start-nrepl)))
         (port (lg--wait-for-port-file port-file)))
    (neat "127.0.0.1" port)))

;;;###autoload
(define-derived-mode lg-mode lisp-mode "lg"
  "Major mode for editing lg source files."
  :syntax-table lg-mode-syntax-table
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(lg-font-lock-keywords))
  (setq-local lisp-indent-function 'common-lisp-indent-function)
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (when (fboundp 'eglot-ensure)
    (add-hook 'eglot-managed-mode-hook #'eldoc-mode nil t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . lg-cljc-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lgi\\'" . lg-lgi-mode))

(with-eval-after-load 'eglot
  (lg-eglot-setup))

(provide 'lg-mode)

;;; lg-mode.el ends here
