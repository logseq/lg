;;; emacs_eglot_test.el --- Tests for lg-mode Eglot integration -*- lexical-binding: t; -*-

(require 'ert)

(defun lg-test--repo-root ()
  (file-name-as-directory
   (expand-file-name
    (or (getenv "LG_TEST_WORKSPACE_ROOT")
        (expand-file-name ".." (file-name-directory load-file-name))))))

(defun lg-test--env-path (name fallback)
  (if-let ((value (getenv name)))
      (expand-file-name value)
    fallback))

(defun lg-test--lsp-command (repo-root)
  (list (lg-test--env-path
         "LG_TEST_LSP_EXE"
         (expand-file-name "_build/default/bin/lg_lsp.exe" repo-root))
        "--state"
        (lg-test--env-path
         "LG_TEST_STDLIB_STATE"
         (expand-file-name
          "_build/default/stdlib/lg_stdlib_native.state"
          repo-root))))

(defun lg-test--eval-command (repo-root)
  (list (lg-test--env-path
         "LG_TEST_CLI_EXE"
         (expand-file-name "_build/default/bin/lg_cli.exe" repo-root))
        "--run-from"
        (lg-test--env-path
         "LG_TEST_STDLIB_STATE"
         (expand-file-name
          "_build/default/stdlib/lg_stdlib_native.state"
          repo-root))
        (lg-test--env-path
         "LG_TEST_STDLIB_ML"
         (expand-file-name
          "_build/default/stdlib/lg_stdlib_native.ml"
          repo-root))))

(let ((repo-root (lg-test--repo-root)))
  (add-to-list 'load-path (expand-file-name "editors/emacs" repo-root)))

(require 'lg-mode)
(require 'eglot)
(require 'jsonrpc)
(require 'seq)
(require 'xref)

(defun lg-test--wait-for-eglot ()
  (let ((deadline (+ (float-time) 120.0)))
    (while (and (not (eglot-current-server)) (< (float-time) deadline))
      (accept-process-output nil 0.2)))
  (or (eglot-current-server)
      (error "Eglot did not start lg server")))

(defmacro lg-test--with-eglot-buffer (source &rest body)
  (declare (indent 1) (debug t))
  `(let* ((repo-root (lg-test--repo-root))
          (directory (make-temp-file "lg-eglot-project-" t))
          (file (expand-file-name ".eglot-xref-smoke.cljc" directory))
          (lg-lsp-command (lg-test--lsp-command repo-root)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".lg-project" directory)
             (insert ""))
           (with-temp-file file
             (insert ,source))
           (find-file file)
           (lg-mode)
           (let ((project (project-current))
                 (contact (lg-eglot-contact nil (project-current))))
             (eglot (list major-mode) project 'eglot-lsp-server contact
                    (list "lg")))
           (lg-test--wait-for-eglot)
           (unwind-protect
               (progn ,@body)
             (when-let ((server (eglot-current-server)))
               (eglot-shutdown server))
             (kill-buffer)))
       (when (file-exists-p file)
         (delete-file file))
       (when (file-directory-p directory)
         (delete-directory directory t)))))

(defmacro lg-test--with-lg-buffer (source &rest body)
  (declare (indent 1) (debug t))
  `(let* ((repo-root (lg-test--repo-root))
          (file (expand-file-name "test/.lg-eval-smoke.cljc" repo-root))
          (lg-use-repository-eval nil)
          (lg-eval-command (lg-test--eval-command repo-root)))
     (unwind-protect
         (progn
           (make-directory (file-name-directory file) t)
           (with-temp-file file
             (insert ,source))
           (find-file file)
           (lg-mode)
           (unwind-protect
               (progn ,@body)
             (kill-buffer)))
       (when (file-exists-p file)
         (delete-file file)))))

(defun lg-test--goto-symbol-use (symbol)
  (goto-char (point-min))
  (search-forward symbol nil t 2)
  (backward-word 1))

(defun lg-test--xref-file (xref)
  (xref-location-group (xref-item-location xref)))

(defun lg-test--xref-line (xref)
  (xref-location-line (xref-item-location xref)))

(ert-deftest lg-mode-loads-for-lg-project-cljc-files ()
  (should (eq (cdr (assoc "\\.cljc\\'" auto-mode-alist)) 'lg-cljc-mode))
  (with-temp-buffer
    (insert "(def answer 42)\n")
    (setq buffer-file-name
          (expand-file-name "test/example.cljc" (lg-test--repo-root)))
    (set-auto-mode)
    (should (derived-mode-p 'lg-mode))
    (should (eq indent-tabs-mode nil))
    (should (equal comment-start ";"))))

(ert-deftest lg-mode-loads-for-lg-project-lgi-files ()
  (should (eq (cdr (assoc "\\.lgi\\'" auto-mode-alist)) 'lg-lgi-mode))
  (let ((directory (make-temp-file "lg-lgi-project-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "dune-project" directory)
            (insert "(lang dune 3.17)\n(package (name chat) (allow_empty))\n"))
          (with-temp-file (expand-file-name "chat.opam" directory)
            (insert "opam-version: \"2.0\"\ndepends: [\n  \"ocaml\"\n  \"lg\"\n]\n"))
          (let ((file (expand-file-name "app.lgi" directory)))
            (with-temp-file file
              (insert "(val answer int)\n"))
            (find-file file)
            (unwind-protect
                (progn
                  (set-auto-mode)
                  (should (derived-mode-p 'lg-mode))
                  (should (equal (project-root (project-current))
                                 (file-name-as-directory directory))))
              (kill-buffer))))
      (delete-directory directory t))))

(ert-deftest lg-mode-keeps-non-lg-cljc-files-in-clojure-mode ()
  (cl-letf (((symbol-function 'clojure-mode)
             (lambda ()
               (interactive)
               (setq major-mode 'clojure-mode)
               (setq mode-name "Clojure"))))
    (let ((directory (make-temp-file "lg-non-lg-project-" t)))
      (unwind-protect
          (with-temp-buffer
            (insert "(ns ordinary.core)\n")
            (setq buffer-file-name (expand-file-name "ordinary.cljc" directory))
            (set-auto-mode)
            (should (eq major-mode 'clojure-mode)))
        (delete-directory directory t)))))

(ert-deftest lg-mode-detects-downstream-dune-lg-projects ()
  (let ((directory (make-temp-file "lg-dune-project-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "dune-project" directory)
            (insert "(lang dune 3.17)\n(package (name demo) (depends lg))\n"))
          (with-temp-buffer
            (insert "(def answer 42)\n")
            (setq buffer-file-name (expand-file-name "demo.cljc" directory))
            (set-auto-mode)
            (should (derived-mode-p 'lg-mode))))
      (delete-directory directory t))))

(ert-deftest lg-mode-detects-opam-projects-depending-on-lg ()
  (let ((directory (make-temp-file "lg-opam-project-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "dune-project" directory)
            (insert "(lang dune 3.17)\n(package (name chat) (allow_empty))\n"))
          (with-temp-file (expand-file-name "chat.opam" directory)
            (insert "opam-version: \"2.0\"\ndepends: [\n  \"ocaml\"\n  \"lg\"\n]\n"))
          (let ((source-directory (expand-file-name "src/chat" directory)))
            (make-directory source-directory t)
            (let ((file (expand-file-name "app.cljc" source-directory)))
	      (with-temp-file file
	        (insert "(def answer 42)\n"))
	      (find-file file)
	      (unwind-protect
	          (progn
	            (set-auto-mode)
	            (should (derived-mode-p 'lg-mode))
	            (should (equal (project-root (project-current))
	                           (file-name-as-directory directory))))
	        (kill-buffer)))))
      (delete-directory directory t))))

(ert-deftest lg-mode-detects-marker-chat-projects ()
  (let ((directory (make-temp-file "lg-chat-project-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".lg-project" directory))
          (let ((file (expand-file-name "chat.cljc" directory)))
            (with-temp-file file
              (insert "(def answer 42)\n"))
            (find-file file)
            (unwind-protect
                (progn
                  (set-auto-mode)
                  (should (derived-mode-p 'lg-mode))
                  (should (equal (project-root (project-current))
                                 (file-name-as-directory directory))))
              (kill-buffer))))
      (delete-directory directory t))))

(ert-deftest lg-mode-prefers-chat-project-over-outer-root ()
  (let* ((top (make-temp-file "lg-top-root-" t))
         (outer (expand-file-name "Top" top))
         (chat-project (expand-file-name "chat-project" outer)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" outer) t)
          (make-directory chat-project t)
          (with-temp-file (expand-file-name ".lg-project" chat-project))
          (let ((file (expand-file-name "chat.cljc" chat-project)))
            (with-temp-file file
              (insert "(def answer 42)\n"))
            (find-file file)
            (unwind-protect
                (progn
                  (set-auto-mode)
                  (should (derived-mode-p 'lg-mode))
                  (should (equal (project-root (project-current))
                                 (file-name-as-directory chat-project))))
              (kill-buffer))))
      (delete-directory top t))))

(ert-deftest lg-mode-registers-eglot-command ()
  (let ((directory (make-temp-file "lg-source-checkout-" t)))
    (unwind-protect
        (let ((eglot-server-programs nil)
              (default-directory (file-name-as-directory directory)))
          (with-temp-file (expand-file-name "dune-project" directory))
          (make-directory (expand-file-name "bin" directory))
          (make-directory (expand-file-name "src" directory))
          (make-directory (expand-file-name "_build/default/stdlib" directory) t)
          (with-temp-file (expand-file-name "bin/lg_lsp.ml" directory))
          (with-temp-file (expand-file-name "src/language_service.ml" directory))
          (with-temp-file
              (expand-file-name
               "_build/default/stdlib/lg_stdlib_native.state"
               directory))
          (lg-eglot-setup)
          (let ((entry (car eglot-server-programs)))
            (should (equal (car entry) '(lg-mode :language-id "lg")))
            (should (eq (cdr entry) 'lg-eglot-contact))
            (should (equal (funcall (cdr entry) nil (project-current))
                           (list "dune" "exec" "lg" "--" "--lsp"
                                 "--state"
                                 (expand-file-name
                                  "_build/default/stdlib/lg_stdlib_native.state"
                                  directory))))))
      (delete-directory directory t))))

  (ert-deftest lg-mode-falls-back-to-installed-lg-command ()
    (let ((lg-use-repository-lsp t)
        (lg-lsp-command '("custom-lg" "--lsp")))
    (should (equal (lg-eglot-server-command temporary-file-directory)
                   '("custom-lg" "--lsp")))))

  (ert-deftest lg-mode-passes-project-local-state-to-eglot ()
    (let ((directory (make-temp-file "lg-state-project-" t)))
      (unwind-protect
          (let ((state (expand-file-name
                        "_build/install/default/lib/signal-lg/states/signal_native.state"
                        directory))
                (lg-lsp-command '("custom-lg-lsp")))
            (make-directory (file-name-directory state) t)
            (with-temp-file state
              (insert "state placeholder"))
            (should (equal (lg-eglot-server-command directory)
                           (list "custom-lg-lsp" "--state" state))))
        (delete-directory directory t))))

(ert-deftest lg-mode-native-compilation-is-disabled ()
  (when (fboundp 'native-compile)
    (should-not (native-compile
                 (expand-file-name
                  "editors/emacs/lg-mode.el"
                  (lg-test--repo-root))))))

(ert-deftest lg-mode-repl-command-uses-repository-state ()
  (let ((root (make-temp-file "lg-source-checkout-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "dune-project" root))
          (make-directory (expand-file-name "bin" root))
          (make-directory (expand-file-name "src" root))
          (make-directory (expand-file-name "_build/default/stdlib" root) t)
          (with-temp-file (expand-file-name "bin/lg_lsp.ml" root))
          (with-temp-file (expand-file-name "src/language_service.ml" root))
          (with-temp-file
              (expand-file-name "_build/default/stdlib/lg_stdlib_native.state" root))
          (should (equal (lg-repl-server-command root)
                         (list "dune" "exec" "lg" "--" "repl" "--state"
                               (expand-file-name
                                "_build/default/stdlib/lg_stdlib_native.state"
                                root))))
          (should (equal (lg-nrepl-server-command root "/tmp/lg-nrepl-port")
                         (list "dune" "exec" "lg" "--" "repl" "--nrepl-listen"
                               "127.0.0.1:0" "--state"
                               (expand-file-name
                                "_build/default/stdlib/lg_stdlib_native.state"
                                root)
                               "--port-file" "/tmp/lg-nrepl-port"))))
      (delete-directory root t))))

(ert-deftest lg-mode-eglot-xref-finds-definitions-and-references ()
  (lg-test--with-eglot-buffer
      "(def answer 41)\n(defn add-one [^:int x] x)\n(def result (add-one answer))\n"
    (lg-test--goto-symbol-use "answer")
    (let* ((backend (xref-find-backend))
           (identifier (thing-at-point 'symbol t))
           (definitions (xref-backend-definitions backend identifier))
           (references (xref-backend-references backend identifier)))
      (should (equal identifier "answer"))
      (should definitions)
      (should (seq-some
               (lambda (xref)
                 (and (string-suffix-p ".eglot-xref-smoke.cljc"
                                       (lg-test--xref-file xref))
                      (= (lg-test--xref-line xref) 1)))
               definitions))
      (should (= (length references) 2))
      (should (seq-some
               (lambda (xref)
                 (and (string-suffix-p ".eglot-xref-smoke.cljc"
                                       (lg-test--xref-file xref))
                      (= (lg-test--xref-line xref) 1)))
               references))
      (should (seq-some
               (lambda (xref)
                 (and (string-suffix-p ".eglot-xref-smoke.cljc"
                                       (lg-test--xref-file xref))
                      (= (lg-test--xref-line xref) 3)))
               references)))))

(ert-deftest lg-mode-eglot-hover-shows-types ()
  (lg-test--with-eglot-buffer
      "(def answer 41)\n(defn add-one [^:int x] x)\n(def result (add-one answer))\n"
    (lg-test--goto-symbol-use "answer")
    (let ((value (lg-type-at-point)))
      (should (string-match-p "answer" value))
      (should (string-match-p "int" value)))))

(ert-deftest lg-mode-eval-buffer-runs-through-lg ()
  (lg-test--with-lg-buffer "(println 42)\n"
    (let ((output (lg-eval-buffer)))
      (should (string-match-p "\n42\n" output)))))

(ert-deftest lg-mode-eval-region-runs-through-lg ()
  (lg-test--with-lg-buffer "(println 1)\n(println 2)\n"
    (goto-char (point-min))
    (forward-line 1)
    (let ((output (lg-eval-region (point) (point-max))))
      (should (not (string-match-p "\n1\n" output)))
      (should (string-match-p "\n2\n" output)))))

(ert-run-tests-batch-and-exit)
