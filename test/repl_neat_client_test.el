;;; repl_neat_client_test.el --- Verify LG with the neat nREPL client -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'neat-client)

(defun lg-neat-test--wait (connection predicate)
  (let ((deadline (+ (float-time) 15)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output (neat-connection-process connection) 0.1))
    (unless (funcall predicate)
      (error "Timed out waiting for an nREPL response"))))

(defun lg-neat-test--request (connection send)
  (let (responses request-id)
    (setq request-id
          (funcall send
                   (lambda (response)
                     (push response responses))))
    (lg-neat-test--wait
     connection
     (lambda ()
       (not (gethash request-id (neat-connection-pending connection)))))
    (nreverse responses)))

(defun lg-neat-test--field (name responses)
  (cl-loop for response in responses
           for value = (neat-bencode-get response name)
           when value return value))

(let* ((port-text (getenv "LG_NREPL_TEST_PORT"))
       (port (and port-text (string-to-number port-text)))
       (connection nil))
  (unless (and port (> port 0))
    (error "LG_NREPL_TEST_PORT must contain the server port"))
  (unwind-protect
      (progn
        (setq connection (neat-connect "127.0.0.1" port))
        (let ((responses
               (lg-neat-test--request
                connection
                (lambda (callback)
                  (neat-describe connection callback)))))
          (unless (lg-neat-test--field "ops" responses)
            (error "LG nREPL describe response did not contain ops")))
        (neat-clone-session connection)
        (lg-neat-test--wait
         connection
         (lambda () (neat-connection-session connection)))
        (let ((responses
               (lg-neat-test--request
                connection
                (lambda (callback)
                  (neat-eval
                   connection
                   "(ns neat.demo)\n(def answer 3)\nanswer"
                   :callback callback)))))
          (unless (cl-some
                   (lambda (response)
                     (equal (neat-bencode-get response "value") "3"))
                   responses)
            (error "LG nREPL did not preserve a typed session through neat: %S"
                   responses)))
        (let* ((candidates
                (neat-completions-sync connection "ans" "neat.demo" 5))
               (answer
                (cl-find-if
                 (lambda (candidate)
                   (equal (neat-bencode-get candidate "candidate") "answer"))
                 candidates)))
          (unless (and answer
                       (equal (neat-bencode-get answer "type") "int"))
            (error "LG nREPL completions did not expose answer : int")))
        (let ((info (neat-lookup-sync connection "answer" "neat.demo" 5)))
          (unless (and info
                       (equal (neat-bencode-get info "name") "answer")
                       (equal (neat-bencode-get info "type") "int"))
            (error "LG nREPL lookup did not expose answer metadata")))
        (lg-neat-test--request
         connection
         (lambda (callback)
           (neat-load-file
            connection
            "(ns neat.loaded)\n(def loaded-value 9)\n"
            :file-path "/tmp/lg-neat-loaded.cljc"
            :file-name "lg-neat-loaded.cljc"
            :callback callback)))
        (let ((info
               (neat-lookup-sync
                connection "loaded-value" "neat.loaded" 5)))
          (unless (and info
                       (equal (neat-bencode-get info "file")
                              "/tmp/lg-neat-loaded.cljc")
                       (= (neat-bencode-get info "line") 2))
            (error "LG nREPL load-file did not preserve source metadata")))
        (let ((info (neat-lookup-sync connection "answer" "neat.demo" 5)))
          (unless (and info
                       (equal (neat-bencode-get info "name") "answer")
                       (equal (neat-bencode-get info "type") "int"))
            (error "LG nREPL lookup did not honor an explicit namespace")))
        (let ((candidates
               (neat-completions-sync connection "ans" "neat.demo" 5)))
          (unless (cl-find-if
                   (lambda (candidate)
                     (and
                      (equal (neat-bencode-get candidate "candidate") "answer")
                      (equal (neat-bencode-get candidate "type") "int")))
                   candidates)
            (error
             "LG nREPL completions did not honor an explicit namespace")))
        (let* ((responses
                (lg-neat-test--request
                 connection
                 (lambda (callback)
                   (neat-eval
                    connection
                    "(do (println \"neat-output\") :ok)"
                    :callback callback))))
               (output
                (mapconcat
                 (lambda (response)
                   (or (neat-bencode-get response "out") ""))
                 responses
                 "")))
          (unless (string-match-p "neat-output" output)
            (error "LG nREPL did not return stdout through neat"))
          (unless (equal (lg-neat-test--field "value" responses) ":ok")
            (error "LG nREPL did not return :ok through neat")))
        (message "LG nREPL passed neat client validation"))
    (when connection
      (ignore-errors (neat-disconnect connection)))))

;;; repl_neat_client_test.el ends here
