(require 'cl-lib)
(require 's)
(require 'detached)

(defvar podbook-configurations nil
  "Configurations for various products/environments.")

(defvar podbook-kubectl "kubectl"
  "The kubectl command.")

(defvar podbook--status (make-hash-table :test 'equal)
  "Retain the realtime statuses of podbook processes.")

(cl-defstruct podbook
  "podbook configuration object"
  exec
  container
  port
  iframe
  livebook
  label)

(defun podbook (&optional obj)
  "Start or connect to a podbook."
  (interactive)
  (let ((obj (or obj (podbook--choose-configuration))))
    (if (podbook-running-p obj)
        (podbook--do-post-startup obj)
      (podbook--do-startup obj))))

(defun podbook-copy-file (file &optional obj)
  "Copy a file into a livebook container."
  (interactive "fFile: ")
  (let* ((obj (or obj (podbook--choose-configuration)))
         (pod (podbook-livebook obj))
         (cmd (format "%s cp %s %s:/data"
                      podbook-kubectl file pod)))
    (shell-command cmd)))

(defun podbook-copy-directory (dir &optional obj)
  "Copy an entire directory to the livebook container."
  (interactive "DDirectory: ")
  (let* ((obj (or obj (podbook--choose-configuration)))
         (pod (podbook-livebook obj))
         (cmd (format "%s cp %s %s:/data"
                      podbook-kubectl dir pod)))
    (shell-command cmd)))

(defun podbook-pull-livebooks (dir &optional obj)
  "Copy the /data directory from a livebook container."
  (interactive "DDirectory: ")
  (let* ((obj (or obj (podbook--choose-configuration)))
         (pod (podbook-livebook obj))
         (cmd (format "%s cp %s:/data %s"
                      podbook-kubectl pod dir)))
    (shell-command cmd)))

(defun podbook--do-startup (obj)
  (podbook-start obj)
  (podbook-wait obj)
  (podbook--do-post-startup obj))

(defun podbook--do-post-startup (obj)
  (when (not (podbook-port-forward-session obj))
    (podbook-start-port-forward obj))
  (podbook-browse obj))

(defun podbook-running-p (&optional obj)
  "Returns t if the podbook is running."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (format "%s get pod %s -o jsonpath=\"{.status.phase}\""
                      podbook-kubectl (podbook-livebook obj))))
    (string= (shell-command-to-string cmd) "Running")))

(defun podbook-pod-name (&optional obj)
  "Return the name of the pod we will attach to."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd
          (concat
           (format "%s get pods " podbook-kubectl)
           "--field-selector=status.phase=Running "
           "-o=custom-columns=NAME:.metadata.name "
           "--no-headers "
           (format "| grep '%s' | sort | head -n 1"
                   (podbook-label obj))))
         (pod-name (s-trim (shell-command-to-string cmd))))
    (podbook--interactive-results pod-name)
    pod-name))

(defun podbook-release-node (&optional obj)
  "Return the name of the Elixir release node."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (concat
               (format "%s exec %s -c %s -- bin/%s "
                       podbook-kubectl
                       (podbook-pod-name obj)
                       (podbook-container obj)
                       (podbook-exec obj))
               "eval 'IO.puts(System.get_env(\"RELEASE_NODE\"))'"))
         (release-node (s-trim (shell-command-to-string cmd))))
    (podbook--interactive-results release-node)
    release-node))

(defun podbook-release-cookie (&optional obj)
  "Return the value of the Elixir release cookie."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (concat
               (format "%s exec %s -c %s -- bin/%s "
                       podbook-kubectl
                       (podbook-pod-name obj)
                       (podbook-container obj)
                       (podbook-exec obj))
               "eval 'IO.puts(System.get_env(\"RELEASE_COOKIE\"))'"))
         (release-cookie (s-trim (shell-command-to-string cmd))))
    (podbook--interactive-results release-cookie)
    release-cookie))

(defun podbook-start (&optional obj)
  "Start a Livebook."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (podbook--start-command obj))
         (res (shell-command-to-string cmd)))
    (podbook--interactive-results res)))

(defun podbook-stop (&optional obj)
  "Stop a Livebook."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (format "%s delete pod %s"
                      podbook-kubectl (podbook-livebook obj))))
    (shell-command cmd)))

(defun podbook-running ()
  "Returns a list of the running livebook pods."
  (interactive)
  (let* ((cmd (concat
               (format "%s get pods " podbook-kubectl)
               "--field-selector=status.phase=Running "
               "-o=custom-columns=NAME:.metadata.name "
               "--no-headers "
               (format "| grep 'livebook' | sort")))
         (pods (string-split (shell-command-to-string cmd))))
    (podbook--interactive-results (string-join pods ", "))
    pods))

(defun podbook-wait (&optional obj)
  "Wait for a livebook to become available; blocks."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (concat
               (format "%s wait --for=condition=ready pod/%s "
                       podbook-kubectl
                       (podbook-livebook obj))
               "--timeout 60s")))
    (shell-command cmd)))

(defun podbook-url (&optional obj)
  "Return the URL for a running livebook."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (format "%s logs %s"
                      podbook-kubectl
                      (podbook-livebook obj)))
         (log (shell-command-to-string cmd))
         (url (podbook--extract-url-from-string log)))
    (if url
        (progn
          (podbook--interactive-results url)
          url)
      (progn
        (when (called-interactively-p 'any)
          (message "Could not find URL."))
        nil))))

(defun podbook-browse (&optional obj)
  "Open the browser to Livebook instance."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (url (podbook-url obj)))
    (if url
        (browse-url url)
      (message "Unable to find URL."))))

(defun podbook-start-port-forward (&optional obj)
  "Establish port forward to Livebook."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (format "%s port-forward %s %d %d "
                      podbook-kubectl
                      (podbook-livebook obj)
                      (podbook-port obj)
                      (podbook-iframe obj)))
         (name (podbook-livebook obj))
         (buf name))
    (detached-shell-command cmd)))

(defun podbook-stop-port-forward (&optional obj)
  "Stop a port forwarding session."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (session (podbook-port-forward-session obj)))
    (when session
      (detached-kill-session session t))))

(defun podbook-port-forward-session (&optional obj)
  "Find a port forwarding session."
  (interactive)
  (let* (session
         (obj (or obj (podbook--choose-configuration)))
         (sessions (detached-get-sessions)))
    (dolist (s sessions)
      (when (and (detached-session-active-p s)
                 (s-contains? (podbook-livebook obj)
                              (detached-session-command s)))
        (setq session s)))
    (when (and session (called-interactively-p 'any))
      (detached-list-view-session session))
    session))

(defun podbook--start-command (obj)
  "Generate a start command.  Slow."
  (concat
   (format "%s run %s " podbook-kubectl (podbook-livebook obj))
   "--image=ghcr.io/livebook-dev/livebook "
   (format "--env LIVEBOOK_DEFAULT_RUNTIME=\"attached:%s:%s\" "
           (podbook-release-node obj)
           (podbook-release-cookie obj))
   "--env RELEASE_DISTRIBUTION=\"name\" "
   (format "--env LIVEBOOK_PORT=%d " (podbook-port obj))
   (format "--env LIVEBOOK_IFRAME_PORT=%d " (podbook-iframe obj))
   "--env RELEASE_NODE=\"livebook@127.0.0.1\""))

(defun podbook--plist-keys (plist)
  "Return a list of all keys in a PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun podbook--extract-url-from-string (s)
    "Extract the first URL found in a string S.
  Returns the URL as a string, or nil if no URL is found."
    (when (string-match "https?://[-a-zA-Z0-9_.:/%#?&=~]*" s)
      (match-string 0 s)))

(defmacro podbook--interactive-results (value)
  `(when (called-interactively-p 'any)
     (let ((result ,value))
       (message "%s" result)
       (kill-new result))))

(defun podbook--make-target (id)
  "Return a podbook object representing ID."
  (let* ((raw (plist-get podbook-configurations id))
         (iframe (1+ (plist-get raw :port)))
         (livebook (format "%s-livebook" (plist-get raw :container)))
         (attrs (append raw `(:iframe ,iframe
                              :livebook  ,livebook))))
    (apply #'make-podbook attrs)))

(defun podbook--choose-configuration ()
  "Returns a podbook object for the selected config key."
  (let* ((keys (podbook--plist-keys podbook-configurations))
         (choices (mapcar #'symbol-name keys))
         (selected (completing-read "Select Livebook " choices nil t)))
    (podbook--make-target (intern selected))))

(provide 'podbook)
