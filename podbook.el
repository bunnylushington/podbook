;;; podbook.el --- Manage Livebook instances in Kubernetes

;;; Commentary:
;;
;; This library provides a set of tools to manage Elixir Livebook
;; instances running within Kubernetes pods. It allows you to start,
;; stop, connect to, and manage file synchronization for different
;; Livebook configurations.
;;
;; To use podbook, you need to configure it with your project details.
;; Here is an example configuration for two different projects:
;;
;; (setq podbook-configurations
;;       '(:project-a
;;         (:exec "project_a"
;;          :container "project-a-container"
;;          :port 8080
;;          :directory "/path/to/your/project-a/notebooks"
;;          :label "project-a-label")
;;         :project-b
;;         (:exec "project_b"
;;          :container "project-b-container"
;;          :port 8090
;;          :directory "/path/to/your/project-b/notebooks"
;;          :label "project-b-label")))
;;
;; ** Configuration
;;
;; The `podbook-configurations` variable is a property list (plist) where
;; each key is a symbol identifying a project, and the value is another
;; plist containing the following attributes:
;;
;; - `:exec`: The name of the executable for the Elixir release (e.g., "my_app").
;;
;; - `:container`: The base name for the new Livebook pod. The final pod name
;;   will be this value with "-livebook" appended (e.g., if you
;;   provide "my-app", the pod will be named "my-app-livebook").
;;
;; - `:port`: The local and remote port to use for the Livebook instance.
;;   The required iframe port will be automatically set to `:port + 1`.
;;
;; - `:directory`: The default local directory for file synchronization
;;   with the Livebook container.
;;
;; - `:label`: A substring used to identify the running application pod.
;;   For example, if your pods are named "my-app-prod-a1b2c3d4",
;;   you could use "my-app-prod" as the label.
;;
;; Main interactive functions:
;;
;; - `podbook`: Start or connect to a podbook. This is the main entry point.
;; - `podbook-start`: Start a Livebook pod.
;; - `podbook-stop`: Stop a Livebook pod.
;; - `podbook-browse`: Open the Livebook URL in a browser.
;; - `podbook-copy-file`: Copy a file into a livebook container.
;; - `podbook-copy-directory`: Copy an entire directory to the livebook container.
;; - `podbook-pull-livebooks`: Copy the /data directory from a livebook container.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'detached)

(defgroup podbook nil
  "Manage Livebook instances in Kubernetes."
  :group 'applications)

(defcustom podbook-configurations nil
  "Configurations for various products/environments."
  :type '(alist :key-type symbol :value-type plist)
  :group 'podbook)

(defcustom podbook-kubectl "kubectl"
  "The kubectl command."
  :type 'string
  :group 'podbook)

(defcustom podbook-livebook-image "ghcr.io/livebook-dev/livebook"
  "The Docker image to use for running Livebook."
  :type 'string
  :group 'podbook)

(defcustom podbook-wait-timeout "60s"
  "The timeout for waiting for a pod to become ready."
  :type 'string
  :group 'podbook)

(defcustom podbook-release-distribution "name"
  "The value for the RELEASE_DISTRIBUTION environment variable."
  :type 'string
  :group 'podbook)

(defcustom podbook-release-node-name "livebook@127.0.0.1"
  "The value for the RELEASE_NODE environment variable inside the Livebook container."
  :type 'string
  :group 'podbook)

(cl-defstruct podbook
  "podbook configuration object"
  exec
  container
  port
  iframe
  livebook
  directory
  label)

;;;###autoload
(defun podbook (&optional obj)
  "Start or connect to a podbook."
  (interactive)
  (let ((obj (or obj (podbook--choose-configuration))))
    (if (podbook-running-p obj)
        (podbook--do-post-startup obj)
      (podbook--do-startup obj))))

;;;###autoload
(defun podbook-copy-file (file &optional obj)
  "Copy a file into a livebook container."
  (interactive "fFile: ")
  (let* ((obj (or obj (podbook--choose-configuration)))
         (pod (podbook-livebook obj))
         (cmd (format "%s cp %s %s:/data"
                      podbook-kubectl file pod)))
    (shell-command cmd)))

;;;###autoload
(defun podbook-copy-directory (&optional dir obj)
  "Copy an entire directory to the livebook container."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (default (expand-file-name (podbook-directory obj)))
         (dir (or dir (read-directory-name "Directory: " default)))
         (pod (podbook-livebook obj))
         (cmd (format "%s cp %s %s:/data"
                      podbook-kubectl (expand-file-name dir) pod)))
    (shell-command cmd)))

;;;###autoload
(defun podbook-pull-livebooks (&optional dir obj)
  "Copy the /data directory from a livebook container."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (default (expand-file-name (podbook-directory obj)))
         (dir (or dir (read-directory-name "Directory: " default)))
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

;;;###autoload
(defun podbook-running-p (&optional obj)
  "Returns t if the podbook is running."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (format "%s get pod %s -o jsonpath=\"{.status.phase}\""
                      podbook-kubectl (podbook-livebook obj))))
    (string= (shell-command-to-string cmd) "Running")))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun podbook-start (&optional obj)
  "Start a Livebook."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (podbook--start-command obj))
         (res (shell-command-to-string cmd)))
    (podbook--interactive-results res)))

;;;###autoload
(defun podbook-stop (&optional obj)
  "Stop a Livebook."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (format "%s delete pod %s"
                      podbook-kubectl (podbook-livebook obj))))
    (shell-command cmd)))

;;;###autoload
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

;;;###autoload
(defun podbook-wait (&optional obj)
  "Wait for a livebook to become available; blocks."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (cmd (concat
               (format "%s wait --for=condition=ready pod/%s "
                       podbook-kubectl
                       (podbook-livebook obj))
               (format "--timeout %s" podbook-wait-timeout))))
    (shell-command cmd)))

;;;###autoload
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

;;;###autoload
(defun podbook-browse (&optional obj)
  "Open the browser to Livebook instance."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (url (podbook-url obj)))
    (if url
        (browse-url url)
      (message "Unable to find URL."))))

;;;###autoload
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

;;;###autoload
(defun podbook-stop-port-forward (&optional obj)
  "Stop a port forwarding session."
  (interactive)
  (let* ((obj (or obj (podbook--choose-configuration)))
         (session (podbook-port-forward-session obj)))
    (when session
      (detached-kill-session session t))))

;;;###autoload
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
   (format "--image=%s " podbook-livebook-image)
   (format "--env LIVEBOOK_DEFAULT_RUNTIME=\"attached:%s:%s\" "
           (podbook-release-node obj)
           (podbook-release-cookie obj))
   (format "--env RELEASE_DISTRIBUTION=\"%s\" " podbook-release-distribution)
   (format "--env LIVEBOOK_PORT=%d " (podbook-port obj))
   (format "--env LIVEBOOK_IFRAME_PORT=%d " (podbook-iframe obj))
   (format "--env RELEASE_NODE=\"%s\"" podbook-release-node-name)))

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
