# podbook.el

> An Emacs helper for managing Elixir Livebook instances in Kubernetes.

This package provides a simple interface for starting, stopping, and connecting to Elixir Livebook pods running in a Kubernetes cluster. It is designed for projects where you want to attach a Livebook instance to an already-running application pod.

## Installation

Ensure that the dependencies (`s.el` and `detached.el`) are installed from MELPA or your preferred package source.

Place `podbook.el` in a directory on your Emacs `load-path` and add the following configuration to your `init.el`.

### use-package

```elisp
(use-package podbook
  :commands (podbook podbook-stop)
  :config
  (setq podbook-configurations
        '(:my-first-project
          (:exec "my_app_a"
           :container "my-app-a-container"
           :port 8080
           :directory "~/projects/my-app-a/notebooks"
           :label "my-app-a-prod")
         :my-second-project
         (:exec "my_app_b"
          :container "my-app-b-container"
          :port 8090
          :directory "~/projects/my-app-b/notebooks"
          :label "my-app-b-prod"))))
```

## Configuration

The `podbook-configurations` variable is a property list (plist) where each key is a symbol identifying a project, and the value is another plist containing the following attributes:

- `:exec`: The name of the executable for the Elixir release (e.g., "my_app").

- `:container`: The base name for the new Livebook pod. The final pod name will be this value with "-livebook" appended (e.g., if you provide "my-app", the pod will be named "my-app-livebook").

- `:port`: The local and remote port to use for the Livebook instance. The required iframe port will be automatically set to `:port + 1`.

- `:directory`: The default local directory for file synchronization with the Livebook container.

- `:label`: A substring used to identify the running application pod you want to attach to. For example, if your pods are named `my-app-prod-a1b2c3d4`, you could use `my-app-prod` as the label.

## Usage

Once configured, run `M-x podbook`.

This will prompt you to select one of the projects you defined in `podbook-configurations`. It will then check if a Livebook pod is already running for that project. If so, it will connect to it. If not, it will attempt to start a new one by attaching to your running application pod.

## Available Commands

- `podbook`: The main entry point. Starts or connects to a Livebook instance.
- `podbook-stop`: Stops a running Livebook pod.
- `podbook-browse`: Opens the URL for a running Livebook in your browser.
- `podbook-copy-file`: Copies a local file to the Livebook container.
- `podbook-copy-directory`: Copies a local directory to the Livebook container.
- `podbook-pull-livebooks`: Copies the `/data` directory from the Livebook container to your local machine.

## Dependencies

- [s.el](https://github.com/magnars/s.el) (String manipulation library)
- [detached.el](https://github.com/jimeh/emacs-detached) (For running background processes)
