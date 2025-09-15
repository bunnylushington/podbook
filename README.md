# podbook.el

> An Emacs helper for managing Elixir Livebook instances in Kubernetes.

This package provides a simple interface for starting, stopping, and connecting to Elixir Livebook pods running in a Kubernetes cluster. It is designed for projects where you want to attach a Livebook instance to an already-running application pod.

## Installation

Ensure that the dependencies (`s.el` and `detached.el`) are installed from MELPA or your preferred package source.

Place `podbook.el` in a directory on your Emacs `load-path` and configure the required variables in your `init.el`.

## Configuration

Configuration requires two global variables to be set:

1.  `podbook-contexts`: An association list (alist) that maps a short, memorable symbol (e.g., `:staging`) to your full Kubernetes context name.
2.  `podbook-configurations`: A property list (plist) defining the specifics for each project you want to connect to.

### Example

```elisp
;; 1. Define your Kubernetes contexts
(setq podbook-contexts
      '((:staging . "gke_my-project_us-central1-a_staging-cluster")
        (:production . "gke_my-project_us-central1-a_production-cluster")))

;; 2. Define your project configurations
(setq podbook-configurations
      '(:stg-app-alpha
        (:exec "app_alpha" :container "app-alpha-container"
               :directory "~/projects/app-alpha/notebooks"
               :context-id :staging
               :port 10000 :label "app-alpha")
        :prod-app-beta
        (:exec "app_beta" :container "app-beta-container"
               :directory "~/projects/app-beta/notebooks"
               :context-id :production
               :port 20000 :label "app-beta")))
```

### Configuration Attributes

- `:exec`: The name of the executable for the Elixir release.
- `:container`: The base name for the new Livebook pod.
- `:port`: The local and remote port to use for the Livebook instance.
- `:directory`: The default local directory for file synchronization.
- `:label`: A substring used to identify the running application pod.
- `:context-id`: A symbol that maps to a context in your `podbook-contexts` alist.

## Usage

Once configured, run `M-x podbook`.

This will prompt you to select one of the projects you defined. It will then use the appropriate Kubernetes context and attempt to start or connect to the Livebook instance.

## Available Commands

- `podbook`: The main entry point. Starts or connects to a Livebook instance.
- `podbook-stop`: Stops a running Livebook pod.
- `podbook-browse`: Opens the URL for a running Livebook in your browser.
- `podbook-copy-file`: Copies a local file to the Livebook container.
- `podbook-copy-directory`: Copies a local directory to the livebook container.
- `podbook-pull-livebooks`: Copies the `/data` directory from the Livebook container to your local machine.

## Dependencies

- [s.el](https://github.com/magnars/s.el) (String manipulation library)
- [detached.el](https://github.com/jimeh/emacs-detached) (For running background processes)