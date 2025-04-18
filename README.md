## Emacs configuration files

This is my current emacs configuration - it's a simple system that
supports multiple "installations" from different machines.

### Some packages that are available

* `projectile`
* `helm`
* `dracula-theme`
* `paredit`
* `magit`
* `treemacs`
* `company-mode`

## Setup

### Option 1: use the default installation
1. Copy the example init file:
  ```sh
  $ cp init.example.el init.el
  ```

### Option 2: make your own
1. Copy an existing file in the `installations` directory and tweak
   it.

2. Load that installation from the `init.el` file.
  ```elisp
  (load "~/.emacs.d/installations/your-installation.el")
  ```

## Background

The way it works is that `modules.el` defines a bunch of macros for
different bits of functionality (e.g. clojure-mode and key bindings)
without explicitly executing them.

Then each machine has a script in the "installations" folder that
includes all the features it wants to use.

The reason they are macros is because `use-package` wants to run at
the top-level, so you can't just run it within a regular function. So
these macros essentially check if the given feature is enabled, and
execute the body of the feature at the top level if so. That way, the
features that are included actually execute at macroexpansion time.

![screenshots](images/emacs-screenshot.png)
