;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(
    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex
    
    ;; Provides Emacs font-lock, indentation, and navigation for the Clojure programming language
    ;; Walkthroughs are available at clojure-doc.org and Clojure for the Brave and the True
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode
    
    ;; Smartparens is minor mode for Emacs that deals with parens pairs and tries to be smart about it
    ;; https://github.com/Fuco1/smartparens
    smartparens
    
    ;; rainbow-delimiters is a "rainbow parentheses"-like mode which highlights delimiters
    ;; such as parentheses, brackets or braces according to their depth.
    ;; https://github.com/Fanael/rainbow-delimiters
    rainbow-delimiters
    
    ;; CIDER (formerly nrepl.el) is the Clojure Interactive Development Environment that Rocks for Emacs,
    ;; built on top of nREPL, the Clojure networked REPL server.
    ;; https://github.com/clojure-emacs/cider
    cider
    
    ;; Company is a text completion framework for Emacs.
    ;; http://company-mode.github.io/
    company))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.emacs.d/theme")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
;; https://github.com/technomancy/better-defaults
(load "better-defaults.el")

;; Snazzy theme
;; https://github.com/jimeh/tomorrow-night-paradise-theme.el
(load "tomorrow-night-paradise-theme.el")

;; clojure-mode hooks

;; parens support
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

;; Line numbers
(add-hook 'clojure-mode-hook #'linum-mode)

;; CIDER config

;;  enable parameter hints
(add-hook 'cider-mode-hook #'eldoc-mode)

;; show nrepl port
(setq nrepl-buffer-name-show-port t)

;; result prefix in the REPL
(setq cider-repl-result-prefix ";; => ")

;; auto-complete
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)

;; Load the default config for smartparens
;; I'm loading it because it disables matching '
;; and it probably does some other neat stuff but I haven't looked at it~
(require 'smartparens-config)

;; don't show the "active processes" warning when trying to exit emacs
(require 'cl)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
           (flet ((process-list ())) ad-do-it))

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
