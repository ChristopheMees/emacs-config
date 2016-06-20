;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

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
    company

    ;; Improved JavaScript editing mode for GNU Emacs
    ;; https://github.com/mooz/js2-mode
    js2-mode

    ;; Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs,
    ;; intended as replacement for the older Flymake extension which is part of GNU Emacs.
    ;; http://www.flycheck.org/
    flycheck
    
    json-mode))

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

;; Don't show the startup message
(setq inhibit-startup-message t)

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

;; Register js files for js2-mode
(add-to-list 'auto-mode-alist ' ("\\.js$" . js2-mode))

;; Register json files for json-mode
(add-to-list 'auto-mode-alist ' ("\\.json$" . json-mode))

;; Add smartparents to js2-mode
(add-hook 'js2-mode-hook #'smartparens-strict-mode)

;; Set highlight level for js2-mode
(setq js2-highlight-level 3)

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking when editing js
(add-hook 'js2-mode-hook 'flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;;; runs eslint --fix on the current file after save
;;; alpha quality -- use at your own risk

(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "eslint --fix " (buffer-file-name))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))

(add-to-list 'load-path "~/.emacs.d/custom/emacs-gulpjs")
(require 'gulpjs)
