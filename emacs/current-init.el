(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(setq mac-mouse-wheel-smooth-scroll nil)
(setq exec-path (append exec-path '("/usr/local/bin")))

(defun setup-straight ()
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defun setup-use-package ()
  (straight-use-package 'use-package))

(defun setup-evil-mode ()
  (use-package evil
    :ensure t
    :config (evil-mode +1)))

(defun setup-style ()
  (straight-use-package '(nano-emacs
                          :type git
                          :host github
                          :repo "rougier/nano-emacs"
                          :no-byte-compile t))

  ;; Default layout (optional)
  (require 'nano-layout)

  ;; Theming Command line options (this will cancel warning messages)
  (add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
  (add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
  (add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
  (add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
  (add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
  (add-to-list 'command-switch-alist '("-compact" . (lambda (args))))

  (cond
   ((member "-default" command-line-args) t)
   ((member "-dark" command-line-args) (require 'nano-theme-dark))
   (t (require 'nano-theme-light)))

  ;; Customize support for 'emacs -q' (Optional)
  ;; You can enable customizations by creating the nano-custom.el file
  ;; with e.g. `touch nano-custom.el` in the folder containing this file.
  (let* ((this-file  (or load-file-name (buffer-file-name)))
         (this-dir  (file-name-directory this-file))
         (custom-path  (concat this-dir "nano-custom.el")))
    (when (and (eq nil user-init-file)
               (eq nil custom-file)
               (file-exists-p custom-path))
      (setq user-init-file this-file)
      (setq custom-file custom-path)
      (load custom-file)))

  ;; Theme
  (require 'nano-faces)
  (nano-faces)

  (require 'nano-theme)
  (nano-theme)

  (require 'nano-colors)
  
  ;; Nano default settings (optional)
  (require 'nano-defaults)

  ;; Nano session saving (optional)
  (require 'nano-session)

  ;; Nano header & mode lines (optional)
  (require 'nano-modeline)

  ;; Nano key bindings modification (optional)
  (require 'nano-bindings)

  ;; Compact layout (need to be loaded after nano-modeline)
  (when (member "-compact" command-line-args)
    (require 'nano-compact))
  
  ;; Nano counsel configuration (optional)
  ;; Needs "counsel" package to be installed (M-x: package-install)
  ;; (require 'nano-counsel)

  ;; Welcome message (optional)
  (let ((inhibit-message t))
    (message "Welcome to GNU Emacs / N Λ N O edition")
    (message (format "Initialization time: %s" (emacs-init-time))))

  ;; Splash (optional)
  (unless (member "-no-splash" command-line-args)
    (require 'nano-splash))

  ;; Help (optional)
  (unless (member "-no-help" command-line-args)
    (require 'nano-help)))

(defun setup-editor ()
  (use-package magit
    :straight t)
  (use-package undo-tree
    :straight t
    :init ((lambda ()
             (global-undo-tree-mode 1)
             (global-set-key (kbd "C-z") 'undo)
             (defalias 'redo 'undo-tree-redo)
             (global-set-key (kbd "C-S-z") 'redo))))
  
  (use-package clojure-mode
    :straight t)
  (use-package cider
    :straight t
    :init ((lambda ())))
  (use-package lispy
    :straight t
    :init ((lambda ()
             (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
             (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))))))

(defun find-in-iroh ()
  (interactive)
  (find-file "/ssh:gbuisson@home.local:/mnt/dev/iroh"))

(defun connect-iroh ()
  (interactive)
  (cider-connect '("home.local" "10.0.0.1" 48372)))

(defun setup-org-mode ()
  (setq org-hide-leading-stars nil)
  (use-package org-roam
    :straight t
    :init (org-roam-mode)
    :custom
    (org-roam-directory "~/org")
    :bind (:map org-roam-mode-map
                (("C-c n l" . org-roam)
                 ("C-c n f" . org-roam-find-file)
                 ("C-c n g" . org-roam-graph))
                :map org-mode-map
                (("C-c n i" . org-roam-insert))
                (("C-c n I" . org-roam-insert-immediate))))
  (use-package ob-http
    :straight t)
  (use-package ox-gfm
    :straight t)

  (use-package toc-org
    :straight t
    :config (lambda ()
              (add-hook 'org-mode-hook 'toc-org-mode)))
  (use-package plantuml-mode
    :straight t
    :config ((lambda ()
               (setq org-plantuml-jar-path (expand-file-name "/usr/local/opt/plantuml/libexec/plantuml.jar"))
               (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
               (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))))))
(defun setup-term ()
  (use-package vterm
    :straight '(vterm :source (melpa gnu-elpa-mirror))))

(defun setup-autocompletion ()
  (use-package counsel
    :straight t)
  (use-package prescient
    :straight t)
  (use-package selectrum-prescient
    :straight t)
  (use-package company-prescient
    :straight t)

  (use-package company
    :straight t
    :config (global-company-mode))
  
  (use-package selectrum
    :straight t
    :config ((lambda ()
               (selectrum-mode +1)
               (selectrum-prescient-mode +1)
               (prescient-persist-mode +1)))))

(setup-straight)
(setup-use-package)
(setup-autocompletion)
(setup-style)
(setup-editor)
(setup-org-mode)
(setup-term)

