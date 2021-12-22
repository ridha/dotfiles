;;; Emacs configuration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User interface

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(mouse-wheel-mode 1)
(display-time-mode 1)
(column-number-mode 1)
(blink-cursor-mode -1)
(size-indication-mode 1)
(show-paren-mode t)
;; Highlight selection
(transient-mark-mode 1)
(icomplete-mode 1)
(setq show-paren-style 'parenthesis)
;; Don't ever use graphic dialog boxes
(setq use-dialog-box nil)
;; Smash the training wheels
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(foreground-color . "green2"))
(add-to-list 'default-frame-alist '(background-color . "black"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Emacs Functionality

(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Don't annoy me with backup files everywhere
(setq make-backup-files nil)
(setq auto-save-default nil)
;; Ask whether to add a final newline
(setq require-final-newline 'ask)

;; Indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin/"))

(setq shell-file-name "/bin/bash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom commands

(global-set-key "\M-g" 'goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages

(when (file-directory-p "~/.emacs.d/elisp")
  (add-to-list 'load-path "~/.emacs.d/elisp")
  (dolist (dirname (directory-files "~/.emacs.d/elisp" t "^[^.]"))
    (when (file-directory-p dirname)
      (add-to-list 'load-path dirname))))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-milkbox" . "https://stable.melpa.org/packages/")))

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)
(require 'yasnippet)

(use-package auto-complete)
(use-package yaml-mode)

(yas-global-mode 1)

;;;;;;;;;;;
;; ido-mode
(use-package ido
  :ensure t
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode +1))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-parentheses-mode

(define-globalized-minor-mode global-highlight-parentheses-mode
highlight-parentheses-mode
(lambda ()
  (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;;;;;;;;;;;;;;;;;;;;;;
;;fill-column-indicator

(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)
(setq fci-rule-column 132)
(setq fci-rule-use-dashes 1)
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")

;;;;;;;;;;;;;;
;; python-mode

(elpy-enable)
(setq elpy-disable-backend-error-display 1)

(setq flymake-gui-warnings-enabled nil)
(setq ropemacs-enable-autoimport t)

(global-auto-complete-mode t)
(add-hook 'python-mode-hook 'auto-complete-mode)

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:complete-on-dot t)

(fset 'set-pdb-trace
   [?\C-a ?\C-m up ?\C-i ?i ?m ?p ?o ?r ?t ?  ?i ?p ?d ?b ?\C-m ?i ?p ?d ?b ?. ?s ?e ?t ?_ ?t ?r ?a ?c ?e ?( ?)])
(global-set-key "\C-xa" 'set-pdb-trace)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'python-mode-hook '(lambda ()
							   (setq python-indent 4)))

(require 'py-autopep8)
(setq py-autopep8-options '("--max-line-length=132"))


;;;;;; to enable in all buffers
(require 'autopair)
(autopair-global-mode)


;;;;; flyspell-mode
(setq-default ispell-program-name "aspell")
(setq flyspell-issue-message-flag nil)
;; spell check comments in prog-mode
(dolist (mode-hook '(c-mode-hook
                     javascript-mode-hook
                     go-mode-hook
                     sql-mode-hook
                     scala-mode-hook
                     python-mode-hook
                     emacs-lisp-mode-hook))
  (add-hook mode-hook 'flyspell-prog-mode))

;;;; auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))

(setq typescript-indent-level 2)
(add-hook 'text-mode-hook 'flyspell-mode)

;;Load Go-specific language syntax
(add-hook 'before-save-hook 'gofmt-before-save)

;;Custom Compile Command
(defun go-mode-setup ()
  (linum-mode 1)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
  ;;  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (local-set-key (kbd "M-,") 'compile)
(add-hook 'go-mode-hook 'go-mode-setup)

;;Load auto-complete
(ac-config-default)
(require 'auto-complete-config)
(require 'go-autocomplete)

;;Go rename
(require 'go-rename)

;;Configure golint
(add-hook 'go-mode-hook #'flymake-go-staticcheck-enable)
(add-hook 'go-mode-hook #'flymake-mode)

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;;Smaller compilation buffer
(setq compilation-window-height 14)
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

;;Other Key bindings
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;;Compilation autoscroll
(setq compilation-scroll-output t)

(setq x-super-keysym 'meta)

(eval-after-load 'flycheck
  (progn
    (require 'flycheck-kotlin)
    (flycheck-kotlin-setup)))

(require 'flycheck-kotlin)
(flycheck-kotlin-setup)

(setenv "GO111MODULE" "on")
(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

;;; .emacs ends here
(set-frame-font "Monaco 16")
