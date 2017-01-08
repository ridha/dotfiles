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

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(foreground-color . "green2"))
(add-to-list 'default-frame-alist '(background-color . "black"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Emacs Functionality

(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin/"))

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
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-milkbox" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)

(use-package auto-complete)
(use-package yaml-mode)

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
(elpy-use-ipython)

(setq flymake-gui-warnings-enabled nil)

;;; .emacs ends here
