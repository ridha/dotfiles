;; UML
(use-package plantuml-mode)
(use-package jinja2-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kotlin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'kotlinc)
(add-to-list 'compilation-error-regexp-alist 'ktlint)

(add-to-list 'compilation-error-regexp-alist-alist
             '(kotlinc "^[ew]: \\(.*?\\): (\\([0-9]+?\\), \\([0-9]+\\)).*$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist-alist
             '(ktlint "^.*] at \\(.*?\\):\\([0-9]+?\\):\\([0-9]+?\\).*$" 1 2 3))

(use-package kotlin-mode
  :hook ((kotlin-mode . kotlin-compile-hook)
         (kotlin-mode . company-mode))
  :config (setq kotlin-tab-width 4))

(use-package ob-kotlin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gradle-mode)
(use-package groovy-mode)
