** Irony
#+begin_src emacs-lisp
(defun vk_irony_mode_hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'vk_irony_mode_hook)

(require 'irony)

;; If irony server was never installed, install it.
(unless (irony--find-server-executable) (call-interactively #'irony-install-server))

(add-hook 'c-mode-hook 'irony-mode)

;; Use compilation database first, clang_complete as fallback.
(setq-default irony-cdb-compilation-databases '(irony-cdb-libclang irony-cdb-clang-complete))

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(irony-mode 1)
#+end_src

** Eldoc
#+begin_src emacs-lisp
(require 'irony-eldoc)
(add-hook 'irony-mode-hook #'irony-eldoc)
#+end_src

** rtags
#+begin_src emacs-lisp
(require 'rtags)

(rtags-enable-standard-keybindings)
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(define-key c-mode-base-map (kbd "M-.")
  (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,")
  (function rtags-find-references-at-point))
#+end_src

** cmake-ide
#+begin_src emacs-lisp
(cmake-ide-setup)
#+end_src

(provide 'c)