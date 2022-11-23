;;; c_setup.el --- c/c++ mode  

(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(company-c-headers-mode 1)


;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)


;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style) "linux" ;; set style to "linux"

(require 'cc-mode)

(define-key c-mode-map  [(tab)] 'company-complete)

(cc-mode-mode 1)





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



(require 'irony-eldoc)
(add-hook 'irony-mode-hook #'irony-eldoc)



(require 'rtags)

(rtags-enable-standard-keybindings)
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(define-key c-mode-base-map (kbd "M-.")
  (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,")
  (function rtags-find-references-at-point))


(cmake-ide-setup)

(provide 'c_setup)
;;; c_setup.el ends here
