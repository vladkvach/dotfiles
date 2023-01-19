(with-eval-after-load 'lsp-mode
    (require 'company)
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay 0.0)
    (setq company-minimum-prefix-length 1))

(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(define-key lsp-mode-map (kbd "<tab>") 'company-indent-or-complete-common)