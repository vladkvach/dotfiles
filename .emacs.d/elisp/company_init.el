(with-eval-after-load 'lsp-mode
    (require 'company)
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay 0.0)
    (setq company-minimum-prefix-length 1))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle))