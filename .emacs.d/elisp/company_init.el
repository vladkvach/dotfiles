(with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
    (require 'company)
    (add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle))