(with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
    (require 'company)
    (add-hook 'after-init-hook 'global-company-mode))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key lsp-mode-map (kbd "<tab>") #'company-indent-or-complete-common))
