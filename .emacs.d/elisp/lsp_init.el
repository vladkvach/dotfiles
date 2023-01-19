(require 'lsp-mode)

(setq company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1
      lsp-enable-which-key-integration t)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'css-mode-hook #'lsp)
(add-hook 'html-mode-hook #'lsp)
(add-hook 'json-mode-hook #'lsp)
(add-hook 'yaml-mode-hook #'lsp)
(add-hook 'shell-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'dockerfile-mode-hook #'lsp)