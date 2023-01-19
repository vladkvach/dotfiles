(require 'lsp-mode)

(setq company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1
      lsp-enable-which-key-integration t
      lsp-headerline-breadcrumb-enable nil
      lsp-headerline-breadcrumb-enable nil
      lsp-modeline-code-actions-enable nil
      lsp-modeline-diagnostics-enable nil
      lsp-semantic-tokens-enable t ;; hide unreachable ifdefs
      ;; LSP UI related tweaks
      lsp-ui-doc-show-with-cursor t
      lsp-ui-doc-show-with-mouse nil
      lsp-ui-sideline-enable nil
      lsp-ui-sideline-show-hover nil
      lsp-ui-sideline-show-symbol nil
      lsp-ui-sideline-show-diagnostics nil
      lsp-ui-sideline-show-code-actions nil)

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
