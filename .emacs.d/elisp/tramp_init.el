(require 'tramp)
(setq tramp-default-method "ssh"
      tramp-verbose 10
      tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>] *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(tramp-set-completion-function "ssh" '((tramp-parse-sconfig "/etc/ssh_config")
                                       (tramp-parse-sconfig "~/.ssh/config")))