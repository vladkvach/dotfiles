; (add-to-list 'load-path "~/.emacs.d/custom")

(mapc 'load (file-expand-wildcards "~/.emacs.d/custom/*.el"))

(require 'which-key)
(setq which-key-idle-delay 0.3)
(which-key-mode 1)

(require 'diminish)

(diminish 'which-key-mode)
(diminish 'hungry-delete-mode)
(diminish 'beacon-mode)
(diminish 'rainbow-mode)
(diminish 'super-save-mode)
(diminish 'visual-line-mode)
(diminish 'org-indent-mode)
(diminish 'ws-butler-mode)
(diminish 'buffer-face-mode)


;; Use no-littering to automatically set common paths to the new user-emacs-directory
(require 'no-littering)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)


(require 'ws-butler)
(add-hook 'text-mode-hook 'ws-butler-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)


(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (let* ((file-name (buffer-file-name))
           (file-root (if (string-match "/ssh:\\([^:]+\\):\\(.*\\)" file-name)
                          (concat "/ssh:"  (match-string 1 file-name)
                                  "|doas:" (match-string 1 file-name)
                                  ":"      (match-string 2 file-name))
                        (concat "/doas:localhost:" file-name))))
      (find-alternate-file file-root))))

(require 'ido)
(ido-mode 1)

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)


(require 'ox-reveal)

(global-auto-revert-mode 1)


; CC++ mode handling
(defun vk_c_hook ()
; Abbrevation expansion
  (abbrev-mode 1)

  (defun vk_header_format ()
    "Format the given file as a header file."
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "#if !defined(")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H)\n")
    (insert "/* ========================================================================\n")
    (insert " * $File: $\n")
    (insert " * $Date: $\n")
    (insert " * $Revision: $\n")
    (insert " * $Creator: Vladislav Kvach $\n")
    (insert " * ========================================================================\n")
    (insert " */")
    (insert "\n")
    (insert "#define ")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H\n")
    (insert "#endif"))

  (defun vk_source_format ()
    "Format the given file as a source file."
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "/* ========================================================================\n")
    (insert " * $File: $\n")
    (insert " * $Date: $\n")
    (insert " * $Revision: $\n")
    (insert " * $Creator: Vladislav Kvach $\n")
    (insert " * ========================================================================\n")
    (insert " */")
    (insert "\n"))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]c" buffer-file-name) (vk_source_format))
        ((string-match "[.]h" buffer-file-name) (vk_header_format)))
  )

(add-hook 'c-mode-common-hook 'vk_c_hook)


(require 'flycheck)

(require 'impatient-mode)


(require 'tramp)
(setq tramp-default-method "ssh"
      tramp-persistency-file-name (concat no-littering-var-directory "tramp")
      tramp-auto-save-directory (concat no-littering-var-directory "tramp-autosave")
      tramp-verbose 10
      tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>] *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(tramp-set-completion-function "ssh" '((tramp-parse-sconfig "/etc/ssh_config")
                                       (tramp-parse-sconfig "~/.ssh/config")))


(require 'magit)
(require 'git-commit)
(require 'git-messenger)

(add-hook 'git-commit-mode-hook 'my-american-dict)

(setq git-messenger:show-detail t
      git-messenger:use-magit-popup t)


(require 'rainbow-mode)
(add-hook 'org-mode-hook 'rainbow-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)


(require 'shackle)
(setq shackle-default-size 0.4
      shackle-rules
      '(("*Calendar*" :select t :size 0.3 :align below)
        ("*Compile-Log*" :ignore t)
        ("*Completions*" :size 0.3  :align t)
        ("*Help*" :select t :inhibit-window-quit t :other t)
        ("*Messages*" :select nil :inhibit-window-quit t :other t)
        ("*Process List*" :select t :size 0.3 :align below)
        ("*Python*" :select t :size 0.3 :align bellow)
        ("*Shell Command Output*" :select nil)
        ("*Warnings*" :ignore t)
        ("*el-get bootstrap*" :ignore t)
        ("*undo-tree*" :size 0.25 :align left)
        ("\\*Async Shell.*\\*" :regexp t :ignore t)
        ("\\*[Wo]*Man.*\\*" :regexp t :select t :inhibit-window-quit t :other t)
        ("\\*poporg.*\\*" :regexp t :select t :other t)
        ("\\*shell*\\*" :regexp t :same t :select t :other t)
        ("\\`\\*ivy.*?\\*\\'" :regexp t :size 0.3 :align t)
        ("edbi-dbviewer" :regexp t :select t :same t)
        ("*edbi:query-result" :regexp t :size 0.8 :align bellow)
        (occur-mode :select nil :align t)
        (pdf-view-mode :other t)
        (compilation-mode :select nil)
        ("\\*Apropos\\|Help\\|Occur\\|tide-references\\*" :regexp t :same t :select t :inhibit-window-quit t)
        ("\\*magit" :regexp t :same t :select t)
        ("\\*PowerShell.*" :regexp t :same t :select t)
        ("*go-guru-output*" :select t :same t)
        ("*Proced*" :select t :same t)
        ("\\*Pp Eval" :regexp t :same nil :select t :other t)
        ("*slime-source*" :select nil :same nil :other t)
        ("*slime-description*" :select nil :other t :inhibit-window-quit t)
        ("\\*slime-repl" :regexp t :same nil :select nil :other t)
        ("\\*sldb" :regexp t :other t :inhibit-window-quit t :select t)
        ("\\*slime-compilation" :regexp t :same nil :select nil :other t)
        ("*slime-scratch*" :same nil :select t :other t)
        ("*ert*" :select nil :same nil :other t)
        ("*sesman CIDER browser*" :inhibit-window-quit t :select t :same t)
        ("\\*cider-repl" :regexp t :same nil :other t)
        ("*Buffer List*" :select t :same t)))
(shackle-mode 1)

(provide 'init)
;;; init.el ends here
