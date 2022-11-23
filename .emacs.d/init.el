  (setq package-list '(shackle company org ox-reveal which-key diminish magit irony company-irony
                               irony-eldoc rtags cmake-ide rainbow-mode rainbow-delimiters ws-butler
                               impatient-mode tramp flycheck git-commit git-messenger company-prescient no-littering))

  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("elpa" . "https://elpa.gnu.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/")))

  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))



  (defmacro defkeys (mapname &rest body)
    `(let ((defs '(,@body)))
       (while defs
         (define-key ,mapname
           (if (vectorp (car defs))
               (car defs)
             (read-kbd-macro (car defs)))
           (if (or (listp (cadr defs)) (functionp (cadr defs)))
               (cadr defs)
             (if `(keymapp (bound-and-true-p ,(cadr defs)))
                 (eval (cadr defs)))))
         (setq defs (cddr defs)))))



  (server-start)



  (defun win_setup ()
    (if (not (file-exists-p "emacs.bat"))
        (progn (with-temp-file "emacs.bat"
                 (insert "@echo off\n\"C:\\Program Files (x86)\\emacs\\bin\\runemacs.exe\" -q -l \"C:\\.emacs.d\\init.el\""))
               (setenv "PATH"
                       (concat
                        "C:\\Windows\\system32" path-separator
                        "C:\\.emacs.d\\" path-separator
                        ))
               (shell-command (format "setx PATH %s"(getenv "PATH")))
               )
      )
    )

  (cond  ((string-equal system-type "windows-nt") 'win_setup)
         ((string-equal system-type "darwin") nil)
         ((string-equal system-type "gnu/linux") nil)
         ((string-equal system-type "gnu/kfreebsd") nil)
         ((string-equal system-type "berkeley-unix") nil)
         ((string-equal system-type "cygwin") nil)
         )



  (setq-default coding-system-for-read    'utf-8)
  (setq file-name-coding-system           'utf-8)
  (set-selection-coding-system            'utf-8)
  (set-keyboard-coding-system        'utf-8-unix)
  (set-terminal-coding-system             'utf-8)
  (prefer-coding-system                   'utf-8)



  (setq auto-mode-alist
        (append '(("\\.txt$" . indented-text-mode)
                  ("\\`/tmp/fol/" . text-mode)
                  ("\\.texinfo\\'" . texinfo-mode)
                  ("\\.texi\\'" . texinfo-mode)
                  ("\\.el\\'" . emacs-lisp-mode)
                  ("\\.c\\'" . c-mode)
                  ("\\.h\\'" . c-mode)
                  ("\\.gen$" . gen-mode)
                  ("\\.ms$" . fundamental-mode)
                  ("\\.mm$" . objc-mode)
                  ("\\.m$" . objc-mode)
                  ("\\.sh\\'" . shell-script-mode)
                  ("\\.jpe?g\\'" . image-mode)
                  ("\\.bat\\'" . bat-mode)
                  ("\\.[Bb][Aa][Tt]\\'" . bat-mode)
                  ("\\.\\(BAT\\||bat\\)\\'" . bat-mode)
                  ("_EDITMSG\\'" . log-entry-mode)
                  ("/cvs[[:alnum:]]*\\'" . log-entry-mode))
                auto-mode-alist))


  (require 'which-key)
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1)



  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)



  (setq inhibit-startup-message t
        visible-bell t
        next-line-add-newlines nil
        truncate-partial-width-windows nil
        ring-bell-function 'ignore
        gc-cons-threshold 50000000)

  (scroll-bar-mode -1)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (set-fringe-mode 10)
  (menu-bar-mode 0)
  (display-time-mode t)
  (display-battery-mode t)
  (blink-cursor-mode 0)
  (global-hl-line-mode 1)

  (custom-set-variables
   '(auto-save-default nil)
   '(auto-save-interval 0)
   '(auto-save-list-file-name nil)
   '(auto-save-list-file-prefix nil)
   '(auto-save-timeout 0)
   '(delete-auto-save-files nil)
   '(delete-old-versions (quote other))
   '(kept-new-versions 5)
   '(kept-old-versions 5)
   '(make-backup-file-name-function (quote ignore))
   '(make-backup-files nil)
   '(mouse-wheel-progressive-speed nil)
   '(mouse-wheel-scroll-amount (quote (15)))
   '(version-control nil))

  (setq undo-limit 20000000
        undo-strong-limit 40000000)


  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse 't
        scroll-step 3
        use-dialog-box nil)


  (column-number-mode)
  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  conf-mode-hook
                  org-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))


  (setq large-file-warning-threshold nil)


  (setq vc-follow-symlinks t)


  (setq ad-redefinition-action 'accept)


  (setq-default indent-tabs-mode nil
                tab-always-indent nil
                tab-width 4)



  (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 110 :foreground "#fdf4c1" :background "#282828")
  (set-face-attribute 'cursor t :background "#fdf4c1")
  (set-face-attribute 'highlight t :background "#333333")
  (set-face-attribute 'hl-line t :background "#333333")
  (set-face-attribute 'fringe t :background "#282828")
  (set-face-attribute 'mode-line t :foreground "#262626" :background "#FE8019")
  (set-face-attribute 'region t :background "#504945")
  (set-face-attribute 'secondary-selection t :background "#3e3834")
  (set-face-attribute 'font-lock-builtin-face t :foreground "#fe8019")
  (set-face-attribute 'font-lock-comment-face t :foreground "#7c6f64")
  (set-face-attribute 'font-lock-function-name-face t :foreground "#a99865")
  (set-face-attribute 'font-lock-keyword-face t :foreground "#dd6f48")
  (set-face-attribute 'font-lock-string-face t :foreground "#429489")
  (set-face-attribute 'font-lock-type-face t :foreground "#66999d")
  (set-face-attribute 'font-lock-constant-face t :foreground "#bbaa97")
  (set-face-attribute 'font-lock-variable-name-face t :foreground "#83a598")
  (set-face-attribute 'minibuffer-prompt t :foreground "#75b45c" :bold t)
  (set-face-attribute 'font-lock-warning-face t :foreground "#ff0000" :bold t)



  (customize-set-variable 'display-time-string-forms
                          '((propertize (concat dayname
                                                " " 12-hours ":" minutes " " (upcase am-pm)))))



  (display-time-update)



  (setq global-mode-string (delq 'display-time-string global-mode-string))



  (setq global-mode-string (delq 'battery-mode-line-string global-mode-string))



  (defun mode-line-fill (reserve)
    "Return empty space using FACE and leaving RESERVE space on the right."
    (unless reserve
      (setq reserve 20))
    (when (and window-system
               (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (propertize " "
                'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

  (customize-set-variable 'mode-line-format
                          '("%e"
                            mode-line-front-space
                            mode-line-client
                            mode-line-remote
                            mode-line-mule-info
                            mode-line-modified
                            "  "
                            ;; Buffer name
                            (:propertize "%b " 'face nil)

                            ;; line and column
                            "["
                            (:propertize "%l" 'face nil)
                            ","
                            (:propertize "%c" 'face nil)
                            "]"

                            ;; relative position, size of file
                            "["
                            (:propertize "%p" 'face nil)
                            "/"
                            (:propertize "%I" 'face nil)
                            "] "

                            ;; the current major mode for the buffer.
                            "["
                            (:propertize "%m" 'face nil
                                         'help-echo buffer-file-coding-system)
                            "] "

                            "["
                            ;; insert vs overwrite mode, input-method in a tooltip
                            (:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                               'face nil))

                            ;; was this buffer modified since the last save?
                            (:eval (when (buffer-modified-p)
                                     (concat "," (propertize "Mod"
                                                             'face nil))))

                            ;; is this buffer read-only?
                            (:eval (when buffer-read-only
                                     (concat "," (propertize "RO"
                                                             'face nil))))
                            "] "

                            ;; Version control
                            (:eval (when vc-mode
                                     (concat " " vc-mode)))

                            (:eval (mode-line-fill (+ (length battery-mode-line-string)
                                                      3
                                                      (length display-time-string))))
                            battery-mode-line-string
                            " "
                            display-time-string
                            ))



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
  (diminish 'eldoc-mode)
  (diminish 'company-mode)



  ;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
  (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
        url-history-file (expand-file-name "url/history" user-emacs-directory))

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

  (setq ido-everywhere t
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window)

  (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)
  (global-set-key (kbd "C-x C-f") 'ido-find-file)




  (setq-default fill-column 80)
  (defun org_mode_setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (setq evil-auto-indent nil)
    (diminish org-indent-mode))

  (require 'org)
  (add-hook 'org-mode-hook 'org_mode_setup)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)))



  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("em" . "src"))



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



  (require 'rainbow-mode)
  (add-hook 'org-mode-hook 'rainbow-mode)

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

  (defkeys global-map
    "C-x g" magit-status
    "C-x M-g" magit-dispatch
    "C-x G" git-messenger:popup-message)



  (require 'company)

  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "M->") 'company-select-last)
  (define-key company-active-map (kbd "M-<") 'company-select-first)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key prog-mode-map (kbd "<tab>") 'company-indent-or-complete-common)

  (setq company-idle-delay 0.0
        company-async-timeout 15
        company-tooltip-align-annotations t
        company-show-numbers t
        company-global-modes '(not shell-mode eaf-mode)
        company-require-match 'never
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2)

  (company-mode 1)

  (with-eval-after-load 'company
    (require 'company-prescient)
    (company-prescient-mode 1)
    (require 'company-irony))



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


(load "./include/c.el")
(provide 'init)
;;; init.el ends here
