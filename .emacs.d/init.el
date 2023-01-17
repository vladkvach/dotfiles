;;; package --- Summary
;;; Commentary:
;;; init.el
;;; Code:

(server-start)

(setq package-list '(diminish rainbow-delimiters ws-butler tramp lsp-mode lsp-ui consult vertico orderless company flycheck))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(defun recursive-subdirs (directory &optional withroot)
  "Return a unsorted list of names of directories in DIRECTORY recursively.
If WITHROOT is non-nil, also DIRECTORY will be include."
  (let (subdirs)
    (dolist (element (directory-files-and-attributes directory nil nil nil))
      (let* ((path (car element))
             (isdir (car (cdr element)))
             (ignore (or (string= path ".") (string= path ".."))))
        (if (and (eq isdir t) (not ignore))
            (let ((dir (concat directory "/" path)))
              (setq subdirs (append (cons dir subdirs)
                                    (recursive-subdirs dir)))))))
    (if (not (eq withroot nil))
        (add-to-list 'subdirs directory))
    subdirs))

(dolist (dir (recursive-subdirs "~/.emacs.d/config" t))
  (dolist (file (directory-files dir t "\.el$" nil))
    (load (file-name-sans-extension file))))

(setq-default coding-system-for-read    'utf-8)
(setq file-name-coding-system           'utf-8)
(set-selection-coding-system            'utf-8)
(set-keyboard-coding-system        'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; OS settings
(defun win_setup ()
  (if (not (file-exists-p "emacs.bat"))
      (progn (with-temp-file "emacs.bat"
               (insert "@echo off\n\"C:\\Program Files (x86)\\emacs\\bin\\runemacs.exe\" -q -l \"C:\\.emacs.d\\init.el\""))
             (setenv "PATH"
                     (concat
                      "C:\\Windows\\system32" path-separator
                      "C:\\.emacs.d\\" path-separator
                      ))
             (shell-command (format "setx PATH %s"(getenv "PATH"))))))

(cond  ((string-equal system-type "windows-nt") 'win_setup)
       ((string-equal system-type "darwin") nil)
       ((string-equal system-type "gnu/linux") nil)
       ((string-equal system-type "gnu/kfreebsd") nil)
       ((string-equal system-type "berkeley-unix") nil)
       ((string-equal system-type "cygwin") nil))

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq inhibit-startup-message t
      visible-bell t
      next-line-add-newlines nil
      truncate-partial-width-windows nil
      ring-bell-function 'ignore
      gc-cons-threshold 50000000)

(setq-default truncate-lines t)
(split-window-horizontally)

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

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 3
      use-dialog-box nil
      undo-limit 20000000
      undo-strong-limit 40000000
      large-file-warning-threshold nil
      vc-follow-symlinks t
      ad-redefinition-action 'accept)

(setq-default indent-tabs-mode nil
              tab-always-indent nil
              tab-width 4)

(column-number-mode)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
(diminish 'hungry-delete-mode)
(diminish 'beacon-mode)
(diminish 'super-save-mode)
(diminish 'visual-line-mode)
(diminish 'ws-butler-mode)
(diminish 'buffer-face-mode)

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

(global-auto-revert-mode 1)

;; C++ indentation style
(defconst vk/c_style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t)))

;; CC++ mode handling
(defun vk_c_hook ()
  "Source Initialization Hook."

  ;; Set my style for the current buffer
  (c-add-style "vk/c_style" vk/c_style t)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; Abbrevation expansion
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
        ((string-match "[.]h" buffer-file-name) (vk_header_format))))

(add-hook 'c-mode-common-hook 'vk_c_hook)

(require 'flycheck)
(global-flycheck-mode)

(require 'tramp)
(setq tramp-default-method "ssh"
      tramp-verbose 10
      tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>] *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(tramp-set-completion-function "ssh" '((tramp-parse-sconfig "/etc/ssh_config")
                                       (tramp-parse-sconfig "~/.ssh/config")))


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(require 'consult)
(setq completion-styles '(substring basic))

(provide 'init)

;;; init.el ends here
