(setq-default coding-system-for-read    'utf-8)
(setq file-name-coding-system           'utf-8)
(set-selection-coding-system            'utf-8)
(set-keyboard-coding-system        'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

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

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)
