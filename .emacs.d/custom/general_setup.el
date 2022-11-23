(server-start)

(setq package-list '(shackle org ox-reveal which-key diminish magit rainbow-mode rainbow-delimiters ws-butler
                             impatient-mode tramp flycheck git-commit git-messenger no-littering))

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

(defkeys global-map
  "C-x g" magit-status
  "C-x M-g" magit-dispatch
  "C-x G" git-messenger:popup-message)

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

