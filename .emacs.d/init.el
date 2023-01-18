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

(dolist (dir (recursive-subdirs "~/.emacs.d/elisp" t))
  (dolist (file (directory-files dir t "\.el$" nil))
    (load (file-name-sans-extension file))))

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

(global-auto-revert-mode 1)

(provide 'init)

;;; init.el ends here
