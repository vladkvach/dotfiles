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