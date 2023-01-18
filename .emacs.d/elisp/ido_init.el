(require 'ido)
(ido-mode 1)

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)