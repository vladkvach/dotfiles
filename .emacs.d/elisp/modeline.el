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