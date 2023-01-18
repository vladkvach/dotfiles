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