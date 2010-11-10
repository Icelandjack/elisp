;; This is dc-vindicia.el, routines to help out with work at Vindicia

;; The repo parameter is optional and should be "vindicia" or
;; "vindicia-www" or something like that
(defun jira-diff (revision &optional (repo "vindicia"))
  (let ((prev-revision (1- revision))
        (svn-uri (concat "https://svn.vindicia.com/svn/" repo)))
    (with-current-buffer (or (get-buffer "jira-diff")
                             (generate-new-buffer "jira-diff"))
      (erase-buffer)
      (insert (shell-command-to-string
               (format "svn diff -r %s:%s %s" prev-revision revision svn-uri)))
      (switch-to-buffer (current-buffer)))))