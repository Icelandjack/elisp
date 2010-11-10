;; This is dc-vindicia.el, routines to help out with work at Vindicia

(defun jira-diff (revision)
  (let ((prev-revision (1- revision)))
    (with-current-buffer (or (get-buffer "jira-diff")
                             (generate-new-buffer "jira-diff"))
      (erase-buffer)
      (insert
       (shell-command-to-string
        (format "svn diff -r %s:%s https://svn.vindicia.com/svn/vindicia"
                prev-revision revision)))
      (switch-to-buffer (current-buffer)))))