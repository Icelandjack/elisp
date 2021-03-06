;; This is dc-vindicia.el, routines to help out with work at Vindicia

;; alias repl '(progn (setf old (eshell/pwd)) nil); /ssh:vincos-5:/usr/bin;
;;       ./re.pl --rcfile /home/dcameron/t/repl.rc; $old'
;; (defun eshell/repl (&optional rcfile)
;;   (let ((rcfile (or rcfile "/ssh:vincos-5:/home/dcameron/t/repl.rc"))
;;         (old-directory (eshell/pwd)))
;;     (eshell/cd "/ssh:vincos-5:/usr/bin")
;;     (shell-command (format "./re.pl --rcfile %s" rcfile))
;;     (eshell/cd old-directory)))
        
(defun document-function ()
  (interactive)
  (insert "#
# Usage: 
#
# Purpose:
#
# Returns:
#
# Parameters:
#
# Comments: No comment.
")
  (forward-line -9)
  (goto-char (point-at-eol)))


(defun ack (regex path)
  (async-shell-command
   (concat "/root/bin/ack --perl --nocolor " regex path)))

;; Everything after this comment was obsoleted by Atlassian Software
;; (Jira, Fisheye, and Crucible).

;; ;; Use this function if you need to get diffs for one or more svn
;; ;; revision numbers. For each revision number R, this function obtains
;; ;; a diff between R-1 and R and adds it to the jira-diff buffer.  The
;; ;; repo parameter is something like "vindicia" or "vindicia-www".
;; ;; Here's an example of how to call the function:
;; ;;
;; ;;    (jira-revision-diffs "vindicia" 19107 19112)
;; ;;
;; ;; Remember to look in the "jira-diffs" buffer for the results. The
;; ;; buffer might not be visible after the function runs. This function
;; ;; can take a long time to run if there are many revision numbers or
;; ;; if the revisions are extensive. Emacs is unusable while the
;; ;; function runs.
;; (defun jira-revision-diffs (repo &rest revisions)
;;   (with-current-buffer (or (get-buffer "jira-diff")
;;                            (generate-new-buffer "jira-diff"))
;;     (erase-buffer)
;;     (let ((svn-uri (concat "https://svn.vindicia.com/svn/" repo)))
;;       (loop for rev in revisions do
;;             (insert (format "\n\n====== REVISION %s ======\n" rev))
;;             (insert (shell-command-to-string
;;                      (format "svn diff -r %s:%s %s" (1- rev) rev svn-uri)))))
;;     (goto-char (point-min)))
;;   nil)

;; ;; Use this function if you need to get diffs for all the svn revision
;; ;; numbers associated with one or more Jira (support.vindicia.com)
;; ;; issues. This function collects all the subversion revision numbers
;; ;; associated with the given Jira issue numbers and then calls
;; ;; jira-revision-diffs with the collected subversion revision
;; ;; numbers. Here's an example of how to call this function:
;; ;;
;; ;;     (jira-issue-diffs "vindicia" "CB-7142")
;; ;;
;; ;; See the comment for jira-issue-diffs for more information. This
;; ;; function can take a long time if a jira-issue has extensive changes
;; ;; associated with it.
;; (defun jira-issue-diffs (repo &rest jira-issues)
;;   (apply 'jira-revision-diffs
;;          (cons repo (jira-issue-revisions repo jira-issues))))

;; ;; This function returns a list of svn revision numbers associated
;; ;; with a Jira issue.
;; (defun jira-issue-revisions (repo jira-issues)
;;   (mapcar 'string-to-number
;;           (loop for jira-issue in
;;                 (if (listp jira-issues) jira-issues (list jira-issues)) 
;;                 append (scrape-string
;;                         ">\\([0-9]\\{5\\}\\)</td>"
;;                         (fetch-jira-subversion-commits-html jira-issue)))))

;; ;; This function logs into support.vindicia.com and fetches the HTML
;; ;; of the page (and tab) that lists the svn revisions associated with
;; ;; the given Jira issue.
;; (defun fetch-jira-subversion-commits-html (jira-issue)
;;   (http-post "https://support.vindicia.com/login.jsp"
;;              `(("os_username" ,vindicia-username)
;;                ("os_password" ,vindicia-password)
;;                ("os_cookie" "true")
;;                ("os_destination" 
;;                 ,(format
;;                   "%s/%s?%s"
;;                   "https://support.vindicia.com/browse"
;;                   jira-issue
;;                   (concat "page=com.atlassian.jira.plugin.ext"
;;                           ".subversion:subversion-commits-tabpanel"
;;                           "#issue-tabs"))))))

;; ;; Use this function if you want to get a list of all the changes that
;; ;; a given commiter made for the given release. This function is ideal
;; ;; for code reviews. It puts in one place (the jira-diff buffer) all
;; ;; the code that the commiter changed for the given release
;; ;; version. Here's an example of how to use this code:
;; ;;
;; ;;    (jira-grab "sbogdanova" "3.6.0")
;; ;;
;; ;; This function can run for a very long time. It's best to make the
;; ;; jira-buffer visible before you run this function. That way, you can
;; ;; see the progress of the function. This function relies on Jon's
;; ;; jira_grab perl program, available in the .../trunk/utilities folder
;; ;; of the Vindicia code base. Please update the location of that folder
;; ;; below before using the function. 
;; (defun jira-grab (commiter version)
;;   (let* ((home-dir (getenv "HOME"))
;;          (exec-dir (concat home-dir "/vindicia/util"))
;;          (command (format "/usr/bin/perl -I %s %s/jira_grab" exec-dir exec-dir))
;;          (target (concat home-dir "/" commiter "-" version))
;;          (params (mapconcat 'identity (list vindicia-username vindicia-password
;;                                             commiter version)
;;                             " ")))
;;     (when (file-directory-p target) (shell-command (concat "rm -rf " target)))
;;     (shell-command (format "mkdir %s" target))
;;     (async-shell-command (format "%s %s; cat %s/*" command params target))))


;; ;; This is an internal function intended to support the
;; ;; jira-diff-filediff function.  Don't call this function directly.
;; (defun dc-int-filediff (line)
;;   "Generates an Subversion command that can run on the shell."
;;   (let* ((earlier (replace-regexps-in-string 
;;                    line
;;                    "3\\.6" "3.5"
;;                    "360" "350"
;;                    "3_6" "3_5"))
;;          (repo "https://svn.vindicia.com/svn/vindicia/")
;;          (later line))
;;     (concat "svn diff " repo earlier " " repo later)))

;; ;; In the jira-diff buffer, you can call this function while on an
;; ;; 'Index:' line.  The function will try to compare the code that the
;; ;; Index line references with similar code in prior releases of the
;; ;; software.  This is useful for when there are check-in anomalies
;; ;; that prevent svn for recognizing a commit as a change and that make
;; ;; svn list all the commited code as new when it isn't.  Basically,
;; ;; when a commiter messes up a commit, you might be able to use this
;; ;; function to identify code that is truly new or different, thereby
;; ;; saving yourself having to page through thousands of lines of old
;; ;; code as if it were brand new code.
;; (defun jira-diff-filediff ()
;;   (interactive)
;;   (let ((command (dc-int-filediff
;;                   (buffer-substring (+ (point-at-bol) 7)
;;                                     (point-at-eol)))))
;;     (async-shell-command
;;      (concat "echo \"" command "\"; echo; " command "; echo; echo Done."))))

(defun replace-in-region (replacements)
  (loop for replacement in replacements
        for search = (first replacement)
        for replace = (second replacement)
        do
        (goto-char (point-min))
        (while (if (third replacement)
                   (re-search-forward search nil t)
                 (search-forward search nil t))
          (replace-match replace))))

(defun ce-to-yaml (beg end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (replace-in-region
       `(("^ \\{4\\}\\+{name => '\\([^']+\\)'," "- autobill_name: TX - \\1" t)
         (" +\\+{name => '\\([^']+\\)', start => '\\([^']+\\)', end => '\\([^']+\\)'}"
          "    \\1      \\2  \\3" t)
         (" =>" ":")
         (" +{\n" "")
         (" +[\n" "")
         ("+[qw/" "")
         ("/]," "")
         ("]," "")
         ("]}," "")
         ("\\},$" "" t)
         ("'" "")
         ("+[+{" "{ ")
         ("entitlements_valid_for" "ent_valid_for")
         (" +expected_entitlements:"
          ,(concat "  billing_results: |\n"
                  "    date         amount\n"
                  "    end\n"
                  "  entitlements_granted: |\n"
                  "    name    start       end\n") t)
         (",$" "" t)

)))))



;; (defun document-perl-program ()
;;   (interactive)
;;   (let ((start (point-max)))
;;     (goto-char start)
;;     (insert "

;; =head1 NAME

;; name - description

;; =head1 SYNOPSIS

;; name [options] [file]

;; =head1 OPTIONS

;; =over 8

;; =item B<--help>|B<-h>

;; Print this documentation.

;; =item B<--brief>|B<-b>

;; Print the most relevant text of each new line in the log
;; file. This makes the log much easier to read, but excludes a lot
;; of text from each log line.

;; =back

;; =head1 DESCRIPTION

;; This program will tail any new file that appears in the Cashbox
;; log directory, typically /var/vindicia/logs or ~/vindicia/logs.

;; =cut

;; ")
;;     (goto-char start)
;;     (next-line 4)
;;     (goto-char (point-at-bol))))

;; (defun vindicia-svn-index (url)
;;   (let* ((regex "<a href=\"\\([^\"]+\\)\">\\(.+?\\)</a>")
;;          (absolute-url (join-paths vindicia-svn-root url))
;;          (authorization `(("Authorization" (,vindicia-username
;;                                             ,vindicia-password))))
;;          (response (http-request :get absolute-url nil authorization))
;;          (result (mapcar (lambda (s)
;;                            (string-match regex s)
;;                            (concat
;;                             "[[" (join-paths absolute-url (match-string 1 s))
;;                             "][" (match-string 2 s) "]]"))
;;                          (remove-if-not
;;                           (lambda(s)
;;                             (and (string-match regex s)
;;                                  (not (member (match-string 2 s)
;;                                               '(".." "Subversion")))))
;;                           (split-string (second response) "\n"))))
;;         (content-buffer (or (get-buffer "web-service-response")
;;                             (generate-new-buffer "web-service-response")))
;;         (headers-buffer (or (get-buffer "web-service-headers")
;;                             (generate-new-buffer "web-service-headers"))))
;;     (with-current-buffer headers-buffer
;;       (conf-mode)
;;       (erase-buffer)
;;       (goto-char (point-min))
;;       (insert (first response))
;;       (goto-char (point-min)))
;;     (with-current-buffer content-buffer
;;       (erase-buffer)
;;       (goto-char (point-min))
;;       (insert (concat absolute-url "\n\n"))
;;       (loop for line in result do (insert (concat "  " line "\n")))
;;       (goto-char (point-min))
;;       (org-mode))))

;; (defun vindicia-svn-file (url)
;;   (let* ((content-buffer (or (get-buffer "web-service-response")
;;                              (generate-new-buffer "web-service-response")))
;;          (headers-buffer (or (get-buffer "web-service-headers")
;;                              (generate-new-buffer "web-service-headers")))
;;          (absolute-url (join-paths vindicia-svn-root url))
;;          (authorization `(("Authorization" (,vindicia-username
;;                                             ,vindicia-password))))
;;          (response (http-request :get absolute-url nil authorization))
;;          (result (second response)))
;;     (with-current-buffer headers-buffer
;;       (conf-mode)
;;       (erase-buffer)
;;       (goto-char (point-min))
;;       (insert (first response))
;;       (goto-char (point-min)))
;;     (with-current-buffer content-buffer
;;       (erase-buffer)
;;       (goto-char (point-min))
;;       (fundamental-mode)
;;       (insert (concat absolute-url "\n\n"))
;;       (insert result)
;;       (goto-char (point-min))
;;       (cond ((or (string-match "\\.\\(pl\\|pm\\)$" url) 
;;                   (string-match "^.+perl" result))
;;               (cperl-mode))
;;             ((or (string-match "\\.xml$" url)
;;                  (string-match "^<\\?xml" result))
;;              (nxml-mode))
;;             (t nil)))))

;; (defun open-vindicia-svn-file ()
;;   (interactive)
;;   (goto-char (+ (point-at-bol) 5))
;;   (let ((url (second (split-string (thing-at-point-url-at-point)
;;                                    vindicia-svn-root))))
;;     (if (string-match "/$" url)
;;         (vindicia-svn-index url)
;;       (vindicia-svn-file url))))
   
;; (defun open-files (dry-run)
;;   (let* ((files '(

;;                   "//vindicia/db/vindicia_classes.xml"
;;                   "//Obj/Entity/MerchantCustomer.pm"
;;                   "//Obj/Entity/AutoBill.pm"
;;                   "//Obj/EntitlementLedger.pm"
;;                   "//QA/ObjTest.pm"
;;                   "//unit_tests/Obj/Entity/ab_seasonal.t"
;;                   "//unit_tests/Obj/Entity/ent.yaml"
;;                   "//t/ce.pl"
;;                   "//t/ce.dat"

;;                   ))
;;          (shortcuts '(("//vindicia" "/base-path")
;;                       ("//Obj" "/base-path/site_perl/Vindicia/Obj")
;;                       ("//QA" "/base-path/site_perl/Vindicia/QA")
;;                       ("//unit_tests"
;;                        "/base-path/qa/server/unit_tests/site_perl/Vindicia/")
;;                       ("//t" "/ssh:dcameron@alejandra#4007:/home/dcameron/t/")))
;;          (paths (loop for file in files
;;                       for name = (loop for shortcut in shortcuts
;;                                        when (string-match
;;                                              (concat "^" (car shortcut)) file)
;;                                        do (return (replace-match
;;                                                    (second shortcut) t t file)))
;;                       collect (if (string-match "^/base-path" name)
;;                                   (replace-match vindicia-base-path t t name)
;;                                 name))))
;;     (loop for path in paths
;;           when (not dry-run)
;;           do (find-file-other-window path)
;;           collect path)))
