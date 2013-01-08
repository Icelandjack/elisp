;; (setf socialtext-settings
;;       (list :host "stevim"
;;             :port 22000
;;             :user "devnull1@socialtext.com"
;;             :pass "d3vnu11l"))

;; (defun initialize-socialtext (list)
;;   (let ((settings (copy-list socialtext-settings)))
;;     (loop for key in '(:host :port :user :pass)
;;           when (member key list)
;;           do (setf (getf

;;             :uri (format "https://%s:%d%s" host port path)
;;             :headers (append headers `(("Authorization" (,user ,pass))))

(defun query-st-dev (type path args headers)
  (let* ((host "stevim")
         (port 22000)
         (user "devnull1@socialtext.com")
         (pass "d3vnu11l")
         (uri (format "https://%s:%d%s" host port path))
         (headers (append headers `(("Authorization" (,user ,pass))))))
    (query-web-service type url args headers)))

(defun query-customjs (type &optional args headers)
  (let* ((host "erin.hsd1.ca.comcast.net.")
         (port 22000)
         (url (format "https://%s:%d/data/workspaces/foobar/customjs"
                      host port)))
    (query-web-service type url args headers)))

;; (defun query-widget (type &optional args headers)
;;   (let* ((host "erin.hsd1.ca.comcast.net.")
;;          (port 22000)
;;          (url (format "https://

(defun stevim-path ()
  (insert "/ssh:darkstar.sinistercode.com#22002:/home/dcameron/src/st/socialtext"))

(global-set-key (kbd "C-c >") 'stevim-path)

(defun perl-c ()
  (interactive)
  (let* ((postfix (concat " && bounce"
                          " && tail -f"
                          " ~/.nlw/log/nlw.log"
                          " | egrep 'debug>|Ceqlotron master: waiting, conc'"))
         (regex "/home/\\([^/]+\\)/\\(src/st/socialtext.+\\(\\.pm\\|\\.pl\\)\\)$")
         (host "stevim\\|\\darkstar\\|darkstar\\.sinistercode\\.com")
         (lib "src/st/socialtext/nlw/lib")
         (perl-check "perl -I /home/%s/%s -c /home/%s/%s")
         (paths (loop for buffer in (buffer-list)
                      for path = (buffer-file-name buffer)
                      for valid-path = (when path (string-match regex path))
                      for user = (when valid-path (match-string 1 path))
                      for path-rest = (when valid-path (match-string 2 path))
                      when (and valid-path (string-match host path))
                      collect (format perl-check user lib user path-rest))))
    (insert (concat (mapconcat 'identity paths " && ") postfix))))

(defun insert-buffer-file-name ()
  (interactive)
  (let* ((buffer (read-string "Buffer? "))
         (path (with-current-buffer buffer (buffer-file-name)))
         (short (if (string-match "src/st/socialtext/\\(.+\\)$" path)
                    (concat "../" (match-string 1 path))
                  path)))
    (insert short)))


(global-set-key (kbd "s-f") 'insert-buffer-file-name)

