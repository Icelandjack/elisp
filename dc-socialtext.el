(setf dc-st-dev-credentials 
      '(("Authorization" ("devnull1@socialtext.com" "d3vnu11l"))))

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

(defun query-st-dev (type path &optional args headers)
  (let* ((host "borg.sinistercode.com")
         (port 21001)
         (user "devnull1@socialtext.com")
         (pass "d3vnu11l")
         (uri (join-paths (format "http://%s:%d" host port) path))
         (headers (append headers `(("Authorization" (,user ,pass))))))
    (query-web-service type uri args headers)))

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


(defun query-dashboard (host)
  (query-web-service :get (format "http://%s:21000/st/dashboard" host)
                     nil dc-st-dev-credentials))

(defun query-captcha (host)
  (query-web-service :get (format "http://%s:21000/data/config/captcha" host)
                     nil dc-st-dev-credentials))

(defun params-to-hash (lambda-list params)
  (when (not (zerop (mod (length params) 2)))
    (error "params-to-hash parameter list is not of even length"))
  (let ((hash (make-hash-table :test 'eql))
        keys)
        
    (loop for a from 0 below (length lambda-list)
          for key-spec = (elt lambda-list a)
          for key = (if (listp key-spec) (car key-spec) key-spec)
          for value = (when (listp key-spec) (second key-spec))
          do
          (puthash key value hash)
          (push key keys))

    (loop for a from 0 below (length params) by 2
          for key = (elt params a)
          for value = (elt params (1+ a))
          when (not (member key keys))
          do (error (format "Unkown parameter key %s" key))
          do (puthash key value hash))

    hash))
    

;; (defun dc-http-get (url &rest params)
;;   "Makes an HTTP GET call to the give URL"
;;   (let ((param (params-to-hash '(:
;;   (let ((url-request-method "GET")
;;         (url-request-extra-headers headers)

  
(cl-defun page-views (&key params)
  (request
   "http://borg.sinistercode.com:21001/data/events"
   :type "GET"
   :headers '(("Authorization" .
               "Basic ZGV2bnVsbDFAc29jaWFsdGV4dC5jb206ZDN2bnUxMWw=")
              ("Accept" . "application/json"))
   :params params
   :parser 'buffer-string
   :success (function* (lambda (&key data &allow-other-keys)
                         (with-current-buffer
                             (get-buffer "web-service-response")
                           (erase-buffer)
                           (insert data)
                           (goto-char (point-min))
                           (while (re-search-forward "," nil t)
                             (replace-match ",\n"))
                           (indent-region (point-min) (point-max)))))
   :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                       (with-current-buffer 
                           (get-buffer "web-service-response")
                         (erase-buffer)
                         (insert (format "Error: %S" error-thrown))))))
  "Done.")

(setf st-get-output-buffer "web-service-response"
      st-get-accept :json
      st-get-username "username"
      st-get-password "password"
      st-get-path "http://borg.sinistercode.com/data/users/3"
      st-get-params nil)

(cl-defun st-get (&key username password accept path params output-buffer)

   "This function calls the given server with an HTTP GET and
   writes the reponse from the server to the specified buffer.
   If you don't speficy a parameter it defaults to something.
   If you provide a value for a parameter, the value is remembered
   until the next time emacs is started. The value for the path
   parameter should look like this: http://abc.com/a/b
   The value for the accept parameter can be one of :json, :xml,
   :html, and :text.  It can also be any string, in which
   case the Accept header will be set to that string.  An
   Authorization header is always sent and it is always a
   Basic Authentication authorization string.  So, make sure
   you provide valid credentials.  The output-buffer value can
   be a buffer or a string representing the name of the buffer. 
   If a buffer with that name doesn't exist, this function
   creates it."

  (setf st-get-output-buffer-name
        (if output-buffer
            (setf st-get-output-buffer-name output-buffer)
          st-get-output-buffer-name))
  (setf st-get-accept 
        (if accept
            (setf st-get-accept
                  (getf (list :json "application/json"
                              :xml "text/xml"
                              :html "text/html"
                              :text "text/plain")
                        accept))
          st-get-accept))
  (let* ((username (if username (setf st-get-username username)
                     st-get-username))
         (password (if password (setf st-get-password password)
                     st-get-password))
         (authorization (concat "Basic "
                                (base64-encode-string 
                                 (concat username ":" password))))
         (headers `(("Authorization" . ,authorization)
                    ("Accept" . ,st-get-accept)))
         (path (if path (setf st-get-path path) st-get-path))
         (params (if params (setf st-get-params params) params)))
    (request
     path
     :type "GET"
     :headers headers
     :params params
     :parser 'buffer-string
     :success (function* 
               (lambda (&key data &allow-other-keys)
                 (with-current-buffer
                     (or (get-buffer st-get-output-buffer-name)
                         (generate-new-buffer st-get-output-buffer-name))
                   (erase-buffer)
                   (insert data)
                   (cond ((string= st-get-accept "application/json")
                          (js-mode)
                          (goto-char (point-min))
                          (while (re-search-forward "," nil t)
                            (replace-match ",\n"))
                          (indent-region (point-min) (point-max)))
                         ((string= st-get-accept "text/xml")
                          (nxml-mode)
                          (indent-region (point-min) (point-max)))
                         ((string= st-get-accept "text/html")
                          (html-mode)
                          (indent-region (point-min) (point-max)))
                         (t nil)))))
     :error (function*
             (lambda (&key error-thrown &allow-other-keys&rest _)
               (with-current-buffer 
                   (or (get-buffer st-get-output-buffer-name)
                       (generate-new-buffer st-get-output-buffer-name))
                 (erase-buffer)
                 (insert (format "Error: %S" error-thrown)))))))
  "Done")
