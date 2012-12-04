
;; Atlassian Confluence Wiki Utilities
;; begin
(defun replace-heading-markers (old new)
  (let ((old-point (point)))
    (goto-char (point-min))
    (while (re-search-forward (concat "^[" old "]+") nil t)
      (replace-match
       (apply 'concat (loop for a from 1 to (length (match-string 0))
                            collect new))))
    (goto-char old-point)))

(defun org-to-confluence ()
  (interactive)
  (replace-heading-markers "*" "#"))

(defun confluence-to-org ()
  (interactive)
  (replace-heading-markers "#" "*"))
;; end


;; Misc
;; begin
(defun replace-regexps-in-string (string &rest replacements)
  (loop for i from 0 below (length replacements) by 2
        for regexp = (elt replacements i)
        for replace-with = (elt replacements (1+ i))
        for result = (replace-regexp-in-string regexp replace-with string)
                     then (replace-regexp-in-string regexp replace-with result)
        finally return result))

(defun unfill-paragraph () ;; bound to C-x M-q
  "Do the opposite of fill-paragraph; stuff all lines in the current
paragraph into a single long line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region (beg end) ;; bound to C-x s-q
  "Do the opposite of fill-region; stuff all paragraphs in the current
region into long lines."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region (beg) (end))))

(defun format-twitter (beg end)
  "Format twitter-list text copied from the wiki."
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (reverse-region beg end)
    (goto-char (point-min))
    (while (re-search-forward "\\( about\\)? [0-9]+ \\(hour\\|day\\)s? ago" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "^# " nil t)
      (replace-match "   * "))
    (goto-char (point-max))))

;; Here's a nice macro for timing functions (works like the common lisp one)
(defmacro time (&rest body)
  (let ((start-time (make-symbol "start-time"))
        (elapsed-time (make-symbol "elapsed-time"))
        (result (make-symbol "result")))
    `(let* ((,start-time (float-time))
            (,result ,@body)
            (,elapsed-time (- (float-time) ,start-time)))
       (list ,elapsed-time ,result))))
;; end


;; Help with lists
;; begin
(defun string-join (separator list)
  "Takes a list of strings and joins them using delimiter. Think Perl join."
  (mapconcat (lambda (x) x) list separator))

(defun shuffle-lines (beg end)
  "Scramble all the lines in region defined by BEG END
   If region contains less than 2 lines, lines are left untouched."
  (interactive "*r")
  (catch 'cancel
    (save-restriction
      (narrow-to-region beg end)
      ;;   Exit when there is not enough lines in region
      (if (< (- (point-max) (point-min)) 3)
          (throw 'cancel t))

      ;;    Prefix lines with a random number and a space
      (goto-char (point-min))
      (while (not (eobp))
        (insert (int-to-string (random (- (point-max) (point-min)))) " ")
        (forward-line 1))

      ;;  Sort lines according to first field (random number)
      (sort-numeric-fields 1 (point-min) (point-max))

      (goto-char (point-min))  ;Remove the prefix fields
      (while (not (eobp))
        (delete-region (point) (progn (forward-word 1) (+ (point) 1)))
        (forward-line 1)))))

(defun distinct-lines (beg end)
  "Remove duplicates from the list in the region defined by BEG END"
  (interactive "*r")
  (catch 'cancel
    (save-restriction
      (narrow-to-region beg end)
      ;; Exit if there are less than 2 lines in region
      (if (< (- (point-max) (point-min)) 2)
          (throw 'cancel t))
      (let ((h (make-hash-table :test 'equal)))
        (goto-char (point-min))
        (while (not (eobp))
          (puthash (buffer-substring (point-at-bol) (point-at-eol)) 1 h)
          (forward-line 1))
        (delete-region (point-min) (point-max))
        (maphash (lambda (k v) (insert k "\n")) h)))))

(defun count-distinct-lines (beg end)
  "Remove duplicates from the list in the region defined by BEG END"
  (interactive "*r")
  (catch 'cancel
    (save-restriction
      (narrow-to-region beg end)
      (let ((h (make-hash-table :test 'equal)))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((k (buffer-substring (point-at-bol) (point-at-eol)))
                 (v (gethash k h 0)))
            (puthash k (1+ v) h))
          (forward-line 1))
        (delete-region (point-min) (point-max))
        (maphash
         (lambda (k v) (insert (concat k " " (int-to-string v) "\n")))
         h)))))

(defun subtract-lists (beg end)
  "This function subtracts one list from another.
   The function accepts a region with two lists separated by one
   blank line, removes from the first list all the elements that
   exist in the second list, then returns a buffer with the
   elements that remain from the first list."
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (let ((list2 nil) (h (make-hash-table :test 'equal))
          (result nil))
      (goto-char (point-min))
      (while
          (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
            (forward-line 1)
            (if (equal line "") nil
              (puthash line (1+ (gethash line h 0)) h))))
      (while (not (eobp))
        (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
          (unless (equal line "")
            (push line list2)))
        (forward-line 1))
      (setf list2 (nreverse list2))
      (dolist (e list2 result)
        (when (not (zerop (gethash e h 0)))
          (puthash e (1- (gethash e h 0)) h)))
      (with-current-buffer (generate-new-buffer "list1-remains")
        (maphash (lambda (k v) (dotimes (a v) (insert (format "%s\n" k)))) h)
        (goto-char (point-min))
        (switch-to-buffer (current-buffer))))))
;; end


;; HTTP utilities
;; begin
(setq http-post-timeout 120)

(defun url-request (key-value-pairs)
  (if (string= (first (first key-value-pairs))
               "POSTDATA")
      (second (first key-value-pairs))
    (mapconcat (lambda (a) (concat (url-hexify-string (first a)) "="
                                   (url-hexify-string (second a))))
               key-value-pairs "&")))


;; Post to URL. Returns a string with the raw response from the HTTP
;; server.
(defun http-request (type url &optional args headers)
  "Sends GET or POST HTTP request to given URL with optional
   arguments and header."
  (when (not (member type '(:get :post)))
    (error "Type must be :get or :post"))
  (let ((url-request-method (substring (upcase (symbol-name type)) 1))
        (url-request-extra-headers (when (and (eql type :post) (not headers))
                                     '(("Content-type" .
                                        "application/x-www-form-urlencoded"))))
        (url-request-data (when (and (eql type :post) args)
                            (url-request args)))
        (encoded-url (if (eql type :get)
                         (concat url (when args
                                       (concat "?" (url-request args))))
                       url)))
    (when headers
      (loop for (key value) in headers
            for xvalue = (cond
                          ((and (equal key "Authorization") (listp value))
                           (concat "Basic "
                                   (base64-encode-string
                                    (concat (first value) ":"
                                            (second value)))))
                          (t value))
            do (push (cons key xvalue) url-request-extra-headers)))
    (let* ((response
            (time
             (with-timeout
                 (http-post-timeout "Gave up. Server was taking too long.")
               (with-current-buffer
                   (url-retrieve-synchronously encoded-url)
                 (buffer-string)))))
           (full-response
            (concat
             (format "[%d milliseconds]\n" (* 1000 (first response)))
             (second response)))
           (header (substring full-response 0 (search "\n\n" full-response)))
           (body (substring full-response (+ (search "\n\n" full-response) 2))))
      (list header body))))


(defun scrape-string (regexp string)
  (let ((start 0) (matches nil))
    (while (string-match regexp string start)
      (push
       (substring string (match-beginning 1) (match-end 1))
       matches)
      (setq start (1+ (match-end 1))))
    matches))

(defun join-paths (&rest path-parts)
  (string-join
   "/"
   (loop for part in path-parts
         for a from 1 to (length path-parts)
         collect
         (cond
          ((= a 1) (replace-regexp-in-string "/$" "" part))
          ((= a (length path-parts))
           (replace-regexp-in-string "^/" "" part))
          (t (replace-regexp-in-string "^/\\|/$" "" part))))))

;; Macro utilities
;; begin
(defun save-macro (name)
  "Save a macro. Take a name as argument and save the last
   defined macro as a function with this name at the end of your
   dc-macros.el file."
  (interactive "SName of the macro: ")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file "~/elisp/dc-macros.el")    ; open the .emacs file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer
;; end

(defun guess-number (max)
  (interactive "nRange of 1 to what number? ")
  (let ((y (1+ (random (if (null max) 10 max))))
        (x 0)
        (tries 0))
    (while (not (= x y))
      (setf x (string-to-number
               (read-from-minibuffer
                (format "Guess a number between 1 and %s: " max))))
      (incf tries)
      (message
       (cond
        ((< x y) "Too small")
        ((> x y) "Too big")
        (t (format "==> %s <==, Yes! You got it in %s tries!" x tries))))
      (when (not (= x y)) (sit-for 2)))))

(defun string-trim(s)
  (replace-regexp-in-string "^\\( \\|\n\\)+\\|\\( \\|\n\\)+$" "" s))

(defun hostname ()
  (string-trim (shell-command-to-string "hostname")))

(defun replace-region-with-hello (beg end)
  (interactive "*r")
  (cl-set-buffer-substring beg end "Hello"))

(defun xmlse-from-url (beg end)
  "Convert the highlighted region from URL-encoded text to plain text"
  (interactive "*r")
  (cl-set-buffer-substring
   beg end (second (car (url-parse-query-string (buffer-substring beg end))))))

(defun flatten (x)
  (cond
   ((null x) nil)
   ((atom x) (list x))
   (t (loop for a in x append (flatten a)))))

(defun reverse-buffer-region (beg end)
  "Reverse the highlighted string"
  (interactive "*r")
  (cl-set-buffer-substring beg end (reverse-string (buffer-substring beg end))))   

(defun reverse-string (string)
  (apply 'concat (reverse (split-string string ""))))

(defun filter-region (beg end filter)
  "Remove lines for which filter, the given Lisp expression,
   returns false. The filter function is passed the 1-based index
   of the line and the text of the line."
  (interactive "*r\nxExp: ")
  (let ((lines nil)
        (index 1))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
          (when (funcall filter index line) (push line lines))
          (forward-line 1)
          (incf index)))
      (delete-region (point-min) (point-max))
      (loop for line in (reverse lines) do (insert (concat line "\n"))))))

(defun nagios-services (beg end host-name)
  "Format the highlighted area as nagios services"
  (interactive "*r\nxHost name: ")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((services nil))
      (while (not (eobp))
        (let ((service (buffer-substring (point-at-bol) (point-at-eol))))
          (when service
            (push (format "define service {\n    %s\n    %s\n    %s\n}\n"
                          "use passive-service"
                          (format "host_name %s" host-name)
                          (format "service_description %s" (string-trim service)))
                  services)))
        (forward-line 1))
      (when services
        (cl-set-buffer-substring
         (point-min) (point-max) (string-join "\n" services))))))

(defun fib (x)
  (cond
   ((zerop x) 0)
   ((= x 1) 1)
   (t (+ (fib (1- x)) (fib (- x 2))))))

(defun memoize (f)
  (lexical-let ((g (symbol-function f))
                (cache (make-hash-table :test 'equal)))
    (setf (symbol-function f)
          (lambda (&rest p)
            (let ((v (gethash p cache)))
              (if v v (puthash p (apply g p) cache)))))))


;; Make a list of the functions in some perl code
(defun list-functions ()
  (interactive)
  (let ((functions nil)
        (source-buffer (buffer-name)))
    (goto-char (point-min))
    (while (re-search-forward
            "^\\(\\t\\| \\)*\\(sub\\|method\\)\\(\\t\\| \\)\\([_a-zA-Z0-9]+\\)" nil t)
      (push (match-string 4) functions))
    (let ((buffer (or (get-buffer "functions")
                      (generate-new-buffer "functions"))))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (concat source-buffer "\n"))
        (insert (string-join "\n"
                             (mapcar (lambda (x) (concat "    " x))
                                     (sort functions 'string<))))
        (goto-char (point-min))
        (switch-to-buffer (current-buffer))))))

(defun goto-function ()
  (interactive)
  (let ((f (string-trim (buffer-substring (point-at-bol) (point-at-eol))))
        (buffer (progn
                  (goto-char (point-min))
                  (string-trim
                   (buffer-substring (point-at-bol) (point-at-eol))))))
    (search-forward (concat "    " f) nil t)
    (goto-char (- (point) (length f)))
    (with-current-buffer (get-buffer buffer)
      (let ((regex (concat "^\\(\\t\\| \\)*sub\\(\\t\\| \\)+" f)))
        (goto-char (point-min))
        (switch-to-buffer (current-buffer))
        (message (concat "Go to " f))
        (goto-char (point-at-bol))
        (re-search-forward regex nil t)
        (goto-char (point-at-bol))))))

(defun query-web-service (type url &optional args headers)
  (let ((result (http-request type url args headers))
        (content-buffer (or (get-buffer "web-service-response")
                            (generate-new-buffer "web-service-response")))
        (headers-buffer (or (get-buffer "web-service-headers")
                            (generate-new-buffer "web-service-headers"))))
    (with-current-buffer headers-buffer
      (conf-mode)
      (erase-buffer)
      (goto-char (point-min))
      (insert (first result))
      (goto-char (point-min)))
    (with-current-buffer content-buffer
      (nxml-mode)
      (erase-buffer)
      (goto-char (point-min))
      (insert (second result))
      (goto-char (point-min))
      (format-xml))
    'OK))

(defun query-chattermancy (type path &optional arguments creds)
  (query-web-service
   type
   (concat chattermancy-protocol "://"
           chattermancy-host ":"
           chattermancy-port
           path)
   arguments
   (if creds
       `(("Authorization" (,(first creds) ,(second creds))))
     `(("Authorization" (,chattermancy-username  ,chattermancy-password))))))

(defun send-message-to-printer (printer message)
  (let ((target (open-network-stream "printer-client" nil printer 9100)))
    (process-send-string
     target
     (string-to-unibyte
      (concat "%-12345X@PJL RDYMSG DISPLAY = \""
              message
              "\"\r\n%-12345X\r\n")))
    (sit-for 3)
    (delete-process "printer-client")
    'OK))

(defun replace-incrementing (beg end)
  "Replace the regex with an incrementing integer that starts at 1."
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((i 0))
      (while (re-search-forward "debug> [0-9]+" nil t)
        (replace-match (format "debug> %s" (incf i)))))
    (goto-char (point-max))))

(defun define-term (beg end)
  "Use Google to define the highlighted term."
  (interactive "*r")
  (browse-url (concat "http://google.com/search?tbs=dfn:1&q="
                      (buffer-substring beg end))))

(defun sum-hours-in-region (beg end)
  "Calculate the total sum of hours in the region. Hours look
like '4h' and are always at the end of a line."
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((hours-n nil)
          (hours-s nil))
      (while (re-search-forward "\\([0-9]+\\(\\.[0-9]\\{1,2\\}\\)?\\)+h *$" nil t)
        (push (string-to-number (match-string-no-properties 1)) hours-n)
        (push (match-string-no-properties 1) hours-s))
      (print (concat (string-join " + " (reverse hours-s)) " = "
                     (number-to-string (reduce '+ hours-n)))))
    (goto-char (point-max))))

(defun sum-hours-in-buffer (buffer-name &optional explain)
  (with-current-buffer buffer-name
    (goto-char (point-min))
    (let ((hours-n nil)
          (hours-s nil))
      (while (re-search-forward "\\([0-9]+\\(\\.[0-9]\\{1,2\\}\\)?\\)+h *$" nil t)
        (push (string-to-number (match-string-no-properties 1)) hours-n)
        (push (match-string-no-properties 1) hours-s))
      (if explain
          (list (reduce '+ hours-n)
                (concat (string-join " + " (reverse hours-s)) " = "
                        (number-to-string (reduce '+ hours-n))))
        (reduce '+ hours-n)))))

(defun title-from-html (html)
  "Get the title of an HTML document"
  (replace-regexps-in-string
   (car (scrape-string "<title>\\(.*?\\)</title>" html))
   " - " ""
   " &mdash; " ""
   " &ndash; " ""
   " &x2013; " ""
   " &8211; " ""
   " &x2014; " ""
   " &8212; " ""
   " &x2012; " ""
   " &8210; " ""
   " &x2015; " ""
   " &8213; " ""
   "^ +\\| +$" ""))

(defun html-to-content (html)
  "Strips HTML, JavaScript, Menus, and other stuff from a Web
   page, attempting to return a plain-text version of the
   content."
  (concat (title-from-html html) "    "
          (string-join
           " "
           (remove-if
            (lambda (s) (< (length s) 140))
            (split-string
             (replace-regexps-in-string
              html
              "\n" " "
              "\r" " "
              "<head.*?</head>" "|"
              "<script.*?</script>" "|"
              "<style.*?</style>" "|"
              "</?\\(p\\|b\\|br\\|hr\\|code\\|i\\|img\\|u\\|del\\|em\\)/?>" " "
              "</?\\(\\|strong\\|a\\)/?>" " "
              "<[^>]+?>" "|"
              "&[^ ]+?;" " "
              " +|\\|| +" "|"
              "||+" "|"
              "  +" " ")
             "|")))))

(defun get-web-page-text (beg end)
  (interactive "*r")
  (let ((result (http-request :get (buffer-substring beg end)))
        (text-buffer (or (get-buffer "web-page-text")
                         (generate-new-buffer "web-page-text"))))
    (with-current-buffer text-buffer
      (erase-buffer)
      (goto-char (point-min))
      (insert (html-to-content (second result)))
      (goto-char (point-min)))
    'OK))

(defun random-hex-number (digits)
  (interactive "nHow many digits? ")
  (let* ((a "0123456789abcdef")
         (b (substring a 1)))
    (apply
     'concat
     (loop for c from 0 below digits collect
           (char-to-string
            (elt (if (zerop c) b a) (random (+ 15 (signum c)))))))))

(defun four-digit-hex-number ()
  (interactive)
  (insert (random-hex-number 4)))

(defun obj-note ()
  (interactive)
  (insert "$self->Note(\"")
  (four-digit-hex-number)
  (insert " debug> \");")
  (goto-char (- (point-at-eol) 3)))

(defun vin-note ()
  (interactive)
  (insert "Note(\"")
  (four-digit-hex-number)
  (insert " debug> \");")
  (goto-char (- (point-at-eol) 3)))

(defun is-prime (n)
  (cond ((< n 2) nil)
        ((= n 2) t)
        ((zerop (mod n 2)) nil)
        (t (loop for x from 3 to (sqrt n) by 2 never (zerop (mod n x))))))

(defun next-prime (n)
  (loop for a = (if (evenp n) (1+ n) n) then (1+ a) when (is-prime a) return a))

(defun find-buffer-matches (buffer-name regex)
  (with-current-buffer buffer-name
    (goto-char (point-min))
    (let ((lines nil))
      (while (re-search-forward regex nil t)
        (push (match-string-no-properties 1) lines))
      lines)))

(defun swap (seq a b)
  (let ((temp (elt seq a)))
    (setf (elt seq a) (elt seq b))
    (setf (elt seq b) temp))
  seq)

(defun k-index (seq)
  (loop for k from (- (length seq) 2) downto 0
        when (< (elt seq k) (elt seq (1+ k))) return k
        finally (return nil)))

(defun l-index (seq k)
  (when k
    (loop for l from (1- (length seq)) downto k
          when (> (elt seq l) (elt seq k)) return l
          finally (return nil))))

(defun permute (seq)
  (let* ((o-seq (stable-sort seq '<))
         (c-seq (copy-seq o-seq)))
    (loop for k = (k-index c-seq)
          for l = (l-index c-seq k)
          while k do
          (setf c-seq (swap c-seq k l))
          (setf c-seq (concat (subseq c-seq 0 k)
                              (subseq (reverse-string c-seq) k)))
          collect (copy-seq c-seq))))

(defun file-extension (filename)
  (string-match "\\.\\([-a-zA-Z0-9]+\\)$" filename)
  (match-string 1 filename))

(defun set-mode (filename)
  (let ((fe (file-extension filename)))
    (cond ((member fe '("pl" "pm" ".t")) (cperl-mode))
          ((member fe '("xml" "inc-xml")) (nxml-mode))
          ((eq fe "java") (java-mode))
          (t (fundamental-mode)))))

(defun open-diff-index (cp)
  (interactive "d")
  (let* ((beg (+ (re-search-backward "^Index: ") 7))
         (end (point-at-eol))
         (buffer (get-buffer "diff-index"))
         (filename (concat diff-index-root (buffer-substring beg end))))
    (goto-char cp)
    (with-current-buffer buffer
      (insert-file-contents filename)
      (set-mode filename))))
    
(defun dc-time-span (t1 t2)
  (let* ((span (- (float-time (date-to-time t2))
                  (float-time (date-to-time t1))))
         (hours (truncate span 3600))
         (minutes (truncate (- span (* hours 3600)) 60)))
    (format "%d:%02d" hours minutes)))

(defun check-clock-buffer (buffer)
  "This function checks my work.org buffer to make sure that work
   times don't overlap and that the times are calculated
   correctly everywhere."
  (let ((l nil)
        (rets "\\[\\([0-9][-0-9]+\\)[^0-9]+\\([:0-9]+\\)\\]")
        (resp ".*=> *\\([:0-9]+\\)")
        (errors nil))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward (concat "^ +CLOCK: " rets "--" rets resp) nil t)
        (push
         (list
          (list (match-string-no-properties 1) (match-string-no-properties 2))
          (list (match-string-no-properties 3) (match-string-no-properties 4))
          (match-string-no-properties 5))
         l))
      (setf l (sort (reverse l)
                    (lambda (r1 r2)
                      (string< (concat (string-join " " (first r1))
                                       (string-join " " (second r1)))
                               (concat (string-join " " (first r2))
                                       (string-join " " (second r2)))))))
      (loop for (start stop span) in l
            for bad-span = (not (string= span (dc-time-span
                                               (string-join " " start)
                                               (string-join " " stop))))
            when (or (string< (string-join " " stop) (string-join " " start))
                     bad-span)
            do (push (concat "[" (string-join " " start)
                             "]--[" (string-join " " stop) "]"
                             (when bad-span " (span)"))
                     errors))
      (let ((k (flatten (loop for (start stop span) in l
                              collect (list (string-join " " start)
                                            (string-join " " stop))))))
        (loop for i from 1 to (1- (length k))
              when (string< (elt k i) (elt k (1- i)))
              do (push (concat (elt k (1- i)) "->"
                               (elt k i) " (overlap)") errors)))
      (if errors
          (cons "Errors" (reverse errors))
        "No errors."))))

(defun eshell/5cmd (&rest args)
  (let ((olddir (eshell/pwd)))
    (eshell/cd "/ssh:vincos-5:/usr/bin")
    (shell-command (eshell-flatten-and-stringify args) "*Shell Command Output*")
    (eshell/cd olddir)))

(defun eshell/eck (&rest args)
  (let* ((olddir (eshell/pwd))
         (path (split-string olddir ":"))
         (dir (if (= (length path) 3)
                  (if (> (length args) 1) (second args) (third path))
                olddir))
         (rdir (if (string-match "^/Users/dcameron" dir)
                   (concat "/home/dcameron" (substring dir 15))))
         (cmd (concat "ack --nocolor " (car args) " " rdir))
         (buffer (or (get-buffer "*eshell/ack output*")
                     (generate-new-buffer "*eshell/ack output*"))))
    (eshell/cd "/ssh:vincos-5:/usr/bin")
    (shell-command cmd buffer)
    ;; (with-current-buffer buffer
    ;;   (erase-buffer)
    ;;   (insert (format "'%s' args='%d'" cmd (length args))))
    (eshell/cd olddir)
    (switch-to-buffer-other-window buffer)
    ""))

(defun clear-buffer (buffer)
  (interactive "b")
  (with-current-buffer buffer
    (let ((eshell-buffer-maximum-lines 0))
      (eshell-truncate-buffer))))

(defun comment-lines (buffer-name regex &rest params)
  "The params list can contain the named parameters :comment
   and :line.  Locates regex in the buffer, then comments the
   line that follows regex.  If the boolean parameter comment is
   true, the line is commented.  Otherwise, the line is
   uncommented.  If the line is already in the target state, this
   function does nothing to the line.  The function continues
   until it has found all such lines."
  (with-current-buffer buffer-name
    (let ((starting-point (point))
          (changed-lines)
          (comment (getf params :comment))
          (line (getf params :line 0)))
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (forward-line line)
        (back-to-indentation)
        (if comment
            (unless (string= (buffer-substring-no-properties (point) (+ (point) 2)) "# ")
              (insert "# ")
              (push (line-number-at-pos) changed-lines))
          (when (string= (buffer-substring-no-properties (point) (+ (point) 2)) "# ")
            (delete-char 2)
            (push (line-number-at-pos) changed-lines)))
        (goto-char (point-at-eol)))
      (goto-char starting-point)
      (reverse changed-lines))))

(defun revert-regular-files (&optional noconfirm regex)
  (loop for buffer in 
        (mapcar 'buffer-name
                (remove-if
                 (lambda (b)
                   (or (string-match "^ *\\*" (buffer-name b))
                       (not (buffer-file-name b))
                       (when regex (not (string-match regex (buffer-name b))))))
                 (buffer-list)))
        do (with-current-buffer buffer
             (revert-buffer t noconfirm))
        collect buffer))

(defun dc-play-sound (filename)
  (shell-command (concat "afplay '" filename "'")))
