;; Misc
;; begin
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

;; Post to URL. Returns a string with the raw response from the HTTP
;; server.
(defun http-post (url args &optional username password)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (concat
          (mapconcat 
           (lambda (arg)
             (concat (url-hexify-string (car arg))
                     "="
                     (url-hexify-string (cadr arg))))
           args
           "&")))
        (error nil))
    (when username
      (push `("Authorization" . 
              ,(concat "Basic " (base64-encode-string 
                                 (concat username ":" password))))
            url-request-extra-headers))
    (let ((response
           (time
            (with-timeout 
                (http-post-timeout "Gave up because server was taking too long.")
              (with-current-buffer 
                  (url-retrieve-synchronously url)
                (buffer-string))))))
      (concat (format "Response Time: %.3f\n\n" (car response)) (cadr response)))))
      
;; Issue a GET request to URL. Returns a string with the raw response from
;; the HTTP server.
(defun http-get (url-path &optional args username password)
  "Send ARGS to URL as a GET request."
  (let ((url-request-extra-headers nil)
        (url-request-method "GET")
        (encoded-url (concat
                      url-path
                      (when args
                        (concat "?" (mapconcat
                                     (lambda (arg)
                                       (concat
                                        (url-hexify-string (car arg))
                                        "="
                                        (url-hexify-string (cadr arg))))
                                     args
                                     "&"))))))
    (when username
      (push `("Authorization" . 
              ,(concat "Basic " (base64-encode-string 
                                 (concat username ":" password))))
            url-request-extra-headers))
    (let ((response
           (time
            (with-timeout 
                (http-post-timeout
                 "Gave up because server was taking too long.")
              (with-current-buffer 
                  (url-retrieve-synchronously encoded-url)
                (buffer-string))))))
      (concat
       (format "Response Time: %.3f\n\n" (car response))
       (cadr response)))))

(defun query-http-server (method url &optional args username password)
  "method => 'http-get or 'http-post; args => '((\"k1\" \"v1\") (\"k2\" \"v2\"))"
  (let ((buffer (or (get-buffer "http-result")
                    (generate-new-buffer "http-result"))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (apply method url args username password))
      (switch-to-buffer (current-buffer)))))

(defun fetch-from-marked-uri (beg end)
  (interactive "*r")
  (query-http-server 'http-get (buffer-substring beg end)))

;; (query-http-server "http://iatse478-dev.org/p/iatse/t.pl"
;;                    '(("last" "Cameron") ("first" "Donnie")))

;; end

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

;; Berkeley DB XML
;; begin
;; (defvar query-dbxml-pipeline "/home/donnie/lib/db-xml/pipeline.pl")

;; (defun query-dbxml-with-region (beg end)
;;   "Query dbxml with selected text"
;;   (interactive "*r")
;;   (let ((newbuffer nil)
;;         (buffer (get-buffer "result"))
;;         (xquery (buffer-substring beg end)))
;;     (setq dbxml-result
;;           (cond
;;            ((buffer-live-p buffer) buffer)
;;            (t (setq newbuffer t) (generate-new-buffer "result"))))
;;     (with-current-buffer dbxml-result
;;       (with-timeout
;;           (10 (insert "Gave up because query was taking too long."))
;;         (erase-buffer)
;;         (insert (query-dbxml xquery t)))
;;       (nxml-mode)
;;       (format-xml)
;;       (goto-char (point-min))
;;       (when newbuffer (switch-to-buffer (current-buffer))))))

;; (defun query-dbxml (xquery &optional timed)
;;   "Query the Momentum Berkeley DBXML database with an XQuery string"
;;   (let ((file (make-temp-file "elisp-dbxml-")))
;;     (write-region xquery nil file)
;;     (let ((result (time (shell-command-to-string
;;                          (concat "cat " file " | " query-dbxml-pipeline)))))
;;       (delete-file file)
;;       (concat
;;        (if timed (format "%.3f seconds\n\n" (car result)) nil)
;;        (cadr result)))))
;; end

;; To allow easy building of functions that query restful Web services
(defun query-service (function &rest function-arguments)
  (let ((newbuffer nil))
    (with-current-buffer
        (cond
         ((buffer-live-p (get-buffer "result")) "result")
         (t (setf newbuffer t) (generate-new-buffer "result")))
      (erase-buffer)
      (insert (apply function function-arguments))
      (nxml-mode)
      (format-xml)
      (goto-char (point-min))
      (when newbuffer (switch-to-buffer (current-buffer))))))

;; end

;; Query momentum DBXML database
(setf momentum-host "windsor84.com")
(setf momentum-username "webmaster")
(setf momentum-password "password")

(defun momentum-dbxml-get (xquery)
  "Send XQuery to the current momentum host (momentum-host) and
   retrieve the result."
  (http-get
   (concat "http://" momentum-host "/wsp/query")
   `(("q" ,xquery))
   momentum-username
   momentum-password))

(defun query-momentum-dbxml (beg end)
  "Query the current momentum service with the XQuery in the selected region"
  (interactive "*r")
  (let ((newbuffer nil) (xquery (buffer-substring beg end)))
    (query-service 'momentum-dbxml-get xquery)))

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

;; (setf nice-fonts
;;       (list
;;        "-adobe-courier-medium-r-normal--10-*-75-75-m-60-iso10646-1"
;;        "-adobe-courier-medium-r-normal--11-*-100-100-m-60-iso10646-1"
;;        "-schumacher-clean-medium-r-normal--12-*-75-75-c-60-iso10646-1"
;;        "-schumacher-clean-medium-r-normal--13-*-75-75-c-80-iso646.1991-irv"
;;        "-schumacher-clean-medium-r-normal--14-*-75-75-c-80-iso646.1991-irv"
;;        "-schumacher-clean-medium-r-normal--15-*-75-75-c-90-iso646.1991-irv"
;;        "-schumacher-clean-medium-r-normal--16-*-75-75-c-80-iso646.1991-irv"
;;        "-adobe-courier-medium-r-normal--17-*-100-100-m-100-iso10646-1"
;;        "-adobe-courier-medium-r-normal--18-*-75-75-m-110-iso10646-1"
;;        "-adobe-courier-medium-r-normal--20-*-100-100-m-110-iso10646-1"
;;        "-adobe-courier-medium-r-normal--24-*-75-75-m-150-iso10646-1"
;;        "-adobe-courier-medium-r-normal--25-*-100-100-m-150-iso10646-1"
;;        "-adobe-courier-medium-r-normal--34-*-100-100-m-200-iso10646-1"))
;; (setf nice-font-index 2)

;; (defun set-nice-font (index)
;;   (cond
;;    ((>= index (length nice-fonts))
;;     (setf nice-font-index (1- (length nice-fonts))))
;;    ((< index 0) (setf nice-font-index 0))
;;    (t (setf nice-font-index index)))
;;   (set-default-font (nth nice-font-index nice-fonts))
;;   (message (format "%s: %s" nice-font-index (nth nice-font-index nice-fonts))))

;; (defun increase-font-size ()
;;   "Make the font bigger."
;;   (interactive)
;;   (incf nice-font-index)
;;   (set-nice-font nice-font-index))

;; (defun decrease-font-size ()
;;   "Make the font smaller."
;;   (interactive)
;;   (decf nice-font-index)
;;   (set-nice-font nice-font-index))

(defun guess-number (max)
  (interactive "nRange of 1 to what number? ")
  (let ((y (1+ (random (if (null max) 10 max))))
        (x 0))
    (while (not (= x y))
      (setf x (string-to-number
               (read-from-minibuffer
                (format "Guess a number between 1 and %s: " max))))
      (message
       (cond
        ((< x y) "Too small")
        ((> x y) "Too big")
        (t (format "==> %s <==, Yes! You got it!" x))))
      (when (not (= x y)) (sit-for 2)))))

;; (defun org-to-twiki (beg end demotion-delta)
;;   "Convert an org-mode document into Twiki topic."
;;   (interactive "r\nnDemotion delta: ")
;;   (let ((data (buffer-substring beg end)))
;;     (with-current-buffer (generate-new-buffer "new-twiki-topic")
;;       (insert data)
;;       (goto-char (point-min))
;;       (while (re-search-forward "^\\(\\*+\\) " nil t)
;;         (replace-match 
;;          (concat "---"
;;                  (make-string
;;                   (+ (length (match-string 1)) demotion-delta)
;;                   ?+)
;;                  " ")))
;;       (goto-char (point-min))
;;       (while (re-search-forward "^\\( +\\* \\)\\(.+?\\) :: " nil t)
;;         (replace-match (concat (match-string 1) "*" (match-string 2) " ::* "))))
;;     (switch-to-buffer "new-twiki-topic")
;;     (goto-char (point-min))))

;; (defun org-todo-to-twiki (beg end heading-level)
;;   "Convert an org-mode todo list into Twiki markup."
;;   (interactive "r\nnHeading level: ")
;;   (let ((data (buffer-substring beg end))
;;         (last-indent 0))
;;     (with-current-buffer (generate-new-buffer "new-twiki-topic")
;;       (insert data)
;;       (goto-char (point-min))
;;       (while (re-search-forward "^\\*+\\|^ +" nil t)
;;         (let ((match (if (match-string 0) (match-string 0) "")))
;;           (cond
;;            ((equal (substring match 0 1) "*")
;;             (cond
;;              ((= (length match) 2)
;;               (setf last-indent (+ 4 heading-level))
;;               (replace-match
;;                (concat "---" (make-string heading-level ?+))))
;;              ((> (length match) 2)
;;               (setf last-indent (* 3 (- (length match) 2)))
;;               (replace-match
;;                (concat (make-string last-indent ?\ ) "*")))
;;              (t (replace-match "!!!! "))))
;;            ((equal (substring match 0 1) " ")
;;              (replace-match
;;               (if (> last-indent 0) (make-string (+ last-indent 2) ?\ )
;;                 "!!!! ")))
;;            (t nil)))))
;;     (switch-to-buffer "new-twiki-topic")
;;     (goto-char (point-min))
;;     (replace-regexp "\n\n+" "\n")
;;     (goto-char (point-min))
;;     (while (re-search-forward "^ +\\* \\(TODO\\|DONE\\|CLOSED:\\) .+$" nil t)
;;       (replace-match (concat (match-string 0) " %BR%")))
;;     (goto-char (point-min))
;;     (while (re-search-forward "\\(^ +\\* \\)TODO " nil t)
;;       (replace-match (concat (match-string 1) "%TODO% ")))
;;     (goto-char (point-min))
;;     (while (re-search-forward "\\(^ +\\* \\)DONE " nil t)
;;       (replace-match (concat (match-string 1) "%DONE% ")))
;;     (goto-char (point-min))))

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

(defun reverse-string (beg end)
  "Reverse the highlighted string"
  (interactive "*r")
  (cl-set-buffer-substring
   beg end
   (apply 'concat (reverse (split-string (buffer-substring beg end) "")))))

(defun filter-region (beg end filter)
  "Remove lines for which the given Lisp expression returns false"
  (interactive "*r\nxExp: ")
  (let ((lines nil)
        (index 1))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
          (when (funcall filter index line) (push line lines))
          (forward-line 1)
          (incf index)))
      (delete-region (point-min) (point-max))
      (loop for line in (reverse lines) do (insert (concat line "\n"))))))

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

