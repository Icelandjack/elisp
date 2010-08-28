;; ISBNs and EANs
;; begin
(defun ean-check-digit (ean)
  (let ((check-digit
         (- 10
            (mod (loop with m = 3
                       for d in (string-to-list (substring ean 0 12))
                       summing (* (- d 48) (setf m (- 4 m))))
                 10))))
    (int-to-string (if (eql check-digit 10) 0 check-digit))))

(defun isbn10-check-digit (isbn10)
  (let ((check-digit 
         (mod
          (loop with m = 0
                for d in (string-to-list (substring isbn10 0 9))
                summing (* (- d 48) (incf m)))
          11)))
    (cond
     ((eql check-digit 10) "X")
     ((eql check-digit 11) "0")
     (t (int-to-string check-digit)))))
     
(defun append-check-digit (id clean-regexp size check-digit-function)
  (let ((clean-id
         (replace-regexp-in-string clean-regexp "" id)))
    (let ((pure-id
           (cond
            ((= (length clean-id) (1- size)) clean-id)
            ((= (length clean-id) size) (substring clean-id 0 (1- size)))
            (t nil))))
      (if pure-id (concat pure-id (funcall check-digit-function pure-id)) nil))))

(defun ean-append-check-digit (ean)
  (append-check-digit ean "[^0-9]" 13 'ean-check-digit))

(defun isbn10-append-check-digit (isbn10)
  (append-check-digit isbn10 "[^0-9Xx]" 10 'isbn10-check-digit))

(defun id-type (id)
  (let* ((clean-id
          (replace-regexp-in-string "[^0-9Xx]" "" id))
         (sizeof-id
          (length clean-id)))
    (cond
     ((and
       (eql sizeof-id 10)
       (equal (isbn10-check-digit clean-id) (substring clean-id -1)))
      'isbn10)
     ((and
       (eql sizeof-id 13)
       (equal (ean-check-digit clean-id) (substring clean-id -1)))
      'ean)
     (t nil))))

(defun extract-isbns (beg end)
  "Erase everything except ISBNs in region defined by BEG END
   And leave one ISBN per line"
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((isbns nil))
      (while (search-forward-regexp "[0-9][-0-9Xx]+" nil t)
        (when (id-type (match-string 0))
          (push (match-string 0) isbns)))
      (delete-region beg end)
      (dolist (isbn isbns)
        (insert (concat isbn "\n"))))))
  
(defun ids-to-eans (beg end)
  "Replace ISBN-10 and ISBN-13 numbers and hyphenated numbers
   with corresponding non-hyphenated EAN numbers"
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (search-forward-regexp "^.+$" nil t)
      (replace-match (to-ean (match-string 0))))
    (goto-char (point-min))
    (while (search-forward-regexp "\n\n+" nil t)
      (replace-match "\n"))))

(defun to-ean (id)
  "Convert the supplied ID into an unhyphenated EAN"
  (cond
   ((equal (id-type id) 'isbn10)
    (ean-append-check-digit (concat "978" (substring id 0 -1))))
   ((equal (id-type id) 'ean)
    (ean-append-check-digit id))
   (t "")))

(defun ean-to-path (id root extension)
  "Convert an EAN or ISBN-10 into a Bowker Image Service file system path"
  (let ((re (concat
             "^"
             (apply 'concat (loop for a from 1 to 4 collect "\\([0-9]\\{3\\}\\)"))))
        (ean (to-ean id)))
    (if (id-type ean)
        (progn
          (string-match re ean)
          (concat
           (if (equal (substring root -1) "/") (substring root 0 -1) root)
           (apply 'concat
                  (loop for a from 4 downto 1 collect (concat "/" (match-string a ean))))
           "/"
           ean
           extension))
      "")))

(defvar *image-store-path* "/www/image_store/gtin")

(defun eans-to-paths (beg end)
  "Convert a list of EANs into a list of Bowker Image Service file system paths"
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
        (delete-region (point-at-bol) (point-at-eol))
        (insert (ean-to-path line *image-store-path* ".png"))
          (forward-line 1)))))

  
;; end


;; Mark Logic Server
;; begin
(setq mlhost "mldeveval.bowker-dmz.com")
;; (setq mlhost "bertha")
(setq mldb "bowker_dev")
(setq mldbport "9002")
(setq mlmarket "USA")
(setq mlusername "admin")
(setq mlpassword "password")
(setq abs-host "mldeveval.bowker-dmz.com")
(setq abs-port "9024")
(setq qbit-host "mldeveval.bowker-dmz.com")
(setq qbit-port "9024")
(setq bms2-username "bowker")
(setq bms2-password "password")
;; (setq bms2-username "bmsall")
;; (setq bms2-password "password")
(setq bms2-uri "http://interloper:3000/rest/books/search")
(setq loader-uri "http://mldeveval.bowker-dmz.com:9002/u/loader.xqy")
(setq ml-utilities-uri "http://mldeveval.bowker-dmz.com:9002/u/utilities.xqy")

;; Get the market uid of a record given the record's ISBN and market
;; code. Returns a number, the market_uid of the specified record.
(defun get-market-uid(host db isbn market)
  (last-number-in-string
    (mlpost host db
            (concat "let $x:= /item_info[.//isbn13 = '"
                    (replace-regexp-in-string "[^0-9X]" "" isbn)
                    "'][.//market_code = '"
                    market
                    "'] return $x//search_market_uid/text()"))))

(defun get-market-uids(beg end)
  "Get the market_uid's for the ISBNs in the region defined by BEG END
   Each line in the region must contain nothing but an ISBN-13. The
   function will write the market_uid for each ISBN immediately to the
   right of the ISBN."
  (interactive "*r")
  (catch 'cancel
    (save-restriction
      (narrow-to-region beg end)

      (goto-char (point-min))
      (while (not (eobp))
        (goto-char (point-at-eol))
        (insert
         (concat
          " "
          (int-to-string
           (get-market-uid mlhost
                           mldb
                           (buffer-substring (point-at-bol) (point-at-eol))
                           mlmarket))))
        (forward-line 1)))))

;;   (dolist (isbn (split-string (buffer-substring beg end)))
;;     (insert
;;      (concat (int-to-string (get-market-uid host db isbn market)) "\n"))))

(defun get-rcl-item-uid(host db isbn)
  "Obtains the rcl_item_uid for the given ISBN from host:db.
   The ISBN you provide must be a hyphenated ISBN-13."
  (last-number-in-string
   (mlpost host db
           (concat "/rcl_item_info[.//isbn13 = '"
                   isbn
                   "']/rcl_item_rec/rcl_item_uid/text()"))))

(defun get-rcl-item-uids(beg end)
  "Get the rcl_item_uid's for the ISBNs in the region defined by BEG END
   Each line in the region must contain nothing but an ISBN-13. The
   function will write the rcl_item_uid for each ISBN immediately to the
   right of the ISBN."
  (interactive "*r")
  (catch 'cancel
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (goto-char (point-at-eol))
        (insert
         (concat
          " "
          (int-to-string
           (get-rcl-item-uid
            mlhost
            mldb
            (buffer-substring (point-at-bol) (point-at-eol))))))
        (forward-line 1)))))

(defun last-number-in-string(s)
  (string-to-number (car (last (split-string s)))))

(defun mlpost (host db xquery)
  "Send xquery to the specified host and database.
   Return a string with the raw reply."
  (http-post
   (concat "http://" host ":" mldbport "/u/pipeline.xqy")
   `(("database" ,db) ("queryInput" ,xquery))
   mlusername mlpassword))

(defun abpost (host port xmlquery)
  "Send XML Query to the Aquabrowser Query Service.
   Return a string with the raw reply."
  (http-post
   (concat "http://" host ":" port "/xml.xqy")
   `(("xmlnode" ,xmlquery))
   mlusername mlpassword))

(defun qbit-post (host port xmlquery)
  "Send an XML Search Expression to the Qbit Service.
   Return a string with the raw reply."
  (http-post
   (concat "http://" host ":" port "/qbit/qbit.xqy")
   `(("xml" ,xmlquery))
   mlusername mlpassword))

(defun bms2-post (uri xmlquery)
  "Send an XML Search Expression to the BMS-2 Web Service.
   Return a stering with the raw reply."
  (http-get uri `(("xml" ,xmlquery)) bms2-username bms2-password))

(defun loader-post (uri xml)
  "Send an XML document to the loader Web service."
  (http-post uri `(("xml" ,xml)) mlusername mlpassword))

(defun sort-test-get (host port query)
  "Run the program sort-test.xqy with no parameters"
  (http-get (concat "http://" host ":" port "/sort-test.xqy")
            nil mlusername mlpassword))

(defun query-sort-test ()
  "Run the program sort-test.xqy on the Mark Logic server"
  (interactive)
  (query-service 'sort-test-get abs-host abs-port nil)
  (message (concat abs-host ":" abs-port)))

(defun query-ml-server (beg end)
  "Query the Mark Logic server with the XQuery in the region defined by BEG END"
  (interactive "*r")
  (query-service 'mlpost mlhost mldb (buffer-substring beg end)))

(defun query-ab-service (beg end)
  "Query the Mark Logic Aquabrowser query service with the specified XML"
  (interactive "*r")
  (query-service 'abpost abs-host abs-port (buffer-substring beg end)))

(defun query-qbit (beg end)
  "Query the Qbit service with the selected XML"
  (interactive "*r")
  (query-service 'qbit-post qbit-host qbit-port (buffer-substring beg end)))

(defun query-bms2 (beg end)
  "Query the BMS-2 Web service with the selected XML"
  (interactive "*r")
  (query-service 'bms2-post bms2-uri (buffer-substring beg end)))

(defun send-to-loader (beg end)
  "Send selected XML to the loader Web service."
  (interactive "*r")
  (query-service 'loader-post loader-uri (buffer-substring beg end)))

(defun query-ml-utilities (beg end)
  "Query the Mark Logic utilities module with the selected XML"
  (interactive "*r")
  (query-service 'loader-post ml-utilities-uri (buffer-substring beg end)))

(defun get-ml-db () 
  (concat mlhost ":" mldb))

(defun get-ab-service ()
  (concat abs-host ":" abs-port))

(defun get-qbit-service ()
  (concat qbit-host ":" qbit-port))

(defun set-ml-db (host-db)
  (let ((a (split-string host-db ":")))
    (setq mlhost (car a))
    (setq mldb (cadr a))
    (get-ml-db)))

(defun set-ab-service (host-port)
  (let ((a (split-string host-port ":")))
    (setq abs-host (car a))
    (setq abs-port (cadr a))
    (get-ab-service)))

(defun set-qbit-service (host-port)
  (let ((a (split-string host-port ":")))
    (setq qbit-host (car a))
    (setq qbit-port (cadr a))
    (get-qbit-service)))

(defun profiler-dmss-environment ()
  (interactive)
  (message
   (string-join
    " "
    (list
     (set-ml-db "bertha.solomonstreet.com:data-mining")
     (setq mldbport "9002")
     (set-ab-service "bertha.solomonstreet.com:9024")
     (set-qbit-service "bertha.solomonstreet.com:9024")
     (setq bms2-uri "http://bote.solomonstreet.com:3000/rest/books/search")
     (setq loader-uri "http://bertha.solomonstreet.com:9002/u/loader.xqy")
     (setq ml-utilities-uri "http://bertha.solomonstreet.com:9002/u/utilities.xqy")))))

(defun profiler-mother-environment ()
  (interactive)
  (message
   (string-join
    " "
    (list
     (set-ml-db "mother:xdl-queue")
     (setq mldbport "9002")
     (set-ab-service "mother:9024")
     (set-qbit-service "mother:9024")
     (setq bms2-uri "http://bote.solomonstreet.com:3000/rest/books/search")
     (setq loader-uri "http://mother:9002/u/loader.xqy")
     (setq ml-utilities-uri "http://mother:9002/u/utilities.xqy")))))

(defun profiler-evidence-1-environment ()
  (interactive)
  (message
   (string-join
    " "
    (list
     (set-ml-db "evidence-1:xdl-queue")
     (setq mldbport "9002")
     (set-ab-service "evidence-1:9023")
     (set-qbit-service "evidence-1:9023")
     (setq bms2-uri "http://interloper:3000/rest/books/search")
     (setq bms2-username "bowker")
     (setq bms2-password "password")
     (setq loader-uri "http://evidence-1:9002/u/loader.xqy")
     (setq ml-utilities-uri "http://evidence-1:9002/u/utilities.xqy")))))

(defun profiler-sinistercode-environment ()
  (interactive)
  (message
   (string-join
    " "
    (list
     (set-ml-db "profiler.sinistercode.com:xdl-queue")
     (setq mldbport "9002")
     (set-ab-service "profiler.sinistercode.com:9023")
     (set-qbit-service "profiler.sinistercode.com:9023")
     (setq bms2-uri "http://profiler.sinistercode.com:3000/rest/books/search")
     (setq bms2-username "bowker")
     (setq bms2-password "password")
     (setq loader-uri "http://profiler.sinistercode.com:9002/u/loader.xqy")
     (setq ml-utilities-uri
           "http://profiler.sinistercode.com:9002/u/utilities.xqy")))))

(defun profiler-cwub3009-environment ()
  (interactive)
  (message
   (string-join
    " "
    (list
     (set-ml-db "cwub3009:xdl-queue")
     (setq mldbport "9004")
     (set-ab-service "cwub3009:9023")
     (set-qbit-service "cwub3009:9023")
     (setq bms2-uri "http://cwub3009:3000/rest/books/search")
     (setq loader-uri "http://cwub:9004/u/loader.xqy")
     (setq bms2-username "bmsall")
     (setq bms2-password "password")
     (setq ml-utilities-uri "http://cwub:9004/u/utilities.xqy")))))


(defun profiler-dmmlprod1 ()
  (interactive)
  (message
   (string-join
    " "
    (list
     (set-ml-db "dmmlprod1.bowker-dmz.com:xdl-queue-1")
     (setq mldbport "9004")))))
    
(defun profiler-dmmltest1 ()
  (interactive)
  (message
   (string-join
    " "
    (list
     (set-ml-db "dmmltest1.bowker-int.com:xdl-queue")
     (setq mldbport "9004")))))

(defun profiler-dmappdev1-environment ()
  (interactive)
  (message
   (string-join
    " "
    (list 
     (set-ml-db "mldeveval.bowker-dmz.com:bowker_dev")
     (setq mldbport "9002")
     (set-ab-service "mldeveval.bowker-dmz.com:9024")
     (set-qbit-service "mldeveval.bowker-dmz.com:9024")
     (setq bms2-uri "http://mldeveval.bowker-dmz.com:9024/qbit/qbit.xqy")
     (setq loader-uri "http://mldeveval.bowker-dmz.com:9002/u/loader.xqy")))))

(defun profiler-dmprofdev-environment ()
  (interactive)
  (message
   (string-join
    " "
    (list
     (set-ml-db "mldevload1.bowker-int.com:xdl-queue")
     (setq mldbport "9004")
     (set-ab-service "mldeveval.bowker-dmz.com:9027")
     (set-qbit-service "mldeveval.bowker-dmz.com:9027")
     (setq bms2-uri "http://dmprofdev:3000/rest/books/search")
     (setq loader-uri "http://mldevload1.bowker-dmz.com:9004/u/loader.xqy")))))

(defun profiler-dmproftest-environment ()
  (interactive)
  (message
   (string-join
    " "
    (list
     (set-ml-db "dmmltest1.bowker-int.com:xdl-queue")
     (setq mldbport "9004")
     (set-ab-service "dmmltest1.bowker-int.com:9023")
     (set-qbit-service "dmmltest1.bowker-int.com:9023")
     (setq bms2-uri "http://dmproftest:3000/rest/books/search")
     (setq loader-uri "http://dmmltest1.bowker-int.com:9004/u/loader.xqy")))))

(defun profiler-dmprofprod-environment ()
  (interactive)
  (message
   (string-join
    " "
    (list
     (set-ml-db "dmmlprod1.bowker-dmz.com:xdl-queue-1")
     (setq mldbport "9004")
     (set-ab-service "dmmlprod1.bowker-dmz.com:9023")
     (set-qbit-service "dmmlprod1.bowker-dmz.com:9023")
     (setq bms2-uri "http://bmsv2-1.bowker-dmz.com/rest/books/search")
     (setq loader-uri "http://dmmlprod1.bowker-dmz.com:9004/u/loader.xqy")))))

(defun ml3eval1-dev-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ml-db "ml3load1.bowker-dmz.com:bowker_prod")
     (setq mldbport "9002")
     (set-ab-service "ml3eval1.bowker-dmz.com:9024")
     (set-qbit-service "ml3eval1.bowker-dmz.com:9024")
     (setq bms2-uri "http://bmsv2-1.bowker-dmz.com/rest/books/search")))))

(defun mltst-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ml-db "mltsteval.bowker-dmz.com:bowker_test")
     (setq mldbport "9002")
     (set-ab-service "mltsteval.bowker-dmz.com:9023")
     (set-qbit-service "mltsteval.bowker-dmz.com:9023")
     (setq bms2-uri "http://bmsv2-1.bowker-dmz.com/rest/books/search")))))

(defun ml3eval1-qa-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ml-db "ml3load1.bowker-dmz.com:bowker_prod")
     (setq mldbport "9002")
     (set-ab-service "ml3eval1.bowker-dmz.com:9025")
     (set-qbit-service "ml3eval1.bowker-dmz.com:9025")
     (setq bms2-uri "http://bmsv2-1.bowker-dmz.com/rest/books/search")))))

(defun ml3eval1-alpha-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ml-db "ml3load1.bowker-dmz.com:bowker_prod")
     (setq mldbport "9002")
     (set-ab-service "ml3eval1.bowker-dmz.com:9023")
     (set-qbit-service "ml3eval1.bowker-dmz.com:9023")
     (setq bms2-uri "http://bmsv2-1.bowker-dmz.com/rest/books/search")))))

(defun ml4eval1-alpha-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ml-db "ml4load1.bowker-dmz.com:bowker_prod")
     (setq mldbport "9002")
     (set-ab-service "ml4eval1.bowker-dmz.com:9023")
     (set-qbit-service "ml4eval1.bowker-dmz.com:9023")
     (setq bms2-uri "http://bmsv2-1.bowker-dmz.com/rest/books/search")))))

(defun mleval-title-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ml-db "ml4load1.bowker-dmz.com:bowker_prod")
     (setq mldbport "9002")
     (set-ab-service "mleval-title.bowker-dmz.com:9023")
     (set-qbit-service "mleval-title.bowker-dmz.com:9023")
     (setq bms2-uri "http://bmsv2-1.bowker-dmz.com/rest/books/search")))))

(defun ml4eval1-dev-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ml-db "ml4load1.bowker-dmz.com:bowker_prod")
     (setq mldbport "9002")
     (set-ab-service "ml4eval1.bowker-dmz.com:9024")
     (set-qbit-service "ml4eval1.bowker-dmz.com:9024")
     (setq bms2-uri "http://bmsv2-1.bowker-dmz.com/rest/books/search")))))

(defun ml5eval1-prod-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ab-service "ml5eval1.bowker-dmz.com:9023")
     (setq mldbport "9002")
     (set-qbit-service "ml5eval1.bowker-dmz.com:9023")))))

(defun ml4eval1-dev-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ml-db "ml4load1.bowker-dmz.com:bowker_prod")
     (setq mldbport "9002")
     (set-ab-service "ml4eval1.bowker-dmz.com:9024")
     (set-qbit-service "ml4eval1.bowker-dmz.com:9024")
     (setq bms2-uri "http://bmsv2-1.bowker-dmz.com/rest/books/search")))))

(defun mldeveval-bk-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ml-db "mldeveval.bowker-dmz.com:bowker_dev")
     (setq mldbport "9002")
     (set-ab-service "mldeveval.bowker-dmz.com:9024")
     (set-qbit-service "mldeveval.bowker-dmz.com:9024")))))

(defun ml-item-info-environment ()
  (interactive)
  (message
   (string-join " "
                (list (set-ml-db "192.168.246.248:bowker_prod")))))

(defun ml3prod1-dev-environment ()
  (interactive)
  (message
   (string-join 
    " "
    (list
     (set-ml-db "ml3prod1.bowker-dmz.com:bowker_prod")
     (setq mldbport "9002")
     (set-ab-service "mlprod1.bowker-dmz.com:9024")
     (set-qbit-service "mlprod1.bowker-dmz.com:9024")
     (setq bms2-uri "http://bmsv2-1.bowker-dmz.com/rest/books/search")))))

(defun format-mantis-issues (beg end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((line 0))
        (while (re-search-forward
                ; "^\t+Update\tIssue[\t ]+\\([0-9]+\\)\t[0-9]+\t\\(.+\\)\t\\(major|minor\\)\t[^\t]+\t\\(-0-9]+\\)[\t ]+\\(.+\\)$"
                (concat "^\t+\\(Update Issue\\)" ; 1 - Update Issue
                        "[\t ]+\\([0-9]+\\)"     ; 2 - Mantis Issue #
                        "\t[0-9 ]+\t\\(.+\\)"    ; 3 - Category
                        "\t\\(major\\|minor\\)"  ; 4 - Severity
                        "\t\\(.+\\)"             ; 5 - Status
                        "\t\\([-0-9]+\\)"        ; 6 - Last updated
                        "[\t ]+\\(.*\\)$")       ; 7 - Summary
                nil t)
          (incf line)
          (replace-match
           (concat (number-to-string line)
                   ". \\2 \\4 \\6 \\7")))))))

(defun fn-query-ml (xquery)
  (let ((r (mlpost mlhost mldb xquery)))
    (progn (string-match "max=[0-9]+\n\n\\(.*\\)" r)
           (match-string 1 r))))

(defun delete-relevant-words-nodes (limit)
  (let* ((n 0.0)
         (max (string-to-number 
               (fn-query-ml "xdmp:estimate(/title-rec[relevant-words])"))))
    (when (< limit max) (setf max limit))
    (while 
        (progn
          (fn-query-ml
           "xdmp:node-delete((/title-rec[relevant-words])[1]/relevant-words)")
          (print (format "%d%%" (* (/ n max) 100.0)))
          (< n max)))))

(defun make-ia-doc-id (type element)
  (let ((s (buffer-name)))
    (concat
     "<" element ">"
     "bowker-profiler-trtest-" type "-"
     (progn (string-match "\\([0-9]+\\)" s) (match-string 0 s))
     "</" element ">")))

(defun replace-title-au-id ()
  (loop for a in '(("Title" "id") ("Identifier" "id") ("Author" "au")) do
        (beginning-of-buffer)
        (replace-regexp
         (concat "<" (car a) ">\\([^<]+\\)</" (car a) ">")
         (make-ia-doc-id (cadr a) (car a)))))

;; Miscellaneous functions
(setq doc-store "/home/donnie/bowker/profiler-tr/data/doc-store")

(defun filename-from-uuid (uuid ext)
  "Load the file that corresponds to the given uuid"
  (let* ((newbuffer nil)
         (buffer (get-buffer "result"))
         (s (string-join "" (split-string uuid "-")))
         (r (apply #'concat (cons "^\\([0-9a-f]\\)"
                                  (mapcar (lambda (x) "\\([0-9a-f]\\{3\\}\\)")
                                          '(1 2 3 4)))))
         (parts (progn (string-match r s)
                       (mapcar (lambda (x) (match-string x s))
                               '(1 2 3 4 5))))
         (filename (concat doc-store "/" (string-join "/" parts) "/"
                           uuid ext)))
    filename))

(defun load-highlighted (beg end)
  (interactive "*r")
  "Load the text file that corresponds to the highlighted uuid"
  (let* ((newbuffer nil)
         (buffer (get-buffer "result"))
         (parts (split-string (buffer-substring beg end) "\\."))
         (extension (concat "." (second parts)))
         (filename (filename-from-uuid (car parts) extension)))
    (setq text-buffer
          (cond
           ((buffer-live-p buffer) buffer)
           (t (setq newbuffer t) (generate-new-buffer "result"))))
    (with-current-buffer text-buffer 
      (erase-buffer)
      (insert (concat filename "\n\n"))
      (insert-file-contents (concat "" filename))
      (goto-char (point-min))
      (when newbuffer (switch-to-buffer (current-buffer))))))

(fset 'order-profile
   [?\M-< ?\C-s ?< ?p ?r ?o ?f ?: ?\C-a ?\C-  ?\C-s ?< ?/ ?p ?r ?o ?f ?: ?r ?e ?p ?o ?r ?t ?> ?\M-w ?\C-x ?h S-insert ?\M-< ?\M-% ?p ?r ?o ?f ?: return return ?! ?\M-< ?\M-% ?\{ return ?\[ ?\[ backspace return ?! ?\M-< ?\M-% ?\} return ?\] return ?! ?\M-< ?l ?e ?t ?  ?$ ?x ?: ?= ?  return C-right ?\C-k ?> ?\M-> return ?f ?o ?r ?  ?$ ?x backspace ?y ?  ?i ?n ?  backspace backspace backspace backspace backspace ?e ?  ?n ?  backspace backspace ?i ?n ?  ?$ ?x ?/ ?h ?i ?s ?t ?o ?g ?r ?a ?m ?/ ?* backspace ?e ?x ?p ?r ?e ?s ?s ?i ?o ?n return ?  ?  ?o ?r ?d ?e ?r ?  ?b ?y ?  ?$ ?e ?/ ?s ?h backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace ?w ?h ?e ?r ?e ?  ?e backspace ?$ ?e ?/ ?s ?h ?a ?l ?l ?o ?w ?- ?t ?i ?m ?e ?  ?n ?e ?  ?\" ?P ?T ?0 ?S ?\" return ?  ?  ?o ?r ?d ?e ?r ?  ?b ?y ?  ?$ ?e ?/ ?s ?h ?a ?l ?l ?o ?w ?- ?t ?i ?m ?e ?  ?d ?e ?s ?c ?e ?n ?d ?i ?n ?g return tab ?r ?e ?t ?u ?r ?n ?  ?$ ?e up up up up up down return ?r ?e ?t ?u ?r ?n ?  ?e ?l ?e ?m ?e ?n ?t ?  ?p ?r ?o ?f ?i ?l ?e ?  ?\{ down ?\C-a ?\C-  down down down right right ?\C-x ?r ?o ?\M-> ?\} return ?\M-x ?x ?q ?u ?e ?r ?y ?  ?m ?o ?d ?e return])

(fset 'bip2-result->title-item-uids
   [?\C-x ?h S-insert ?\M-< ?\C-  C-down C-down down ?l ?e ?t ?  ?$ ?r ?: ?= ?  ?\M-> return ?f ?o ?r ?  ?$ ?i ?  ?i ?n ?  ?$ ?r ?/ ?/ ?i ?t ?m backspace backspace ?i ?t ?e backspace backspace ?t ?e backspace backspace backspace ?t ?e ?m ?- ?r ?e ?c return ?r ?e ?t ?u ?r ?n ?  ?$ backspace backspace ?c ?o backspace backspace ?  ?c ?o ?n ?c ?a ?t ?\( ?$ ?i ?/ ?t ?i ?t ?l ?e ?- ?u ?i ?d ?  backspace ?, ?  ?\" ?  ?- ?> ?\S-  ?\" ?, ?  ?$ ?i ?/ ?i ?t ?e ?m ?- ?u ?d ?i backspace backspace ?i ?d ?\) return ?\C-x ?h ?\M-> ?\C-x ?h])

;; end