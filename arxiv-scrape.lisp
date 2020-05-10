(ql:quickload (list "alexandria" "drakma" "xmls" "archive" "gzip-stream" "trivial-gray-streams" "cl-ppcre" "flexi-streams"))

(defparameter *arxiv-prefix* "math.AT")

(defun process-tex (raw-contents)
  (handler-case
      (loop :for i :from 1
            :for line := (read-line raw-contents)
            :with ended? := nil
            :unless ended?
              :do (setf ended? (and (cl-ppcre:scan "\\\\end\\{document\\}" line)
                                    (not (cl-ppcre:scan "\\\\bye" line))))
                  (alexandria:when-let ((comment (cl-ppcre:scan-to-strings "%(.*)" line
                                                                           :sharedp t)))
                    (format t "[~5d] ~a~%" i comment))
            :when ended?
              :do (format t "[~5d] ~a~%" i line))
    (end-of-file ()
      nil)))

(defun entry->string (entry)
  (let* ((bounded-stream (archive:entry-stream entry))
         (size (slot-value bounded-stream 'archive::n-bytes-remaining))
         (array (make-array size
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (trivial-gray-streams:stream-read-sequence bounded-stream array 0 size)
    (flexi-streams:octets-to-string array)))

(defun ABS-URL->ID (url)
  (aref (nth-value 1 (cl-ppcre:scan-to-strings "abs/(.*)" url)) 0))

(defun ID->GZ-URL (id)
  (format nil "https://arxiv.org/e-print/~a" id))

(defun ID->metadata-URL (id)
  (format nil "http://export.arxiv.org/api/query?id_list=~a" id))

(defun xml-find (xml name)
  (find name (xmls:node-children xml) :key #'xmls:node-name :test #'string=))

(defun ID->metadata (id)
  (let* ((url (ID->metadata-URL id))
         (raw-octets (drakma:http-request url))
         (raw-string (flexi-streams:octets-to-string raw-octets))
         (xml (xmls:parse raw-string))
         (payload (xml-find xml "entry")))
    ;; (VALUES TITLE AUTHORS)
    (values (first (xmls:node-children (xml-find payload "title")))
            (loop :for child :in (xmls:node-children payload)
                  :when (string= "author" (xmls:node-name child))
                    :collect (first (xmls:node-children (first (xmls:node-children child))))))))

(defun print-metadata (id)
  (multiple-value-bind (title authors) (ID->metadata id)
    (format t "Title: ~a~%By: ~{~a~^, ~}~%" title authors)))

(defun rss-url ()
  (format nil "https://arxiv.org/rss/~a" *arxiv-prefix*))

(defun unicodeish-p (byte-array)
  (flet ((asciip (c) (<= c 126)))
    (let ((length (length byte-array)))
      (every (lambda (a b c d e)
               (some #'asciip (list a b c d e)))
             byte-array
             (make-array (- length 1) :element-type '(unsigned-byte 8)
                                      :displaced-to byte-array :displaced-index-offset 1)
             (make-array (- length 2) :element-type '(unsigned-byte 8)
                                      :displaced-to byte-array :displaced-index-offset 2)
             (make-array (- length 3) :element-type '(unsigned-byte 8)
                                      :displaced-to byte-array :displaced-index-offset 3)
             (make-array (- length 4) :element-type '(unsigned-byte 8)
                                      :displaced-to byte-array :displaced-index-offset 4)))))

(defun process-ID (id)
  (let* ((gzip-link (ID->GZ-URL id))
         (raw-contents (drakma:http-request gzip-link)))
    (handler-case
        (let ((unzipped-sequence (gzip-stream:gunzip-sequence raw-contents)))
          (handler-case
              (flexi-streams:with-input-from-sequence (s unzipped-sequence)
                (let ((archive (archive:open-archive 'archive:tar-archive s)))
                  (unwind-protect
                       (archive:do-archive-entries (entry archive)
                         (when (equalp ".TEX" (coerce (last (coerce (archive:name entry) 'list) 4) 'string))
                           (format t "== Processing ~A ==~%" (archive:name entry))
                           (with-input-from-string (s (entry->string entry))
                             (process-tex s))))
                    (archive:close-archive archive))))
            (error (c)
              (format t "== Caught error: ~a ==~%" c)
              (format t "== PROCESSING AS RAW TEX ==~%")
              (cond
                ((unicodeish-p unzipped-sequence)
                 (with-input-from-string
                     (s (flexi-streams:octets-to-string unzipped-sequence))
                   (process-tex s)))
                (t
                 (error "Found a high octet. Probably not plain-text."))))))
      (error (c)
        (format t "== Caught error: ~a ==~%" c)
        (format t "== UNKNOWN FILE FORMAT ==~%")))))

(defun popular-IDs (count &key (start 0))
  (let* ((url (format nil "http://export.arxiv.org/api/query?search_query=cat:~a&start=~a&max_results=~a"
                      *arxiv-prefix* start count))
         (raw-octets (drakma:http-request url))
         (raw-string (flexi-streams:octets-to-string raw-octets))
         (xml (xmls:parse raw-string)))
    (loop :for item :in (nthcdr 7 (xmls:node-children xml))
          :for children := (xmls:node-children item)
          :for subitem := (find "id" children :key #'xmls:node-name :test #'string=)
          :collect (abs-url->id (first (xmls:node-children subitem))))))

(defun scrape-popular (count &key (start 0) (sleep-delay 10))
  (dolist (id (popular-IDs count :start start))
    (sleep sleep-delay)
    (print-metadata id)
    (process-ID id)
    (format t "~%~%~%=========================~%~%~%")))

(defun recent-IDs ()
  (let* ((rss-response (drakma:http-request (rss-url)))
         (xml (xmls:parse rss-response)))
    (loop :for node :in (cddr (xmls:node-children xml))
          :for (str payload) := (assoc "about" (xmls:node-attrs node) :test #'string=)
          :collect (ABS-URL->ID payload))))

(defun scrape-recent (&key (sleep-delay 10))
  (dolist (id (recent-IDs))
    (sleep sleep-delay)
    (print-metadata id)
    (process-ID id)
    (format t "~%~%~%=========================~%~%~%")))

(defun scrape-list (IDs &key (sleep-delay 10))
  (dolist (id IDs)
    (sleep sleep-delay)
    (print-metadata id)
    (process-ID id)
    (format t "~%~%~%=========================~%~%~%")))
