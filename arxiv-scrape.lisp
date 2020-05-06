(ql:quickload (list "alexandria" "drakma" "s-xml" "archive" "gzip-stream" "trivial-gray-streams" "cl-ppcre" "flexi-streams"))

(defparameter *arxiv-prefix* "math.AT")

(defun process-tex (raw-contents)
  (handler-case
      (loop :for i :from 1
            :for line := (read-line raw-contents)
            :with ended? := nil
            :unless ended?
              :do (setf ended? (cl-ppcre:scan "\\\\end\\{document\\}" line))
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

(defun ID->metadata (id)
  (let* ((url (ID->metadata-URL id))
         (raw-octets (drakma:http-request url))
         (raw-string (flexi-streams:octets-to-string raw-octets))
         (xml (s-xml:parse-xml-string raw-string))
         (response (rest (assoc 'NS-2:|entry| xml))))
    (values (second (assoc 'NS-2:|title| response))
            (loop :for (sigil . body) :in response
                  :when (eql sigil 'NS-2:|author|)
                    :collect (second (first body))))))

(defun print-metadata (id)
  (multiple-value-bind (title authors) (ID->metadata id)
    (format t "Title: ~a~%By: ~{~a~^, ~}~%" title authors)))

(defun rss-url ()
  (format nil "https://arxiv.org/rss/~a" *arxiv-prefix*))

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
                ((every (lambda (x) (< x 128)) unzipped-sequence)
                 (with-input-from-string
                     (s (flexi-streams:octets-to-string unzipped-sequence))
                   (process-tex s)))
                (t
                 (error "Found a high octet. Probably not plain-text."))))))
      (error (c)
        (format t "== Caught error: ~a ==~%" c)
        (format t "== UNKNOWN FILE FORMAT ==~%")))))

(defun popular-IDs (count)
  (let* ((url (format nil "http://export.arxiv.org/api/query?search_query=cat:~a&start=0&max_results=~a"
                      *arxiv-prefix* count))
         (raw-octets (drakma:http-request url))
         (raw-string (flexi-streams:octets-to-string raw-octets))
         (xml (nthcdr 8 (s-xml:parse-xml-string raw-string))))
    (loop :for blob :in xml
          :collect (abs-url->id (second (assoc 'NS-2:|id| (rest blob)))))))

(defun scrape-popular (count &key (sleep-delay 10))
  (dolist (id (popular-IDs count))
    (sleep sleep-delay)
    (print-metadata id)
    (process-ID id)
    (format t "~%~%~%=========================~%~%~%")))

(defun recent-IDs ()
  (let* ((rss-response (drakma:http-request (rss-url)))
         (xml (s-xml:parse-xml-string rss-response)))
    (loop :for item :in (cdadr (assoc 'NS-0:|items| (second xml)))
          :collect (ABS-URL->ID (third (first item))))))

(defun scrape-recent (&key (sleep-delay 10))
  (dolist (id (recent-IDs))
    (sleep sleep-delay)
    (print-metadata id)
    (process-ID id)
    (format t "~%~%~%=========================~%~%~%")))
