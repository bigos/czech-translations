;;;; bible-translations.lisp

(in-package #:bible-translations)


(format t "~&~S~%" *translations*)

(defun download ( translation book chapter)
  (let* ((downloaded
          (drakma:http-request
           (format nil "http://www.obohu.cz/bible/index.php?~a&~a&~d"
                   (format nil "styl=~a" translation)
                   (format nil "k=~a" book)
                   (format nil "kap=~a" chapter))
           :external-format-in :UTF-8 ))
         (parsed (html-parse:parse-html downloaded)))
    ;;the line below extracts text of the chapter
    ;;(nth 6 (nth 2 (nth 2 (cadr parsed))))
     downloaded))

;; you can run it like this: (run "JB" "J" 1)

(defun downloaded-path (tran book chapter)
  (merge-pathnames (format nil "downloaded/~a/~a/~a" tran book chapter)
                   *default-pathname-defaults*))

(defun chapter-path (folder arglist &optional (suffix ""))
  (merge-pathnames (format nil "~a/~a/~a/~a~a"
                           folder
                           (nth 0 arglist)
                           (nth 1 arglist)
                           (nth 2 arglist)
                           suffix)
                   *default-pathname-defaults*))

(defun print-parsed (ch)
  (let* ((downloaded (chapter-path "downloaded" ch)))
    (with-open-file (stream downloaded)
      (html-parse:parse-html stream))))

(defun each-translation-book-chapter ()
  (let ((results))
    (dolist (translation *translations*)
      (dolist (book-data (cadr translation))
        (dotimes (zchapter (cadr book-data))
          (setq results (nconc results
                               (list (list (car translation)
                                           (car book-data)
                                           (1+ zchapter))))))))
    results))

;;; let closing over several functions ;; start ;;;;;;;;;
(let ((result))
  (defun walk-init ()
    (setq result nil))

  (defun walk (x)
    (let ((head (car x))
          (tail (cdr x)))
      (if (equalp head '(:DIV :ID "blok_versu"))
          (setf result (nconc result x))
          (progn
            (when (listp head) (walk head))
            (when tail (walk tail))))))

  (defun walk-tree (x)
    (walk-init)
    (walk x)
    result))
;;; let closing over several functions ;; end ;;;;;;;

(defun genesis-1 ()
  (loop for x in (each-translation-book-chapter)
     when (and (equal (nth 1 x) "Gn")
               (eq (nth 2 x) 1))
     collect x))

(defun revelation-21 ()
  (loop for x in (each-translation-book-chapter)
     when (and (equal (nth 1 x) "Zj")
               (eq (nth 2 x) 21))
     collect x))
;; getting beginning (subseq) of Revelation 21 in every translation
;; (dolist  (b (revelation-21))
;;   (format t "~S ~S~%~%~%" (car b)  (subseq (walk-tree (print-parsed b)) 0 10)))

(defun extract-text ()
  (let ((saved))
    (dolist  (b (each-translation-book-chapter))
      (format t "~S  ~S~%" (nth 0 b) (nth 1 b))
      (setq saved (chapter-path "extracted" b))
      (ensure-directories-exist saved)
      (with-open-file (stream saved :direction :output )
        (format stream "'~S" (walk-tree (print-parsed b)))))))

(defun translation-data (code)
  (loop for tr in *translations*
     when (equalp code (car tr))
     return  tr))

(defun book-nw-name (book-code)
  (loop for bk in *catholic-books*
     when (equalp book-code (car bk))
     return (third bk)))

(defun create-html  (extracted preklad kniha kapitola)
  `(:html (:head
           ((:META :HTTP-EQUIV "Content-Type"
                   :CONTENT "text/html; charset=utf-8"))
           ((:STYLE :TYPE "text/css")
            "body{} .cisloverse{color:green; padding-right:0.5em;}")
           ((:LINK :REL "stylesheet" :TYPE "text/css"
                   :HREF "../../style.css"))
           (:TITLE
            ,(format nil "~a  - kniha ~a - kapitola ~a"
                     (third (translation-data preklad))
                     kniha
                     kapitola)))
          (:body
           (:h1 ((:a :href ,(format nil "../../index_~a.html" (string-downcase preklad)))
                  ,(format nil "preklad ~a kniha ~a kapitola ~a"
                          preklad
                          (book-nw-name kniha)
                          kapitola)))
           (:hr)
           ,(eval extracted))
          (:hr)
          ((:div :class "other_translations")
           ,@(same-chapter-links kniha kapitola))))


(defun same-chapter-links (bk ch)
  (let ((path)
        (tr-codes
         (loop for tr in *translations*
            collecting (car tr))))
    (loop for tc in tr-codes
       do
         (setq path (format nil "~a/~a/~a.html" tc bk ch ))
       when (probe-file (merge-pathnames (format nil "extracted/~a/~a/~a" tc bk ch)))
       collect (format nil "<a href=\"../../~a\">~a</a>"  path tc))))

(defun translation-indexes ()
  (let ((tr-codes
         (loop for tr in *translations*
            collecting (car tr))))
    (loop for tc in tr-codes
       collecting (format nil "<a href=\"index_~a.html\">~a</a>" (string-downcase tc) tc))))

(defun index-links (tr1 b c)
  (loop for x from 1 to c
     collect (format nil "<a href=\"~a/~a/~a.html\"> ~A </a> " tr1 b x x)))

(defun create-index-file (tr)
  `(:html (:head
           ((:META :HTTP-EQUIV "Content-Type"
                   :CONTENT "text/html; charset=utf-8"))
           ((:LINK :REL "stylesheet" :TYPE "text/css"
                   :HREF "style.css"))
           (:TITLE
            ,(format nil "Index - ~a" (third tr))
            ))
          (:body
           (:h3 ,(format nil "~a" (third tr)))
           ,@(loop for book in (second tr)
                collecting
                  `(:div (:h5 ,(third  book)) ,@(index-links
                                                 (first tr)
                                                 (first book)
                                                 (second book))))
           (:hr)
           ((:div :class "other_translations")
            ,@(translation-indexes)))))

(defun create-indexes ()
  (let ((index-file) )
    (dolist (tr *translations*)
      (format t "~s ~s~%~%" (first tr) (third tr))
      (setq index-file (format nil "try/index_~a.html"
                               (string-downcase (first tr))))
      (with-open-file (stream (merge-pathnames
                               index-file
                               *default-pathname-defaults*)
                              :direction :output
                              :if-exists :supersede)
        (lml2:html-print (create-index-file tr) stream)))))

(defun try-extracted ()
  (let ((extracted-path) (try-path) (extracted) (preklad) (kniha) (kapitola))
    (dolist (b (subseq (each-translation-book-chapter) 0 ))
      (setq preklad (first b)
            kniha (second b)
            kapitola (third b) )
      (setq extracted-path (chapter-path "extracted" b)
            try-path (chapter-path "try" b ".html"))
      (with-open-file (in-stream extracted-path)
        (setq extracted (read in-stream)))
      (ensure-directories-exist try-path)
      (with-open-file (out-stream try-path
                                  :direction :output
                                  :if-exists :supersede)
        (lml2:html-print
         (create-html extracted preklad kniha kapitola)
         out-stream)))))

(defun verify-extracted ()
  (let ((extracted-path) (extracted) (fragment))
    (dolist (b (subseq (each-translation-book-chapter) 0 2))
      (setq extracted-path (chapter-path "extracted" b))
      (with-open-file (stream extracted-path)
        (setq extracted (read stream)))
      (setq fragment (subseq extracted 0 2))
      (format t "~&>>> ~S~%" (caadr fragment))
      (if (equal (caadr fragment)
                 '(:DIV :ID "blok_versu"))
          (princ #\T) (princ #\-)))))

(defun translation-books (translation)
  (loop for x in (each-translation-book-chapter)
     when (equal (nth 0 x) translation)
     collect x))

(defun zzz ()
  (let ((translation-code)
        (book-chapters)
        (translation-name)
        (book-code)
        (max-chapter)
        (downloaded-page))
    (dolist (tran *translations*)
      (setq translation-code (nth 0 tran)
            book-chapters    (nth 1 tran)
            translation-name (nth 2 tran))
      (format t "~&~s~%~%" translation-code)
      (loop
         for bx from 0 to (1- (list-length book-chapters)) by 2
         do
           (setq book-code (nth bx book-chapters))
           (setq max-chapter (nth (1+ bx) book-chapters))
           (format t "~%")
           (loop for chapter from 1 to max-chapter
              do
                (sleep 1)
                (setq downloaded-page
                      (downloaded-path translation-code book-code chapter))
                (ensure-directories-exist downloaded-page)
                (format t "~s ~s ~s  " translation-code book-code chapter)
                (with-open-file (stream downloaded-page :direction :output)
                  (format stream
                          "~a"
                          (download translation-code book-code chapter))))))))
