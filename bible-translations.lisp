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

(defun chapter-path (folder tran book chapter)
  (merge-pathnames (format nil "~a/~a/~a/~a" folder tran book chapter)
                   *default-pathname-defaults*))

(defun chapter-path (folder arglist)
  (merge-pathnames (format nil "~a/~a/~a/~a" folder
                           (nth 0 arglist)
                           (nth 1 arglist)
                           (nth 2 arglist))
                   *default-pathname-defaults*))

(defun print-parsed ()
  "unfinished experimental code"
  (let* ((ch '("PNS" "Zj" 21))
         (downloaded (chapter-path "downloaded" ch)))
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
                  (format stream "~a" (download translation-code book-code chapter)))
                )
           ))))
