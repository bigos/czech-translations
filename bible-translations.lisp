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

;; (defun parsed-path (tran book chapter)
;;         (merge-pathnames (format nil "parsed/~a/~a/~a" tran book chapter)))

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
