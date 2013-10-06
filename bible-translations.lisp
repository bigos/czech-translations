;;;; bible-translations.lisp

(in-package #:bible-translations)


(format t "~&~S~%" *translations*)

(defun run ( translation book chapter)
  (let* ((downloaded
          (drakma:http-request
           (format nil "http://www.obohu.cz/bible/index.php?~a&~a&~d"
                   (format nil "styl=~a" translation)
                   (format nil "k=~a" book)
                   (format nil "kap=~a" chapter))))
         (parsed (html-parse:parse-html downloaded)))
    ;;the line below extracts text of the chapter
    (nth 6 (nth 2 (nth 2 (cadr parsed))))))

;; you can run it like this: (run "JB" "J" 1)


(defun downloaded-folder (tran book)
  (merge-pathnames (format nil "downloaded/~a/~a/" tran book) *default-pathname-defaults*))


(defun zzz ()
  (let ((translation-code)
        (book-chapters)
        (translation-name)
        (book-code)
        (max-chapter))
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
                (ensure-directories-exist
                 (downloaded-folder translation-code book-code))
                (format t "~s ~s ~s  " translation-code book-code chapter))
           ))))
