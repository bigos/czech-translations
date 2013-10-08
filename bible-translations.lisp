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

(defun sample-parsed ()
  '((:!DOCTYPE
    " html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"")
   ((:HTML :XMLNS "http://www.w3.org/1999/xhtml")
    (:HEAD
     ((:META :HTTP-EQUIV "Content-Type" :CONTENT "text/html; charset=utf-8"))
     ((:META :HTTP-EQUIV "language" :CONTENT "CZ"))
     ((:META :NAME "Author" :CONTENT))
     ((:META :NAME "viewport" :CONTENT
             "initial-scale=1, maximum-scale=1, user-scalable=no"))
     ((:SCRIPT :LANGUAGE "JavaScript" :CHARSET "utf-8" :SRC "tooltip.js"))
     ((:STYLE :TYPE "text/css" :MEDIA "all"))
     ((:LINK :REL "stylesheet" :TYPE "text/css" :HREF "style.css"))
     ((:SCRIPT :SRC "ajax.js" :TYPE "text/javascript"))
     ((:SCRIPT :TYPE "text/javascript" :SRC "styleswitcher.js"))
     ((:LINK :REL "alternate stylesheet" :TYPE "text/css" :HREF "css/nula.css"
             :TITLE "normal"))
     (:TITLE
      "New World"))
    ((:BODY :BGCOLOR "#FFFFFF" :TEXT "#000000" :LINK "#FF0000" :VLINK "#804000"
            :ALINK "#B55515" :LEFTMARGIN "20" :TOPMARGIN "5" :RIGHTMARGIN "20"
            :BOTTOMMARGIN "10" :MARGINWIDTH "20")
     ((:DIV :ALIGN "center")
      ((:DIV :ALIGN "center")
       ((:DIV :ID "nadpisyknih")
        ((:SPAN :CLASS "nazev_knih")
         "New World"))))
     ((:DIV :ALIGN "center")
      ((:DIV :ID "blok_versu")
       "A viděl jsem nové nebe" (:SUP "1")
       " a novou zemi," (:SUP "2") " neboť dřívější nebe" (:SUP "3")
       " a dřívější země" (:SUP "4") " pominuly a moře" (:SUP "5") " již není. "

       "&nbsp;" ((:SPAN :CLASS "cisloverse") "2") "Viděl jsem také svaté město"
       (:SUP "1") ", Nový Jeruzalém, jak sestupuje z nebe od Boha" (:SUP "2")
       " a " (:EM "je") " připravený jako nevěsta" (:SUP "3")
       " ozdobená pro svého manžela."))))))

(defun walk (x)
  (let ((head (car x))
        (tail (cdr x)))
    (format T "~&>>> ~a ~%" x)
    (when (listp head) (walk head))
    (when tail (walk tail))))

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
