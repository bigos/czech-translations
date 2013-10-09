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

(defun chapter-path (folder arglist)
  (merge-pathnames (format nil "~a/~a/~a/~a" folder
                           (nth 0 arglist)
                           (nth 1 arglist)
                           (nth 2 arglist))
                   *default-pathname-defaults*))

(defun print-parsed (ch)
  "unfinished experimental code"
  ;;(ch '("PNS" "Zj" 21))
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

(defun try-extracted ()
  (let ((extracted-path) (try-path) (x))
    (dolist (b (subseq (each-translation-book-chapter) 0 2))
      (setq extracted-path (chapter-path "extracted-path" b)
            try-path (chapter-path "try" b))
      (with-open-file (in-stream extracted-path)
        (setq x (lml2 in-stream))) ;todo
      (with-open-file (out-stream try-path :direction :output)
        (format out-stream "~a" x)))))

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


(defmacro every-extracted (body)
  `(funcall ,body ))

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
