;;;; bible-translations.lisp

(in-package #:bible-translations)

(defparameter *pentateuch*
  '("Gn"  50 "Ex" 40 "Lv" 27 "Nu" 36 "Dt" 34))

(defparameter *hebrew-scriptures*
  (append *pentateuch*
          '("Joz" 24 "Sd" 21 "Rt" 4 "1S" 31 "2S" 24 "1Kr" 22 "2Kr" 25
            "1Pa" 29 "2Pa" 36 "Ezd" 10 "Neh" 13 "Est" 10
            "Jb" 42 "Z" 150  "Pr" 31 "Kaz" 12 "Pis" 8 "Iz" 66
            "Jr" 52 "Pl" 22 "Ez" 48 "Da" 12 "Oz" 14 "Jl" 20
            "Am" 9 "Abd" 21 "Jon" 4 "Mi" 7 "Na" 3
            "Abk" 3 "Sf" 3 "Ag" 2 "Za" 14 "Mal" 4)))

(defparameter *greek-scriptures*
  '("Mt" 28 "Mk" 16 "L" 24 "J" 21 "Sk" 28 "R" 16
    "1K" 16 "2K" 13 "Ga" 6 "Ef" 6 "Fp" 4
    "Ko" 4 "1Te" 5 "2Te" 3 "1Tm" 6 "2Tm" 4 "Tit" 3
    "Fm" 1 "Zd" 13 "Jk" 5 "1P" 5 "2P" 3
    "1J" 5 "2J" 1 "3J" 1 "Ju" 1 "Zj" 22))


(defparameter *canonical-books*
  (append *hebrew-scriptures* *greek-scriptures*))

(defparameter *apocrypha*
  '("Tob" 14 "Jud" 16 "Mou" 19 "Sir" 51 "Bar" 6
    "1Ma" 16 "2Ma" 15 "Zuz" 1 "Bel" 1))

(defparameter *catholic-books*
  (append *canonical-books* *apocrypha*))

(defparameter *translations* '(("CSP" "Czech Study Bible Translation (CZ)")
                               ("PMP" "Miloš Pavlík Translation - NT (CZ)")
                               ("ZP" "František Žilka Translation - NT (CZ)")
                               ("OP" "Ondřej M. Petrů Translation - NT (CZ)")
                               ("CRP" "Czech Rabbinical Translation (Pentateuch - CZ)")
                               ("BKR1" "Bible of Kralice (CZ - 1579)")
                               ("BKR" "Bible of Kralice (CZ - 1613)")
                               ("NBK98" "New Bible of Kralice (CZ) (1998 - only NT)")
                               ("NBK" "New Bible of Kralice (CZ) (2002 - only NT)")
                               ("NBK06" "New Bible of Kralice (CZ) (2006)")
                               ("B21" "Bible 21 (CZ)")
                               ("PNS" "New World Translation of the Holy Scriptures")
                               ("NK" "New Covenant")
                               ("COL" "New Testament - Dr. Rudolf Col (1947)")
                               ("HEJCL" "Dr. Jan Hejčl (1930) - Old Testament and Deuterocanonical books")
                               ("ROH" "Jozef Roháček Translation - Autorized Version Dušan Seberíni (SK)")
                               ("SKP" "Slovak Catholic translation")
                               ("SEP" "Slovak ecumenical translation (2008)")
                               ("SEVP" "Slovak Evangelical translation")))

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
