;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname directorix) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(require htdp/dir)



; data definitions

#;
(define-struct file [name size content])
; An File is a [String N String]
#;
(define (fn-on-file f)
  (... (fn-on-string (file-name f))
       ... (fn-on-num (file-size f))
       ... (fn-on-string (file-content f))))


; An LFile is one of
; – '()
; - (cons File LFile)
#;
(define (fn-on-lfile lf)
  (match lf
    [(? empty?) ...]
    [(? list?) (map fn-on-file lf)]))


; An LDir is one of
; – '()
; - (cons Dir LDir)
#;
(define (fn-on-ldir ld)
  (match ld
    [(? empty?) ...]
    [(? list?) (map fn-on-dir ld)]))

#;
(define-struct dir [name dirs files size readability])
; A Dir is a [String [ListOf Dir] [ListOf File]]
#;
(define (fn-on-dir dir)
  (... (fn-on-string (dir-name dir))
       ... (fn-on-ldir (dir-dirs dir))
       ... (fn-on-lfile (dir-files dir))))



; =================================
; constants

(define O (create-dir "/Users/robert/Documents")) ; on OS X
(define draw (make-file "draw" 2 ""))
(define hang (make-file "hang" 8 "you rang?"))
(define read!2 (make-file "read!" 19 "better stuff"))
(define Docs (make-dir "Docs" '() `(,read!2)))
(define Code (make-dir "Code" '() `(,hang ,draw)))
(define part3 (make-file "part3" 17 "and void"))
(define part2 (make-file "part2" 52 "the earth was without form"))
(define part1 (make-file "part1" 99 "in the beginning"))
(define Libs (make-dir "Libs" `(,Code ,Docs) '()))
(define read!1 (make-file "read!" 10 "good stuff"))
(define Text (make-dir "Text" '() `(,part1 ,part2 ,part3)))
(define TS (make-dir "TS" `(,Text ,Libs) `(,read!1)))



; =================================
; functions


(define (how-many dir)
  ; Dir -> N
  ; determines the number of files in a directory and its subs
  (+ (foldr + 0 (map how-many (dir-dirs dir)))
     (length (dir-files dir))))


(define (find? nm dir)
  ; String Dir -> Boolean
  ; determines if a given filename corresponds to a
  ; file in a directory tree with root Dir
  (or
   (ormap
    (lambda (f) (string=? nm (file-name f)))
    (dir-files dir))
   (ormap
    (lambda (d) (find? nm d))
    (dir-dirs dir))))


(define (ls dir)
  ; Dir -> [ListOf String]
  ; lists all directories and files contained in a given directory
  (append (map dir-name (dir-dirs dir)) (map file-name (dir-files dir))))


(define (du dir)
  ; Dir -> N
  ; tallys up the total storage requirements a given directory
  (foldr + 1 (append (map file-size (dir-files dir))
                     (map du (dir-dirs dir)))))


(define (find nm dir)
  ; String Dir -> [Maybe [ListOf String]]
  ; returns the path to a file, if the file exists
  (local (
          ; a) check files in this dir before looking deeper
          (define check-files
            (filter (lambda (f) (string=? nm f))
                    (map file-name (dir-files dir)))))
    ; - IN -
    (cond
      [(empty? check-files)
       ;[(false? check-files)
       ; nothing found in a); dig deeper
       (local (
               (define check-subdirs
                 (cons
                  #f ; b) ensures the list of results isn't empty
                  (map (lambda (d) (find nm d)) (dir-dirs dir)))))
         ; - IN -
         (cond
           [(andmap boolean? check-subdirs) #f] ; see b) above 
           [else (cons (dir-name dir)
                       (first  ; first success is all you need
                        (filter list? check-subdirs)))]))] ; gets rid of #f's
      [else (cons (dir-name dir) check-files)]))) ; success in a); return result


(define (find-all nm dir)
  ; String Dir -> [Maybe [ListOf [ListOf String]]]
  ; returns the path to a file, if the file exists
  (local (
          (define check-files
            (filter (lambda (f) (string=? nm f))
                    (map file-name (dir-files dir))))
          (define check-subdirs
            (foldr append '()
                   (map (lambda (d) (find-all nm d)) (dir-dirs dir))))
          (define check-contents (cons check-files check-subdirs)))
    ; - IN -
    (map (lambda (l) (cons (dir-name dir) l))
         (filter (lambda (x) (not (empty? x))) check-contents))))


(define (ls-R dir)
  ; Dir -> [ListOf [ListOf String]]
  ; lists the paths to every file in the given directory's tree
  (local (
          (define herein-files
            (map (lambda (f) (list (dir-name dir) f))
                 (map file-name (dir-files dir))))
          (define herein-subdirs
            (map (lambda (p) (cons (dir-name dir) p))
                 (foldr append '()
                        (map (lambda (d) (ls-R d)) (dir-dirs dir))))))
    ; - IN -
    (append herein-files herein-subdirs)))


; ======================
; checks

(check-expect (how-many Docs) 1)
(check-expect (how-many TS) 7)
(check-expect (find? "hang" TS) #t)
(check-expect (find? "hang" Text) #f)
(check-expect (ls TS) '("Text" "Libs" "read!"))
(check-expect (ls Docs) '("read!"))
(check-expect (du Docs) 20)
(check-expect (du TS) (+ 10 99 52 17 19 8 2 5))
(check-expect (find "ready" Docs) #f)
(check-expect (find "read!" Docs) '("Docs" "read!"))
(check-expect (find "part3" TS) '("TS" "Text" "part3"))
(check-expect (find "hang" TS) '("TS" "Libs" "Code" "hang"))
(check-expect (find "ready" TS) #f)
(check-expect (find "read!" TS) '("TS" "read!"))
(check-expect (find-all "hang" TS) '(("TS" "Libs" "Code" "hang")))
(check-expect (find-all "ready" TS) '())
(check-expect (find-all "read!" TS) '(("TS" "read!") ("TS" "Libs" "Docs" "read!")))
(check-expect (ls-R Libs)
              '(("Libs" "Code" "hang") ("Libs" "Code" "draw")
                                       ("Libs" "Docs" "read!")))


; ==========================
; action!



(define X (create-dir "/Users/robert/Documents"))
(find-all "HW1.jl" X)