#!/usr/bin/guile \
-e main -s
!#

; hs - don't ask.
; Revision 2 (4/15/03)
; Copyright (C) 2003  Jeff Binder (jbinder@members.fsf.org)

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


(use-modules (ice-9 getopt-long)
	     (ice-9 regex)
	     (ice-9 rdelim)
	     (ice-9 format)
	     (srfi srfi-1))

(define option-spec
  '((version (single-char #\v) (value #f))
    (help    (single-char #\h) (value #f))
    (make-sense (value #f))
    (obfuscate (value #f))
    (syntax (value #f))))

(define make-sense? #f)
(define syntax? #f)

(define (main args)
  (let ((options (getopt-long
                  args
                  option-spec)))

    (if (option-ref options 'make-sense #f)
	(set! make-sense? #t))

    (if (option-ref options 'syntax #f)
	(set! syntax? #t))

    (cond
     ((option-ref options 'help #f)
      (format #t 
"~a: the only truly usable programming language
Extremely simple usage: ~a FILE

The --make-sense option causes hs to spew out all kinds of information.

The --syntax option allows you to specify the tree directly in LISP syntax.
  It's like the trees outputted by --make-sense, except without the last
  element in the vectors.
"
              (car args)
              (car args)))

     ((option-ref options 'version #f)
      (format #t
       "Versions are an outmoded concept.\n"))

     (#t
      (run ((if syntax? read parse) (open (cadar options) O_RDONLY)))))))


(define (parse in)
  (let ((first-line (read-line in 'concat)))
    (if (eof-object? first-line)
	(begin
	  (display "In Homespring, the null program is not a quine.\n")
	  (exit 0))
	(let read ((tokens '())
		   (line first-line))
	  (if (eof-object? line)
	      (begin
		(if make-sense?
		    (begin
		      (write tokens)
		      (newline)))
		(really-parse tokens))
	      (read (append tokens
			    (parse-line line))
		    (read-line in 'concat)))))))

(define regex (make-regexp "((\\. | \\.|[^ .])*\\.?)[ \n](.*)"))
(define (parse-line line)
  (let ((list '()))
  (while (not (string=? line ""))
      (let ((match (regexp-exec regex line)))
	(if (not match)
	    '()
	    (let ((token (regexp-substitute/global #f
			  " \\."
			  (regexp-substitute/global #f
			   "\\. "
			   (regexp-substitute/global #f
			    "([^ ]|^)\\.($|[^ ])"
			    (substring (vector-ref match 0)
				       (car (vector-ref match 2))
				       (cdr (vector-ref match 2)))
			    'pre 1 "\n" 2 'post)
			   'pre " " 'post)
			  'pre "." 'post))
		  (rest (substring (vector-ref match 0)
				       (car (vector-ref match 4))
				       (cdr (vector-ref match 4)))))
	      (set! list (cons token list))
	      (set! line rest)))))
  (reverse list)))

(define (really-parse tokens)
  (let ((tree
	 (let do-that-funky-thing ((node (list (vector (car tokens)
						       #f #f #f '() #f '())))
				   (tokens (cdr tokens)))
	   
	   (if (null? tokens)
	       (let end ((t node))
		 (if (null? (vector-ref (car t) 6))
		     t
		     (end (vector-ref (car t) 6))))
	       (if (and (string=? (car tokens) "")
			(not (null? (vector-ref (car node) 6))))
		   (do-that-funky-thing (vector-ref (car node) 6) (cdr tokens))
		   (let ((new (list (vector (car tokens)
					    #f #f #f
					    '() #f node))))
		     (set-cdr! node (append (cdr node) (list new)))
		     (do-that-funky-thing new (cdr tokens))))))))
    (tree-fold
     (lambda (node)
       (if (and (not (null? (cdr node)))
		(null? (cddr node)))
	   (set-cdr! (cdr node) (list (list (vector "" #f #f #f
						    '() #f '()))))))

     tree)
    tree))

(define (run tree)
  (while #t
	 (if make-sense?
	     (begin
	       (display "doing the thing...\n")
	       (write tree)
	       (newline)))
	 (process-snowmelts! tree)
	 (process-water! tree)
	 (process-electricity! tree)
	 (process-salmon! tree)
	 (process-etc.! tree)
	 (if (char-ready?)
	     (let ((line (read-line)))
	       (if (eof-object? line)
		   (exit 0)
		   (vector-set! (car tree) 4
				(cons
				 (allocate-salmon!!!! line #t #t)
				 (node-salmon.. tree))))))))

(define (tree-fold proc tree)
  (proc tree)
  (if (null? (cdr tree))
      #f
      (begin
	(tree-fold proc (cadr tree))
	(tree-fold proc (caddr tree)))))

(define (tree-fold-after proc tree)
  (if (null? (cdr tree))
      #f
      (begin
	(tree-fold-after proc (cadr tree))
	(tree-fold-after proc (caddr tree))))
  (proc tree))

(define (tree-fold-middle proc tree)
  (if (null? (cdr tree))
      #f
      (tree-fold-middle proc (cadr tree)))
  (proc tree)
  (if (null? (cdr tree))
      #f
      (tree-fold-middle proc (caddr tree))))

(define (process-snowmelts! tree)
  (tree-fold
   (lambda (node)
     (if (and (not (null? (cdr node)))
	      (not (snowmelt-blocked??? node))
	      (or (and (node-snowmelt?? (cadr node))
		       (or (not (vector-ref (caadr node) 5))
			   (begin
			     (if make-sense?
				 (display
       "the snowmelt slows down for the marsh, only to speed up again...\n"))
			     (vector-set! (caadr node) 5 #f)
			     #f))
		       (begin
			 (vector-set! (caadr node) 1 #f)
			 #t))
		  (and (node-snowmelt?? (caddr node))
		       (or (not (vector-ref (caaddr node) 5))
			   (begin
			     (if make-sense?
				 (display
       "the snowmelt slows down for the marsh, only to speed up again...\n"))
			     (vector-set! (caaddr node) 5 #f)
			     #f))
		       (begin
			 (vector-set! (caaddr node) 1 #f)
			 #t))))
	 (begin
	   (if make-sense?
	       (format #t
		"downward flows the snowmelt, that it reaches the ~a...\n"
		(node-feature.. node)))
	   (if (and (is-this-thing-one-of-these-types-of-things????
		     (node-feature.. node)
		     '("marshy"))
		    (eqv? (vector-ref (car node) 1) #f))
	       (vector-set! (car node) 5 #t))
	   (vector-set! (car node) 1 #t)
	   (break-stuff!!! node)))

     (create-snowmelt!!! node))
   tree))

(define (process-water! tree)
  (tree-fold
   (lambda (node)
     (if (node-water?? node)
	 (vector-set! (car node) 2 #f))

     (create-water!!! node)

     (if (and (not (null? (cdr node)))
	      (or (node-water?? (cadr node))
		  (node-water?? (caddr node)))
	      (not (water-blocked??? node)))
	 (begin
	   '(if make-sense?
	       (format #t
		"downward flows the water, that it reaches the ~a...\n"
		(node-feature.. node)))
	   (vector-set! (car node) 2 #t))))
   tree))

(define (process-electricity! tree)
  (tree-fold-after
   (lambda (node)
     (if (node-elec?? node)
	 (vector-set! (car node) 3 #f))
     (create-elec!!! node))
   tree))

(define (process-salmon! tree) ;; Mmm, processed salmon...

#!  (display "now\n")
  (tree-fold
   (lambda (node)
     (map (lambda (fish)
	    (if (or (string=? (fish-name... fish) "something")
		    (string=? (fish-name... fish) "0")
		    (string=? (fish-name... fish) "1")
		    (string=? (fish-name... fish) "0n")
		    (string=? (fish-name... fish) "1n")
		    (string=? (fish-name... fish) "= 1\n"))
		(format #t "~a (~a) going ~a at ~a\n"
			(fish-name... fish)
			(fish-downstream??? fish)
			(fish-young??? fish)
			(node-feature.. node))))
	  (node-salmon.. node)))
   tree)
!#

  (map (lambda (fish)
	 (if (and (fish-downstream??? fish)
		  (not (salmon-blocked??? tree)))
	     (begin
	       (if make-sense?
		   (format #t
		    "the fish ~a has reached the great beyond...\n"
		    (fish-name... fish)))
	       (display (fish-name... fish))
	       (vector-set! (car tree) 4
			    (delete
			     fish
			     (node-salmon.. tree))))))
       (node-salmon.. tree))

  (tree-fold
   (lambda (node)
     (if (not (null? (cdr node)))
	 (begin
	   (map (lambda (fish)
		  (if (and (fish-downstream??? fish)
			   (not (salmon-blocked??? (cadr node)))
			   (not (down-salmon-blocked??? node))
			   (not (salmon-very-blocked??? node))
			   (or (not (mature-salmon-blocked??? node))
			       (fish-young??? fish))
			   (or (not (young-salmon-blocked??? node))
			       (fish-mature??? fish))
			   (or (not
				(is-this-thing-one-of-these-types-of-things????
				 (node-feature.. (cadr node))
				 '("shallows")))
			       (not (fish-mature??? fish))
			       (fish-held-up??? fish)
			       (begin
				 (if make-sense?
				     (format #t
				      "the fish ~a is stuck...\n"
				      (fish-name... fish)))
				 (vector-set! fish 3 #t)
				 #f))
			   (or (not
				(is-this-thing-one-of-these-types-of-things????
				 (node-feature.. (cadr node))
				 '("rapids")))
			       (fish-mature??? fish)
			       (fish-held-up??? fish)
			       (begin
				 (if make-sense?
				     (format #t
				      "the fish ~a is stuck...\n"
				      (fish-name... fish)))
				 (vector-set! fish 3 #t)
				 #f)))
		      (begin
			(vector-set! fish 3 #f)
			(vector-set! fish 4 #t)
			(if make-sense?
			    (format #t
			 "the fish ~a has reached ~a, nearer the end...\n"
			 (fish-name... fish)
			 (node-feature.. node)))
			(vector-set! (caadr node) 4
				     (delete
				      fish
				      (node-salmon.. (cadr node))))
			(vector-set! (car node) 4
				     (cons
				      fish
				      (node-salmon.. node))))))
		(node-salmon.. (cadr node)))
	   (map (lambda (fish)
		  (if (and (fish-downstream??? fish)
			   (not (salmon-blocked??? (caddr node)))
			   (not (down-salmon-blocked??? node))
			   (not (salmon-very-blocked??? node))
			   (or (not (mature-salmon-blocked??? node))
			       (fish-young??? fish))
			   (or (not (young-salmon-blocked??? node))
			       (fish-mature??? fish))
			   (or (not
				(is-this-thing-one-of-these-types-of-things????
				 (node-feature.. (caddr node))
				 '("shallows")))
			       (not (fish-mature??? fish))
			       (fish-held-up??? fish)
			       (begin
				 (if make-sense?
				     (format #t
				      "the fish ~a is stuck...\n"
				      (fish-name... fish)))
				 (vector-set! fish 3 #t)
				 #f))
			   (or (not
				(is-this-thing-one-of-these-types-of-things????
				 (node-feature.. (caddr node))
				 '("rapids")))
			       (fish-mature??? fish)
			       (fish-held-up??? fish)
			       (begin
				 (if make-sense?
				     (format #t
				      "the fish ~a is stuck...\n"
				      (fish-name... fish)))
				 (vector-set! fish 3 #t)
				 #f)))
		      (begin
			(vector-set! fish 3 #f)
			(vector-set! fish 4 #f)
			(if make-sense?
			    (format #t
			 "the fish ~a has reached ~a, nearer the end...\n"
			 (fish-name... fish)
			 (node-feature.. node)))
			(vector-set! (caaddr node) 4
				     (delete
				      fish
				      (node-salmon.. (caddr node))))
			(vector-set! (car node) 4
				     (cons
				      fish
				      (node-salmon.. node))))))
		(node-salmon.. (caddr node))))))
   tree)

  (tree-fold-after
   (lambda (node)
     (map (lambda (fish)
	    (if (fish-upstream??? fish)
		(fish-move!!! fish node)))
	  (node-salmon.. node)))
   tree)

  (tree-fold
   (lambda (node)
     (create-salmon!!! node))
   tree))

(define (process-etc.! tree)
  (tree-fold
   (lambda (node)
     (if (and (is-this-thing-one-of-these-types-of-things????
	       (node-feature.. node)
	       '("universe"))
	      (node-snowmelt?? node))
	 (begin
	   (if make-sense?
	       (display "the world ends.\n"))
	   (exit 0)))
     (if (and (is-this-thing-one-of-these-types-of-things????
	       (node-feature.. node)
	       '("upstream killing device"))
	      (node-powered?? node))
	 (begin
	   (if make-sense?
	       (display "die, upstream salmon...\n"))
	   (vector-set! (caaddr node) 4 '())))
     (if (and (is-this-thing-one-of-these-types-of-things????
	       (node-feature.. node)
	       '("spawn"))
	      (node-powered?? node))
	 (begin
	   (if make-sense?
	       (display "spawn, upstream salmon...\n"))
	   (tree-fold
	    (lambda (node)
	      (map (lambda (fish)
		     (fish-spawn!!! fish node))
		   (node-salmon.. node)))
	    node)))
     (if (is-this-thing-one-of-these-types-of-things????
	  (node-feature.. node)
	  '("clone"))
	 (begin
	   (if make-sense?
	       (display "clonin' a salmon...\n"))
	   (map (lambda (fish)
		  (vector-set! (car node) 4
			       (cons
				(allocate-salmon!!!!
				 (fish-name... fish)
				 #f #f)
				(node-salmon.. node))))
		(node-salmon.. node))))
     (if (is-this-thing-one-of-these-types-of-things????
	  (node-feature.. node)
	  '("bear"))
	 (begin
	   (if (and (not (null? (node-salmon.. node)))
		    make-sense?)
	       (display "bear eats...\n"))
	   (vector-set! (car node) 4
			(filter
			 fish-young???
			 (node-salmon.. node)))))
     (if (is-this-thing-one-of-these-types-of-things????
	  (node-feature.. node)
	  '("young bear"))
	 (begin
	   (if (and (not (null? (node-salmon.. node)))
		    make-sense?)
	       (display "young bear eats...\n"))
	   (vector-set! (car node) 4
			(append
			 (filter
			  fish-young???
			  (node-salmon.. node))
			 (let eliminate-every-other
			     ((list (filter
				     fish-mature???
				     (node-salmon.. node))))
			   (if (or (null? list)
				   (null? (cdr list)))
			       '()
			       (cons (car list)
				     (eliminate-every-other (cddr list)))))))))
     (if (is-this-thing-one-of-these-types-of-things????
	  (node-feature.. node)
	  '("bird"))
	 (begin
	   (if (and (not (null? (node-salmon.. node)))
		    make-sense?)
	       (display "bird eats...\n"))
	   (vector-set! (car node) 4
			(filter
			 fish-mature???
			 (node-salmon.. node)))))
     (if (is-this-thing-one-of-these-types-of-things????
	  (node-feature.. node)
	  '("youth fountain"))
	 (begin
	   (if (and (not (null? (node-salmon.. node)))
		    make-sense?)
	       (display "youth bestowed...\n"))
	   (map (lambda (fish)
		  (vector-set! fish 2 #f))
		(node-salmon.. node))))
     (if (is-this-thing-one-of-these-types-of-things????
	  (node-feature.. node)
	  '("time"))
	 (begin
	   (if (and (not (null? (node-salmon.. node)))
		    make-sense?)
	       (display "youth bestowed...\n"))
	   (map (lambda (fish)
		  (vector-set! fish 2 #t))
		(node-salmon.. node))))
     (if (is-this-thing-one-of-these-types-of-things????
	  (node-feature.. node)
	  '("oblivion"))
	 (begin
	   (if (and (not (null? (node-salmon.. node)))
		    make-sense?)
	       (display "fish's memory erased...\n"))
	   (map (lambda (fish)
		  (vector-set! fish 0 ""))
		(node-salmon.. node))))
     (if (is-this-thing-one-of-these-types-of-things????
	  (node-feature.. node)
	  '("split"))
	 (begin
	   (if (and (not (null? (node-salmon.. node)))
		    make-sense?)
	       (display "salmon splitting...\n"))
	   (let ((salmon (node-salmon.. node)))
	     (vector-set! (car node) 4 '())
	     (map (lambda (fish)
		    (string-map
		     (lambda (char)
		       (vector-set! (car node) 4
				    (cons (allocate-salmon!!!!
					   (string char)
					   (fish-upstream??? fish)
					   (fish-mature??? fish))
					  (vector-ref
					   (car node) 4))))
		     (fish-name... fish))
		  salmon)))))
     (if (and (is-this-thing-one-of-these-types-of-things????
	       (node-feature.. node)
	       '("reverse up" "force up"))
	      (not (null? (node-salmon.. node)))
	      (not (salmon-blocked??? (cadr node)))
	      (not (salmon-very-blocked??? (cadr node))))
	 (begin
	   (if make-sense?
	       (format #t "fish reversed up to ~a...\n"
		       (node-feature.. (cadr node))))
	   (vector-set! (car node) 4
			(filter
			 (lambda (fish)
			   (if (and (not (vector-ref fish 4))
				    (fish-downstream??? fish)
				    (or (not (mature-salmon-blocked???
					      (cadr node)))
					(fish-young??? fish))
				    (or (not (young-salmon-blocked???
					      (cadr node)))
					(fish-mature??? fish)))
			       (begin
				 (vector-set! fish 1 #t)
				 (vector-set! (caadr node) 4
					      (cons fish
						    (vector-ref
						     (caadr node) 4)))
				 #f)
			       #t))
			 (node-salmon.. node)))))
     (if (and (is-this-thing-one-of-these-types-of-things????
	       (node-feature.. node)
	       '("reverse down" "force down"))
	      (not (null? (node-salmon.. node)))
	      (not (salmon-blocked??? (caddr node)))
	      (not (salmon-very-blocked??? (caddr node))))
	 (begin
	   (if make-sense?
	       (format #t "fish reversed down to ~a...\n"
		       (node-feature.. (caddr node))))
	   (vector-set! (car node) 4
			(filter
			 (lambda (fish)
			   (if (and (vector-ref fish 4)
				    (fish-downstream??? fish)
				    (or (not (mature-salmon-blocked???
					      (caddr node)))
					(fish-young??? fish))
				    (or (not (young-salmon-blocked???
					      (caddr node)))
					(fish-mature??? fish)))
			       (begin
				 (vector-set! fish 1 #t)
				 (vector-set! (caaddr node) 4
					      (cons fish
						    (vector-ref
						     (caaddr node) 4)))
				 #f)
			       #t))
			 (node-salmon.. node)))))
     (if (is-this-thing-one-of-these-types-of-things????
	       (node-feature.. node)
	       '("append up"))
	 (begin
	   (if (and make-sense?
		    (is-one-of-these-salmon-up???? (node-salmon.. node))
		    (is-one-of-these-salmon-upstream???? (node-salmon.. node)))
	       (display "appending fish...\n"))
	   (let ((appendage ""))
	     (vector-set! (car node) 4
			  (filter
			   (lambda (fish)
			     (if (or (fish-upstream??? fish)
				     (vector-ref fish 4))
				 #t
				 (begin
				   (set! appendage (string-append
						    appendage
						    (fish-name... fish)))
				   #f)))
			   (node-salmon.. node)))
	     (map
	      (lambda (fish)
		(if make-sense?
		    (format #t "appending fish ~a...\n"
			    (fish-name... fish)))
		(if (fish-upstream??? fish)
		    (vector-set! fish 0 (string-append
					 (fish-name... fish)
					 appendage))))
	      (node-salmon.. node)))))
     (if (is-this-thing-one-of-these-types-of-things????
	       (node-feature.. node)
	       '("append down"))
	 (begin
	   (if (and make-sense?
		    (is-one-of-these-salmon-downstream????
		     (node-salmon.. node)))
	       (display "appending fish...\n"))
	   (let ((appendage ""))
	     (vector-set! (car node) 4
			  (filter
			   (lambda (fish)
			     (if (or (fish-upstream??? fish)
				     (vector-ref fish 4))
				 #t
				 (begin
				   (set! appendage (string-append
						    appendage
						    (fish-name... fish)))
				   #f)))
			   (node-salmon.. node)))
	     (map
	      (lambda (fish)
		(if (fish-downstream??? fish)
		    (vector-set! fish 0 (string-append
					 (fish-name... fish)
					 appendage))))
	      (node-salmon.. node))))))
     tree))


(define (node-feature.. node)
  (vector-ref (car node) 0))

(define (node-snowmelt?? node)
  (vector-ref (car node) 1))

(define (node-water?? node)
  (vector-ref (car node) 2))

(define (node-elec?? node)
  (vector-ref (car node) 3))
(define (node-powered?? node)
  (if (is-this-thing-one-of-these-types-of-things????
       (node-feature.. node)
       '("power invert"))
      (not (or (and (not (elec-blocked??? (cadr node)))
		    (node-powered?? (cadr node)))
	       (and (not (elec-blocked??? (caddr node)))
		    (node-powered?? (caddr node)))))
      (or (node-elec?? node)
	  (and (not (null? (cdr node)))
	       (or (and (not (elec-blocked??? (cadr node)))
			(node-powered?? (cadr node)))
		   (and (not (elec-blocked??? (caddr node)))
			(node-powered?? (caddr node))))))))

(define (node-salmon.. node)
  (vector-ref (car node) 4))


(define (create-snowmelt!!! node)
  (if (is-this-thing-one-of-these-types-of-things????
       (node-feature.. node)
       '("snowmelt"))
      (begin
	(if make-sense?
	    (display "melting snow...\n"))
	(vector-set! (car node) 1 #t))))
(define (snowmelt-blocked??? node)
  (or (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("blockage"))
	   (begin
	     (if make-sense?
		 (display "the snowmelt can't get around...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("force field" "evaporates"))
	   (node-powered?? node)
	   (begin
	     (if make-sense?
		 (display "the snowmelt cowers before the force field...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("lock"))
	   (node-powered?? node)
	   (begin
	     (if make-sense?
		 (display "the snowmelt cowers before the lock...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("inverse lock"))
	   (not (node-powered?? node))
	   (begin
	     (if make-sense?
		 (display "the snowmelt cowers before the lock...\n"))
	     #t))))
(define (break-stuff!!! node)
  (if (is-this-thing-one-of-these-types-of-things????
       (node-feature.. node)
       '("hydro power" "oblivion" "power invert"))
      (begin
	(if make-sense?
	    (format #t "smashing the ~a...\n" (node-feature.. node)))
	(vector-set! (car node) 0 "")))
  (if (is-this-thing-one-of-these-types-of-things????
       (node-feature.. node)
       '("bridge"))
      (begin
	(if make-sense?
	    (format #t "burning the ~a...\n" (node-feature.. node)))
	(vector-set! (car node) 0 "blockage"))))

(define (create-water!!! node)
  (if (is-this-thing-a-spring???? node)
      (begin
	'(if make-sense?
	    (display "seeping water...\n"))
	(vector-set! (car node) 2 #t))))
(define (water-blocked??? node)
  (and (is-this-thing-one-of-these-types-of-things????
	(node-feature.. node)
	'("force field" "evaporates"))
       (node-powered?? node)
       (begin
	 (if make-sense?
	     (display "the force field holds the water down...\n"))
	 #t)))

(define (create-elec!!! node)
  (if (or (is-this-thing-one-of-these-types-of-things????
	   (node-feature.. node)
	   '("powers"))
	  (and (is-this-thing-one-of-these-types-of-things????
		(node-feature.. node)
		'("hydro power"))
	       (node-water?? node)))
      (begin
	(if make-sense?
	    (display "generating electricity...\n"))
	(vector-set! (car node) 3 #t))))
(define (elec-blocked??? node)
  (or (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("blockage"))
	   (begin
	     (if make-sense?
		 (display "the electricity can't get around...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("force field"))
	   (node-powered?? node)
	   (begin
	     (if make-sense?
		 (display "the energy cowers before the force field...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("sense"))
	   (not (null? (node-salmon.. node)))
	   (is-one-of-these-salmon-mature????
	    (node-salmon.. node))
	   (begin
	     (if make-sense?
		 (display "electricity has been blocked...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("young sense"))
	   (not (null? (node-salmon.. node)))
	   (is-one-of-these-salmon-young????
	    (node-salmon.. node))
	   (begin
	     (if make-sense?
		 (display "electricity has been blocked...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("range sense"))
	   (mature-salmon-upstream???? node)
	   (begin
	     (if make-sense?
		 (display "electricity has been blocked...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("young range sense" "young range sense2"))
	   (young-salmon-upstream???? node)
	   (begin
	     (if make-sense?
		 (display "electricity has been blocked...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("upstream sense"))
	   (not (null? (node-salmon.. node)))
	   (is-one-of-these-salmon-mature????
	    (node-salmon.. node))
	   (is-one-of-these-salmon-upstream????
	    (node-salmon.. node))
	   (begin
	     (if make-sense?
		 (display "electricity has been blocked...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("downstream sense"))
	   (not (null? (node-salmon.. node)))
	   (is-one-of-these-salmon-mature????
	    (node-salmon.. node))
	   (is-one-of-these-salmon-downstream????
	    (node-salmon.. node))
	   (begin
	     (if make-sense?
		 (display "electricity has been blocked...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("switch"))
	   (or (null? (node-salmon.. node))
	       (not (is-one-of-these-salmon-mature????
		     (node-salmon.. node))))
	   (begin
	     (if make-sense?
		 (display "electricity has been blocked...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("young switch"))
	   (or (null? (node-salmon.. node))
	       (not (is-one-of-these-salmon-young????
		     (node-salmon.. node))))
	   (begin
	     (if make-sense?
		 (display "electricity has been blocked...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("range switch"))
	   (not (mature-salmon-upstream???? node))
	   (begin
	     (if make-sense?
		 (display "electricity has been blocked...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("young range switch"))
	   (not (young-salmon-upstream???? node))
	   (begin
	     (if make-sense?
		 (display "electricity has been blocked...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("insulated"))
	   (begin
	     (if make-sense?
		 (display "electricity has been insulated...\n"))
	     #t))
      (begin
	'(if make-sense?
	    (format #t "electricity makes it through ~a\n"
		    (node-feature.. node)))
	#f)))

(define (create-salmon!!! node)
  (if (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("hatchery"))
	   (node-powered?? node))
      (begin
	(if make-sense?
	    (display "hatching a fish...\n"))
	(vector-set! (car node) 4
		     (cons
		      (allocate-salmon!!!! "homeless" #t #t)
		      (node-salmon.. node))))))
(define (salmon-blocked??? node)
  (or (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("force field"))
	   (node-powered?? node)
	   (begin
	     (if make-sense?
		 (display "the salmon stop to admire the force field...\n"))
	     #t))))
(define (up-salmon-blocked??? node)
  (or (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("waterfall"))
	   (begin
	     (if make-sense?
		 (display "the salmon stops to admire the waterfall...\n"))
	     #t))))
(define (down-salmon-blocked??? node)
  (or (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("lock"))
	   (node-powered?? node)
	   (begin
	     (if make-sense?
		 (display "the salmon stop to admire the lock...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("inverse lock"))
	   (not (node-powered?? node))
	   (begin
	     (if make-sense?
		 (display "the salmon stop to admire the lock...\n"))
	     #t))))
(define (mature-salmon-blocked??? node)
  (or (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("net"))
	   (begin
	     (if make-sense?
		 (display "the salmon refuses to go into the net...\n"))
	     #t))))
(define (young-salmon-blocked??? node)
  (or (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("current"))
	   (begin
	     (if make-sense?
		 (display "the salmon refuses to go into the net...\n"))
	     #t))))
(define (salmon-very-blocked??? node)
  (or (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("blockage"))
	   (begin
	     (if make-sense?
		 (display "the salmon can't get around...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("fear"))
	   (node-powered?? node)
	   (begin
	     (if make-sense?
		 (display "the salmon freak out...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("pump"))
	   (not (node-powered?? node))
	   (begin
	     (if make-sense?
		 (display "the salmon needs the pump to be activated...\n"))
	     #t))
      (and (is-this-thing-one-of-these-types-of-things????
	    (node-feature.. node)
	    '("narrows"))
	   (not (null? (node-salmon.. node)))
	   (begin
	     (if make-sense?
		 (display "the salmon can't fit in...\n"))
	     #t))))
(define (fish-downstream??? fish)
  (not (vector-ref fish 1)))
(define (fish-upstream??? fish)
  (vector-ref fish 1))
(define (fish-move!!! fish node)
  (let ((next-move (find-move.... (fish-name... fish) node)))
    (if (or (null? (cdr node))
	    (and (not next-move)
		 (or (string=? (node-feature.. (cadr node)) "")
		     (and (or (salmon-very-blocked??? (cadr node))
			      (and (mature-salmon-blocked???
				    (cadr node))
				   (fish-mature??? fish))
			      (and (young-salmon-blocked???
				    (cadr node))
				   (fish-young??? fish))
			      (is-this-thing-one-of-these-types-of-things????
			       (node-feature.. node)
			       '("force up")))
			  (or (salmon-very-blocked??? (caddr node))
			      (and (mature-salmon-blocked???
				    (caddr node))
				   (fish-mature??? fish))
			      (and (young-salmon-blocked???
				    (caddr node))
				   (fish-young??? fish))
			      (is-this-thing-one-of-these-types-of-things????
			       (node-feature.. node)
			       '("force down"))))))
	    (and (is-this-thing-a-spring???? node)
		 (string=? (fish-name... fish)
			   (node-feature.. node))))
	(fish-spawn!!! fish node)
	(if next-move
	    (begin
	      (if make-sense?
		  (format #t
			  "the fish ~a has moved to ~a, closer to home...\n"
			  (fish-name... fish)
			  (node-feature.. next-move)))
	      (vector-set! (car node) 4
			   (delete
			    fish
			    (node-salmon.. node)))
	      (vector-set! (car next-move) 4
			   (cons
			    fish
			    (node-salmon.. next-move))))
	    (if (or (salmon-very-blocked??? (cadr node))
		    (is-this-thing-one-of-these-types-of-things????
		     (node-feature.. node)
		     '("force up")))
		(if (or (string=? (node-feature.. (caddr node)) "")
			(is-this-thing-one-of-these-types-of-things????
			 (node-feature.. node)
			 '("force down")))
		    (fish-spawn!!! fish node)
		    (begin
		      (if make-sense?
			  (format #t
				  "the fish ~a has moved to ~a, making do...\n"
				  (fish-name... fish)
				  (node-feature.. (caddr node))))
		      (vector-set! (car node) 4
				   (delete
				    fish
				    (node-salmon.. node)))
		      (vector-set! (caaddr node) 4
				   (cons
				    fish
				    (node-salmon.. (caddr node))))))
		(begin
		  (if make-sense?
		      (format #t
			   "the fish ~a has moved to ~a, a better place...\n"
			      (fish-name... fish)
			      (node-feature.. (cadr node))))
		  (vector-set! (car node) 4
			       (delete
				fish
				(node-salmon.. node)))
		  (vector-set! (caadr node) 4
			       (cons
				fish
				(node-salmon.. (cadr node))))))))))
(define (fish-spawn!!! fish node)
  (if make-sense?
      (format #t
	      "the fish ~a spawns at ~a...\n"
	      (fish-name... fish)
	      (node-feature.. node)))
  (vector-set! fish 1 #f)
  (vector-set! fish 2 #t)
  (vector-set! (car node) 4
	       (cons
		(allocate-salmon!!!! (node-feature.. node) #f #f)
		(node-salmon.. node))))
(define (fish-mature??? fish)
  (vector-ref fish 2))
(define (fish-young??? fish)
  (not (vector-ref fish 2)))
(define (fish-name... fish)
  (vector-ref fish 0))
(define (fish-held-up??? fish)
  (vector-ref fish 3))


(define (is-this-thing-one-of-these-types-of-things???? this-thing
							these-types-of-things)
  (find (lambda (one-of-these-types-of-things)
	  (string-ci=? this-thing one-of-these-types-of-things))
	these-types-of-things))

(define (is-one-of-these-salmon-mature???? these-salmon)
  (find (lambda (this-fish)
	  (fish-mature??? this-fish))
	these-salmon))

(define (is-one-of-these-salmon-young???? these-salmon)
  (find (lambda (this-fish)
	  (fish-young??? this-fish))
	these-salmon))

(define (is-one-of-these-salmon-upstream???? these-salmon)
  (find (lambda (this-fish)
	  (fish-upstream??? this-fish))
	these-salmon))

(define (is-one-of-these-salmon-downstream???? these-salmon)
  (find (lambda (this-fish)
	  (fish-downstream??? this-fish))
	these-salmon))

(define (is-one-of-these-salmon-up???? these-salmon)
  (find (lambda (this-fish)
	  (vector-ref this-fish 4))
	these-salmon))

(define (is-one-of-these-salmon-down???? these-salmon)
  (find (lambda (this-fish)
	  (not (vector-ref this-fish 4)))
	these-salmon))

(define (is-this-thing-a-spring???? this-thing)
  (not (is-this-thing-one-of-these-types-of-things????
	(node-feature.. this-thing)
	'("hatchery"
	  "hydro power"
	  "snowmelt"
	  "shallows"
	  "rapids"
	  "append down"
	  "bear"
	  "force field"
	  "sense"
	  "clone"
	  "young bear"
	  "bird"
	  "upstream killing device"
	  "waterfall"
	  "universe"
	  "powers"
	  "marshy"
	  "insulated"
	  "upstream sense"
	  "downstream sense"
	  "evaporates"
	  "youth fountain"
	  "oblivion"
	  "pump"
	  "range sense"
	  "fear"
	  "reverse up"
	  "reverse down"
	  "time"
	  "lock"
	  "inverse lock"
	  "young sense"
	  "switch"
	  "young switch"
	  "narrows"
	  "append up"
	  "young range sense"
	  "net"
	  "force down"
	  "force up"
	  "spawn"
	  "power invert"
	  "current"
	  "bridge"
	  "split"
	  "range switch"
	  "young range switch"
	  ""))))


(define (mature-salmon-upstream???? node)
  (or (and (not (null? (node-salmon.. node)))
	   (is-one-of-these-salmon-mature????
	    (node-salmon.. node)))
      (and (not (null? (cdr node)))
	   (or (mature-salmon-upstream???? (cadr node))
	       (mature-salmon-upstream???? (caddr node))))))

(define (young-salmon-upstream???? node)
  (or (and (not (null? (node-salmon.. node)))
	   (is-one-of-these-salmon-young????
	    (node-salmon.. node)))
      (and (not (null? (cdr node)))
	   (or (young-salmon-upstream???? (cadr node))
	       (young-salmon-upstream???? (caddr node))))))

(define (allocate-salmon!!!! name upstream? mature?)
  (vector name upstream? mature? #f #f (gensym)))

(define (find-move.... name node)
  (if (or (salmon-blocked??? node)
	  (up-salmon-blocked??? node))
      #f
      (if (and (is-this-thing-a-spring???? node)
	       (string=? name (node-feature.. node)))
	  #t
	  (cond
	   ((or (null? (cdr node))
		(salmon-very-blocked??? node))
	    #f)
	   ((and (not (is-this-thing-one-of-these-types-of-things????
		       (node-feature.. node)
		       '("force up")))
		 (find-move.... name (cadr node)))
	    (cadr node))
	   ((and (not (is-this-thing-one-of-these-types-of-things????
		       (node-feature.. node)
		       '("force down")))
		 (find-move.... name (caddr node)))
	    (caddr node))
	   (#t #f)))))
