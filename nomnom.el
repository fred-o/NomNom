;;
;; nomnom.el
;;
;; A bare-bones parser for picking apart Java source files and
;; analyzing the structure of class, interface and enum definitions.
;;
;; The most interesting functions are at the bottom: NOM/PARSE-BUFFER
;; and NOM/CLASS-AT-POINT.
;;
;; Author: fredrik.appelberg@gmail.com
;; Licence: Public Domain
;; Version: 0.2
;; Updated: 2010-09-01
;;

(setq *nom/tokens*
      (concat
       "//.*$" ;; single-line comment
       "\\|/\\*[^]]*?\\*/" ;; multiline comment
       "\\|\"\\([^\"]\\|\\\"\\)*?\"" ;; strings (that may contain escaped "'s)
       "\\|\\(\\(class\\|enum\\|interface\\)\s+[^{}]+" ;; class/enum/interface def    
       "\\|\\(for\\|while\\)"
       "\\|\\(new\\)?\s[^\s\.]+?(.*?)[^(;{]*{" ;; method def/anonymous class creation
       "\\|\{" ;; start curly
       "\\|\}\\)")) ;; end curly

(defun nom/split-tokens (s idx)
  "Split composite tokens into subtokens and calculate correct position for each."
  (remove-if (lambda (token) (or (= 0 (length (car token)))
				 (string-match "[\s,]" (car token))))
	     (loop for start = 0 then (1+ pos)
		   for pos = (string-match "\\([()<>\s\n,]\\)" s start)
		   while pos
		   append (list 
			   (list (substring s start pos) 
				 (+ idx start) (+ idx pos))
			   (list (match-string 0 s)
				 (match-beginning 0) (match-end 0))) 
		   into tokens
		   finally return (append tokens (list 
						  (list (substring s start)
							(+ idx start) (+ idx (length s))))))))

(defun nom/tokenize-string (s)
  "Split the string S into tokens."
  (remove-if-not #'identity 
		 (loop for start = 0 then end
		       for pos = (string-match *nom/tokens* s start)
		       while pos
		       for beg = (match-beginning 0)
		       for end = (match-end 0)
		       for m = (match-string 0 s)
		       unless (string-match "^\\(//\\|/\\*\\)" m)
		       append (nom/split-tokens m beg))))

(defun nom/tokenize-buffer ()
  "Tokenize the current java buffer."
  (let ((s (buffer-string)))
    (set-text-properties 0 (length s) nil s)
    (nom/tokenize-string s)))

(defun nom/expect-pair-open (tokens open closed)
  (cond ((null tokens) nil)
	((string-equal closed (caar tokens)) nil)
	((string-equal open (caar tokens)) tokens)
	(t (nom/expect-pair-open (rest tokens) open closed))))

(defun nom/expect-pair-close (tokens open closed)
  (cond ((null tokens) nil)
	((string-equal open (caar tokens)) 
	 (nom/expect-pair-close (rest (nom/expect-pair-close (rest tokens) open closed)) open closed))
	((string-equal closed (caar tokens)) tokens)
	(t (nom/expect-pair-close (rest tokens) open closed))))

(defun nom/expect-curly-open (tokens)
  (nom/expect-pair-open tokens "{" "}"))

(defun nom/expect-curly-close (tokens)
  (nom/expect-pair-close tokens "{" "}"))

(defun nom/expect-bracket-open (tokens)
  (nom/expect-pair-open tokens "<" ">"))

(defun nom/expect-bracket-close (tokens)
  (nom/expect-pair-close tokens "<" ">"))

(defun nom/skip-bracket-pair (tokens)
  (if (string-equal "<" (caar tokens))
      (rest (nom/expect-bracket-close (rest tokens)))
    tokens))

(defun nom/expect-class-body (tokens)
  (let* ((co (nom/expect-curly-open tokens))
	 (cls (nom/expect-class-equivalents (rest co)))
	 (cc (nom/expect-curly-close (or (rest cls)
					 (rest co)))))
    (when cc
      (cons
       (remove-if-not #'identity
		      (list
		       (list (cadar co) (caddar cc))
		       (car cls)))
       (rest cc)))))

(defun nom/expect-extends (tokens)
  (cond ((null tokens) nil)
	((string-equal "extends" (caar tokens))
	 (let ((cls (cadr tokens)))
	   (cons cls
		 (nom/skip-bracket-pair (cddr tokens)))))
	(t (cons nil tokens))))

(defun nom/expect-interface (tokens)
  (cond ((string-equal "{" (caar tokens)) (cons nil tokens))
	(t (cons (caar tokens)
		 (nom/expect-bracket-pair (cdr tokens))))))

(defun nom/expect-implements (tokens)
  (cond ((null tokens) nil)
	((string-equal "implements" (caar tokens))
	 (loop for tok = (cdr tokens) then (cdr iface)
	       for iface = (nom/expect-interface tok)
	       while (car iface)
	       collect (car iface) into ifaces
	       finally return (cons ifaces tok)))
	(t (cons nil tokens))))

(defun nom/expect-class-equivalent (tokens)
  (cond ((null tokens) nil)
	((or (string-equal "class" (caar tokens))
	     (string-equal "interface" (caar tokens))
	     (string-equal "enum" (caar tokens)))
	 (let* ((ex (nom/expect-extends 
		     (nom/skip-bracket-pair (cddr tokens))))
		(im (nom/expect-implements (rest ex)))
		(cb (nom/expect-class-body (rest im))))
	   (cons
	    (remove-if-not #'identity
			   (list 
			    (intern (concat ":" (caar tokens))) (caadr tokens)
			    (cons :bounds (caar cb))
			    (when (car ex) (list :extends (caar ex)))
			    (when (car im) (cons :implements (car im)))
			    (when (cdar cb) (list :inner (cadar cb)))))
	    (rest cb))))
	(t (nom/expect-class-equivalent (rest tokens)))))

(defun nom/expect-class-equivalents (tokens)
  (when tokens
    (let* ((cls (nom/expect-class-equivalent tokens))
	   (nxt (nom/expect-class-equivalents (rest cls))))
      (cons (when cls
	      (cons (car cls)
		    (car nxt)))
	    (rest nxt)))))

(defun nom/parse-buffer ()
  "Parses the current Java buffer and returns a parse tree
containing an outline of its class, interface and enum
definitions."
  (car (nom/expect-class-equivalents (nom/tokenize-buffer))))

(defun nom/parse-file (file-name)
  "Parses the contents of file FILE-NAME and returns a parse
tree containing an outline of its class, interface and enum
definitions."
  (with-temp-buffer
    (insert-file file-name)
    (nom/parse-buffer)))


(defun nom/class-at-char (tree pos)
  "Returns a hierarchical list of the class definitions
surrounding the character POS in the given parse TREE."
  (when tree
    (let ((cls (first tree)))
      (if (and (>= pos (car (third cls))) (< pos (cadr (third cls))))
	  (cons (second cls)
		(nom/class-at-char (fourth cls) pos))
	(nom/class-at-char (rest tree) pos)))))

(defun nom/class-at-point ()
  "Returns a hierarchical list of the class definitions
surrounding (point)."
  (nom/class-at-char (nom/parse-buffer) (1- (point))))

(provide 'nomnom)