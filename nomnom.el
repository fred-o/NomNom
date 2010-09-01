;;
;; nomnom.el
;;
;; A bare-bones parser for java source files.
;;
;; Author: fredrik.appelberg@gmail.com
;; Licence: Public Domain
;; Version: 0.1
;;

(setq *nom/tokens*
      (concat
       "//.*$" ;; single-line comment
       "\\|/\\*[^]]*\\*/" ;; multiline comment
       "\\|\\(\\(class\\|enum\\|interface\\)\s+[^{}\s<]+" ;; class/enum/interface def
       "\\|\\(for\\|while\\)"
       "\\|\\(new\\)?\s[^\s\.]+?(.*?)[^(;{]*{" ;; method def/anonymous class creation
       "\\|\{" ;; start curly
       "\\|\}\\)")) ;; end curly

(defun nom/split-tokens (s idx)
  "Split composite tokens into subtokens and calculate correct position for each."
  (remove-if (lambda (token) (or (= 0 (length (car token)))
				 (string-match "[\s,]" (car token))))
	     (loop for start = 0 then (1+ pos)
		   for pos = (string-match "\\([()\s,]\\)" s start)
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

(defun nom/expect-curly-open (tokens)
  (cond ((null tokens) nil)
	((string-equal "}" (caar tokens)) nil)
	((string-equal "{" (caar tokens)) tokens)
	(t (nom/expect-curly-open (rest tokens)))))

(defun nom/expect-curly-close (tokens)
  (cond ((null tokens) nil)
	((string-equal "{" (caar tokens)) 
	 (nom/expect-curly-close (rest (nom/expect-curly-close (rest tokens)))))
	((string-equal "}" (caar tokens)) tokens)
	(t (nom/expect-curly-close (rest tokens)))))

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

(defun nom/expect-class-equivalent (tokens)
  (cond ((null tokens) nil)
	((or (string-equal "class" (caar tokens))
	     (string-equal "interface" (caar tokens))
	     (string-equal "enum" (caar tokens)))
	 (let ((cb (nom/expect-class-body (rest tokens))))
	   (cons
	    (append 
	     (list (intern (caar tokens)) (caadr tokens))
	     (car cb))
	    (rest cb))))))

(defun nom/expect-class-equivalents (tokens)
  (when tokens
    (let* ((cls (nom/expect-class-equivalent tokens))
	   (nxt (nom/expect-class-equivalents (rest cls))))
      (cons (when cls
	      (cons (car cls)
		    (car nxt)))
	    (rest nxt)))))

(defun nom/parse-buffer ()
  "Parse the current java buffer."
  (car (nom/expect-class-equivalents (nom/tokenize-buffer))))
