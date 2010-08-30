
(setq *nom/tokens*
      (concat
       "\\(\\(class\\|enum\\|interface\\)\s+[^{}\s<]+" ;; class/enum/interface def
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
			    for end = (match-end 0)
			    append (nom/split-tokens (match-string 0 s) (match-beginning 0)))))
  
(defun nom/tokenize-buffer ()
  "Tokenize the current java buffer."
  (let ((s (buffer-string)))
    (set-text-properties 0 (length s) nil s)
    (nom/tokenize-string s)))

(defun nom/expect-token (token tokens)
  (cond ((null tokens) nil)
	((string-equal token (caar tokens)) tokens)
	(t (nom/expect-token token (cdr tokens)))))

(defun nom/expect-curly-close (tokens)
  (cond ((null tokens) nil)
	((string-equal "{" (caar tokens)) 
	 (nom/expect-curly-close (rest (nom/expect-curly-close (rest tokens)))))
	((string-equal "}" (caar tokens)) tokens)
	(t (nom/expect-curly-close (rest tokens)))))

(defun nom/expect-class-equivalent (tokens)
  (cond ((null tokens) nil)
	((or (string-equal "class" (caar tokens))
	     (string-equal "interface" (caar tokens))
	     (string-equal "enum" (caar tokens)))
	 (let* ((c1 (nom/expect-token "{" tokens))
		(c2 (nom/expect-curly-close (rest c1))))
	   (cons
	    (list 'class (caadr tokens)
		  (cadar c1) (caddar c2))
	    (nom/expect-class-equivalent (rest c2)))))
	(t (nom/expect-class-equivalent (rest tokens)))))

(defun nom/parse-buffer ()
  "Parse the current java buffer."
  (nom/expect-class-equivalent (nom/tokenize-buffer)))

;; (defun count-braces (tokens)
;;   (format "open: %d, closed: %d" 
;; 	  (length (remove-if-not (lambda (s) (string-equal "{" (car s))) tokens))
;; 	  (length (remove-if-not (lambda (s) (string-equal "}" (car s))) tokens))))
