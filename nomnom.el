
(setq *nom/tokens*
      (concat
       "\\(\\(class\\|enum\\|interface\\)\s+[^\s<]+" ;; class/enum/interface def
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

(defun nom/tokenize-buffer ()
  (save-excursion
    (let ((*nom/doc* 
	   (let ((s (buffer-string)))
	     (set-text-properties 0 (length s) nil s)
	     s)))
      (remove-if-not #'identity 
		     (loop for start = 0 then end
			   for pos = (string-match *nom/tokens* *nom/doc* start)
			   while pos
			   for end = (match-end 0)
			   append (nom/split-tokens (match-string 0 *nom/doc*) (match-beginning 0)))))))

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

(defun nom/parse-java-class (tokens)
  (let ((cls (nom/expect-token "class" tokens)))
    (when cls
      (let* ((c1 (nom/expect-token "{" cls))
	     (c2 (nom/expect-curly-close (rest c1))))
	(print c2)
	(list 'class (caadr cls)
	      (cadar c1) (caddar c2))))))

;; (defun count-braces (tokens)
;;   (format "open: %d, closed: %d" 
;; 	  (length (remove-if-not (lambda (s) (string-equal "{" (car s))) tokens))
;; 	  (length (remove-if-not (lambda (s) (string-equal "}" (car s))) tokens))))
