




(setq *nom/tokens*
      (concat
       "\\(\\(class\\|enum\\|interface\\)\s+[^\s<]+" ;; class/enum/interface def
       "\\|\\(for\\|while\\)"
       "\\|\\(new\\)?\s[^\s\.]+?(.*?)[^(;{]*{" ;; method def/anonymous class creation
       "\\|\{" ;; start curly
       "\\|\}\\)")) ;; end curly

(defun nom/split-tokens (str)
  (mapcan (lambda (s)
	    (loop for start = 0 then (1+ pos)
		  for pos = (string-match "\\([()]\\)" s start)
		  while pos
		  append (list (substring s start pos) (match-string 0 s)) into tokens
		  finally return (append tokens (list (substring s start)))))
	  (split-string str "[\s,]" t)))

(defun nom/tokenize-buffer ()
  (save-excursion
    (let ((*nom/doc* 
	   (let ((s (buffer-string)))
	     (set-text-properties 0 (length s) nil s)
	     s)))
      (remove-if-not #'identity
		     (loop for pos = 0 then (string-match *nom/tokens* *nom/doc* (1+ pos))
			   while pos
			   append (nom/split-tokens (match-string 0 *nom/doc*)))))))
