(defun tree-every (tree fn)
  (cond ((atom tree)
	 (funcall fn tree))
	(t (every (lambda(el) (tree-every el fn)) tree))))

(defun tree-replace (tree replacement-fn)
  (cond ((atom tree)
	 (funcall replacement-fn tree))
	(t (mapcar (lambda(el) (tree-replace el replacement-fn)) tree))))
  
(defun read-and-process-axioms (bfo2 path)
  (with-open-file (f path )
    (loop with axs
       for form = (read f nil :eof) 
       until (eq form :eof)
       do (cond ((eq (car form) 'object-property)
		 (setq axs (append axs (translate-object-property-axioms bfo2 form))))
		((consp (car form))
		 (setq axs (append (generate-from-lispy-axiom bfo2 form) axs))))
       finally (return axs))))

(defun translate-object-property-axioms (bfo2 form)
  (with-bfo-uris bfo2
    (if (consp (car form)) ; a regular functional syntax axiom
	(generate-from-lispy-axiom bfo2 form)
	(let* ((arity (second form))
	       (prop> (third form))
	       (prop< (when (consp prop>) (prog1 (second prop>) (setq prop> (first prop>)))))
	       (inverses? (second (assoc 'inverses (cdddr form))))
	       (axs nil))
	  ;;; get the potential time-dependent relations. Many of the relations are expressed 
	  (let* ((uris (cdr (bfo-uris bfo2)))
		 (at> (third (assoc (at-term prop>) uris)))
		 (st> (third (assoc (st-term prop>) uris)))
		 (at< (third (assoc (at-term prop<) uris)))
		 (st< (third (assoc (st-term prop<) uris))))
	    (when (eq arity :temporal)
	      (setq axs (append axs (maybe-transitive-at-a-time bfo2 form st> at>)))
	      (setq axs (append axs (maybe-transitive-at-a-time bfo2 form st< at<)))
	      )
	    ;; now we decide which properties we're actually going to assert on.
	    (loop for expression in (cdddr form)
	       do (if (or at> st> at< st<) ;; If we've actually got some temporalized properties
		      (progn
			(when (or st> st<) ;; if we've got at least one -at-some-time property then call the generators using those
			  (setq axs (append (process-one-object-property-expression bfo2 form expression st> st< nil 's nil nil) axs)))

			;; this is wrong. We need instead to know which axioms inherit downward,
			;; and for those, suppress at-all-times assertions when there is the
			;; at-some-times property.
			(when (or (and at> (not st>)) (and at< (not st<) ))
			  (setq axs (append (process-one-object-property-expression bfo2 form expression at> at< inverses? 'a nil nil) axs)))

			)
		      (setq axs (append (process-one-object-property-expression bfo2 form expression (eval prop>) (eval prop<) inverses? nil nil nil) axs)))
	       finally (return axs)))))))

(defun process-one-object-property-expression (bfo2 form expression prop> prop< inverses? a-or-s reversed =def &aux axs keys)
  (with-bfo-uris bfo2
    (unless (eq (first expression) 'temporal)
      (if (eq (first expression) '=)
	  (progn
;	    (print-db form (second expression) prop< prop> inverses? a-or-s t t)
	    (setq axs (append (process-one-object-property-expression bfo2 form (second expression) prop> prop< inverses? a-or-s nil t)
			   axs)))
	 (if (eq (first expression) '<)
	     (loop for expr in (rest expression)
		do (setq axs (append (process-one-object-property-expression bfo2 form expr prop< prop> inverses? a-or-s t =def)
				     axs)))
	     (if (member (first expression) '(a s))
		 (when (eq a-or-s (first expression))
		   (loop for expr in (rest expression)
		      do (setq axs (append (process-one-object-property-expression bfo2 form expr prop> prop< inverses? a-or-s reversed =def)
					   axs))))
		 (cond ((member :cant expression) t)
		       ((member (second expression) '(<- -> <-> -+> +>))
			(setq axs (append 
				   (process-arrow-expression bfo2 form expression prop> prop< inverses? a-or-s reversed =def)
				   axs)))
		       ((eq (first expression) 'o)
			(let ((chain (loop for (class . rest) on (cdr expression) while
					  (not (keywordp class)) 
					  collect (eval class) into chain
					  finally (setq keys (cons class rest)) (return chain))))
			  (push `(sub-object-property-of ,@(maybe-object-property-annotations keys) (object-property-chain ,@chain)
							 ,prop>) axs)))
		       ((eq (first expression) 'domain-narrowed)
			(setq axs (append 
				   (process-domain-narrowed-expression bfo2 form expression prop> prop< inverses? a-or-s reversed =def))))
		       (t 
			(setq axs (append
				   (process-property-property-expression bfo2 form expression prop> prop< inverses? a-or-s reversed )
				   axs)))
		       ))))))
  axs)

(defun process-arrow-expression (bfo2 form expression prop> prop< inverses? a-or-s reversed =def &aux axs)
  (destructuring-bind (from operator to &rest keys) expression
    (let ((from (class-expressionize from))
	  (to (class-expressionize to)))
					;			 (print-db from to (second expression) prop> prop< a-or-s)
      (if (eq to 'self)
	  (progn
	    (assert (eq operator '->) () "-> self supported, else not: ~a" expression)
	    (push `(sub-class-of ,@(maybe-object-property-annotations keys) ,from (object-has-self ,prop>)) axs))
	  (cond ((eq operator '->)
		 (push `(,(if =def 'equivalent-classes 'sub-class-of) ,@(maybe-object-property-annotations keys) ,from (object-all-values-from ,prop> ,to)) axs))
		((eq operator '+>)
		 (and prop>
		      (push `(,(if =def 'equivalent-classes 'sub-class-of) ,@(maybe-object-property-annotations keys) ,from (object-some-values-from ,prop> ,to)) axs)))
		((eq operator '-+>)
		 (setq axs
		       (append
			(process-one-object-property-expression bfo2 form `(,from -> ,to ,@keys) prop> prop< inverses? a-or-s reversed =def)
			(process-one-object-property-expression bfo2 form `(,from +> ,to ,@keys) prop> prop< inverses? a-or-s reversed =def)
			axs)))
		((eq operator '<-)
		 (push `(,(if =def 'equivalent-classes 'sub-class-of) ,@(maybe-object-property-annotations keys) ,to (object-all-values-from ,prop< ,from)) axs))
		((eq operator '<->)
		 (warn "=def not processed on <-> operator ~a in ~a" operator expression)
		 (push `(sub-class-of ,@(maybe-object-property-annotations keys) ,from (object-all-values-from ,prop> ,to)) axs)
		 (push `(sub-class-of ,@(maybe-object-property-annotations keys) ,to (object-all-values-from ,prop< ,from)) axs))
		))))

  axs)


(defun process-domain-narrowed-expression (bfo2 form expression prop> prop< inverses? a-or-s =def reversed &aux axs)

  "A domain narrowed rule on an object property r1, expressed
   (domain-narrowed (r2 inv-r2)) says that
   if x r2 y and x a C1, then x r1 y, where r1 subproperty r2.
   We translate this first into a GCI. (C1 and r2 some (range r1) equivalentTo (r1 some (range r1))). 

   e.g. q inheres-in c -> q quality-of c 
   => (equivalent-classes (object-intersection-of q (object-some-values-from inheres-in c))
                          (object-some-values-from quality-of c))

   We can also add a dl-safe rule
   (rule (!continuant !?x) (!inheres-in !?x !?y) -> (!quality-of !?x !?y))
"
  (let ((range (class-expressionize (or (second (find-if (lambda(e) (and (consp e) (eq (car e) 'range))) form)) !owl:Thing)))
	(domain (class-expressionize (or (second (find-if (lambda(e) (and (consp e) (eq (car e) 'domain))) form)) !owl:Thing)))
	(keys (cddr expression)))
    (let* ((general-rel (first (second expression)))
	   (general-rel-t (if (eq a-or-s 'a) (at-term general-rel) (if (eq a-or-s 's) (st-term general-rel)))))
      (with-bfo-uris bfo2
	(and range
	     ;; inherits downward
	     (if (and a-or-s (boundp general-rel-t) prop>)
		 (push `(equivalent-classes ,@(maybe-object-property-annotations keys)
					    (object-intersection-of ,domain (object-some-values-from ,(eval general-rel-t) ,range))
					    (object-some-values-from ,prop> ,range))
		       axs))))))
  axs)

(defun process-range-narrowed-expression (bfo2 form expression prop> prop< inverses? a-or-s =def reversed &aux axs)
  (let ((domain (second (find-if (lambda(e) (and (consp e) (eq (car e) 'domain))) form)))
	(range (second (find-if (lambda(e) (and (consp e) (eq (car e) 'range))) form)))
	(keys (cddr expression)))
    (let* ((general-rel (second (second expression)))
	   (general-rel-t (if (eq a-or-s 'a) (at-term general-rel) (if (eq a-or-s 's) (st-term general-rel)))))

;      (print-db a-or-s prop> prop< domain general-rel general-rel-t reversed)
      (and domain
	   (let ((domain-expression (class-expressionize domain)))
	     ;; inherits downward
	     (if (and a-or-s (boundp general-rel-t) prop> )
		 (push `(equivalent-classes ,@(maybe-object-property-annotations keys) (object-some-values-from ,(eval general-rel-t) ,domain-expression)
					    (object-some-values-from ,prop> ,domain-expression)) axs)
		 '(when (and (not a-or-s) (boundp general-rel) (if reversed prop< prop>))
		   (push `(equivalent-classes ,@(maybe-object-property-annotations keys) (object-some-values-from ,(eval general-rel) ,domain-expression)
			   (object-some-values-from ,(if reversed prop< prop>) ,domain-expression)) axs)))))))
;  (print-db axs)
  axs)

(defun process-property-property-expression (bfo2 form expression prop> prop< inverses? a-or-s reversed &aux axs)
  (let ((feature (first expression))
	(classex (class-expressionize (second expression))))
    (cond ((eq feature 'domain) ; if domain assert domain on > and range on < , inherits downward
	   (and prop> (push `(object-property-domain ,@(maybe-object-property-annotations (cddr expression)) ,prop> ,classex) axs))
	   (and prop< (push `(object-property-range ,@(maybe-object-property-annotations (cddr expression)) ,prop< ,classex) axs)))

	  ((eq feature 'range) ; if range assert range on > and domain on < , inherits downward
	   (and prop> (push `(object-property-range ,@(maybe-object-property-annotations (cddr expression)) ,prop> ,classex) axs))
	   (and prop< (push `(object-property-domain ,@(maybe-object-property-annotations (cddr expression)) ,prop< ,classex) axs)))

	  ((or (eq feature 'transitive) (and (eq a-or-s 'a) (eq feature 'transitive-at-a-time))) ; if transitive assert on both, doesn't inherit
	   (and prop> (push `(transitive-object-property ,@(maybe-object-property-annotations (cdr expression)) ,prop>) axs))
	   (and prop< (push `(transitive-object-property ,@(maybe-object-property-annotations (cdr expression)) ,prop<) axs)))

	  ((eq feature 'functional) ; if functional assert functional on > and inverse-functional on <, doesn't inherit
	   (and prop> (push `(functional-object-property ,@(maybe-object-property-annotations (cdr expression)) ,prop>) axs))
	   (and prop< (push `(inverse-functional-object-property ,@(maybe-object-property-annotations (cdr expression)) ,prop<) axs)))

	  ((eq feature 'inverse-functional) ; if inverse-functional assert functional on > and inverse-functional on <, doesn't inherit
	   (and prop> (push `(inverse-functional-object-property ,@(maybe-object-property-annotations (cdr expression)) ,prop>) axs))
	   (and prop< (push `(functional-object-property ,@(maybe-object-property-annotations (cdr expression)) ,prop<) axs)))

	  ((eq feature 'symmetric) ; symmetric assert symmetric on > and <, doesn't inherit
	   (and prop> (push `(symmetric-object-property ,@(maybe-object-property-annotations (cdr expression)) ,prop>) axs))
	   (and prop< (push `(symmetric-object-property ,@(maybe-object-property-annotations (cdr expression)) ,prop< ) axs)))

	  ((and (eq feature 'irreflexive) (not classex)) ; if irreflexive assert irreflexive on > and  <, doesn't inherit
	   (and prop> (push `(irreflexive-object-property ,@(maybe-object-property-annotations (cdr expression)) ,prop>) axs))
	   (and prop< (push `(irreflexive-object-property ,@(maybe-object-property-annotations (cdr expression)) ,prop< ) axs)))
	  ))
  axs)

(defun maybe-object-property-annotations (keys)
  (when (getf keys :id)
    `((annotation !axiomid ,(make-uri nil (format nil "obo:bfo/axiom/~a" (getf keys :id)))))))

(defun class-expressionize (form)
  (cond ((eq form 'thing) !owl:Thing)
	((eq form 'nothing) !owl:Nothing)
	((eq form 'self) 'self)
	((atom form) (eval form))
	((eq (car form) 'or)
	 `(object-union-of ,@(mapcar 'class-expressionize (cdr form))))
	((eq (car form) 'and)
	 `(object-intersection-of ,@(mapcar 'class-expressionize (cdr form))))
	((eq (car form) 'some)
	 `(object-some-values-from ,(class-expressionize (second form)) ,(class-expressionize (third form))))
	((eq (car form) 'all)
	 `(object-all-values-from ,(class-expressionize (second form)) ,(class-expressionize (eval (third form)))))
	((eq (car form) 'not)
	 `(object-complement-of ,(class-expressionize (second form))))
	(t (error "don't understand expression ~a" form))))
  
; for debugging
(defmacro object-property ( &whole form &rest all)
  (with-bfo-uris (read-bfo2-reference-spec)
    (eval-uri-reader-macro (translate-object-property-axioms b form))))


(defun generate-from-lispy-axiom (bfo2 form)
  (destructuring-bind (axiom . plist) form
    (let ((axiom-substituted (eval-bfo-uris axiom bfo2))
	  (id (getf plist :id)))
      (assert id (form) "Hey! you didn't assign an axiom id here: ~a" form)
      `((,(car axiom-substituted)
	 (annotation ,!axiomid ,(make-uri nil (format nil "obo:bfo/axiom/~7,'0d" id)))
	 ,@(if (getf plist :seealso)
	       (list (list 'annotation !rdfs:seeAlso (getf plist :seealso))))
	 ,@(if (getf plist :note)
	       (list (list 'annotation !rdfs:comment (getf plist :note))))
	 ,@(cdr axiom-substituted))))))

(defun maybe-transitive-at-a-time-chain (bfo2 form stprop atprop)
  (when (and (find 'transitive-at-a-time form :key (lambda(el) (and (consp el) (car el))))
	     (and stprop atprop))
    `((sub-object-property-of (object-property-chain ,(eval stprop) ,(eval atprop)) ,(eval stprop)))))

(defun maybe-transitive-at-a-time (bfo2 form stprop atprop)
  (when (and (find 'transitive-at-a-time form :key (lambda(el) (and (consp el) (car el))))
	     (and stprop atprop))
    `((transitive-object-property ,(eval atprop)))))

;(defun maybe-locally-reflexive (bfo2 form prop stprop atprop)
;  (let ((sprop 

(defun gather-extra-gcis (bfo2)
  (with-obo-metadata-uris
    (with-bfo-uris bfo2
      (with-open-file (f "bfo:src;ontology;owl-group;specification;gcis-with-no-other-home.lisp")
	(loop for entry = (eval-uri-reader-macro (read f nil :eof))
	   until (eq entry :eof)
	   append (generate-from-lispy-axiom bfo2 entry))))))
