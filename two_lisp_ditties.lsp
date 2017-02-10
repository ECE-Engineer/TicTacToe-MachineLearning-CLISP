(defmethod select (lst)
	(if	(null lst) 
		nil
	)
	(cond 
		((singleton lst)
			(car lst)
		)
		(t
			(nth (random ( - (length lst) 1 ) ) lst)
		)
	)
)

(defmethod snoc (sym lst)
	(cond
		((null lst)
			(list sym)
		)
		(t
			(cons (car lst) (snoc sym (cdr lst)))
		)
	)
)

(defmethod singleton(lst)
	(eq (length lst) 1)
)