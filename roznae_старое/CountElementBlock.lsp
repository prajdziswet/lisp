(defun c:CountElementBlock (/ nabor_blocks list_block len i spis spis1 model model1 proverca spis2 true_explode count nameblock fist elem)
  (vl-load-com)
  (setq	nabor_blocks
	 (ssget	"_X"
		(list (cons 0 "INSERT")
		      (cons 100 "AcDbBlockReference")
		)
	 )
  )					;block

  (setq model (getvar "ctab"));model -list
  (if (/= nil nabor_blocks)
    (progn
      (setq i	-1
	    len	(sslength nabor_blocks)
      )
      (repeat len
	(setq i (1+ i))			; Выбор следующего примитива и получение его списка
	(princ (itoa i))
	(princ "\n")
	(setq list_block (ssname nabor_blocks i))

					;block
    (setq proverca T
	  spis1	spis
    )
    (setq nameblock (vla-get-effectivename
		      (setq obj (vlax-ename->vla-object list_block))
		    )
    )
					;пошук што незмяшчае элемент в спісе
    (while (and proverca (/= spis1 nil))
      (if (= nameblock (car (car spis1)))
	(setq proverca nil)
	 (setq spis1 (cdr spis1))
      )
    )					;end while

					;взрыв і востановленіе	    
    (if	(or (= spis nil) (= proverca T))
      (if (vlax-method-applicable-p obj 'Explode)
	(progn
	  (setq model1 (cdr (assoc 410 (entget list_block))))
	  (if (/= (getvar "ctab") model1)
	    (setvar "ctab" model1)
	    )
	  (command "_.Explode" list_block)
	  (setq count (sslength (ssget "_p")))

	  (if (= spis nil)
	    (setq spis (list (list nameblock count)))
	    (setq spis (append spis (list (list nameblock count))))
	  )
	  (command "_u")
	)
      )					;if
    )
					; end if взрыв




	

	);end repeat
(setvar "ctab" model)      
      (if (/= nil spis)
	(progn
(alert (strcat "прошли все блоки, количество =" (itoa (length spis)) "\nсортировка,ух..."))
      ;спісок утварыуся
      ;сортіруем по колькасці
(setq spis1 nil)
      (while (/= nil spis)
	(setq fist (car spis))
	(setq spis (cdr spis))
	(setq spis2 spis)
	(setq spis nil)
	;while0
	(while (/= spis2 nil)
	(setq elem (car spis2))
	(setq spis2 (cdr spis2))
	  (if (< (last fist) (last elem))
	      (progn
	       (if (= nil spis)
		(setq spis (list fist))
		(setq spis (append (list fist) spis))
		);id if
	       (setq fist elem)
		);end progn
	     (if (= nil spis)
		(setq spis (list elem))
		(setq spis (append (list elem) spis))
		);id if
	   );id if
	  
	 );while0
	(if (= nil spis1)
	(setq spis1 (list fist))
	  (setq spis1 (append spis1 (list fist)))
	  )
	(if (= 1 (length spis))
	  (progn
	    (setq spis1 (append spis1 spis))
	    (setq spis nil)
	   )
	 )
       )
	));end if progn




      );end progn


  )
  
    (if (/= spis1 nil)
      (progn
    (setq i 1 spis nil)
    (while (and (< i 21) (/= spis1 nil))
      (if (/= nil spis)
      (setq spis (strcat spis  "\n" (car (car spis1)) "===" (itoa (last (car spis1)))))
	(setq spis (strcat (car (car spis1)) "===" (itoa (last (car spis1)))))
	)
      (setq spis1 (cdr spis1) i (1+ i))
     )
    (alert spis)
    ));для вывода
  
)