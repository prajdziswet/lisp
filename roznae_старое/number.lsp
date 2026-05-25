(defun number_p ( / nabor temp pref suf zam obj mpref msuf prov probel)
(print "выделите тект поэлементно, другие элементы оно необращает внимание?\nтак что можите выделять дальше,\n даже если выбрали другой элемент (кроме текста)")
  (setq nabor nil)
  (setq nomer (- nomer 1))
  ;(txt (car (entsel "выбирите элемент")))
(while (/= nil (setq temp (entget (setq obj (car (entsel "выбирите элемент"))))))
	   (if (OR (= (cdr (assoc 0 temp)) "MTEXT") (= (cdr (assoc 0 temp)) "TEXT"))
	     (cond
	       ((= vubor "0")
		(setq mpref "" msuf "")
		(setq zam (assoc 1 temp))
		(setq zam_nov (num1 (cdr zam)))
		(setq temp (subst (cons 1 (strcat mpref pref pref_num (rtos (setq nomer (+ nomer 1)) 2 0) probel zam_nov suf msuf)) zam temp))
		(entmod temp)
		);end0
	       	((= vubor "1")
		 (setq mpref "" msuf "")
		(setq zam (assoc 1 temp))
		(setq zam_nov (num1 (cdr zam)))
		(setq temp (subst (cons 1 (strcat mpref pref_num (rtos (setq nomer (+ nomer 1)) 2 0) probel zam_nov msuf)) zam temp))
		(entmod temp))
		((= vubor "2")
		  (setq mpref "" msuf "")
		(setq zam (assoc 1 temp))
		(setq zam_nov (num1 (cdr zam)))
		(setq temp (subst (cons 1 (strcat mpref pref zam_nov suf msuf)) zam temp))
		(entmod temp)
		);end0
	     ));end if-cond
 );end while

  
 
  );end defun

;-------------------------------------------------------
(defun num1 (text / t1 t2)
  (if (= (cdr (assoc 0 temp)) "MTEXT")
    (txt)
    )
  (setq prov nil)
  (setq t1 (vl-string-left-trim " " text))
  (setq t2 (vl-string-right-trim " " text))
  (setq pref (substr text 1 (- (strlen text) (strlen t1))))
  (setq suf (substr text (+ 1 (strlen t2))))
  (setq t1 (vl-string-trim " " text))
  (setq t1
  (cond
    ((wcmatch t1 "##`.###*") (setq prov T) (substr t1 7) )
    ((wcmatch t1 "##`.##*") (setq prov T) (substr t1 6) )
    ((wcmatch t1 "##`.#*") (setq prov T) (substr t1 5) )
    ((wcmatch t1 "##*") (setq prov T) (substr t1 3) )
    ((wcmatch t1 "#*") (setq prov T) (substr t1 2) )
    (T (substr t1 1))
   );end cond
	)
  (setq probel (cond ((= nil prov) " ") (T "")))
  (cond
    ((= vubor "2") (vl-string-trim " " t1))
    (T t1)
   )
 )

;----------------
(defun txt (/ block len t1 poz)
  (command "_.Explode" obj)
  (setq block (ssget "_p" '((0 . "TEXT"))))
  (setq len (sslength block))
  (if (and (= len 1) (setq poz (vl-string-search (setq t1 (cdr (assoc 1 (entget (ssname block 0))))) text)))
(progn
  (setq mpref (substr text 1 poz))
  (setq msuf (substr text (+ poz (strlen t1) 1)))
  (if (= vubor "1") (setq msuf (vl-string-trim " " msuf)))
  (setq text t1)
 )
    );end if
  
;;;  (repeat len
;;;    (setq temp (entget (ssname block i)))
;;;	  (if (AND (= max1 nil) (= min1 nil))
;;;	    (setq max1 temp min1 temp)
;;;	    )
;;;    (if (and (< (nth 0 (assoc 10 temp)) (nth 0 (assoc 10 min1)))
;;;	     (> (nth 1 (assoc 10 temp)) (nth 1 (assoc 10 min1))))
;;;      (setq min1 temp)
;;;      (if (< (nth 1 (assoc 10 temp)) (nth 1 (assoc 10 max1)))
;;;      (setq max1 temp)
;;;      (if (> (nth 0 (assoc 10 temp)) (nth 0 (assoc 10 min1)))
;;;	(setq max1 temp)
;;;       )));end if
;;;    (setq i (+ 1 i))
;;;     );end repeat
;;;   (setq t1 (cdr (assoc 1 min1)))
;;;   (setq t2 (cdr (assoc 1 max1)))
  (command "_u")
      )

  ;выклтк дыалогу----------------------------------------
  (defun c:dial_num (/ vubor nomer pref_num)
    (vl-load-com)
  (setq done nil)
  (setq dcl_id (load_dialog "d:\\Косов\\prog\\_Програм\\_acad\\для панельки\\num.dcl"))
  (if (not (new_dialog "num" dcl_id))
    (exit)
  )
    
  (action_tile "accept" "(paslja1) (done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq ddi (start_dialog))
  (unload_dialog dcl_id)
    (number_p)
    );end defun

;---------------------------------------------------------
(defun paslja1 (/ start)
    (setq vubor (get_tile "b0"))
    (setq start (vl-string-trim " " (get_tile "b1")))
    (if (and (= start "") (/= "2" vubor))
      (progn
	(alert "Не задано число")
	(exit)
       )); end-progn
    (if (and (wcmatch start "*`.*") (/= 0 (atoi (setq pref_num (substr start 1 (+ 1 (vl-string-search "." start)))))))
(setq nomer (atoi (substr start (+ 2 (vl-string-search "." start)))))
(progn
  (setq pref_num "")
  (setq nomer (atoi start))
 ));end if progn

)
  

;;;(defun sortXY ( / i list1 temp_nabor temp_nabor1 i i1 temp1 mash podspis vuxod v1 min1 min2)
;;;  (setq i 0)
;;;  (repeat len
;;;    	(setq i (1+ i))	; Выбор следующего примитива и получение его списка(entget
;;;	(setq list1 (ssname nabor i))
;;;    (if (OR (= (cdr (assoc 0 list1)) "TEXT") (= (cdr (assoc 0 list1)) "MTEXT"))
;;;    ;утварэнне спису - перанос
;;;    (if (= temp_nabor nil)
;;;      (setq temp_nabor (list list1))
;;;      (setq temp_nabor (append temp_nabor (list list1)))
;;;     );end if
;;;);end if Text Mtext
;;;    
;;;    );end len
;;;
;;;;вызначэнне маштабу
;;; (setq mash (/ (assoc 40 (entget (car temp_nabor))) 2.5))
;;;
;;;  (while (/= nil temp_nabor)
;;;    (setq fist (car temp_nabor) i 0 podspis nil)
;;;    (setq XY (assoc 10 (entget fist)))
;;;    (setq temp_nabor (cdr temp_nabor))
;;;    ;отбор элементов соответствующие равнениен к 1 элементу
;;;    (while (and (/= nil temp_nabor) (< i (length temp_nabor)));while 0
;;;      (setq XY1 (assoc 10 (entget (nth i temp_nabor))))
;;;      (if (and (equal (nth 0 XY) (nth 0 XY1) (* mash 140)) (equal (nth 1 XY) (nth 1 XY1) (* mash 70)))
;;;	(progn
;;;	 (if (/= nil podspis)
;;;	  (setq podspis (append podspis (list (nth i temp_nabor))))
;;;	  (setq podspis (list (nth i temp_nabor)))
;;;	  )
;;;	      (setq temp_nabor (cdr (subst (car temp_nabor) (nth  i temp_nabor) temp_nabor)))
;;;	 );end progn
;;;	(setq i (+ 1 i))
;;;	);end if
;;;     );end while0
;;;    
;;;    ;внес первого элемента и проверка остальных
;;;    (setq vuxod (list fist))
;;;    (while (/= nil podspis)
;;;      (setq fist (car podspis) i 0)
;;;      (setq vuxod (append vuxod (list fist)))
;;;      (setq XY (assoc 10 (entget fist)))
;;;     (setq podspis (cdr podspis))
;;;
;;;          (while (and (/= nil temp_nabor) (< i (length temp_nabor)));while 0
;;;      (setq XY1 (assoc 10 (entget (nth i temp_nabor))))
;;;      (if (and (equal (nth 0 XY) (nth 0 XY1) (* mash 140)) (equal (nth 1 XY) (nth 1 XY1) (* mash 70)))
;;;	(progn
;;;	 (if (/= nil podspis)
;;;	  (setq podspis (append podspis (list (nth i temp_nabor))))
;;;	  (setq podspis (list (nth i temp_nabor)))
;;;	  )
;;;	      (setq temp_nabor (cdr (subst (car temp_nabor) (nth  i temp_nabor) temp_nabor)))
;;;	 );end progn
;;;	(setq i (+ 1 i))
;;;	);end if
;;;     );end while0
;;;      
;;;      );end while1
;;;    (setq vuxod (list vuxod))
;;;
;;;    (if (= nil v1)
;;;      (setq v1 vuxod)
;;;      (setq v1 (append v1 vuxod))
;;;     )
;;;    (setq vuxod nil)
;;;    );конец выделенние блоков текста
;;;  ;---------------
;;;  
;;;  (setq len (length v1))
;;;  (repeat len
;;;    (setq fist (sortyY (car v1)))
;;;    (setq v1 (cdr v1))
;;;
;;;    (if (= nil vuxod)
;;;      (setq vuxod (list fist))
;;;      (setq vuxod (append vuxod (list fist)))
;;;      )
;;;   );end repeat
;;;  ;скончыли сартыраваць внутри блока
;;;
;;;  
;;;         (setq i 0 i1 1)
;;;      
;;;      (while (< i len)
;;;	(setq min2 (nth i vuxod))
;;;	(setq min1 (car min2))
;;;	(setq XY (assoc 10 (entget min1)))
;;;	;вызначэнн мін
;;;	(while (< i1 len)
;;;	  (setq XY1 (assoc 10 (entget (car (nth i1 vuxod)))))
;;;	  (if (> (nth 1 XY) (nth 1 XY1))
;;;	    (progn
;;;	      (setq min2 (nth i1 vuxod))
;;;	      (setq min1 (car min2))
;;;	      (setq XY (assoc 10 (entget min1)))
;;;	      ))
;;;	  )
;;;	;ставим мин на первое место
;;;	(setq temp1 (car vuxod))
;;;	(setq vuxod (subst "xxx" temp1 vuxod))
;;;	(setq vuxod (subst temp1 min2 vuxod))
;;;	(setq vuxod (subst min2 "xxx" vuxod))
;;;	(setq i1 (+ i 1))
;;;	;сортируем удовлет мин
;;;	(while i1<len
;;;	  (setq XY1 (assoc 10 (entget (car (nth i1 vuxod)))))
;;;	  (if (and (> (nth 0 XY) (nth 0 XY1)) (equal (nth 1 XY) (nth 1 XY1) (* mash 70)))
;;;	    (progn
;;;	      (setq temp1 (car vuxod))
;;;	      (setq temp2 (nth  i1 vuxod))
;;;	      (setq vuxod (subst "xxx" temp1 vuxod))
;;;	      (setq vuxod (subst temp1 temp2 vuxod))
;;;	      (setq vuxod (subst temp2 "xxx" vuxod))
;;;	     );progn
;;;	    ;-------------------------------------------------------------
;;;	    );end if
;;;	  (setq i1 (1+ i1))
;;;	);end while 2
;;;	
;;;	);end while
;;;  
;;;; дробим
;;;  (temp_nabor nil)
;;;  (while (/= nil vuxod)
;;;    (setq fist (car vuxod))
;;;    (setq vuxod (cdr vuxod))
;;;    (while (/= nil fist)
;;;      (if (= nil temp_nabor)
;;;	(progn
;;;	  (setq temp_nabor (list (car fist)))
;;;	  (setq fist (cdr fist))
;;;	  )
;;;	(progn
;;;	  (setq temp_nabor (append temp_nabor (list (car fist))))
;;;	  (setq fist (cdr fist))
;;;	  )
;;;      )));end while-s
;;;
;;;  (setq nabor temp_nabor)
;;;  );end defun
;;;
;;;;(entget (ssname (ssget) 0))  10
;;;
;;;(defun sortyY (temp / len temp1 temp2)
;;;  (if (/= nil temp)
;;;    (progn
;;;   (setq len (length temp))
;;;
;;;         (setq i 0 i1 1)
;;;      
;;;      (while i<len
;;;	(setq XY (assoc 10 (entget (nth i temp))))
;;;	(while i1<len
;;;	  (setq XY1 (assoc 10 (entget (nth i1 temp))))
;;;	  (if (> (nth 1 XY) (nth 1 XY1)) 
;;;	    (progn
;;;	      (setq temp1 (car temp))
;;;	      (setq temp2 (nth  i1 temp))
;;;	      (setq temp (subst "xxx" temp1 temp))
;;;	      (setq temp (subst temp1 temp2 temp))
;;;	      (setq temp (subst temp2 "xxx" temp))
;;;	     );progn
;;;	    ;-------------------------------------------------------------
;;;	    );end if
;;;	  (setq i1 (1+ i1))
;;;	);end while 2
;;;	(setq i (1+ i))
;;;	(setq i1 (+ i 1))
;;;	);enw while1
;;;   
;;;   (setq temp temp);вывод
;;;  
;;;  )));eden-progn