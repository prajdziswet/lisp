;;---------------------вызначэнне СПДС формат----------------------
(defun v_format_spds (/ i z f_temp numa)

  (setq format nil)
  ;;вызначэнне маштабу
  (if (/= (assoc 40 list1) nil)
    (setq mash (/ 1 (cdr (assoc 40 list1))))
    (setq mash 1)
  )
;нормализация координат
  (if (and (= 3 (length x1)) (= 3 (length x2)) (/= 0 (length x1)))
    (progn
      (setq x1 (list (car x1) (nth 1 x1)))
      (setq x2 (list (car x2) (nth 1 x2)))
    )
  )
  ;;вызначэнне положения
  (if (> (abs (- (car x2) (car x1)))
	 (abs (- (last x1) (last x2)))
      )
    (SETQ poloz "А")
    (SETQ poloz "К")
  )
  (setq	i 0
	z 0
  )
  (while (< i 10)
    (if	(= "Info" (cdr (nth (+ 10 z) list1)))
      (progn
	(setq i 10)
	(setq z (1+ z))
      )
      (progn
	(setq z (1+ z))
	(setq i (1+ i))
      )
    )
  )					;end while

  (if (/= z 10)
    (progn
      (setq f_temp (vl-string-trim " " (cdr (nth (+ 10 z) list1))))
      (setq f_temp (substr f_temp
			   (+ (vl-string-search "\t" f_temp) 2)
			   (strlen f_temp)
		   )
      )
      (setq f_temp (substr f_temp
			   (+ (vl-string-search "\t" f_temp) 2)
			   (strlen f_temp)
		   )
      )
      (setq
	numa (norma_n2
	       (substr f_temp (+ (vl-string-search "\t" f_temp) 2) 10)
	     )
      )
      (setq f_temp (substr f_temp 1 (vl-string-search "\n" f_temp)))
      (setq f_temp (vl-string-subst "А" "A" f_temp))
      (setq f_temp (vl-string-subst "х" "x" f_temp))
    )
  )

  (if (= poloz "А")
    (SETQ format
	   (COND
	     ((= "А4х3" f_temp) "А4х3 (630.00 x 297.00 мм)")
	     ((= "А4х4" f_temp) "А4х4 (841.00 x 297.00 мм)")
	     ((= "А4х5" f_temp) "А4х5 (1051.00 x 297.00 мм)")
	     ((= "А4х6" f_temp) "А4х6 (1261.00 x 297.00 мм)")
	     ((= "А4х7" f_temp) "А4х7 (1471.00 x 297.00 мм)")
	     ((= "А4х8" f_temp) "А4х8 (1682.00 x 297.00 мм)")
	     ((= "А3х3" f_temp) "А3х3 (891.00 x 420.00 мм)")
	     ((= "А3х4" f_temp) "А3х4 (1189.00 x 420.00 мм)")
	     ((= "А3х5" f_temp) "А3х5 (1486.00 x 420.00 мм)")
	     ((= "А3х6" f_temp) "А3х6 (1783.00 x 420.00 мм)")
	     ((= "А3х7" f_temp) "А3х7 (2080.00 x 420.00 мм)")
	     ((= "А2х3" f_temp) "А2х3 (1261.00 x 594.00 мм)")
	     ((= "А2х4" f_temp) "А2х4 (1682.00 x 594.00 мм)")
	     ((= "А2х5" f_temp) "А2х5 (2102.00 x 594.00 мм)")
	     ((= "А1х3" f_temp) "А1х3 (1783.00 x 841.00 мм)")
	     ((= "А1х4" f_temp) "А1х4 (2378.00 x 841.00 мм)")
	     ((= "А0х2" f_temp) "А0х2 (1682.00 x 1189.00 мм)")
	     ((= "А0х3" f_temp) "А0х3 (2523.00 x 1189.00 мм)")
	     ((= "А0" f_temp) "А0 (1189.00 x 841.00 мм)")
	     ((= "А1" f_temp) "А1 (841.00 x 594.00 мм)")
	     ((= "А2" f_temp) "А2 (594.00 x 420.00 мм)")
	     ((= "А3" f_temp) "А3 (420.00 x 297.00 мм)")
	     ((= "А4" f_temp) "А4 (297.00 x 210.00 мм)")
	     (T nil)
	   )
    )					;то
    (SETQ format
	   (COND
	     ((= "А4х3" f_temp) "А4х3 (297.00 x 630.00 мм)")
	     ((= "А4х4" f_temp) "А4х4 (297.00 x 841.00 мм)")
	     ((= "А4х5" f_temp) "А4х5 (297.00 x 1051.00 мм)")
	     ((= "А4х6" f_temp) "А4х6 (297.00 x 1261.00 мм)")
	     ((= "А4х7" f_temp) "А4х7 (297.00 x 1471.00 мм)")
	     ((= "А4х8" f_temp) "А4х8 (297.00 x 1682.00 мм)")
	     ((= "А3х3" f_temp) "А3х3 (420.00 x 891.00 мм)")
	     ((= "А3х4" f_temp) "А3х4 (420.00 x 1189.00 мм)")
	     ((= "А3х5" f_temp) "А3х5 (420.00 x 1486.00 мм)")
	     ((= "А3х6" f_temp) "А3х6 (420.00 x 1783.00 мм)")
	     ((= "А3х7" f_temp) "А3х7 (420.00 x 2080.00 мм)")
	     ((= "А2х3" f_temp) "А2х3 (594.00 x 1261.00 мм)")
	     ((= "А2х4" f_temp) "А2х4 (594.00 x 1682.00 мм)")
	     ((= "А2х5" f_temp) "А2х5 (594.00 x 2102.00 мм)")
	     ((= "А1х3" f_temp) "А1х3 (841.00 x 1783.00 мм)")
	     ((= "А1х4" f_temp) "А1х4 (841.00 x 2378.00 мм)")
	     ((= "А0х2" f_temp) "А0х2 (1189.00 x 1682.00 мм)")
	     ((= "А0х3" f_temp) "А0х3 (1189.00 x 2523.00 мм)")
	     ((= "А0" f_temp) "А0 (841.00 x 1189.00 мм)")
	     ((= "А1" f_temp) "А1 (594.00 x 841.00 мм)")
	     ((= "А2" f_temp) "А2 (420.00 x 594.00 мм)")
	     ((= "А3" f_temp) "А3 (297.00 x 420.00 мм)")
	     ((= "А4" f_temp) "А4 (210.00 x 297.00 мм)")
	     (T nil)
	   )
    )
    ;;закрывает иначе
  )
  (if (/= format nil)
    (utvar numa)
  )
)					;канец вызначэнне формата


;;---------------------утварэнне списау druk_v druk_n----------------------
(defun utvar (numa0 / numa)
  (if (= numar 1)			;калі не вызначать нумары
    (if	(and (/= numa0 nil) (/= numa0 0))
      (setq numa numa0)
      (setq numa (numar_s))
    )					;end if
    (setq numa 0)
  )
  (if (or (= numa nil) (= numa 0))
    (if	(/= druk_n nil)
      (setq druk_n
	     (cons (list 0 x1 x2 format mash poloz model nil) druk_n)
      )
      (setq druk_n (list (list 0 x1 x2 format mash poloz model nil)))
    )					;end if
    (if	(/= druk_v nil)
      (setq druk_v (cons (list numa x1 x2 format mash poloz model nil)
			 druk_v
		   )
      )
      (setq druk_v (list (list numa x1 x2 format mash poloz model nil)))
    )					;end if
  )
)

;;---------------------нормализация № старонки----------------------
(defun norma_n (/ str)
  (setq str (cdr (assoc 1 (entget (ssname nabor_s 0)))))
  (setq str (vl-string-trim " " str))
  (if (wcmatch str "#,##,###,#`.#,##`.#,###`.#,#`.##,##`.##,###`.##,#`.###,##`.###,###`.###")
(princ str)
(princ nil)
    )
					;end if
)

;;---------------------нормализация № старонки(спдс)----------------------
(defun norma_n2	(str)
  (setq str (vl-string-trim " " str))
  (if (wcmatch str "#,##,###,#`.#,##`.#,###`.#,#`.##,##`.##,###`.##,#`.###,##`.###,###`.###")
(princ str)
(princ nil)
    )					;end if
)


;;---------------------вызначэнне № старонки----------------------
(defun numar_s (/ x_temp1 x_temp2 nabor_s xtemp temp_lm)
  (setq temp_lm (getvar "ctab"))	;атрыманне ліста або мадэлі дзе знаходзіца карыстальнік
					;нормализация координат
  (if (and (= 3 (length x1)) (= 3 (length x2)) (/= 0 (length x1)))
    (progn
      (setq x1 (list (car x1) (nth 1 x1)))
      (setq x2 (list (car x2) (nth 1 x2)))
    )
  )

  (if (> (last x2) (last x1))
    (progn
      (setq xtemp (last x1))
      (setq x1 (list (car x1) (last x2)))
      (setq x2 (list (car x2) xtemp))
    )
  )					;end if
  (setq
    x_temp1 (list (- (car x2) (/ 40 mash)) (+ (last x2) (/ 30 mash)))
  )
  (setq
    x_temp2 (list (- (car x2) (/ 25 mash)) (+ (last x2) (/ 20 mash)))
  )
  (if (/= model temp_lm)
    (setvar "ctab" model)		;пераход на патрэбны ліст або мадель
  )
  (vl-cmdf "_zoom" "_W" x_temp1 x_temp2) ;зумаванне акна нумара
  (setq nabor_s (ssget "_C" x_temp1 x_temp2 '((0 . "*EXT"))))
  (vl-cmdf "_zoom" "_p" x_temp1 x_temp2) ;вяртанне зумавання
  (if (/= model temp_lm)
    (setvar "ctab" temp_lm)		;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
  )

  (if (and (/= nabor_s nil) (= (sslength nabor_s) 1))
    (norma_n)
    (progn
      (setq x_temp1 (list (- (car x2) (/ 15 mash))
			  (+ (last x2) (/ 13 mash))
		    )
      )
      (setq x_temp2
	     (list (- (car x2) (/ 5 mash)) (+ (last x2) (/ 5 mash)))
      )

      (if (/= model temp_lm)
	(setvar "ctab" model)		;пераход на патрэбны ліст або мадель
      )
      (vl-cmdf "_zoom" "_W" x_temp1 x_temp2) ;зумаванне акна нумара
      (setq nabor_s (ssget "_W" x_temp1 x_temp2 '((0 . "*ext"))))
      (vl-cmdf "_zoom" "_p" x_temp1 x_temp2) ;вяртанне зумавання
      (if (/= model temp_lm)
	(setvar "ctab" temp_lm)		;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
      )

      (if (and (/= nabor_s nil) (= (sslength nabor_s) 1))
	(norma_n)
      )					;end if
    )					;end progn
  )					;end if

)					;канец вызначэнне старонки

;;---------------------праверка супадзенне нумароу----------------------
(defun prin_numar (/ nov_spis druk_v1 nomer_s nlist xt1 xt2 xt11 xt22)
  (setq nov_spis nil)
  (if (and (/= druk_v nil) (/= (last druk_v) nil))
    (progn
      (while (and (/= druk_v nil) (/= (last druk_v) nil))
	(setq druk_v1 nil)
	(if (= nov_spis nil)
	  (setq	nov_spis (list (cons (car (car druk_v))
				     (cdr (car druk_v))
			       )
			 )
	  )
	  (setq
	    nov_spis (append (list (cons (car (car druk_v))
					 (cdr (car druk_v))
				   )
			     )
			     nov_spis
		     )
	  )
	)				;end if
	(setq nomer_s (car (car druk_v))) ;атрыманне нумару старонки
	(setq xt1 nil
	      xt2 nil
	)
	(setq xt1 (nth 1 (car druk_v)))
	(setq xt2 (nth 2 (car druk_v)))
	(setq nlist 1)
	(setq druk_v (cdr druk_v))	;знишчэнне першага-ен перайшоу у новы спис
	(while (/= druk_v nil)
	  (if (equal nomer_s (car (car druk_v)))
	    (progn
	      (setq xt11 nil
		    xt22 nil
	      )
	      (setq xt11 (nth 1 (car druk_v)))
	      (setq xt22 (nth 2 (car druk_v)))
	      (if (and (= (nth 0 xt1) (nth 0 xt11))
		       (= (nth 1 xt1) (nth 1 xt11))
		       (= (nth 0 xt2) (nth 0 xt22))
		       (= (nth 1 xt2) (nth 1 xt22))
		  )
		(setq druk_v (cdr druk_v)) ;знишчэнне першага
		(progn
		  (setq
		    nov_spis (cons (cons (strcat nomer_s
						 "+"
						 (rtos nlist 2 0)
					 )
					 (cdr (car druk_v))
				   )
				   nov_spis
			     )
		  )
		  (setq nlist (1+ nlist))
		  (setq druk_v (cdr druk_v)) ;знишчэнне першага
		)
	      )				;end - progn

	    )
	    (progn
	      (if (/= druk_v1 nil)
		(setq druk_v1 (cons (car druk_v) druk_v1))
		(setq druk_v1 (list (car druk_v)))
	      )
	      (setq druk_v (cdr druk_v)) ;знишчэнне першага
	    )
	  )				;end if
	)				;end while
	(setq druk_v druk_v1)

      )					;end while
      (if (and (/= druk_v nil) (= (last druk_v) nil))
	(setq nov_spis
	       (append
		 nov_spis
		 (list (cons (car druk_v) (cdr druk_v)))
	       )
	)
      )
      (setq druk_v nov_spis)
    )					;end progn
    (progn
      (if (/= druk_v nil)
	(setq druk_v (cons (car druk_v) (cdr druk_v)))
      )
    )
  )					;end if
)					;end defun vuznach povtor

;;---------------------прастауленне невызначаных нумароу----------------------
(defun zad_n (/ nlist)
  (setq nlist 1)
  (while (/= druk_n nil)
    (if	(/= druk_v nil)
      (if (/= (last druk_n) nil)
	(setq druk_v (append druk_v
			     (list (cons (strcat (rtos nlist 2 0) "~")
					 (cdr (car druk_n))
				   )
			     )
		     )
	)
	(setq druk_v
	       (cons (cons (strcat (rtos nlist 2 0) "~") (cdr druk_n))
		     druk_v
	       )
	)
      )					;end if
      (if (/= (last druk_n) nil)
	(setq druk_v (list (cons (strcat (rtos nlist 2 0) "~")
				 (cdr (car druk_n))
			   )
		     )
	)
	(setq druk_v (cons (strcat (rtos nlist 2 0) "~") (cdr druk_n)))
      )					;end if
    )					;end if ///
    (setq nlist (1+ nlist))
    (if	(/= (last druk_n) nil)
      (setq druk_n (cdr druk_n))
      (setq druk_n nil)
    )
  )
)					;end zadфту num


;;---------------------вызначэнне фармату----------------------
(defun v_format	(x1 x2 / dis1 dis2)

  (SETQ dis1 (ABS (- (CAR x1) (CAR x2))))
  (SETQ dis2 (ABS (- (CADR x2) (CADR x1))))

  (setq format nil)
  ;;вызначэнне маштабу 1-1 або 1-100
  (if (> dis1 dis2)
    (if	(< dis1 27000)
      (SETQ mash 1)
      (progn
	(SETQ dis1 (/ dis1 100))
	(SETQ dis2 (/ dis2 100))
	(SETQ mash 0.01)
      )
    )
    (if	(< dis2 27000)
      (SETQ mash 1)
      (progn
	(SETQ dis1 (/ dis1 100))
	(SETQ dis2 (/ dis2 100))
	(SETQ mash 0.01)
      )
    )
  )

  (Defun ff1 (dis11 dis22 x1 x2)
    (if	(and (equal dis11 x1 2.01) (equal dis22 x2 2.01))
      T
      nil
    )
  )

  (if (= format nil)
    (if	(> dis1 dis2)
      (progn
	(SETQ poloz "А")
	(SETQ format
	       (COND
		 ((ff1 dis1 dis2 630 297) "А4х3 (630.00 x 297.00 мм)")
		 ((ff1 dis1 dis2 841 297) "А4х4 (841.00 x 297.00 мм)")
		 ((ff1 dis1 dis2 1051 297) "А4х5 (1051.00 x 297.00 мм)")
		 ((ff1 dis1 dis2 1261 297) "А4х6 (1261.00 x 297.00 мм)")
		 ((ff1 dis1 dis2 1471 297) "А4х7 (1471.00 x 297.00 мм)")
		 ((ff1 dis1 dis2 1682 297) "А4х8 (1682.00 x 297.00 мм)")
		 ((ff1 dis1 dis2 891 420) "А3х3 (891.00 x 420.00 мм)")
		 ((ff1 dis1 dis2 1189 420) "А3х4 (1189.00 x 420.00 мм)")
		 ((ff1 dis1 dis2 1486 420) "А3х5 (1486.00 x 420.00 мм)")
		 ((ff1 dis1 dis2 1783 420) "А3х6 (1783.00 x 420.00 мм)")
		 ((ff1 dis1 dis2 2080 420) "А3х7 (2080.00 x 420.00 мм)")
		 ((ff1 dis1 dis2 1261 594) "А2х3 (1261.00 x 594.00 мм)")
		 ((ff1 dis1 dis2 1682 594) "А2х4 (1682.00 x 594.00 мм)")
		 ((ff1 dis1 dis2 2102 594) "А2х5 (2102.00 x 594.00 мм)")
		 ((ff1 dis1 dis2 1783 841) "А1х3 (1783.00 x 841.00 мм)")
		 ((ff1 dis1 dis2 2378 841) "А1х4 (2378.00 x 841.00 мм)")
		 ((ff1 dis1 dis2 1682 1189) "А0х2 (1682.00 x 1189.00 мм)")
		 ((ff1 dis1 dis2 2523 1189) "А0х3 (2523.00 x 1189.00 мм)")
		 ((ff1 dis1 dis2 1189 841) "А0 (1189.00 x 841.00 мм)")
		 ((ff1 dis1 dis2 841 594) "А1 (841.00 x 594.00 мм)")
		 ((ff1 dis1 dis2 594 420) "А2 (594.00 x 420.00 мм)")
		 ((ff1 dis1 dis2 420 297) "А3 (420.00 x 297.00 мм)")
		 ((ff1 dis1 dis2 297 210) "А4 (297.00 x 210.00 мм)")
		 (T nil)
	       )
	)
      )					;то
      (progn
	(SETQ poloz "К")
	(SETQ format
	       (COND
		 ((ff1 dis2 dis1 630 297) "А4х3 (297.00 x 630.00 мм)")
		 ((ff1 dis2 dis1 841 297) "А4х4 (297.00 x 841.00 мм)")
		 ((ff1 dis2 dis1 1051 297) "А4х5 (297.00 x 1051.00 мм)")
		 ((ff1 dis2 dis1 1261 297) "А4х6 (297.00 x 1261.00 мм)")
		 ((ff1 dis2 dis1 1471 297) "А4х7 (297.00 x 1471.00 мм)")
		 ((ff1 dis2 dis1 1682 297) "А4х8 (297.00 x 1682.00 мм)")
		 ((ff1 dis2 dis1 891 420) "А3х3 (420.00 x 891.00 мм)")
		 ((ff1 dis2 dis1 1189 420) "А3х4 (420.00 x 1189.00 мм)")
		 ((ff1 dis2 dis1 1486 420) "А3х5 (420.00 x 1486.00 мм)")
		 ((ff1 dis2 dis1 1783 420) "А3х6 (420.00 x 1783.00 мм)")
		 ((ff1 dis2 dis1 2080 420) "А3х7 (420.00 x 2080.00 мм)")
		 ((ff1 dis2 dis1 1261 594) "А2х3 (594.00 x 1261.00 мм)")
		 ((ff1 dis2 dis1 1682 594) "А2х4 (594.00 x 1682.00 мм)")
		 ((ff1 dis2 dis1 2102 594) "А2х5 (594.00 x 2102.00 мм)")
		 ((ff1 dis2 dis1 1783 841) "А1х3 (841.00 x 1783.00 мм)")
		 ((ff1 dis2 dis1 2378 841) "А1х4 (841.00 x 2378.00 мм)")
		 ((ff1 dis2 dis1 1682 1189) "А0х2 (1189.00 x 1682.00 мм)")
		 ((ff1 dis2 dis1 2523 1189) "А0х3 (1189.00 x 2523.00 мм)")
		 ((ff1 dis2 dis1 1189 841) "А0 (841.00 x 1189.00 мм)")
		 ((ff1 dis2 dis1 841 594) "А1 (594.00 x 841.00 мм)")
		 ((ff1 dis2 dis1 594 420) "А2 (420.00 x 594.00 мм)")
		 ((ff1 dis2 dis1 420 297) "А3 (297.00 x 420.00 мм)")
		 ((ff1 dis2 dis1 297 210) "А4 (210.00 x 297.00 мм)")
		 (T nil)
	       )
	)
	;;закрывает иначе
      )					;закрывает сам иф
    )					;закрывает верх иф   
  )
)					;канец вызначэнне формата

;;-----------------------------друк поліліній-------------------------
(defun poli (/	     x3	     len     nabor_poly	     list_pol
	     x1	     x2	     format  mash    poloz   i	     i1
	     x3	     model
	    )
(vl-load-com)
  (setq nabor_poly (ssget "_X" '((0 . "LWPOLYLINE"))))

  (if (null nabor_poly)

    (progn
      (princ "\nНе составлен список полініній. ")
					; сообщение об отсутствии
      (princ)				; тихий выход
    )					; конец progn

    (progn
      (setq i	-1
	    len	(sslength nabor_poly)
      )
      (repeat len
	(setq i (1+ i))
					; Выбор следующего примитива и получение его списка
	(setq list_pol (vlax-ename->vla-object (ssname nabor_poly i)))


;;;      (setq list_pol (entget (ssname nabor_poly i)))
;;;      (setq i1 0)
;;;      (while (and (/= i1 5) (/= (car (nth (+ 15 i1) list_pol)) 10))
;;;	(setq i1 (1+ i1))
;;;	)
	(setq format nil)
	(setq x1 nil
	      x2 nil
	      x3 nil
	)
	(vla-GetBoundingBox list_pol 'x1 'x2)
	(setq x1 (vlax-safearray->list x1))
	(setq x2 (vlax-safearray->list x2))
;;;      (if (= 10 (car (nth (+ 35 i1) list_pol))) (setq x1 (cdr (nth (+ 35 i1) list_pol))));каардыната кропкі 5
;;;      (setq x2 (cdr (nth (+ 25 i1) list_pol)));каардыната кропкі 3
;;;      (if (= 10 (car (nth (+ 40 i1) list_pol))) (setq x3 (cdr (nth (+ 40 i1) list_pol))));каардыната кропкі 6, што няма
	(setq list_pol (entget (ssname nabor_poly i)))
	(setq model (cdr (assoc 410 list_pol))) ;model-list
	(if (and (/= x1 nil) (/= x2 nil) (= x3 nil))
	  (v_format x1 x2)
	)
	(if (/= format nil)
	  (utvar nil)
	)

      )
      ;;конец repeat
    )					;конец progn

  )
  ;;конец if

)					;канец дефана полі


;;-----------------------------друк блокау-------------------------
(defun blocks (/	 x3	   len	     nabor_blocks
	       list_pol	 x1	   x2	     x3	       format
	       mash	 poloz	   model     list_block
	       i	 i1	   model1    temp_lm   true_explode
	       obj	 len1  list_b1   list_b2
	      )
(vl-load-com)
  (setq format nil)
  (vl-load-com)
  (setq	nabor_blocks
	 (ssget	"_X"
		(list (cons 0 "INSERT")
		      (cons 100 "AcDbBlockReference")
		)
	 )
  )

  (if (null nabor_blocks)

    (progn
      (princ "\nНе составлен список блоков. ")
					; сообщение об отсутствии
      (princ)				; тихий выход
    )					; конец progn

    (progn
      (setq i	-1
	    len	(sslength nabor_blocks)
      )
      (repeat len
	(setq i (1+ i))			; Выбор следующего примитива и получение его списка
	(setq list_block (ssname nabor_blocks i)) ;имя примитива
	;--------------------------------------------------
;;;	(princ (strcat (rtos i) "\n"))
;;;	(if (= i 56)
;;;	  (princ "xxx")
;;;	 )
	;--------------------------------------------------
;;;      (setq obj (vlax-ename->vla-object list_block))
;;;      (if (vlax-method-applicable-p obj 'Explode)
;;;	(setq ents (vlax-safearray->list (vlax-variant-value (vla-Explode obj)))))
;;;      (foreach ent ents (progn
;;;			  (setq ar (vlax-vla-object->ename ent))
;;;			  (entget ar)
;;;			  )
;;;	)
	(if (= "форматы-рамок" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block))))
	  (progn
	    (setq model1 (cdr (assoc 410 (entget list_block)))) ;model-list
	    (setq temp_lm (getvar "ctab")) ;атрыманне ліста або мадэлі дзе знаходзіца карыстальнік
	    (setq true_explode nil)
	    (if	(vlax-method-applicable-p obj 'Explode)
	      (progn
		;кали не на патрэбнам листе
		(if (/= model1 temp_lm)
		  (setvar "ctab" model1);пераход на патрэбны ліст або мадель
		)
		
		(command "_.Explode" list_block) ;получение 2 полилиний
		(setq list_block (ssget "_p" '((0 . "LWPOLYLINE"))))

		;иф 111
		(if (and (/= list_block nil) (= 2 (sslength list_block)))
	      (progn
		
		(setq list_b1 (entget (ssname list_block 0)))
		(setq list_b2 (entget (ssname list_block 1)))
		;выртанне выбуху
		(command "_u")

		(if (/= model1 temp_lm)
		  (setvar "ctab" temp_lm)
					;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
		)
		(if (> (nth 2 (assoc '10 list_b1))
		       (nth 2 (assoc '10 list_b2))
		    )
		  (setq list_b1 list_b2)
		)
					;вызначэнне першага элемента-каардынаты
		(setq i1 0)
		(while
		  (and (/= i1 5) (/= (car (nth (+ 15 i1) list_b1)) 10))
		   (setq i1 (1+ i1))
		)
		(setq x1 nil
		      x2 nil
		      x3 nil
		)
		(if (= 10 (car (nth (+ 35 i1) list_b1)))
		  (setq x1 (cdr (nth (+ 35 i1) list_b1)))
		)			;каардыната кропкі 5
		(setq x2 (cdr (nth (+ 25 i1) list_b1)))
					;каардыната кропкі 3
		(if (= 10 (car (nth (+ 40 i1) list_b1)))
		  (setq x3 (cdr (nth (+ 40 i1) list_b1)))
		)			;каардыната кропкі 6, што няма

		(setq model (cdr (assoc 410 list_b1))) ;model-list
		(setq format nil)
		(if (and (/= x1 nil) (/= x2 nil) (= x3 nil))
		  (v_format x1 x2)
		)
		(if (/= format nil)
		  (utvar nil)
		)
		
		);прогн 111
		  
		 ;;вяртанне выбуха
	      (if (= true_explode T)
		(progn
		  (command "_u")
		  (if (/= model1 temp_lm)
		    (setvar "ctab" temp_lm);пераход на папярэдні ліст дзе знаходзіуся карыстальнік
		  )
		)
	      )
		  );иф 111
		
	    ));end pod if-progn
	    
	    );end progn
	  );end if




;---------------------------------стары працоуны метад---------------------------------------
;;;	(if (wcmatch (cdr (assoc '2 (entget list_block))) "`*U*")
;;;	  (progn
;;;
;;;	    (setq model1 (cdr (assoc 410 (entget list_block))))
;;;					;model-list
;;;	    (setq temp_lm (getvar "ctab"))
;;;					;атрыманне ліста або мадэлі дзе знаходзіца карыстальнік
;;;
;;;					;проверка што магчыма выбухнуть
;;;	    (setq true_explode nil)
;;;	    (setq obj (vlax-ename->vla-object list_block))
;;;	    (if	(vlax-method-applicable-p obj 'Explode)
;;;	      (setq true_explode T)
;;;	    )
;;;					;калі магчыма зрабіць выбух
;;;	    (if	(= true_explode T)
;;;	      (progn
;;;		(if (/= model1 temp_lm)
;;;		  (setvar "ctab" model1);пераход на патрэбны ліст або мадель
;;;		)
;;;					;взрыв
;;;		(command "_.Explode" list_block) ;получение 2 полилиний
;;;		(setq list_block (ssget "_p" '((0 . "LWPOLYLINE"))))
;;;	      )
;;;	       (setq list_block nil list_block_x nil)
;;;	    )
;;;	    (setq len1 nil)
;;;
;;;	    (if	(/= list_block nil)
;;;	       (setq len1 (sslength list_block))
;;;	    )
;;;
;;;	    (if	(= len1 2)
;;;	      (progn
;;;		(setq list_b1 (entget (ssname list_block 0)))
;;;		(setq list_b2 (entget (ssname list_block 1)))
;;;		;выртанне выбуху
;;;				  (command "_u")
;;;
;;;		(if (/= model1 temp_lm)
;;;		  (setvar "ctab" temp_lm)
;;;					;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
;;;		)
;;;		(if (> (nth 2 (assoc '10 list_b1))
;;;		       (nth 2 (assoc '10 list_b2))
;;;		    )
;;;		  (setq list_b1 list_b2)
;;;		)
;;;					;вызначэнне першага элемента-каардынаты
;;;		(setq i1 0)
;;;		(while
;;;		  (and (/= i1 5) (/= (car (nth (+ 15 i1) list_b1)) 10))
;;;		   (setq i1 (1+ i1))
;;;		)
;;;		(setq x1 nil
;;;		      x2 nil
;;;		      x3 nil
;;;		)
;;;		(if (= 10 (car (nth (+ 35 i1) list_b1)))
;;;		  (setq x1 (cdr (nth (+ 35 i1) list_b1)))
;;;		)			;каардыната кропкі 5
;;;		(setq x2 (cdr (nth (+ 25 i1) list_b1)))
;;;					;каардыната кропкі 3
;;;		(if (= 10 (car (nth (+ 40 i1) list_b1)))
;;;		  (setq x3 (cdr (nth (+ 40 i1) list_b1)))
;;;		)			;каардыната кропкі 6, што няма
;;;
;;;		(setq model (cdr (assoc 410 list_b1))) ;model-list
;;;		(setq format nil)
;;;		(if (and (/= x1 nil) (/= x2 nil) (= x3 nil))
;;;		  (v_format x1 x2)
;;;		)
;;;		(if (/= format nil)
;;;		  (utvar nil)
;;;		)
;;;	      )
;;;	      ;;вяртанне выбуха
;;;	      (if (= true_explode T)
;;;		(progn
;;;		  (command "_u")
;;;		  (if (/= model1 temp_lm)
;;;		    (setvar "ctab" temp_lm)
;;;					;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
;;;		  )
;;;		)
;;;	      )
;;;	    )
;;;	    
;;;	      
;;;
;;;
;;;	  )
;;;	)
      )

      ;;конец repeat
    )					;конец progn

  )
  ;;конец if

)					;канец дефана блокау


					;---------------------print_s-------------------
(defun print_s (spis / x1 x2 format mash poloz model temp_lm list_n
		fileuser fi0 cmd)
					;
  (if (< (vl-bb-ref 'lik_open) 20)
    (vl-bb-set 'lik_open (+ (vl-bb-ref 'lik_open) 1))
    (progn
      (startapp "__prog\\pdf_exit.exe")
      (vl-bb-set 'lik_open 0)
     )
    );end if
  (setq temp_lm (getvar "ctab"))	;атрыманне ліста або мадэлі дзе знаходзіца карыстальнік
  (setq list_n (car spis))
  (setq spis (cdr spis))
  (setq x1 (nth 0 spis))
  (setq x2 (nth 1 spis))
  (setq format (nth 2 spis))
  (setq mash (nth 3 spis))
  (setq poloz (nth 4 spis))
  (setq model (nth 5 spis))

  (setvar "ctab" model)			;пераход на патрэбны ліст або мадель

  (if (and (/= format nil) (/= (GETVAR "LOCALE") "ENG"))
    (progn
					;имя рысунка
      (if (/= list_n nil)
	(SETQ
	  fileuser (STRCAT (SUBSTR (GETVAR "dwgname")
				   1
				   (- (STRLEN (GETVAR "dwgname")) 4)
			   )
			   "_"
			   list_n
		   )
	)
	(SETQ fileuser (SUBSTR (GETVAR "dwgname")
			       1
			       (- (STRLEN (GETVAR "dwgname")) 4)
		       )
	)
      )
      (if (equal f_temp "")
	(setq f_temp (strcat fileuser ".pdf"))
	(setq f_temp (strcat f_temp "?" fileuser ".pdf"))
      )
					;утварэнне пути с файлом
      (SETQ fileuser (STRCAT (GETVAR "dwgprefix") fileuser ".pdf"))

      (if (= (setq fi0 (open fileuser "w")) nil)
	(progn
	  (print
	    "Файл ужо адчыненны іншай праграмай,\nабо немагчыма запісаць у гэты каталог (няма праў)"
	  )
	  (princ)			; тихий выход
	)				;канец прогона
	(progn
	  (close fi0)
	  (vl-file-delete fileuser)
	  (setq cmd (GETVAR "cmdecho"))
	  (SETVAR "cmdecho" 0)
	  (if (/= model "Model")
	    (command "_plot"		;Сама команда	
		     "_y"		;Выполнить детальное задание конфигурации?:	
		     ""			;Имя листа или <печать>:	
		     "_DWG в PDF ЭТО.pc3" ;Имя устройства вывода:	
		     format		;Формат листа бумаги
		     "_Millimeters"
		     ;;Единицы измерения размеров листа :
		     poloz
		     ;;Ориентация чертежа [Книжная/Альбомная]: 
		     "_N"
		     ;;Перевернуть чертеж? :
		     "Рамка"
		     ;;Печатаемая область [Экран/Границы/Лист/Вид/Рамка]:
		     x1
		     ;;першая коордыната
		     x2
		     ;;другая коордяната
		     mash
		     ;;Масштаб печати :
		     "_center"
		     ;;Смещение от начала(x,y)или[Центрировать]:
		     "_y"
		     ;;Учитывать стили печати? [Да/Нет]:
		     "monochrome.ctb"
		     ;;Имя таблицы стилей печати: 
		     "_y"
		     ;;Учитывать веса линий? [Да/Нет]:
		     "_N"
		     ;;Масштабировать веса линий?:
		     "_N"
		     ;;Печатать объекты листа первыми?:
		     "_N"
		     ;;Скрывать объекты листа?:
		     fileuser
		     ;;Введите имя файла : 
		     "_n"
		     ;;Сохранить изменения параметров листа
		     "_y"
		     ;;Перейти к печати [Да/Нет]
		    )
	    ;;калі модель
	    (command "_plot"		;Сама команда	
		     "_y"		;Выполнить детальное задание конфигурации?:	
		     ""			;Имя листа или <печать>:	
		     "_DWG в PDF ЭТО.pc3" ;Имя устройства вывода:	
		     format		;Формат листа бумаги
		     "_Millimeters"
		     ;;Единицы измерения размеров листа :
		     poloz
		     ;;Ориентация чертежа [Книжная/Альбомная]: 
		     "_N"
		     ;;Перевернуть чертеж? :
		     "Рамка"
		     ;;Печатаемая область [Экран/Границы/Лист/Вид/Рамка]:
		     x1
		     ;;першая коордыната
		     x2
		     ;;другая коордяната
		     mash
		     ;;Масштаб печати :
		     "_center"
		     ;;Смещение от начала(x,y)или[Центрировать]:
		     "_y"
		     ;;Учитывать стили печати? [Да/Нет]:
		     "monochrome.ctb"
		     ;;Имя таблицы стилей печати: 
		     "_y"
		     ;;Учитывать веса линий? [Да/Нет]:
		     "_N"
		     ;;Режим вывода тонированных ВЭ:
		     fileuser
		     ;;Введите имя файла : 
		     "_n"
		     ;;Сохранить изменения параметров листа
		     "_y"
		     ;;Перейти к печати [Да/Нет]
		    )

	  )
	  (SETVAR "cmdecho" cmd)
	  (princ (STRCAT "Файл захаваны:" fileuser))
	  (PRINC
	    "\n-----------------------------------------------------------\n"
	  )
	  (princ)
	)
      )
    )
  )					;канец ифа

  (setvar "ctab" temp_lm)		;пераход на папярэдні ліст дзе знаходзіуся карыстальнік  
)					;канец принта

					;------------------------апрацоука  палей диалогу---------------------------------
(defun palja ()
  (setq spds 1)
  (setq kos 1)
  (setq polir 1)
  (setq numar 1)
  
  ;(setq spds (atoi (get_tile "spds")))
  ;(setq kos (atoi (get_tile "kos")))
  ;(setq polir (atoi (get_tile "polir")))
  (setq pdf 1)
  ;(setq numar (atoi (get_tile "numar")))
  (setq rys 1)
  (setq sfile 0)
  (setq sfile_all 1)
)

					;------------------------СПДС вызначэнне-------------------------------
(defun spds1
       (/ x1 x2 format mash poloz model i1 i nabor len list1 list2)
  (setq nabor (ssget "X"))
(vl-load-com)
  (if (null nabor)

    (progn
      (princ "\nНе составлен список. ")	; сообщение об отсутствии
      (princ)				; тихий выход
    )					; конец progn

    (progn
      (setq i	-1
	    len	(sslength nabor)
      )
      (repeat len
	(setq i (1+ i))
					; Выбор следующего примитива и получение его списка
	(setq list1 (entget (ssname nabor i)))
	(if (= "SPDSFORMAT" (strcase (cdr (assoc 0 list1))))
	  (progn

					;взрыв
					;(command "_.Explode" (cdr (car list1)));получение 2 полилиний
					;(setq list_block (ssget "_p" '((0 . "LWPOLYLINE"))))

					;(setq len1 nil)
					;(setq len1 (sslength list_block))

	    (setq list2 (vlax-ename->vla-object (ssname nabor i)))
	    (vla-GetBoundingBox list2 'x1 'x2)
	    (setq x1 (vlax-safearray->list x1))
	    (setq x2 (vlax-safearray->list x2))

;;;	(if (= len1 2)
;;;	  (progn
;;;	    (setq list_b1 (entget (ssname list_block 0)))
;;;	    (setq list_b2 (entget (ssname list_block 1)))
;;;	    ;;вяртанне выбуха
;;;	    (command "_u")
;;;	    (if	(> (nth 2 (nth 17 list_b1))
;;;		   (nth 2 (nth 17 list_b2))
;;;		)
;;;	      (setq list_b1 list_b2)
;;;	    )
;;;
;;;	(setq i1 0)    
;;;      	(while (and (/= i1 5) (/= (car (nth (+ 15 i1) list_b1)) 10))
;;;	(setq i1 (1+ i1))
;;;	)
;;;	    (if	(= 10 (car (nth (+ 35 i1) list_b1)))
;;;	      (setq x1 (cdr (nth (+ 35 i1) list_b1)))
;;;	    );каардыната кропкі 5
;;;	    (setq x2 (cdr (nth (+ 25 i1) list_b1))) ;каардыната кропкі 3
;;;	    (if	(= 10 (car (nth (+ 40 i1) list_b1)))
;;;	      (setq x3 (cdr (nth (+ 40 i1) list_b1)))
;;;	    );каардыната кропкі 6, што няма

	    (setq model (cdr (assoc 410 list1))) ;model-list
	    (if	(and (/= x1 nil) (/= x2 nil))
	      (v_format_spds)
	    )

	  )
;;;	  (;;вяртанне выбуха
;;;	    (command "_u"))
;;;	)

	)				;);end if spdsFormat

      )
      ;;конец repeat
    )					;конец progn

  )
  ;;конец if
)

					;------------------------ПЕЧАТЬ---------------------------------
(defun peshat ()
  (if (and (/= druk_v nil) (= (length druk_v) 1))
    (setq druk_v (list (cons nil (cdr (car druk_v)))))
  )
  (if (/= druk_v nil)
    (if	(/= (last druk_v) nil)
      (progn
	(while (and (/= druk_v nil) (/= (last druk_v) nil))
	  (print_s (car druk_v))
	  (setq druk_v (cdr druk_v))
	)				;end while
	(if (and (/= druk_v nil) (= (last druk_v) nil))
	  (progn
	    (print_s druk_v)
	    (setq druk_v nil)
	  )
	)
      )					;end progn
      (print_s druk_v)
    )					;end if
  )					;end if

)

;------------------------параунанне каардынат--------------------------
(defun kordinat (x1 x2 x4 x5 / xtemp)

  ;нармалізація групп каардынат
    (if (and (= 3 (length x1)) (= 3 (length x2)) (/= 0 (length x1)))
    (progn
      (setq x1 (list (car x1) (nth 1 x1)))
      (setq x2 (list (car x2) (nth 1 x2)))
    )
  )

  (if (> (last x2) (last x1))
    (progn
      (setq xtemp (last x1))
      (setq x1 (list (car x1) (last x2)))
      (setq x2 (list (car x2) xtemp))
    )
  )

      (if (and (= 3 (length x4)) (= 3 (length x5)) (/= 0 (length x4)))
    (progn
      (setq x4 (list (car x4) (nth 1 x4)))
      (setq x5 (list (car x5) (nth 1 x5)))
    )
  )

  (if (> (last x5) (last x4))
    (progn
      (setq xtemp (last x4))
      (setq x4 (list (car x4) (last x5)))
      (setq x5 (list (car x5) xtemp))
    )
  )
;сканчэнне нармалізаціі
  
  (if (and (equal (car x1) (car x4) 0.0001) (equal (last x1) (last x4) 0.0001) (equal (car x2) (car x5) 0.0001) (equal (last x2) (last x5) 0.0001))
(princ T)
(princ nil)
 )
  
  )

;------------------------знішчэнне дулікатау--------------------------
(defun del_dubl (/ temp_car temp1 temp2 temp3)
  ;апрацоука нявызначанных
(while (/= nil druk_n)
  (setq temp_car (car druk_n))
  (setq temp1 (cdr druk_n))
  (while (/= nil temp1)
    (if (kordinat (nth 1 temp_car) (nth 2 temp_car) (nth 1 (car temp1)) (nth 2 (car temp1)))
      (setq temp1 (cdr temp1))
      (progn
	(if (= nil temp2)
	  (setq temp2 (list (car temp1)))
	  (setq temp2 (append temp2 (list (car temp1))))
	  );end if
	(setq temp1 (cdr temp1))
	);end progn
      )
    );end pod while

  ;перанос каров в масів
  	(if (= nil temp3)
	  (setq temp3 (list temp_car))
	  (setq temp3 (append temp3 (list temp_car)))
	  );end if
  
  ;сброс значэннеу
  (setq druk_n temp2)
  (setq temp1 nil temp2 nil)
  
  );end while
  (setq druk_n temp3)
  (setq temp1 nil temp2 nil temp3 nil)

    ;апрацоука вызначанных
(while (/= nil druk_v)
  (setq temp_car (car druk_v))
  (setq temp1 (cdr druk_v))
  (while (/= nil temp1)
    (if (kordinat (nth 1 temp_car) (nth 2 temp_car) (nth 1 (car temp1)) (nth 2 (car temp1)))
      (setq temp1 (cdr temp1))
      (progn
	(if (= nil temp2)
	  (setq temp2 (list (car temp1)))
	  (setq temp2 (append temp2 (list (car temp1))))
	  );end if
	(setq temp1 (cdr temp1))
	);end progn
      )
    );end pod while

  ;перанос каров в масів
  	(if (= nil temp3)
	  (setq temp3 (list temp_car))
	  (setq temp3 (append temp3 (list temp_car)))
	  );end if
  
  ;сброс значэннеу
  (setq druk_v temp2)
  (setq temp1 nil temp2 nil)
  
  );end while
  (setq druk_v temp3)
  (setq temp1 nil temp2 nil temp3 nil)
  
)
					;------------------------утварэнне спісау друку іх нумерацыя,ад дыялогу---------------------------------
(defun zapusk_spds (/ f_temp)
  (cond	((and (= spds 0) (= kos 1) (= polir 1))
	 (progn	(poli)
		(blocks)
	 )
	)
	((and (= spds 0) (= kos 1) (= polir 0)) (blocks))
	((and (= spds 0) (= kos 0) (= polir 1)) (poli))
	((and (= spds 1) (= kos 1) (= polir 1))
	 (progn	(poli)
		(blocks)
		(spds1)
	 )
	)
	((and (= spds 1) (= kos 0) (= polir 1))
	 (progn	(poli)
		(spds1)
	 )
	)
	((and (= spds 1) (= kos 1) (= polir 0))
	 (progn
	   (blocks)
	   (spds1)
	 )
	)
	((and (= spds 1) (= kos 0) (= polir 0)) (spds1))
  )					;конец конда

  (del_dubl);знішчэнне дублікатау
  (prin_numar)				;прастаука нумароу вызначанага спису
  (zad_n)				;прастаука нумароу невызначанага спису

  (setq f_temp "")
  (peshat)
  
  (vl-bb-set 'file_all
	     (strcat (vl-bb-ref 'file_all) "&" f_temp)
  )
  (princ)

)					;end function

					;------------------------друк у іншых файлах---------------------------------
(defun open-p (/ spds kos polir numar)
					;задаванне параметрау выбару
  (if (= (vl-bb-ref 'directory-p) 1)
    (progn
      (vl-bb-set 'directory-p nil)
      (setq spds (nth 0 (vl-bb-ref 'dil-p)))
      (setq kos (nth 1 (vl-bb-ref 'dil-p)))
      (setq polir (nth 2 (vl-bb-ref 'dil-p)))
      (setq numar (nth 3 (vl-bb-ref 'dil-p)))
					;друк
      (zapusk_spds)
    )
  )
)
					;запуск в открываемых файлах
(if (/= (vl-bb-ref 'directory-p) nil)
  (open-p)
)
					;------------------------перадача друку у іншых файлах(адчынене файлау)---------------------------------
(defun peshat-spds-file	(/ peshat-files peshat-file)
  (vl-bb-set 'dil-p (list spds kos polir numar))
					;перадача аргументау дыялогу
  (setq	peshat-files
	 (vl-directory-files (GETVAR "dwgprefix") "*.dwg" 1)
  )					;выбар файлау
  (while (/= peshat-files nil)
    (if	(/= (GETVAR "dwgname") (car peshat-files))
      (progn
	(vl-bb-set 'directory-p 1)	;запуск у іншых адчыняем дакумент  
	(setq peshat-file
	       (strcat (GETVAR "dwgprefix") (car peshat-files))
	)				;адчыняемы файл
	(setq peshat-files (cdr peshat-files)) ;обрезка
	(setq peshat-file
	       (vla-Open
		 (vla-get-Documents (vlax-get-acad-object))
		 peshat-file
		 :flax-true
		 ""
	       )
	)				;адчыненне
	(vla-Close peshat-file :vlax-false) ;зачыненне
      )					;end progn
      (setq peshat-files (cdr peshat-files)) ;обрезка
    )
  )					;end while and if
  (vl-bb-set 'directory-p nil)		;забарона друку у іншых файлах
)					;--


					;------------------------выклік діалогу---------------------------------
(defun dil_spds (/	    dcl_id   spds     kos      pdf
		   polir    numar    rys      ddi      druk_n
		   druk_v   done     file_all sfile_all
		   sfile  lik_open data
		  )
  (PRINC "\n---------------------------------------------------------------------------\n")
  (PRINC "Праграмма распрацавана на lisp, prajdziswet-ам (Косаў Уладзімір) у 2014 годзе\n")
  (PRINC "-----------------------------------------------------------------------------\n")
  (PRINC "\n")

  (setq data (atoi (rtos (getvar "CDATE") 2 0)))
  (if (> data 20170201)
    (progn
      (alert "тэрмін дзеянне праграммы скончыуся\nзвярніцесь да вытворцы")
      (exit)
     )
  )
  
(vl-load-com)
  (vl-bb-set 'lik_open 0)
  (setq done nil ddi 1)

(palja)
  (if (= ddi 1)
    (progn
					;усталека первічных параметрау
      (vl-bb-set 'file_all
		 (strcat (GETVAR "dwgprefix")
			 "&"
			 (if (equal sfile 1)
			   (princ "true")
			   (princ "false")
			 )
			 "&"
			 (if (equal sfile_all 1)
			   (princ "true")
			   (princ "false")
			 )
		 )
      )
      (zapusk_spds)
      (if (= rys 0)
	(peshat-spds-file)
      )
      (princ (vl-bb-ref 'file_all));прі тесте
      (if (OR (= pdf 1) (= sfile_all 1) (= sfile 1))
	(if (OR (equal sfile_all 1) (equal sfile 1))
	  (startapp (strcat "__prog\\pdf_exit.exe " (vl-bb-ref 'file_all)))
	  (startapp "__prog\\pdf_exit.exe")
	)
      )
    )
  )					;end if

)
;

(dil_spds)