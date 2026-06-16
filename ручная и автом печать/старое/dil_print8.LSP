;----вызначэнне битнасти----------
(defun bit64 ()
  (vl-load-com) 
  (> (strlen (vl-prin1-to-string (vlax-get-acad-object))) 40) 
 )

;вяртанне назову рысунка
(defun namelist (/ x_temp1 x_temp2 nabor_s temp_all text n len i text)
  (setq x_temp1 (list (- (car x2) (/ 125 mash)) (+ (last x2) (/ 20 mash))))
  (setq x_temp2 (list (- (car x2) (/ 55 mash)) (+ (last x2) (/ 5 mash))))
  (setq text "")
  (vl-cmdf "_zoom" "_W" x_temp1 x_temp2) ;зумаванне акна нумара
  (setq nabor_s (ssget "_W" x_temp1 x_temp2 '((0 . "*EXT"))))
  (if (and (/= nil nabor_s) (= (sslength nabor_s) 1))
;progn
    (progn
      (setq temp_all (entget (ssname nabor_s 0)))
      (if (= (cdr (assoc 0 temp_all)) "MTEXT")
	(progn
	  (command "_.Explode" (ssname nabor_s 0)) ;получение текста
	  (setq temp_all (ssget "_p" '((0 . "TEXT"))))
	  (setq i 0 len (sslength temp_all))
	  (while (< i len)
	    (setq text (strcat text (cdr (assoc 1 (entget (ssname temp_all i))))))
	    (setq i (+ 1 i))
		   )
	  
	  (command "_u")
	 )
	(if (= (cdr (assoc 0 temp_all)) "TEXT")
	  (setq text (strcat text (cdr (assoc 1 temp_all))))
	 )
       );end if
     );end progn
    (if (and (/= nil nabor_s) (> (sslength nabor_s) 1))
      (progn
	(setq i 0 len (sslength nabor_s))
      (while (< i len)
	    (setq text (strcat text (cdr (assoc 1 (entget (ssname nabor_s i))))))
	(setq i (+ 1 i))
		   )
	
      ))
   );end if
  (command "_u")
  (if (= text nil) (setq text ""))
  (setq text text)
  
 );end defun

;вяртанне шифру
(defun nameshifr (temp / x_temp1 x_temp2 nabor_s temp_all text n len i text);temp nil-верхняя шапка, инакш-нижняя
  (if (/= nil temp)
    (progn
  (setq x_temp1 (list (- (car x2) (/ 125 mash)) (+ (last x2) (/ 20 mash))))
  (setq x_temp2 (list (- (car x2) (/ 15 mash)) (+ (last x2) (/ 5 mash))))
  )
    (progn
  (setq x_temp1 (list (- (car x2) (/ 125 mash)) (+ (last x2) (/ 60 mash))))
  (setq x_temp2 (list (- (car x2) (/ 5 mash)) (+ (last x2) (/ 50 mash))))
  ));сканчэнне вызначэнне каардынат
  (setq text "")
  (vl-cmdf "_zoom" "_W" x_temp1 x_temp2) ;зумаванне акна нумара
  (setq nabor_s (ssget "_W" x_temp1 x_temp2 '((0 . "*EXT"))))
  (if (and (/= nil nabor_s) (= (sslength nabor_s) 1))
;progn
    (progn
      (setq temp_all (entget (ssname nabor_s 0)))
      (if (= (cdr (assoc 0 temp_all)) "MTEXT")
	(progn
	  (command "_.Explode" (ssname nabor_s 0)) ;получение текста
	  (setq temp_all (ssget "_p" '((0 . "TEXT"))))
	  (setq i 0 len (sslength temp_all))
	  (while (< i len)
	    (setq text (strcat text (cdr (assoc 1 (entget (ssname temp_all i))))))
	    (setq i (+ 1 i))
		   )
	  
	  (command "_u")
	 )
	(if (= (cdr (assoc 0 temp_all)) "TEXT")
	  (setq text (strcat text (cdr (assoc 1 temp_all))))
	 )
       );end if
     );end progn
    (if (and (/= nil nabor_s) (> (sslength nabor_s) 1))
      (progn
	(setq i 0 len (sslength nabor_s))
      (while (< i len)
	    (setq text (strcat text (cdr (assoc 1 (entget (ssname nabor_s i))))))
	(setq i (+ 1 i))
		   )
	
      ))
   );end if
  (command "_u")
  (setq text text)
  
 )
;сканчэнне вяртанне шифру

;--------------функция атрыманне с дин.блоку тексту----------------
(defun get-block-maj (name_bl / din_bl spis sv1 sv din_sv atts)
  ;преобразование имени ва вла
  (setq din_bl (vlax-ename->vla-object name_bl))
  ;получение динамических свойств
  (setq sv (vlax-safearray->list(vlax-variant-value (vla-getdynamicblockproperties din_bl))))
  (foreach sv1 sv
    (if (/= (vlax-variant-type (vla-get-value sv1)) 8197)
    (if (/= nil spis)
      (setq spis (append spis (list (vlax-variant-value (vla-get-value sv1)))))
      (setq spis (list (vlax-variant-value (vla-get-value sv1))))
      )
)
   )

  ;получили название видимости или формат-размер

  ;получение атрибутов
(setq atts (vlax-safearray->list 
                       (vlax-variant-value
                         (vla-getattributes din_bl)
                       )))
  ;(vla-get-tagstring tag)-название атрибута тут не будем проверять, возьмет сам атрибут  
(foreach tag atts
  (setq spis (append spis (list (vla-get-TextString tag))))
)
  )


;выхад з пдф
(defun exit_pdf (CloseOne)
  (if (= nil CloseOne) (setq CloseOne ""))
  (if (OR (= nil vers) (= nil proc))
    (progn
      ;вызначенне года аутакада
(setq vers (substr (vl-bb-ref 'dirpol) (+ (vl-string-search "AUTODESK" (strcase (vl-bb-ref 'dirpol))) 18) 4))
;вызначэнне процессара
(if (bit64)
  (setq proc "64")
  (setq proc "32")
 )
)); канец ифа

  (if (and (OR (= vers "2012") (= vers "2017") (= vers "2018") (= vers "2014")) (= proc "64"))
	  (pdf_close_one CloseOne)
	  (startapp "__prog\\pdf_exit.exe")
)
    
  );выхад з пдф


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
(if (> (last x2) (last x1))
    (progn
      (setq xtemp (last x1))
      (setq x1 (list (car x1) (last x2)))
      (setq x2 (list (car x2) xtemp))
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
      ;вызначэнне нумару спдс
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
	     ((= "А4х9" f_temp) "А4х9 (1892.00 x 297.00 мм)")
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
	     ((= "А4х9" f_temp) "А4х9 (297.00 x 1892.00 мм)")
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
;сканчэнне нармализации каардынат
(if (> (nth 1 x2) (nth 1 x1))
    (progn
      (setq xtemp (nth 1 x1))
      (setq x1 (list (car x1) (nth 1 x2)))
      (setq x2 (list (car x2) xtemp))
    )
  )
(if (> (nth 0 x1) (nth 0 x2))
    (progn
      (setq xtemp (nth 0 x1))
      (setq x1 (list (car x2) (nth 1 x1)))
      (setq x2 (list xtemp (nth 1 x2)))
    ))
;;;  (if (= numar 1)			;калі не вызначаны нумар
    (if	(and (/= numa0 nil) (/= numa0 0) (/= numa0 "0"))
      (setq numa numa0)
      (setq numa (numar_s))
    )
;сканчэнне нармализации каардынат
  (if (and (OR (= nameris "") (= nameris nil)) (/= zapret_name T))
    (setq nameris (namelist))
    (setq nameris "")
   )
;end if
;;;    (setq numa 0)
;;;  )
  (if (or (= numa nil) (= numa 0))
    (if	(/= druk_n nil)
      (setq druk_n
	     (cons (list 0 x1 x2 format mash poloz model nameris nil) druk_n)
      )
      (setq druk_n (list (list 0 x1 x2 format mash poloz model nameris nil)))
    )					;end if
    (if	(/= druk_v nil)
      (setq druk_v (cons (list numa x1 x2 format mash poloz model nameris nil)
			 druk_v
		   )
      )
      (setq druk_v (list (list numa x1 x2 format mash poloz model nameris nil)))
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

 (setq zapret_nomer nil)
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
  (command "_u")
  ;(vl-cmdf "_zoom" "_p" x_temp1 x_temp2) ;вяртанне зумавання
  (if (/= model temp_lm)
    (setvar "ctab" temp_lm)		;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
  )

  (if (and (/= nabor_s nil) (= (sslength nabor_s) 1))
    (norma_n)
    (progn
      ;праверка наяунасти "листов"----------------------------
      (setq x_temp1 (list (- (car x2) (/ 25 mash))
			  (+ (last x2) (/ 30 mash))
		    )
      )
      (setq x_temp2
	     (list (- (car x2) (/ 5 mash)) (+ (last x2) (/ 20 mash)))
      )
      (if (/= model temp_lm)
	(setvar "ctab" model)		;пераход на патрэбны ліст або мадель
      )
      (vl-cmdf "_zoom" "_W" x_temp1 x_temp2) ;зумаванне акна нумара
      (setq nabor_s (ssget "_C" x_temp1 x_temp2 '((0 . "*EXT"))))
      (command "_u")
      ;(vl-cmdf "_zoom" "_p" x_temp1 x_temp2) ;вяртанне зумавання
      (if (/= model temp_lm)
	(setvar "ctab" temp_lm)		;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
      )

      (if (and (/= nabor_s nil) (= (sslength nabor_s) 1))
	(setq zapret_nomer (norma_n))
      )
      ;---------------сканчэнне листов-------------------------
      (if (= nil zapret_nomer)
	(progn
	  ;калі малы штамп
	(setq zapret_name T)
	
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
      (command "_u")
      ;(vl-cmdf "_zoom" "_p" x_temp1 x_temp2) ;вяртанне зумавання
      (if (/= model temp_lm)
	(setvar "ctab" temp_lm)		;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
      )

      (if (and (/= nabor_s nil) (= (sslength nabor_s) 1))
	(setq zapret_nomer (norma_n))
      )					;end if

      )
	(princ zapret_nomer)
	);end if с запретом zapret_nomer-номер листов
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
(defun v_formats ()
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
		 ((ff1 dis1 dis2 1892 297) "А4х9 (1892.00 x 297.00 мм)")
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
		 ((ff1 dis2 dis1 1892 297) "А4х9 (297.00 x 1892.00 мм)")
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
 )
;;---------------------вызначэнне фармату бэсп(блок богославч)----------------------
(defun v_formats_besp (f_besp / f_temp)
 (defun utvar_k (sp)
   (setq x2 (list (+ (car x1) (car sp)) (+ (nth 1 x1) (nth 1 sp))))
   )
   (defun utvar_k1 (sp)
   (setq x2 (list (+ (car x1) (nth 1 sp)) (+ (nth 1 x1) (car sp))))
   )
  (if (= format nil)
    (if	(wcmatch f_besp "*альбомная*")
      (progn
	(SETQ poloz "А")
	(setq f_temp (vl-string-trim " " (substr f_besp 1 (- (strlen f_besp) (strlen "(альбомная)")))))
	(setq f_temp (vl-string-subst "А" "A" f_temp))
	(setq f_temp (vl-string-subst "А" "А" f_temp))
	       (COND
		 ((= f_temp "А4х3")(utvar_k '(630 297)) (SETQ format "А4х3 (630.00 x 297.00 мм)"))
		 ((= f_temp "А4х4")(utvar_k '(841 297)) (SETQ format  "А4х4 (841.00 x 297.00 мм)"))
		 ((= f_temp "А4х5")(utvar_k '(1051 297)) (SETQ format  "А4х5 (1051.00 x 297.00 мм)"))
		 ((= f_temp "А4х6")(utvar_k '(1261 297)) (SETQ format  "А4х6 (1261.00 x 297.00 мм)"))
		 ((= f_temp "А4х7")(utvar_k '(1471 297)) (SETQ format  "А4х7 (1471.00 x 297.00 мм)"))
		 ((= f_temp "А4х8")(utvar_k '(1682 297)) (SETQ format  "А4х8 (1682.00 x 297.00 мм)"))
		 ((= f_temp "А4х9")(utvar_k '(1892 297)) (SETQ format  "А4х9 (1892.00 x 297.00 мм)"))
		 ((= f_temp "А3х3")(utvar_k '(891 420)) (SETQ format  "А3х3 (891.00 x 420.00 мм)"))
		 ((= f_temp "А3х4")(utvar_k '(1189 420)) (SETQ format  "А3х4 (1189.00 x 420.00 мм)"))
		 ((= f_temp "А3х5")(utvar_k '(1486 420)) (SETQ format  "А3х5 (1486.00 x 420.00 мм)"))
		 ((= f_temp "А3х6")(utvar_k '(1783 420)) (SETQ format  "А3х6 (1783.00 x 420.00 мм)"))
		 ((= f_temp "А3х7")(utvar_k '(2080 420)) (SETQ format  "А3х7 (2080.00 x 420.00 мм)"))
		 ((= f_temp "А2х3")(utvar_k '(1261 594)) (SETQ format  "А2х3 (1261.00 x 594.00 мм)"))
		 ((= f_temp "А2х4")(utvar_k '(1682 594)) (SETQ format  "А2х4 (1682.00 x 594.00 мм)"))
		 ((= f_temp "А2х5")(utvar_k '(2102 594)) (SETQ format  "А2х5 (2102.00 x 594.00 мм)"))
		 ((= f_temp "А1х3")(utvar_k '(1783 841)) (SETQ format  "А1х3 (1783.00 x 841.00 мм)"))
		 ((= f_temp "А1х4")(utvar_k '(2378 841)) (SETQ format  "А1х4 (2378.00 x 841.00 мм)"))
		 ((= f_temp "А0х2")(utvar_k '(1682 1189)) (SETQ format  "А0х2 (1682.00 x 1189.00 мм)"))
		 ((= f_temp "А0х3")(utvar_k '(2523 1189)) (SETQ format  "А0х3 (2523.00 x 1189.00 мм)"))
		 ((= f_temp "А0")(utvar_k '(1189 841)) (SETQ format  "А0 (1189.00 x 841.00 мм)"))
		 ((= f_temp "А1")(utvar_k '(841 594)) (SETQ format  "А1 (841.00 x 594.00 мм)"))
		 ((= f_temp "А2")(utvar_k '(594 420)) (SETQ format  "А2 (594.00 x 420.00 мм)"))
		 ((= f_temp "А3")(utvar_k '(420 297)) (SETQ format  "А3 (420.00 x 297.00 мм)"))
		 ((= f_temp "А4")(utvar_k '(297 210)) (SETQ format  "А4 (297.00 x 210.00 мм)"))
		 (T nil)
	       )
      )					;то
      (progn
	(SETQ poloz "К")
	(setq f_temp (vl-string-trim " " (substr f_besp 1 (- (strlen f_besp) (strlen "(книжная)")))))
	(setq f_temp (vl-string-subst "А" "A" f_temp))
	(setq f_temp (vl-string-subst "А" "А" f_temp))
	       (COND
		 ((= f_temp "А4х3")(utvar_k1 '(630 297)) (SETQ format "А4х3 (297.00 x 630.00 мм)"))
		 ((= f_temp "А4х4")(utvar_k1 '(841 297)) (SETQ format "А4х4 (297.00 x 841.00 мм)"))
		 ((= f_temp "А4х5")(utvar_k1 '(1051 297)) (SETQ format "А4х5 (297.00 x 1051.00 мм)"))
		 ((= f_temp "А4х6")(utvar_k1 '(1261 297)) (SETQ format "А4х6 (297.00 x 1261.00 мм)"))
		 ((= f_temp "А4х7")(utvar_k1 '(1471 297)) (SETQ format "А4х7 (297.00 x 1471.00 мм)"))
		 ((= f_temp "А4х8")(utvar_k1 '(1682 297)) (SETQ format "А4х8 (297.00 x 1682.00 мм)"))
		 ((= f_temp "А4х9")(utvar_k1 '(1892 297)) (SETQ format "А4х9 (297.00 x 1892.00 мм)"))
		 ((= f_temp "А3х3")(utvar_k1 '(891 420)) (SETQ format "А3х3 (420.00 x 891.00 мм)"))
		 ((= f_temp "А3х4")(utvar_k1 '(1189 420)) (SETQ format "А3х4 (420.00 x 1189.00 мм)"))
		 ((= f_temp "А3х5")(utvar_k1 '(1486 420)) (SETQ format "А3х5 (420.00 x 1486.00 мм)"))
		 ((= f_temp "А3х6")(utvar_k1 '(1783 420)) (SETQ format "А3х6 (420.00 x 1783.00 мм)"))
		 ((= f_temp "А3х7")(utvar_k1 '(2080 420)) (SETQ format "А3х7 (420.00 x 2080.00 мм)"))
		 ((= f_temp "А2х3")(utvar_k1 '(1261 594)) (SETQ format "А2х3 (594.00 x 1261.00 мм)"))
		 ((= f_temp "А2х4")(utvar_k1 '(1682 594)) (SETQ format "А2х4 (594.00 x 1682.00 мм)"))
		 ((= f_temp "А2х5")(utvar_k1 '(2102 594)) (SETQ format "А2х5 (594.00 x 2102.00 мм)"))
		 ((= f_temp "А1х3")(utvar_k1 '(1783 841)) (SETQ format "А1х3 (841.00 x 1783.00 мм)"))
		 ((= f_temp "А1х4")(utvar_k1 '(2378 841)) (SETQ format "А1х4 (841.00 x 2378.00 мм)"))
		 ((= f_temp "А0х2")(utvar_k1 '(1682 1189)) (SETQ format "А0х2 (1189.00 x 1682.00 мм)"))
		 ((= f_temp "А0х3")(utvar_k1 '(2523 1189)) (SETQ format "А0х3 (1189.00 x 2523.00 мм)"))
		 ((= f_temp "А0")(utvar_k1 '( 1189 841)) (SETQ format "А0 (841.00 x 1189.00 мм)"))
		 ((= f_temp "А1")(utvar_k1 '(841 594)) (SETQ format "А1 (594.00 x 841.00 мм)"))
		 ((= f_temp "А2")(utvar_k1 '(594 420)) (SETQ format "А2 (420.00 x 594.00 мм)"))
		 ((= f_temp "А3")(utvar_k1 '(420 297)) (SETQ format "А3 (297.00 x 420.00 мм)"))
		 ((= f_temp "А4")(utvar_k1 '(297 210)) (SETQ format "А4 (210.00 x 297.00 мм)"))
		 (T nil))
	;;закрывает иначе
      )					;закрывает сам иф
    )					;закрывает верх иф   
  )
 )


;вызначэнне фармату полилилиний
(defun v_format	(x1 x2 / dis1 dis2)

  (setq nameris nil zapret_name nil)
  
  (SETQ dis1 (ABS (- (CAR x1) (CAR x2))))
  (SETQ dis2 (ABS (- (CADR x2) (CADR x1))))

  (setq format nil)

  ;нормализация координат
  (if (and (= 3 (length x1)) (= 3 (length x2)) (/= 0 (length x1)))
    (progn
      (setq x1 (list (car x1) (nth 1 x1)))
      (setq x2 (list (car x2) (nth 1 x2)))
    )
  )

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

(v_formats)  
)					;канец вызначэнне формата

;;-----------------------------друк поліліній-------------------------
(defun poli (/	     x3	     len     nabor_poly	     list_pol
	     x1	     x2	     format  mash    poloz   i	     i1
	     x3	     model
	    )
  (setq zapret_name nil)
(vl-load-com)
  (if (= true_explode nil)
  (setq nabor_poly (ssget "_X" '((0 . "LWPOLYLINE"))))
  (setq nabor_poly (ssget "_p" '((0 . "LWPOLYLINE"))))
    )

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

	(setq format nil)
	(setq x1 nil
	      x2 nil
	      x3 nil
	)
	(vla-GetBoundingBox list_pol 'x1 'x2)
	(setq x1 (vlax-safearray->list x1))
	(setq x2 (vlax-safearray->list x2))
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

;;---------------------вызначэнне фармату блока----------------------
(defun v_format_bl (name_bl / dis1 dis2 temp point2 tempf tempx tempy dis)

  (setq temp (get-block-maj name_bl));возращение свойств дин блока
  (setq mash (/ 1 (cdr (assoc 41 (entget name_bl)))));маштаб
  (setq point2 (cdr (assoc 10 (entget name_bl))))
  ;-------------------------под бэсп----------------
  (setq x2 (list (+ (car point2) (/ 5 mash)) (- (nth 1 point2) (/ 5 mash))));вторая координата
;;;  (if (= "формат-бэсп-опс" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block))))

  ;-------------------------
  (if (= "форматы-рамок" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block))))       
    (progn
  (setq nameris (nth 4 temp) zapret_name nil)
  (setq numo (norma_n2 (nth 6 temp)))

  ;(setq tempf (substr (car temp) 0 (vl-string-search " " (car temp))))
  (setq dis (substr (car temp) (+ 2 (vl-string-search "(" (car temp)))
		    (- (vl-string-search ")" (car temp)) (+ 1 (vl-string-search "(" (car temp))))
		       ))
  (setq dis2 (atoi (substr dis 1 (vl-string-search "х" dis))))
  (setq dis1 (atoi (substr dis (+ 2 (vl-string-search "х" dis)))))
)
    (if (= "Рамка в подшивку" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block))))
      (progn
	(setq nameris (nth 62 temp) zapret_name nil)
	(setq numo (norma_n2 (nth 85 temp)))
       );сканчэнне
      ;---------------------------------------------------------------
      (if (= "бэсп-опс-new" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block))))
(progn
  (setq nameris (nth 21 temp) zapret_name nil)
  (setq numo (norma_n2 (nth 19 temp)))

  (setq dis2 (nth 2 temp))
  (setq dis1 (nth 1 temp))
);---------------------------------------------------------------
      (if (OR (= (car temp) "Первый-альбомная") (= (car temp) "Первый-книжная"))
	(progn
	  (setq dis1 (* mash (nth 1 temp)))
	  (setq dis2 (* mash (nth 2 temp)))
	  (setq numo (norma_n2 (vl-string-trim " " (nth 16 temp))))
	  (setq nameris (vl-string-trim " " (nth 18 temp)) zapret_name nil)
	 )
	(progn
	  (setq dis1 (* mash (nth 1 temp)))
	  (setq dis2 (* mash (nth 2 temp)))
	  (setq numo (norma_n2 (vl-string-trim " " (nth 16 temp))))
	  (setq nameris nil zapret_name T)
	)
       )
	)
      )
    )
  (if (/= "Рамка в подшивку" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block))))
  (setq x1 (list (- (car x2) (/ dis1 mash)) (+ (nth 1 x2) (/ dis2 mash))))
    (setq x1 point2)
    )
  (setq format nil)

  (if (/= "Рамка в подшивку" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block))))
    (v_formats)(v_formats_besp (nth 25 temp))) 
);канец вызначэнне формата блока

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
	(if (OR (= "форматы-рамок" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block))))
		(= "формат-это" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block))))
		(and (= "Рамка в подшивку" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block)))) (/= "Model" (cdr (assoc 410 (entget list_block)))))
		(and (= "бэсп-опс-new" (vla-get-effectivename (setq obj (vlax-ename->vla-object list_block)))) (/= "Model" (cdr (assoc 410 (entget list_block)))))
		);формат-бэсп-опс
	  (progn 
	    (setq temp_lm (getvar "ctab")) ;атрыманне ліста або мадэлі дзе знаходзіца карыстальнік
	    (setq model (cdr (assoc 410 (entget list_block)))) ;model-list
	    ;кали не на патрэбнам листе
		(if (/= model temp_lm)
		  (setvar "ctab" model);пераход на патрэбны ліст або мадель
		)
(setq numo nil)	    
(v_format_bl list_block);вызначэнне параметрау
(if (/= format nil)
		  (utvar numo)
		)	    
	    
	    (if (/= model temp_lm)
		  (setvar "ctab" temp_lm)
					;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
		)
    
	    );end progn "форматы-рамок"
	  
	  ;---------------------------------стары працоуны метад-для іншых блокау---------------------------------------
	(if (and (= stor 1) (not (wcmatch (cdr (assoc '2 (entget list_block))) "`*U*")))
	  (progn

	    (setq model1 (cdr (assoc 410 (entget list_block))))	;model-list
	    (setq temp_lm (getvar "ctab"));атрыманне ліста або мадэлі дзе знаходзіца карыстальнік

	    ;проверка што магчыма выбухнуть
	    (setq true_explode nil)
	    (setq obj (vlax-ename->vla-object list_block))
	    (if	(vlax-method-applicable-p obj 'Explode)
	      (setq true_explode T)
	    )
	    ;калі магчыма зрабіць выбух
	    (if	(= true_explode T)
	      (progn
		(if (/= model1 temp_lm)
		  (setvar "ctab" model1);пераход на патрэбны ліст або мадель
		)
					;взрыв
		(command "_.Explode" list_block) ;получение 2 полилиний
		(poli)
		(command "_u")
	      )
	       (setq list_block nil list_block_x nil)
	    )
	    (setq len1 nil)
	    
	  )
	)
	  ;стары метад -для іншых блокаў
	  
	  );end if

      )

      ;;конец repeat
    )					;конец progn

  )
  ;;конец if

)					;канец дефана блокау


					;---------------------print_s-------------------
(defun print_s (spis / x1 x2 format mash poloz model temp_lm list_n
		fileuser fi0 cmd nameris)
					;

  (setq temp_lm (getvar "ctab"))	;атрыманне ліста або мадэлі дзе знаходзіца карыстальнік
  (setq list_n (car spis))
  (if (/= list_n nil) (setq list_n1 list_n))
  (setq spis (cdr spis))
  (setq x1 (nth 0 spis))
  (setq x2 (nth 1 spis))
  (setq format (nth 2 spis))
  (setq mash (nth 3 spis))
  (setq poloz (nth 4 spis))
  (setq model (nth 5 spis))
  (setq nameris (nth 6 spis));назва рысынка, у "наступных" не вызначаецца

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
      
      (if (equal ris_temp "")
	(if (/= nil nameris)
	(setq ris_temp (strcat fileuser ".pdf" "?"  nameris "?" list_n1))
	(setq ris_temp (strcat fileuser ".pdf" "?"  "" "?" list_n1))
	  )
	(if (/= nil nameris)
	(setq ris_temp (strcat ris_temp "|" fileuser ".pdf" "?" nameris "?" list_n1))
	(setq ris_temp (strcat ris_temp "|" fileuser ".pdf" "?" "" "?" list_n1))
	  )
      )
					;утварэнне пути с файлом
      (SETQ fileuser (STRCAT (GETVAR "dwgprefix") fileuser ".pdf"))

  ;закрытие файла если он открыт тут (на этом компе
  (if (= (setq fi0 (open fileuser "w")) nil)
    (exit_pdf fileuser)
    (close fi0)
    )
      
      (if (= (setq fi0 (open fileuser "w")) nil)
	(progn
	  (print
	    "Файл ужо адчыненны іншай праграмай,\nабо немагчыма запісаць у гэты каталог (няма праў)"
	  )
	  (princ)			; тихий выход
	)				;канец прогона
	(progn
	  (close fi0)
	  (if (or (wcmatch format "А0х2*") (wcmatch format "А0х3*")) (alert "Даннй формат(А0х2 или А0х2) будет перевен в pdf,\n но технически его невозможно будет распечатать"))
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

  ;закрытие пдф
;;;    (if (< (vl-bb-ref 'lik_open) 14)
;;;    (vl-bb-set 'lik_open (+ (vl-bb-ref 'lik_open) 1))
;;;    (progn
;;;      (vl-bb-set 'lik_open 0)
      (exit_pdf fileuser)
   
;;;     )
;;;    );end if
  
)					;канец принта

					;------------------------апрацоука  палей диалогу---------------------------------
(defun palja ()
  
;;;  (setq numar 1)

  ;закрыть пдф
  ;(setq pdf (atoi (get_tile "pdf")))
  ;закрыть сторонние блоки
  (setq stor (atoi (get_tile "stor")))
  
  ;теукщий чертеж или папку
  (if (= (atoi (get_tile "Radio1")) 1)
    (setq rys 1)
    (setq rys 0)
    )
  ;склеить как
  (if (= 0 (atoi (get_tile "sfile_merge")))
	 (setq sfile 0 sfile_all 0)
	 )
    (if (= 1 (atoi (get_tile "sfile_merge")))
	 (setq sfile 1 sfile_all 0)
	 )
  (if (= 2 (atoi (get_tile "sfile_merge")))
	 (setq sfile 0 sfile_all 1)
	 )
   
)

					;------------------------СПДС вызначэнне-------------------------------
(defun spds1
       (/ x1 x2 format mash poloz model i1 i nabor len list1 list2 name1 name2 name3)
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
	    (setq nameris nil zapret_name nil)
	    (setq list2 (vlax-ename->vla-object (ssname nabor i)))
	    (vla-GetBoundingBox list2 'x1 'x2)
	    (setq x1 (vlax-safearray->list x1))
	    (setq x2 (vlax-safearray->list x2))

	    (setq model (cdr (assoc 410 list1))) ;model-list
	    (if (<= (length list1) 95)
	    	(if (/= (vl-position (cons 301 "Drawing type") list1) nil)
	      (setq zapret_name T nameris nil)
		(setq zapret_name nil nameris nil)
		  )
	      (progn
	    (setq name1 (cdr (nth (+ 1 (vl-position (cons 301 "Drawing type") list1)) list1)))
	    (if (/= nil (vl-position (cons 301 "Drawing type1") list1))
	    (setq name2 (cdr (nth (+ 1 (vl-position (cons 301 "Drawing type1") list1)) list1)))
	      )
	    (if (/= nil (vl-position (cons 301 "Drawing type1") list1))
	    (setq name3 (cdr (nth (+ 1 (vl-position (cons 301 "Drawing type2") list1)) list1)))
	      )
	    (if (and (/= name2 nil) (/= name3 nil))
	    (setq nameris (vl-string-trim " " (strcat name1 name2 name3)))
	      (setq nameris (vl-string-trim " " name1))
	      )
	     ));nd progn if
	    ;вызначэнне назову рысынка

	    (if	(and (/= x1 nil) (/= x2 nil))
	      (v_format_spds)
	    )

	  )


	)				;);end if spdsFormat

      )
      ;;конец repeat
    )					;конец progn

  )
  ;;конец if
)

					;------------------------ПЕЧАТЬ---------------------------------
(defun peshat (/ list_n1)
  (if (and (/= druk_v nil) (= (length druk_v) 1))
    (progn
      (if (= (setq list_n1 (caar druk_v)) nil)
	(setq list_n1 "")
       )
     (setq druk_v (list (cons nil (cdr (car druk_v)))))
  ))
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
    (if (and (kordinat (nth 1 temp_car) (nth 2 temp_car) (nth 1 (car temp1)) (nth 2 (car temp1))) (= (nth 6 (car temp1)) (nth 6 temp_car)))
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
    (if (and (kordinat (nth 1 temp_car) (nth 2 temp_car) (nth 1 (car temp1)) (nth 2 (car temp1))) (= (nth 6 (car temp1)) (nth 6 temp_car)))
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
(defun zapusk_spds (/ f_temp ris_temp)
  
;друк полилиний-блокау-спдс
	(poli)(blocks)(spds1)
  
;знішчэнне дублікатау
  (del_dubl)
  (prin_numar);прастаука нумароу вызначанага спису
  (zad_n);прастаука нумароу невызначанага спису

  (setq f_temp "")
  (setq ris_temp "")
  (peshat)

  (if (/= (vl-bb-ref 'file_ris) nil)
    (vl-bb-set 'file_ris
	     (strcat (vl-bb-ref 'file_ris) "&" ris_temp)
  )
    (vl-bb-set 'file_ris ris_temp)
   )

  
  (vl-bb-set 'file_all
	     (strcat (vl-bb-ref 'file_all) "&" f_temp)
  )
  (princ)

)					;end function

					;------------------------друк у іншых файлах---------------------------------
(defun open-p ();numar
					;задаванне параметрау выбару
  (if (= (vl-bb-ref 'directory-p) 1)
    (progn
      (vl-bb-set 'directory-p nil)
;;;      (setq numar (nth 3 (vl-bb-ref 'dil-p)))
      (setq stor (vl-bb-ref 'dil-p))
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
  (vl-bb-set 'dil-p stor)
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
(defun c:dil_spds (/	    dcl_id    pdf
		   rys      ddi      druk_n
		   druk_v   done     file_all sfile_all
		   sfile  lik_open data stor data1 nameris
		  );numar
  (PRINC "\n---------------------------------------------------------------------------\n")
  (PRINC "Праграмма распрацавана на lisp, prajdziswet-ам (Косаў Уладзімір) у 2014 годзе\n")
  (PRINC "-----------------------------------------------------------------------------\n")
  (PRINC "\n")

  ;----------------------------------------------------------------------
  ;вызначэнне версии акада-трошки недакладна
(setq vers (substr (vl-bb-ref 'dirpol) (+ (vl-string-search "AUTODESK" (strcase (vl-bb-ref 'dirpol))) 18) 4))

(if (bit64)
  (setq proc "64")
  (setq proc "32")
 )
  ;end if
  ;----------------------------------------------------------------------
  
  ; праверка даты и выхад если надо
  (setq data (atoi (rtos (getvar "CDATE") 2 0)))
  (setq data1 (atoi (rtos (- (getvar "CDATE") (- (getvar "DATE") (getvar "TDUUPDATE"))) 2 0)))
  (if (or (> data 20200101) (> data1 20200101))
    (progn
      (alert "Тэрмін дзеянне праграммы скончыуся 01-01-2020\nзвярніцесь да вытворцы(за больш актуальнай версіяй)")
      (exit)
     )
  )
  
(vl-load-com)
  (vl-bb-set 'lik_open 0)
  ;if auto
  (if (/= auto T)
    (progn
  (setq done nil)
  ;d:\\Косов\\prog\\_Програм\\_acad\\для панельки\\ручная и автом печать\\ h:\\prog\\_Програм\\_acad\\для панельки\\ручная и автом печать\\
  (setq dcl_id (load_dialog "print_spds7.dcl"))
  (if (not (new_dialog "prindcl" dcl_id))
    (exit)
  )
  (action_tile "accept" "(palja)(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq ddi (start_dialog))
  (unload_dialog dcl_id)
  )
    ;progn auto
    (progn
     (setq rys 1 sfile_all 1 sfile 0 ddi 1);numar 1 pdf 1
     )
    );end if auto
  
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
      (vl-bb-set 'file_ris nil);обнуление
      (zapusk_spds)
      (if (= rys 0)
	(peshat-spds-file)
      )
      (if (= memory nil) (setq memory 0))
      (if (OR (= sfile_all 1) (= sfile 1)) ;(= pdf 1)   
	(if (OR (equal sfile_all 1) (equal sfile 1))
	    ;адпраука файлау на друн пдф
	  (if (and (OR (= vers "2012") (= vers "2017") (= vers "2018") (= vers "2014")) (= proc "64"))
  (progn
   (bookmark (vl-bb-ref 'file_ris))
   (add_edit memory (vl-bb-ref 'file_all))
   )
    (if (= proc "64")
      (startapp (strcat "__prog\\pdf_exit64.exe " (vl-bb-ref 'file_all)))
      (startapp (strcat "__prog\\pdf_exit.exe " (vl-bb-ref 'file_all)))
      )
  );vers
  	   ;(exit_pdf)//закоментировано при закрытии одного файла
	)
      )
    )
  )					;end if
  (vl-bb-set 'lik_open nil)

)
;

;------------------------выклік діалогу з рэдактара---------------------------------
(defun dil_spds_new (memory stor / auto)
  (setq auto T)
  (c:dil_spds)
  )


