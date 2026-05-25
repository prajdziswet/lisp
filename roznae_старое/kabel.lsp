;вызначэнне шляху
(setq temp (getvar "ACADPREFIX"))
(while (not (wcmatch (substr temp 1 (vl-string-search ";" temp 0)) "*Users*support*"))
  (setq temp (substr temp (+ 1 (vl-string-search ";" temp 0)) (- (strlen temp) (+ 1 (vl-string-search ";" temp 0)))))
 )
(setq dirpol (strcat (substr temp 1 (vl-string-search ";" temp 0))"\\"))
(setq temp nil)
  
(if (= dirpol nil)
(progn  
(setq dirpol (strcat "C:/Users/" (getvar "LOGINNAME")
		     "/AppData/Roaming/Autodesk/AutoCAD 2010/R18.0/rus/Support/"))
(setvar "CMDECHO" 0)
;ад весрии акада
(cond
  ((= "18.0" (substr (getvar "acadver") 1 4))
   (setq dirpol (strcat "C:/Users/" (getvar "LOGINNAME")
		     "/AppData/Roaming/Autodesk/AutoCAD 2010/R18.0/rus/Support/"))
   )
   ((= "18.1" (substr (getvar "acadver") 1 4))
   (setq dirpol (strcat "C:/Users/" (getvar "LOGINNAME")
		     "/AppData/Roaming/Autodesk/AutoCAD 2011/R18.1/rus/Support/"))
   )
   ((= "18.2" (substr (getvar "acadver") 1 4))
   (setq dirpol (strcat "C:/Users/" (getvar "LOGINNAME")
		     "/AppData/Roaming/Autodesk/AutoCAD 2012/R18.2/rus/Support/"))
   )
 ) 
));end if
;канец 
;___________________________________________

;функція вяртання х
(defun retx (tee / x)
(setq x (car (cdr (assoc 10 (entget tee)))))
  )
;канец функціі
;___________________________________________

;функція appends
(defun appends (t1 t2 / t3)
(if (= t1 nil)
  (progn
    (if (= (type t2) (type '(1 2)))
      (setq t3 t2)
      (setq t3 (list t2))
      )
   );end progn
  (progn
    (if (= (type t1) (type '(1 2)))
      (progn
	(if (= (type t2) (type '(1 2)))
	  (setq t3 (append t1 t2))
	  (setq t3 (append t1 (list t2)))
	  )
       );end progn
      (progn
	(if (= (type t2) (type '(1 2)))
	  (setq t3 (append (list t1) t2))
	  (setq t3 (append (list t1) (list t2)))
	  )
       );end progn
      )
   );end progn
 );end if
  )
;канец функціі
;___________________________________________

;функція зліяння
(defun merge (left rigth / rez)
  (while (and (> (length left) 0) (> (length rigth) 0))
    (if (<= (retx (car left)) (retx (car rigth)))
      (progn
	(if (= (retx (car left)) (retx (car rigth)))
	  (progn
	(setq rez (appends rez (car left)))
	(setq rez (appends rez (car rigth)))
	(setq left (cdr left))
	(setq rigth (cdr rigth))
	    );end progn
	  (progn
	(setq rez (appends rez (car left)))
	(setq left (cdr left))
	  ));end if
	);end progn
      (progn
	(setq rez (appends rez (car rigth)))
	(setq rigth (cdr rigth))
       );end progn
     );end if
   );end while
    
    (if (> (length left) 0)
      (setq rez (appends rez left))
     );end if

     (if (> (length rigth) 0)
      (setq rez (appends rez rigth))
     );end if

 (setq rez rez) 
  )
;канец функціі
;___________________________________________

;функція зліянне
(defun mergesort (tee / t3 left rigth len)
(if (<= (length tee) 1)
  (setq t3 tee)
  (progn
    (setq m (fix (/ (length tee) 2)) i 0)
    (while (< i m)
      (setq i (1+ i))
      (setq left (appends left (car tee)))
      (setq tee (cdr tee))
      );end while

    (setq rigth tee)
    (setq tee nil)

    (setq left (mergesort left))
    (setq rigth (mergesort rigth))
    (setq t3 (merge left rigth))
    
   );end progn
 );end if
  )
;канец функціі
;___________________________________________

;прагледзіць базу
(defun c:baz_print (/ temp)
  (setq temp (kabel_baz))
  (foreach element temp
    (print element)
    ;(princ "\n")
    ;(princ (strcat element "\n"))
    (princ)
	   )
  (princ)
 )
;канец функціі
;___________________________________________

;каардынаты
(defun XY (temp1 temp2)
(list (+ x (* temp1 mash)) (+ y (* temp2 mash)))
)
;канец функціі
;___________________________________________

;вызначэнне каардынаты тэкста
(defun xytext(obj / list_nabor x1 x2 x list_pol)
  (setq list_nabor (entget obj ))

  (setq list_pol (vlax-ename->vla-object obj))
  	(vla-GetBoundingBox list_pol 'x1 'x2)
  (setq x1 (car (vlax-safearray->list x1)))
  (setq x2 (car (vlax-safearray->list x2)))
  (vla-GetBoundingBox list_pol 'y1 'y2)
   (setq y1 (car (cdr (vlax-safearray->list y1))))
  (setq y2 (car (cdr (vlax-safearray->list y2))))
	(setq x (/ (+ x1 x2) 2))
  (setq y (/ (+ y1 y2) 2))
  (setq point (list x y))

)
;канец функціі
;___________________________________________

;функція знішчэнне элемента са спісу
(defun delel (spis el)
  (if (/= (car spis) el)
    (append (list (car spis)) (delel spis el))
   )
  )
;канец функціі
;___________________________________________


;падрыхтоука спіс набору новы 
(defun spis_nabor_nov (nabor mash hiba del / len temp list_nabor i
		       temp_rez temp_e dakl imja1 imja2 x1 x2 y1 y2 xy
		       xxx proverka point p1 p2 priva)
  (setq len (sslength nabor) i -1 temp nil);даужыня спісу
  (setq dakl hiba)
  ;падрыхтоука спісу з адніх іменау і тэксту
  (repeat len
        (setq i (1+ i))		
	(setq list_nabor (entget (ssname nabor i)))
    (if (or (= (cdr (assoc 0 list_nabor )) "TEXT") (= (cdr (assoc 0 list_nabor )) "MTEXT"))
(if (= temp nil)
  (setq temp (list (ssname nabor i)))
  (setq temp (append temp (list (ssname nabor i))))
  );end if sklad spis
      );end if text
    );end repeat
  
  ;калі неадпаведная кольксць
;;;  (if (/= (rem (length temp) del) 0)
;;;    (progn
;;;      (princ "неадпаведная колькасць\n")
;;;      (princ)
;;;      (exit)
;;;     ));end if progn

  ;(setq len (length temp))
  (princ (strcat "Усяго=" (rtos (length temp) 2 0) "\n"))
  (princ)
  (princ (strcat "Сартаванне масіва можа заняць нейкі час.\n Калі ласка пачакайце.\n"))
  (princ)

  ;сартаванне
  (setq temp (mergesort temp))

;--------------------------------------------  
;внешні
;--------------------------------------------  
(while (/= temp nil)

;калі спіс вынікау пуст
;----------------------
(if (= nil temp_e)
  (progn
 (setq imja1 (car temp))
(setq temp (cdr temp))
(setq xy (xytext imja1)) 
(setq temp_e (list (list xy imja1)))
   );end progn
);


    ;праходзим па спису перад вынику
    ;кали спис прадвынику у колькасти перасоуваем у выник
    ;инакш правяраем и дадаем
(if (/= nil temp_e)
  (setq proverka T)
  (setq proverka nil)
  );end if

 ;прімітів на ввходе 
(setq imja1 (car temp))
(setq temp (cdr temp))
(setq xy (xytext imja1))  
(setq x1 (car xy) y1 (cadr xy))
(setq len (length temp_e) i 0)    

 ;проверяем прімітв по temp_e дадаем у спіс 
(while (and proverka (< i len))
(princ "\r        ")(princ)
(princ (strcat "\r*" (rtos (length temp) 2 0)))
(princ)
  
  (setq xxx (nth i temp_e))
  (setq i (1+ i))
  (if (= (length xxx) (+ del 1))
    (progn
      (setq temp_e (subst "zzz" xxx temp_e))
      (setq temp_e (subst (car temp_e) "zzz" temp_e))
      (setq temp_e (cdr temp_e))
      
      (if (= nil temp_rez)
	(setq temp_rez (list (cdr xxx)))
	(setq temp_rez (append temp_rez (list (cdr xxx))))
	);
      (setq len (1- len) i (1- i))
     );progn
  (progn
  (setq xy1 (car xxx))
  (setq x2 (car xy1) y2 (cadr xy1))
  (if (and (equal x1 x2 (* 350 mash)) (equal y1 y2 dakl))
    (progn
      (setq temp_e (subst (appends xxx imja1) xxx temp_e))
      (setq proverka nil)
    ));end if
    )); end if-progn
 );end while
   
;калі элемент не увайшоу-дадаем
(if (= proverka T)
  (setq temp_e (append temp_e (list (list xy imja1))))
   );end if
;---------------------    

(princ "\r        ")(princ)
(princ (strcat "\r" (rtos (length temp) 2 0)))
(princ)    
    
   )
  ;--------------------------------------------
  ;end while vneshni
  ;--------------------------------------------  

(setq i 0 proverka nil)

;
  
(if (/= temp_e nil)
  (progn
    (while (/= temp_e nil)
      (if (= (+ 1 del) (length (car temp_e)))
	(progn
	  (if (= temp_rez nil)
	    (progn
	    (setq temp_rez (list (cdar temp_e)))
	    (setq temp_e (cdr temp_e))
	    )
	    (progn
	    (setq temp_rez (append temp_rez (list (cdar temp_e))))
	    (setq temp_e (cdr temp_e))	      
	     )
	   )
	  );end progn
	(progn
	  (setq point (caar temp_e))
	  (setq x1 (car point) y1 (cadr point))
	  (setq p1 (list (- x1 (* 7 mash)) y1 )  p2 (list (+ x1 (* 7 mash)) y1 ))
	  (setq priva (getvar "OSMODE"))
  		(setvar "OSMODE" 1024)
	  (command "_line" p1 p2 "")
	  (setvar "OSMODE" priva)
	  (setq proverka T)
	  );end progn
	)
      );while
   )
 )
;выхад кали праверка неадпавежныя
  (if (= proverka T)
    (progn
	  (alert "Rолькасць неадпаведна,\n глядзіце закрэсленныя абъекты")
	  (exit)
	  )
   )
;выкид вынику  
(setq temp_rez temp_rez)  

)
;канец функціі
;___________________________________________

;спис трасс
(defun spis_tras(temp)
  (setq temp (vl-string-subst "," ";" (vl-string-trim " " temp)))
  (setq temp (vl-string-trim "," temp))
  
  (cons (vl-string-trim " " (substr temp 1 (vl-string-search "," temp))) (if (/= (vl-string-search "," temp) nil) (spis_tras (substr temp (+ (vl-string-search "," temp) 1) (strlen temp)))))
)
;канец функціі
;___________________________________________

;пераутварэнне у лик
(defun atofk(temp / temp1 temp2 poz)
  (setq temp (vl-string-subst "x" "х" (vl-string-trim " " temp)))
  (setq poz (vl-string-search "x" temp))
  (if (/= poz nil)
    (progn
      (setq temp1 (atofk (substr temp 1 poz)))
      (setq temp2 (atofk (substr temp (+ poz 2) (strlen temp))))
      (setq temp (* temp1 temp2))
      )
    (setq temp (atof (vl-string-subst "." "," temp)))
    )  
)
;канец функціі
;___________________________________________

;вызначенне сячэнне па кабелю
(defun sech_kabel(temp / poz1 poz2 poz rez kol)
  (setq temp (vl-string-subst "x" "х" temp))
  ;вызначаем колькасть кабелей
  (if (/= nil (vl-string-search "x(" temp))
    (progn
      (setq kol (substr temp (- (vl-string-search "x(" temp) 1) 2))
      (if (wcmatch kol "##")
	(setq kol (atoi kol))
	(if (wcmatch (substr kol 2 1) "#")
	  (setq kol (atoi (substr kol 2 1)))
	  (setq kol nil)
	 )
	)
      (setq temp (substr temp (+ 3 (vl-string-search "x(" temp))))
      (setq temp (substr temp 1 (vl-string-search ")" temp)))
      (setq temp (vl-string-subst "x" "х" temp))
   ))
  ;сканчэнне колькасці
  (setq temp (vl-string-subst "." "," temp))
  (while (/= temp nil)
    (setq poz (vl-string-search "x" temp))
    (if (not (and (wcmatch (substr temp poz 1) "#") (wcmatch (substr temp (+ poz 2) 1) "#")))
      (setq temp (substr temp (+ poz 2)))
      (progn
	(setq poz1 poz)
	(setq poz2 (+ 2 poz))
	(while (and (> poz1 0) (wcmatch (substr temp poz1 1) "#"))
	  (setq poz1 (- poz1 1))
	 );end while poz1
	(setq poz1 (+ poz1 1))
	
	(while (and (<= poz2 (strlen temp)) (or (wcmatch (substr temp poz2 1) "#") (= (substr temp poz2 1) ".") (= (substr temp poz2 1) "x") (= (substr temp poz2 1) "х")) )
	  (setq poz2 (+ poz2 1))
	 );end while poz2

	(setq rez (substr temp poz1 (- poz2 poz1)) temp nil)
       );end progn
      );end if
    );end while
  (setq rez (list kol rez))
  )
;канец функціі
;___________________________________________

;база кабелю
(defun kabel_baz(/ temp1 temp2 temp3 modul linestr spis_kab)
  (princ (strcat "База " dirpol "__prog/kabel_baz.txt\n")) (princ)
  (if (/= nil (setq modul (open (strcat dirpol "__prog/kabel_baz.txt") "r")))
  (progn
    (while (/= nil
	       (setq linestr (read-line modul))
	   )
      (if (/= (vl-string-trim " " linestr) "")
	(progn
      (setq temp1 nil temp2 nil temp3 nil)
      (setq linestr (VL-STRING-SUBST " " "\t" linestr))
      (setq linestr (vl-string-trim " " linestr))
      (setq temp1 (vl-string-trim " " (substr linestr 1 (vl-string-search " " linestr))))
      (setq linestr (vl-string-trim " " (setq linestr (substr linestr (+ (vl-string-search " " linestr) 1) (- (strlen linestr) (vl-string-search " " linestr))))))
      (setq temp2 (vl-string-trim " " (substr linestr 1 (vl-string-search " " linestr))))
      (setq linestr (vl-string-trim " " (setq linestr (substr linestr (+ (vl-string-search " " linestr) 1) (- (strlen linestr) (vl-string-search " " linestr))))))
      (setq temp3 (vl-string-trim " " linestr))
	    (if (= spis_kab nil)
	      (setq spis_kab (list (list (vl-string-subst "x" "х" temp1) (atofk temp2) (atofk temp3))))
	      (setq spis_kab (append spis_kab (list (list (vl-string-subst "x" "х" temp1) (atofk temp2) (atofk temp3)))))
	      )
      ));end if
          
  );end while
    (close modul)
    )
    );end if
  (setq spis_kab spis_kab )
  )
;канец функціі
;___________________________________________

;база кабелю запіс
(defun kabel_w(/ temp1 temp2 temp3 modul zapis)
  (if (/= nil (setq modul (open (strcat dirpol "__prog/kabel_baz.txt") "w")))
  (progn
    (while (/= nil spis_kab)
(setq zapis (car spis_kab) spis_kab (cdr spis_kab))
      (setq temp1 (car zapis) zapis (cdr zapis))
      (setq temp2 (car zapis) zapis (cdr zapis))
      (setq temp3 (car zapis))
      (write-line (strcat temp1 " " temp2 " " temp3) modul)
  );end while
    (close modul)
    )
    );end if
  )
;канец функціі
;___________________________________________

;дадаванне кабелю
(defun c:kabel_dob(/ temp temp1 temp2 temp3 spis_kab i)
  (setq spis_kab nil)
  (kabel_baz)
  (setq i 0 temp 1)
  (princ "дадаванне кабелю да базы\n
	 для выхаду -просты ўвод\n
	 ------------------------\n")
  (princ)
  (while (/= nil temp)
(if (= temp1 nil)
    (setq temp1 (vl-string-trim " " (getstring "Увядзіцце уцінак які дадаць(прыклад = 3х6): \n")))
    )
    (if (= temp1 "")
      (progn
	(setq temp1 (vl-string-trim " " (getstring "Уцінак або ўвод для выхаду: \n")))
	(if (= temp1 "") (setq temp nil))
       )
      (progn
	(if (= temp2 nil)
    (setq temp2 (vl-string-trim " " (getstring "Увядзіцце дыяметр мм: \n")))
    );end pod if
(if (= temp2 "")
      (progn
	(setq temp2 (vl-string-trim " " (getstring "дыяметр мм або ўвод для выхаду: \n")))
	(if (= temp2 "") (setq temp nil))
       )
  (progn
(if (= temp3 nil)
    (setq temp3 (vl-string-trim " " (getstring "Увядзіцце вагу 1 км: \n")))
  );end if
(if (= temp3 "")
      (progn
	(setq temp3 (vl-string-trim " " (getstring "вага або ўвод для выхаду: \n")))
	(if (= temp3 "") (setq temp nil))
	));end if
   )
  );end if temp2
       );end progn
      );end if
(if (and (/= temp nil) (/= temp1 nil) (/= temp1 "")
	 (/= temp2 nil) (/= temp2 "")
	 (/= temp2 nil) (/= temp2 ""))
(progn
  (setq temp1 (vl-string-subst "x" "х" temp1))
  (setq spis_kab (append spis_kab (list (list temp1 temp2 temp3)))
	temp1 nil temp2 nil temp3 nil))
  );end if
    
    );end while
  (kabel_w)
  (setq spis_kab nil)
  (princ "-------------------------------\n
	 у базу ўнесенны адпаведные змены\n
	 --------------------------------\n")
  (princ)
  )
;канец функціі
;___________________________________________


;дадавання трасс
(defun spis_tras_dob(temp temp1 kabel / tras_b tras_d info zam zam1
		     sech utin mas sech_m16 sech_b16 sech_k s1 s2 len_td kol)
(setq sech (sech_kabel kabel))
  (if (= (car sech) nil)
    (setq kol 1 sech (cadr sech))
    (setq kol (car sech) sech (cadr sech))
   )
(setq utin (atofk (substr sech (+ (vl-string-search "x" sech) 2) 10)))
(setq tras_d (spis_tras temp1) tras_b temp info (assoc sech kabel_baz1))
  (if (/= nil info)
    (progn
(setq s1 (* kol (/ (* (nth 1 info) (nth 1 info) 3.14) 4)))
(setq s2 (* kol (/ (* (nth 1 info) 2) 1000)) len_td (length tras_d))
  (repeat len_td 
    (if (= tras_b nil)
      (progn
	(if (< utin 16)
	  (setq mas (* kol (nth 2 info)) sech_m16 s1 sech_b16 0 sech_k 0)
	  (setq mas (* kol (nth 2 info)) sech_m16 0 sech_b16 s1 sech_k s2)
	  );if <16
	(setq tras_b (list (list (car tras_d) mas sech_m16 sech_b16 sech_k)))
       );end progn
      (progn
	(if (< utin 16)
	  (setq mas (* kol (nth 2 info)) sech_m16 s1 sech_b16 0 sech_k 0)
	  (setq mas (* kol (nth 2 info)) sech_m16 0 sech_b16 s1 sech_k s2)
	  );if <16
	
	(if (/= (setq zam (assoc (car tras_d) tras_b)) nil)
	  (progn
	    (setq zam1 (list (car zam)
			     (+ (nth 1 zam) mas)
			     (+ (nth 2 zam) sech_m16)
			     (+ (nth 3 zam) sech_b16)
			     (+ (nth 4 zam) sech_k)));setq
	    (setq tras_b (subst zam1 zam tras_b))
	   )
	  (setq tras_b (append tras_b (setq tras_b (list (list (car tras_d)
							 mas
							 sech_m16
							 sech_b16
							 sech_k)))))
	 )
       );progn
     );end if
    (setq tras_d (cdr tras_d))
  );end repeat
  );enf progn vnesh
    ;-----------------
    ;калі няма кабелю
    (progn
      (if (= kabel_net nil)
	  (setq kabel_net (list kabel))
	(progn
      (repeat (length kabel_net)
	    (if (= (member kabel kabel_net) nil)
	      (setq kabel_net (append kabel_net (list kabel)))
	      );end if
	    
       );end repeat
      ));end if-progn
     ));end progn if
  (setq tras_b tras_b)
)
;канец функціі
;___________________________________________

;апрацоука
(defun aprac(list_nabor / temp1 temp text_sum text_temp nabor_text len i)
(if (= (cdr (assoc 0 list_nabor)) "MTEXT")
  (progn
(command "_Explode" (cdr (assoc -1 list_nabor)))
(setq nabor_text (ssget "_p"))
(setq len (sslength nabor_text))
(setq i	-1 text_sum "")
(repeat len
(setq i (1+ i))
(setq text_temp (cdr (assoc 1 (entget (ssname nabor_text i)))))
(if (/= text_temp nil) (setq text_sum (strcat text_sum text_temp)))  
 )
(setq temp (vl-string-trim " " text_sum))
(command "_u")
) ;канец верх прогона
(setq temp (vl-string-trim " " (cdr (assoc 1 list_nabor))))
  );канец ифа

  (setq temp1 temp)

  )
;канец функціі
;___________________________________________


;галоуная функція апрацоуцы
(defun c:kabel(/ temp temp1 ks dazvol_tras i i1 simvol priva element
	       mash adl dlina_all kabel kabel_n mypoint sum dlina list_nabor
	       kabel_net kabel_tras_1 len_ks kabel_baz1 m l u n nabor1 num_k)
(vl-load-com)
  (princ "-------------------\n
  Зыходные дадзенныя дзе:\n
  М-марка кабелю | Н-напружанне\n
  Л(ч)-лік жыл | У(с)-уцінак\n
  Х-пусто | Д-даўжыня\n
  Т-трасіроўка
  К-тип и марка кабеля и інш.(=МЛУН)\n
  1- М-Н-Л(ч)-У(с)-Д (або просто увод)\n
  2- М-Н-Л(ч)-У(с)-Д-Т | 3- К-Д\n | 4 -Т-М-Л-Д | 5 -М-У-Д
  літары МЛУН складаюцца і утвараюць кабель,\n
  літары -гэта асобны текст\n
  ")
  (princ)
  (setq dazvol_tras T)
  (setq temp (vl-string-trim " " (getstring "Увядзіцце паслядоўнасць(або лічбу):")))
  
  (if (= temp "") (setq temp "М-Н-Л-У-Д"))
  ;(if (= temp "") (setq temp "М-Л-Х-Х-Х-Д-Т"))
  (setq temp (vl-string-subst "Л" "Ч" (strcase temp)))
  (setq temp (vl-string-subst "У" "С" (strcase temp)))
  (setq temp (strcase (vl-string-trim " " temp)))
  (if (= temp "1") (setq temp "М-Н-Л-У-Д"))
  (if (= temp "2") (setq temp "М-Н-Л-У-Д-Т"))
  (if (= temp "3") (setq temp "К-Д"))
  (if (= temp "4") (setq temp "Т-М-Л-Д"))
  (if (= temp "5") (setq temp "М-У-Д"))
  (princ (strcat temp "\n"))
  (princ)
  
  (setq temp1 (vl-string-trim " " (getstring "Адлегласць памиж радками(800):")))
  (if (= (vl-string-trim " " temp1) "") (setq temp1 "800"))
  (setq adl (atofk temp1))
  (cond
    ((and (> adl 5) (< adl 30)) (setq mash 1))
    ((> adl 30) (setq mash 100))
    (T (setq mash 0.01))
    )
  
  (princ (strcat temp1 "\n"))
  (princ)


  (setq i 1)
  (while (<= i (strlen temp))
    (setq simvol (substr temp i 1))
    (cond
      ((= simvol "Х") (setq ks (append ks '(0))));пропуск
      ((= simvol "М") (setq ks (append ks '(1))));марка кабеля
      ((= simvol "Л") (setq ks (append ks '(2))));лик жыл
      ((= simvol "У") (setq ks (append ks '(3))));уцинак жыл
      ((= simvol "Н") (setq ks (append ks '(4))));напружанне
      ((= simvol "К") (setq ks (append ks '(5))));кабель марка и уцинак
      ((= simvol "Д") (setq ks (append ks '(6))));даужыня
      ((= simvol "Т") (setq ks (append ks '(7))));трасироука
      )
    (setq i (1+ i))
    )
  (if (= ks nil) (exit))
(setq m "" l "" u "" n "");задаванне МЛУН
(setq i -1 i1 0)
 ;nabor------------ 
(setq nabor (ssget))
  ;-----------------
  (setq kabel_baz1 (kabel_baz))
  (if (or (= (sslength nabor) 0) (= (length ks) 0)) (exit))
  (setq len_ks (length ks))
;;;  (if (/= (rem (sslength nabor) (length ks)) 0)
;;;    (progn
;;;      (alert "лік аб'ектаў не цотна з колькасцю палёў апрацоўцы")
;;;      (exit)
;;;     )
;;;    )

(setq kabel_n nil);поуная назва кабелю

(setq nabor1 (spis_nabor_nov nabor mash (/ (- adl (* 2 mash)) 2) len_ks))
(setq len (length nabor1));даужыня спісу
;------------------------------------------------------------
  (repeat len
    (setq temp (car nabor1))
    (setq nabor1 (cdr nabor1) i1 0)
    
    (repeat len_ks
	(setq list_nabor (entget (car temp)))
        (setq temp (cdr temp))

  
  (setq num_k (nth i1 ks))
  (cond
    ((= num_k 1) (setq m (aprac list_nabor)))
    ((= num_k 2) (setq l (aprac list_nabor)));лик
    ((= num_k 3) (setq u (aprac list_nabor)));уцинак
    ((= num_k 4) (setq n (aprac list_nabor)))
    ((= num_k 5) (setq kabel_n (aprac list_nabor)))
    ((= num_k 6) (setq dlina (atofk (aprac list_nabor))));длинна
    ((= num_k 7) (setq spis_tras1 (aprac list_nabor)));трассы
    );end cond
  (setq i1 (1+ i1))
      
  (if (= len_ks i1)
(progn
(if (= kabel_n nil)
  (progn
;;;    (if (and (= nil l) (= nil u))
      
   (setq sech (strcat l "x" u))
   (if (/= u "") (setq u (strcat "x" u)))
   (if (/= l "") (setq l (strcat "-" l)))
   (if (/= n "") (setq n (strcat "-" n)))
   (setq kabel_n (strcat m l u n))
    )
  );end if
(if (/= nil (member 7 ks))
(setq kabel_tras_1 (spis_tras_dob kabel_tras_1 spis_tras1 kabel_n))
  )
;устаноука даужыни кабеля-падлик
(if (= nil dlina_all) 
   (setq dlina_all (list (list kabel_n dlina)))
  (if (= nil (assoc kabel_n dlina_all))
    (setq dlina_all  (append dlina_all (list (list kabel_n dlina))))
    (setq dlina_all (subst (list kabel_n (+ (car (cdr (assoc kabel_n dlina_all))) dlina)) (assoc kabel_n dlina_all) dlina_all))
    )
  );end if

(setq m "" l "" u "" n "" k_sech nil kabel_n nil dlina nil spis_tras1 nil)
		     (setq i1 -1)
		     (setq kabel_n nil)
));end if len_ks

  
    ));end repeatS

  (princ "--------------------------------\n") (princ)

					;рисование таблицы
(setq priva (getvar "OSMODE"))
(setvar "OSMODE" 1024)
  (if (/= dlina_all nil)
    (progn
(setq mypoint (getpoint "Увядзце кропку устаукі"))
(setq x	(car mypoint)
      y	(car (cdr mypoint))
)
(command "_line" mypoint (XY 70 0) "") (princ)
(princ "cccccccccc")
(entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 10 -6))
	       (cons 40 (* 3.5 mash)) (cons 1 "Марка кабеля")))
(entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 53 -6))
	       (cons 40 (* 3.5 mash)) (cons 1 "Длина")))  
(command "_line" (XY 0 -8) (XY 70 -8) "")
(setq x	(car (XY 0 -8))
      y	(car (cdr (XY 0 -8)))
)
(setq i 1 dlina nil sum 0)  
  (foreach element dlina_all
    (setq text1 (car element))
    (setq dlina (car (cdr element)))
    (setq text2 (rtos dlina 2 1))
    (if (and (= kabel_net nil) (/= kabel_baz1 nil) (/= nil (setq x_temp (assoc (cadr (sech_kabel text1)) kabel_baz1))))
      (progn
	(setq temp (nth 1 x_temp))
	(if (= nil (car (sech_kabel text1)))
	(setq sum (+ sum (* dlina temp PI)))
	  (setq sum (+ sum (* (car (sech_kabel text1)) (* dlina temp PI))))
	  )
       )
     );end if
        (entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 20 20))
	       (cons 40 (* 5 mash)) (cons 1 "Даўжыня/Трассы вылічаны аўтаматычна (КАЛІ ЛАСКА, ПЕРАЛІЧЫЦЕ ЎРУЧНУЮ!!!)")))
    
    (entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 3 (+ 2 (* i -8))))
	       (cons 40 (* 2.5 mash)) (cons 1 text1)))
    (entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 53 (+ 2 (* i -8))))
	       (cons 40 (* 2.5 mash)) (cons 1 text2)))
    (command "_line" (XY 0 (* -8 i)) (XY 70 (* -8 i)) "")
    (setq i (+ i 1))
    )
      (if (= kabel_net nil)
      (progn
    (entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 3 (+ 2 (* i -8))))
	       (cons 40 (* 2.5 mash)) (cons 1 (strcat "Огракс-ВВ=" (rtos (* 1.5 (/ sum 1000))) "кг"))))
    (command "_line" (XY 0 (* -8 i)) (XY 70 (* -8 i)) "")
	));end if progn

(setq i (- i 1))
(command "_line" (XY 0 8) (XY 0 (* -8 i)) "")
(command "_line" (XY 50 8) (XY 50 (* -8 i)) "")
(command "_line" (XY 70 8) (XY 70 (* -8 i)) "")

  (if (and (= kabel_net nil) (/= kabel_tras_1 nil))
    (progn

(command "_line" (XY 100 8) (XY 250 8) "")
(entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 105 2))
	       (cons 40 (* 3.5 mash)) (cons 1 "Трасса")))
(entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 126 2))
	       (cons 40 (* 3.5 mash)) (cons 1 "Сум. <16")))
(entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 165 2))
	       (cons 40 (* 3.5 mash)) (cons 1 "Сум. >16")))
(entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 200 2))
	       (cons 40 (* 3.5 mash)) (cons 1 "2Д >16(м)")))
(entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 235 2))
	       (cons 40 (* 3.5 mash)) (cons 1 "кг/1м")))  
(command "_line" (XY 100 0) (XY 250 0) "")

  (command "_line" (XY 100 8) (XY 100 0) "")
  (command "_line" (XY 123 8) (XY 123 0) "")
  (command "_line" (XY 250 8) (XY 250 0) "")

(command "_line" (XY 160 8) (XY 160 0) "")
(command "_line" (XY 196 8) (XY 196 0) "")
(command "_line" (XY 230 8) (XY 230 0) "")

(setq x	(car (XY 100 0))
      y	(car (cdr (XY 100 0)))
)

(setq i 1)
(foreach element kabel_tras_1
  
  (setq text1 (car element))

    (entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 3 (+ 2 (* i -8))))
	       (cons 40 (* 2.5 mash)) (cons 1 text1)))
    (entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 27 (+ 2 (* i -8))))
	       (cons 40 (* 2.5 mash)) (cons 1 (rtos (nth 2 element) 2 1))))
      (entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 62 (+ 2 (* i -8))))
	       (cons 40 (* 2.5 mash)) (cons 1 (rtos (nth 3 element) 2 1))))
  (command "_line" (XY 60 (* -8 (- i 1))) (XY 60 (* -8 i)) "")
      (entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 97 (+ 2 (* i -8))))
	       (cons 40 (* 2.5 mash)) (cons 1 (rtos (nth 4 element) 2 1))))
  (command "_line" (XY 96 (* -8 (- i 1))) (XY 96 (* -8 i)) "")
      (entmake (list '(0 . "TEXT") '(6 . "Continuous") (append (list 10) (XY 132 (+ 2 (* i -8))))
	       (cons 40 (* 2.5 mash)) (cons 1 (rtos (/ (nth 1 element) 1000) 2 4))))
  (command "_line" (XY 130 (* -8 (- i 1))) (XY 130 (* -8 i)) "")
    (command "_line" (XY 0 (* -8 i)) (XY 150 (* -8 i)) "")
  
  (command "_line" (XY 0 (* -8 (- i 1))) (XY 0 (* -8 i)) "")
  (command "_line" (XY 23 (* -8 (- i 1))) (XY 23 (* -8 i)) "")
  (command "_line" (XY 150 (* -8 (- i 1))) (XY 150 (* -8 i)) "")
  
(setq i (+ i 1))
  ;if
  (if (> i 25)
    (progn
 (setq i 1)
(setq x	(car (XY 200 0))
      y	(car (cdr (XY 200 0)))
)
    ));end progn if
  
	   );end foreach

));end if progn kabel_net
));end if progn  


(setvar "OSMODE" priva)

 ;друк кабелей якіх няма 
  (if (/= kabel_net nil)
    (progn
     (princ "для вызначэнне загрузкі трасс дадайце кабелі у базу\nз дапамогай каманды kabel_dob, або адразу ў файл базы (шлях глядзі вышэй)\nпрагледзіць базу baz_print\nняма кабелей:\n")
     (princ)
   (foreach element kabel_net
    (princ (strcat element "\n"))
    (princ)
	   )
     )
    )
  ;---

  
  (princ)
 )

(defun c:кабель ()
  (c:kabel)
 )

(defun c:лаиуд_kabel ()
  (c:kabel)
 )