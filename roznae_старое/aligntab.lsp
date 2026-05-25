(defun times (/ thh tmm tss strok2 time1)

  (setq time1 (substr (rtos (- (getvar "cdate") (fix (getvar "cdate"))) 2 6) 3))


  (setq thh (substr time1 1 2)

        tmm (substr time1 3 2)

        tss (substr time1 5 2)

  )

  (if (= (strlen tss) 1)
    (setq tss (strcat tss "0"))
    )

  (setq strok2 (strcat thh ":" tmm ":" tss))

)
(defun lentime (temp1 temp2 / ch mi se ch1 mi1 se1 ch2 mi2 se2)
  (setq ch2 (atoi (substr temp2 1 2)) mi2 (atoi (substr temp2 4 2)) se2 (atoi (substr temp2 7 2)))
  (setq ch1 (atoi (substr temp1 1 2)) mi1 (atoi (substr temp1 4 2)) se1 (atoi (substr temp1 7 2)))

  (if (>= ch2 ch1)
    (setq ch (- ch2 ch1))
    (setq ch (+ (- 24 ch1) ch2))
    )
  (if (>= mi2 mi1)
    (setq mi (- mi2 mi1))
    (setq mi (+ (- 60 mi1) mi2) ch (- ch 1))
    )
  (if (>= se2 se1)
    (setq se (- se2 se1))
    (setq se (+ (- 60 se1) se2) mi (- mi 1))
    )
  (if (>= se 60)
    (setq se (- 60 se) mi (+ mi 1))
   )
  (if (<= se 9)
    (setq se (strcat "0" (rtos se 2 0)))
    (setq se (rtos se 2 0))
    )
  (if (>= mi 60)
    (setq mi (- 60 mi) ch (+ ch 1))
   )
  (if (<= mi 9)
    (setq mi (strcat "0" (rtos mi 2 0)))
    (setq mi (rtos mi 2 0))
    )
  (if (<= ch 9)
    (setq ch (strcat "0" (rtos ch 2 0)))
    (setq ch (rtos ch 2 0))
    )
  
  (setq se (strcat ch ":" mi ":" se))
  
      )

;сложение времен
(defun addtime (temp1 temp2 / ch mi se ch1 mi1 se1 ch2 mi2 se2)
  (setq ch2 (atoi (substr temp2 1 2)) mi2 (atoi (substr temp2 4 2)) se2 (atoi (substr temp2 7 2)))
  (setq ch1 (atoi (substr temp1 1 2)) mi1 (atoi (substr temp1 4 2)) se1 (atoi (substr temp1 7 2)))

  (setq se (+ se1 se2) mi (+ mi1 mi2) ch (+ ch1 ch2))
  (if (>= se 60)
    (setq mi (+ mi 1) se (- se 60))
   )
  (if (>= mi 60)
    (setq ch (+ ch 1) mi (- mi 60))
   )
    (if (<= se 9)
    (setq se (strcat "0" (rtos se)))
    (setq se (rtos se))
    )
    (if (<= mi 9)
    (setq mi (strcat "0" (rtos mi)))
    (setq mi (rtos mi))
    )
  (if (<= ch 9)
    (setq ch (strcat "0" (rtos ch)))
    (setq ch (rtos ch))
    )
  
  (setq se (strcat ch ":" mi ":" se))
  )

;получение координат
(defun active_viewport
      (/ vCen vHei vPix vWid lBot rTop)
 (setq vCen(getvar "VIEWCTR")
  vHei(getvar "VIEWSIZE")
  vPix(getvar "SCREENSIZE")
  vWid(* vHei(/(car vPix)(cadr vPix)))
  lBot(list(-(car vCen)(/ vWid 2))
      (+(cadr vCen)(/ vHei 2))
      ); end list
  rTop(list(+(car vCen)(/ vWid 2))
      (-(cadr vCen)(/ vHei 2))
      ); end list
  ); end setq
 (list lBot rTop)
 ); end active_viewport_coners

;адшуканне кропак і 
(defun pointpoline (temp / object tmp1 xmax xmin ymax ymin)
  (setq object (entget temp))
  (while (/= object nil)
    (setq tmp1 (car object))
    (setq object (cdr object))
    (if (= (car tmp1) 10)
      (progn
	(if (OR (= xmax nil) (< xmax (nth 1 tmp1)))
		(setq xmax (nth 1 tmp1)))
	(if (OR (= xmin nil) (> xmin (nth 1 tmp1)))
		(setq xmin (nth 1 tmp1)))
	(if (OR (= ymax nil) (< ymax (nth 2 tmp1)))
		(setq ymax (nth 2 tmp1)))
	(if (OR (= ymin nil) (> ymin (nth 2 tmp1)))
		(setq ymin (nth 2 tmp1)))
     ))
   )
  (setq object (list (list xmin ymin) (list xmax ymax)))
 )
;--------------------------------------------------------
(defun deleteassoc (spis tmp1 tmp2)
  (if (and (= (type (car spis)) (type '(1 2))) (or (/= (caar spis) tmp1) (/= (caar spis) tmp2)))
    (setq spis (append (list (car spis)) (deleteassoc (cdr spis) tmp1 tmp2)))
    )
  )
;--------------------------------------------------------


(defun c:aligntab ( / nabor name i len nabor_new p h mash pointw timetemp)
  (setq nabor (ssget))
  (print (setq timetemp (times))) (princ)
  (setq len (sslength nabor))
  (setq pri (getvar "osmode"))
  (setvar "osmode" 0)
  (if (and (/= nil len)(> len 0))
    (progn
      (setq pointw (active_viewport))
      ;(vl-cmdf "_zoom" "_A")
      (setq i 0)
  (while (< i len)
    (if (= nabor_new nil)
      ;утварэнне спісу
    (setq nabor_new (list (ssname nabor i)))
      (setq nabor_new (append nabor_new (list (ssname nabor i))))
      )
    (setq i (+ 1 i))
  )
      (setq nabor nil)
      
      (initget "Налева Строга Пасярэдзіне нАправа")
            (setq optans
              (getkword 
                "\nВыраўноўваць: [Налева/Строга налева/Пасярэдзіне/нАправа] < >: ")
            )
      (cond
	((= optans "Строга") (setq p 2))
	((= optans "Пасярэдзіне") (setq p 3))
	((= optans "нАправа") (setq p 4))
	(T (setq p 1))
       )
      ;проход по спісу
	  (setq i 0)
	  (if (/= nabor_new nil) (setq len (length nabor_new)))
  (while (/= nabor_new nil)
     (princ (strcat "\r" (itoa i) "//" (itoa len)))(princ)

	 (setq i (+ 1 i))
    (setq name (entget (car nabor_new)))
    (setq nabor_new (cdr nabor_new))
    (if (OR (= "TEXT" (cdr (assoc 0 name))) (= "MTEXT" (cdr (assoc 0 name))))
      (progn
	  (setq h (cdr (assoc 40 name)))
  	  (if (> h 200) (setq mash 100) (setq mash 1))
	  (fun)
     ));end if
   );end while

      (vl-cmdf "_zoom" "_W" (car pointw) (cadr pointw))
);progn
);if
  (setvar "osmode" pri)
  (print (strcat "Час пачатку:" timetemp)) (print)
  (princ (strcat "Час сканчэнне:" (times))) (princ)
  (print (strcat "Працягласць: " (lentime timetemp (times)))) (princ)
    (gc)
    (command "_purge" "_a" "" "_n" )
    (command "_audit" "_y")
)

;(acet-geom-textbox (entget (car (entsel))) 0)
;(textbox (entget (car (entsel))))
;;;(textbox (entget (ssname (ssget) 0)))
;((40 . 250.0) (1 . "r ajnjlfnxrbe")(7 . "GOST 2.304"))
;;;  (setq tt (entsel))
;;;(acet-geom-textbox (list) 0)
;;;(setq tex (substr (cdr (assoc 1 tt)) 1 (vl-string-search "\P" (cdr (assoc 1 tt)))))
;;;(setq tt (subst (cons 1 tex) (assoc 1 tt) tt))
;;;(acet-geom-textbox tt 0)
;;;(setq p1 (nth 0 (acet-geom-textbox tt 0)) p2 (nth 2 (acet-geom-textbox tt 0)))
;;;(acet-geom-textbox (list (assoc 10 tt) (assoc 40 tt) (assoc 1 tt) (assoc 7 tt)))
  
(defun fun (/ last1 last2 temp p1 p2 p3 nameobject point spisdel htext ht kofmtext)

(setq nameobject (cdr (assoc 0 name)) point (cdr (assoc 10 name)))
;определяем точки прямоугольников
  (if (= "TEXT" nameobject)
    (progn
    (setq temp (textbox name) point (cdr (assoc 10 name)))
    (setq p1 (list (- (nth 0 point) (nth 0 (car temp))) (+ (nth 1 point) (nth 1 (car temp)))))
    (setq p2 (list (+ (nth 0 point) (nth 0 (cadr temp))) (+ (nth 1 point) (nth 1 (cadr temp)))))
    (setq p3 (list (/ (+ (car p1) (car p2)) 2) (/ (+ (cadr p1) (cadr p2)) 2)))
    (setq temp (list p1 p2 p3))
    (vl-cmdf "_zoom" "_W" (list (- (car p3) (* mash 130)) (+ (cadr p3) (* mash 8)))
	     (list (+ (car p3) (* mash 130)) (- (cadr p3) (* mash 8))))
    )
    (if (= "MTEXT" nameobject)
      (progn
       (setq temp (acet-geom-textbox name 0))
       (vl-cmdf "_zoom" "_W"  (list (- (nth 0 (nth 0 temp)) (* 130 mash))
				    (+ (nth 1 (nth 0 temp)) (* 8 mash)))
		(list (+ (nth 0 (nth 2 temp)) (* 130 mash))
				    (- (nth 1 (nth 2 temp)) (* 8 mash))))
       (if (wcmatch (cdr (assoc 1 name)) "*\\S*")
	 (progn
	   (setq ht (textbox (list (assoc 40 name) '(1 . "r ajnjlfnxrbe") (assoc 7 name))))
	   ;величина шрифта
	   (setq ht (- (nth 1 (nth 1 ht)) (nth 1 (nth 0 ht))))
	   ;выличваем коэф при дроби
	   (setq htext (substr (cdr (assoc 1 name)) 1 (vl-string-search "x;\\S" (cdr (assoc 1 name)))))
	   (while (wcmatch htext "*{*")
	     (setq htext (substr htext (+ 2 (vl-string-search "{" htext))))
	     )
	   (setq htext (substr htext (+ 2 (vl-string-search "H" htext))))
	   (setq htext (atof htext))
	   (setq ht (* ht htext 2))
	   (setq p1 (list (nth 0 (nth 3 temp)) (- (nth 1 (nth 3 temp)) ht)))
	   (setq p2 (nth 2 temp))
	   (setq p3 (list (/ (+ (car p1) (car p2)) 2) (/ (+ (cadr p1) (cadr p2)) 2)))
	  )
	 (progn
	   (setq ht (textbox (list (assoc 40 name) (assoc 1 name) (assoc 7 name))))
	   (setq ht (nth 1 (nth 1 ht)))
	   (setq p1 (list (nth 0 (nth 3 temp)) (- (nth 1 (nth 3 temp)) ht)))
	   (setq p2 (nth 2 temp))
	   (setq p3 (list (/ (+ (car p1) (car p2)) 2) (/ (+ (cadr p1) (cadr p2)) 2))) 
	  )
	 )
   )))

(setvar "HPISLANDDETECTIONMODE" 0)
(setq last1 (entlast))  
(command "_BOUNDARY" p3 "")
(setq last2 (entnext last1))
(if (and (/= nil last2) (/= last1 last2))
  (progn
   (setq pp1 (pointpoline last2))
   (entdel last2)
  ))  
;(command "_point" p3) 
(setvar "HPISLANDDETECTIONMODE" 1)  

(if (/= nil pp1)
  ;утварэнне спису
 (cond
   ;выраунованне левый бок
   ((= p 1)
    (if (= "TEXT" nameobject)
    (progn
      (setq name (subst (cons 72 0) (assoc 72 name) name))
      (setq name (subst (cons 73 0) (assoc 73 name) name))
      (setq name (subst (cons 1 (vl-string-trim " "(cdr (assoc 1 name)))) (assoc 1 name) name))
      (if (< (nth 1 (assoc 10 name)) (+ (caar pp1) (* 0.65 (- (caadr pp1) (caar pp1)))))
	(progn
      (setq name (subst (list 10 (+ (caar pp1) (* 1.5 mash)) (+ (cadar pp1) (* 1.5 mash))) (assoc 10 name) name))
      (entmod name)
      )
	(progn
	  (setq name (subst (cons 72 2) (assoc 72 name) name))
	  (setq name (subst (list 11 (- (caadr pp1) (* 1.5 mash)) (+ (cadar pp1) (* 1.5 mash))) (assoc 11 name) name))
      	(entmod name)
	  ));end if
     )
      ;mtext
      (progn
      (setq name (subst (cons 71 1) (assoc 71 name) name))
      (if (wcmatch (cdr (assoc 1 name)) "*\\S*")
	(setq kofmtext (/ (- (nth 1 (nth 1 pp1)) (nth 1 (nth 0 pp1)) ht) 2))
	(setq kofmtext (* 1.5 mash)) 
	)
            (if (< (nth 1 (assoc 10 name)) (+ (caar pp1) (* 0.65 (- (caadr pp1) (caar pp1)))))
	(progn
      (setq name (subst (list 10 (+ (caar pp1) kofmtext) (+ (cadar pp1) kofmtext ht)) (assoc 10 name) name))
      (entmod name)
      )
	(progn
	  (setq name (subst (cons 71 3) (assoc 71 name) name))
      (setq name (subst (list 10 (- (caar pp1) kofmtext) (+ (cadar pp1) mash ht)) (assoc 10 name) name))
      (entmod name)
	  ));end if
       )
    );выравнивание нестрогое левый бок
  );выравнивание строгое левый бок
   ((= p 2)
    (if (= "TEXT" nameobject)
    (progn
      (setq name (subst (cons 72 0) (assoc 72 name) name))
      (setq name (subst (cons 73 0) (assoc 73 name) name))
      (setq name (subst (list 10 (+ (caar pp1) (* 1.5 mash)) (+ (cadar pp1) (* 1.5 mash))) (assoc 10 name) name))
      (entmod name)
     )
      ;mtext
      (progn
	      (if (wcmatch (cdr (assoc 1 name)) "*\\S*")
	(setq kofmtext (/ (- (nth 1 (nth 1 pp1)) (nth 1 (nth 0 pp1)) ht) 2))
	(setq kofmtext (* 1.5 mash)) 
	)
      (setq name (subst (cons 71 1) (assoc 71 name) name))
      (setq nameobject (subst (list 10 (+ (caar pp1) (* 1.5 mash)) (+ (cadar pp1) kofmtext ht)) (assoc 10 name) name))
      (entmod nameobject)
       )
    );канец ифа
  );выравнивание строгое левый бок
   ;выравнивание середины
   ((= p 3)
    (if (= "TEXT" nameobject)
    (progn
      (setq name (subst (cons 72 1) (assoc 72 name) name))
      (setq name (subst (cons 73 0) (assoc 73 name) name))
      (setq name (subst (list 11 (/ (+ (caar pp1) (caadr pp1)) 2) (+ (cadar pp1) (* 1.5 mash))) (assoc 11 name) name))
      (entmod name)
     )
      ;mtext
      (progn
	(if (wcmatch (cdr (assoc 1 name)) "*\\S*")
	(setq kofmtext (/ (- (nth 1 (nth 1 pp1)) (nth 1 (nth 0 pp1)) ht) 2))
	(setq kofmtext (* 1.5 mash)) 
	)
      (setq name (subst (cons 71 2) (assoc 71 name) name))
      (setq name (subst (list 10 (/ (+ (caar pp1) (caadr pp1)) 2) (+ (cadar pp1) kofmtext ht)) (assoc 10 name) name))
      (entmod name)
       )
    );канец ифа
  );выравнивание середины
      ;выравнивание вправо
   ((= p 4)
    (if (= "TEXT" nameobject)
    (progn
      (setq name (subst (cons 72 2) (assoc 72 name) name))
      (setq name (subst (cons 73 0) (assoc 73 name) name))
      (setq name (subst (list 11 (- (caadr pp1) (* 1.5 mash)) (+ (cadar pp1) (* 1.5 mash))) (assoc 11 name) name))
      (entmod name)
     )
      ;mtext
      (progn
		(if (wcmatch (cdr (assoc 1 name)) "*\\S*")
	(setq kofmtext (/ (- (nth 1 (nth 1 pp1)) (nth 1 (nth 0 pp1)) ht) 2))
	(setq kofmtext (* 1.5 mash)) 
	)
      (setq name (subst (cons 71 3) (assoc 71 name) name))
      (setq name (subst (list 10 (- (caadr pp1) (* 1.5 mash)) (+ (cadar pp1) kofmtext ht)) (assoc 10 name) name))
      (entmod name)
       )
    );канец ифа
  );выравнивание вправо
    );end cond
);end if
  ) 
;;;(while (/= nil (setq last2 (entnext last2)))
;;;  (if (= nil spisdel)
;;;    (setq spisdel (list last2))
;;;    (setq spisdel (append spisdel (list last2)))
;;;   )
;;; )
;;;
;;;  (cond
;;;    ((and (/= nil spisdel) (= 1 (length spisdel)))
;;;     )
;;;   )





  