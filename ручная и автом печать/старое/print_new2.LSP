;---- формы зліцця --------
(defun c:StartForm ()
(if (boundp 'add_edit) 
(command "new_edit") 
(startapp "__prog\\exe\\FormMerge\\FormMergeExe.exe")
)
)

;---- дадавання --------
(defun c:AddForm (/ kword)
(if (boundp 'add_edit)
(command "add_dwg") 
(progn 
(initget "Да Не _Yes No") 
(setq kword (getkword "\nРучны (або аутаматычны-не) друк? [Да/Не]")) 
(cond 
((= kword "Yes") (pri_new 1)) 
((= kword "No") (dil_spds_new 1 T)))
)))

;---------------------------функция хибы--------------------------------------------------
(Defun ff1 (dis11 dis22 x1 x2)
  (if (and (>= dis11 (- x1 3.0001)) (<= dis11 (+ x1 3.0001)) (>= dis22 (- x2 3.0001)) (<= dis22 (+ x2 3.0001)))
	   T nil)
  )

(Defun ff2 (dis11 dis22 x1 x2)
  (if (and (>= dis11 (- x1 0.0001)) (<= dis11 (+ x1 0.0001)) (>= dis22 (- x2 0.0001)) (<= dis22 (+ x2 0.0001)))
	   T nil)
  )

;-------------------функция праверки фарматау--------------------------------------------
(defun vformat ()
   (if (> dis1 dis2)
    (progn
      (SETQ list1 "А")
      (SETQ format
	     (COND
	       ((ff2 dis1 dis2 630 297) "А4х3 (630.00 x 297.00 мм)")
	       ((ff2 dis1 dis2 841 297) "А4х4 (841.00 x 297.00 мм)")
	       ((ff2 dis1 dis2 1051 297) "А4х5 (1051.00 x 297.00 мм)")
	       ((ff2 dis1 dis2 1261 297) "А4х6 (1261.00 x 297.00 мм)")
	       ((ff2 dis1 dis2 1471 297) "А4х7 (1471.00 x 297.00 мм)")
	       ((ff2 dis1 dis2 1682 297) "А4х8 (1682.00 x 297.00 мм)")
	       ((ff2 dis1 dis2 1892 297) "А4х9 (1892.00 x 297.00 мм)")
	       ((ff2 dis1 dis2 891 420) "А3х3 (891.00 x 420.00 мм)")
	       ((ff2 dis1 dis2 1189 420) "А3х4 (1189.00 x 420.00 мм)")
	       ((ff2 dis1 dis2 1486 420) "А3х5 (1486.00 x 420.00 мм)")
	       ((ff2 dis1 dis2 1783 420) "А3х6 (1783.00 x 420.00 мм)")
	       ((ff2 dis1 dis2 2080 420) "А3х7 (2080.00 x 420.00 мм)")
	       ((ff2 dis1 dis2 1261 594) "А2х3 (1261.00 x 594.00 мм)")
	       ((ff2 dis1 dis2 1682 594) "А2х4 (1682.00 x 594.00 мм)")
	       ((ff2 dis1 dis2 2102 594) "А2х5 (2102.00 x 594.00 мм)")
	       ((ff2 dis1 dis2 1783 841) "А1х3 (1783.00 x 841.00 мм)")
	       ((ff2 dis1 dis2 2378 841) "А1х4 (2378.00 x 841.00 мм)")
	       ((ff2 dis1 dis2 1682 1189) "А0х2 (1682.00 x 1189.00 мм)")
	       ((ff2 dis1 dis2 2523 1189) "А0х3 (2523.00 x 1189.00 мм)")
	       ((ff2 dis1 dis2 1189 841) "А0 (1189.00 x 841.00 мм)")
	       ((ff2 dis1 dis2 841 594) "А1 (841.00 x 594.00 мм)")
	       ((ff2 dis1 dis2 594 420) "А2 (594.00 x 420.00 мм)")
	       ((ff2 dis1 dis2 420 297) "А3 (420.00 x 297.00 мм)")
	       ((ff2 dis1 dis2 297 210) "А4 (297.00 x 210.00 мм)")
	       (T nil)
	     )
      )
    )					;то
    (progn
     (SETQ list1 "К")
      (SETQ format
	     (COND
	       ((ff2 dis2 dis1 630 297) "А4х3 (297.00 x 630.00 мм)")
	       ((ff2 dis2 dis1 841 297) "А4х4 (297.00 x 841.00 мм)")
	       ((ff2 dis2 dis1 1051 297) "А4х5 (297.00 x 1051.00 мм)")
	       ((ff2 dis2 dis1 1261 297) "А4х6 (297.00 x 1261.00 мм)")
	       ((ff2 dis2 dis1 1471 297) "А4х7 (297.00 x 1471.00 мм)")
	       ((ff2 dis2 dis1 1682 297) "А4х8 (297.00 x 1682.00 мм)")
	       ((ff2 dis2 dis1 1892 297) "А4х9 (297.00 x 1892.00 мм)")
	       ((ff2 dis2 dis1 891 420) "А3х3 (420.00 x 891.00 мм)")
	       ((ff2 dis2 dis1 1189 420) "А3х4 (420.00 x 1189.00 мм)")
	       ((ff2 dis2 dis1 1486 420) "А3х5 (420.00 x 1486.00 мм)")
	       ((ff2 dis2 dis1 1783 420) "А3х6 (420.00 x 1783.00 мм)")
	       ((ff2 dis2 dis1 2080 420) "А3х7 (420.00 x 2080.00 мм)")
	       ((ff2 dis2 dis1 1261 594) "А2х3 (594.00 x 1261.00 мм)")
	       ((ff2 dis2 dis1 1682 594) "А2х4 (594.00 x 1682.00 мм)")
	       ((ff2 dis2 dis1 2102 594) "А2х5 (594.00 x 2102.00 мм)")
	       ((ff2 dis2 dis1 1783 841) "А1х3 (841.00 x 1783.00 мм)")
	       ((ff2 dis2 dis1 2378 841) "А1х4 (841.00 x 2378.00 мм)")
	       ((ff2 dis2 dis1 1682 1189) "А0х2 (1189.00 x 1682.00 мм)")
	       ((ff2 dis2 dis1 2523 1189) "А0х3 (1189.00 x 2523.00 мм)")
	       ((ff2 dis2 dis1 1189 841) "А0 (841.00 x 1189.00 мм)")
	       ((ff2 dis2 dis1 841 594) "А1 (594.00 x 841.00 мм)")
	       ((ff2 dis2 dis1 594 420) "А2 (420.00 x 594.00 мм)")
	       ((ff2 dis2 dis1 420 297) "А3 (297.00 x 420.00 мм)")
	       ((ff2 dis2 dis1 297 210) "А4 (210.00 x 297.00 мм)")
	       (T nil)
	     )
      )
      ;;закрывает иначе
    )					;закрывает сам иф
  )					;закрывает верх иф

 (if (= format nil)
 (if (> dis1 dis2)
    (progn
      (SETQ list1 "А")
      (SETQ formatfalse T)
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
     (SETQ list1 "К")
     (SETQ formatfalse T)
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

;-------------------надрукаванне уручную--------------------------------------------------
(DEFUN c:pri (/ formatfalse cmd dis1 dis2 mashrus point1 point2 color struser fi0 list1 data data1)
  
  (SETQ formatfalse nil)
  ; праверка даты и выхад если надо
  (setq data (atoi (rtos (getvar "CDATE") 2 0)))
  (setq data1 (atoi (rtos (- (getvar "CDATE") (- (getvar "DATE") (getvar "TDUUPDATE"))) 2 0)))
;;;  (if (or (> data 20200101) (> data1 20200101))
;;;    (progn
;;;      (alert "Тэрмін дзеянне праграммы скончыуся 01-05-2018\nзвярніцесь да вытворцы(за больш актуальнай версіяй)")
;;;      (exit)
;;;     )
;;;  )
  ;енд
  (PRINC "\n---------------------------------------------------------------------------\n")
  (PRINC "Праграмма распрацавана на lisp, prajdziswet-ам (Косаў Уладзімір) у 2014 годзе\n")
  (PRINC "-----------------------------------------------------------------------------\n")
  (PRINC "\n")
  (SETQ cmd (GETVAR "cmdecho"))
  (SETVAR "cmdecho" 1)
  (SETQ point1 (GETPOINT "Першая кропка рамкі: "))
  (PRINC point1)
  (SETQ point2 (GETCORNER point1 "\nДругая кропка рамкі(па ўскосіне): "))
  (PRINC point2)
  (PRINC "\n")
  (SETQ dis1 (ABS (- (CAR point1) (CAR point2))))
  (SETQ dis2 (ABS (- (CADR point2) (CADR point1))))
  
;;вызначэнне маштабу 1-1 або 1-100
  (if (> dis1 dis2)
    (if (< dis1 27000)
      (SETQ mashrus 1)
      (progn
      (SETQ dis1 (/ dis1 100))
      (SETQ dis2 (/ dis2 100))
      (SETQ mashrus 0.01)))
    (if (< dis2 27000)
      (SETQ mashrus 1)
      (progn
      (SETQ dis1 (/ dis1 100))
      (SETQ dis2 (/ dis2 100))
      (SETQ mashrus 0.01)))
    )
  
(if (and (/= mash1 0) (/= mash1 nil))
	 (setq mashrus mash1)
  )
  
(vformat)
  
 (if (and (/= format nil) (/= (GETVAR "LOCALE") "ENG"))
 (progn
  (if (= formatfalse T) (alert "Фармат рысунка адрозніваецца менш чым 3 мм \n(або менш 300 мм для 100 маштабу).\nНа запыт назвы рысунка адкажыце на пытанне
або націсніце \"Esc\"-каб адмовіцца ад друку"))

  ;задаванне назова малюнка
  (if (= AddDwgBool nil)
  (SETQ struser (GETSTRING T "Назва рысунка pdf (увядзіцце калі ласка імя)\nнацісніце проста ўвод або 00 - каб атрымать\nназоў бягучага рысунка:\nп.с для другу каляровым дадайце + наперадзе"))
  (setq struser (strcat (if (= colordwg 1) (princ "+") (princ "")) namememory))
    )
    (if (wcmatch struser "+*")
    (progn
      (setq struser (SUBSTR struser 2))
      (setq color "acad")
      )
    (setq color "monochrome.ctb")
   )
  (COND ((and (= (STRLEN struser) 2) (wcmatch struser "00*"))
	 (SETQ fileuser (SUBSTR (GETVAR "dwgname") 1 (- (STRLEN (GETVAR "dwgname")) 4))) )
	((and (> (STRLEN struser) 2) (wcmatch struser "00*"))
	 (SETQ fileuser (STRCAT (SUBSTR (GETVAR "dwgname") 1 (- (STRLEN (GETVAR "dwgname")) 4))
	  (SUBSTR struser 3 (- (STRLEN struser) 2)))))
	((= struser "")
	 (SETQ fileuser (SUBSTR (GETVAR "dwgname") 1 (- (STRLEN (GETVAR "dwgname")) 4))))
	(T (SETQ fileuser struser))
	)

  (SETQ fileuser (STRCAT (GETVAR "dwgprefix") fileuser ".pdf"))

  ;закрытие файла если он открыт тут (на этом компе
  (if (= (setq fi0 (open fileuser "w")) nil)
    (exit_pdf)
    (close fi0)
    )
  
  (if (= (setq fi0 (open fileuser "w")) nil)
    (alert "Файл ужо адчыненны іншай праграмай,\nабо немагчыма запісаць у гэты каталог (няма праў)")
(progn
  (close fi0)
  (if (or (wcmatch format "А0х2*") (wcmatch format "А0х3*")) (alert "Даннй формат(А0х2 или А0х2) будет перевен в pdf,\n но технически его невозможно будет распечатать"))
  (vl-file-delete fileuser)
  (SETVAR "cmdecho" 0)
  (if (= (GETVAR "tilemode") 0)
  (command "_plot"		     	;Сама команда	
	     "_y"		      	;Выполнить детальное задание конфигурации?:	
	     ""			     	;Имя листа или <печать>:	
	     "_DWG в PDF ЭТО.pc3"     	;Имя устройства вывода:	
	     format			;Формат листа бумаги
	     "_Millimeters"		;;Единицы измерения размеров листа :
	     list1			;;Ориентация чертежа [Книжная/Альбомная]: 
	     "_N"			;;Перевернуть чертеж? :
	     "Рамка"			;;Печатаемая область [Экран/Границы/Лист/Вид/Рамка]:
	     point1			;;першая коордыната
	     point2			;;другая коордяната
	     mashrus			;;Масштаб печати :
	     "_center"			;;Смещение от начала(x,y)или[Центрировать]:
	     "_y"			;;Учитывать стили печати? [Да/Нет]:
	     color 			;;Имя таблицы стилей печати: 
	     "_y"			;;Учитывать веса линий? [Да/Нет]:
	     "_N"			;;Масштабировать веса линий?:
	     "_N"			;;Печатать объекты листа первыми?:
	     "_N"			;;Скрывать объекты листа?:
	     fileuser			;;Введите имя файла : 
	     "_n"			;;Сохранить изменения параметров листа
	     "_y"			;;Перейти к печати [Да/Нет]
	    )
    ;;калі модель
      (command "_plot"		     	;Сама команда	
	     "_y"		      	;Выполнить детальное задание конфигурации?:	
	     ""			     	;Имя листа или <печать>:	
	     "_DWG в PDF ЭТО.pc3"     	;Имя устройства вывода:	
	     format			;Формат листа бумаги
	     "_Millimeters"		;;Единицы измерения размеров листа :
	     list1			;;Ориентация чертежа [Книжная/Альбомная]: 
	     "_N"			;;Перевернуть чертеж? :
	     "Рамка"			;;Печатаемая область [Экран/Границы/Лист/Вид/Рамка]:
	     point1			;;першая коордыната
	     point2			;;другая коордяната
	     mashrus			;;Масштаб печати :
	     "_center"			;;Смещение от начала(x,y)или[Центрировать]:
	     "_y"			;;Учитывать стили печати? [Да/Нет]:
	     color 			;;Имя таблицы стилей печати: 
	     "_y"			;;Учитывать веса линий? [Да/Нет]:
	     "_N"			;;Режим вывода тонированных ВЭ:
	     fileuser			;;Введите имя файла : 
	     "_n"			;;Сохранить изменения параметров листа
	     "_y"			;;Перейти к печати [Да/Нет]
	    )
    
    )
    (SETVAR "cmdecho" cmd)
  (setq printtrue T)
    (princ(STRCAT "Файл захаваны:" fileuser))
    (PRINC "\n-----------------------------------------------------------\n")
    (princ)
    )
    )
  )
   (alert "Недакладны формат - \nтакога фармату неіснуе або памылка\n(праграмма дзенічае ў маштабе 1:1\n або 1:100 і ў русскай версіі аўтакада)..."))
  (if (= printtrue T)
    (progn
	(exit_pdf)
	  (if (OR (= nil sufix) (and (/= nil sufix) (/= (substr sufix 1 1) "r")))
	    (alert "пдф-нанадрукован, глядзі у тэчке")
	   )
      ));end if
)

(DEFUN c:зкш ()
(c:pri)
  )

;--------------------------функция перадачы праз разлучонную памяць--------------------------------------------
(defun pri_new (AddDwgBool / temp_start sufix filereturn filename namememory
		dcl_id ddi colordwg printtrue mash1 spislist mod vers proc)
  ;---------------------------------------------------------------------- 
(setq temp_start T filename (SUBSTR (GETVAR "dwgname") 1 (- (STRLEN (GETVAR "dwgname")) 4)) mash1 0)
(setq i 1)
  (vl-load-com)
  (setq mod (getvar "ctab"))
  (setq spislist (append '("Model") (layoutlist)))
  (setq listpp (vl-position mod spislist));
  ;while
  (while temp_start
    ;dialog  d:\\Косов\\prog\\_Програм\\_acad\\для панельки\
    (setq dcl_id (load_dialog "f:\\Mega\\_Програм\\_acad\\для панельки\\ручная и автом печать\\print_new1.DCL"))
  (if (not (new_dialog "printnew" dcl_id))
    (exit)
  )
  (setq pp nil)
  (set_tile "mash" (rtos mash1)) 
  (action_tile "btn1" "(setq listpp (1+ listpp) pp T) (done_dialog 2)")  
  (action_tile "accept" "(setq mash1 (atof (get_tile \"mash\"))) (setq colordwg (atoi (get_tile \"colordwg\"))) (done_dialog 1)")
  (action_tile "cancel" "(setq temp_start nil) (done_dialog 0)")
  (setq ddi (start_dialog))
  (unload_dialog dcl_id)
  ;end dialog
    (if (>= listpp (length spislist)) (setq listpp 0))
    (if (= pp T) (setvar "ctab" (setq mod (nth listpp spislist))))
    (if (and (= temp_start T) (/= pp T))
      (progn
    (setq sufix (strcat "r" (itoa i)) printtrue nil)
    (setq namememory (strcat filename sufix))
    (setq i (1+ i))
    (c:pri)
    (if (/= printtrue nil)
      (exit_pdf)
      )
    ;вызов діалога
  
    (if (/= filereturn nil)
      (if (/= printtrue nil)
	(setq filereturn (strcat filereturn "?" namememory ".pdf"))
       )
            (if (/= printtrue nil)
	(setq filereturn (strcat namememory ".pdf"))
       )
     )
    ));end if
    
    );end while


  (if (/= filereturn nil)
    (progn
  (SETQ filereturn (STRCAT (GETVAR "dwgprefix") "&false&true&" filereturn ))

  (alert "пдф-наlнадрукаваны, праверце тэчку")
  ;адпраука файлау у редактор
  (if (boundp 'add_edit)
  (add_edit AddDwgBool filereturn)
  (progn
    (startapp "__prog\\exe\\FormMerge\\FormMergeExe.exe" (strcat "\"" filereturn "\""))
    (command)
    )
  );vers
  
  ));end if
  (setq AddDwgBool nil)
  
  )

;выхад з пдф
(defun exit_pdf ()
  (startapp "__prog\\exe\\pdf_exit.exe")
  (command)
);выхад з пдф


