(setvar "CMDECHO" 0)
(print (strcat "_______________________Загрузка панелькі і праграмм_______________________"))
;------------------------1 определение пути------------------------
(setq temp (getvar "ACADPREFIX") verauto nil verauto1 nil)

;вызначенне версии автокад
(setq temp2 (strcase temp))
(setq coun1 0 coun2 0)
(if (vl-string-search "AUTOCAD" temp2)
  (while (and (/= temp2 nil) (vl-string-search "AUTOCAD" temp2))
     (setq temp2 (substr temp2 (vl-string-search "AUTOCAD" temp2)))
    (if (> (strlen temp2) 13)
      (if (= verauto nil)
	(progn
	  (setq verauto (atoi (substr temp2 10 4)))
	  (setq temp2 (substr temp2 13))
	  (setq coun1 (+ 1 coun1))
	  )
	(if (= verauto (atoi (substr temp2 10 4)))
	  (progn
	    (setq coun1 (+ 1 coun1))
	    (setq temp2 (substr temp2 13))
	   )
	  (if (and (= verauto1 nil) (/= 0 (atoi (substr temp2 10 4))))
	(progn
	  (setq verauto1 (atoi (substr temp2 10 4)))
	  (setq temp2 (substr temp2 13))
	  (setq coun2 (+ 1 coun2))
	  )
	    (if (= verauto1 (atoi (substr temp2 10 4)))
	      (progn
	    (setq coun1 (+ 1 coun1))
	    (setq temp2 (substr temp2 13))
	   )
       (setq temp2 (substr temp2 13))
        ))
	   )
	 )
      (setq temp2 nil)
     );end if
    ))
(if (<= coun1 coun2)
  (if (>= verauto1 2007) (setq verauto verauto1 verauto1 nil temp2 nil coun1 nil coun2 nil))
  (setq verauto1 nil temp2 nil coun1 nil coun2 nil)
  )
;вызначэнне бітності
(if (> (strlen (vl-prin1-to-string (vlax-get-acad-object))) 40)
   (setq bitauto "64")
   (setq bitauto "32")
   )
;------------------------------------------------------------------------------------------------

;------------------------2 определение пути загрузкі------------------------
(while (not (wcmatch (substr temp 1 (vl-string-search ";" temp 0)) "*Users*upport*"))
  (setq temp (substr temp (+ 1 (vl-string-search ";" temp 0)) (- (strlen temp) (+ 1 (vl-string-search ";" temp 0)))))
 )

(setq dirpol (strcat (substr temp 1 (vl-string-search ";" temp 0))"\\"))
(print "Шлях даверанай крыніцы:")
(print dirpol)
(print)
(print (strcat "Версія AutoCad-" (itoa verauto)))(princ)
(print (strcat "Бітносць-" bitauto))(princ)
(print "Версія загрузчыка ад 26.12.24 Косаў Ул.//prajdziswet@mail.ru")(princ)
(print "_______загрузка праграмм_______")(princ)
(setq temp nil)
  
;калі шлях невызначаны ставім уручную
(if (= dirpol nil)
(progn
 (setq temp (vl-directory-files (strcat "C:/Users/" (getvar "LOGINNAME") "/AppData/Roaming/Autodesk/AutoCAD " (itoa verauto) "/")))
 (while (/= temp nil)
   (if (or (= (car temp) ".") (= (car temp) ".."))
     (setq temp (cdr temp))
     (setq dirpol (car temp) temp (cdr temp))
    )
  )
(setq dirpol (strcat "C:/Users/" (getvar "LOGINNAME")
		     "/AppData/Roaming/Autodesk/AutoCAD " (itoa verauto) "/" dirpol "/rus/Support/"))
));end if
(setq temp nil)

;установка дирпол везде
(vl-bb-set 'dirpol dirpol)
;-------------------------------------------------------------------------
(If (and (/= nil (getvar "SECURELOAD")) (/= (getvar "SECURELOAD") 1)) (setvar "SECURELOAD" 1))

;----------------------удаление дубликатов доверенных істочніков -------------------------
(setq trusted (getvar "TRUSTEDPATHS") pathlist nil)
(if (OR (<= (strlen trusted) 3) (and (/= nil (vl-string-search ";" trusted)) (<= (vl-string-search ";" trusted) 4)))
  (progn
    (print "..пры памылке доступа паспрабуйце перазапусціць аутакад:..")(princ)
    (setvar "TRUSTEDPATHS" (strcat  dirpol "..."))
    (setq trusted (getvar "TRUSTEDPATHS"))
   )
  )
(if (vl-string-search ";" trusted) 
  (progn
(while (/= "" trusted)
  (setq pozition (vl-string-search ";" trusted))
(if (and (/= nil pozition) (>= pozition 3))
  (progn
  (setq path (substr trusted 1 (+ 1 pozition)))
  (while (and (/= nil trusted) (/= nil (setq pozdubl(vl-string-search path trusted))))
    (setq trusted (substr trusted (+ pozdubl 1 (strlen path))))
    )
  (if (= nil pathlist)
    (setq pathlist (list path))
    (setq pathlist (append pathlist (list path)))
   )
  (setq path (substr path 1 (1- (strlen path))))
  (while (and (/= nil trusted) (/= nil (setq pozdubl(vl-string-search path trusted))))
    (setq trusted (substr trusted (+ pozdubl 1 (strlen path))))
    )
  ); if not nil - progn
  (if (/= nil pathlist)
    (progn
      (setq pathlist (append pathlist (list trusted)))
  	(setq trusted "")
      )
(setq trusted "")
    )
  );if
 )
(while (/= nil pathlist)
  (setq trusted (strcat trusted (car pathlist)))
  (setq pathlist (cdr pathlist))	
 )
(if (and (/= "" trusted) (= ";" (substr trusted (strlen trusted)) ))
  (setq trusted (substr trusted 1 (1- (strlen trusted))))
  )


(if (and (/= "" trusted) (/= (strlen (getvar "TRUSTEDPATHS")) (strlen trusted)))
  (progn
  (print "..пры памылке доступа паспрабуйце перазапусціць аутакад:..")(princ)
  (setvar "TRUSTEDPATHS" trusted)
  ))
))
(setq trusted nil pathlist nil)
;----------------------3 установка доверенных істочніков -------------------------
(if (= nil (vl-string-search (strcat dirpol "...") (getvar "TRUSTEDPATHS")))
  (progn
(print "..пры памылке доступа паспрабуйце перазапусціць аутакад:..")(princ)
(setvar "TRUSTEDPATHS" (strcat (getvar "TRUSTEDPATHS") ";" dirpol "..."))
  ))
;-------------------------------------------------------------------------
(defun clearpath (/ temp spis ttt spis2 ttt1)
  (setq temp (getvar "TRUSTEDPATHS")) 
  (while (/= nil temp)
    (setq ttt (substr temp 1 (vl-string-search ";" temp 0)))
    (if (/= nil (vl-string-search ";" temp 0)) (setq temp (substr temp (+ 1 (vl-string-search ";" temp 0)))) (setq temp nil))
	(if (= nil spis)
	   (setq spis (list ttt))
	  (setq spis (append spis (list ttt)))
	  );end if
	   (if (and (/= nil temp) (wcmatch temp ";*")) (setq temp (substr temp 2)))
   );end while
  (while (/= nil spis)
    (setq ttt (car spis) spis (cdr spis))
    
    (while (/= nil spis)
      (setq ttt1 (car spis) spis (cdr spis))
      	(if (and (/= nil ttt1) (/= ttt ttt1))
	  (if (= nil spis2)
	  (setq spis2 (list ttt1))
	  (setq spis2 (append (list ttt1)))
	  ));end if
      );endwhile
    (setq spis spis2 spis2 nil)
    (if (= temp nil) (setq temp ttt) (setq temp (strcat temp ";" ttt)))
      )
  (if (/= nil temp)
  (setvar "TRUSTEDPATHS" temp))
   )


;----------------------установка пакі фонтс---------------------
(setq trustpath (getenv "ACAD"))
(if (and (= nil (vl-string-search (strcat dirpol "\Fonts") trustpath)) (vl-file-directory-p (strcat dirpol "\\Fonts")))
  (progn
    (setq trustpathTemp trustpath countTrust 0 positionTruth 0)
    (while (setq positionTruth (vl-string-search ";" trustpathTemp positionTruth))
      (setq countTrust (+ 1 countTrust) positionTruth (+ 1 positionTruth))
     )
    (if (< countTrust 15)
      (progn
      (setq acadobj (vlax-get-acad-object))
      (setq filesTrust (vlax-get-property (vlax-get-property acadobj "Preferences") "Files"))
      (setq trustpath (strcat trustpath dirpol "\Fonts"))
      (vlax-put-property filesTrust "SupportPath" trustpath)
       )
      ;(setenv "ACAD" (strcat trustpath dirpol "\\Fonts;"))
      (progn (print "Не удалося дадаць папку шрыўтоў панелькі, так як колькасць дададных папак = 15") (princ)) 
     )
   );end progn
  );end if


(setq trustpath nil trustpathTemp nil positionTruth nil)
;-------------------------------------------------------------------------  
;-----------------------------разлічные функцііі---------------------------------------------
;-------------------------------------------------------------------------

;--------------спробы загрузкі--------------------------
(defun tryload (func input / result)
  (if (vl-catch-all-error-p (setq result (vl-catch-all-apply func (list input))))
    (print (strcat "Памылка зашрузкі: " (vl-catch-all-error-message result)))
    (print (strcat "Файл загружан: " (vl-filename-base input)))
  )
)
;try
(defun try-load (strcom / result)
  (if (vl-catch-all-error-p (setq result (vl-catch-all-apply 'loadcad (list strcom))))
    (print (strcat "Памылка загрузкі: " (vl-catch-all-error-message result)))
    ;(print (strcat "Файл загружан: " (vl-filename-base strcom)))
  )
  )

;загрузка
(DEFUN loadcad (strcom)
(setq strcom (vl-string-trim " " strcom))
       (cond ((= ";" (substr strcom 1 1)) (print strcom))
	    ((wcmatch (strcase strcom) "*`.DLL" )
	     (if (NULL (command "_netload" strcom))
	       (print (strcat "DLL загружен:___" (vl-filename-base strcom)))
	       (strcat "_ошибка загрузки или загруженна ранее:___" strcom " ")
	     )	     
	    );конец длл	

      ((wcmatch (strcase strcom) "*`.CUIX")
	(if (NULL (command "_CUILOAD" strcom))
	  (progn
	   (command)
	   (print (strcat "CUIX загружен:____" (vl-filename-base strcom) " ")))
	(print (strcat "_ошибка загрузки CUIX или загруженна ранее:___" (vl-filename-base strcom) " ")))
      )	;конец cuix

      ((wcmatch (strcase strcom) "*`.ARX")
	(arxload strcom (print (strcat "Невозможно загрузить арх,или он загружен:___" (vl-filename-base strcom))))
        (princ)
	);конец arx
      ((= strcom "") (print "load пустой параметр"))
      ((OR (wcmatch (strcase strcom) "*`.LSP") (wcmatch (strcase strcom) "*`.FAS") (wcmatch (strcase strcom) "*`.VLX"))
       (progn (tryload 'vl-load-all strcom) (princ)))
);конец конд  
 )

;еденичная загрузка
(DEFUN oloadcad (strcom)
  (setq strcom (vl-string-trim " " strcom))
    (load strcom (print (strcat "Невозможно загрузить lisp,или он загружен" (vl-filename-base strcom))))
  )

;выгрузка
(DEFUN unloadcad (strcom)
(setq strcom (vl-string-trim " " strcom))
  (cond ((wcmatch strcom "*`.arx") (arxunload strcom "невыгружен арх"))
	((wcmatch strcom "*`.cuix")
	 (if (menugroup (substr strcom 1 (- (strlen strcom) 5)))
	   (progn
	   (command "_CUIUNLOAD" (substr strcom 1 (- (strlen strcom) 5)))
	     )
	   ))
	((wcmatch strcom "*`.vlx") (if (vl-vlx-loaded-p strcom) (vl-unload-vlx strcom "невыгружен vlx")))
    )
  )

;удаление
;;;(DEFUN delfilecad (strcom)
;;;(setq strcom (vl-string-trim " " strcom))
;;; (if (OR (wcmatch strcom "__prog/*") (/= T (wcmatch strcom "*/*")))
;;;   (vl-file-delete (strcat dirpol strcom));progn
;;;  (vl-file-delete strcom)
;;;   );end if
;;;)

;;;;удаление строки
;;;(DEFUN deletecad (strcom spis)
;;;(setq strcom (vl-string-trim " " strcom))  
;;;(cond ((NULL spis) nil)
;;;      ((wcmatch (CAR spis) (strcat "*" strcom "*")) (deletecad strcom (CDR spis)))
;;;      (T (cons (car spis) (deletecad strcom (CDR spis))))
;;; )
;;;  )
;;;
;;;;добавление строки
;;;(DEFUN addcad (strcom spis)
;;;(setq strcom (vl-string-trim " " strcom))
;;;(cond ((NULL spis) nil)
;;;      ((wcmatch (CAR spis) (strcat "*" strcom "*")) T)
;;;      (T (addcad strcom (CDR spis)))
;;; )
;;;  )


;-------------------------------------------------------------------------

;-------------------------------------------------------------------------
;--------------------функція проверкі года і бітності---------------------------
(defun checkyearbit (file / ret data1 data2 year)
  (setq year verauto)
  (if (not (wcmatch file "*20*"))
    (setq ret -1)
    (progn
      (if (<= (+ (vl-string-search "20" file) 5) (strlen file))
	(setq data1 (substr file (+ (vl-string-search "20" file) 1) 4))
	)
      (if (and (<= (+ (vl-string-search "20" file) 10) (strlen file))
	       (wcmatch (setq ret (substr file (+ (vl-string-search "20" file) 5))) "-*"))
	(setq data2 (substr ret 2 4))
	);data2
   ))
  (cond
    ((= nil data1) (setq ret -1))
    ((and (= nil data2) (= year (atoi data1))) (setq ret 1))
    ((and (/= nil data2) (and (<= (atoi data1) year) (<= year (atoi data2)))) (setq ret 1))
    (T (setq ret 0))
   )
  (if (and (= ret 1) (wcmatch file (strcat "*" bitauto "*")))
(setq ret T)
(setq ret nil)
    )
 )
;--------------------функція проверкі года---------------------------
(defun checkyear (file / ret data1 data2 year)
  (setq year verauto)
  (if (not (wcmatch file "*20*"))
    (setq ret -1)
    (progn
      (if (<= (+ (vl-string-search "20" file) 5) (strlen file))
	(setq data1 (substr file (+ (vl-string-search "20" file) 1) 4))
	)
      (if (and (<= (+ (vl-string-search "20" file) 10) (strlen file))
	       (wcmatch (setq ret (substr file (+ (vl-string-search "20" file) 5))) "-*"))
	(setq data2 (substr ret 2 4))
	);data2
   ))
  (cond
    ((= nil data1) (setq ret -1))
    ((and (= nil data2) (= year (atoi data1))) (setq ret 1))
    ((and (/= nil data2) (and (<= (atoi data1) year) (<= year (atoi data2)))) (setq ret 1))
    (T (setq ret 0))
   )
  (if (or (= ret -1) (= ret 1))
(setq ret T)
(setq ret nil)
    )
 )

;--------------------функція проверкі версіі-------------
(defun checkbit (file / temp)
  (if (OR (wcmatch file "*32*") (wcmatch file "*64*"))
  (if (wcmatch file (strcat "*" bitauto "*")) (setq temp T) (setq temp nil))
  (setq temp T)
  )
    )
;----------------------возвращает іммяфайла с расширением-----------------
(defun namefile (sourse)
  (strcat (vl-filename-base sourse) (vl-filename-extension sourse))
  )
;----------------------проверяет суўествованіе-----------------
(defun fileexist (file path)
  (member file (vl-directory-files path))
 )
;----------------------проверяет дату-----------------
(defun checkdata (data1 data2 / temp1 temp2 ttt t1 t2 t3 t4)
  (if (< (nth 1 data1) 10)
    (setq t1 (strcat "0" (rtos (nth 1 data1))))
    (setq t1 (rtos (nth 1 data1)))
   )
  (if (< (nth 3 data1) 10)
    (setq t2 (strcat "0" (rtos (nth 3 data1))))
    (setq t2 (rtos (nth 3 data1)))
   )
  (if (< (nth 4 data1) 10)
    (setq t3 (strcat "0" (rtos (nth 4 data1))))
    (setq t3 (rtos (nth 4 data1)))
   )
  (if (< (nth 5 data1) 10)
    (setq t4 (strcat "0" (rtos (nth 5 data1))))
    (setq t4 (rtos (nth 5 data1)))
   )
  (setq temp1 (strcat (rtos (nth 0 data1)) t1 t2 t3 t4))
  
    (if (< (nth 1 data2) 10)
    (setq t1 (strcat "0" (rtos (nth 1 data2))))
    (setq t1 (rtos (nth 1 data2)))
   )
  (if (< (nth 3 data2) 10)
    (setq t2 (strcat "0" (rtos (nth 3 data2))))
    (setq t2 (rtos (nth 3 data2)))
   )
    (if (< (nth 4 data2) 10)
    (setq t3 (strcat "0" (rtos (nth 4 data2))))
    (setq t3 (rtos (nth 4 data2)))
   )
  (if (< (nth 5 data2) 10)
    (setq t4 (strcat "0" (rtos (nth 5 data2))))
    (setq t4 (rtos (nth 5 data2)))
   )
  (setq temp2 (strcat (rtos (nth 0 data2)) t1 t2 t3 t4))

  (if (> (atof temp1) (atof temp2))
    (setq ttt T)
    (setq ttt nil)
    )
 )
;----------------------копірует файл, еслі есть папка, еслі нет создает і копірует----------------
(defun copyeasy (pathsourse pathdirect file)
(if (and (not (= file "ACAD.VLX"))
  (OR (= nil exception) (and (/= nil exception)
	     (not (or (member file exception) (member (vl-filename-base file) exception)))))
	 (checkyear file) (checkbit file))
 (if (OR (= nil (vl-file-systime (strcat pathdirect file))) (checkdata (vl-file-systime (strcat pathsourse file)) (vl-file-systime (strcat pathdirect file))))
   (progn
     (if (not (wcmatch file "*GeomProps*"))
 (unloadcad file);выгрузка
    )
 (vl-file-delete (strcat pathdirect file))    
 (vl-file-copy (strcat pathsourse file) (strcat pathdirect file))
     )
)
  ))

;-------------------------------------------------------------------------
(defun upcopy (sourse container exception / linestr listdir temp)
  (setq listdir (vl-directory-files sourse))
  (while (and (/= nil listdir) (setq temp (car listdir)))
    (setq listdir (cdr listdir))
    (IF (or (wcmatch sourse "*dllarx*") (= temp "Update.txt"))
      (PRINT)
      )
    (if (and (/= temp ".") (/= temp "..")) 
    (if (vl-file-directory-p (strcat sourse temp))
      (progn
	(vl-mkdir (strcat container temp));создаем папку на всякий случай
	(upcopy (strcat sourse temp "\\") (strcat container temp "\\") exception);повторяем
       )
      (progn
	(if (and (wcmatch sourse "*__prog*") (OR (not (checkyear temp)) (checkbit temp)) (fileexist temp container))
	  (vl-file-delete (strcat container temp))
	  )
	(copyeasy sourse container temp);копируем	  
	)
   ))
  )
  ;знішчэнне лішкавых файлаў
  (if (OR (wcmatch sourse "*__prog\\*") (wcmatch sourse "*ToolPalette\\*"))
    (progn
    (setq listdir (vl-directory-files container))
    (while (and (/= nil listdir) (setq temp (car listdir)))
      (setq listdir (cdr listdir))
      (if (not (fileexist temp sourse))
	(vl-file-delete (strcat container temp)))
      );endwhile
    ));end if
  )
;-------------------------------------------------------------------------
(defun updatecopy (sourse container / exception linestr)
  (while (setq linestr (read-line modul1))
    (if (= nil exception)
      (setq exception (list linestr))
      (setq exception (append exception (list linestr)))
     )
   );чытанне выключэнняу
  (close modul1)
  (upcopy sourse container exception)
  )

;загрузка апдейт
(if (/= nil (setq modul (open (strcat dirpol "__prog/Update.txt") "r")))
  (progn
    (setq dataupdate (vl-string-trim " " (read-line modul)));дата
    (setq linestr (read-line modul));откуда
    (if (/= nil linestr)
      (if (/= nil (setq modul1 (open (vl-string-trim " " (strcat linestr "__prog/Update.txt")) "r")))
	(if (/= dataupdate (setq dataupdate1 (read-line modul1)))
	  (progn
	    (close modul)
	    (print dataupdate1)
	    (clearpath)
	    (setq linestr (vl-string-trim " " (read-line modul1)))
	    (updatecopy (vl-string-trim " " linestr) dirpol)
	 ))
	)
   )
  ))
(if (/= nil modul)
    (close modul)
   )
(if (/= nil modul1)
    (close modul1)
   )

;-------------------------------------сама функция запуска-----------------------------------------
(DEFUN puskcad (path / spis obj ext)
  (setq spis (vl-directory-files path))
  (while (and (/= nil spis) (setq obj (car spis)))
    (setq spis (cdr spis))
    (if (and (/= obj ".") (/= obj ".."))
      (progn
    (if (/= nil (vl-filename-extension obj)) (setq ext (strcase (vl-filename-extension obj))))
    (cond
      ((and (/= nil ext) (/= nil obj) (OR (= ext ".DLL") (= ext ".ARX")) (checkyearbit obj)) (try-load (strcat path obj)))
      ((wcmatch path "lspone")
	  (loado (strcat path obj))
	  (vl-file-delete (strcat path obj)))
      ((and (/= nil ext) (OR (= ext ".LSP") (= ext ".FAS") (= ext ".VLX"))) (try-load (strcat path obj)))
      ((and (/= nil ext) (= ext ".CUIX")) (try-load (strcat path obj)))
       );end cond
    ))
     );end while
);defun

(if (= nil (OR (menugroup "roznae.cuix") (menugroup "roznae"))) (try-load (strcat dirpol "roznae.cuix")))
(puskcad (strcat dirpol "__prog\\dllarx\\"))
(puskcad (strcat dirpol "__prog\\lsp\\"))

(print (strcat "___________________________END___________________________"))
(setvar "CMDECHO" 1)
