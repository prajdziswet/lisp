;определение пути
(setq temp (getvar "ACADPREFIX"))
(while (not (wcmatch (substr temp 1 (vl-string-search ";" temp 0)) "*Users*support*"))
  (setq temp (substr temp (+ 1 (vl-string-search ";" temp 0)) (- (strlen temp) (+ 1 (vl-string-search ";" temp 0)))))
 )

(setq dirpol (strcat (substr temp 1 (vl-string-search ";" temp 0))"\\"))
(print dirpol)
(princ)
(setq temp nil)
  
;калі шлях невызначаны ставім уручную
(if (= dirpol nil)
(progn  
(setq dirpol (strcat "C:/Users/" (getvar "LOGINNAME")
		     "/AppData/Roaming/Autodesk/AutoCAD 2010/R18.0/rus/Support/"))
(setvar "CMDECHO" 0)
;от версии акада
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

;установка доверенных істочніков
(if (/= T (wcmatch (getvar "TRUSTEDPATHS") "*C:\\Users\\*\\appdata\\roaming\\autodesk\\*support\\*"))
(if (/= (getvar "TRUSTEDPATHS") "")
(setvar "TRUSTEDPATHS" (strcat (getvar "TRUSTEDPATHS") ";" dirpol "..."))
(setvar "TRUSTEDPATHS" (strcat dirpol "..."))
  ))

;загрузка апдейт
(if (/= nil (setq modul (open (strcat dirpol "__prog/Update.txt") "r")))
  (progn
    (setq linestr (read-line modul))
    (if (= linestr "1")
      (vl-file-delete (strcat dirpol "__prog/AcadLoadConstantly.txt"))
     )
   )
  )
(if (/= nil modul)
  (progn
    (close modul)
    (vl-file-delete(strcat dirpol "__prog/Update.txt"))
   )
  )

;установка дирпол везде
(vl-bb-set 'dirpol dirpol)
  
;загрузка
(DEFUN loadcad (strcom)
(setq strcom (vl-string-trim " " strcom))
       (cond ((= ";" (substr strcom 1 1)) (print strcom))
	    ((wcmatch strcom "*`.dll" )
	     (if (NULL (command "_netload" strcom))
	       (print (strcat "DLL загружен\n" strcom "\n"))
	     )
	     (print "ошибка загрузки или загруженна ранее")
	    );конец длл	

      ((wcmatch strcom "*`.cuix")
	(if (NULL (command "_CUILOAD" strcom))
	  (progn
	   (command)
	   (print (strcat "CUIX загружен " strcom " ")))
	(print "-ошибка загрузки или загруженна ранее-"))
      )
	;конец cuix

      ((wcmatch strcom "*`.arx")
	(arxload strcom (print "Невозможно загрузить арх,или он загружен"))
        (princ)
	);конец arx
      ((= strcom "") (print "load пустой параметр"))
      (T (progn (vl-load-all strcom) (command) (princ)))
);конец конд
 )

;еденичная загрузка
(DEFUN oloadcad (strcom)
  (setq strcom (vl-string-trim " " strcom))
    (load strcom (print "Невозможно загрузить lisp,или он загружен"))
  )

;выгрузка
(DEFUN unloadcad (strcom)
(setq strcom (vl-string-trim " " strcom))
  (cond ((wcmatch strcom "*`.arx") (arxunload strcom "невыгружен арх"))
	((wcmatch strcom "*`.cuix") (command "_CUIUNLOAD" (substr strcom
								 (- (strlen strcom) 4) 4)))
	((wcmatch strcom "*`.vlx") (vl-unload-vlx strcom "невыгружен vlx"))
    )
  )

;удаление
(DEFUN delfilecad (strcom)
(setq strcom (vl-string-trim " " strcom))
 (if (OR (wcmatch strcom "__prog/*") (/= T (wcmatch strcom "*/*")))
   (vl-file-delete (strcat dirpol strcom));progn
  (vl-file-delete strcom)
   );end if
)

;удаление строки
(DEFUN deletecad (strcom spis)
(setq strcom (vl-string-trim " " strcom))  
(cond ((NULL spis) nil)
      ((wcmatch (CAR spis) (strcat "*" strcom "*")) (deletecad strcom (CDR spis)))
      (T (cons (car spis) (deletecad strcom (CDR spis))))
 )
  )

;добавление строки
(DEFUN addcad (strcom spis)
(setq strcom (vl-string-trim " " strcom))
(cond ((NULL spis) nil)
      ((wcmatch (CAR spis) (strcat "*" strcom "*")) T)
      (T (addcad strcom (CDR spis)))
 )
  )

  ;-----------капираванне з файла AcadSetup у спис, яки пасля перанясецца у AcadLoadConstantly-------
;-----------выконванне простых каманд лоад і іншых-------------------------------------------------
(DEFUN setupid (/ modul_t proverka spis_t temp linestr spis_set)
  (setq proverka T)
  (setq spis_t spis)
  ;proverka
  (while (and (/= nil spis_t) proverka)
    (setq temp (vl-string-trim " " (car spis_t)))
    (setq spis_t (cdr spis_t))
    (if (not (or (wcmatch temp ";*") (= temp "")))
      (setq proverka nil)
      );end if
    );end while
  (setq spis_t nil)
  
  ;утварэнне спісу сетуп і калі есть простыя каманды выкананне
  (if (and (= proverka T) (/= nil (setq modul_t (open (strcat dirpol "__prog/AcadSetup.txt") "r"))))
  (progn
    (while (/= nil
	       (setq linestr (read-line modul_t))
	   )
      (setq linestr (vl-string-trim " " linestr))
      ;выкананне простых каманд
        (cond
    ((or (= linestr "")))
    ((wcmatch linestr "load *") (loadcad (substr linestr 6 (- (strlen linestr) 5))))
    ((wcmatch linestr "loado *") (oloadcad (substr linestr 7 (- (strlen linestr) 6))))
    ((wcmatch linestr "delete *") (setq spis (deletecad (substr linestr 8 (- (strlen linestr) 7)) spis)))
    ((wcmatch linestr "deletefile *") (delfilecad (substr linestr 12 (- (strlen linestr) 11))))
    ((wcmatch linestr "unload *") (unloadcad (substr linestr 8 (- (strlen linestr) 7))))
    (T (if (/= spis_set nil) (setq spis_set (append spis_set (list linestr))) (setq spis_set (list linestr))
			));end last command
    )
  );end while
    (close modul_t)
    
    ;убіранне першых трох радкоў
    (if (and (/= spis_set nil) (> (length spis_set) 3))
      (setq spis_set (cdr (cdr (cdr spis_set))))
      )
    ;
    (setq spis_t spis)
    ;дадаванне у спіс
    (while (/= spis_set nil)
      (setq temp (car spis_set))
      (setq spis_set (cdr spis_set))
      (if (not (addcad temp spis))
	(if (/= spis nil) (setq spis (append spis (list temp))) (setq spis (list temp))
			)
	);end if
     );end while
    
    );end progn
   );endif
  (setq spis (reverse spis))
);канец функцыі


;-------------------------------------сама функция запуска-----------------------------------------
(DEFUN puskcad (/ spis modul modul1 linestr)


(setq spis nil) 
(if (/= nil (setq modul (open (strcat dirpol "__prog/AcadLoadConstantly.txt") "r")))
  (progn
    (while (/= nil
	       (setq linestr (read-line modul))
	   )
      (setq linestr (vl-string-trim " " linestr))
      (setq spis (cons linestr spis))
  );end while
    (close modul)
    )
  (progn
    (setq modul (open (strcat dirpol "__prog/AcadLoadConstantly.txt") "w"))
    (close modul)
    (setupid)
    )
  );end if

  (setq nnn 1)
  (if (/= nil (setq modul (open (strcat dirpol "__prog/AcadLoadChange.txt") "r")))
  (progn
    (setq modul1 (open (strcat dirpol "__prog/AcadLoadChange1.txt") "w"))
    (while (/= nil
	       (setq linestr (read-line modul))
	   )
      (setq linestr (vl-string-trim " " linestr))
      (if (< nnn 12)
	(progn
	 (write-line linestr modul1)
	  (setq nnn (+ nnn 1))
	  )
(progn
  (cond
    ((wcmatch linestr "load *") (loadcad (substr linestr 6 (- (strlen linestr) 5))))
    ((wcmatch linestr "loado *") (oloadcad (substr linestr 7 (- (strlen linestr) 6))))
    ((wcmatch linestr "delete *") (setq spis (deletecad (substr linestr 8 (- (strlen linestr) 7)) spis)))
    ((wcmatch linestr "deletefile *") (delfilecad (substr linestr 12 (- (strlen linestr) 11))))
    ((wcmatch linestr "unload *") (unloadcad (substr linestr 8 (- (strlen linestr) 7))))
    ((wcmatch linestr "add alc *")
     (if (addcad (substr linestr 9 (- (strlen linestr) 8)) spis)
       ()
	(setq spis (cons (substr linestr 9 (- (strlen linestr) 8)) spis))
	 )

     )
    ((wcmatch linestr "add alch *") ((write-line (substr linestr 10 (- (strlen linestr) 9)) modul1)))
 ))
	);end if while
     
  );end while
      (close modul)
      (close modul1)
      (vl-file-delete(strcat dirpol "__prog/AcadLoadChange.txt"))
      (vl-file-rename (strcat dirpol "__prog/AcadLoadChange1.txt")
	(strcat dirpol "__prog/AcadLoadChange.txt"))

    )
  );end if
  (if (/= nil spis)
    (progn
  (setq spis (reverse spis))
  (setq mmm (open (strcat dirpol "__prog/AcadLoadConstantly.txt") "w"))
  (while (/= nil spis)
    (setq linestr (vl-string-trim " " (CAR spis)))
    (if (/= nil linestr)
    (write-line (CAR spis) mmm)
      );end if
    (setq spis (CDR spis))
    );end while
    (close mmm)
    );end progn
  )
  )
;------------------автоматическая загрузка длл---- verauto bitauto verautoarx-----------------
(defun zdll (/ listfile listfile temp tempdir verautoarx)
  (vl-load-com)
 (if (> (strlen (vl-prin1-to-string (vlax-get-acad-object))) 40)
   (setq bitauto "64")
   (setq bitauto "32")
   )
  (cond
  ((= "18.0" (substr (getvar "acadver") 1 4)) (setq verauto "2010")(setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2010*" bitauto "*" ".arx") 0)))
  ((= "18.1" (substr (getvar "acadver") 1 4)) (setq verauto "2011")
   (setq listfile (append
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2010*" bitauto "*" ".arx") 0))
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2011*" bitauto "*" ".arx") 0)))))
  ((= "18.2" (substr (getvar "acadver") 1 4)) (setq verauto "2012")(setq listfile (append
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2010*" bitauto "*" ".arx") 0))
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2011*" bitauto "*" ".arx") 0))
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2012*" bitauto "*" ".arx") 0)))))
  ((= "19.0" (substr (getvar "acadver") 1 4)) (setq verauto "2013")(setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2013*" bitauto "*" ".arx") 0)))
  ((= "19.1" (substr (getvar "acadver") 1 4)) (setq verauto "2014")(setq listfile (append
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2013*" bitauto "*" ".arx") 0))
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2014*" bitauto "*" ".arx") 0)))))
  ((= "20.0" (substr (getvar "acadver") 1 4)) (setq verauto "2015")(setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2015*" bitauto "*" ".arx") 0)))
  ((= "20.1" (substr (getvar "acadver") 1 4)) (setq verauto "2016")(setq listfile (append
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2015*" bitauto "*" ".arx") 0))
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2016*" bitauto "*" ".arx") 0)))))
  ((= "21.0" (substr (getvar "acadver") 1 4)) (setq verauto "2017")(setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2017*" bitauto "*" ".arx") 0)))
  ((= "22.0" (substr (getvar "acadver") 1 4)) (setq verauto "2018")(setq listfile (append
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2017*" bitauto "*" ".arx") 0))
     (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*2018*" bitauto "*" ".arx") 0)))))
  )
 (foreach temp listfile
   (arxload (strcat tempdir temp) (print "Невозможно загрузить арх,или он загружен"))
  )
  
   (setq listfile (vl-directory-files (setq tempdir (strcat dirpol "__prog\\dllarx\\")) (strcat "*" verauto "*" bitauto "*" ".dll") 0))
 (foreach temp listfile
   (command "_netload" (strcat tempdir temp))
  )
;;; (if (/= nil listfile)
;;;  )
 )
(zdll)

(puskcad)
(setq spis nil)

(if (/= nil (setq modul (open (strcat dirpol "__prog/AcadLoadConstantly.txt") "r")))
  (progn
    (while (/= nil
	       (setq linestr (read-line modul)))
	   
      (vl-string-trim " " linestr)
      ;(if (= linestr "") (setq linestr nil))
      (if (/= nil linestr)
      (loadcad linestr)
  ))
    (close modul)
)
(print "Файл AcadLoadConstantly.txt ненайден")
)

(princ)