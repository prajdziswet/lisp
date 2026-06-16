;определение пути
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

;сама функция запуска
(DEFUN puskcad ()


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