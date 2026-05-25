(setq vers (substr dirpol (+ (vl-string-search "autodesk" dirpol) 18) 4))

(if (vl-file-directory-p "C:\\Program Files (x86)")
(if (vl-file-directory-p (strcat "C:\\Program Files (x86)\\Autodesk\\AutoCAD " vers))
  (setq proc "86")
  (setq proc "64")
 )
  (setq proc "86")
 );end if

(setq f1 nil f2 nil)

(cond
  ((and (>= (atoi vers) 2013) (= proc "64"))  (setq f1 "MtmdAcadTabs.2013.x64.arx" f2 "mtmdacadtabs.lsp") )
  ((and (>= (atoi vers) 2013) (/= proc "64")) (setq f1 "MtmdAcadTabs.2013.arx" f2 "mtmdacadtabs.lsp")  )
  ((and (= vers "2012") (= proc "64")) (setq f1 "MtmdAcadTabs.2012.x64.arx" f2 "mtmdacadtabs.lsp"))
  ((and (= vers "2012") (/= proc "64")) (setq f1 "MtmdAcadTabs.2012.arx" f2 "mtmdacadtabs.lsp"))
  ((and (OR (= vers "2010") (= vers "2011")) (= proc "64")) (setq f1 "MtmdAcadTabs.10-11.x64.arx" f2 "mtmdacadtabs0.lsp"))
  ((and (OR (= vers "2010") (= vers "2011")) (/= proc "64")) (setq f1 "MtmdAcadTabs.10-11.arx" f2 "mtmdacadtabs0.lsp"))
  ((and (= vers "2009") (= proc "64")) (setq f1 "MtmdAcadTabs.9.x64.arx" f2 "mtmdacadtabs0.lsp"))
  (T (setq f1 "MtmdAcadTabs.7-9.arx" f2 "mtmdacadtabs0.lsp"))
 )

(setq dir (strcat dirpol "__prog\\"))

(setq m_open (open (strcat dir "mtmdacadtabs0.lsp") "r"))
(setq m_open1 (open (strcat dir "mtmdacadtabs.lsp") "r"))

(if (and (/= nil m_open) (/= nil m_open))
  (progn
    (close m_open) (close m_open1)
    (if (/= f1 "MtmdAcadTabs.2013.x64.arx") (vl-file-delete (strcat dir "MtmdAcadTabs.2013.x64.arx")))
    (if (/= f1 "MtmdAcadTabs.2013.arx") (vl-file-delete (strcat dir "MtmdAcadTabs.2013.arx")))
    (if (/= f1 "MtmdAcadTabs.2012.x64.arx") (vl-file-delete (strcat dir "MtmdAcadTabs.2012.x64.arx")))
    (if (/= f1 "MtmdAcadTabs.2012.arx") (vl-file-delete (strcat dir "MtmdAcadTabs.2012.arx")))
    (if (/= f1 "MtmdAcadTabs.10-11.x64.arx") (vl-file-delete (strcat dir "MtmdAcadTabs.10-11.x64.arx")))
    (if (/= f1 "MtmdAcadTabs.10-11.arx") (vl-file-delete (strcat dir "MtmdAcadTabs.10-11.arx")))
    (if (/= f1 "MtmdAcadTabs.9.x64.arx") (vl-file-delete (strcat dir "MtmdAcadTabs.9.x64.arx")))
    (if (/= f1 "MtmdAcadTabs.7-9.arx") (vl-file-delete (strcat dir "MtmdAcadTabs.7-9.arx")))
    (if (/= f2 "mtmdacadtabs0.lsp") (vl-file-delete (strcat dir "mtmdacadtabs0.lsp")))
    (if (/= f2 "mtmdacadtabs.lsp") (vl-file-delete (strcat dir "mtmdacadtabs.lsp")))
    (arxload (strcat dir f1) (print "Ќевозможно загрузить арх,или он загружен"))
    (vl-load-all (strcat dir f2)) (command) (princ)
    (if (> (atoi vers) 2013)
      (progn
	(command "_.FILETABCLOSE")
	(sEtvar "SECURELOAD" 0)
      ))
   );end progn
  (progn
    (if (/= nil m_open) (close m_open))
    (if (/= nil m_open1) (close m_open1))
    (arxload (strcat dir f1) (print "Ќевозможно загрузить арх,или он загружен"))
    (vl-load-all (strcat dir f2)) (command) (princ)
    );end progn
 )

(setq f1 nil f2 nil dir nil proc nil vers nil)