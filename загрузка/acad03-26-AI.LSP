(setvar "CMDECHO" 0)
(vl-load-com)
(print (strcat "_______________________Загрузка панелькі і праграмм_______________________"))
=========================================================================
; 1. ОПРЕДЕЛЕНИЕ ПАРАМЕТРОВ (ВЕРСИЯ, БИТНОСТЬ, ПУТИ)
;=========================================================================
(setq temp (getvar "ACADPREFIX") verauto nil verauto1 nil)
(setq temp2 (strcase temp) coun1 0 coun2 0)
(if (vl-string-search "AUTOCAD" temp2)
  (while (and (/= temp2 nil) (vl-string-search "AUTOCAD" temp2))
    (setq temp2 (substr temp2 (vl-string-search "AUTOCAD" temp2)))
    (if (> (strlen temp2) 13)
      (progn
        (if (= verauto nil)
          (setq verauto (atoi (substr temp2 10 4)) coun1 (1+ coun1))
          (if (= verauto (atoi (substr temp2 10 4)))
            (setq coun1 (1+ coun1))
            (if (= verauto1 nil)
              (setq verauto1 (atoi (substr temp2 10 4)) coun2 (1+ coun2))
              (if (= verauto1 (atoi (substr temp2 10 4))) (setq coun2 (1+ coun2))))))
        (setq temp2 (substr temp2 13)))
      (setq temp2 nil))))
(if (<= coun1 coun2) (if (>= verauto1 2007) (setq verauto verauto1)))
(if (> (strlen (vl-prin1-to-string (vlax-get-acad-object))) 40) (setq bitauto "64") (setq bitauto "32"))

(setq temp (getvar "ACADPREFIX"))
(while (and temp (not (wcmatch (substr temp 1 (vl-string-search ";" temp 0)) "*Users*upport*")))
  (setq temp (substr temp (+ 1 (vl-string-search ";" temp 0)) (- (strlen temp) (+ 1 (vl-string-search ";" temp 0))))))
(if (and temp (/= (vl-string-search ";" temp 0) nil))
  (setq dirpol (strcat (substr temp 1 (vl-string-search ";" temp 0)) "\\")))
(if (or (= dirpol nil) (= dirpol "\\"))
  (setq dirpol (strcat "C:/Users/" (getvar "LOGINNAME") "/AppData/Roaming/Autodesk/AutoCAD " (itoa verauto) "/R" (substr (getvar "ACADVER") 1 4) "/rus/Support/")))
(vl-bb-set 'dirpol dirpol)

(if (and verauto bitauto dirpol)
(progn
(print "Шлях даверанай крыніцы:")
(print dirpol)
(print)
(print (strcat "Версія AutoCad-" (itoa verauto)))(princ)
(print (strcat "Бітносць-" bitauto))(princ)
(print "Версія загрузчыка+AI ад 03.26 Косаў Ул.//prajdziswet@mail.ru")(princ)
(print "_______загрузка праграмм_______")(princ)
)
(progn
(print "Штосьці пайшло не так, не высзанчан адзін або некалькі параметраў:бітнасць,версія,шлях...") (princ)
));вывад шляху
;-------------------------------------------------------------------------
(If (and (/= nil (getvar "SECURELOAD")) (/= (getvar "SECURELOAD") 1)) (setvar "SECURELOAD" 1))
;=========================================================================
; 2. ВСПОМОГАТЕЛЬНЫЕ
;=========================================================================
(defun string-to-list (str delim / pos lst)
  (setq lst nil)
  (while (setq pos (vl-string-search delim str))
    (setq lst (cons (substr str 1 pos) lst) str (substr str (+ (strlen delim) pos 1))))
  (if (and str (/= str "")) (setq lst (cons str lst))) (reverse lst))
 (defun setup-paths (force-clean / current-str path-list clean-list result-str p fonts-path acad-pref)
  
  ; --- Вспомогательная функция для безопасного слияния строк ---
  (defun merge-path (old-str new-path / trimmed)
    (setq trimmed (if old-str (vl-string-trim " " old-str) ""))
    (if (or (= trimmed "") (= trimmed nil))
      new-path
      (strcat trimmed ";" new-path)
    )
  )

  ; --- 1. Работа с TRUSTEDPATHS ---
  (setq current-str (getvar "TRUSTEDPATHS"))
  (setq path-list (string-to-list current-str ";"))
  
  ; Очистка (если принудительно T или путей > 15)
  (if (or (= force-clean T) (> (length path-list) 15))
    (progn
      (setq clean-list nil)
      (foreach p path-list
        (setq p (vl-string-trim " " p))
        (if (and (/= p "") (not (member (strcase p) clean-list)))
          (setq clean-list (cons (strcase p) clean-list))))
      (if clean-list
        (progn
          (setq result-str (car clean-list))
          (foreach p (cdr clean-list) (setq result-str (strcat p ";" result-str))))
        (setq result-str "")))
    (setq result-str current-str)
  )

  ; Добавляем основной путь через безопасное слияние
  (if (not (vl-string-search (strcase dirpol) (strcase result-str)))
    (setq result-str (merge-path result-str (strcat dirpol "...")))
  )
  (setvar "TRUSTEDPATHS" result-str)

  ; --- 2. Работа с ACADPREFIX (Шрифты) ---
  (setq fonts-path (strcat dirpol "Fonts")) ; Папка шрифтов с большой буквы
  (if (vl-file-directory-p fonts-path)
    (progn
      (setq acad-pref (getvar "ACADPREFIX"))
      (if (not (vl-string-search (strcase fonts-path) (strcase acad-pref)))
        (vla-put-SupportPath 
  (vla-get-Files (vla-get-Preferences (vlax-get-acad-object))) 
  (strcat (getvar "ACADPREFIX") ";" fonts-path)
)
      )
    )
  )
  (princ)
)
;=========================================================================; 
;3. Загрузка і копірованіе
;=========================================================================
;--------------------функція проверкі года-------------------------
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
(defun checkbit (file)
  (if (or (wcmatch (strcase file) "*32*") (wcmatch (strcase file) "*64*"))
    (wcmatch (strcase file) (strcat "*" bitauto "*"))
    T))

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

;try
(defun try-load (strcom / result)
  (if (vl-catch-all-error-p (setq result (vl-catch-all-apply 'loadcad (list strcom))))
    (print (strcat "Памылка загрузкі: " (vl-catch-all-error-message result)))
    ;(print (strcat "Файл загружан: " (vl-filename-base strcom)))
  )
  )

; --- Ваша расширенная логика loadcad ---
(defun loadcad (strcom / ext)
  (setq strcom (vl-string-trim " " strcom))
  (setq ext (if (vl-filename-extension strcom) (strcase (vl-filename-extension strcom)) ""))
  
  (cond 
    ; 1. Если строка начинается с ; (ваша проверка комментариев)
    ((= ";" (substr strcom 1 1)) (print strcom))
    
    ; 2. Пустая строка
    ((= strcom "") (print "load пустой параметр"))
    
    ; 3. Загрузка DLL
    ((= ext ".DLL")
     (if (NULL (command "_netload" strcom))
       (print (strcat "DLL загружен:___" (vl-filename-base strcom)))
       (print (strcat "_ошибка загрузки или загружена ранее:___" strcom))))
    
    ; 4. Загрузка CUIX
    ((= ext ".CUIX")
     (if (not (menugroup (vl-filename-base strcom)))
       (if (NULL (command "_CUILOAD" strcom))
         (progn (command) (print (strcat "CUIX загружен:____" (vl-filename-base strcom))))
         (print (strcat "_ошибка загрузки CUIX:___" (vl-filename-base strcom))))
       (print (strcat "CUIX уже загружен:___" (vl-filename-base strcom)))))
    
    ; 5. Загрузка ARX
    ((= ext ".ARX")
     (arxload strcom (print (strcat "Невозможно загрузить ARX, или он загружен:___" (vl-filename-base strcom))))
     (princ))
    
    ; 6. Загрузка DVB
    ((= ext ".DVB")
     (vl-vbaload strcom (print (strcat "Невозможно загрузить DVB (VBA), или он загружен:___" (vl-filename-base strcom))))
     (princ))
    
    ; 7. Загрузка LSP, FAS, VLX
    ((member ext '(".LSP" ".FAS" ".VLX"))
     (vl-load-all strcom))
  )
  (princ)
)

(defun unloadcad (strcom / ext name)
  (setq strcom (vl-string-trim " " strcom))
  (setq ext (if (vl-filename-extension strcom) (strcase (vl-filename-extension strcom)) ""))
  (setq name (vl-filename-base strcom)) ; Имя файла без расширения и пути

  (cond 
    ; 1. Выгрузка меню (CUIX)
    ((= ext ".CUIX")
     (if (menugroup name)
       (if (NULL (command "_CUIUNLOAD" name))
         (print (strcat "CUIX выгружен:____" name))
         (print (strcat "_ошибка выгрузки CUIX:___" name)))))

    ; 2. Выгрузка ARX
    ((= ext ".ARX")
     (if (member (strcase (strcat name ".arx")) (mapcar 'strcase (arx)))
       (arxunload strcom (print (strcat "Не удалось выгрузить ARX:___" name)))))

    ; 3. Выгрузка DVB (VBA)
    ((= ext ".DVB")
     ; VBA выгружается по полному пути или имени проекта
     (vl-vbaunload strcom) 
     (princ (strcat "\nПроект DVB (VBA) отправлен на выгрузку: " name)))
    
    ; 4. DLL (Netload) - к сожалению, выгрузить DLL без перезапуска AutoCAD невозможно
    ((= ext ".DLL")
     (princ (strcat "\nDLL нельзя выгрузить динамически: " name)))
  )
  (princ)
)

;----------------------возвращает іммяфайла с расширением-----------------
(defun namefile (sourse)
  (strcat (vl-filename-base sourse) (vl-filename-extension sourse))
  )
;----------------------проверяет суўествованіе-----------------
(defun fileexist (file path)
  (member file (vl-directory-files path))
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
  
(defun puskcad (path / spis obj ext fullpath)
  (setq spis (vl-directory-files path))
  (while (and (/= nil spis) (setq obj (car spis)))
    (setq spis (cdr spis))
    (if (and (/= obj ".") (/= obj ".."))
      (progn
        (setq ext (if (vl-filename-extension obj) (strcase (vl-filename-extension obj)) ""))
        (setq fullpath (strcat path obj))
        
        (cond
          ; --- ЛОГИКА LSPONE: Загрузить и УДАЛИТЬ ---
          ((wcmatch (strcase path) "*LSPONE*")
           (loadcad fullpath)
           (vl-file-delete fullpath)
           (princ (strcat "\nФайл из LSPONE загружен и удален: " obj)))

; 2. Удаление ненужных (только если это TARGET файл и он не подходит)
          ((and (member ext '(".DLL" ".ARX" ".DVB")) 
                (is-target-file obj) 
                (not (checkyearbit obj)))
           (vl-file-delete fullpath))
; --- ОБЫЧНАЯ ЗАГРУЗКА (DLL, ARX, DVB) с проверкой года/битности --
	  ((and (member ext '(".DLL" ".ARX" ".DVB")) (checkyearbit obj))
           (loadcad fullpath))

          ; --- ОБЫЧНАЯ ЗАГРУЗКА (LSP, FAS, VLX, CUIX) ---
          ((member ext '(".LSP" ".FAS" ".VLX" ".CUIX"))
           (loadcad fullpath))
        )
      )
    )
  )
  (princ)
)

(defun c:stop-my-panel (/ f)
  ; dirpol берется из глобальной переменной, которую мы определили при запуске
  (if (and dirpol (/= dirpol ""))
    (progn
      (setq f (open (strcat dirpol "delete-my-panel.txt") "w"))
      (write-line "Этот файл блокирует автозагрузку и миграцию панелей." f)
      (write-line (strcat "Создан: " (rtos (getvar "CDATE") 2 4)) f)
      (close f)
      (princ (strcat "\nФайл блокировки создан в: " dirpol))
    )
    (princ "\nОшибка: путь dirpol не определен.")
  )
  (princ)
)
(defun c:delete-my-panel ()
(c:stop-my-panel)
)
(defun is-target-file (file / pos year-str)
  (setq file (strcase file))
  ; Ищем "20". Если нашли, проверяем, что это 4 цифры года (20xx)
  (if (setq pos (vl-string-search "20" file))
    (progn
      (setq year-str (substr file (1+ pos) 4))
      (wcmatch year-str "20##") ; Возвращает T, если это 20 и две любые цифры
    )
    nil ; Если "20" нет, файл точно не целевой
  )
) 
;=========================================================================
; ; 4. ВЫПОЛНЕНИЕ
;=========================================================================

(setq stop-file (strcat dirpol "delete-my-panel.txt"))

; --- Вспомогательные функции для деинсталляции ---

; 1. Рекурсивное удаление папок со всем содержимым
(defun delete-folder (path / files full)
  (setq path (if (wcmatch path "*\\") path (strcat path "\\")))
  (if (vl-file-directory-p path)
    (progn
      (setq files (vl-directory-files path nil 1))
      (foreach f files
        (if (and (/= f ".") (/= f ".."))
          (progn
            (setq full (strcat path f))
            (if (vl-file-directory-p full)
              (delete-folder full) ; Уходим в подпапку
              (progn 
                (vl-file-chmod full 128) ; Снимаем атрибут "Только чтение"
                (vl-file-delete full)))))) ; Удаляем файл
      (vl-file-delete path)))) ; Удаляем пустую папку

; 2. Умная очистка ACADPREFIX (удаляет только конкретный путь Fonts)
(defun clean-acadprefix ( / acad-obj pref-obj files-obj current-paths target path-list clean-list result)
  (vl-load-com)
  
  ; 1. Атрымліваем доступ да налад AutoCAD праз ActiveX
  (setq acad-obj (vlax-get-acad-object)
        pref-obj (vla-get-Preferences acad-obj)
        files-obj (vla-get-Files pref-obj))
  
  ; 2. Чытаем бягучыя шляхі падтрымкі (гэта і ёсць тое, што паказвае ACADPREFIX)
  (setq current-paths (vla-get-SupportPath files-obj))
  
  ; 3. Фармуем мэту для выдалення
  (setq target (strcase (strcat dirpol "Fonts")))
  
  ; 4. Разбіваем на спіс і фільтруем (як у вашым кодзе)
  (setq path-list (string-to-list current-paths ";"))
  (setq clean-list nil)
  
  (foreach p path-list
    (setq p (vl-string-trim " " p))
    (if (and (/= p "") 
             (/= (strcase p) target) 
             (/= (strcase p) (strcat target "\\")))
      (setq clean-list (cons p clean-list))
    )
  )
  
  ; 5. Збіраем назад і ЗАПІСВАЕМ праз ActiveX
  (if clean-list
    (progn
      (setq clean-list (reverse clean-list))
      (setq result (car clean-list))
      (foreach p (cdr clean-list) (setq result (strcat result ";" p)))
      
      ; КРЫТЫЧНЫ МОМАНТ: Запісваем новы шлях у налады
      (vla-put-SupportPath files-obj result)
      (princ "\n[OK]: Шлях Fonts выдалены з налад профілю.")
    )
  )
  (princ)
)

; --- ОСНОВНОЕ УСЛОВИЕ БЛОКА 4 ---

(if (findfile stop-file)
  (progn
    (princ "\n![РЕЖИМ ОЧИСТКИ]: Обнаружен стоп-файл. Начинается деинсталляция..")

    ; 4.1 ВЫГРУЗКА ИЗ ПАМЯТИ
    ; Выгружаем CUIX (меню)
    (setq cuix-p (strcat dirpol "__prog\\cuix\\"))
    (if (vl-file-directory-p cuix-p)
      (foreach f (vl-directory-files cuix-p "*.cuix" 1) (unloadcad f)))
    
    ; Выгружаем ARX и DVB (библиотеки)
    (setq dll-p (strcat dirpol "__prog\\dllarx\\"))
    (if (vl-file-directory-p dll-p)
      (foreach f (vl-directory-files dll-p "*.*" 1)
        (setq ext (strcase (vl-filename-extension f)))
        (if (member ext '(".ARX" ".DVB")) (unloadcad f))))

    ; 4.2 ОЧИСТКА ПУТЕЙ (только Fonts)
    (clean-acadprefix)

    ; 4.3 ФИЗИЧЕСКОЕ УДАЛЕНИЕ КАТАЛОГОВ
    (princ "\n..Удаление рабочих файлов..")
    (delete-folder (strcat dirpol "__prog"))
    (delete-folder (strcat dirpol "Color"))
    (delete-folder (strcat dirpol "Fonts"))

    ; 4.4 УДАЛЕНИЕ СТОП-ФАЙЛА
    (vl-file-delete stop-file)
    (princ "\n[ГОТОВО]: Панель успешно удалена.")
  )

  ; --- ИНАЧЕ: ШТАТНАЯ ЗАГРУЗКА ---
  (progn
    (setq cuix-dir (strcat dirpol "__prog\\cuix\\"))
    
    ; 4.1 Миграция старого меню
    (if (and (findfile (strcat dirpol "roznae.cuix")) (vl-file-directory-p cuix-dir))
      (progn
        (princ "\n..Выполняется миграция меню..")
        (foreach f (vl-directory-files cuix-dir "*.cuix" 1)
          (if (and (/= (strcase f) "ACAD.CUIX") (menugroup (vl-filename-base f)))
            (unloadcad f)))
        (if (menugroup "roznae") (unloadcad "roznae.cuix"))
        (vl-file-delete (strcat dirpol "roznae.cuix"))))

    ; 4.2 Настройка путей (TRUSTEDPATHS и Fonts)
    (if (>= verauto 2013)
      (setup-paths nil) ; Очистка дублей + наш путь
      (progn ; Для старых версий только Fonts
        (setq fonts-path (strcat dirpol "Fonts"))
        (if (and (vl-file-directory-p fonts-path)
                 (not (vl-string-search (strcase fonts-path) (strcase (getvar "ACADPREFIX")))))
          (progn
		  (vla-put-SupportPath 
  (vla-get-Files (vla-get-Preferences (vlax-get-acad-object))) 
  (strcat (getvar "ACADPREFIX") ";" fonts-path))
		  ))))

    ; 4.3 Обновление файлов
    (if (setq modul (open (strcat dirpol "__prog/Update.txt") "r"))
      (progn
        (setq dataupdate (vl-string-trim " " (read-line modul))
              linestr (read-line modul))
        (close modul)
        (if (and linestr (/= (vl-string-trim " " linestr) ""))
          (if (setq modul1 (open (strcat (vl-string-trim " " linestr) "__prog/Update.txt") "r"))
            (progn
              (if (/= dataupdate (read-line modul1)) (updatecopy (vl-string-trim " " linestr) dirpol))
              (close modul1))))))

    ; 4.4 Запуск модулей
    (princ "\n..Запуск программ..")
    (puskcad (strcat dirpol "__prog\\dllarx\\"))
    (puskcad (strcat dirpol "__prog\\lsp\\"))
    (puskcad (strcat dirpol "__prog\\lspone\\"))
    (puskcad cuix-dir)
  )
)

(princ (strcat "\nAutoCAD: " (itoa verauto) " | Загрузка завершена успешно.\n"))