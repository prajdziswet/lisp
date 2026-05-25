(vl-load-com)

;; ==========================================================================
;; ПОДФУНКЦИЯ: Сбор дампов в лог-файл
;; ==========================================================================
(defun LogDump ( / blk ent objName )
  ;; Включаем запись лога
  (setvar "LOGFILEMODE" 1)
  
  (princ "\n=== ПОИСК НОВЫХ ОБЪЕКТОВ ===\n")
  
  ;; Обход всех объектов чертежа (использует переменные из главной функции)
  (vlax-for blk blocks
    (vlax-for ent blk
      (setq objName (vla-get-ObjectName ent))
      
      ;; Если файла с таким именем еще нет в папке
      (if (not (member objName processed))
        (progn
          (setq processed (cons objName processed)) ; Добавляем в список, чтобы не дублировать
          (setq hasNewDumps t)                      ; Флаг, что есть новые данные
          
          (princ (strcat "\n\n@@" objName "\n"))
          (vlax-dump-object ent T)                  ; Вывод дампа в лог
        )
      )
    )
  )
  
  (princ "\n=== ПОИСК ЗАВЕРШЕН ===\n")
  
  ;; Отключаем запись лога, чтобы AutoCAD освободил файл
  (setvar "LOGFILEMODE" origLogMode)
)

;; ==========================================================================
;; ГЛАВНАЯ КОМАНДА: Обновление и создание дампов
;; ==========================================================================
(defun c:UpdateEntityDumps ( / targetDir existingFiles processed acadObj doc blocks 
                               origLogMode hasNewDumps sysLog tempLog dwgPath )
  
  ;; 1. Ввод пути к папке через командную строку (флаг T позволяет вводить путь с пробелами)
  (setq targetDir (getstring T "\nВставьте путь к папке для сохранения дампов: "))
  
  ;; Проверка: ввели ли путь и существует ли такая папка физически
  (if (or (= targetDir "") (not (vl-file-directory-p targetDir)))
    (progn 
      (princ "\n[ОТМЕНА] Указанная папка не существует или путь не введен.") 
      (exit)
    )
  )

  ;; Добавляем слэш в конец пути, если его забыли при копировании
  (if (/= (substr targetDir (strlen targetDir)) "\\")
    (setq targetDir (strcat targetDir "\\"))
  )

  ;; 2. Получаем список УЖЕ существующих файлов в этой папке
  (setq existingFiles (vl-directory-files targetDir "*.txt" 1))
  (setq processed nil)
  (if existingFiles
    (foreach f existingFiles
      ;; Убираем ".txt" и добавляем имя в список обработанных
      (setq processed (cons (vl-filename-base f) processed))
    )
  )
  (princ (strcat "\n[ИНФО] Найдено существующих дампов в папке: " (itoa (length processed))))

  ;; 3. Подготовка к сканированию чертежа
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq blocks (vla-get-Blocks doc))
  
  (setq hasNewDumps nil)
  (setq origLogMode (getvar "LOGFILEMODE"))
  
  ;; 4. Запускаем сканирование
  (LogDump)

  ;; 5. Если найдены новые объекты, обрабатываем файл лога
  (if hasNewDumps
    (progn
      ;; Получаем пути
      (setq sysLog (getvar "LOGFILENAME"))
      (setq dwgPath (getvar "DWGPREFIX"))
      (if (or (not dwgPath) (= dwgPath ""))
        (setq dwgPath (getvar "ROAMABLEROOTPREFIX")) ; Если чертеж не сохранен
      )
      
      ;; Копируем системный лог во временный файл в папке чертежа
      (setq tempLog (strcat dwgPath "Temp_Dump.txt"))
      
      (if (findfile tempLog) (vl-file-delete tempLog)) ; Удаляем старый, если застрял
      (vl-file-copy sysLog tempLog)
      
      ;; ЗАПУСКАЕМ ФУНКЦИЮ РАЗДЕЛЕНИЯ
      (princ "\n[ИНФО] Запущено разделение временного лога на файлы...\n")
      (SplitTempLogToUTF8 tempLog targetDir)
      
      ;; Удаляем временный файл после успешного разделения
      (if (findfile tempLog)
        (vl-file-delete tempLog)
      )
      (princ "\n[ИНФО] Временный лог-файл удален.")
    )
    ;; Иначе (если ничего нового нет)
    (princ "\n[ГОТОВО] Новых типов объектов в чертеже не найдено. База дампов актуальна.")
  )
  
  (textpage) ; Открыть текстовое окно для просмотра результата
  (princ)
)

;; ==========================================================================
;; ПОДФУНКЦИЯ: Разделение файла и конвертация Windows-1251 -> UTF-8
;; ==========================================================================
(defun SplitTempLogToUTF8 ( logPath targetDir / inFile streamObj objName tLine isDataStarted newFilePath header line )
  
  (setq inFile (open logPath "r")) ; LISP читает файл (Windows-1251 считывается корректно в ру-системе)
  (setq streamObj nil)
  (setq isDataStarted nil)

  (while (setq line (read-line inFile))
    (setq tLine (vl-string-trim " \t\r\n" line))

    (cond
      ;; 1. Нашли маркер нового блока
      ((and (>= (strlen tLine) 2) (= (substr tLine 1 2) "@@"))
        
        ;; Закрываем предыдущий стрим, если он был
        (if streamObj 
          (progn
            (vlax-invoke streamObj 'SaveToFile newFilePath 2)
            (vlax-invoke streamObj 'Close)
            (vlax-release-object streamObj)
            (setq streamObj nil)
          )
        )
        
        (setq objName (vl-string-trim " \t\r\n" (substr tLine 3)))
        (setq newFilePath (strcat targetDir objName ".txt"))
        
        ;; Создаем новый ADODB.Stream для сохранения в UTF-8
        (setq streamObj (vlax-create-object "ADODB.Stream"))
        (vlax-put-property streamObj 'Type 2)      
        (vlax-put-property streamObj 'Charset "UTF-8")
        (vlax-invoke streamObj 'Open)
        
        (setq isDataStarted nil)
        
        ;; Пишем заголовок
        (setq header (strcat "дамп по объекту [" objName "]\r\n"))
        (setq header (strcat header "========================================\r\n"))
        (vlax-invoke streamObj 'WriteText header)
        
        (princ (strcat " -> Создан новый дамп: " objName ".txt\n"))
      )

      ;; 2. Запись строк с данными
      ((and streamObj (/= tLine ""))
        (vlax-invoke streamObj 'WriteText (strcat line "\r\n"))
        (setq isDataStarted t)
      )

      ;; 3. Конец блока дампа
      ((and streamObj (= tLine "") isDataStarted)
        (vlax-invoke streamObj 'SaveToFile newFilePath 2)
        (vlax-invoke streamObj 'Close)
        (vlax-release-object streamObj)
        (setq streamObj nil)
        (setq isDataStarted nil)
      )
    )
  )

  ;; Финальное закрытие
  (if inFile (close inFile))
  (if streamObj 
    (progn
      (vlax-invoke streamObj 'SaveToFile newFilePath 2)
      (vlax-invoke streamObj 'Close)
      (vlax-release-object streamObj)
    )
  )
)

(princ "\n=== Скрипт загружен. Введите UpdateEntityDumps для запуска. ===")
(princ)