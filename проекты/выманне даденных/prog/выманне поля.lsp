(vl-load-com)

;; --- 1. МОСТ В VBA (Оставляем как есть, он работает идеально) ---
(defun vla-GetHandleFields (tbl row col / h-vba res)
  (setq h-vba (vla-get-Handle tbl))
  (setvar "USERS1" h-vba)
  (setvar "USERI1" row)
  (setvar "USERI2" col)
  (vl-catch-all-apply 'vl-vbarun (list "ExtractFieldHandle"))
  (setq res (getvar "USERS1"))
  (setvar "USERS1" "")
  (if (and res (/= res "") (/= res h-vba)) res nil)
)

;; --- 2. ГЛАВНЫЙ ПОТРОШИТЕЛЬ ---
(defun c:TOTAL_DUMP ( / start_ent tbl hnd beacon_hnd dwg_path filename queue visited count ename elist obj_type line ptr_hnd blk_obj str_val f wsh)
  
  (setq start_ent (car (entsel "\nВыберите таблицу для Глобального Дампа: ")))
  (if (not start_ent) (exit))
  (setq tbl (vlax-ename->vla-object start_ent))
  
  ;; Получаем маяк
  (setq beacon_hnd (vla-GetHandleFields tbl 1 1))
  (if beacon_hnd 
    (princ (strcat "\n[Маяк установлен] Целевое поле: " beacon_hnd))
    (princ "\n[Внимание] Поле в 1,1 не найдено, маяк не установлен.")
  )

  ;; Настраиваем пути сохранения
  (setq dwg_path (getvar "DWGPREFIX"))
  (if (or (not dwg_path) (= dwg_path "")) 
    (setq dwg_path (strcat (vl-filename-directory (vl-filename-mktemp)) "\\"))
  )
  (setq filename (strcat dwg_path "Table_Full_Anatomy.txt"))

  ;; =============================================================
  ;; ИСПОЛЬЗУЕМ СТАНДАРТНЫЙ LISP-ВЫВОД (100% защита от крашей)
  ;; =============================================================
  (setq f (open filename "w"))
  (if (not f)
    (progn (princ "\nОшибка: Не удалось создать файл!") (exit))
  )

  (write-line "=== ГЛОБАЛЬНЫЙ ДАМП ТАБЛИЦЫ ===" f)
  (write-line (strcat "Дата: " (rtos (getvar "CDATE") 2 6)) f)
  (write-line (strcat "Целевое поле (1,1): " (if beacon_hnd beacon_hnd "НЕТ")) f)
  (write-line "===================================" f)

  ;; Инициализация очереди
  (setq hnd (cdr (assoc 5 (entget start_ent))))
  (setq queue (list hnd))
  (setq visited nil)
  (setq count 0)

  ;; Функция добавления в очередь
  (defun enqueue (h)
    (if (and h (= (type h) 'STR) (not (assoc h visited)))
      (setq queue (append queue (list h)))
    )
  )

  (princ "\nСканирование начато... ")

  ;; ЦИКЛ-ПАУК (ограничение 20000 объектов)
  (while (and queue (< count 20000))
    (setq h (car queue))
    (setq queue (cdr queue))

    (if (not (assoc h visited))
      (progn
        ;; Помечаем как посещенный
        (setq visited (cons (cons h T) visited))
        (setq ename (handent h))

        (if ename
          (progn
            ;; Получаем ВСЕ группы
            (setq elist (entget ename '("*")))
            (setq obj_type (cdr (assoc 0 elist)))
            
            ;; Разделитель для каждого объекта
            (write-line "\n-------------------------------------------------" f)
            (write-line (strcat "HANDLE: " h "  |  ТИП: " (if obj_type obj_type "UNKNOWN")) f)
            
            ;; Если это наш МАЯК, ставим метку!
            (if (and beacon_hnd (= h beacon_hnd))
              (write-line "[!!! ЦЕЛЬ №1: ПОЛЕ ИЗ ЯЧЕЙКИ 1,1 !!!]" f)
            )
            (write-line "-------------------------------------------------" f)

            ;; ЕСЛИ ЭТО БЛОК: Вытаскиваем его содержимое (линии, мтексты)
            (if (= obj_type "BLOCK_RECORD")
              (if (not (vl-catch-all-error-p (setq blk_obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))))
                (vlax-for sub_ent blk_obj
                  (if (vlax-property-available-p sub_ent 'Handle)
                    (progn
                      (enqueue (vla-get-Handle sub_ent))
                      (write-line (strcat "[ВЛОЖЕННЫЙ ОБЪЕКТ] -> Handle: " (vla-get-Handle sub_ent) " (" (vla-get-ObjectName sub_ent) ")") f)
                    )
                  )
                )
              )
            )

            ;; ПЕРЕБИРАЕМ DXF КОДЫ И ПИШЕМ В ФАЙЛ
            (foreach item elist
              (if (= (type (cdr item)) 'ENAME)
                (progn
                  (setq ptr_hnd (cdr (assoc 5 (entget (cdr item)))))
                  (if (and ptr_hnd (= (type ptr_hnd) 'STR))
                    (progn
                      (write-line (strcat "  (Группа " (itoa (car item)) ") УКАЗАТЕЛЬ -> Handle: " ptr_hnd) f)
                      (enqueue ptr_hnd) ;; Ставим в очередь
                    )
                  )
                )
                (progn
                  (setq str_val (vl-princ-to-string (cdr item)))
                  (write-line (strcat "  (" (itoa (car item)) " . " str_val ")") f)
                  
                  ;; Группа 1005 (XData Handle)
                  (if (= (car item) 1005)
                    (enqueue str_val)
                  )
                )
              )
            )

            (setq count (1+ count))
            ;; Индикатор прогресса в консоли
            (if (= (rem count 100) 0)
              (princ (strcat "\rОбработано объектов: " (itoa count) " ..."))
            )
          )
        )
      )
    )
  )

  ;; Закрываем файл
  (close f)

  (princ (strcat "\n\n>>> ГОТОВО! Проанализировано " (itoa count) " объектов."))
  (princ (strcat "\n>>> Дамп сохранен: " filename "\n"))
  
  ;; Открываем файл в Блокноте
  (if (setq wsh (vlax-create-object "WScript.Shell"))
    (progn
      (vlax-invoke wsh 'Run (strcat "notepad.exe \"" filename "\"") 1 :vlax-false)
      (vlax-release-object wsh)
    )
  )
  (princ)
)