(vl-load-com)

;; =========================================================================
;; 1. getTextFields (Наша базовая функция расшифровки формулы)
;; =========================================================================
(defun getTextFields (master_handle / fld_ent master_code child_fields child_id child_ent child_code i idx_str item c_item)
  (setq fld_ent (entget (handent master_handle)))
  (setq master_code "")
  
  (foreach item fld_ent
    (if (or (= (car item) 2) (= (car item) 3))
      (setq master_code (strcat master_code (cdr item)))
    )
  )
  
  (setq child_fields nil)
  (foreach item fld_ent
    (if (= (car item) 360)
      (progn
        (setq child_id (cdr item))
        (setq child_ent (entget child_id))
        (if (= (cdr (assoc 0 child_ent)) "FIELD")
          (progn
            (setq child_code "")
            (foreach c_item child_ent
              (if (or (= (car c_item) 2) (= (car c_item) 3))
                (setq child_code (strcat child_code (cdr c_item)))
              )
            )
            (setq child_fields (append child_fields (list child_code)))
          )
        )
      )
    )
  )
  
  (setq i 0)
  (foreach child child_fields
    (setq idx_str (strcat "%<\\_FldIdx " (itoa i) ">%"))
    (setq master_code (vl-string-subst (strcat "%<" child ">%") idx_str master_code))
    (setq i (1+ i))
  )
  master_code
)

;; =========================================================================
;; 2. Вспомогательная функция (удаляет из пула только одно совпадение)
;; =========================================================================
(defun remove-first-match (txt lst / res found)
  (setq res nil found nil)
  (foreach item lst
    (if (and (not found) (= (car item) txt))
      (setq found T) ; Пропускаем (удаляем) только первое совпадение
      (setq res (cons item res))
    )
  )
  (reverse res)
)

;; =========================================================================
;; 3. get-table-cells-with-fields (ГЛАВНЫЙ КАРТОГРАФ)
;; ВОЗВРАЩАЕТ: Список вида ((Row Col "Формула") (Row Col "Формула") ...)
;; =========================================================================
(defun get-table-cells-with-fields (tbl_vla / tbl_ent blk_rec_ent blk_obj mtext_ent mtext_elist xdict_member xdict fld_dict hnd eval_txt formula pool rows cols r c cell_txt match result)
  
  ;; 1. Принудительно генерируем блок таблицы в базе
  (vl-catch-all-apply 'vlax-invoke (list tbl_vla 'GenerateLayout))
  (setq tbl_ent (vlax-vla-object->ename tbl_vla))
  
  ;; 2. Ищем скрытый блок (Группа 343)
  (setq blk_rec_ent (cdr (assoc 343 (entget tbl_ent))))
  (setq pool nil)
  
  ;; 3. СОБИРАЕМ ПУЛ ПОЛЕЙ ИЗ БЛОКА
  (if blk_rec_ent
    (progn
      (setq blk_obj (vlax-ename->vla-object blk_rec_ent))
      (vlax-for sub_obj blk_obj
        (if (= (vla-get-ObjectName sub_obj) "AcDbMText")
          (progn
            (setq mtext_ent (vlax-vla-object->ename sub_obj))
            (setq mtext_elist (entget mtext_ent '("*")))
            
            ;; Ищем словарь ACAD_FIELD
            (if (setq xdict_member (member '(102 . "{ACAD_XDICTIONARY") mtext_elist))
              (if (setq xdict (cdr (assoc 360 xdict_member)))
                (if (setq fld_dict (dictsearch xdict "ACAD_FIELD"))
                  (progn
                    ;; Сохраняем видимый текст (для поиска)
                    (setq eval_txt (vla-get-TextString sub_obj))
                    
                    (foreach item fld_dict
                      (if (= (car item) 360)
                        (progn
                          ;; Достаем полную формулу поля
                          (setq hnd (cdr (assoc 5 (entget (cdr item)))))
                          (setq formula (getTextFields hnd))
                          
                          ;; Добавляем в "Пул" пару: (Текст Формула)
                          (setq pool (cons (list eval_txt formula) pool))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  
  ;; 4. КАРТОГРАФИРУЕМ ЯЧЕЙКИ ТАБЛИЦЫ
  (setq result nil)
  (setq rows (vla-get-Rows tbl_vla))
  (setq cols (vla-get-Columns tbl_vla))
  
  (setq r 0)
  (while (< r rows)
    (setq c 0)
    (while (< c cols)
      ;; Запрашиваем текст ячейки (объединенные скрытые ячейки вернут пустоту или ошибку)
      (setq cell_txt (vl-catch-all-apply 'vla-GetTextString (list tbl_vla r c 0)))
      
      (if (and (not (vl-catch-all-error-p cell_txt)) (/= cell_txt ""))
        (progn
          ;; Ищем этот текст в нашем Пуле
          (setq match (assoc cell_txt pool))
          (if match
            (progn
              ;; БИНГО! Связываем координаты (r, c) с формулой
              (setq result (cons (list r c (cadr match)) result))
              
              ;; Удаляем найденную формулу из Пула, чтобы обработать 
              ;; таблицы, где одинаковые поля повторяются в разных ячейках
              (setq pool (remove-first-match cell_txt pool))
            )
          )
        )
      )
      (setq c (1+ c))
    )
    (setq r (1+ r))
  )
  
  ;; Возвращаем итоговый список
  (reverse result)
)

;; =========================================================================
;; 4. ПРОВЕРОЧНАЯ КОМАНДА
;; =========================================================================
(defun c:MAP_TABLE_FIELDS ( / tbl mapped_fields item r c formula )
  (setq tbl (vlax-ename->vla-object (car (entsel "\nВыберите таблицу: "))))
  
  (princ "\nКартографирование полей таблицы...")
  
  ;; Получаем список: ((Row Col Formula) (Row Col Formula) ...)
  (setq mapped_fields (get-table-cells-with-fields tbl))
  
  (if mapped_fields
    (progn
      (princ (strcat "\n\n>>> УСПЕХ! НАЙДЕНО ЯЧЕЕК С ПОЛЯМИ: " (itoa (length mapped_fields))))
      
      (foreach item mapped_fields
        (setq r (car item))
        (setq c (cadr item))
        (setq formula (caddr item))
        
        (princ (strcat "\n\n--- ЯЧЕЙКА (Строка " (itoa r) ", Столбец " (itoa c) ") ---"))
        (princ (strcat "\nКод поля: " formula))
      )
      (princ "\n\n=======================================================\n")
      (textpage) ;; Разворачивает окно консоли
    )
    (princ "\nВ таблице нет ни одного поля.")
  )
  (princ)
)