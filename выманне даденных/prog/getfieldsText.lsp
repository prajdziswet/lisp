(vl-load-com)

;; --- 1. Функция получения Хэндла (Мост в VBA) ---
(defun vla-GetHandleFields (objOrHandle-vba row col / h-vba res)
  ;; Проверка типа входных данных
  (if (= (type objOrHandle-vba) 'VLA-OBJECT)
    (setq h-vba (vla-get-Handle objOrHandle-vba))
    (setq h-vba objOrHandle-vba)
  )
  
  ;; Передача данных в VBA
  (setvar "USERS1" h-vba)
  (setvar "USERI1" row)
  (setvar "USERI2" col)
  
  ;; Вызов VBA (Убедитесь, что макрос в VBA называется ExtractFieldHandle)
  (vl-catch-all-apply 'vl-vbarun (list "ExtractFieldHandle"))
  
  (setq res (getvar "USERS1"))
  
  ;; Проверка результата: он должен отличаться от входного хэндла таблицы
  (if (and res (/= res "") (/= res h-vba))
    res
    nil
  )
)

;; --- 2. Функция сборки текста (С ПРАВИЛЬНЫМИ ИМЕНАМИ ПЕРЕМЕННЫХ) ---
(defun getTextFields (fld_handle / fld_ent master_code child_fields child_ent child_code i idx_str item c_item)
  (setq fld_ent (entget (handent fld_handle)))
  (setq master_code "")
  
  ;; Сборка мастер-кода
  (foreach item fld_ent
    (if (or (= (car item) 2) (= (car item) 3))
      (setq master_code (strcat master_code (cdr item)))
    )
  )
  
  ;; Поиск вложенных полей
  (setq child_fields nil)
  (foreach item fld_ent
    (if (= (car item) 360)
      (progn
        (setq child_ent (entget (cdr item)))
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
  
  ;; Сборка итоговой строки (Исправлено idx_s -> idx_str)
  (setq i 0)
  (foreach child child_fields
    (setq idx_str (strcat "%<\\_FldIdx " (itoa i) ">%"))
    
    ;; Теперь используем правильную переменную idx_str
    (setq master_code (vl-string-subst (strcat "%<" child ">%") idx_str master_code))
    
    (setq i (1+ i))
  )
  master_code
)