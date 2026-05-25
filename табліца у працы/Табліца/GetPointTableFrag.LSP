(vl-load-com)

;; =========================================================================
;; ШАГ 2 (ИСПРАВЛЕННЫЙ): GetPointTableFrag
;; ОПИСАНИЕ: Вычисляет точку фрагмента на основе встроенного порядка генерации,
;;           игнорируя ручное визуальное перемещение пользователя.
;; =========================================================================
(defun GetPointTableFrag (tbl_arg frag_index / tbl_vla tbl_ent elist blk_rec_ent blk_obj 
                                               break_space tol boxes p1 p2 merge-clusters 
                                               sorted_boxes target_box idx
                                               minX maxY ip dir norm y_axis wcs_x wcs_y wcs_z)
  
  (if (= (type tbl_arg) 'VLA-OBJECT)
    (setq tbl_vla tbl_arg tbl_ent (vlax-vla-object->ename tbl_vla))
    (setq tbl_ent tbl_arg tbl_vla (vlax-ename->vla-object tbl_ent))
  )
  
  (vl-catch-all-apply 'vlax-invoke (list tbl_vla 'GenerateLayout))
  
  (setq elist (entget tbl_ent))
  (setq blk_rec_ent (cdr (assoc 343 elist)))
  (if (not blk_rec_ent) (return nil))
  
  (setq blk_obj (vlax-ename->vla-object blk_rec_ent))
  (setq break_space (vla-get-BreakSpacing tbl_vla))
  (setq tol (max (/ break_space 2.0) 0.1))
  
  ;; 1. СБОР ЛИНИЙ С ПОРЯДКОВЫМИ НОМЕРАМИ (Индексами)
  (setq boxes nil idx 0)
  (vlax-for sub blk_obj
    (if (= (vla-get-ObjectName sub) "AcDbLine")
      (progn
        (setq p1 (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint sub))))
        (setq p2 (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint sub))))
        ;; Сохраняем список: (Индекс MinX MinY MaxX MaxY)
        (setq boxes (cons (list idx 
                                (min (car p1) (car p2)) (min (cadr p1) (cadr p2))
                                (max (car p1) (car p2)) (max (cadr p1) (cadr p2))) boxes))
        (setq idx (1+ idx))
      )
    )
  )
  ;; Восстанавливаем хронологический порядок
  (setq boxes (reverse boxes))
  
  ;; 2. СЛИЯНИЕ (с сохранением минимального индекса)
  (defun merge-clusters (lst t_tol / changed new_lst b1 b2 merged remain)
    (setq changed T)
    (while changed
      (setq changed nil new_lst nil)
      (while lst
        (setq b1 (car lst) lst (cdr lst) merged nil remain nil)
        (foreach b2 lst
          ;; Проверка пересечения по координатам (элементы 1, 2, 3, 4 списка)
          (if (not (or (< (+ (nth 3 b1) t_tol) (nth 1 b2)) (< (+ (nth 3 b2) t_tol) (nth 1 b1))
                       (< (+ (nth 4 b1) t_tol) (nth 2 b2)) (< (+ (nth 4 b2) t_tol) (nth 2 b1))))
            (progn
              ;; СЛИЯНИЕ! Берем минимальный Индекс (car) из двух коробок
              (setq b1 (list (min (car b1) (car b2))
                             (min (nth 1 b1) (nth 1 b2)) (min (nth 2 b1) (nth 2 b2))
                             (max (nth 3 b1) (nth 3 b2)) (max (nth 4 b1) (nth 4 b2))))
              (setq merged T changed T)
            )
            (setq remain (cons b2 remain))
          )
        )
        (setq new_lst (cons b1 new_lst) lst remain)
      )
      (setq lst new_lst)
    )
    lst
  )
  
  (setq boxes (merge-clusters boxes tol))
  
  ;; 3. СОРТИРУЕМ ПО ИНДЕКСУ СОЗДАНИЯ АВТОКАДОМ!
  (setq sorted_boxes (vl-sort boxes '(lambda (a b) (< (car a) (car b)))))
  
  ;; 4. Вычисление WCS координат нужного фрагмента
  (if (and (> frag_index 0) (<= frag_index (length sorted_boxes)))
    (progn
      (setq target_box (nth (1- frag_index) sorted_boxes))
      ;; Координаты: Индекс(0) MinX(1) MinY(2) MaxX(3) MaxY(4)
      (setq minX (nth 1 target_box))
      (setq maxY (nth 4 target_box))
      
      ;; Математика перевода из блока в пространство чертежа
      (setq ip (cdr (assoc 10 elist)))
      (setq dir (cdr (assoc 11 elist)))
      (setq norm (cdr (assoc 210 elist)))
      
      (setq y_axis (list (- (* (cadr norm) (caddr dir)) (* (caddr norm) (cadr dir)))
                         (- (* (caddr norm) (car dir)) (* (car norm) (caddr dir)))
                         (- (* (car norm) (cadr dir)) (* (cadr norm) (car dir)))))
                         
      (setq wcs_x (+ (car ip) (* minX (car dir)) (* maxY (car y_axis))))
      (setq wcs_y (+ (cadr ip) (* minX (cadr dir)) (* maxY (cadr y_axis))))
      (setq wcs_z (+ (caddr ip) (* minX (caddr dir)) (* maxY (caddr y_axis))))
      
      (list wcs_x wcs_y wcs_z)
    )
    nil
  )
)

;; =========================================================================
;; ПРОВЕРОЧНАЯ ФУНКЦИЯ
;; =========================================================================
(defun c:TEST_STEP_2 ( / ent tbl p_calc p_user dist )
  (setq ent (entsel "\n[ШАГ 2] Выберите таблицу (разбитую на фрагменты): "))
  (if ent
    (progn
      (setq tbl (car ent))
      (setq p_calc (GetPointTableFrag tbl 2))
      
      (if p_calc
        (progn
          (princ (strcat "\n>>> Скрипт рассчитал точку 2-го фрагмента: " (rtos (car p_calc) 2 3) ", " (rtos (cadr p_calc) 2 3)))
          (setq p_user (getpoint "\n>>> Кликните мышкой ТОЧНО В ЛЕВЫЙ ВЕРХНИЙ УГОЛ ВТОРОГО ФРАГМЕНТА: "))
          
          (if p_user
            (progn
              (setq dist (distance (list (car p_calc) (cadr p_calc)) (list (car p_user) (cadr p_user))))
              (if (< dist 1.0)
                (alert (strcat "УСПЕХ! Координаты идеально совпадают!\n\nГде бы ни был фрагмент, скрипт нашел его."))
                (alert (strcat "ОШИБКА!\nОтклонение: " (rtos dist 2 2)))
              )
            )
          )
        )
        (alert "Ошибка: 2-й фрагмент не найден!")
      )
    )
    (princ "\nТаблица не выбрана.")
  )
  (princ)
)

(c:TEST_STEP_2)