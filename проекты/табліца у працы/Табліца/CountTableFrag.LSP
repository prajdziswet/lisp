(vl-load-com)

;; =========================================================================
;; ШАГ 1: Функция CountTableFrag
;; ПРИНИМАЕТ: Имя примитива (ENAME) или VLA-объект таблицы.
;; ВОЗВРАЩАЕТ: Целое число (количество визуальных фрагментов таблицы).
;; ОПИСАНИЕ: Анализирует скрытый блок таблицы и группирует геометрию в кластеры.
;; =========================================================================
(defun CountTableFrag (tbl_arg / tbl_vla tbl_ent elist blk_rec_ent blk_obj break_space tol boxes objName p1 p2 minPt maxPt merge-clusters)
  
  ;; 1. Обработка входящего аргумента
  (if (= (type tbl_arg) 'VLA-OBJECT)
    (setq tbl_vla tbl_arg tbl_ent (vlax-vla-object->ename tbl_vla))
    (setq tbl_ent tbl_arg tbl_vla (vlax-ename->vla-object tbl_ent))
  )
  
  ;; 2. Форсируем генерацию графики, чтобы блок 100% существовал
  (vl-catch-all-apply 'vlax-invoke (list tbl_vla 'GenerateLayout))
  
  ;; 3. Находим скрытый блок таблицы (Группа 343)
  (setq elist (entget tbl_ent))
  (setq blk_rec_ent (cdr (assoc 343 elist)))
  
  ;; Если блока нет, значит таблица пустая или не сгенерирована — считаем как 1 фрагмент
  (if (not blk_rec_ent)
    (return 1)
  )
  
  (setq blk_obj (vlax-ename->vla-object blk_rec_ent))
  
  ;; Узнаем системный интервал между фрагментами
  (setq break_space (vla-get-BreakSpacing tbl_vla))
  ;; Допуск для слияния (половина интервала разрыва)
  (setq tol (/ break_space 2.0))
  (if (< tol 0.1) (setq tol 0.1))
  
  ;; 4. Собираем габаритные контейнеры всех линий и текстов внутри блока
  (setq boxes nil)
  (vlax-for sub blk_obj
    (setq objName (vla-get-ObjectName sub))
    (cond
      ((= objName "AcDbLine")
       (setq p1 (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint sub))))
       (setq p2 (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint sub))))
       (setq boxes (cons (list (min (car p1) (car p2)) (min (cadr p1) (cadr p2))
                               (max (car p1) (car p2)) (max (cadr p1) (cadr p2))) boxes))
      )
      ((= objName "AcDbMText")
       (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list sub 'minPt 'maxPt))))
         (progn
           (setq p1 (vlax-safearray->list minPt))
           (setq p2 (vlax-safearray->list maxPt))
           (setq boxes (cons (list (car p1) (cadr p1) (car p2) (cadr p2)) boxes))
         )
       )
      )
    )
  )
  
  ;; 5. Внутренняя функция: Слияние пересекающихся контейнеров в единые фрагменты
  (defun merge-clusters (lst t_tol / changed new_lst b1 b2 merged remain)
    (setq changed T)
    (while changed
      (setq changed nil)
      (setq new_lst nil)
      (while lst
        (setq b1 (car lst))
        (setq lst (cdr lst))
        (setq merged nil)
        (setq remain nil)
        (foreach b2 lst
          ;; Если контейнеры пересекаются или касаются (с учетом допуска)
          (if (not (or (< (+ (caddr b1) t_tol) (car b2))   ; maxX1 < minX2
                       (< (+ (caddr b2) t_tol) (car b1))   ; maxX2 < minX1
                       (< (+ (cadddr b1) t_tol) (cadr b2)) ; maxY1 < minY2
                       (< (+ (cadddr b2) t_tol) (cadr b1)))) ; maxY2 < minY1
            (progn
              ;; Объединяем их в один большой контейнер (фрагмент)
              (setq b1 (list (min (car b1) (car b2))
                             (min (cadr b1) (cadr b2))
                             (max (caddr b1) (caddr b2))
                             (max (cadddr b1) (cadddr b2))))
              (setq merged T)
              (setq changed T)
            )
            ;; Иначе откладываем для следующей проверки
            (setq remain (cons b2 remain))
          )
        )
        (setq new_lst (cons b1 new_lst))
        (setq lst remain)
      )
      (setq lst new_lst)
    )
    lst
  )
  
  ;; 6. Возвращаем количество получившихся кластеров (фрагментов)
  (if boxes
    (length (merge-clusters boxes tol))
    1
  )
)

;; =========================================================================
;; ПРОВЕРОЧНАЯ ФУНКЦИЯ ДЛЯ ШАГА 1
;; =========================================================================
(defun c:TEST_STEP_1 ( / ent count )
  (setq ent (entsel "\n[ШАГ 1] Выберите таблицу для подсчета фрагментов: "))
  (if ent
    (progn
      (setq count (CountTableFrag (car ent)))
      (princ (strcat "\n\n>>> УСПЕХ! Найдено фрагментов таблицы: " (itoa count) " <<<"))
    )
    (princ "\nТаблица не выбрана.")
  )
  (princ)
)

;; Автозапуск при загрузке скрипта
(c:TEST_STEP_1)