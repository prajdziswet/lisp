(defun c:SelectNotZ0 ( / ss ss_result i ent vla-obj minPt maxPt zMin zMax)
  (vl-load-com)
  ;; Выбираем вообще все объекты на чертеже
  (if (setq ss (ssget "X"))
    (progn
      ;; Создаем пустой набор для результатов
      (setq ss_result (ssadd))
      
      ;; Быстрый перебор
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq vla-obj (vlax-ename->vla-object ent))
        
        ;; Пытаемся получить габариты (у некоторых объектов типа бесконечных лучей их нет)
        (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list vla-obj 'minPt 'maxPt))))
          (progn
            ;; Извлекаем координаты Z из габаритов
            (setq zMin (caddr (vlax-safearray->list minPt))
                  zMax (caddr (vlax-safearray->list maxPt)))
            
            ;; Если минимальный Z или максимальный Z не равны нулю (с небольшой погрешностью)
            (if (or (> (abs zMin) 1e-8) (> (abs zMax) 1e-8))
              (ssadd ent ss_result) ; Добавляем объект в итоговый набор
            )
          )
        )
      )
      ;; Выделяем найденные объекты
      (if (> (sslength ss_result) 0)
        (progn
          (sssetfirst nil ss_result)
          (princ (strcat "\nВыбрано объектов не на Z=0: " (itoa (sslength ss_result))))
        )
        (princ "\nВсе объекты лежат строго на Z=0.")
      )
    )
  )
  (princ)
)