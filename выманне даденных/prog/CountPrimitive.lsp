(vl-load-com)

(defun c:CountAllAcDbEntities ( / acadObj doc blocks counts sortedCounts objName pair total padLen padStr )
  
  ;; Получаем доступ к текущему чертежу и коллекции блоков
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq blocks (vla-get-Blocks doc))
  
  (setq counts nil)
  (setq total 0)

  (princ "\nИдет подсчет примитивов, пожалуйста подождите...\n")

  ;; Перебираем все блоки в чертеже (включая Модель, Листы и пользовательские блоки)
  (vlax-for blk blocks
    ;; Перебираем все примитивы внутри конкретного блока
    (vlax-for ent blk
      ;; Получаем COM-имя объекта (например, "AcDbHatch")
      (setq objName (vla-get-ObjectName ent))
      (setq total (1+ total))
      
      ;; Проверяем, есть ли уже такой тип в нашем списке
      (if (setq pair (assoc objName counts))
        ;; Если есть, увеличиваем счетчик на 1
        (setq counts (subst (cons objName (1+ (cdr pair))) pair counts))
        ;; Если нет, добавляем новый элемент со значением 1
        (setq counts (cons (cons objName 1) counts))
      )
    )
  )

  ;; Если примитивы найдены, сортируем и выводим
  (if counts
    (progn
      ;; Сортировка по количеству (по убыванию)
      (setq sortedCounts
        (vl-sort counts
          '(lambda (a b) (> (cdr a) (cdr b)))
        )
      )
      
      ;; Вывод в консоль
      (textpage) ; Открывает текстовое окно AutoCAD для удобства просмотра
      (princ "\n==============================================")
      (princ "\nТип примитива (AcDb...)          | Количество ")
      (princ "\n==============================================")
      
      (foreach item sortedCounts
        ;; Форматирование для ровных столбцов (выравнивание по 30 символам)
        (setq padLen (- 32 (strlen (car item))))
        (if (< padLen 1) (setq padLen 1))
        (setq padStr "")
        (repeat padLen (setq padStr (strcat padStr " ")))
        
        ;; Печать строки
        (princ (strcat "\n" (car item) padStr "| " (itoa (cdr item))))
      )
      
      (princ "\n==============================================")
      (princ (strcat "\nВСЕГО ПРИМИТИВОВ В БАЗЕ:         | " (itoa total)))
      (princ "\n==============================================\n")
    )
    (princ "\nЧертеж пуст.")
  )
  (princ)
)

;; === АВТОМАТИЧЕСКИЙ ЗАПУСК ===
;; Вызываем функцию сразу после загрузки файла
(c:CountAllAcDbEntities)
(princ "\nСкрипт загружен и выполнен. Для повторного запуска введите CountAllAcDbEntities.")
(princ)