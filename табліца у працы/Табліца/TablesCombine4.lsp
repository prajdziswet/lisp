(vl-load-com)

;; ===========================================================================
;; ОСНОВНАЯ ФУНКЦИЯ: TablesCombine
;; Назначение: Координация всех шагов объединения таблиц
;; ===========================================================================
(defun c:TablesCombine (/ *error* activeDoc tablesList clusterData)
  
  ;; --- Обработчик ошибок ---
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nОстановка: " msg))
    )
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ)
  )

  (setq activeDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark activeDoc)

  ;; --- ВЫПОЛНЕНИЕ ШАГОВ ---
  
  ;; TC:GetTables (Шаг 1) - внутри зашит выход при недостаточном кол-ве таблиц
  (setq tablesList (TC:GetTables))

  ;; Если мы здесь, значит таблиц > 1
  (princ (strcat "\nПодготовка к анализу " (itoa (length tablesList)) " таблиц..."))
  
  ;; TC:ClusterAnalysis (Шаг 2)
  (setq clusterData(TC:ClusterAnalysis tablesList))
  
  ;; UserInterface (Шаг 3)
  (setq uiFlags (TC:UserInterface clusterData))
  ;; uiFlags теперь имеет вид: (T T) или (nil T) и т.д.
  
  ;; ExtractData (Шаг 4)
  (setq data (TC:ExtractData clusterData uiFlags))
  ;; ExtractData
  
  ;; AnalizHeader (Шаг 5)
  (setq data (TC:AnalizHeader data))
  ;; ExtractData
  
   ;; AddPureGap (+доп Шаг 5)
  (setq data (TC:AddPureGap data))
  ;; ExtractData 
  
   ;; CreateTables (Шаг 6 созданіе табліцы)
  (TC:CreateTables data uiFlags)
  ;; ExtractData 
  
  ;(PrintStep5Cluster data)
  

  (vla-EndUndoMark activeDoc)
  (princ)
)

;; ===========================================================================
;; ФУНКЦИЯ ШАГА 1: TC:GetTables
;; Назначение: Сбор объектов ACAD_TABLE, их фильтрация и первичная сортировка.
;;             Проверка на минимальное количество для объединения.
;; Принимает:  Ничего
;; Выдает:     Список VLA-объектов [Y desc, X asc]. Если < 2 - прерывает программу.
;; Структура:  (vla-obj1 vla-obj2 ... vla-objN)
;; ===========================================================================
(defun TC:GetTables (/ ss i lst ent vla-obj sort-tables-by-coords)
  (princ "\nЗапуск функции TC:GetTables (Шаг 1)...")

  ;; Внутренняя подфункция сортировки
  (defun sort-tables-by-coords (lst / fuzzy y1 y2 x1 x2)
    (setq fuzzy 5.0) 
    (vl-sort lst
      '(lambda (a b / y1 y2 x1 x2)
         (setq y1 (cadr (cdr a)) y2 (cadr (cdr b)) ;; Извлекаем Y из (vla . (X Y Z))
               x1 (car (cdr a))  x2 (car (cdr b))) ;; Извлекаем X
         (if (not (equal y1 y2 fuzzy))
           (> y1 y2) 
           (< x1 x2) 
         )
      )
    )
  )

  (setq ss (ssget "_I" '((0 . "ACAD_TABLE"))))
  (if (not ss)
    (progn
      (princ "\nВыберите две или более таблицы для объединения: ")
      (setq ss (ssget '((0 . "ACAD_TABLE"))))
    )
  )

  (if (or (not ss) (<= (sslength ss) 1))
    (progn
      (princ "\nОшибка: Для объединения необходимо выбрать минимум 2 таблицы.")
      (exit)
    )
  )

  (setq i 0 lst '())
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq vla-obj (vlax-ename->vla-object ent))
    ;; Формируем список пар: (vla-объект . список-координат)
    (setq lst (cons (cons vla-obj (vlax-get vla-obj 'InsertionPoint)) lst))
    (setq i (1+ i))
  )
  
  (setq lst (sort-tables-by-coords lst))
  (mapcar 'car lst) ;; Возвращаем только VLA-объекты
)
;; ===========================================================================
;; ФУНКЦИЯ ШАГА 2: TC:ClusterAnalysis
;; Назначение: Группировка таблиц в кластеры по столбцам, целым масштабам и рядам.
;; Принимает:  Список VLA-объектов (результат Шага 1)
;; Выдает:     Список кластеров: 
;;             ( ((Cols M S1 S2 W_Type AvgFragH) (vla-obj1 vla-obj2 ...)) ... )
;; ===========================================================================
(defun TC:ClusterAnalysis (tablesList / clusters get-table-params split-by-y)
  (princ "\nЗапуск функции TC:ClusterAnalysis (Шаг 2)...")

  ;; --- Подфункция 1: Анализ параметров таблицы ---
  (defun get-table-params (vla-obj / w cols h_cell p1 p2 m1 m2 diff1 diff2 m s1 s2 w_type frag_h)
    (setq w (vla-get-Width vla-obj)
          cols (vla-get-Columns vla-obj)
          s1 2.5
          s2 3.5
          frag_h (vla-get-Height vla-obj) 
    )

    ;; 1. Расчет отклонений для стандартов
    (setq p1 (/ w 185.0)
          p2 (/ w 395.0)
          m1 (fix (+ p1 0.5)) ;; Округляем до целого
          m2 (fix (+ p2 0.5))
          diff1 (abs (- p1 m1)) 
          diff2 (abs (- p2 m2))
    )

    (cond
      ;; Выбираем 185, если ближе и в допуске 8%
      ((and (< diff1 diff2) (<= diff1 0.08) (> m1 0))
       (setq m m1 w_type 185.0)) ;; M теперь целое (int)
      
      ;; Выбираем 395, если ближе и в допуске 7%
      ((and (<= diff2 0.07) (> m2 0))
       (setq m m2 w_type 395.0))

      ;; Случай Г: По тексту ячейки (0,0)
      (t
       (setq h_cell (vl-catch-all-apply 'vla-GetCellTextHeight (list vla-obj 0 0)))
       (if (vl-catch-all-error-p h_cell) (setq h_cell 2.5))

       (setq p1 (/ h_cell 2.5)
             p2 (/ h_cell 3.5)
             m1 (fix (+ p1 0.5))
             m2 (fix (+ p2 0.5))
       )
       (if (< (abs (- p1 m1)) (abs (- p2 m2)))
         (setq m (max 1 m1))
         (setq m (max 1 m2))
       )
       (setq w_type (/ w (float m)))
      )
    )
    ;; Возвращаем параметры (M принудительно целое)
    (list cols (fix m) s1 s2 w_type frag_h)
  )

  ;; --- Подфункция 2: Разделение по Y (ряды) ---
  (defun split-by-y (lst params / gap result current_sub last_y cur_y)
    ;; 10 * 2.5 * M (целое)
    (setq gap (* 10.0 (nth 2 params) (float (nth 1 params)))) 
    (setq result nil current_sub nil last_y nil)
    
    (foreach obj lst
      (setq cur_y (cadr (vlax-get obj 'InsertionPoint)))
      (if (or (null last_y) (<= (abs (- last_y cur_y)) gap))
        (setq current_sub (cons obj current_sub))
        (progn
          (setq result (cons (reverse current_sub) result))
          (setq current_sub (list obj))
        )
      )
      (setq last_y cur_y)
    )
    (if current_sub (setq result (cons (reverse current_sub) result)))
    (reverse result)
  )

  ;; --- ОСНОВНАЯ ЛОГИКА ---
  (setq clusters '())
  
  (foreach tbl tablesList
    (setq p (get-table-params tbl))
    (setq found nil)
    (setq clusters 
      (mapcar 
        '(lambda (c / cp)
           (setq cp (car c))
           ;; Сравниваем: Кол-во столбцов и целое число масштаба
           (if (and (= (car cp) (car p))          ;; Столбцы
                    (= (cadr cp) (cadr p))       ;; Целый масштаб
                    (equal (nth 4 cp) (nth 4 p) 2.0)) ;; Тип ширины
             (progn 
               (setq found t) 
               ;; Усредняем высоту фрагмента
               (setq new_avg_h (/ (+ (nth 5 cp) (nth 5 p)) 2.0))
               (list (list (car cp) (cadr cp) (nth 2 cp) (nth 3 cp) (nth 4 cp) new_avg_h) 
                     (append (cadr c) (list tbl)))
             )
             c
           )
        ) 
        clusters
      )
    )
    (if (not found) (setq clusters (cons (list p (list tbl)) clusters)))
  )

  ;; Финальное разбиение на ряды
  (setq final_clusters '())
  (foreach c clusters
    (setq sub_lists (split-by-y (cadr c) (car c)))
    (foreach sl sub_lists
      (setq final_clusters (cons (list (car c) sl) final_clusters))
    )
  )

  (princ (strcat "\nОпределено групп: " (itoa (length final_clusters))))
  (reverse final_clusters)
)

;; ===========================================================================
;; ФУНКЦИЯ ШАГА 3: TC:UserInterface
;; Назначение: Интерактивная настройка параметров (Цвет, Направление)
;; Принимает:  clusterData (из Шага 2)
;; Выдает:     Список флагов (T/Nil): (KeepColor LeftToRight)
;; ===========================================================================
(defun TC:UserInterface (clusterData / multi_table keepColor l2r choice init_str prompt_str)
  (setvar "DYNMODE" 3)
  
  (setq multi_table nil)
  (foreach c clusterData
    (if (> (length (cadr c)) 1) (setq multi_table t))
  )

  (setq keepColor nil l2r t choice "Start")

  (while (not (or (equal choice "Продолжить") (= choice nil)))
    
    (setq init_str "Продолжить Сбросить-цвет Оставить-цвет сЛево-направо спРаво-налево") 
    (initget init_str)

    (setq prompt_str 
      (strcat "\nПродолжить или изменить: (" 
              (if keepColor "Оставить_цвет," "Сбросить_цвет,")
              (if l2r "Слево-направо" "Справо-налево") ") ["
              "Продолжить/" 
              (if keepColor "Сбросить-цвет" "Оставить-цвет")
              (if multi_table 
                (strcat "/" (if l2r "сЛево-направо" "спРаво-налево"))
                ""
              )
              "] <Продолжить>: ")
    )

    (setq choice (getkword prompt_str))

    ;; Обработка
    (cond
      ((or (equal choice "Оставить-цвет") (equal choice "Сбросить-цвет")) (setq keepColor (not keepColor))) ;; Инверсия цвета    
      ((or (equal choice "сЛево-направо")  (equal (strcase choice) "спРаво-налево")) (setq l2r (not l2r)))  ;; Справа-налево
    )
  )
  
  (list keepColor l2r)
)
;; =========================Получение фрагметов и высоты============================
(defun GetTableFragments (obj / res i frags numFrags)
  (setq i 0 
        frags nil)
  
  ;; Универсальный цикл: читаем высоты фрагментов через скрытый метод API
  ;; Работает для любого направления разбиения (вправо, влево, вниз)
  (while (and 
           ;; Пробуем получить высоту фрагмента с индексом i
           (not (vl-catch-all-error-p (setq res (vl-catch-all-apply 'vla-GetBreakHeight (list obj i)))))
           (numberp res)
           (> res 1e-3) ; Крутим цикл, пока AutoCAD не вернет 0.0 (конец разрывов)
         )
    ;; Округляем лимит высоты по правилам математики (например, 57.6 -> 58, 92.03 -> 92)
    (setq frags (cons (fix (+ res 0.5)) frags))
    (setq i (1+ i))
  )
  
  ;; Формируем итоговый ответ
  (if frags
    (progn
      (setq frags (reverse frags)) ; Разворачиваем список в правильный порядок
      (setq numFrags (length frags))
    )
    (progn
      ;; Если метод вернул 0.0 сразу (таблица не разбита или метод не сработал)
      (setq numFrags 1)
      (setq frags (list (fix (+ (vla-get-Height obj) 0.5))))
    )
  )

  ;; Возвращаем результат: (Кол-во (Высота1 Высота2 ...))
  (list numFrags frags)
)
;; ===========================================================================
;; ФУНКЦИЯ ШАГА 4: TC:ExtractData
;; Назначение: Сбор данных из ячеек, геометрии фрагментов и объединений.
;; Принимает:  clusterData (Шаг 2), uiFlags (Шаг 3: (KeepColor L2R))
;; Выдает:     Обновленный список кластеров с данными внутри.
;; ===========================================================================
(defun TC:ExtractData (clusterData uiFlags / keepColor l2r finalData frags)
  (princ "\nЗапуск функции TC:ExtractData (Шаг 4)...")
  
  (setq keepColor (car uiFlags)
        l2r       (cadr uiFlags))

  (setq finalData 
    (mapcar 
      '(lambda (cluster / params tables mergeReg colWidths updatedTables)
         (setq params (car cluster)
               tables (cadr cluster))

         ;; 1. Учет направления (Инверсия списка, если Справа-налево)
         (if (not l2r) (setq tables (reverse tables)))

         ;; 2. Сбор ширин столбцов (берем из первой таблицы кластера)
         (setq colWidths '())
         (setq firstTbl (car tables))
         (repeat (car params) ;; Cols
           (setq colWidths (cons (vla-GetColumnWidth firstTbl (length colWidths)) colWidths))
         )
         ;; Добавляем ширины в параметры кластера (7-й элемент)
         (setq params (append params (list (reverse colWidths))))

         ;; 3. Глубокий сбор данных по каждой таблице
         (setq updatedTables 
           (mapcar 
             '(lambda (vla-obj / rows cols hList row i j cellData locMerge 
                                repeatLabels isBreak fragsCount)
                (setq rows (vla-get-Rows vla-obj)
                      cols (vla-get-Columns vla-obj)
                      hList '()
                      cellData '()
                      locMerge '()
                )

                ;; А. Анализ фрагментов и переноса заголовков
		;(vlax-dump-object vla-obj)
                (setq isBreak (vla-get-BreaksEnabled vla-obj))
                (setq repeatLabels (vla-get-RepeatTopLabels vla-obj))
                
                (if (= isBreak :vlax-true)
                  (progn
                    ;; Если разбивка включена, собираем высоты фрагментов
                    (setq frags (GetTableFragments vla-obj))
					(setq fragsCount (car frags))
                    ;; Примечание: в ActiveX это может быть сложнее, берем общую высоту 
                    ;; и делим на кол-во фрагментов или считываем через get-Height
                    (setq hList (last frags)) 
                  )
                  (setq hList (list (vla-get-Height vla-obj)))
                )

                ;; Б. Сбор данных ячеек (Список_3)
                (setq i 0)
                (repeat rows
                  (setq j 0)
                  (repeat cols
                    ;; Извлекаем данные ячейки
                    (setq cellData 
                      (cons 
                        (list i j 
                          (vla-GetText vla-obj i j) 
                          (vla-GetCellAlignment vla-obj i j)
                          (if keepColor (vla-GetHasCustomFormat vla-obj i j) nil) ;; Упростим до флага или TrueColor
                        ) 
                        cellData
                      )
                    )
                    
;; В. Сбор локальных объединений (Список_4)
(setq minRow 0 maxRow 0 minCol 0 maxCol 0) ;; Инициализация переменных
(if (and 
      (= (vla-IsMergedCell vla-obj i j 'minRow 'maxRow 'minCol 'maxCol) :vlax-true)
      (= i minRow) 
      (= j minCol)
    )
    ;; Если текущая ячейка (i, j) является левой верхней в объединении:
    (setq locMerge (cons (list minRow minCol maxRow maxCol) locMerge))
)
                    (setq j (1+ j))
                  )
                  (setq i (1+ i))
                )

                ;; Возвращаем расширенный список таблицы
                (list vla-obj hList rows (reverse cellData) (reverse locMerge) repeatLabels)
             )
             tables
           )
         )

         ;; Итоговая структура кластера: (Параметры_с_Ширинами  Массив_Таблиц)
         (list params updatedTables)
      )
      clusterData
    )
  )

  (princ (strcat "\nЭкспорт завершен. Обработано кластеров: " (itoa (length finalData))))
  finalData
)

  ;; ----------------------------------------------------
  ;; 1. Подфункция: CleanerTextAllFormat (Очистка текста)
  ;; ----------------------------------------------------
(defun CleanerTextAllFormat ( str / regex )
    (if (= (type str) 'STR)
      (progn
        ;; Операция 0: Приоритетное удаление \P и \n (переносы строк AutoCAD)
        (while (vl-string-search "\\P" str)
          (setq str (vl-string-subst "" "\\P" str))
        )
        (while (vl-string-search "\n" str)
          (setq str (vl-string-subst "" "\n" str))
        )

        ;; Операция 1: Удаление блоков форматирования (\C, \W, \f, \px и т.д.) через RegExp
        (setq regex (vlax-create-object "VBScript.RegExp"))
        (vlax-put-property regex 'Global :vlax-true)
        (vlax-put-property regex 'IgnoreCase :vlax-true)
        (vlax-put-property regex 'Pattern "\\\\[CcfWp][^;]*;")
        (setq str (vlax-invoke regex 'Replace str ""))
        (vlax-release-object regex)

        ;; Операция 2: Логика экранирования скобок
        ;; Маскируем \{ и \}, удаляем технические { }, возвращаем текстовые скобки обратно
        (setq str (vl-string-subst "@@LBR@@" "\\{" str))
        (setq str (vl-string-subst "@@RBR@@" "\\}" str))
        
        (while (vl-string-search "{" str) (setq str (vl-string-subst "" "{" str)))
        (while (vl-string-search "}" str) (setq str (vl-string-subst "" "}" str)))
        
        (setq str (vl-string-subst "{" "@@LBR@@" str))
        (setq str (vl-string-subst "}" "@@RBR@@" str))

        ;; Операция 3: Удаление пробелов, табуляций и дефисов (переносов)
        (while (vl-string-search " " str) (setq str (vl-string-subst "" " " str)))
        (while (vl-string-search "\t" str) (setq str (vl-string-subst "" "\t" str)))
        (while (vl-string-search "-" str) (setq str (vl-string-subst "" "-" str)))
        
        str ; Возвращаем очищенную "монолитную" строку
      )
      "" ; Если на вход пришла не строка, возвращаем пустоту
    )
  )
  
  ;; Сравнение списков строк: расчет среднего % совпадения ячеек (порог 70%)
(defun ListComparator ( list1 list2 / sum i res Comparator)
    ;; ----------------------------------------------------
  ;; 2. Подфункция: Comparator (Компаратор)
  ;; ----------------------------------------------------
  (defun Comparator ( strRef strTarget / len1 len2 shortStr longStr lenShort lenLong pct )
    (setq len1 (strlen strRef) 
          len2 (strlen strTarget))
    
    ;; Определяем, какая строка короче
    (if (<= len1 len2)
      (setq shortStr strRef longStr strTarget lenShort len1 lenLong len2)
      (setq shortStr strTarget longStr strRef lenShort len2 lenLong len1)
    )
    
    ;; 1. Проверяем, является ли короткая строка подстрокой длинной
    (if (and (> lenShort 0) (> lenLong 0) (vl-string-search shortStr longStr))
      (progn
        ;; 2. Рассчитываем процент совпадения
        (setq pct (* (/ (float lenShort) (float lenLong)) 100.0))
        
        ;; Возвращаем результат в зависимости от порога в 30%
        (if (> pct 30.0)
          (list T pct)
          (list nil 0)
        )
      )
      (list nil 0) ; Если не подстрока
    )
  )
  
  (if (= (length list1) (length list2))
    (progn
      (setq sum 0.0 i 0)
      (while (< i (length list1))
        (setq res (Comparator (nth i list1) (nth i list2)))
        (if (car res)
          (setq sum (+ sum (cadr res)))
          (setq sum (+ sum 0.0))
        )
        (setq i (1+ i))
      )
      ;; Проверка: средний процент по всем ячейкам > 70.0
      (if (> (/ sum (float (length list1))) 70.0) T nil)
    )
    nil ; Если разное количество ячеек в шапке
  )
)

  
  ;; ====================================================
  ;; ОСНОВНОЕ ТЕЛО ШАГА 5 AnalizHeader
  ;; ====================================================
(defun TC:AnalizHeader ( clusters /  GeometryDetector ContentDetector ClusterDetector updatedClusters GetHeaderHeigh
IdentifyMasterHeader TransformCluster)
  ;; Подгружаем расширения ActiveX (нужно для RegExp)
  (vl-load-com)

;; ===========================================================================
;; БЛОК ДЕТЕКТОРОВ ШАПКИ (3.1 -> 3.2 -> 3.0) И ЛОГИКА КЛАСТЕРА (4)
;; ===========================================================================

;; 3.1 System Detector (Системный определитель)
(defun SystemDetector ( fragment / vlaTable rows headerCount i rType repeatFlag exitLoop countrepeat)
  (setq vlaTable (car fragment)
        rows (nth 2 fragment)
        headerCount 0
        i 0
        exitLoop nil)
  
  (while (and (< i rows) (not exitLoop))
    (setq rType (vla-GetRowType vlaTable i))
    (if (or (= rType 2) (= rType 4))
      (setq headerCount (1+ headerCount)
            i (1+ i))
      (setq exitLoop T) 
    )
  )

  (setq repeatFlag (if (= (nth 5 fragment) :vlax-true) T nil))
  (if repeatFlagT (setq countrepeat (length (nth 1 fragment))))

  (if (> headerCount 0)
    ;; Внутренний возврат для цепочки: добавляем инфо-блок 
    (list T headerCount repeatFlag countrepeat)
    nil
  )
)

;; 3.2 GEOMETRY DETECTOR: Анализ границ шапки по объединениям (Merge) ячеек
(defun GeometryDetector ( fragment / locMerge maxRow exitLoop i m)
  (setq locMerge (nth 4 fragment)
        maxRow 0
        exitLoop nil 
        i 0)

  (while (and (< i (length locMerge)) (not exitLoop))
    (setq m (nth i locMerge))
    (if (= (car m) 0) 
      (progn
        (if (and (= (cadr m) 0) (= maxRow 0))
          (setq maxRow (nth 2 m))
          (if (> (nth 2 m) maxRow) (setq exitLoop T))
        )
      )
      (setq exitLoop T) 
    )
    (setq i (1+ i))
  )

  (if (and exitLoop (> (car (nth (1- i) locMerge)) 0))
    (setq exitLoop nil) 
  )

  (if (or exitLoop (and (= maxRow 0) (not (= (caar locMerge) 0))))
    (list nil 1 nil nil)
    ;; Внутренний возврат для цепочки: добавляем инфо-блок
    (list nil (1+ maxRow) nil nil) 
  )
)

;;; Функция: TC:GetHeaderHeight
;;; Назначение: Расчет физической высоты шапки таблицы
(defun GetHeaderHeight ( vlaTable headerRows / h total i hList )
  (setq total 0.0
        i 0
        hList nil)
  (if (and vlaTable (> headerRows 0))
    (repeat headerRows
      (setq h (vla-GetRowHeight vlaTable i))
      (setq h (fix (+ h 0.5))) ;; Округляем до целого: 8.4 -> 8, 8.6 -> 9
      (setq hList (cons h hList))
      (setq total (+ total h))
      (setq i (1+ i)))
  )
  ;; Возвращаем (ОбщаяВысота (СписокВысот))
  (list total (reverse hList))
)
;; 3.0 CONTENT DETECTOR: Валидация текста в "активных" ячейках
(defun ContentDetector (modFragment info / headerRows isSystem cellData locMerge cleanTexts rawHeaderCells emptyCount excludedCells r c txt i cell m )
  (setq cellData (nth 3 modFragment)
        locMerge (nth 4 modFragment)
		headerRows (nth 1 info)
        isSystem (car info)
        cleanTexts nil
        rawHeaderCells nil
        emptyCount 0
        excludedCells nil
        i 0)

  ;; 1. Собираем исключенные ячейки (только для строк шапки)
  ;; Используем while, так как locMerge отсортирован
  (while (and (< i (length locMerge)) 
              (< (car (setq m (nth i locMerge))) headerRows))
    (setq r (car m))
    (while (and (<= r (nth 2 m)) (< r headerRows))
      (setq c (cadr m))
      (while (<= c (nth 3 m))
        (if (not (and (= r (car m)) (= c (cadr m))))
          (setq excludedCells (cons (cons r c) excludedCells)))
        (setq c (1+ c)))
      (setq r (1+ r)))
    (setq i (1+ i)))

  ;; 2. Цикл по ячейкам с досрочным выходом
  (setq i 0)
  (while (and (< i (length cellData))
              (< (setq r (car (setq cell (nth i cellData)))) headerRows))
    (setq c (cadr cell)
          txt (nth 2 cell))
    
    (if (not (member (cons r c) excludedCells))
      (progn
        (setq txt (CleanerTextAllFormat txt))
        (if (= txt "") (setq emptyCount (1+ emptyCount)))
        (setq cleanTexts (cons txt cleanTexts))))
    (setq i (1+ i)))

  ;; 3. Проверка условий заполненности
  (if (or (and isSystem (<= emptyCount 1))
          (and (not isSystem) (= emptyCount 0)))
    (append info (list (reverse cleanTexts)))
    nil
  )
)

;пошук супадзенняў шапак
(defun IdentifyMasterHeader (resList / statusList i masterItem masterText totalCount matchIdxs j curItem curText foundMaster finalMasterText)
  (setq i 0)
  (setq foundMaster nil)
  (setq finalMasterText nil)

  ;; 1. Инициализация statusList: (nil 1 nil 3 4 nil ...)
  (setq statusList 
    (mapcar '(lambda (x) 
               (setq i (1+ i)) 
               (if x (1- i) nil)) 
            resList)
  )

  ;; 2. Основной цикл поиска
  (setq i 0)
  (while (< i (length resList))
    (setq masterItem (nth i resList))
    
    ;; Проверяем только если это потенциальный кандидат (индекс в statusList)
    (if (and masterItem (numberp (nth i statusList)))
      (progn
        (setq masterText (last masterItem)
              totalCount (if (and (nth 2 masterItem) (nth 3 masterItem)) (nth 3 masterItem) 1)
              matchIdxs (list i)
              j (1+ i))

        ;; 3. Внутренний цикл: ищем все такие же тексты дальше по списку
        (while (< j (length resList))
          (setq curItem (nth j resList))
          (if curItem
            (progn
              (setq curText (last curItem))
              (if (ListComparator masterText curText) ;; Сравниваем тексты
                (setq matchIdxs (cons j matchIdxs)
                      totalCount (+ totalCount (if (and (nth 2 curItem) (nth 3 curItem)) (nth 3 curItem) 1)))
              )
            )
          )
          (setq j (1+ j))
        )

        ;; 4. Вердикт: Шапка это или нет?
        ;; Условие: (Кол-во > 1)
        (if (> totalCount 1)
          (progn
            ;; ПРОВЕРКА НА КОНФЛИКТ
            ;; Если мы уже находили ДРУГУЮ шапку ранее
            (if (and foundMaster (not (equal masterText finalMasterText)))
              (progn
                (alert "Конфликт структур! В одном кластере обнаружены разные типы шапок.")
                (exit)
              )
            )

            ;; Утверждаем шапку
            (setq foundMaster T
                  finalMasterText masterText)
            
            ;; Записываем (ID Count) во все найденные позиции
            (foreach idx matchIdxs
              (setq statusList (subst-nth idx (list i totalCount) statusList))
            )
          )
          ;; Иначе: встретилась 1 раз (даже если системная) -> зануляем
          (setq statusList (subst-nth i nil statusList))
        )
      )
    )
    (setq i (1+ i))
  )
  statusList
)

;; Вспомогательная функция для замены элемента в списке по индексу
(defun subst-nth (idx val lst / i)
  (setq i -1)
  (mapcar '(lambda (x) (if (= (setq i (1+ i)) idx) val x)) lst)
)
;Функция трансформации кластера
;; Функция трансформации кластера
(defun TransformCluster (cluster resList masterInfo / 
                         params tableList statusList masterIdx masterRes 
                         headerRows masterFrag hListHeader rawCells headerMerge
                         i frag isHeaderHere dataRows curHeaderRows totalHeaderHeight
                         globalOffset vlaObjects allHLists allCellData allLocMerge
                         newRow newTop newBottom headerData tmpLst
                         curRes isSystemHeader isRepeatLabels fragHList headerInfo)

  (setq params (car cluster)
        tableList (cadr cluster)
        statusList masterInfo
        masterIdx nil
        masterRes nil
        vlaObjects nil allHLists nil allCellData nil allLocMerge nil
        globalOffset 0)

  ;; 1. Поиск индекса Master-шапки
  (setq i 0)
  (while (and (< i (length statusList)) (not masterIdx))
    (if (listp (nth i statusList))
      (setq masterIdx (car (nth i statusList))
            masterRes (nth masterIdx resList))
    )
    (setq i (1+ i))
  )

  ;; 2. Сбор геометрии шапки
  (if masterIdx
    (progn
      (setq headerRows (nth 1 masterRes)) 
      (setq masterFrag (nth masterIdx tableList))
      
      (setq headerInfo (GetHeaderHeight (car masterFrag) headerRows))
      (setq totalHeaderHeight (car headerInfo)
            hListHeader (cadr headerInfo))
      
      (setq rawCells nil)
      (foreach c (nth 3 masterFrag)
        (if (and (< (car c) headerRows) (nth 2 c) (/= (nth 2 c) ""))
          (setq rawCells (cons c rawCells))
        )
      )
      
      (setq headerMerge nil)
      (foreach m (nth 4 masterFrag)
        (if (< (car m) headerRows) 
          (setq headerMerge (cons m headerMerge))
        )
      )
      
      (setq rawCells (reverse rawCells)
            headerMerge (reverse headerMerge))
    )
    (setq headerRows 0)
  )

  ;; 3. Сборка данных всех фрагментов
  (setq i 0)
  (while (< i (length tableList))
    (setq frag (nth i tableList)
          curRes (nth i resList)
          isSystemHeader (if curRes (nth 0 curRes) nil)   ;; Параметр 0 (Системная)
          isRepeatLabels (if curRes (nth 2 curRes) nil)   ;; Параметр 2 (Повтор меток)
          fragHList (nth 1 frag) ;; Список исходных высот (числа)
          isHeaderHere (listp (nth i statusList))
          curHeaderRows (if isHeaderHere headerRows 0)
          dataRows (- (nth 2 frag) curHeaderRows))
          
    (setq vlaObjects (cons (nth 0 frag) vlaObjects))
    
    ;; --- Сбор высот (hList) ---
    (cond
      ;; 1. Системная + Повтор + Есть шапка: Просто забираем и округляем
      ((and isSystemHeader isRepeatLabels isHeaderHere)
       (foreach h fragHList
         (setq allHLists (append allHLists (list (fix (+ h 0.5)))))
       )
      )
      ;; 2. Только есть шапка (без спец. флагов): Первое число как есть, к остальным + высота шапки
      (isHeaderHere
       (setq allHLists (append allHLists (list (fix (+ (car fragHList) 0.5)))))
       (foreach h (cdr fragHList)
         (setq allHLists (append allHLists (list (fix (+ h totalHeaderHeight 0.5)))))
       )
      )
      ;; 3. Все остальные случаи (Шапки нет): К каждому числу прибавляем высоту шапки
      (t
       (foreach h fragHList
         (setq allHLists (append allHLists (list (fix (+ h totalHeaderHeight 0.5)))))
       )
      )
    )
    
    ;; --- Пересчет ячеек данных (с учетом смещения на мастер-шапку) ---
    (foreach c (nth 3 frag)
      ;; ВАЖНО: Используем >= чтобы не терять самую первую строку данных каждого фрагмента!
      (if (and (>= (car c) curHeaderRows) 
               (nth 2 c) 
               (/= (nth 2 c) ""))
        (progn
          ;; Смещение: (текущая строка - шапка фрагмента) + накопленное смещение + размер одной мастер-шапки
          (setq newRow (+ (- (car c) curHeaderRows) globalOffset headerRows))
          (setq allCellData (cons (cons newRow (cdr c)) allCellData))
        )
      )
    )
    
    ;; --- Пересчет объединений ---
    (foreach m (nth 4 frag)
      (if (>= (car m) curHeaderRows)
        (progn
          (setq newTop (+ (- (car m) curHeaderRows) globalOffset headerRows))
          (setq newBottom (+ (- (nth 2 m) curHeaderRows) globalOffset headerRows))
          (setq allLocMerge (cons (list newTop (cadr m) newBottom (nth 3 m)) 
                                 allLocMerge))
        )
      )
    )
    
    (setq globalOffset (+ globalOffset dataRows))
    (setq i (1+ i))
  )

  ;; 4. Формирование финальных параметров
  (if masterIdx
    (setq headerData (list headerRows rawCells headerMerge hListHeader globalOffset)
          params (append params (list headerData)))
    (setq params (append params (list nil)))
  )

  ;; Возвращаем результат
  (list params 
        (list (reverse vlaObjects) 
              allHLists 
              (reverse allCellData) 
              (reverse allLocMerge)))
);; 4. CLUSTER DETECTOR: Обработка всего кластера
(defun ClusterDetector ( cluster / tableList resList res masterInfo updatedCluster )
  (setq tableList (cadr cluster)
        resList nil)
        
  ;; 1. Первичный сбор данных
  (foreach frag tableList
    (setq res (SystemDetector frag))
    (if (not res) (setq res (GeometryDetector frag)))
    (if res (setq res (ContentDetector frag res)))
    (setq resList (cons res resList))
  )
  (setq resList (reverse resList))
  
  ;; 2. Идентификация структуры
  (setq masterInfo (IdentifyMasterHeader resList))
  
 
  ;; 3. Трансформация данных
  (setq updatedCluster (TransformCluster cluster resList masterInfo))
  
  ;; Возвращаем модифицированный кластер
  updatedCluster
)



  (princ "\nЗапуск Общего шага 5...")
  (setq updatedClusters nil)

  ;; Итерационный проход по всему списку кластеров проекта
  (foreach cluster clusters
    ;; Запускаем функцию 4 для каждого кластера и собираем результат
    (setq updatedClusters (cons (ClusterDetector cluster) updatedClusters))
  )

  (princ "\nШаг 5 завершен.")
  
  ;; Возвращаем обновленную глобальную структуру (переворачиваем список обратно)
  (reverse updatedClusters)
)

;; ===========================================================================
;; ДОПОЛНИТЕЛЬНАЯ ФУНКЦИЯ: TC:AddPureGap (Универсальная)
;; Назначение: Вычисляет зазор по X (для рядов) или по Y (для колонок)
;; ===========================================================================
(defun TC:AddPureGap (clusters / updatedClusters)
  (setq updatedClusters 
    (mapcar 
      '(lambda (cluster / params tables vlaObjs totalGap count i obj1 obj2 p1 p2 dx dy gap avgGap headerData)
         (setq params  (car cluster)
               tables  (cadr cluster)
               vlaObjs (car tables) ;; Это список (vla1 vla2 ...)
               totalGap 0.0
               count 0)

         ;; 1. Расчет зазоров
         (setq i 0)
         (while (< i (1- (length vlaObjs)))
           (setq obj1 (nth i vlaObjs)
                 obj2 (nth (1+ i) vlaObjs)
                 p1 (vlax-get obj1 'InsertionPoint)
                 p2 (vlax-get obj2 'InsertionPoint)
                 dx (abs (- (car p1) (car p2)))
                 dy (abs (- (cadr p1) (cadr p2))))

           (if (> dx dy)
             (setq gap (- dx (vla-get-Width obj1)))  ;; Горизонтально
             (setq gap (- dy (vla-get-Height obj1))) ;; Вертикально
           )
           (setq totalGap (+ totalGap (max 0.0 gap))
                 count (1+ count)
                 i (1+ i))
         )

         ;; 2. Округление среднего значения
         (setq avgGap (if (> count 0) 
                        (fix (+ (/ totalGap (float count)) 0.5)) 
                        0))

         ;; 3. Внедрение в 8-й параметр (индекс 7)
         ;; Изначально там (headerRows rawCells headerMerge hListHeader globalOffset)
         (setq headerData (nth 7 params))
         (if headerData
           (setq headerData (append headerData (list avgGap))) ;; Добавляем 6-м элементом в список шапки
         )

         ;; Пересборка списка параметров с обновленным headerData
         (setq params (subst-nth 7 headerData params))

         (list params tables)
      )
      clusters
    )
  )
  updatedClusters
)

;; Вспомогательная функция замены для надежности
(defun subst-nth (idx val lst / i)
  (setq i -1)
  (mapcar '(lambda (x) (if (= (setq i (1+ i)) idx) val x)) lst)
);

;; ===========================================================================
;; ФУНКЦИЯ: TC:IdentifyHeaderStyle
;; Назначение: Менеджер определения стиля. Проверяет условия и вызывает детектор.
;; Возвращает: Пока принудительно nil (для пошаговой отладки)
;; ===========================================================================
(defun TC:IdentifyHeaderStyle (space insPoint params / headerData formatID totalRows newVlaTable loc-detect-format get-style-name-by-flag check-table-style-exists INSERTSPEC loc-insert-styletab)

  ;; ====================================================================
  ;; ВНУТРЕННЯЯ ФУНКЦИЯ: loc-detect-format
  ;; Назначение: Основной модуль анализа. Извлекает, чистит и сверяет данные.
  ;; Принимает: Список сырых ячеек (rawCells) и кол-во колонок (cols).
  ;; Возвращает: formatID (1-5) или nil.
  ;; ====================================================================
  (defun loc-detect-format (rawCells cols / currentHeaderText t2 t3 t4 t5 id choice)
    ;; 1. Подготовка эталонов
    (setq t2 '("Поз." "Наименованиеитехническаяхарактеристика" "Тип,марка,обозначениедокумента,опросноголиста" 
               "Кодоборудования,изделия,материала" "Поставщик" "Ед.измерения" "Кол." "Массаед.,кг" "Примечание" "Дополнительныехарактеристики")
          t3 '("Поз." "Обозначение" "Наименование" "Кол." "Массаед.,кг" "Примечание")
          t4 '("№п/п" "Наименованиеработ" "Ед.изм." "Количество" "Формуларасчетаобъемовработирасходаматериалов" 
               "Ссылканачертежи,спецификациивпроектнойдокументации" "Примечание")
          ;; ТИП 5 (5 колонок - Ведомость работ усеченная)
          t5 '("№п/п" "Наименованиеработ" "Ед.изм." "Количество" "Примечание")
    )

    ;; 2. Сбор и очистка текста первой строки (row 0)
    (setq currentHeaderText '())
    (foreach cell rawCells
      (if (= (car cell) 0) 
        (setq currentHeaderText (cons (CleanerTextAllFormat (nth 2 cell)) currentHeaderText))
      )
    )
    (setq currentHeaderText (reverse currentHeaderText))

    ;; 3. Определение ID через сравнение
    (setq id 
      (cond
        ;; ТИП 1 и 2 (СО)
        ((and (>= cols 9) (ListComparator (if (= cols 9) (reverse (cdr (reverse t2))) t2) currentHeaderText))
         (if (= cols 9) 1 2))
        ;; ТИП 3 (Спецификация 6 кол)
        ((and (= cols 6) (ListComparator t3 currentHeaderText)) 3)
        ;; ТИП 4 (Ведомость работ полная 7 кол)
        ((and (= cols 7) (ListComparator t4 currentHeaderText)) 4)
        ;; ТИП 5 (Ведомость работ усеченная 5 кол)
        ((and (= cols 5) (ListComparator t5 currentHeaderText)) 5)
        (t nil)
      )
    )

    ;; 4. Уточнение для СО (Логика расширения Типа 1 до Типа 2)
    (if (= id 1)
      (progn
        (initget "Оставить Расширить")
        (setq choice (getkword "\nОбнаружена СО (9 кол.). Оставить или Расширить до 10 кол.? [Оставить/Расширить] <Оставить>: "))
        (if (equal choice "Расширить") (setq id 2))
      )
    )
    id ;; Возвращаем найденный ID
  )
;;; ==========================================================================
;;; Функция: Получает имя стиля таблицы по номеру флага
;;; ==========================================================================
(defun get-style-name-by-flag (flag)
  (cond
    ((= flag 1) "Общая спецификация")
    ((= flag 2) "Общая спецификация Расшир")
    ((= flag 3) "Листовая спецификация")
    ((= flag 4) "Ведомость объемов работ форма 4")
    ((= flag 5) "Ведомость объемов работ 21.111-84")
    (t nil)
  )
)

;;; ==========================================================================
;;; Функция: Проверка существования стиля таблицы
;;; ==========================================================================
(defun check-table-style-exists (flag / sName table-dict)
  (setq sName (get-style-name-by-flag flag))
  (if (and sName
           (setq table-dict (dictsearch (namedobjdict) "ACAD_TABLESTYLE"))
           (dictsearch (cdr (assoc -1 table-dict)) sName))
    T
    nil
  )
)

;;; ==========================================================================
;;; Функция INSERTSPEC
;;; Возвращает: T при успешной загрузке, nil при ошибке. Работает без лишнего вывода.
;;; ==========================================================================
(defun INSERTSPEC (flag / bName fName fPath pt acadDoc targetBlocks dbxDoc sourceBlock objArray space blkRef result styleBefore styleAfter)

  (setq result nil)
  
  ;; 1. Определяем имя блока и файла
  (cond
    ((= flag 1)
     (setq bName "спецификация_общая_табл(взорвать)"
           fName "спецификация общая.dwg"))
    ((= flag 2)
     (setq bName "Раширенная общая спецификация"
           fName "спецификация общая.dwg"))
    ((= flag 3)
     (setq bName "Листовая спец_таблица_блок(взорвать)"
           fName "спецификация листовая.dwg"))
    ((or (= flag 4) (= flag 5))
     (setq bName "Ведомость объемов работ"
           fName "Ведомость объемов работ.dwg"))
  )
  
  (if (and bName fName)
    (if (not dirpol)
      (princ "\nОшибка: Глобальная переменная 'dirpol' не задана!")
      (progn
        ;; Проверка ДО (тихая)
        (setq styleBefore (check-table-style-exists flag))

        (setq fPath (strcat dirpol "\\ToolPalette\\Palettes\\" fName)
              pt '(0.0 0.0 0.0)
              acadDoc (vla-get-ActiveDocument (vlax-get-acad-object))
              targetBlocks (vla-get-Blocks acadDoc))
        
        ;; 2. Копируем блок через ObjectDBX
        (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-Item (list targetBlocks bName)))
          (progn
            (setq dbxDoc (vlax-create-object (strcat "ObjectDBX.AxDbDocument." (substr (getvar "ACADVER") 1 2))))
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-Open (list dbxDoc fPath))))
              (progn
                (setq sourceBlock (vl-catch-all-apply 'vla-Item (list (vla-get-Blocks dbxDoc) bName)))
                (if (not (vl-catch-all-error-p sourceBlock))
                  (progn
                    (setq objArray (vlax-make-safearray vlax-vbObject '(0 . 0)))
                    (vlax-safearray-put-element objArray 0 sourceBlock)
                    (vla-CopyObjects dbxDoc objArray targetBlocks)
                  )
                  (princ (strcat "\nОшибка: Блок '" bName "' не найден в файле."))
                )
              )
              (princ (strcat "\nОшибка: Не удалось открыть файл: " fPath))
            )
            (if dbxDoc (vlax-release-object dbxDoc))
          )
        )
        
        ;; 3. Вставка блока и его удаление
        (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-Item (list targetBlocks bName))))
          (progn
            (setq space (vla-get-Block (vla-get-ActiveLayout acadDoc)))
            (setq blkRef (vl-catch-all-apply 'vla-InsertBlock (list space (vlax-3d-point pt) bName 1.0 1.0 1.0 0.0)))
            
            (if (not (vl-catch-all-error-p blkRef))
              (progn
                (vla-Delete blkRef)
                
                ;; Проверка ПОСЛЕ (тихая)
                (setq styleAfter (check-table-style-exists flag))
                
                (setq result T) ;; Успех
              )
              (princ "\nОшибка при вставке блока.")
            )
          )
        )
      )
    )
    (princ "\nОшибка: Неверный флаг.")
  )
  
  result
)

  (defun loc-insert-styletab (flag rowCount pt / sName cmdRows e1 e2)
  ;; 1. Маппинг имен стилей
  (setq sName (get-style-name-by-flag flag))

  (if (and sName pt)
    (progn
      ;; ВЫЧИСЛЕНИЕ СТРОК: N - 2
      (setq cmdRows (- rowCount 2))
      (if (< cmdRows 1) (setq cmdRows 1)) 

      (setq e1 (entlast))
      
      (princ (strcat "\n--- Тест стиля " (itoa flag) ": [" sName "] ---"))

      ;; ПРАВИЛЬНЫЙ ВЫЗОВ КОМАНДЫ:
      ;; _-TABLE -> _S (Стиль) -> ИмяСтиля -> _R (Строки) -> Кол-воСтрок -> ТочкаВставки
      ;; Мы НЕ вызываем опцию _C (Columns), поэтому она берется из стиля автоматически.
      (vl-cmdf "_-TABLE" 
               "_S" sName 
               "_R" (itoa cmdRows) 
               pt)
      
      (setq e2 (entlast))
      
      (if (and e2 (not (equal e1 e2)))
        (progn
          (princ (strcat "\n[OK] Таблица '" sName "' создана успешно."))
          (vlax-ename->vla-object e2)
        )
        (progn
          (princ (strcat "\n[ERROR] Не удалось создать таблицу '" sName "'. Проверьте консоль."))
          nil
        )
      )
    )
    nil
  )
)

;; --- ЛОГИКА МЕНЕДЖЕРА (IdentifyHeaderStyle) ---

  (setq headerData (nth 7 params))
  (setq newVlaTable nil) ;; Инициализируем переменную возврата

  ;; 1. Проверка условий: шапка должна существовать и состоять строго из ОДНОЙ строки
  (if (and headerData (= (car headerData) 1))
    (progn
      ;; 2. Запуск детектора формата (возвращает 1-5 или nil)
      (setq formatID (loc-detect-format (cadr headerData) (nth 0 params)))

      ;; 3. Если стиль опознан, пытаемся подготовить чертеж и вставить таблицу
      (if formatID 
        (progn
          (princ (strcat "\nМенеджер: Определен стиль Тип " (itoa formatID)))
          
          ;; Загружаем определения блоков/стилей из внешнего файла
          (if (INSERTSPEC formatID)
            (progn
              ;; Рассчитываем общее количество строк для вставки:
              ;; 1 (сама шапка) + globalOffset (количество строк данных из исходных таблиц)
              (setq totalRows (+ 1 (nth 4 headerData)))
              
              ;; Вызываем вставку по стилю (теперь возвращает VLA-объект или nil)
              (setq newVlaTable (loc-insert-styletab formatID totalRows insPoint))
              
              (if newVlaTable
                (princ "\nМенеджер: Таблица по стилю успешно создана.")
                (princ "\nМенеджер: Ошибка при выполнении команды вставки стиля.")
              )
            )
            (princ "\nМенеджер: Ошибка загрузки ресурсов (INSERTSPEC вернул nil).")
          )
        )
        (princ "\nМенеджер: Состав ячеек не совпадает со стандартами.")
      )
    )
    (princ (strcat "\nМенеджер: Анализ стиля пропущен: " 
                   (if (not headerData) "шапка отсутствует" "шапка многострочная")))
  )
  
  ;; ФИНАЛЬНЫЙ ВОЗВРАТ: VLA-объект (если создан) или nil (если идем по стандартному пути)
  newVlaTable 
)

;; ===========================================================================
;; ФУНКЦИЯ ШАГА 6: TC:CreateTables
;; Назначение: Создание итоговых таблиц на основе собранных данных
;; ===========================================================================
(defun TC:CreateTables (data uiFlags / 
                        space stripColor params hParams cols colWidths headerData 
                        headerRows rawCells headerMerge hListHeader globalOffset 
                        avgGap vlaObjects allHLists allCellData allLocMerge 
                        totalRows insPoint newTable isStyled curRows c r txt 
                        align cleanTxt oldTbl m cell idx bh cellsToFill 
                        scaleFac baseTxtH calcTxtH CleanText SetTableLineweights 
                        GetFirstTextStyle targetTextStyle lentab lentabcount)
  
  ;;; ========================================================================
  ;;; задавание жирности (с добавленным счетчиком)
  ;;; ========================================================================
  (defun SetTableLineweights (vla-table num-rows / total-cols total-rows r c)
    (setq total-cols (vla-get-Columns vla-table))
    (setq total-rows (vla-get-Rows vla-table))
    (setq r 0)
    (while (< r total-rows)
      ;; ПРОГРЕСС-БАР для 5 шага: Затираем строку (\r)
      (princ (strcat "\rНастройка линий: строка " (itoa (1+ r)) " из " (itoa total-rows) "   "))
      
      (setq c 0)
      (while (< c total-cols)
        (vl-catch-all-apply 'vla-SetCellGridLineWeight (list vla-table r c 8 50))
        (vl-catch-all-apply 'vla-SetCellGridLineWeight (list vla-table r c 2 50))
        (if (< r num-rows)
          (progn
            (vl-catch-all-apply 'vla-SetCellGridLineWeight (list vla-table r c 1 50))
            (vl-catch-all-apply 'vla-SetCellGridLineWeight (list vla-table r c 4 50))
          )
        )
        (setq c (1+ c))
      )
      (setq r (1+ r))
    )
    (princ "\n") ;; Перенос строки после завершения
  )

  ;; --- Вложенная функция очистки текста (ОРИГИНАЛЬНАЯ) ---
  (defun CleanText (str / regex resStr hasOther)
    (if (and (= (type str) 'STR) (vl-string-search "\\" str))
      (progn
        (setq regex (vlax-create-object "VBScript.RegExp"))
        (vlax-put-property regex 'Global :vlax-true)
        (vlax-put-property regex 'IgnoreCase :vlax-true)
        (vlax-put-property regex 'Pattern "\\\\[Cc][0-9]+;")
        (setq resStr (vlax-invoke regex 'Replace str ""))
        (vlax-release-object regex)
        (setq hasOther (vl-string-search "\\" resStr))
        (if (not hasOther)
          (progn
            (while (vl-string-search "{" resStr) (setq resStr (vl-string-subst "" "{" resStr)))
            (while (vl-string-search "}" resStr) (setq resStr (vl-string-subst "" "}" resStr)))
          )
        )
        resStr
      )
      str
    )
  )

  ;; --- Вложенная функция получения стиля текста из первой непустой ячейки ---
  (defun GetFirstTextStyle (tbl / r c txt style rows cols done)
    (setq rows (vla-get-Rows tbl)
          cols (vla-get-Columns tbl)
          r 0 done nil style nil)
    (while (and (< r rows) (not done))
      (setq c 0)
      (while (and (< c cols) (not done))
        (setq txt (vl-catch-all-apply 'vla-GetText (list tbl r c)))
        (if (and (not (vl-catch-all-error-p txt)) (/= (vl-string-trim " \t\n" txt) ""))
          (progn
            (setq style (vl-catch-all-apply 'vla-GetCellTextStyle (list tbl r c)))
            (if (or (vl-catch-all-error-p style) (= style ""))
              (setq style nil)
              (setq done T)
            )
          )
        )
        (setq c (1+ c))
      )
      (setq r (1+ r))
    )
    (if style style (getvar "TEXTSTYLE"))
  )

  (princ "\nЗапуск создания таблиц...")
  (setq space (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq stripColor (not (car uiFlags)))

  (foreach cluster data
    (setq params (car cluster) hParams (cadr cluster))
    
    (setq cols (nth 0 params)
          scaleFac (nth 1 params)
          baseTxtH (nth 2 params)
          colWidths (nth 6 params)
          headerData (nth 7 params))
          
    (setq calcTxtH (* scaleFac baseTxtH))
          
    (setq headerRows (if headerData (nth 0 headerData) 0)
          rawCells (if headerData (nth 1 headerData) nil)
          headerMerge (if headerData (nth 2 headerData) nil)
          hListHeader (if headerData (nth 3 headerData) nil)
          globalOffset (if headerData (nth 4 headerData) 0)
          avgGap (if headerData (nth 5 headerData) 0.0))
          
    (setq vlaObjects (nth 0 hParams)
          allHLists (nth 1 hParams)
          allCellData (nth 2 hParams)
          allLocMerge (nth 3 hParams))
          
    (setq totalRows (+ headerRows globalOffset))
    
    (if (> totalRows 0)
      (progn
        (setq insPoint (vlax-get (car vlaObjects) 'InsertionPoint))
        (setq targetTextStyle (GetFirstTextStyle (car vlaObjects)))
        
        ;; ОПТИМИЗАЦИЯ 1: Удаляем исходные таблицы СРАЗУ, освобождая память
        (foreach oldTbl vlaObjects 
          (if (not (vlax-erased-p oldTbl)) (vl-catch-all-apply 'vla-Delete (list oldTbl)))
        )

        (setq newTable (TC:IdentifyHeaderStyle space insPoint params))
        (setq isStyled (if newTable T nil))

        (if (not isStyled)
          (progn
            (setq newTable (vla-AddTable space (vlax-3d-point insPoint) 3 cols (* scaleFac 8.0) (car colWidths)))
            (vla-DeleteRows newTable 0 1)

            (vl-catch-all-apply 'vla-SetTextStyle (list newTable 1 targetTextStyle)) 
            (vl-catch-all-apply 'vla-SetTextStyle (list newTable 2 targetTextStyle)) 
            (vl-catch-all-apply 'vla-SetTextStyle (list newTable 4 targetTextStyle)) 

            (if (> headerRows 1)
              (vla-InsertRowsAndInherit newTable 1 0 (1- headerRows))
            )
            
            (cond
              ((> globalOffset 1)
               (vla-InsertRowsAndInherit newTable (vla-get-Rows newTable) (1- (vla-get-Rows newTable)) (1- globalOffset))
              )
              ((= globalOffset 0)
               (vla-DeleteRows newTable headerRows 1)
              )
            )
          )
          (progn
            (setq curRows (vla-get-Rows newTable))
            (if (< curRows totalRows)
              (vla-InsertRowsAndInherit newTable curRows (1- curRows) (- totalRows curRows))
            )
          )
        )

        ;; === ОПТИМИЗАЦИЯ 2: Отключаем пересчет геометрии для ВСЕХ последующих операций ===
        (vl-catch-all-apply 'vlax-put-property (list newTable 'RegenerateTableSuppressed :vlax-true))

        ;; 2. УСТАНОВКА ШИРИНЫ СТОЛБЦОВ
        (setq c 0)
        (foreach cw colWidths
          (vla-SetColumnWidth newTable c cw)
          (setq c (1+ c))
        )

        ;; 3. ГЕОМЕТРИЯ И ОБЪЕДИНЕНИЯ
        (if (not isStyled)
          (progn
            (vla-put-HorzCellMargin newTable (* scaleFac 1.5))
            (vla-put-VertCellMargin newTable (* scaleFac 0.5))
            
            (foreach m headerMerge
              (vla-MergeCells newTable (nth 0 m) (nth 2 m) (nth 1 m) (nth 3 m))
            )

            ;; ПРАВКА 1: Отступы 0.5 только для ячеек шапки (Используем SetMargin)
            (setq r 0)
            (while (< r headerRows)
              (setq c 0)
              (while (< c cols)
                (vl-catch-all-apply 'vla-SetMargin (list newTable r c 2 (* scaleFac 0.5)))
                (vl-catch-all-apply 'vla-SetMargin (list newTable r c 8 (* scaleFac 0.5)))
                (setq c (1+ c))
              )
              (setq r (1+ r))
            )
          )
        )
        
        (foreach m allLocMerge
          (vla-MergeCells newTable (nth 0 m) (nth 2 m) (nth 1 m) (nth 3 m))
        )

        ;; 4. ЗАПОЛНЕНИЕ ТЕКСТОМ
        (if (not isStyled) (vla-SetTextHeight newTable 7 calcTxtH))

        (setq cellsToFill (if isStyled allCellData (append rawCells allCellData)))
        
        ;; ОПТИМИЗАЦИЯ 3: while с удалением обработанных элементов
        (setq lentab (length cellsToFill) lentabcount 0)
        (while cellsToFill
          (setq cell (car cellsToFill))
          (setq cellsToFill (cdr cellsToFill))
          
          (setq lentabcount (1+ lentabcount))
          (princ (strcat "\rЗаполнение ячеек: " (itoa lentabcount) " из " (itoa lentab) "   "))
          
          (setq r (nth 0 cell) c (nth 1 cell) txt (nth 2 cell) align (nth 3 cell))
          (if (and txt (/= txt ""))
            (progn
              (setq cleanTxt (if stripColor (CleanText txt) txt))
                
              (if (not isStyled)
                (progn
                  (vla-SetCellTextHeight newTable r c calcTxtH)
                  (vl-catch-all-apply 'vla-SetCellTextStyle (list newTable r c targetTextStyle))
                  
                  ;; ПРАВКА 2: Если строка относится к шапке, центрируем (5). Иначе берем оригинал.
                  (if (< r headerRows)
                    (vla-SetCellAlignment newTable r c 5) 
                    (if align (vla-SetCellAlignment newTable r c align))
                  )
                )
              )
              
              (vla-SetText newTable r c cleanTxt)
            )
          )
        )
        (princ "\n")

        ;; 5. УСТАНОВКА ВЫСОТ СТРОК И ЛИНИЙ (Только если нет стиля)
        (if (not isStyled)
          (progn
            (vla-SetTextHeight newTable 7 calcTxtH)
            (setq r 0)
            (if hListHeader
              (foreach rh hListHeader
                (if (< r (vla-get-Rows newTable)) (vla-SetRowHeight newTable r rh))
                (setq r (1+ r))
              )
            )
            (while (< r totalRows)
              (vla-SetRowHeight newTable r (* scaleFac 8.0))
              (setq r (1+ r))
            )
            (vla-put-Lineweight newTable 25) 
            (SetTableLineweights newTable headerRows) ;; Внутри теперь тоже есть счетчик
          )
        )

        ;; 6. УСТАНОВКА ПОВТОРОВ ШАПКИ, РАЗРЫВОВ
        (vla-put-BreaksEnabled newTable :vlax-true)
        (if (> headerRows 0)
          (progn
            (vla-put-RepeatTopLabels newTable :vlax-true)
            (vl-catch-all-apply 'vla-put-AllowManualPositions (list newTable :vlax-true))
            (vl-catch-all-apply 'vla-put-AllowManualHeights (list newTable :vlax-true))
          )
        )
        (if (and avgGap (> avgGap 0)) (vla-put-BreakSpacing newTable avgGap))

        ;; 7. УСТАНОВКА ВЫСОТ РАЗРЫВОВ (ФРАГМЕНТОВ)
        (if (and allHLists (listp allHLists))
          (progn
            (setq idx 0)
            (foreach bh allHLists
              (vl-catch-all-apply 'vla-SetBreakHeight (list newTable idx (float bh)))
              (setq idx (1+ idx))
            )
          )
        )
        
        ;; === ОПТИМИЗАЦИЯ 4: Включаем пересчет геометрии в САМОМ КОНЦЕ ===
        (vl-catch-all-apply 'vlax-put-property (list newTable 'RegenerateTableSuppressed :vlax-false))
        (vla-Update newTable)

      )
    )
  )
  (princ "\nТаблицы успешно созданы.")
  (princ)
)
; ====================================================
(defun PrintStep5Cluster ( cluster / params hParams tables i frag marker data )
      (setq cluster (car cluster)
      		params  (nth 0 cluster)
            hParams (nth 1 cluster)
            )

  (foreach item (nth 7 params)
(print item) (princ "\n") (princ )
)
(foreach item hParams
(print item) (princ "\n") (princ )
)
)




