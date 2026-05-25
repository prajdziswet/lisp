;; =================================================================
;; 00 блок міні функціі CreateTableFromLines
;; =================================================================
;функція аднаулення налад
(defun CT:RestoreSettings ()
  ;; Восстанавливаем настройки
            (setvar "OSMODE" old_osmode)
            (setvar "CMDECHO" old_cmdecho)
            (setvar "CMDDIA" old_cmddia)
            (setvar "FILLETRAD" old_filletrad)
            
            ;; Ждем завершения команды
            (while (= (logand (getvar "CMDACTIVE") 4) 4)
              (command)
            )
    )
(defun CT:SaveSettings ()
;; Сохраняем настройки
            (setq old_osmode (getvar "OSMODE")
                  old_cmdecho (getvar "CMDECHO")
                  old_cmddia (getvar "CMDDIA")
                  old_filletrad (getvar "FILLETRAD"))
            
            (setvar "OSMODE" 0)
            (setvar "CMDECHO" 0)
            (setvar "CMDDIA" 0)
            (setvar "FILLETRAD" 0.0)
  
)
;; =================================================================
;; Дапаможная функцыя для апрацоўкі тэкставага аб'екту
;; =================================================================
;; ________________________________
;; Вспомогательная функция для обработки текстового объекта (TEXT или MTEXT)
;; Принимает имя объекта (ent_name), DXF-данные (ent_data), тип объекта (ent_type)
;; Возвращает список (pt_min pt_max pt_center text_height) или nil в случае ошибки
(defun PO:ProcessTextEntity (ent_name ent_data ent_type / obj_vla bounding_box pt_min pt_max pt_center text_height temp)
  ;; Атрыманне вышыні тэксту з DXF кода 40
  (setq text_height (cdr (assoc 40 ent_data)))
  ;; Праверка, ці вышыня атрымана і станоўчая
  (if (and text_height (> text_height 0))
    (progn
      (setq obj_vla (vlax-ename->vla-object ent_name))
      ;; Праверка, ці VLA-аб'ект атрыманы паспяхова
      (if obj_vla
        (progn
          (vla-getboundingbox obj_vla 'pt_min 'pt_max)
          (setq pt_min (vlax-safearray->list pt_min))
          (setq pt_max (vlax-safearray->list pt_max))
          ;; Упорядочивание bbox: pt_min - с меньшими координатами (X, затем Y)
          ;; Проверяем, если pt_min.X > pt_max.X или (pt_min.X == pt_max.X и pt_min.Y > pt_max.Y),
          ;; то меняем местами.
          (if (or (> (car pt_min) (car pt_max))
                  (and (= (car pt_min) (car pt_max)) (> (cadr pt_min) (cadr pt_max))))
            (progn
              (setq temp pt_min)
              (setq pt_min pt_max)
              (setq pt_max temp)
            )
          )
          ;; Цэнтр вылічваецца заўсёды аднолькава пасля ўпарадкавання
          (setq pt_center (list (/ (+ (car pt_min) (car pt_max)) 2.0)
                          (/ (+ (cadr pt_min) (cadr pt_max)) 2.0)
                          (/ (+ (caddr pt_min) (caddr pt_max)) 2.0)))
          ;; Вяртанне выніку
          (list pt_min pt_max pt_center text_height)
        )
        ;; Калі VLA-аб'ект не атрыманы
        (progn
          (princ (strcat "\nНе атрымалася атрымаць VLA-аб'ект для " ent_type ": " (itoa (handent ent_name))))
          nil ; Вяртаем nil у выпадку памылкі
        )
      )
    )
    ;; Калі вышыня не знойдзена альбо <= 0
    (progn
      (princ (strcat "\nПапярэджанне: Не знойдзена альбо некарэктная вышыня " ent_type " для аб'екту: " (itoa (handent ent_name))))
      nil ; Вяртаем nil у выпадку памылкі
    )
  )
)
;; ________________________________
;; Конец функции ProcessTextEntity
;; ________________________________

;; =================================================================
;; ФУНКЦЫІ ДЛЯ ЗМАНЕННЯ ЧАСУ ВЫКАНАННЯ
;; =================================================================

;; Запусціць таймер з імем
(defun start-timer (name)
  (setq *timers* (cons (list name (getvar "millisecs") nil) *timers*))
  name
)

;; Спыніць таймер з імем
(defun end-timer (name)
  (setq *timers* 
    (mapcar 
      (function
        (lambda (timer)
          (if (eq (car timer) name)
            (list name (cadr timer) (getvar "millisecs"))
            timer
          )
        )
      )
      *timers*
    )
  )
  name
)

;; Вывесці ўсе таймеры
;; Вывесці ўсе таймеры ў зваротным парадку
(defun print-timers (/ total-time)
(princ "\n================================")
(princ "\n==== КАШТАРЫС ЧАСУ ВЫКАНАННЯ ===")
;; Выводим таймеры в обратном порядке (последний запущенный — первый в списке)
(foreach timer (reverse *timers*)
(if (caddr timer)
(progn
(setq duration (- (caddr timer) (cadr timer)))
(princ (strcat "\n" (car timer) ": " (rtos (/ duration 1000.0) 2 2) " с"))
)))
(princ "\n================================")
(setq *timers* nil)
)

;; Ачысціць усе таймеры
(defun reset-timers ()
  (setq *timers* nil)
)
;; =================================================================
;; 00 блок канец міні функціі CreateTableFromLines
;; =================================================================

;;; =================================================================
;;; Блок 01 PripareObj
;;; Уваходныя параметры:
;;;   sel_set    - Selection Set (набор) аб'ектаў, атрыманых ад карыстальніка.
;;;
;;; Выхадныя даныя (Глабальныя спісы і зменныя):
;;;   line_list  - Спіс ліній. Кожны элемент: 
;;;                (ename start_pt end_pt mid_pt color dxf_data) [cite: 7, 21]
;;;   text_list  - Спіс TEXT. Кожны элемент:
;;;                (ename min_pt max_pt center_pt height color dxf_data) [cite: 10, 11]
;;;   mtext_list - Спіс MTEXT. Структура аналагічная text_list. [cite: 12]
;;;   final_trash - Selection Set з аб'ектамі, якія не з'яўляюцца 
;;;                 лініямі або тэкстам. [cite: 22, 23, 27]
;;;   first_text_height_found - Вышыня першага знойдзенага тэкставага 
;;;                             аб'екта для вылічэння параметраў. [cite: 11]
;;; =================================================================
(defun CT:PripareObj (sel_set / i ent_data ent_type ent_name pt_start pt_end 
                                ordered_pt_start ordered_pt_end temp_ss 
                                temp_ent_data temp_ent_type text_result
                                pt_center current_height pt_min pt_max 
                                temp_ent j ent_color bb dxf-TEXT pt_mid temp_color)
  (setq i 0)
  (repeat (sslength sel_set)
    (setq ent_name (ssname sel_set i))
    (if ent_name
      (progn
        (setq ent_data (entget ent_name))
        (if ent_data
          (progn
            (setq ent_type (cdr (assoc 0 ent_data)))
            (setq ent_color (if (assoc 62 ent_data) (cdr (assoc 62 ent_data)) 256))
            
            (cond
              ;; --- LINE ---
              ((= ent_type "LINE")
               (setq pt_start (cdr (assoc 10 ent_data))
                     pt_end (cdr (assoc 11 ent_data)))
               (if (or (< (car pt_start) (car pt_end))
                       (and (= (car pt_start) (car pt_end)) (< (cadr pt_start) (cadr pt_end))))
                 (setq ordered_pt_start pt_start ordered_pt_end pt_end)
                 (setq ordered_pt_start pt_end ordered_pt_end pt_start))
               
               (setq pt_mid (list (/ (+ (car ordered_pt_start) (car ordered_pt_end)) 2.0)
                                  (/ (+ (cadr ordered_pt_start) (cadr ordered_pt_end)) 2.0)
                                  0.0))
               ;; СТРУКТУРА: 0:name 1:start 2:end 3:mid 4:color 5:dxf
               (setq line_list (cons (list ent_name ordered_pt_start ordered_pt_end pt_mid ent_color ent_data) line_list))
              )

              ;; --- TEXT & MTEXT ---
              ((or (= ent_type "TEXT") (= ent_type "MTEXT"))
               (setq text_result (PO:ProcessTextEntity ent_name ent_data ent_type))
               (if (and text_result (nth 0 text_result) (nth 1 text_result) (nth 2 text_result))
                 (progn
                   (setq pt_min (nth 0 text_result)
                         pt_max (nth 1 text_result)
                         pt_center (nth 2 text_result)
                         current_height (nth 3 text_result))
                   
                   ;; СТРУКТУРА: 0:name 1:min 2:max 3:center 4:height 5:color 6:dxf
                   (setq dxf-TEXT (list ent_name pt_min pt_max pt_center current_height ent_color ent_data))
                   
                   (if (and (not first_text_height_found) current_height (> current_height 0))
                     (progn 
					 (setq first_text_height_found current_height)
					 (setq current_style (cdr (assoc 7 ent_data))) ;; Извлекаем стиль напрямую из данных
					 ))

                   (if (= ent_type "TEXT")
                     (setq text_list (cons dxf-TEXT text_list))
                     (setq mtext_list (cons dxf-TEXT mtext_list)))
                 )
               )
              )

              ;; --- EXPLODE POLYLINE ---
              ((or (= ent_type "LWPOLYLINE") (= ent_type "POLYLINE"))
               (command "_explode" ent_name)
               (setq temp_ss (ssget "_P"))
               (if temp_ss
                 (progn
                   (setq j 0)
                   (repeat (sslength temp_ss)
                     (setq temp_ent (ssname temp_ss j))
                     (if (and temp_ent (setq temp_ent_data (entget temp_ent)))
                       (progn
                         (setq temp_ent_type (cdr (assoc 0 temp_ent_data)))
                         (if (= temp_ent_type "LINE")
                           (progn
                             (setq pt_start (cdr (assoc 10 temp_ent_data))
                                   pt_end (cdr (assoc 11 temp_ent_data))
                                   temp_color (if (assoc 62 temp_ent_data) (cdr (assoc 62 temp_ent_data)) 256))
                             
                             (if (or (< (car pt_start) (car pt_end))
                                     (and (= (car pt_start) (car pt_end)) (< (cadr pt_start) (cadr pt_end))))
                               (setq ordered_pt_start pt_start ordered_pt_end pt_end)
                               (setq ordered_pt_start pt_end ordered_pt_end pt_start))
                             
                             (setq pt_mid (list (/ (+ (car ordered_pt_start) (car ordered_pt_end)) 2.0)
                                                (/ (+ (cadr ordered_pt_start) (cadr ordered_pt_end)) 2.0)
                                                0.0))
                             ;; Добавляем НОВЫЙ элемент temp_ent и его данные
                             (setq line_list (cons (list temp_ent ordered_pt_start ordered_pt_end pt_mid temp_color temp_ent_data) line_list))
                           )
                           (ssadd temp_ent final_trash) ; Если не линия после взрыва
                         )
                       )
                     )
                     (setq j (1+ j))
                   )
                 )
               )
              )

              (T (ssadd ent_name final_trash))
            ) ; cond
          )
        )
      )
    )
    (setq i (1+ i))
  )
  ;; Дадайце гэта ў канец CT:PripareObj
(setq text_list (vl-sort text_list '(lambda (a b) (> (cadr (nth 3 a)) (cadr (nth 3 b))))))
(setq line_list (vl-sort line_list '(lambda (a b) (> (cadr (nth 3 a)) (cadr (nth 3 b))))))

(CT:DefinitionParameters first_text_height_found)
        ;; Вывад інфармацыі пра маштаб і допуск
	  (princ "\n------------------------")
	  (princ "\n01 Зыходныя даныя:")		  
      (princ (strcat "\nВышыня тэксту (вызначана): " (rtos calculated_height 2 4)))
      (princ (strcat "\nМаштаб: " (itoa scale)))
	  (CT:Debug 1 "message" (strcat "Допуск: " (rtos tolerance 2 4)))
	  (CT:Debug 1 "message" (strcat "Павышаная вышыня шрыфту: " (rtos elevated_scale 2 4)))
        
  (princ (strcat "\nАгульная колькасць аб'ектаў:" (itoa lengthobj)))
  (princ (strcat "\nКолькасць рысаў: " (itoa (length line_list))))
  (CT:Debug 1 "list" line_list)
  (princ (strcat "\nКолькасць TEXT: " (itoa (length text_list))))
  (CT:Debug 1 "list" text_list)
  (princ (strcat "\nКолькасць MTEXT: " (itoa (length mtext_list))))
  (CT:Debug 1 "list" mtext_list)
  (princ (strcat "\nКолькасць смецця: " (itoa (sslength final_trash))))
  (CT:Debug 1 "ss" final_trash)
);; 01 Канец падрыхтоукі аб'ектау PripareObj
;; =================================================================

;; =================================================================
;; 01.1 блок Дапаможная функцыя для вылічэння маштабу і допуску па вышыні тэксту
;; =================================================================
(defun DF:CalculateScaleAndTolerance (height / scale tolerance calculated_height elevated_scale)
  (setq scale (fix (+ 0.5 (/ height 2.5)))) ; Дзялім на 2.5 і акругляем
  (if (= scale 0) (setq scale 1)) ; Мінімальны маштаб 1
  (setq tolerance (* scale 0.5)) ; Допуск = маштаб * 0.5
  (setq calculated_height (* scale 2.5)) ; вышыня шрыфту = маштаб * 2.5
  (setq elevated_scale (* scale 3.5)) ; каэфіцыент павелічэння вышыні тэксту
  (list scale tolerance calculated_height elevated_scale)
)

(defun CT:DefinitionParameters (first_text_height_found / scale_tolerance_list)
  ;; Вылічэнне глабальнага маштабу і допуску
  (if first_text_height_found
    (progn
      (setq scale_tolerance_list (DF:CalculateScaleAndTolerance first_text_height_found))
      (setq scale (nth 0 scale_tolerance_list))
      (setq tolerance (nth 1 scale_tolerance_list))
      (setq calculated_height (nth 2 scale_tolerance_list))
      (setq elevated_scale (nth 3 scale_tolerance_list))
    )
    ;; Калі тэксту няма
    (progn
      (princ "\nСярод вылучаных аб'ектаў не знойдзена тэксту. Выкарыстоўваем значэнні па змаўчанні.")
      (setq scale 1)
      (setq tolerance 0.5)
      (setq calculated_height 2.5)
      (setq elevated_scale 3.5)
    )
  )
)
;; =================================================================
;; 01.1  Дапаможная функцыя для вылічэння маштабу і допуску па вышыні тэксту
;; =================================================================

;; ________________________________
;; 02 Функция определения дроби, подчеркивания и зачеркивания defineTextAndLine
;; ________________________________
(defun CT:defineTextAndLine (lines_list text_list mtext_list tolerance print_defun / 
                           new_text_list new_mtext_list
                           temp_text_list current_text pair_text found_pair
                           current_frac_lines current_strike_lines current_under_lines
                           remaining_lines line_obj pt_center1 text1_height 
                           results lines_count text_count mtext_count i txt_y search_range ln_y)

(if print_defun
(progn
  (princ "\n-------------------------")
  (princ "\n02 АПРАЦОЎКА РЫС І ТЭКСТУ")
  ))
  (setq lines_count (length lines_list) text_count (length text_list) mtext_count (length mtext_list))
  
 ;; Физическое удаление объектов из списка с проверкой
(defun delete-objects (obj_list)
  (if obj_list
    (foreach obj obj_list
      (if (and obj (car obj)) (entdel (car obj)))))
) ;; end of CT:delete-objects

;; Проверка: является ли линия нижним подчеркиванием (с расширенным допуском по Y вниз)
;; Проверка: является ли линия разделителем дроби (Сценарий А)
;; t1, t2 - списки (ename p_min p_max p_cen ...)
(defun is-fraction-line (t1 t2 line_obj tol / p1 p2 mid_l top_bot bot_top)
  ;; Сортируем тексты по вертикали, чтобы знать кто сверху, а кто снизу
  (if (> (cadr (nth 3 t1)) (cadr (nth 3 t2)))
      (setq top_txt t1 bot_txt t2)
      (setq top_txt t2 bot_txt t1))

  (setq p1 (nth 3 top_txt)    ;; Центр верхнего
        p2 (nth 3 bot_txt)    ;; Центр нижнего
        mid_l (nth 3 line_obj) ;; Центр линии
        y_top_bot (cadr (nth 1 top_txt)) ;; Y-min верхнего текста (его низ)
        y_bot_top (cadr (nth 2 bot_txt))) ;; Y-max нижнего текста (его верх)
  
  (and 
    ;; 1. Горизонтальность линии
    (<= (abs (- (cadr (nth 1 line_obj)) (cadr (nth 2 line_obj)))) tol)  
    ;; 2. Центровка по X (среднее между центрами текстов)
    (<= (abs (- (car mid_l) (/ (+ (car p1) (car p2)) 2.0))) tol)
    ;; 3. Положение по Y:
    ;; Линия должна быть выше "верха нижнего текста минус допуск"
    ;; И ниже "низа верхнего текста плюс допуск"
    (>= (cadr mid_l) (- y_bot_top tol))
    (<= (cadr mid_l) (+ y_top_bot tol))
  )
) ;; end of is-fraction-line

;; Подчеркивание: допуск 2.0*tol вниз (наружу), 1.0*tol вверх (на текст)
(defun is-underlined (text_obj line_obj tol / p_min p_max p_cen tc_x tw tb_y mid_l ll)
  (setq p_min (nth 1 text_obj) p_max (nth 2 text_obj) p_cen (nth 3 text_obj))
  (setq tc_x (car p_cen) tw (- (car p_max) (car p_min)) tb_y (cadr p_min))
  (setq mid_l (nth 3 line_obj))
  
  (if (<= (abs (- (car mid_l) tc_x)) (* 2.0 tol))
    (progn
      (setq ll (distance (nth 1 line_obj) (nth 2 line_obj)))
      (and (<= (abs (- (cadr (nth 1 line_obj)) (cadr (nth 2 line_obj)))) tol)
           (<= (cadr mid_l) (+ tb_y (* 1.13 tol)))               ;; Наезд на текст (вверх)
           (>= (cadr mid_l) (- tb_y (* 2.0 tol)))      ;; Зазор вниз (наружу)
           (>= ll (- tw (* 2.0 tol))) (<= ll (+ tw (* 2.0 tol))))))
)

;; Надчеркивание: допуск 2.0*tol вверх (наружу), 1.0*tol вниз (на текст)
(defun is-overlined (text_obj line_obj tol / p_min p_max p_cen tc_x tw tt_y mid_l ll)
  (setq p_min (nth 1 text_obj) p_max (nth 2 text_obj) p_cen (nth 3 text_obj))
  (setq tc_x (car p_cen) tw (- (car p_max) (car p_min)) tt_y (cadr p_max))
  (setq mid_l (nth 3 line_obj))
  
  (if (<= (abs (- (car mid_l) tc_x)) (* 2.0 tol))
    (progn
      (setq ll (distance (nth 1 line_obj) (nth 2 line_obj)))
      (and (<= (abs (- (cadr (nth 1 line_obj)) (cadr (nth 2 line_obj)))) tol)
           (<= (cadr mid_l) (+ tt_y (* 2.0 tol)))      ;; Зазор вверх (наружу)
           (>= (cadr mid_l) (- tt_y (* 1.13 tol)))               ;; Наезд на текст (вниз)
           (>= ll (- tw (* 2.0 tol))) (<= ll (+ tw (* 2.0 tol))))))
)
;; Проверка: образуют ли два текста вертикальную пару для дроби
(defun is-pair-text (t1 t2 tol / p1 p2 h1 h2 h_avg y_dist)
    (setq p1 (nth 3 t1) p2 (nth 3 t2) 
          h1 (nth 4 t1) h2 (nth 4 t2)
          h_avg (/ (+ (abs h1) (abs h2)) 2.0)
          y_dist (abs (- (cadr p1) (cadr p2))))
    (and (<= (abs (- (car p1) (car p2))) (* tol 2))
         (>= y_dist (* 0.9 h_avg))
         (<= y_dist (* 1.7 h_avg))
         (<= (abs (- h1 h2)) tol))
  ) ;; end of is-pair-text
;; Применение форматирования к тексту (%%U, %%O, %%K) без дублирования
;; Проверка: образуют ли тексты дробь на основе их подчеркиваний (Сценарий Б)
(defun is-fraction (t1 t2 / s1 s2 u1 o2)
  ;; Сортировка для проверки: t1 - верх (на %%U), t2 - низ (на %%O)
  (if (< (cadr (nth 3 t1)) (cadr (nth 3 t2)))
      (setq s1 (strcase (cdr (assoc 1 (nth 6 t2))))
            s2 (strcase (cdr (assoc 1 (nth 6 t1)))))
      (setq s1 (strcase (cdr (assoc 1 (nth 6 t1))))
            s2 (strcase (cdr (assoc 1 (nth 6 t2))))))
  
  (setq u1 (vl-string-search "%%U" s1)
        o2 (vl-string-search "%%O" s2))
  
  ;; Ваша уточненная логика
  (or (and u1 o2) (or u1 o2))
) ;; end of is-fraction

;; Очистка текста от управляющих кодов %%U и %%O из данных объекта
(defun ClearText_OU (text_obj / txt pos_u pos_o)
  (setq txt (cdr (assoc 1 (nth 6 text_obj))))
  (while (setq pos_u (vl-string-search "%%U" (strcase txt)))
    (setq txt (vl-string-subst "" (substr txt (1+ pos_u) 3) txt pos_u))
  )
  (while (setq pos_o (vl-string-search "%%O" (strcase txt)))
    (setq txt (vl-string-subst "" (substr txt (1+ pos_o) 3) txt pos_o))
  )
  txt
) ;; end of ClearText_OU
;; Применение форматирования к тексту и возврат обновленного объекта
(defun MakeSimpleText (text_obj under_lines over_lines strike_lines / ent data txt_str upper_txt new_data)
  (setq ent (car text_obj))
  (if (setq data (entget ent))
    (progn
      (setq txt_str (cdr (assoc 1 data)) upper_txt (strcase txt_str))
      (if (and under_lines (not (vl-string-search "%%U" upper_txt))) (setq txt_str (strcat "%%U" txt_str)))
      (if (and over_lines (not (vl-string-search "%%O" upper_txt))) (setq txt_str (strcat "%%O" txt_str)))
      (if (and strike_lines (not (vl-string-search "%%K" upper_txt))) (setq txt_str (strcat "%%K" txt_str)))
      
      (entmod (subst (cons 1 txt_str) (assoc 1 data) data))
      
      ;; Пересчет границ после модификации
      (setq new_data (entget ent))
      (setq res (PO:ProcessTextEntity ent new_data "TEXT"))
      
      (delete-objects under_lines) (delete-objects over_lines) (delete-objects strike_lines)
      
      ;; Возвращаем объект в формате (ename p_min p_max p_cen height color entget_data)
      (if res
        (list ent (nth 0 res) (nth 1 res) (nth 2 res) (nth 3 res) (nth 5 text_obj) new_data)
        nil
      )
    )
  )
) ;; end of MakeSimpleText

(defun is-struck (text_obj line_obj tol / p_min p_max p_cen tc_x tc_y tb_y tt_y tw td mid_l ll ydl mid_x mid_y)
  (setq p_cen (nth 3 text_obj) 
        p_min (nth 1 text_obj) 
        p_max (nth 2 text_obj))
  
  (setq tc_x (car p_cen) 
        tc_y (cadr p_cen) 
        tb_y (cadr p_min) 
        tt_y (cadr p_max) 
        tw (- (car p_max) (car p_min))
        td (sqrt (+ (expt tw 2) (expt (- tt_y tb_y) 2))))

  (setq mid_l (nth 3 line_obj)
        mid_x (car mid_l)
        mid_y (cadr mid_l))

  ;; 1. Џроверка совпадениЯ центров по X (базовый фильтр)
  (if (<= (abs (- mid_x tc_x)) (* 2.0 tol))
    (progn
      (setq ll (distance (nth 1 line_obj) (nth 2 line_obj))
            ydl (abs (- (cadr (nth 1 line_obj)) (cadr (nth 2 line_obj)))))
      
      (or 
        ;; ‘ценарий Ђ: ѓоризонтальное зачеркивание
        (and 
          (<= ydl (* 2.0 tol))                             ; ѓоризонтальность линии
          (> mid_y (+ tb_y tol))                           ; ‚ыше низа + допуск
          (< mid_y (- tt_y tol))                           ; Ќиже верха - допуск
          (>= ll (- tw (* 2.0 tol)))                       ; „лина ~ ширине текста
          (<= ll (+ tw (* 2.0 tol))))

        ;; ‘ценарий Ѓ: Ќаклонное зачеркивание
        (and 
          (> ydl (* 2.0 tol))                              ; ‹иниЯ имеет наклон
          (<= (abs (- mid_y tc_y)) (* 2.0 tol))            ; –ентр Y совпадает с центром текста
          (<= (abs (- mid_x tc_x)) (* 2.0 tol))            ; –ентр X совпадает с центром текста
          (>= ll (- td (* 2.0 tol)))                       ; „лина ~ диагонали текста
          (<= ll (+ td (* 2.0 tol))))
      )
    )
  )
)
;; Создание дроби и возврат данных нового MText объекта
(defun MakeFraction (t1 t2 f_line u_lines o_lines s_lines / 
                    p_num p_den s_num s_den ins_pt f_txt h_avg c_num t_style new_ent new_data res)
  (if (> (cadr (nth 3 t1)) (cadr (nth 3 t2))) (setq p_num t1 p_den t2) (setq p_num t2 p_den t1))
  (setq h_avg (/ (+ (abs (nth 4 t1)) (abs (nth 4 t2))) 2.0) c_num (nth 5 p_num))
  (setq s_num (ClearText_OU p_num)
        s_den (ClearText_OU p_den)
        t_style (cdr (assoc 7 (nth 6 p_num))))
  
  (setq ins_pt (list (car (nth 1 p_num)) (+ (cadr (nth 2 p_num)) (* 0.2 h_avg)) 0.0))
  (setq f_txt (strcat "{\\H0.98x;\\C" (itoa c_num) ";\\S" s_num "/" s_den ";}"))
  
  (setq new_ent (entmakex (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                                (cons 10 ins_pt) (cons 40 h_avg) (cons 1 f_txt) 
                                (cons 7 t_style) (cons 62 c_num))))
  
  (if new_ent
    (progn
      (setq new_data (entget new_ent))
      ;; Пересчет границ нового MText
      (setq res (PO:ProcessTextEntity new_ent new_data "MTEXT"))
      
      (delete-objects (list t1 t2))
      (if f_line (delete-objects (list f_line)))
      (delete-objects u_lines) (delete-objects o_lines) (delete-objects s_lines)
      
      ;; Возвращаем новый объект в общем формате
      (if res
        (list new_ent (nth 0 res) (nth 1 res) (nth 2 res) (nth 3 res) c_num new_data)
        nil
      )
    )
  )
) ;; end of MakeFraction



  
  



 
;; --- 2. Основной алгоритм с тройным циклом ---

(setq new_text_list '() 
      new_mtext_list mtext_list 
      temp_text_list text_list)

(while temp_text_list
  ;; Берем первый текст из очереди
  (setq current_text (car temp_text_list) 
        temp_text_list (cdr temp_text_list))
  
  (if (and current_text (car current_text))
    (progn
      (setq pt_center1 (nth 3 current_text)
            text1_height (nth 4 current_text)
            txt_y (cadr pt_center1)
            search_range (* text1_height 2.5))

      ;; --- ЦИКЛ 2: Поиск пары для текущего текста ---
      (setq found_pair nil pair_text nil i 0)
      (setq pair_search_list temp_text_list) ;; Используем копию для поиска

      (while (and (not found_pair) pair_search_list)
        (setq pair_candidate (car pair_search_list)
              pair_search_list (cdr pair_search_list))
        
        ;; Сравнение по Y (список отсортирован от большего к меньшему)
        (if (> (- txt_y (cadr (nth 3 pair_candidate))) (* text1_height 2.0))
          (setq pair_search_list nil) ;; Слишком низко, прекращаем поиск пары
          (if (is-pair-text current_text pair_candidate tolerance)
            (setq found_pair t pair_text pair_candidate)
          )
        )
      )

      ;; Если пара найдена, исключаем её из основного списка обработки
      (if found_pair 
          (setq temp_text_list (vl-remove pair_text temp_text_list)))

      ;; --- ЦИКЛ 3: Сбор линий (Очищающий проход через car/cdr) ---
      (setq c_under '() c_over '() c_strike '() ;; Линии для текущего текста
            p_under '() p_over '() p_strike '() ;; Линии для пары
            f_line nil                          ;; Линия-разделитель
            temp_lines lines_list              
            remaining_lines '())               

      (while temp_lines
        (setq line_obj (car temp_lines)
              temp_lines (cdr temp_lines)
              ln_y (cadr (nth 3 line_obj))
              is_used nil)

        ;; Проверка попадания линии в вертикальную зону поиска
        (if (<= (abs (- txt_y ln_y)) search_range)
          (progn
            ;; 1. Приоритет: Разделитель дроби (Сценарий А)
            (if (and found_pair (not f_line) (is-fraction-line current_text pair_text line_obj tolerance))
                (setq f_line line_obj is_used t)
                (progn
                  ;; 2. Линии для текущего (верхнего) текста
                  (cond 
                    ((is-underlined current_text line_obj tolerance) (setq c_under (cons line_obj c_under) is_used t))
                    ((is-overlined current_text line_obj tolerance)  (setq c_over (cons line_obj c_over) is_used t))
                    ((is-struck current_text line_obj tolerance)     (setq c_strike (cons line_obj c_strike) is_used t))
                  )
                  ;; 3. Линии для найденной пары (если не заняты первым текстом)
                  (if (and found_pair (not is_used))
                    (cond 
                      ((is-underlined pair_text line_obj tolerance) (setq p_under (cons line_obj p_under) is_used t))
                      ((is-overlined pair_text line_obj tolerance)  (setq p_over (cons line_obj p_over) is_used t))
                      ((is-struck pair_text line_obj tolerance)     (setq p_strike (cons line_obj p_strike) is_used t))
                    )
                  )
                )
            )
          )
        )
        
        ;; Если линия не подошла ни одному тексту, сохраняем её в остатке
        (if (not is_used)
            (setq remaining_lines (cons line_obj remaining_lines)))
      )
      
      ;; Обновляем глобальный список линий (убираем "съеденные")
      (setq lines_list remaining_lines)

      ;; --- ПРИНЯТИЕ РЕШЕНИЯ И СОЗДАНИЕ ОБЪЕКТОВ ---
      (cond
        ;; СЛУЧАЙ 1: Классическая дробь (Есть пара + есть физическая линия)
        ((and found_pair f_line)
         (setq res (MakeFraction current_text pair_text f_line 
                                 (append c_under p_under) (append c_over p_over) (append c_strike p_strike)))
         (if res (setq new_mtext_list (cons res new_mtext_list))))

        ;; СЛУЧАЙ 2: Дробь по форматированию (Есть пара + признаки %%U/%%O)
        ((and found_pair (is-fraction current_text pair_text))
         (setq res (MakeFraction current_text pair_text nil 
                                 (append c_under p_under) (append c_over p_over) (append c_strike p_strike)))
         (if res (setq new_mtext_list (cons res new_mtext_list))))

        ;; СЛУЧАЙ 3: Обычные тексты (Дробь не сложилась или пары нет)
        (t
         ;; Обрабатываем текущий текст
         (setq res (MakeSimpleText current_text c_under c_over c_strike))
         (if res (setq new_text_list (cons res new_text_list)))
         
         ;; Если пара была найдена, обрабатываем её сразу здесь же
         (if found_pair
           (progn
             (setq res (MakeSimpleText pair_text p_under p_over p_strike))
             (if res (setq new_text_list (cons res new_text_list)))
           )
         )
        )
      )
    )
  )
)
  
  (CT:Debug 2 "message" (strcat "Змянене колькасці рыс" (itoa (- lines_count (length lines_list)))))
  (CT:Debug 2 "list" lines_list)
  (CT:Debug 2 "message" (strcat "Змянене колькасці тэксту" (itoa (- (length new_text_list) text_count))))
  (CT:Debug 2 "list" new_text_list)
  (CT:Debug 2 "message" (strcat "Змянене колькасці мтэксту" (itoa (- (length new_mtext_list) mtext_count))))
  (CT:Debug 2 "list" new_mtext_list)
  (list lines_list new_text_list new_mtext_list)
);; 02 Конец функции defineTextAndLine
;; ________________________________



;; =================================================================
;;  03 Функцыя геаметрычнага аб'яднання ліній
;; =================================================================
(defun CT:Mergelines (line_list final_trash tolerance / temp_count horizontal_lines vertical_lines)
  (setq temp_count (+ (length line_list) (sslength final_trash)))
  (setq line_results (CT:ProcessRemainingLines line_list final_trash tolerance))
  (setq horizontal_lines (nth 0 line_results))  ; Гарызантальныя рысы
  (setq vertical_lines (nth 1 line_results))    ; Вяртыкальныя рысы
  
  ;; Геаметрычнае аб'яднанне
  (setq horizontal_lines (PRL:MergeOverlappingSegments horizontal_lines tolerance nil))
  (setq vertical_lines (PRL:MergeOverlappingSegments vertical_lines tolerance T))
 
  ;; Вывод вынікаў
  (if (/= temp_count(+  (length horizontal_lines) (length vertical_lines) (sslength final_trash)))
  (princ (strcat "\nЗмяненне колькасці рыс: " (itoa (- temp_count(+  (length horizontal_lines) (length vertical_lines) (sslength final_trash))))))
  );end if
  (CT:Debug 3 "list" horizontal_lines)
  (CT:Debug 3 "list" vertical_lines)
  (CT:Debug 3 "ss" final_trash)
  (list horizontal_lines vertical_lines)
)
;; =================================================================
;;  03.1 Функцыя геаметрычнага аб'яднання накладваючыхся і калінеарных ліній
;; =================================================================
(defun PRL:MergeOverlappingSegments (lines_list tolerance is_vertical /
       sorted_list merged_list
       current_seg next_seg
       cur_start cur_end cur_const cur_ent cur_layer cur_color
       next_start next_end next_const
       new_start_pt new_end_pt new_ent
       p1 p2)
  
   ;; 1. Сартаванне спісу
  (setq sorted_list
    (vl-sort lines_list
      (function
        (lambda (a b)
          (setq p1 (nth 1 a) p2 (nth 1 b)) ; Бярэм pt_start
          (if is_vertical
            ;; Для вертыкальных рысаў: Сартаванне па X, затым па Y
            (if (< (abs (- (car p1) (car p2))) tolerance)
              (< (cadr p1) (cadr p2)) ; Калі X роўныя, параўноўваем Y
              (< (car p1) (car p2))   ; Інакш параўноўваем X
            )
            ;; Для гарызантальных рысаў: Сартаванне па Y, затым па X
            (if (< (abs (- (cadr p1) (cadr p2))) tolerance)
              (< (car p1) (car p2))   ; Калі Y роўныя, параўноўваем X
              (< (cadr p1) (cadr p2)) ; Інакш параўноўваем Y
            )
          )
        )
      )
    )
  )
  
  (setq merged_list '())
  
  ;; 2. Ітэрацыя па адсартаваным спісе
  (while sorted_list
    ;; Бярэм першы элемент як бягучы "акумулятар"
    (setq current_seg (car sorted_list))
    (setq sorted_list (cdr sorted_list))
    (setq cur_ent (car current_seg))
    
    ;; Атрымліваем уласцівасці (бяспечна, калі cur_ent існуе)
    (if cur_ent
      (progn
        (setq cur_layer (cdr (assoc 8 (entget cur_ent))))
        (setq cur_color (cdr (assoc 62 (entget cur_ent))))
      )
      (setq cur_layer "0" cur_color nil) ;; На ўсякі выпадак
    )
    
    ;; Вызначаем каардынаты для параўнання
    (if is_vertical
      (setq cur_const (car (nth 1 current_seg))      ; X
            cur_start (cadr (nth 1 current_seg))     ; Y min
            cur_end   (cadr (nth 2 current_seg)))    ; Y max
      (setq cur_const (cadr (nth 1 current_seg))     ; Y
            cur_start (car (nth 1 current_seg))      ; X min
            cur_end   (car (nth 2 current_seg)))     ; X max
    )
    
    (setq continue_merge T)
    
    ;; Унутраны цыкл: спрабуем "з'есці" наступныя лініі
    (while (and continue_merge sorted_list)
      (setq next_seg (car sorted_list))
      (if is_vertical
        (setq next_const (car (nth 1 next_seg))
              next_start (cadr (nth 1 next_seg))
              next_end   (cadr (nth 2 next_seg)))
        (setq next_const (cadr (nth 1 next_seg))
              next_start (car (nth 1 next_seg))
              next_end   (car (nth 2 next_seg)))
      )
      
      ;; Праверка 1: Ці ляжаць лініі на адной "прамой"
      (if (<= (abs (- cur_const next_const)) tolerance)
        (progn
          ;; Праверка 2: Ці перасякаюцца яны альбо кранаюцца?
          (if (<= next_start (+ cur_end tolerance))
            (progn
              ;; === АБ'ЯДНАННЕ ===
              ;; Пашыраем бягучы канец, калі наступны даўжэйшы
              (if (> next_end cur_end)
                (setq cur_end next_end)
              )
              ;; Выдаляем стары "з'едзены" аб'ект з чарцяжа
              (if (and (car next_seg) (entget (car next_seg))) (entdel (car next_seg)))
              ;; Выдаляем бягучы аб'ект з чарцяжа (толькі адзін раз)
              (if (and cur_ent (entget cur_ent)) (entdel cur_ent))
              ;; Выдаляем апрацаваны сегмент з спісу
              (setq sorted_list (cdr sorted_list))
              ;; Адзначаем, што бягучы аб'ект "распусціўся" у новым
              (setq cur_ent nil)
            )
            ;; Інакш: Разрыў занадта вялікі, перарываць унутраны цыкл
            (setq continue_merge nil)
          )
        )
        ;; Інакш: Зрушыліся па асноўнай каардынаце, перарываць унутраны цыкл
        (setq continue_merge nil)
      )
    ) ;; канец while continue_merge
    
    ;; Калі cur_ent стаў nil, значыць было аб'яднанне -> ствараем новую лінію
    (if (null cur_ent)
      (progn
        ;; Фарміруем пункты
        (if is_vertical
          (setq new_start_pt (list cur_const cur_start 0.0)
                new_end_pt   (list cur_const cur_end 0.0))
          (setq new_start_pt (list cur_start cur_const 0.0)
                new_end_pt   (list cur_end cur_const 0.0))
        )
        ;; ENTMAKE з абаронай ад пустых палёў
        (setq new_ent (entmakex
          (vl-remove-if 'null
            (list
              '(0 . "LINE")
              (cons 10 new_start_pt)
              (cons 11 new_end_pt)
              ;; Слой абавязковы
              (cons 8 (if cur_layer cur_layer "0"))
              ;; Колер апцыянальны
              (if cur_color (cons 62 cur_color))
            )
          )
        ))
        (if new_ent
          (setq merged_list (cons (list new_ent new_start_pt new_end_pt) merged_list))
        )
      )
      ;; Калі аб'яднання не было (cur_ent жывы), проста дадаем яго ў спіс як ёсць
      (setq merged_list (cons current_seg merged_list))
    )
  )
  
  (reverse merged_list)
)

;; =================================================================
;; 03.2 Функцыя апрацоўкі застаўшыхся ліній
;; =================================================================
;; ________________________________
(defun CT:ProcessRemainingLines (lines_list final_trash tolerance / 
                           horizontal_list vertical_list other_list
                           nabor
                           result_horizontal result_vertical
                           double_tolerance
                           i poly_obj poly_data vertices
                           pt_start pt_end pt_temp valid_polyline
                           new_line_ent final_horizontal final_vertical)
  (princ "\n------------------------")
  (princ "\n03 ЗЛУЧЭННЕ РЫС")
  ;; =============== УДВАИВАЕМ ДОПУСК ===============
  (setq double_tolerance (* 2.0 tolerance))
  
  ;; =============== ШАГ 1: РАЗДЕЛЕНИЕ НА 2 СПИСКА + ДАДАВАННЕ У СМЕЦЦЕ ===============
  (setq horizontal_list '())
  (setq vertical_list '())
  (setq other_list '()) ; тымчасовы спіс для адладкі
  
  (foreach line_obj lines_list
    (setq pt_start (nth 1 line_obj))
    (setq pt_end (nth 2 line_obj))
    (if (<= (abs (- (cadr pt_start) (cadr pt_end))) double_tolerance)
      (setq horizontal_list (cons line_obj horizontal_list))
      (if (<= (abs (- (car pt_start) (car pt_end))) double_tolerance)
        (setq vertical_list (cons line_obj vertical_list))
        ;; Нахільныя лініі - дадаем у смецце
        (progn
          (if (and (car line_obj) (entget (car line_obj)) final_trash)
            (ssadd (car line_obj) final_trash) ; < дадаем у смецце
          )
          (setq other_list (cons line_obj other_list)) ; для статыстыкі
        )
      )
    )
  )
  ;; Вспомогательная функция: получить список всех объектов в чертеже
(defun list-all-entities (/ ent all_ents)
  (setq all_ents '())
  (setq ent (entnext))
  (while ent
    (setq all_ents (cons ent all_ents))
    (setq ent (entnext ent))
  )
  (reverse all_ents)
)

;; Вспомогательная функция: найти новые объекты после операции
(defun get-new-entities (before after / new_ents)
  (setq new_ents '())
  (foreach ent after
    (if (not (member ent before))
      (setq new_ents (cons ent new_ents))
    )
  )
  (reverse new_ents)
)

;; Вспомогательная функция: найти неизмененные линии
(defun get-unchanged-lines (original_lines current_entities / unchanged)
  (setq unchanged '())
  (foreach line original_lines
    (if (and (car line) (member (car line) current_entities))
      (setq unchanged (cons line unchanged))
    )
  )
  (reverse unchanged)
)
  ;; =============== ВСПОМОГАТЕЛЬНАЯ ФУНКЦИЯ ОБЪЕДИНЕНИЯ ===============
  (defun JoinLines (line_list tolerance / ss_temp
                  ent_before new_ents ent result_ss entities_before entities_after
                  unchanged_lines new_lines i)
  
  ;; ВАЖНОЕ УЛУЧШЕНИЕ 1: Обработка одиночных линий
  (if line_list
    (if (= (length line_list) 1)
      (progn
        ;; Создаем selection set с одной линией
        (setq ss_temp (ssadd))
        (ssadd (car (car line_list)) ss_temp)
        ss_temp
      )
      ;; Основной блок для двух и более линий
      (progn
        ;; Запоминаем все существующие объекты ДО операции
        (setq entities_before (list-all-entities))
        
        (setq ss_temp (ssadd))
        (foreach line_obj line_list
          (if (and (car line_obj) (entget (car line_obj)))
            (ssadd (car line_obj) ss_temp)
          )
        )
        
        (if (<= (sslength ss_temp) 1)
          (progn
            (princ "\nJoinLines: Недостаточно линий для объединения")
            ss_temp ;; Возвращаем исходный ss_temp с одной линией
          )
          (progn
           
            ;; Выполняем объединение
            (if (= (getvar "PEDITACCEPT") 1)
              (vl-cmdf "_.pedit" "_Multiple" ss_temp "" "_Join" tolerance "")
              (vl-cmdf "_.pedit" "_Multiple" ss_temp "" "_Yes" "_Join" tolerance "")
            )
            
          
            ;; ВАЖНОЕ УЛУЧШЕНИЕ 2: Надежное определение результатов
            (setq entities_after (list-all-entities))
            (setq new_lines (get-new-entities entities_before entities_after))
            (setq unchanged_lines (get-unchanged-lines line_list entities_after))
            
            ;; Создаем результатирующий selection set
            (setq result_ss (ssadd))
            
            ;; Добавляем новые полилинии
            (foreach e new_lines
              (if (wcmatch (cdr (assoc 0 (entget e))) "*POLYLINE")
                (ssadd e result_ss)
              )
            )
            
            ;; Добавляем неизмененные линии
            (foreach line unchanged_lines
              (ssadd (car line) result_ss)
            )
            
            ;; Отладочная информация
            (if (> (sslength result_ss) 0)
                result_ss
                ss_temp ;; Возвращаем исходный набор
            )
          )
        )
      )
    )
  )
)
  
  ;; =============== ШАГ 2: ОБЪЕДИНЕНИЕ С УДВОЕННЫМ ДОПУСКОМ ===============
  (setq result_horizontal (JoinLines horizontal_list double_tolerance))
  (setq result_vertical (JoinLines vertical_list double_tolerance))
  
  ;; =============== ШАГ 3: ЗАМЕНА ПОЛИЛИНИЙ НА ОТРЕЗКИ ===============
  (defun ReplacePolylinesWithLines (ss_poly tolerance ifvertical / result_list i poly_obj poly_data vertices vertex_list pt_start pt_end valid_polyline new_line_ent)
  (setq result_list '())
  (if ss_poly
    (progn
      (setq i 0)
      (while (< i (sslength ss_poly))
        (setq poly_obj (ssname ss_poly i))
        (setq poly_data (entget poly_obj))
        
        (if (wcmatch (cdr (assoc 0 poly_data)) "*POLYLINE")
          (progn
            ;; Отримуємо вершини залежно від типу полілінії
            (if (= (cdr (assoc 0 poly_data)) "LWPOLYLINE")
              (setq vertices (vl-remove-if-not '(lambda (x) (= (car x) 10)) poly_data))
              (setq vertices (get_polylines_vertices poly_obj)) ; Для старих POLYLINE
            )
            
            (setq vertex_list (mapcar 'cdr vertices))
            
            (if (>= (length vertex_list) 2) ; Перевіряємо, що є хоча б 2 точки
              (progn
                (setq pt_start (car vertex_list))
                (setq pt_end (last vertex_list))
                
                (setq valid_polyline (= (length vertex_list) 2)) ; True якщо рівно 2 точки
                
                ;; Якщо більше 2 точок, перевіряємо чи всі проміжні точки близькі до кінців
                (if (not valid_polyline)
                  (progn
                    (setq valid_polyline T)
                    (foreach pt_temp (cdr (reverse (cdr (reverse vertex_list))))
		      (if ifvertical
			(if (not (<= (abs (- (car pt_temp) (car pt_start))) tolerance))
                        (setq valid_polyline nil)
                      )
			(if (not (<= (abs (- (nth 1 pt_temp) (nth 1 pt_start))) tolerance))
                        (setq valid_polyline nil)
                      )
		       )
                    )
                  )
                )
                
                (if valid_polyline
                  (progn
                    (setq new_line_ent
  (entmakex
    (vl-remove-if 'null
      (list 
        '(0 . "LINE")
        (cons 10 pt_start)
        (cons 11 pt_end)
        (assoc 8 poly_data)   ; может быть nil
        (assoc 62 poly_data)  ; часто nil!
      )
    )
  )
)
                    
                    (if new_line_ent
                      (progn
                        (entdel poly_obj)
                        (setq result_list (cons (list new_line_ent pt_start pt_end) result_list))
                      )
                      (setq result_list (cons (list poly_obj pt_start pt_end) result_list))
                    )
                  )
                  (setq result_list (cons (list poly_obj pt_start pt_end) result_list))
                )
              )
              (setq result_list (cons (list poly_obj nil nil) result_list))
            )
          )
          (setq result_list (cons (list poly_obj nil nil) result_list))
        )
        (setq i (1+ i))
      )
    )
  )
  result_list
)

;; Допоміжна функція для отримання вершин старих POLYLINE
(defun get_polylines_vertices (poly_obj / vertices vertex_obj)
  (setq vertices '())
  (setq vertex_obj (entnext poly_obj))
  (while (and vertex_obj (= (cdr (assoc 0 (entget vertex_obj))) "VERTEX"))
    (setq vertices (cons (assoc 10 (entget vertex_obj)) vertices))
    (setq vertex_obj (entnext vertex_obj))
  )
  (reverse vertices)
)
;-------------end 
  
  ;; Применяем замену для горизонтальных и вертикальных полилиний
  (setq final_horizontal (ReplacePolylinesWithLines result_horizontal double_tolerance nil))
  (setq final_vertical (ReplacePolylinesWithLines result_vertical double_tolerance T))
   
  ;; =============== ВОЗВРАТ ТОЛЬКІ ДВУХ СПИСКАЎ ===============
  (list
    final_horizontal
    final_vertical
    ; other_list больш не вяртаецца
  )
)
;; =================================================================
;; 03 Канец Функцыя апрацоўкі застаўшыхся ліній
;; =================================================================
;; =============================================================================
;; === 04.2 НАЧАЛО ФУНКЦИИ: SortTableClusters ===
;; =============================================================================
(defun CL:SortTableClusters (tables_list tol / sorted_tables)


  ;; 1. Сортировка кластеров таблиц (сверху-вниз, затем слева-направо)
  (setq sorted_tables 
    (vl-sort tables_list 
      '(lambda (a b / y_a y_b x_a x_b) 
        ;; Получаем Ymax (индекс 3) и Xmin (индекс 0) для обеих таблиц
        (setq x_a (car (nth 2 a))
              y_a (nth 3 (nth 2 a))
              x_b (car (nth 2 b))
              y_b (nth 3 (nth 2 b)))
              
        (if (> (abs (- y_a y_b)) tol)
          (> y_a y_b) ;; Сначала по высоте (Y)
          (< x_a x_b) ;; Если высота одна — по горизонтали (X)
        )
      )
    )
  )

  ;; 2. Внутренняя сортировка линий в каждой таблице
  (setq sorted_tables
    (mapcar 
      '(lambda (table / h_list v_list bbox)
        (setq h_list (car table)
              v_list (cadr table)
              bbox   (nth 2 table))

        ;; Сортируем горизонтальные линии: сверху-вниз (по убыванию Y)
        (setq h_list (vl-sort h_list 
          '(lambda (l1 l2) (> (cadr (nth 1 l1)) (cadr (nth 1 l2))))))
        
        ;; Сортируем вертикальные линии: слева-направо (по возрастанию X)
        (setq v_list (vl-sort v_list 
          '(lambda (l1 l2) (< (car (nth 1 l1)) (car (nth 1 l2))))))

        (list h_list v_list bbox)
      )
      sorted_tables
    )
  )

  sorted_tables
)
;; =============================================================================
;; === КОНЕЦ ФУНКЦИИ: SortTableClusters ===
;; =============================================================================
;; =================================================================
;; 04 Новая функцыя кластарызацыі табліц (Метад звязных кампанентаў)
;;    Дададзены аргумент current_bbox для карэктнага абнаўлення межаў
;; =================================================================
(defun CL:ExtractV (h_line v_pool tol current_bbox / p1 p2 h_x1 h_x2 h_y found remaining v v_p1 v_p2 v_x v_y1 v_y2 bbox)
  ;; Капіруем bbox, каб не псаваць знешні, калі нічога не знойдзена (хаця тут гэта не крытычна)
  (setq bbox current_bbox) 
  (setq p1 (nth 1 h_line)
        p2 (nth 2 h_line)
        h_x1 (- (min (car p1) (car p2)) tol)
        h_x2 (+ (max (car p1) (car p2)) tol)
        h_y  (cadr p1) ;; Лічым, што Y1 = Y2 для гарызантальнай лініі
        found '()
        remaining '())
  
  (while v_pool
    (setq v (car v_pool)
          v_pool (cdr v_pool)
          v_p1 (nth 1 v)
          v_x (car v_p1)) ;; X каардыната пачатку вертыкальнай лініі
    (cond
      ;; Лінія злева - дадаць у застаўшыяся
      ((< v_x h_x1) (setq remaining (cons v remaining)))
      ;; Лінія справа - дадаць у застаўшыяся і выйсці раней (pool сартаваны па X)
      ((> v_x h_x2)
        (setq remaining (cons v remaining))
        (setq remaining (append (reverse v_pool) remaining)) ;; Дадаем хвост спісу
        (setq v_pool nil)) ;; Спыняем цыкл
      ;; Тая ж X вобласць - праверыць перасячэнне па Y
      (t
        (setq v_p2 (nth 2 v)
              v_y1 (- (min (cadr v_p1) (cadr v_p2)) tol)
              v_y2 (+ (max (cadr v_p1) (cadr v_p2)) tol))
       (if (<= (- v_y1 tol) h_y (+ v_y2 tol))
         (progn
           (setq found (cons v found))
           ;; ОБНОВЛЯЕМ ГРАНИЦЫ (BBOX)
           (setq bbox (list (min (car bbox) v_x)          ;; Xmin
                            (max (cadr bbox) v_x)         ;; Xmax
                            (min (caddr bbox) v_y1)       ;; Ymin
                            (max (cadddr bbox) v_y2)))    ;; Ymax
         )
         (setq remaining (cons v remaining)) ;; Не перасеклася па Y
       )
      )
    )
  )
  ;; Возвращаем список: ( (найденные) (остаток) (новый_bbox) )
  (list found (reverse remaining) bbox)
)

;; =================================================================
;; 2. Экстракцыя гарызантальных ліній, якія перасякаюць вертыкальную
;;    Выпраўлена: дададзены аргумент current_bbox
;; =================================================================
(defun CL:ExtractH (v_line h_pool tol current_bbox / p1 p2 v_x v_y1 v_y2 found remaining h h_p1 h_p2 h_y h_x1 h_x2 bbox)
  (setq bbox current_bbox)
  (setq p1 (nth 1 v_line)
        p2 (nth 2 v_line)
        v_x  (car p1)
        v_y1 (- (min (cadr p1) (cadr p2)) tol)
        v_y2 (+ (max (cadr p1) (cadr p2)) tol)
        found '()
        remaining '())
  
  (while h_pool
    (setq h (car h_pool)
          h_pool (cdr h_pool)
          h_p1 (nth 1 h)
          h_y (cadr h_p1))
    (cond
      ;; Лінія ніжэй
      ((< h_y v_y1) (setq remaining (cons h remaining)))
      ;; Лінія вышэй - выйсці раней (pool сартаваны па Y)
      ((> h_y v_y2)
        (setq remaining (cons h remaining))
        (setq remaining (append (reverse h_pool) remaining))
        (setq h_pool nil))
      (t
        (setq h_p2 (nth 2 h)
              h_x1 (- (min (car h_p1) (car h_p2)) tol)
              h_x2 (+ (max (car h_p1) (car h_p2)) tol))
        (if (<= h_x1 v_x h_x2)
         (progn
           (setq found (cons h found))
           ;; ОБНОВЛЯЕМ ГРАНИЦЫ (BBOX)
           (setq bbox (list (min (car bbox) h_x1)
                            (max (cadr bbox) h_x2)
                            (min (caddr bbox) h_y)
                            (max (cadddr bbox) h_y)))
         )
          (setq remaining (cons h remaining))
        )
      )
    )
  )
  (list found (reverse remaining) bbox)
)

;;; =================================================================
;;; 04 Кластырызацыя
;;;
;;; Уваходныя параметры:
;;;   hor_list   - Спіс гарызантальных ліній[cite: 2]. 
;;;                Фармат: ((ename p1 p2) (ename p1 p2) ...) [cite: 2, 3]
;;;   ver_list   - Спіс вертыкальных ліній[cite: 2]. 
;;;                Фармат: ((ename p1 p2) (ename p1 p2) ...) [cite: 2, 4]
;;;   trash      - Selection Set (набор), куды скідаюцца лініі, што не 
;;;                ўтварылі табліцу (не маюць перасячэнняў).
;;;   tol        - Лічбавы допуск (Tolerance) для вызначэння кантакту 
;;;                паміж лініямі[cite: 4, 6].
;;;
;;; Выхадныя даныя (Return):
;;;   rezult     - Спіс кластараў, адсартаваных функцыяй CL:SortTableClusters.
;;;                Кожны кластар у спісе мае структуру:
;;;                (
;;;                  ( (ename p1 p2) ... ) ; Спіс гарызантальных ліній [cite: 7]
;;;                  ( (ename p1 p2) ... ) ; Спіс вертыкальных ліній [cite: 7]
;;;                  (min_x max_x min_y max_y) ; Bounding Box кластара [cite: 3, 7]
;;;                )
;;; =================================================================
(defun CT:ClusterLinesIntoTables (hor_list ver_list trash tol /
                                 h_pool v_pool tables_list seed queue_h queue_v res_split 
                                 cur_table_h cur_table_v cur_bbox res_h h v rezult)

  (princ "\n------------------------")
  (princ "\n04 Кластарызацыя рыс у табліцы")
  
  ;; Сортировка пулов
  (setq h_pool (vl-sort hor_list '(lambda (a b) (< (cadr (nth 1 a)) (cadr (nth 1 b))))))
  (setq v_pool (vl-sort ver_list '(lambda (a b) (< (car (nth 1 a)) (car (nth 1 b))))))
  (setq tables_list '())
  
  (while h_pool
    (setq seed (car h_pool)
          h_pool (cdr h_pool)
          cur_table_h (list seed)
          cur_table_v '()
          queue_h (list seed)
          queue_v '())

    (setq cur_bbox (list (min (car (nth 1 seed)) (car (nth 2 seed)))
                         (max (car (nth 1 seed)) (car (nth 2 seed)))
                         (cadr (nth 1 seed))
                         (cadr (nth 1 seed))))

    (while (or queue_h queue_v)
      (if queue_h
        (progn
          (setq h (car queue_h) queue_h (cdr queue_h))
          (setq res_split (CL:ExtractV h v_pool tol cur_bbox)) 
          (if (car res_split)
            (progn
              (setq queue_v (append queue_v (car res_split)))
              (setq cur_table_v (append cur_table_v (car res_split)))))
          (setq v_pool (cadr res_split))
          (setq cur_bbox (caddr res_split))
        )
      )
      (if queue_v
        (progn
          (setq v (car queue_v) queue_v (cdr queue_v))
          (setq res_h (CL:ExtractH v h_pool tol cur_bbox))
          (if (car res_h)
            (progn
              (setq queue_h (append queue_h (car res_h)))
              (setq cur_table_h (append cur_table_h (car res_h)))))
          (setq h_pool (cadr res_h))
          (setq cur_bbox (caddr res_h))
        )
      )
    )

    (if (and cur_table_h cur_table_v)
      (progn 
        (setq tables_list (cons (list cur_table_h cur_table_v cur_bbox) tables_list))
        
        ;; --- ОТЛАДКА: Показываем найденный кластер ---
		(CT:Debug 4 "list" cur_table_h)
		(CT:Debug 4 "list" cur_table_v)
      )
      (progn 
        (foreach h_item cur_table_h (ssadd (car h_item) trash))
        (foreach v_item cur_table_v (ssadd (car v_item) trash))
      )
    )
  )

  (if v_pool 
  ( foreach v_item v_pool 
  (ssadd (car v_item) trash))
)
  (CT:Debug 4 "ss" trash) 
  (setq rezult (CL:SortTableClusters tables_list tol))
  
  (princ (strcat "\nКолькасць знойдзеных табліц: " (itoa (length rezult))))
  ;(princ (strcat "\nАб'ектаў у кошыку (final_trash): " (itoa (sslength trash))))
  
  rezult
);; ==============Канец 04=================================

;;; =================================================================
;;; CT:ProcessClustersToCells
;;;
;;; Уваходныя параметры:
;;;   *GL_TABLES_LIST* - Спіс кластараў ліній. Кожны элемент:
;;;                      ( (hor_lines) (ver_lines) (bbox_coords) )
;;;   tolerance        - Лічбавы допуск для праверкі кантакту ліній.
;;;   trash            - Selection Set (набор) для збору ліній, якія 
;;;                      не ўтварылі сценкі ячэек. [cite: 1, 20]
;;;
;;; Выхадныя даныя (Return):
;;;   *GL_TABLES_LIST* - Абноўлены спіс табліц. Кожны элемент (табліца)
;;;                      цяпер мае структуру:
;;;                      (
;;;                        (bbox_xmin bbox_xmax bbox_ymin bbox_ymax) ; Межы
;;;                        ( ((x1 y1 z1) (x2 y2 z2)) ... )           ; Спіс ячэек
;;;                        (ename1 ename2 ...)                       ; Выкарыстаныя лініі
;;;                      ) [cite: 21]
;;; =================================================================
(defun CT:ProcessClustersToCells (*GL_TABLES_LIST* tolerance trash 
/ updated_tables all_cells_debug all_used_lines_debug result cluster_cells used_lines bbox)
  ;; Абавязковая загрузка ActiveX функцый
  (vl-load-com)
  
  (princ "\n------------------------")
  (princ "\n05 ВЫЗНАЧЭННЕ ЯЧЭЕК ТАБЛІЦЫ")

  (defun CT:GetCellsFromCluster (cluster tol all_garbage_ss / 
                                 h_lines v_lines bbox unique_x unique_y
                                 matrix_cols matrix_rows used_matrix r c
                                 cell_x1 cell_x2 cell_y1 cell_y2 cell_y_mid
                                 c_next r_next cells_found used_lines_list used_lines_ss)

    (defun GetUnique (vals tol / sorted res last_v)
      (setq sorted (vl-sort vals '<))
      (setq res (list (car sorted)) last_v (car sorted))
      (foreach v (cdr sorted) 
        (if (> (- v last_v) tol) 
          (setq res (cons v res) last_v v)
        )
      )
      (reverse res)
    )

    (defun is-v-wall (x y_top y_bot / found_l ly1 ly2)
      (setq found_l nil)
      (foreach l v_lines
        (if (<= (abs (- (car (nth 1 l)) x)) (* tol 1.5))
          (progn
            (setq ly1 (min (cadr (nth 1 l)) (cadr (nth 2 l)))
                  ly2 (max (cadr (nth 1 l)) (cadr (nth 2 l))))
            (if (and (<= ly1 (+ y_bot (* tol 2.0) 0.1)) 
                     (>= ly2 (- y_top (* tol 2.0) 0.1)))
              (progn
                (setq found_l t)
                (if (not (member (car l) used_lines_list))
                  (setq used_lines_list (cons (car l) used_lines_list))
                )
              )
            )
          )
        )
      )
      (or found_l (equal x (car unique_x) tol) (equal x (last unique_x) tol))
    )

    (defun is-h-wall (y x_left x_right / found_l lx1 lx2)
      (setq found_l nil)
      (foreach l h_lines
        (if (<= (abs (- (cadr (nth 1 l)) y)) (* tol 1.5))
          (progn
            (setq lx1 (min (car (nth 1 l)) (car (nth 2 l)))
                  lx2 (max (car (nth 1 l)) (car (nth 2 l))))
            (if (and (<= lx1 (+ x_left (* tol 2.0) 0.1)) 
                     (>= lx2 (- x_right (* tol 2.0) 0.1)))
              (progn
                (setq found_l t)
                (if (not (member (car l) used_lines_list))
                  (setq used_lines_list (cons (car l) used_lines_list))
                )
              )
            )
          )
        )
      )
      (or found_l (equal y (car unique_y) tol) (equal y (last unique_y) tol))
    )

    (setq h_lines (car cluster) 
          v_lines (cadr cluster) 
          bbox (nth 2 cluster)
          bbox_xmin (car bbox) bbox_xmax (cadr bbox) 
          bbox_ymin (nth 2 bbox) bbox_ymax (nth 3 bbox)
          used_lines_list '()
          cells_found '())

    (setq unique_x (GetUnique (mapcar '(lambda (x) (car (nth 1 x))) v_lines) tol))
    (if (> (abs (- (car unique_x) bbox_xmin)) (* tol 2.0)) (setq unique_x (cons bbox_xmin unique_x)))
    (if (> (abs (- (last unique_x) bbox_xmax)) (* tol 2.0)) (setq unique_x (append unique_x (list bbox_xmax))))
    (setq unique_x (vl-sort unique_x '<))

    (setq unique_y (GetUnique (mapcar '(lambda (x) (cadr (nth 1 x))) h_lines) tol))
    (if (> (abs (- (car unique_y) bbox_ymin)) (* tol 2.0)) (setq unique_y (cons bbox_ymin unique_y)))
    (if (> (abs (- (last unique_y) bbox_ymax)) (* tol 2.0)) (setq unique_y (append unique_y (list bbox_ymax))))
    (setq unique_y (vl-sort unique_y '>))

    (setq matrix_cols (1- (length unique_x)) 
          matrix_rows (1- (length unique_y)))

    ;; Сварэнне бяспечнага масіва (Safearray)
    ;; Выкарыстоўваем vlax-vbInteger (0/1) замест Boolean для надзейнасці ў розных версіях CAD
    (setq used_matrix (vlax-make-safearray vlax-vbInteger (cons 0 (1- (* matrix_cols matrix_rows)))))

    (setq r 0)
    (while (< r matrix_rows)
      (setq c 0)
      (while (< c matrix_cols)
        ;; ПРАВІЛЬНАЯ ФУНКЦЫЯ: vlax-safearray-get-element
        (if (= (vlax-safearray-get-element used_matrix (+ (* r matrix_cols) c)) 0)
          (progn
            (setq cell_y1 (nth r unique_y) 
                  cell_x1 (nth c unique_x) 
                  cell_y_mid (/ (+ (nth r unique_y) (nth (1+ r) unique_y)) 2.0))
            
            (setq c_next (1+ c))
            (while (and (< c_next matrix_cols) (not (is-v-wall (nth c_next unique_x) cell_y1 cell_y_mid)))
              (setq c_next (1+ c_next)))
            
            (setq cell_x2 (nth c_next unique_x)
                  r_next (1+ r))
            
            (while (and (< r_next matrix_rows) (not (is-h-wall (nth r_next unique_y) cell_x1 cell_x2)))
              (setq r_next (1+ r_next)))
            
            (setq cell_y2 (nth r_next unique_y))

            (if (and (> (abs (- cell_x2 cell_x1)) (* tol 2.1))
                     (> (abs (- cell_y1 cell_y2)) (* tol 2.1)))
              (setq cells_found (cons (list (list cell_x1 cell_y2 0.0) (list cell_x2 cell_y1 0.0)) cells_found))
            )
            
            (setq rr r)
            (while (< rr r_next)
              (setq cc c)
              (while (< cc c_next)
                ;; ПРАВІЛЬНАЯ ФУНКЦЫЯ: vlax-safearray-put-element
                (vlax-safearray-put-element used_matrix (+ (* rr matrix_cols) cc) 1)
                (setq cc (1+ cc)))
              (setq rr (1+ rr)))))
        (setq c (1+ c)))
      (setq r (1+ r)))
	  
	      ;; === РАТАВАННЕ ВОНКАВЫХ ЛІНІЙ (РАМКІ ТАБЛІЦЫ) ===
    ;; Паколькі матрычны алгарытм шукае толькі ўнутраныя сценкі, рамка фізічна заставалася ў смецці.
    ;; Правяраем лініі, ці ляжаць яны на крайніх восях X і Y.
    
    (foreach l h_lines
      (if (not (member (car l) used_lines_list))
        (if (or (<= (abs (- (cadr (nth 1 l)) (car unique_y))) (* tol 1.5))   ;; Верхняя мяжа
                (<= (abs (- (cadr (nth 1 l)) (last unique_y))) (* tol 1.5))) ;; Ніжняя мяжа
          (setq used_lines_list (cons (car l) used_lines_list))
        )
      )
    )

    (foreach l v_lines
      (if (not (member (car l) used_lines_list))
        (if (or (<= (abs (- (car (nth 1 l)) (car unique_x))) (* tol 1.5))   ;; Левая мяжа
                (<= (abs (- (car (nth 1 l)) (last unique_x))) (* tol 1.5))) ;; Правая мяжа
          (setq used_lines_list (cons (car l) used_lines_list))
        )
      )
    )
    ;; ===============================================

    (foreach l h_lines (if (not (member (car l) used_lines_list)) (ssadd (car l) all_garbage_ss)))
    (foreach l v_lines (if (not (member (car l) used_lines_list)) (ssadd (car l) all_garbage_ss)))
	
	(setq used_lines_ss (ssadd)) 
	(foreach ent used_lines_list (ssadd ent used_lines_ss))
    
    (list (reverse cells_found) used_lines_ss)
  )

  (if *GL_TABLES_LIST*
    (progn
      (foreach cluster *GL_TABLES_LIST*
         (setq bbox (nth 2 cluster))
         (setq result (CT:GetCellsFromCluster cluster tolerance trash))
         (setq cluster_cells (car result)
               used_lines    (cadr result))
         
         ;; --- БЛОК АДЛАДКІ ---
		 (CT:Debug 5 "cells" cluster_cells)
		 (CT:Debug 5 "list" used_lines)
         
         (setq updated_tables (cons (list bbox cluster_cells used_lines) updated_tables))
      )
      
      (setq *GL_TABLES_LIST* (reverse updated_tables))
      
    )
  )
  *GL_TABLES_LIST* 
)
;;; =================================================================
;;; Блок 06: CT:AnalyzeTableStructure
;;; Назначэнне: Аналіз геаметрыі ячэек ва ўсіх кластарах, пабудова сеткі
;;;             і вызначэнне параметраў будучай табліцы AutoCAD.
;;;
;;; Уваход: 
;;;   cluster_res — спіс кластараў выгляду ((BBox Cells Lines) ...)
;;;   tolerance   — лікавы допуск для групавання каардынат.
;;;
;;; Выхад (Спіс пашпартоў табліц):
;;;   Кожны элемент спісу ўтрымлівае:
;;;   1. ColCount (Int)    — Колькасць слупкоў.
;;;   2. RowCount (Int)    — Колькасць радкоў.
;;;   3. ColWidths (List)  — Спіс цэлых шырынь слупкоў (злева направа).
;;;   4. RowHeights (List) — Спіс цэлых вышынь радкоў (зверху ўніз).
;;;   5. MergeList (List)  — Спіс аб'яднанняў ((R1 C1 R2 C2) ...), 
;;;                          дзе індэксы пачынаюцца з 0.
;;; =================================================================
(defun CT:AnalyzeTableStructure (cluster_res tolerance / 
                                 _get_unique_coords _find_index _process_single_cluster
                                 final_tables_data res)

  (princ "\n--------------------------------")
  (princ "\n06: Аналіз структуры табліц")

  (defun _get_unique_coords (coords tol / sorted unique last_val)
    (if (and coords (setq sorted (vl-sort coords '<)))
      (progn
        (setq unique (list (setq last_val (car sorted))))
        (foreach val (cdr sorted)
          (if (> (- val last_val) tol)
            (setq unique (cons (setq last_val val) unique))
          )
        )
        (reverse unique)
      )
    )
  )

  (defun _find_index (val lst tol / i found len)
    (setq i 0 found nil len (length lst))
    (while (and (not found) (< i len))
      (if (<= (abs (- val (nth i lst))) tol)
        (setq found i)
        (setq i (1+ i))
      )
    )
    found
  )

  (defun _process_single_cluster (cells tol / x_coords y_coords x_lines y_lines 
                                              col_widths row_heights col_count row_count 
                                              merge_list is_valid_table c1 c2 r1 r2)
    (if (and cells (> (length cells) 0))
      (progn
        (foreach cell cells
          (setq x_coords (cons (caar cell) (cons (caadr cell) x_coords))
                y_coords (cons (cadar cell) (cons (cadadr cell) y_coords)))
        )

        (setq x_lines (_get_unique_coords x_coords tol))
        (setq y_lines (reverse (_get_unique_coords y_coords tol)))

        (setq col_count (1- (length x_lines))
              row_count (1- (length y_lines)))

        (setq is_valid_table t)
        (foreach cell cells
          (if (not (and (_find_index (min (caar cell) (caadr cell)) x_lines tol)
                        (_find_index (max (caar cell) (caadr cell)) x_lines tol)
                        (_find_index (max (cadar cell) (cadadr cell)) y_lines tol)
                        (_find_index (min (cadar cell) (cadadr cell)) y_lines tol)))
            (setq is_valid_table nil)
          )
        )

        (if (not is_valid_table)
          (progn
            (princ "\n   ! Памылка: Геаметрыя аднаго з кластараў не ўтварае адзіную табліцу.")
            (exit) ;; Вернута жорсткае перарыванне
          )
        )

        (setq col_widths (mapcar '(lambda (a b) (fix (+ 0.5 (abs (- a b))))) 
                                 (cdr x_lines) (reverse (cdr (reverse x_lines)))))
        (setq row_heights (mapcar '(lambda (a b) (fix (+ 0.5 (abs (- a b))))) 
                                  (cdr y_lines) (reverse (cdr (reverse y_lines)))))

        (setq merge_list '())
        (foreach cell cells
          (setq c1 (_find_index (min (caar cell) (caadr cell)) x_lines tol)
                c2 (_find_index (max (caar cell) (caadr cell)) x_lines tol)
                r1 (_find_index (max (cadar cell) (cadadr cell)) y_lines tol)
                r2 (_find_index (min (cadar cell) (cadadr cell)) y_lines tol))
          (if (and c1 c2 r1 r2 (or (> (- c2 c1) 1) (> (- r2 r1) 1)))
            (setq merge_list (cons (list r1 c1 (1- r2) (1- c2)) merge_list))
          )
        )
        (list col_count row_count col_widths row_heights (reverse merge_list))
      )
    )
  )

  (setq final_tables_data '())
  (foreach cluster cluster_res
    (setq res (_process_single_cluster (cadr cluster) tolerance))
    (if res
      (progn
        (setq final_tables_data (cons res final_tables_data))
        (CT:Debug 6 "message" (strcat "Табліца: " (itoa (car res)) "x" (itoa (cadr res))))
      )
    )
  )

  (princ (strcat "\nУсяго табліц сфарміравана: " (itoa (length final_tables_data))))

  (reverse final_tables_data)
)
;;; КАНЕЦ БЛОКА 06
;;; =================================================================

;; --- ПАДФУНКЦЫЯ ПУНКТА 7: Зборка кантэнту ячэйкі ---
;;; =================================================================
;;; ФУНКЦЫЯ ЗБОРКІ ТЭКСТУ ЎНУТРЫ ЯЧЭЙКІ (З КАНТРОЛЕМ ФАРМАТУ)
;;; =================================================================
(defun DTTC_CombineCellContent (cell_items cell_coords tol / 
                                DTTC_FormatText GetW GetColor
                                final_str item ent txt_content
                                all_w all_colors unique_colors avg_w
                                min_x max_x min_y max_y 
                                cell_xmin cell_xmax cell_ymin cell_ymax
                                cell_midx cell_midy group_midx group_midy 
                                cel-al pxsm_prefix w_prefix last_col 
                                s_pos h_pos h_val comp_h last_y curr_y new_h_inner
                                first_semi second_semi tail h_end_idx is_elevated curr_h prefix_part)

  (setq new_h_inner 0.98) 

  (defun DTTC_FormatText (ent / str)
    (setq str (cdr (assoc 1 ent)))
    (while (or (vl-string-search "%%u" str) (vl-string-search "%%U" str))
      (setq str (vl-string-subst "\\L" "%%u" (vl-string-subst "\\L" "%%U" str)))
      (if (not (vl-string-search "\\l" str)) (setq str (strcat str "\\l"))))
    str
  )

  
  (defun GetRotation (ent / ang deg)
  (setq ang (cdr (assoc 50 ent))) ; Угол в радианах
  (if ang
    (progn
      (setq deg (fix (+ (/ (* ang 180.0) PI) 0.5))) ; Перевод в градусы
      (setq deg (rem (abs deg) 180)) ; Приводим к диапазону 0-179
      (if (and (> deg (- 90 2)) (< deg (+ 90 2))) ; Допуск 2 градуса
        90
        nil
      )
    )
    nil
  )
)
  
  (defun GetW (ent / w_pos str)
    (if (= (cdr (assoc 0 ent)) "TEXT")
      (cdr (assoc 41 ent))
      (progn 
        (setq str (cdr (assoc 1 ent)))
        (if (setq w_pos (vl-string-search "\\W" str))
          (atof (substr str (+ w_pos 3) 4))
          1.0))))

  (defun GetColor (ent / c)
    (if (setq c (assoc 62 ent)) (cdr c) 256))

  (if (and cell_items (listp cell_items))
    (progn
      (setq all_w (mapcar '(lambda (x) (GetW (nth 6 x))) cell_items)
            all_colors (mapcar '(lambda (x) (GetColor (nth 6 x))) cell_items)
            avg_w (/ (apply '+ all_w) (float (length all_w)))
            unique_colors nil)
      
      (foreach c all_colors 
        (if (not (member c unique_colors)) (setq unique_colors (cons c unique_colors))))

      (setq min_x 1e10 max_x -1e10 min_y 1e10 max_y -1e10)
      (foreach itm cell_items
        (setq p1 (nth 1 itm) p2 (nth 2 itm))
        (setq min_x (min min_x (car p1) (car p2))
              max_x (max max_x (car p1) (car p2))
              min_y (min min_y (cadr p1) (cadr p2))
              max_y (max max_y (cadr p1) (cadr p2))))

      (setq cell_xmin (apply 'min (mapcar 'car cell_coords))
            cell_xmax (apply 'max (mapcar 'car cell_coords))
            cell_ymin (apply 'min (mapcar 'cadr cell_coords))
            cell_ymax (apply 'max (mapcar 'cadr cell_coords))
            cell_midx (/ (+ cell_xmin cell_xmax) 2.0)
            cell_midy (/ (+ cell_ymin cell_ymax) 2.0)
            group_midx (/ (+ min_x max_x) 2.0)
            group_midy (/ (+ min_y max_y) 2.0))

;; --- НОВЫЙ БЛОК ОПРЕДЕЛЕНИЯ ВЫРАВНИВАНИЯ ---
;; --- ЛОГИКА ЧЕРЕЗ ОТНОШЕНИЯ С ГИБКИМ ДОПУСКОМ ---
      
      (setq dist_L (- group_midx cell_xmin)
            dist_R (- cell_xmax group_midx)
            dist_T (- cell_ymax group_midy)
            dist_B (- group_midy cell_ymin))

      ;; 1. Горизонталь (X)
      ;; Защита от деления на 0 через допуск
      (setq ratio_X (/ dist_L (max (/ tol 10.0) dist_R)))
      
      (setq col_pos 
        (cond 
          ;; Сверяем отклонение от 1.0 напрямую с tol
          ((<= (abs (- 1.0 ratio_X)) (/ tol 4)) "CENTER")
          ((< ratio_X 1.0) "LEFT")
          (t "RIGHT")))

      ;; 2. Вертикаль (Y)
      (setq ratio_Y (/ dist_T (max (/ tol 10.0) dist_B)))
      
      (setq row_pos 
        (cond 
          ;; Сверяем отклонение от 1.0 напрямую с tol
          ((<= (abs (- 1.0 ratio_Y)) (/ tol 4)) "MIDDLE")
          ((< ratio_Y 1.0) "TOP")
          (t "BOTTOM")))

      ;; 3. Итоговый индекс AutoCAD (1-9)
      (setq cel-al 
        (cond 
          ((= row_pos "TOP")
            (cond ((= col_pos "LEFT") 1) ((= col_pos "RIGHT") 3) (t 2)))
          ((= row_pos "BOTTOM")
            (cond ((= col_pos "LEFT") 7) ((= col_pos "RIGHT") 9) (t 8)))
          (t 
            (cond ((= col_pos "LEFT") 4) ((= col_pos "RIGHT") 6) (t 5)))))
      ;; --- КОНЕЦ НОВОГО БЛОКА ---

;; Добавил last_h для правильной работы вашего условия переноса строк
      (setq final_str "" last_col -1 last_y nil is_elevated nil last_h nil) ;;
      
      ;; Используем while вместо foreach, чтобы иметь возможность "съесть" следующий элемент
      (setq remaining_items cell_items)
      (while remaining_items
        (setq itm (car remaining_items))
        (setq remaining_items (cdr remaining_items))
        
        (setq ent      (nth 6 itm)
              txt      (DTTC_FormatText ent)
              curr_col (GetColor ent)
              curr_rot (GetRotation ent) 
              curr_h   (cdr (assoc 40 ent)) ;; Дастаем вышыню тэксту
              curr_y   (/ (+ (cadr (nth 1 itm)) (cadr (nth 2 itm))) 2.0)
              clean_txt (vl-string-trim " " txt)) ;; Текст без пробелов
              
        ;; === ПЕРЕХВАТ ОТОРАВННОГО ИНДЕКСА ИЗ-ЗА СОРТИРОВКИ ===
        (if remaining_items
          (progn
            ;; Заглядываем на следующий текст
            (setq next_itm (car remaining_items)
                  next_ent (nth 6 next_itm)
                  next_txt (DTTC_FormatText next_ent)
                  next_clean (vl-string-trim " " next_txt))
            
            ;; СЦЕНАРИЙ А: Сначала "2" или "3", а за ней текст, заканчивающийся на "м" (напр. "4х2,5 мм")
            (if (and (member clean_txt '("2" "3")) 
                     (wcmatch next_clean "*м"))
              (progn
                (if (= clean_txt "2")
                  (setq txt (strcat next_txt "\\U+00B2"))
                  (setq txt (strcat next_txt "\\U+00B3"))
                )
                ;; Берем координаты и свойства от базового текста (он лежит на базовой линии)
                (setq curr_col (GetColor next_ent)
                      curr_rot (GetRotation next_ent)
                      curr_h   (cdr (assoc 40 next_ent))
                      curr_y   (/ (+ (cadr (nth 1 next_itm)) (cadr (nth 2 next_itm))) 2.0))
                ;; Удаляем "мм" из очереди, чтобы не дублировать
                (setq remaining_items (cdr remaining_items))
              )
              
              ;; СЦЕНАРИЙ Б: Сначала идет текст, заканчивающийся на "м", а за ней "2" или "3"
              (if (and (wcmatch clean_txt "*м")
                       (member next_clean '("2" "3")))
                (progn
                  (if (= next_clean "2")
                    (setq txt (strcat txt "\\U+00B2"))
                    (setq txt (strcat txt "\\U+00B3"))
                  )
                  ;; Свойства и так правильные, просто удаляем "2" из очереди
                  (setq remaining_items (cdr remaining_items))
                )
              )
            )
          )
        )
        ;; === КОНЕЦ ПЕРЕХВАТА ИНДЕКСОВ ===

        ;; ПРАВЕРКА ВЫШЫНІ (дастаткова аднаго элемента)
        (if (> curr_h (+ calculated_height (/ tol 2.0)))
            (setq is_elevated T))
        
        ;; ВАШ КОД ФОРМАТИРОВАНИЯ \W И \S (НЕ ТРОНУТ)
        (while (setq w_start (vl-string-search "\\W" txt))
          (if (setq w_end (vl-string-search ";" txt w_start))
              (setq txt (strcat (substr txt 1 w_start) (substr txt (+ w_end 2))))
              (setq w_start nil)))

        (if (setq s_pos (vl-string-search "\\S" txt))
          (progn
            (setq h_pos (vl-string-search "\\H" txt))
            (if (and h_pos (< h_pos s_pos))
              (progn
                (setq h_end_idx (vl-string-search ";" txt h_pos))
                (setq txt (strcat (substr txt 1 h_pos) "\\H" (rtos new_h_inner 2 2) "x" (substr txt (1+ h_end_idx))))
                (setq s_pos (vl-string-search "\\S" txt))
                (setq first_semi (vl-string-search ";" txt s_pos))
                (setq second_semi (if first_semi (vl-string-search ";" txt (1+ first_semi)) nil))
                (if second_semi
                  (progn
                    (setq tail (vl-string-trim " }\\P" (substr txt (+ second_semi 2))))
                    (if (/= tail "")
                      (setq txt (strcat txt "{\\H" (rtos (/ 1.0 new_h_inner) 2 5) "x;}")))))))))

        (if (> (length unique_colors) 1)
          (if (/= curr_col last_col)
            (setq txt (strcat "\\C" (itoa curr_col) ";" txt)
                  last_col curr_col)))
        
        ;; ВАШ ОРИГИНАЛЬНЫЙ СБОРЩИК final_str С УСЛОВИЕМ \P
        (cond 
          ((= final_str "") (setq final_str txt))
          ((and last_y (> (abs (- last_y curr_y)) (- (min curr_h (if last_h last_h curr_h)) tol)))
            (setq final_str (strcat final_str "\\P" txt)))
          (t (setq final_str (strcat final_str " " txt))))
        
        (setq last_y curr_y)
        (setq last_h curr_h) ;; Сохраняем last_h для следующего прохода (важно для условия выше!)
      )

      (setq pxsm_prefix (if (> (length cell_items) 1) "\\pxsm0.8;" ""))
      (setq w_prefix (if (> (abs (- avg_w 1.0)) (/ tol 10.0)) (strcat "\\W" (rtos avg_w 2 2) ";") ""))

      (if (or (/= pxsm_prefix "") (/= w_prefix ""))
          (setq final_str (strcat pxsm_prefix "{" w_prefix final_str "}")))
		  
	(if (setq s_pos (vl-string-search "\\S" final_str)) ;; Калі ў ратку наогул ёсць дроб
          (progn
            ;; Бярэм фрагмент тэксту ад пачатку да сімвала \S
            (setq prefix_part (substr final_str 1 s_pos))
            ;; Калі ў гэтым фрагменце НЕ сустракаецца "\A"
            (if (not (vl-string-search "\\A" prefix_part))
                (setq final_str (strcat "\\A1;" final_str)) ;; Дадаем інструкцыю выраўноўвання
            )
          )
      )  

      ;; Вынік цяпер з 4 параметрамі
      (list 
        final_str 
        cel-al 
        (if (and (= (length unique_colors) 1) (/= (car unique_colors) 256) (/= (car unique_colors) 0)) 
            (car unique_colors) 
            nil)
        is_elevated
		curr_rot
      )
    ) (list "" 5 nil nil nil)) ;; Пустая клетка таксама з 4 параметрамі
	)

;;; =================================================================
;;; Функцыя: CT:ProcessMTextSelection
;;; Прызначана для расчлянення набору MText і наступнага аналізу 
;;; атрыманых прымітываў.
;;;
;;; Уваходныя параметры:
;;;   ss_mtext    - Selection Set з аб'ектамі MText для апрацоўкі.
;;;   final_trash - Selection Set для збору непрыдатных аб'ектаў.
;;;   tolerance   - Дапушчальная хібнасць для геаметрыі.
;;;
;;; Выхадныя даныя:
;;;   Вяртае аб'яднаны спіс тэкставых аб'ектаў (апрацаваных функцыяй 
;;;   defineTextAndLine або проста сабраных пасля расчлянення).
;;; =================================================================
(defun DTTC::ProcessMTextSelection (ss_mtext final_trash tolerance / 
                                 i ent_name temp_ss j sub_ent sub_data sub_type
                                 res_lines res_texts res_mtexts proc_results final_list
                                 p_start p_end p_mid p_min p_max p_cen h clr results)

  (setq i 0)
  (repeat (sslength ss_mtext)
    (setq ent_name (ssname ss_mtext i))
    
    ;; 1) Расчляненне праз vl-cmdf
    (vl-cmdf "_explode" ent_name)
    (setq temp_ss (ssget "_P"))
    
    (if temp_ss
      (progn
        (setq j 0)
        (repeat (sslength temp_ss)
          (setq sub_ent (ssname temp_ss j))
          (setq sub_data (entget sub_ent))
          (setq sub_type (cdr (assoc 0 sub_data)))
          (setq clr (if (assoc 62 sub_data) (cdr (assoc 62 sub_data)) 256))
          
          (cond
            ;; 2) Аналіз LINE (фармат як у PripareObj)
            ((= sub_type "LINE")
             (setq p_start (cdr (assoc 10 sub_data))
                   p_end (cdr (assoc 11 sub_data)))
             (if (or (< (car p_start) (car p_end))
                     (and (= (car p_start) (car p_end)) (< (cadr p_start) (cadr p_end))))
                 (setq p_start p_start p_end p_end)
                 (setq p_mid p_start p_start p_end p_end p_mid))
             (setq p_mid (list (/ (+ (car p_start) (car p_end)) 2.0)
                               (/ (+ (cadr p_start) (cadr p_end)) 2.0) 0.0))
             (setq res_lines (cons (list sub_ent p_start p_end p_mid clr sub_data) res_lines)))

            ;; 2) Аналіз TEXT і MTEXT (фармат як у PripareObj)
            ((or (= sub_type "TEXT") (= sub_type "MTEXT"))
             (setq results (PO:ProcessTextEntity sub_ent sub_data sub_type))
             (if results
               (progn
                 (setq p_min (nth 0 results) p_max (nth 1 results) 
                       p_cen (nth 2 results) h (nth 3 results))
                 (setq results (list sub_ent p_min p_max p_cen h clr sub_data))
                 (if (= sub_type "TEXT")
                     (setq res_texts (cons results res_texts))
                     (setq res_mtexts (cons results res_mtexts))))))
            
            ;; Астатняе адразу ў мусар
            (t (ssadd sub_ent final_trash))
          )
          (setq j (1+ j))
        )
      )
    )
    (setq i (1+ i))
  )

  ;; 3) Апрацоўка вынікаў
  (if res_lines
      (progn
        ;; Выклікаем асноўную функцыю апрацоўкі
        (setq proc_results (CT:defineTextAndLine res_lines res_texts res_mtexts tolerance nil))
        ;; Аналізуем тое, што вярнуў defineTextAndLine (спіс з 3-х падспісаў)
        ;; Нам патрэбны тэксты (2-і элемент) і мтэксты (3-і элемент)
        (setq final_list (append (nth 1 proc_results) (nth 2 proc_results)))
      )
      ;; 4) Калі ліній няма, проста аб'ядноўваем усё ў адзін спіс
      (setq final_list (append res_texts res_mtexts))
  )

  final_list
)

;; Канец функцыі CT:ProcessMTextSelection

;;; =================================================================
;;; БЛОК 07: CT:DistributeTextToCells
;;; =================================================================
;;; Назначэнне: Размеркаванне тэксту па ячэйках з захаваннем выкарыстаных
;;;             ename у Selection Set для кожнага кластара.
;;;
;;; Уваходныя параметры:
;;;   text_list    - Спіс TEXT [cite: 3]
;;;   mtext_list   - Спіс MTEXT [cite: 3]
;;;   clusters_data- Зыходныя кластары табліц (Блок 05) [cite: 1]
;;;   tolerance    - Геаметрычны допуск [cite: 1]
;;;   trash        - Набор для аб'ектаў, якія не ўлезлі [cite: 1]
;;; Выхад:
;;;   *GL_TABLES_LIST* - Спіс апрацаваных табліц (кластараў). Кожны элемент спіса:
;;;   (
;;;     (Xmin Xmax Ymin Ymax)         ; 1. cluster_box: межы ўсёй табліцы
;;;     (                             ; 2. current_table_cells: спіс ячэек
;;;        (
;;;          ((x1 y1) (x2 y2))        ;    - cell_coords: каардынаты межаў ячэйкі
;;;          ("Тэкст\\PРадок" 10)     ;    - (змест_радок код_выраўноўвання)
;;;        )
;;;        ... (астатнія ячэйкі)
;;;     )
;;;     used_lines                    ; 3. Selection Set усіх  выкарыстанных ліній табліцы кластера
;;;     used_ss                       ; 4. Selection Set тэкстаў, прывязаных да кластара
;;;   )
;;; =================================================================
(defun CT:DistributeTextToCells (text_list mtext_list clusters_data tolerance trash / 
                                 all_text_pool updated_tables table_item 
                                 cluster_box cluster_cells used_lines used_ss 
                                 not_used_ss pool cell_res final_list 
                                 current_table_cells cell_entry txts temp_ss
                                 DTTC:GetClusterPool DTTC:ProcessDistribution)

  (princ "\n--------------------------------")
  (princ "\n07: Прыналежнасць тэксту да ячэйке")
  
(defun DTTC:SortPool (p tol)
  (vl-sort p 
    '(lambda (a b / p1 p2 h1 h2 h_limit dy)
       (setq p1 (nth 3 a) 
             p2 (nth 3 b)
             h1 (nth 4 a) ;; Высота текста A
             h2 (nth 4 b)) ;; Высота текста B
       
       ;; ПОРОГ ВЫРАВНИВАНИЯ: используем 60% от максимальной высоты двух объектов.
       ;; Это надежно группирует надстрочные/подстрочные индексы в одну строку с базой.
       (setq h_limit (* (max h1 h2) 0.6))
       
       (if (and (listp p1) (listp p2))
         (progn
           (setq dy (abs (- (cadr p1) (cadr p2))))
           (if (<= dy h_limit)
               (< (car p1) (car p2))    ;; Одна строка -> слева направо
               (> (cadr p1) (cadr p2))  ;; Разные строки -> сверху вниз
           )
         )
         nil))))
		 
  ;; --- 1. УНУТРАНАЯ ФУНКЦЫЯ: Фільтрацыя (Чыстая логіка без пабочных эфектаў) ---
(defun DTTC:GetClusterPool (box tol / cb_min cb_max c_pool new_pool p_min p_max ent_line)
    ;; 1. ПРАВІЛЬНЫ РАЗБОР: box = (Xmin Xmax Ymin Ymax) [cite: 3]
    (if (and (= (length box) 4) (numberp (car box)))
        (setq cb_min (list (nth 0 box) (nth 2 box)) ;; (Xmin Ymin)
              cb_max (list (nth 1 box) (nth 3 box))) ;; (Xmax Ymax)
        (setq cb_min (car box) 
              cb_max (cadr box))
    )


    (setq c_pool '() new_pool '())
    
;; 3. ФІЛЬТРАЦЫЯ ПУЛА
    (foreach txt all_text_pool
      (if (and (listp txt) (listp (nth 1 txt)) (listp (nth 2 txt)))
        (progn
          ;; Берем центр текста (индекс 3) вместо габаритов
          (setq p_cen (nth 3 txt)) 
          (if (and (>= (car p_cen) (- (car cb_min) tol))
                   (<= (car p_cen) (+ (car cb_max) tol))
                   (>= (cadr p_cen) (- (cadr cb_min) tol))
                   (<= (cadr p_cen) (+ (cadr cb_max) tol)))
            (setq c_pool (cons txt c_pool))
            (setq new_pool (cons txt new_pool))
          )
        )
      )
    )
    
    ;; 4. АБНАЎЛЕННЕ ГЛАБАЛЬНАГА ПУЛА [cite: 6]
    (setq all_text_pool (reverse new_pool))
    
    ;; 5. САРТАВАННЕ: Выкарыстоўваем вашу падфункцыю, як і дамаўляліся
    (if c_pool (DTTC:SortPool c_pool tol) nil)
)
;; --- 2. УНУТРАНАЯ ФУНКЦЫЯ: Універсальнае размеркаванне ---
  (defun DTTC:ProcessDistribution (pool cells tol u_ss nu_ss / 
                                   txt_item t_min t_max tx_min ty_min tx_max ty_max
                                   found final_c head tail cell_coords bot_y 
                                   cell_entry c_coords c_p1 c_p2 cx_min cx_max cy_min cy_max)
    (setq final_c '())
    
    ;; Падрыхтоўка: калі прыйшоў проста спіс каардынат, ператвараем у фармат entry ((p1 p2) ())
    (setq cells (mapcar '(lambda (x) 
                           (if (listp (car x)) x (list x '()))) 
                        cells))

    (while (and pool (setq txt_item (car pool)))
      (setq t_min (nth 1 txt_item) t_max (nth 2 txt_item)
            tx_min (car t_min) ty_min (cadr t_min)
            tx_max (car t_max) ty_max (cadr t_max)
            found nil)

      ;; 1. Адсякаем ячэйкі, якія засталіся вышэй за тэкст
      (while (and cells 
                  (setq cell_entry (car cells))
                  (setq cell_coords (car cell_entry))
                  (setq bot_y (min (cadr (car cell_coords)) (cadr (cadr cell_coords))))
                  (> bot_y (+ ty_max tol)))
        (setq final_c (cons (car cells) final_c))
        (setq cells (cdr cells))
      )

      ;; 2. Пошук падыходзячай ячэйкі (head/tail)
      (setq head '() tail cells)
      (while (and tail (not found))
        (setq cell_entry (car tail)
              c_coords (car cell_entry)
              c_p1 (car c_coords) c_p2 (cadr c_coords)
              cx_min (- (min (car c_p1) (car c_p2)) tol)
              cx_max (+ (max (car c_p1) (car c_p2)) tol)
              cy_min (- (min (cadr c_p1) (cadr c_p2)) tol)
              cy_max (+ (max (cadr c_p1) (cadr c_p2)) tol))

        (if (and (>= tx_min cx_min) (<= tx_max cx_max)
                 (>= ty_min cy_min) (<= ty_max cy_max))
            (progn
              (setq found t)
              (ssadd (car txt_item) u_ss)
              ;; Дадаем тэкст да існуючага спіса ў ячэйцы
              (setq cell_entry (list c_coords (cons txt_item (cadr cell_entry))))
              (setq cells (append (reverse head) (cons cell_entry (cdr tail))))
            )
            (setq head (cons cell_entry head) tail (cdr tail))
        )
      )
      
      (if (not found) (ssadd (car txt_item) nu_ss))
      (setq pool (cdr pool))
    )
    (append (reverse final_c) cells)
  )
  ;; --- 3. АСНОЎНЫ ЦЫКЛ ---
  (setq all_text_pool (append text_list mtext_list)
        updated_tables '())

  (foreach table_item clusters_data
    (setq cluster_box   (car table_item)
          cluster_cells (nth 1 table_item)
          used_lines    (nth 2 table_item)
          used_ss       (ssadd)
          not_used_ss   (ssadd)
          ;; Выклікаем падрыхтаваную функцыю
          pool          (DTTC:GetClusterPool cluster_box tolerance)
          cell_res      (mapcar '(lambda (x) (list x '())) cluster_cells)
          final_list    (DTTC:ProcessDistribution pool cell_res tolerance used_ss not_used_ss))
		  
(setq pool (DTTC::ProcessMTextSelection not_used_ss trash tolerance))
;; Проста сартуем атрыманы спіс перад размеркаваннем
(setq pool (DTTC:SortPool pool tolerance)) 

(setq not_used_ss (ssadd)
      final_list (DTTC:ProcessDistribution pool final_list tolerance used_ss not_used_ss))
		  

(setq current_table_cells 
  (mapcar '(lambda (entry / coords content_data)
             (setq coords (car entry))
             ;; Передаем координаты ячейки (coords) в функцию сборки
             (setq content_data (DTTC_CombineCellContent (reverse (cadr entry)) coords tolerance))
             ;; Результат: ((p1 p2) "Текст" КодВыравнивания)
             (cons coords content_data)
          )
          final_list))

    (CT:Debug 7 "ss" used_ss)
    (CT:Debug 7 "ss" not_used_ss)
	
;; --- ПЕРАКідВАЕМ У СМЕТНІЦУ (Trash) ---
(if not_used_ss (repeat (setq i (sslength not_used_ss)) (ssadd (ssname not_used_ss (setq i (1- i))) trash)))

    (setq updated_tables 
          (cons (list cluster_box current_table_cells used_lines used_ss) 
                updated_tables))
  )

  ;; Усё, што наогул не патрапіла ў кластары
  (if all_text_pool 
    (foreach txt all_text_pool 
      (if (and (listp txt) (car txt)) (ssadd (car txt) trash))
    )
  )
  
  (setq *GL_TABLES_LIST* (reverse updated_tables))
)
;;; =================================================================

;;; ==============================================================================
;;; БЛОК 08: ГАЛОЎНАЯ ФУНКЦЫЯ СТВАРЭННЯ ТАБЛІЦ
;;; ==============================================================================
;;; Прызначана для генерацыі аб'ектаў "Table" на аснове кластараў і пашпартоў.
;;; Уваход:
;;;    clusters_list  - Спіс (*GL_TABLES_LIST*): (box cells_data lines used_ss)
;;;    passports_list - Спіс пашпартоў: (cols rows widths heights merges)
;;;    tol            - Геаметрычны допуск (Double)
;;; Выхад:
;;;    Выдаляе зыходныя лініі/тэксты і стварае новыя табліцы.
;;; ==============================================================================
(defun CT:BuildAllTables (clusters_list passports_list tol text_h text_s / 
                          acadObj doc space i cluster passport count tbl_vla
                          BAT:CreateEmptyTable BAT:FillTableData BAT:CorrectIndicesForMerges used_objs obj)
  (princ "\n--------------------------------")
  (princ "\n08: Блок стварэння табліц...")
  
  ;; --- ПАДФУНКЦЫЯ: КАРЭКЦЫЯ ІНДЭКСАЎ ЯЧЭЙКІ ПРЫ АБ'ЯДНАННІ ---
  (defun BAT:CorrectIndicesForMerges (r c merges / m res)
    (setq res (list r c))
    (if merges
      (foreach m merges
        ;; Структура m: (R1 C1 R2 C2)
        (if (and (>= r (nth 0 m)) (<= r (nth 2 m))
                 (>= c (nth 1 m)) (<= c (nth 3 m)))
          (setq res (list (nth 0 m) (nth 1 m))) ; Возвращаем индексы первой ячейки объединения
        )
      )
    )
    res
  )

  ;; --- ПАДФУНКЦЫЯ 1: КАНСТРУКТАР КАРКАСА ---
  (defun BAT:CreateEmptyTable (space cluster passport text_h text_s tol / 
                               box cols rows col_widths row_heights merges 
                               ins_pt tbl r_idx c_idx margin)
    (setq box         (nth 0 cluster)
          cols        (nth 0 passport)
          rows        (nth 1 passport)
          col_widths  (nth 2 passport)
          row_heights (nth 3 passport)
          merges      (nth 4 passport)
          ins_pt      (vlax-3d-point (list (car box) (nth 3 box) 0.0))
          margin      tol)

    (setq tbl (vla-AddTable space ins_pt (+ rows 2) cols 1.0 1.0))
    (vla-DeleteRows tbl 0 1)
    (vla-DeleteRows tbl 0 1)

    (vl-catch-all-apply 'vla-SetTextStyle (list tbl 7 text_s))
    (vl-catch-all-apply 'vla-SetTextHeight (list tbl 7 text_h))
    
    (vla-put-HorzCellMargin tbl margin)
    (vla-put-VertCellMargin tbl margin)

    (setq c_idx 0)
    (foreach w col_widths (vla-SetColumnWidth tbl c_idx (float w)) (setq c_idx (1+ c_idx)))
    (setq r_idx 0)
    (foreach h row_heights (vla-SetRowHeight tbl r_idx (float h)) (setq r_idx (1+ r_idx)))

    (if (and merges (listp merges))
      (foreach m merges
        (vl-catch-all-apply 'vla-MergeCells (list tbl (nth 0 m) (nth 2 m) (nth 1 m) (nth 3 m))))
    )
    tbl
  )

  ;; --- ПАДФУНКЦЫЯ 2: ЗАПАЎНЯЛЬНІК ТЭКСТУ ---
(defun BAT:FillTableData (tbl cluster passport tol / 
                            cells_data col_widths row_heights Xmin Ymax
                            cell text_str cell_aln cell_col coords p1 p2 cx cy 
                            c_idx r_idx curr_x curr_y found_col found_row
                            n_cols n_rows any_filled final_h)
    
    (setq cells_data  (nth 1 cluster)
          col_widths  (nth 2 passport)
          row_heights (nth 3 passport)
          Xmin        (car (nth 0 cluster))
          Ymax        (nth 3 (nth 0 cluster))
          any_filled  nil
          n_cols      (length col_widths)
          n_rows      (length row_heights))

    (if (and cells_data (listp cells_data))
      (progn
        (vla-put-RegenerateTableSuppressed tbl :vlax-true)

        (foreach cell cells_data
          (setq text_str (cadr cell))
          
          (if (and text_str (/= text_str ""))
            (progn
              (setq coords   (car cell)
                    ;; 1. Выравнивание (обязательно индекс 2)
                    cell_aln (if (>= (length cell) 3) (nth 2 cell) 5)
                    ;; 2. Цвет (индекс 3, если есть)
                    cell_col (nth 3 cell) ;цвет
                    is_elevated (nth 4 cell) ;; Чацвёрты параметр повышенный лі шріфт
					cell_rot(nth 5 cell) ;; Поворот
                    p1       (car coords)
                    p2       (cadr coords)
                    cx       (/ (+ (car p1) (car p2)) 2.0)
                    cy       (/ (+ (cadr p1) (cadr p2)) 2.0))

              ;; Поиск колонки
              (setq curr_x Xmin c_idx 0 found_col nil)
              (while (and (< c_idx n_cols) (not found_col))
                (setq w (float (nth c_idx col_widths)))
                (if (and (>= cx (- curr_x tol)) (<= cx (+ curr_x w tol))) 
                    (setq found_col c_idx))
                (setq curr_x (+ curr_x w) c_idx (1+ c_idx)))

              ;; Поиск строки
              (setq curr_y Ymax r_idx 0 found_row nil)
              (while (and (< r_idx n_rows) (not found_row))
                (setq h (float (nth r_idx row_heights)))
                (if (and (<= cy (+ curr_y tol)) (>= cy (- curr_y h tol))) 
                    (setq found_row r_idx))
                (setq curr_y (- curr_y h) r_idx (1+ r_idx)))

;; 3. Заполнение ячейки
              (if (and found_row found_col)
                (progn
                  ;; Коректировка индексов под объединенные ячейки
                  (setq corrected_idx (BAT:CorrectIndicesForMerges found_row found_col (nth 4 passport)))
                  (setq found_row (car corrected_idx) found_col (cadr corrected_idx))

                  (vla-SetText tbl found_row found_col text_str)
                  (vla-SetCellAlignment tbl found_row found_col cell_aln)
				  
				  ;; Поворот, если данные содержат пометку 90 cell_rot
                  (if (= cell_rot 90)
					(vla-SetTextRotation tbl found_row found_col 1)
                  )
                  ;; 2. Вызначаем вышыню: калі is_elevated = T, бярэм павялічаную
      ;; Абарона ад nil: калі зменныя не вызначаны, выкарыстоўваем 2.5 (або сваё значэнне)
(setq final_h (cond 
               ((and is_elevated elevated_scale) elevated_scale)
               (calculated_height calculated_height)
               (t 2.5))) ;; Значэнне "на крайні выпадак"
      (vla-SetCellTextHeight tbl found_row found_col final_h)
                  ;; Установка цвета, если он передан (не nil)
(if (and cell_col (numberp cell_col))
  (progn
  ;; 1. Получаем существующий объект цвета из конкретной ячейки
(setq objColor (vla-GetCellContentColor tbl found_row found_col))

;; 2. Меняем в этом объекте индекс цвета (ACI) на наш cell_col (например, 7)
;; Здесь мы работаем со свойством самого объекта Color
(vla-put-ColorIndex objColor cell_col)

;; 3. Записываем измененный объект цвета обратно в ячейку
(vla-SetCellContentColor tbl found_row found_col objColor)

;; 4. Обязательно освобождаем память от временного объекта
(vlax-release-object objColor)
  )
)
                  
                  (setq any_filled t)
                )
              )
            )
          )
        )

        (vla-put-RegenerateTableSuppressed tbl :vlax-false)
        any_filled
      )
      nil
    )
  )
  ;; --- ГЛАЎНЫ ЦЫКЛ ---
  (setq acadObj (vlax-get-acad-object)
        doc     (vla-get-ActiveDocument acadObj)
        space   (vla-get-Block (vla-get-ActiveLayout doc))
        count   0) 
		
  (if (and (listp clusters_list) (listp passports_list))
    (progn
      (setq i 0)
      (repeat (length clusters_list)
        (setq cluster  (nth i clusters_list))
        (setq passport (nth i passports_list))
        (setq tbl_vla  (BAT:CreateEmptyTable space cluster passport text_h text_s tol))
        
(if tbl_vla
          (progn
            ;; Ловім памылку падчас запаўнення праз асобны выклік
            (setq fill_res (vl-catch-all-apply 'BAT:FillTableData (list tbl_vla cluster passport tol)))

            (if (vl-catch-all-error-p fill_res)
              (progn 
                ;; 1. Ачыстка: выдаляем пустую табліцу
                (vla-Delete tbl_vla)
                ;; 2. Пераход у асноўны апрацоўшчык памылак (пракід далей)
                (exit (strcat "\nКрытычная памылка ў Блоку 08: " (vl-catch-all-error-message fill_res)))
              )
              (if fill_res
                (progn
                  (setq count (1+ count))
                  ;; ВЫДАЛЯЕМ арыгінальныя лініі/тэкст
                  (if (and (setq used_objs (nth 2 cluster)) (> (sslength used_objs) 0))
                      (vl-cmdf "_.erase" used_objs ""))
                  (if (and (setq used_objs (nth 3 cluster)) (> (sslength used_objs) 0))
                      (vl-cmdf "_.erase" used_objs ""))
                )
                ;; Калі функцыя вярнула nil без памылкі
                (vla-Delete tbl_vla) 
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  ;; ВЫЛУЧАЕМ мусар (trash), калі ён быў перададзены ў праграму (напрыклад, як асобная зменная)
  ;; Калі ў цябе мусар прыходзіць звонку, тут павінна быць логіка яго дадання ў ss_trash
  (if (> (sslength final_trash) 0) (sssetfirst nil final_trash))

  (princ (strcat "\nТабліц створана: " (itoa count)))
  (princ)
)
;;; --- КАНЕЦ ФУНКЦЫІ CT:BuildAllTables ---


;; =================================================================
;; ==================Галоуная функция=======================
;; =================================================================
(defun c:CreateTableFromLines (/ 
		*error* old_osmode old_cmdecho old_cmddia old_filletrad
       sel_set line_list final_trash
		text_list mtext_list current_height current_style scale tolerance calculated_height elevated_scale temp
       first_text_height_found lengthobj 
       result_lists
       line_results horizontal_lines vertical_lines other_lines ;магчыма знішчыц
       cluster_res count_tables i temp_count *CT_DEBUG* Structure)
	   
;; --- Внутренняя функция обработки ошибок ---
  (defun *error* (msg)
    ;; Проверяем, не нажал ли пользователь Esc или не ввел ли Exit
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nПамылка: " msg))
      (princ "\nДзеянне скасаванна (націснута Esc).")
    )
    (if old_osmode (CT:RestoreSettings)) 
    (princ "\nНастройки восстановлены.")
    (princ)
  )
  
;;; =================================================================
;;; Глобальная зменная кіравання адладкай (спіс па блоках 01-06)
(setq *CT_DEBUG* '(nil nil nil nil nil nil nil 1)) 
;;; nil - адладка цалкам адключана для блока
;;; Т - кансолні вывад адладкі для усіх блокаў
;;;  0  - вывад толькі тэкставых паведамленняў у кансоль
;;;  1  - візуальная адладка: падсветка аб'ектаў (redraw) 
;;;       або адмалёўка ячэек (Polyline) + прыпынак праграмы
;;;  2  - інтэрактыўная адладка: выдзяленне аб'ектаў "з ручкамі" 
;;;       (sssetfirst) + прыпынак праграмы
;;; Прыклад: '(1 nil 0) -> Блок 01 (візуальна), Блок 02 (выкл), Блок 03 (кансоль)
;;; =================================================================
	   
  ;Захаванне налад 
 (CT:SaveSettings)
  
 (reset-timers);; Скід усіх таймераў на пачатку
 (start-timer "Каманда CreateTableFromLines")

  ;; Атрыманне ЦЯПЕРКАШНЯГА выбару
  (setq sel_set (ssget "_I")) ; "_I" азначае "Implied" - цяперашні выбар
  
    ;; Ініцыялізацыя спісаў
  (setq line_list '())  ; Элемент: (ename pt_start_ordered pt_end_ordered)
  (setq text_list '())  ; Элемент: (ename pt_min pt_max pt_center)
  (setq mtext_list '()) ; Элемент: (ename pt_min pt_max pt_center)
  (setq final_trash (ssadd)); ствараем смецце
  ;; Ініцыялізацыя пераменных для глабальнага маштабу і допуску
  (setq first_text_height_found nil)
  
  ;; Праверка, ці ёсць вылучаныя аб'екты
  (if (null sel_set)
    (progn
      (alert "Няма выбару. \nВылучыце аб'екты перад запускам каманды.")
      (exit) ; Перарываем выкананне функцыі
    )
	(progn
	 (start-timer "01 Падрыхтоўка аб'ектаў")
	 (setq lengthobj (sslength sel_set))
	 (CT:PripareObj sel_set)
 	 (end-timer "01 Падрыхтоўка аб'ектаў")
	)
  )

    
  ;; ========== 02 АПРАЦОЎКА РЫСАЎ І ТЭКСТУ==========
  (start-timer "02 Апрацоўка рыс і тэксту")
  (setq temp (CT:defineTextAndLine line_list text_list mtext_list tolerance T))
  (setq line_list (nth 0 temp)   ; < только неиспользованные линии
      text_list  (nth 1 temp)   ; < только обработанные тексты
      mtext_list (nth 2 temp))  ; < исходные MTEXT + новые дроби
  (end-timer "02 Апрацоўка рыс і тэксту")
  ;; ========== АПРАЦОЎКА РЫСАЎ І ТЭКСТУ==========
  
    ;; ========== 03 ЗЛУЧЭННЕ РЫСАЎ ==========
  (if (and line_list (>= (length line_list) 1))
  (progn
  (start-timer "03 Апрацоўка застаўшыхся рысаў")
  (setq rezult (CT:Mergelines line_list final_trash tolerance))
  (setq horizontal_lines (car rezult) vertical_lines (cadr rezult))
  (end-timer "03 Апрацоўка застаўшыхся рысаў")
  ));; ========== ЗЛУЧЭННЕ РЫСАЎ ==========
  
      ;; ========== Кластарызацыя ==========
  (if (>= (+ (length horizontal_lines) (length vertical_lines)) 1)
  (progn
  ;; Кластарызацыя
  (start-timer "04 Кластарызацыя рыс у табліцы")
  (setq cluster_res (CT:ClusterLinesIntoTables horizontal_lines vertical_lines final_trash tolerance))
  (end-timer "04 Кластарызацыя рыс у табліцы")
  ));; ========== Кластарызацыя ==========
  ; 05 вызначэнне ячэек табліцы
  (start-timer "05 Вызначэнне ячэек табліцы")
  (setq cluster_res (CT:ProcessClustersToCells cluster_res tolerance final_trash))

  (end-timer "05 Вызначэнне ячэек табліцы")
  ; 05 Канец вызначэнне ячэек табліцы
  ; 06 вызначэнне структуры і злучэнне
  (start-timer "06: Аналіз структуры табліцы")
  (setq Structure (CT:AnalyzeTableStructure cluster_res tolerance))
  (end-timer "06: Аналіз структуры табліцы")
  
  ; 07 блок прыналежнасці тэксту да ячэйке
  (start-timer "07: Прыналежнасць тэксту да ячэйке")
  (setq cluster_data (CT:DistributeTextToCells text_list mtext_list cluster_res tolerance final_trash))
  (end-timer "07: Прыналежнасць тэксту да ячэйке")
  ; Канец блока 07
  
   ; 08 блок прыналежнасці тэксту да ячэйке
  (start-timer "08: Блок стварэння табліц")
  (CT:BuildAllTables cluster_data Structure tolerance calculated_height current_style)
  (end-timer "08: Блок стварэння табліц")
  ; Канец блока 07
  
  
  
  (end-timer "Каманда CreateTableFromLines")
  ;; Вывад часу выканання
  (print-timers)
  (reset-timers)
   ;; Аднаўленне settings
  (CT:RestoreSettings)
  (princ)
)
;; =================================================================
;; Канец каманда для стварэння табліцы з прымітываў
;; =================================================================


;;; =================================================================
;;; CT:Debug
;;; Назначэнне: Менеджэр адладкі. Выводзіць інфармацыю ў кансоль, 
;;;             падсвечвае аб'екты альбо малюе часовыя контуры.
;;; Уваход:
;;;   block_num - нумар блока (Int)
;;;   msg_type  - тып паведамлення ("message", "cells", "list", "ss")
;;;   data      - дадзеныя (String, List каардынат, List ename ці Pickset)
;;; Глобальная зменная *CT_DEBUG*:
;;;   nil       - адладка адключана
;;;   T         - толькі кансоль для ўсіх блокаў
;;;   (0 1 2..) - спіс рэжымаў для кожнага блока:
;;;               0 - толькі кансоль
;;;               1 - падсветка (redraw) / адмалёўка (LWPolyline) + паўза
;;;               2 - выдзяленне (sssetfirst) + паўза
;;; =================================================================
;;; =================================================================
;;; CT:Debug
;;; Назначэнне: Менеджэр адладкі (Вярнуў getstring для стабільнасці)
;;; =================================================================
(defun CT:Debug (block_num msg_type data / mode color ss_tmp i p_min p_max p1 p2 p3 p4 tmp_ents ent)
  ;; 1. Вызначэнне рэжыму
  (setq mode (cond 
               ((= (type *CT_DEBUG*) 'LIST) (nth (1- block_num) *CT_DEBUG*))
               ((= *CT_DEBUG* T) 0)
               (t nil)))

  ;; Калі адладка для блока не патрэбна - выхад
  (if (not mode) (setq mode -1)) 

  ;; 2. Колер на аснове нумара блока
  (setq color (1+ (rem block_num 6)))

  (cond
    ;; --- ТЭКСТАВАЕ ПАВЕДАМЛЕННЕ ---
    ((and (= msg_type "message") (>= mode 0))
     (princ (strcat "\n[Debug]: " data)))

    ;; --- ЯЧЭЙКІ (Малюем часовыя LWPOLYLINE) ---
    ((= msg_type "cells")
     (cond 
       ((member mode '(1 2))
        (setq ss_tmp (ssadd))
        (foreach cell data
          (setq p_min (car cell) p_max (cadr cell))
          (setq p1 p_min
                p2 (list (car p_max) (cadr p_min))
                p3 p_max
                p4 (list (car p_min) (cadr p_max)))
          
          (setq ent (entmakex (list 
                      '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                      (cons 62 color) (cons 90 4) (cons 70 1)
                      (cons 10 p1) (cons 10 p2) (cons 10 p3) (cons 10 p4)
                    )))
          (if ent (ssadd ent ss_tmp))
        )
        (if (= mode 2) (sssetfirst nil ss_tmp))
        ;; Паўза праз getstring
        (getstring "\n[Debug]: Ячэйкі адмаляваны. Націсні Enter для выдалення...")
        (vl-cmdf "._erase" ss_tmp "")
       )
       ((= mode 0)
        (princ (strcat "\n[Debug]: Спіс ячэек (" (itoa (length data)) " oo.)")))
     ))

    ;; --- СПІСЫ ОБ'ЕКТАЎ (Грубы спіс кластара ці Selection Set) ---
    ((or (= msg_type "list") (= msg_type "ss"))
     (setq ss_tmp (ssadd))
     (cond 
       ((= (type data) 'PICKSET) (setq ss_tmp data))
       ((= (type data) 'LIST)
        (foreach item data
          (setq ent (if (= (type item) 'LIST) (car item) item))
          (if (= (type ent) 'ENAME) (ssadd ent ss_tmp))
        )
       )
     )
     
     (cond
       ((= mode 1) ;; Падсветка (пункцір)
	   (if (> (sslength ss_tmp) 0)
	   (progn
        (setq i 0)
        (repeat (sslength ss_tmp) (redraw (ssname ss_tmp i) 3) (setq i (1+ i)))
        (getstring "\n[Debug]: Аб'екты падсвечаны. Націсні Enter для скіду...")
        (setq i 0)
        (repeat (sslength ss_tmp) (redraw (ssname ss_tmp i) 4) (setq i (1+ i)))
		))
       )
       ((= mode 2) ;; Выдзяленне "з ручкамі"
	   (if (> (sslength ss_tmp) 0)
	   (progn
        (sssetfirst nil ss_tmp)
        (getstring "\n[Debug]: Аб'екты выбраны. Націсні Enter для працягу...")
		))
       )
       ((= mode 0)
        (princ (strcat "\n[Debug]: Знойдзена аб'ектаў: " (itoa (sslength ss_tmp)))))
     )
    )
  )
  (princ)
)
