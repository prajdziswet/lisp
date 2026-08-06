; Функция для поиска текста в прямоугольной области (работает в фоновых документах без _zoom)
(defun get-texts-in-box (tx1 ty1 tx2 ty2 / nabor_s text i len ename cur-ent etype pt 
                         txt-part vla-obj minpt maxpt bminX bminY bmaxX bmaxY catchbox 
                         in-zone tminX tminY tmaxX tmaxY
                        ) 
  (setq text "")
  (setq tminX (min tx1 tx2)
        tmaxX (max tx1 tx2)
  )
  (setq tminY (min ty1 ty2)
        tmaxY (max ty1 ty2)
  )
  (setq nabor_s (ssget "_X" 
                       (list (cons 0 "TEXT,MTEXT") 
                             (cons 410 (if model model (getvar "ctab")))
                       )
                )
  )
  (if nabor_s 
    (progn 
      (setq i   0
            len (sslength nabor_s)
      )
      (while (< i len) 
        (setq ename (ssname nabor_s i))
        (setq vla-obj (vlax-ename->vla-object ename))
        (setq catchbox (vl-catch-all-apply 'vla-GetBoundingBox 
                                           (list vla-obj 'minpt 'maxpt)
                       )
        )
        (setq in-zone nil)
        (if (not (vl-catch-all-error-p catchbox)) 
          (progn 
            (setq bminX (car (vlax-safearray->list minpt)))
            (setq bminY (cadr (vlax-safearray->list minpt)))
            (setq bmaxX (car (vlax-safearray->list maxpt)))
            (setq bmaxY (cadr (vlax-safearray->list maxpt)))
            ; Проверка на пересечение прямоугольников
            (if 
              (not 
                (or (< bmaxX tminX) 
                    (> bminX tmaxX)
                    (< bmaxY tminY)
                    (> bminY tmaxY)
                )
              )
              (setq in-zone T)
            )
          )
          (progn 
            (setq cur-ent (entget ename))
            (setq pt (cdr (assoc 10 cur-ent)))
            (if 
              (and (>= (car pt) tminX) 
                   (<= (car pt) tmaxX)
                   (>= (cadr pt) tminY)
                   (<= (cadr pt) tmaxY)
              )
              (setq in-zone T)
            )
          )
        )
        (if in-zone 
          (progn 
            (setq cur-ent (entget ename))
            (setq etype (cdr (assoc 0 cur-ent)))
            (if (= etype "MTEXT") 
              (setq txt-part (if (boundp 'clear-mtext) 
                               (clear-mtext (vla-get-TextString vla-obj))
                               (vla-get-TextString vla-obj)
                             )
              )
              (setq txt-part (cdr (assoc 1 cur-ent))) ; TEXT
            )
            (if txt-part (setq text (strcat text txt-part)))
          )
        )
        (setq i (+ 1 i))
      )
    )
  )
  text
)

;вяртанне назову рысунка
(defun namelist (/ x_temp1 x_temp2 text temp_lm) 
  (setq x_temp1 (list (- (car x2) (/ 125 mash)) (+ (cadr x2) (/ 20 mash))))
  (setq x_temp2 (list (- (car x2) (/ 55 mash)) (+ (cadr x2) (/ 5 mash))))

  (setq text "")
  (setq text (get-texts-in-box 
               (car x_temp1)
               (cadr x_temp1)
               (car x_temp2)
               (cadr x_temp2)
             )
  )
  (if (= text nil) (setq text ""))
  text
)

;вяртанне шифру
(defun nameshifr (temp / x_temp1 x_temp2 text temp_lm)  ;temp nil-верхняя шапка, инакш-нижняя
  (if (/= nil temp) 
    (progn  ; --- БОЛЬШОЙ ШТАМП (Шифр сверху) ---
           (setq x_temp1 (list (- (car x2) (/ 125 mash)) (+ (last x2) (/ 60 mash))))
           (setq x_temp2 (list (- (car x2) (/ 5 mash)) (+ (last x2) (/ 50 mash))))
    )
    (progn  ; --- МАЛЫЙ ШТАМП (Шифр снизу) ---
           (setq x_temp1 (list (- (car x2) (/ 125 mash)) (+ (last x2) (/ 20 mash))))
           (setq x_temp2 (list (- (car x2) (/ 15 mash)) (+ (last x2) (/ 5 mash))))
    )
  ) ;сканчэнне вызначэнне каардынат

  (setq text "")
  (setq text (get-texts-in-box 
               (car x_temp1)
               (cadr x_temp1)
               (car x_temp2)
               (cadr x_temp2)
             )
  )
  (if (= text nil) (setq text ""))
  text
)
;сканчэнне вяртанне шифру

; Возвращает T если большой штамп, nil если малый.
; Зависит от внешних переменных: x2, mash, model.
(defun is-big-stamp-p (/ det-min det-max nabor_s i ename edata etype txt ins-x ins-y 
                       sx sy rot blk-defname cur-ent cur-data lx ly wx wy temp_lm 
                       cur-ename found
                      ) 

  ; Вычисленные пользователем координаты: от +28 до +40
  (setq det-min (list (- (car x2) (/ 60 mash)) 
                      (+ (cadr x2) (/ 28 mash))
                )
  )
  (setq det-max (list (- (car x2) (/ 5 mash)) 
                      (+ (cadr x2) (/ 40 mash))
                )
  )

  ; --- Переключиться на нужный лист ---
  (setq temp_lm (getvar "ctab"))
  (if (and model (/= model temp_lm)) (setvar "ctab" model))

  (princ 
    (strcat "\n[is-big-stamp-p] Отладка масштаба: MASH = " 
            (rtos mash 2 4)
            (if x1 
              (strcat ", Ширина рамки: " 
                      (rtos (abs (- (car x2) (car x1))) 2 2)
                      ", Высота рамки: "
                      (rtos (abs (- (cadr x2) (cadr x1))) 2 2)
              )
              ""
            )
    )
  )

  ;(setq mid_pt (list (/ (+ (car det-min) (car det-max)) 2.0)
  ;                   (/ (+ (cadr det-min) (cadr det-max)) 2.0)))
  ;(vl-cmdf "_zoom" "_C" mid_pt (* 150 mash))

  ; Красная диагональ больше не нужна, убираем её


  ; Секущая рамка — заменяем на _X с проверкой BoundingBox!
  (setq nabor_s (ssget "_X" 
                       (list (cons 0 "TEXT,MTEXT,INSERT") 
                             (cons 410 (if model model (getvar "ctab")))
                       )
                )
  )

  (princ 
    (strcat "\n[is-big-stamp-p] Зона поиска от X2,Y2 (правый нижний угол):" 
            "\n   dX: "
            (rtos (- (car det-min) (car x2)) 2 2)
            " .. "
            (rtos (- (car det-max) (car x2)) 2 2)
            "\n   dY: "
            (rtos (- (cadr det-min) (cadr x2)) 2 2)
            " .. "
            (rtos (- (cadr det-max) (cadr x2)) 2 2)
    )
  )
  (if nabor_s 
    (princ 
      (strcat "\n[is-big-stamp-p] Найдено объектов: " (itoa (sslength nabor_s)))
    )
    (princ "\n[is-big-stamp-p] Объекты в зоне не найдены!")
  )

  (if (and model (/= model temp_lm)) (setvar "ctab" temp_lm))
  (setq found nil)

  (if nabor_s 
    (progn 
      (setq i 0)
      (while (and (< i (sslength nabor_s)) (not found)) 
        (setq ename (ssname nabor_s i))
        (setq edata (entget ename))
        (setq etype (cdr (assoc 0 edata)))

        ; Проверка попадания габаритного контейнера объекта в зону det-min .. det-max
        (setq in-zone nil)
        (setq catchbox (vl-catch-all-apply 'vla-GetBoundingBox 
                                           (list (vlax-ename->vla-object ename) 
                                                 'minpt
                                                 'maxpt
                                           )
                       )
        )
        (if (not (vl-catch-all-error-p catchbox)) 
          (progn 
            (setq bminX (car (vlax-safearray->list minpt)))
            (setq bminY (cadr (vlax-safearray->list minpt)))
            (setq bmaxX (car (vlax-safearray->list maxpt)))
            (setq bmaxY (cadr (vlax-safearray->list maxpt)))
            (setq in-zone (not 
                            (or (< bmaxX (car det-min)) 
                                (> bminX (car det-max))
                                (< bmaxY (cadr det-min))
                                (> bminY (cadr det-max))
                            )
                          )
            )
          )
          (setq in-zone T) ; fallback к проверке координат внутри cond
        )

        (if in-zone 
          (cond 

            ; --- TEXT или MTEXT: проверяем текст напрямую ---
            ((or (= etype "TEXT") (= etype "MTEXT"))
             (setq txt (strcase (cdr (assoc 1 edata))))
             (if 
               (or (vl-string-search "ТАДИЯ" txt) 
                   (vl-string-search "ИСТОВ" txt)
               )
               (setq found T)
             )
            )

            ; --- INSERT: без взрыва — идём в определение блока ---
            ((= etype "INSERT")
             (setq ins-x (car (cdr (assoc 10 edata)))
                   ins-y (cadr (cdr (assoc 10 edata)))
                   sx    (cdr (assoc 41 edata))
                   sy    (cdr (assoc 42 edata))
                   rot   (cdr (assoc 50 edata))
             )
             (if (not sx) (setq sx 1.0))
             (if (not sy) (setq sy 1.0))
             (if (not rot) (setq rot 0.0))

             ; 1) ATTRIBs вставки (мировые координаты — группа 11)
             (setq cur-ename (entnext ename))
             (while (and cur-ename (not found)) 
               (setq cur-data (entget cur-ename))
               (if (= (cdr (assoc 0 cur-data)) "SEQEND") 
                 (setq cur-ename nil)
                 (progn 
                   (if (= (cdr (assoc 0 cur-data)) "ATTRIB") 
                     (progn 
                       (setq txt (strcase (cdr (assoc 1 cur-data))))
                       (setq pt (cdr (assoc 11 cur-data)))
                       (if (and (= (car pt) 0.0) (= (cadr pt) 0.0)) 
                         (setq pt (cdr (assoc 10 cur-data)))
                       )
                       (setq wx (car pt)
                             wy (cadr pt)
                       )
                       (if 
                         (and (>= wx (car det-min)) 
                              (<= wx (car det-max))
                              (>= wy (cadr det-min))
                              (<= wy (cadr det-max))
                         )
                         (progn 
                           (if 
                             (or (vl-string-search "ТАДИЯ" txt) 
                                 (vl-string-search "ИСТОВ" txt)
                             )
                             (setq found T)
                           )
                         )
                       )
                     )
                   )
                   (setq cur-ename (entnext cur-ename))
                 )
               )
             )

             ; 2) Определение блока (TEXT, MTEXT, ATTDEF) — с трансформацией
             (setq blk-defname (tblobjname "BLOCK" (cdr (assoc 2 edata))))
             (if blk-defname 
               (progn 
                 (setq cur-ent (entnext blk-defname))
                 (while (and cur-ent (not found)) 
                   (setq cur-data (entget cur-ent))
                   (setq etype (cdr (assoc 0 cur-data)))
                   (if (member etype '("TEXT" "MTEXT" "ATTDEF")) 
                     (progn 
                       ; Проверяем группу 60 (0 = видим, 1 = невидим, nil = видим по умолчанию)
                       (if 
                         (not 
                           (and (assoc 60 cur-data) 
                                (= (cdr (assoc 60 cur-data)) 1)
                           )
                         )
                         (progn 
                           (setq txt (strcase (cdr (assoc 1 cur-data))))
                           (setq lx (car (cdr (assoc 10 cur-data))))
                           (setq ly (cadr (cdr (assoc 10 cur-data))))
                           (setq wx (+ ins-x 
                                       (* sx lx (cos rot))
                                       (- (* sy ly (sin rot)))
                                    )
                           )
                           (setq wy (+ ins-y 
                                       (* sx lx (sin rot))
                                       (* sy ly (cos rot))
                                    )
                           )
                           (if 
                             (and (>= wx (car det-min)) 
                                  (<= wx (car det-max))
                                  (>= wy (cadr det-min))
                                  (<= wy (cadr det-max))
                             )
                             (progn 
                               (if 
                                 (or (vl-string-search "ТАДИЯ" txt) 
                                     (vl-string-search "ИСТОВ" txt)
                                 )
                                 (setq found T)
                               )
                             )
                           )
                         )
                       )
                     )
                   )
                   (if (= (cdr (assoc 0 cur-data)) "ENDBLK") 
                     (setq cur-ent nil)
                     (setq cur-ent (entnext cur-ent))
                   )
                 )
               )
             )
            )
          )
        )

        (setq i (1+ i))
      )
    )
  )

  (if (and model (/= model temp_lm)) (setvar "ctab" temp_lm))
  found ; T = большой штамп, nil = малый
)

(defun get-shifr-from-blocks (is-big / shifr-found all_blocks iblk blk_name vla-blk 
                              minpt maxpt pt_min pt_max catchbox sminX smaxX sminY 
                              smaxY bminX bminY bmaxX bmaxY pt x1t y1t x2t y2t in-zone 
                              tag edata ins-x ins-y sx sy rot blk-defname cur-ent 
                              cur-data etype vla-obj txt lx ly wx wy
                             ) 
  (setq shifr-found nil)
  (setq all_blocks (ssget "_X" 
                          (list (cons 0 "INSERT") 
                                (cons 410 (if model model (getvar "ctab")))
                          )
                   )
  )
  (if all_blocks 
    (progn 
      (setq sminX (- (car x2) (* mash 190)))
      (setq smaxX (+ (car x2) (* mash 10)))
      (setq sminY (- (cadr x2) (* mash 10)))
      (setq smaxY (+ (cadr x2) (* mash 70)))
      (setq iblk 0)
      (while (and (< iblk (sslength all_blocks)) (= shifr-found nil)) 
        (setq blk_name (ssname all_blocks iblk))
        (setq vla-blk (vlax-ename->vla-object blk_name))
        (setq catchbox (vl-catch-all-apply 'vla-GetBoundingBox 
                                           (list vla-blk 'minpt 'maxpt)
                       )
        )
        (if (not (vl-catch-all-error-p catchbox)) 
          (progn 
            (setq pt_min (vlax-safearray->list minpt))
            (setq pt_max (vlax-safearray->list maxpt))
            (setq bminX (car pt_min)
                  bminY (cadr pt_min)
            )
            (setq bmaxX (car pt_max)
                  bmaxY (cadr pt_max)
            )
          )
          (progn 
            (setq pt_min (cdr (assoc 10 (entget blk_name))))
            (setq bminX (- (car pt_min) 2000)
                  bminY (- (cadr pt_min) 2000)
            )
            (setq bmaxX (+ (car pt_min) 2000)
                  bmaxY (+ (cadr pt_min) 2000)
            )
          )
        )
        (if (and (< bminX smaxX) (> bmaxX sminX) (< bminY smaxY) (> bmaxY sminY)) 
          (progn 
            ; блок попал в зону штампа — ищем атрибуты
            (if (= (vla-get-HasAttributes vla-blk) :vlax-true) 
              (progn 
                (foreach tag 
                  (vlax-safearray->list 
                    (vlax-variant-value (vla-getattributes vla-blk))
                  )
                  (if 
                    (and (= shifr-found nil) 
                         (= (vla-get-visible tag) :vlax-true)
                    )
                    (progn 
                      (setq pt (cdr 
                                 (assoc 11 (entget (vlax-vla-object->ename tag)))
                               )
                      )
                      (if (and (= (car pt) 0.0) (= (cadr pt) 0.0)) 
                        (setq pt (cdr 
                                   (assoc 10 (entget (vlax-vla-object->ename tag)))
                                 )
                        )
                      )
                      (setq x1t (nth 0 pt)
                            y1t (nth 1 pt)
                      )
                      (setq x2t (nth 0 x2)
                            y2t (nth 1 x2)
                      )

                      (setq in-zone nil)
                      (if is-big 
                        ; Зона большого штампа (шифр наверху)
                        (setq in-zone (and (<= (- x2t (* mash 126)) x1t) 
                                           (>= (- x2t (* mash 4)) x1t)
                                           (>= y1t (+ (* mash 49) y2t))
                                           (<= y1t (+ (* mash 61) y2t))
                                      )
                        )
                        ; Зона малого штампа
                        (setq in-zone (and (<= (- x2t (* mash 126)) x1t) 
                                           (>= (- x2t (* mash 14)) x1t)
                                           (>= y1t (+ (* mash 4) y2t))
                                           (<= y1t (+ (* mash 21) y2t))
                                      )
                        )
                      )
                      (setq txt (vla-get-TextString tag))
                      (if in-zone 
                        (setq shifr-found txt)
                      )
                    )
                  )
                )
              )
            )

            ; Если шифр не найден в атрибутах, ищем текст в определении блока (fallback)
            (if (= shifr-found nil) 
              (progn 
                (setq edata (entget blk_name))
                (setq ins-x (car (cdr (assoc 10 edata)))
                      ins-y (cadr (cdr (assoc 10 edata)))
                      sx    (cdr (assoc 41 edata))
                      sy    (cdr (assoc 42 edata))
                      rot   (cdr (assoc 50 edata))
                )
                (if (not sx) (setq sx 1.0))
                (if (not sy) (setq sy 1.0))
                (if (not rot) (setq rot 0.0))

                (setq blk-defname (tblobjname "BLOCK" (cdr (assoc 2 edata))))
                (if blk-defname 
                  (progn 
                    (setq cur-ent (entnext blk-defname))
                    (while (and cur-ent (= shifr-found nil)) 
                      (setq cur-data (entget cur-ent))
                      (setq etype (cdr (assoc 0 cur-data)))
                      (if (member etype '("TEXT" "MTEXT" "ATTDEF")) 
                        (progn 
                          (if 
                            (not 
                              (and (assoc 60 cur-data) 
                                   (= (cdr (assoc 60 cur-data)) 1)
                              )
                            )
                            (progn 
                              (setq vla-obj (vlax-ename->vla-object cur-ent))
                              (if (= etype "ATTDEF") 
                                (setq txt (cdr (assoc 1 cur-data)))
                                (setq txt (vla-get-TextString vla-obj))
                              )
                              (setq lx (car (cdr (assoc 10 cur-data))))
                              (setq ly (cadr (cdr (assoc 10 cur-data))))
                              (setq wx (+ ins-x 
                                          (* sx lx (cos rot))
                                          (- (* sy ly (sin rot)))
                                       )
                              )
                              (setq wy (+ ins-y 
                                          (* sx lx (sin rot))
                                          (* sy ly (cos rot))
                                       )
                              )
                              (setq x1t wx
                                    y1t wy
                              )
                              (setq x2t (nth 0 x2)
                                    y2t (nth 1 x2)
                              )

                              (setq in-zone nil)
                              (if is-big 
                                (setq in-zone (and (<= (- x2t (* mash 126)) x1t) 
                                                   (>= (- x2t (* mash 4)) x1t)
                                                   (>= y1t (+ (* mash 49) y2t))
                                                   (<= y1t (+ (* mash 61) y2t))
                                              )
                                )
                                (setq in-zone (and (<= (- x2t (* mash 126)) x1t) 
                                                   (>= (- x2t (* mash 14)) x1t)
                                                   (>= y1t (+ (* mash 4) y2t))
                                                   (<= y1t (+ (* mash 21) y2t))
                                              )
                                )
                              )
                              (if in-zone 
                                (setq shifr-found (if 
                                                    (or (= etype "MTEXT") 
                                                        (= etype "ATTDEF")
                                                    )
                                                    (clear-mtext txt)
                                                    txt
                                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                      (if (= (cdr (assoc 0 cur-data)) "ENDBLK") 
                        (setq cur-ent nil)
                        (setq cur-ent (entnext cur-ent))
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (setq iblk (1+ iblk))
      )
    )
  )
  shifr-found
)

; для выбранного многострочного текста очищает форматирование.=============================================================================|;
(defun clear-mtext (str / pos temp end) 
  (if (and str (= (type str) 'STR)) 
    (progn 
      ;; Замена \P на пробел
      (while (setq pos (vl-string-search "\\P" (strcase str))) 
        (setq str (strcat (substr str 1 pos) " " (substr str (+ pos 3))))
      )
      (setq pos 0)
      (while (< pos (strlen str)) 
        (setq temp (substr str (1+ pos) 2))
        (cond 
          ;; Escaped characters: \\, \{, \}
          ((member (strcase temp) '("\\\\" "\\{" "\\}"))
           (setq str (strcat (substr str 1 pos) 
                             (substr temp 2 1)
                             (substr str (+ pos 3))
                     )
                 pos (1+ pos)
           )
          )
          ;; Toggle codes: \L, \l, \O, \o, \K, \k
          ((member (strcase temp) '("\\L" "\\O" "\\K"))
           (setq str (strcat (substr str 1 pos) (substr str (+ pos 3))))
          )
          ;; Non-breaking space \~
          ((= temp "\\~")
           (setq str (strcat (substr str 1 pos) " " (substr str (+ pos 3)))
                 pos (1+ pos)
           )
          )
          ;; Stacking \S...; keep the content
          ((= (strcase temp) "\\S")
           (if (setq end (vl-string-position 59 str pos))  ; 59 is ";"
             (setq str (strcat (substr str 1 pos) 
                               (substr str (+ pos 3) (- end pos 2))
                               (substr str (+ end 2))
                       )
             )
             (setq pos (1+ pos))
           )
          )
          ;; Formatting codes ending with ";" e.g., \fArial; \A1; \C1; \H1.5; \T1; \Q15; \W1; \p;
          ((and (= (substr temp 1 1) "\\") 
                (member (strcase (substr temp 2 1)) 
                        '("F" "A" "C" "H" "T" "Q" "W" "P")
                )
           )
           (if (setq end (vl-string-position 59 str pos))  ; 59 is ";"
             (setq str (strcat (substr str 1 pos) (substr str (+ end 2))))
             (setq pos (1+ pos)) ; fallback if no ';' found
           )
          )
          ;; Unescaped braces { and }
          ((= (substr temp 1 1) "{")
           (setq str (strcat (substr str 1 pos) (substr str (+ pos 2))))
          )
          ((= (substr temp 1 1) "}")
           (setq str (strcat (substr str 1 pos) (substr str (+ pos 2))))
          )
          (t (setq pos (1+ pos)))
        )
      )
      (vl-string-trim " \t\r\n" str)
    )
    ""
  )
)
;----------------------------------------------------

;--------------функция атрыманне с дин.блоку тексту----------------
(defun get-block-att (name_bl / vla-nameobj spis atts x1temp y1temp x2temp y2temp 
                      point tag copy_obj exploded_objs catchit catchbox minpt maxpt
                     ) 
  (if (= 'LIST (type name_bl)) 
    (progn 
      (setq name_bl (car name_bl))
    )
  )
  ;преобразование имени ва вла
  (setq vla-nameobj (vlax-ename->vla-object name_bl))

  ;получение атрибутов
  (if (= (vla-get-HasAttributes vla-nameobj) :vlax-true) 
    (progn 
      (setq atts (vlax-safearray->list (vlax-variant-value (vla-getattributes vla-nameobj))))
      (foreach tag atts 
        (if (= (vla-get-visible tag) :vlax-true) 
          (progn 
            (setq point (cdr (assoc 11 (entget (vlax-vla-object->ename tag)))))
            (setq x1temp (nth 0 point)
                  y1temp (nth 1 point)
            )
            (setq x2temp (nth 0 x2)
                  y2temp (nth 1 x2)
            )
            (cond 
              ((and (< (- x2temp (* mash 40)) x1temp) 
                    (> (- x2temp (* mash 25)) x1temp)
                    (> y1temp (+ (* mash 20) y2temp))
                    (< y1temp (+ (* mash 30) y2temp))
               )
               (setq numa (vla-get-TextString tag))
              )
              ((and (< (- x2temp (* mash 15)) x1temp) 
                    (> (- x2temp (* mash 5)) x1temp)
                    (> y1temp (+ (* mash 5) y2temp))
                    (< y1temp (+ (* mash 13) y2temp))
               )
               (setq numa (vla-get-TextString tag))
              )
              ((and (< (- x2temp (* mash 125)) x1temp) 
                    (> (- x2temp (* mash 55)) x1temp)
                    (> y1temp (+ (* mash 5) y2temp))
                    (< y1temp (+ (* mash 20) y2temp))
               )
               (setq nameris (clear-mtext (vla-get-TextString tag)))
              )
            )
          )
        )
      )
    )
  )

  ; Если имя или номер не найдены в атрибутах, ищем текст в определении блока
  (if (or (= numa nil) (= nameris nil)) 
    (progn 
      (setq edata (entget name_bl))
      (setq ins-x (car (cdr (assoc 10 edata)))
            ins-y (cadr (cdr (assoc 10 edata)))
            sx    (cdr (assoc 41 edata))
            sy    (cdr (assoc 42 edata))
            rot   (cdr (assoc 50 edata))
      )
      (if (not sx) (setq sx 1.0))
      (if (not sy) (setq sy 1.0))
      (if (not rot) (setq rot 0.0))

      (setq blk-defname (tblobjname "BLOCK" (cdr (assoc 2 edata))))
      (if blk-defname 
        (progn 
          (setq cur-ent (entnext blk-defname))
          (while cur-ent 
            (setq cur-data (entget cur-ent))
            (setq etype (cdr (assoc 0 cur-data)))
            (if (member etype '("TEXT" "MTEXT" "ATTDEF")) 
              (progn 
                ; Проверяем группу 60 (0 = видим, 1 = невидим, nil = видим по умолчанию)
                (if (not (and (assoc 60 cur-data) (= (cdr (assoc 60 cur-data)) 1))) 
                  (progn 
                    (setq vla-obj (vlax-ename->vla-object cur-ent))
                    (if (= etype "ATTDEF") 
                      (setq txt (cdr (assoc 1 cur-data)))
                      (setq txt (vla-get-TextString vla-obj))
                    )
                    (setq lx (car (cdr (assoc 10 cur-data))))
                    (setq ly (cadr (cdr (assoc 10 cur-data))))
                    ; пересчёт в мировые
                    (setq wx (+ ins-x (* sx lx (cos rot)) (- (* sy ly (sin rot)))))
                    (setq wy (+ ins-y (* sx lx (sin rot)) (* sy ly (cos rot))))
                    (setq x1temp wx
                          y1temp wy
                    )
                    (setq x2temp (nth 0 x2)
                          y2temp (nth 1 x2)
                    )

                    (cond 
                      ((and (= numa nil) 
                            (< (- x2temp (* mash 40)) x1temp)
                            (> (- x2temp (* mash 25)) x1temp)
                            (> y1temp (+ (* mash 20) y2temp))
                            (< y1temp (+ (* mash 30) y2temp))
                       )
                       (setq numa (if (or (= etype "MTEXT") (= etype "ATTDEF")) 
                                    (clear-mtext txt)
                                    txt
                                  )
                       )
                      )
                      ((and (= numa nil) 
                            (< (- x2temp (* mash 15)) x1temp)
                            (> (- x2temp (* mash 5)) x1temp)
                            (> y1temp (+ (* mash 5) y2temp))
                            (< y1temp (+ (* mash 13) y2temp))
                       )
                       (setq numa (if (or (= etype "MTEXT") (= etype "ATTDEF")) 
                                    (clear-mtext txt)
                                    txt
                                  )
                       )
                      )
                      ((and (= nameris nil) 
                            (< (- x2temp (* mash 125)) x1temp)
                            (> (- x2temp (* mash 55)) x1temp)
                            (> y1temp (+ (* mash 5) y2temp))
                            (< y1temp (+ (* mash 20) y2temp))
                       )
                       (setq nameris (clear-mtext txt))
                      )
                    )
                  )
                )
              )
            )
            (if (= (cdr (assoc 0 cur-data)) "ENDBLK") 
              (setq cur-ent nil)
              (setq cur-ent (entnext cur-ent))
            )
          )
        )
      )
    )
  )
)
;-------------------------------
(defun find-stamp-blocks (/ all_blocks iblk blk_name vla-blk minpt maxpt pt_min 
                          pt_max bminX bminY bmaxX bmaxY sminX smaxX sminY smaxY 
                          catchbox
                         ) 
  (setq all_blocks (ssget "_X" 
                          (list (cons 0 "INSERT") 
                                (cons 410 (if model model (getvar "ctab")))
                          )
                   )
  )
  (if all_blocks 
    (progn 
      (setq sminX (- (car x2) (* mash 190)))
      (setq smaxX (+ (car x2) (* mash 10)))
      (setq sminY (- (cadr x2) (* mash 10)))
      (setq smaxY (+ (cadr x2) (* mash 60)))
      (setq iblk 0)
      (while (< iblk (sslength all_blocks)) 
        (setq blk_name (ssname all_blocks iblk))
        (setq vla-blk (vlax-ename->vla-object blk_name))
        (setq catchbox (vl-catch-all-apply 'vla-GetBoundingBox 
                                           (list vla-blk 'minpt 'maxpt)
                       )
        )
        (if (not (vl-catch-all-error-p catchbox)) 
          (progn 
            (setq pt_min (vlax-safearray->list minpt))
            (setq pt_max (vlax-safearray->list maxpt))
            (setq bminX (car pt_min)
                  bminY (cadr pt_min)
            )
            (setq bmaxX (car pt_max)
                  bmaxY (cadr pt_max)
            )
            (if 
              (and (< bminX smaxX) (> bmaxX sminX) (< bminY smaxY) (> bmaxY sminY))
              (try 'get-block-att (list blk_name))
            )
          )
        )
        (setq iblk (1+ iblk))
      )
    )
  )
)
;-------------------------------




;;---------------------нормализация № старонки----------------------
(defun norma_n (/ str) 
  (setq str (cdr (assoc 1 (entget (ssname nabor_s 0)))))
  (setq str (vl-string-trim " " str))
  (if 
    (wcmatch str 
             "#,##,###,#`.#,##`.#,###`.#,#`.##,##`.##,###`.##,#`.###,##`.###,###`.###"
    )
    (princ str)
    (princ nil)
  )
  ;end if
)

;;---------------------нормализация № старонки(спдс)----------------------
(defun norma_n2 (str) 
  (setq str (vl-string-trim " " (vl-princ-to-string str)))
  (if 
    (wcmatch str 
             "#,##,###,#`.#,##`.#,###`.#,#`.##,##`.##,###`.##,#`.###,##`.###,###`.###"
    )
    str
    nil
  ) ;end if
)


;;---------------------вызначэнне № старонки----------------------
(defun numar_s (/ x_temp1 x_temp2 text) 
  (setq zapret_nomer nil)

  (setq x_temp1 (list (- (car x2) (/ 40 mash)) (+ (last x2) (/ 30 mash))))
  (setq x_temp2 (list (- (car x2) (/ 25 mash)) (+ (last x2) (/ 20 mash))))

  (setq text (get-texts-in-box 
               (car x_temp1)
               (cadr x_temp1)
               (car x_temp2)
               (cadr x_temp2)
             )
  )

  (if (and text (/= text "")) 
    (norma_n2 text)
    (progn 
      ;праверка наяунасти "листов"----------------------------
      (setq x_temp1 (list (- (car x2) (/ 25 mash)) (+ (last x2) (/ 30 mash))))
      (setq x_temp2 (list (- (car x2) (/ 5 mash)) (+ (last x2) (/ 20 mash))))
      (setq text (get-texts-in-box 
                   (car x_temp1)
                   (cadr x_temp1)
                   (car x_temp2)
                   (cadr x_temp2)
                 )
      )
      (if (and text (/= text "")) 
        (setq zapret_nomer (norma_n2 text))
      )

      ;калі малы штамп
      (if (= nil zapret_nomer) 
        (progn 
          (setq zapret_name T)
          (setq x_temp1 (list (- (car x2) (/ 15 mash)) (+ (last x2) (/ 13 mash))))
          (setq x_temp2 (list (- (car x2) (/ 5 mash)) (+ (last x2) (/ 5 mash))))
          (setq text (get-texts-in-box 
                       (car x_temp1)
                       (cadr x_temp1)
                       (car x_temp2)
                       (cadr x_temp2)
                     )
          )
          (if (and text (/= text "")) 
            (setq zapret_nomer (norma_n2 text))
          )
        )
      )
      (if zapret_nomer (princ zapret_nomer))
    )
  )
)

;;---------------------праверка супадзенне нумароу----------------------
(defun prin_numar (/ nov_spis druk_v1 nomer_s nlist xt1 xt2 xt11 xt22) 
  (setq nov_spis nil)
  (if (and (/= druk_v nil) (/= (last druk_v) nil)) 
    (progn 
      (while (and (/= druk_v nil) (/= (last druk_v) nil)) 
        (setq druk_v1 nil)
        (if (= nov_spis nil) 
          (setq nov_spis (list 
                           (cons (car (car druk_v)) 
                                 (cdr (car druk_v))
                           )
                         )
          )
          (setq nov_spis (append 
                           (list 
                             (cons (car (car druk_v)) 
                                   (cdr (car druk_v))
                             )
                           )
                           nov_spis
                         )
          )
        ) ;end if
        (setq nomer_s (car (car druk_v))) ;атрыманне нумару старонки
        (setq xt1 nil
              xt2 nil
        )
        (setq xt1 (nth 1 (car druk_v)))
        (setq xt2 (nth 2 (car druk_v)))
        (setq nlist 1)
        (setq druk_v (cdr druk_v)) ;знишчэнне першага-ен перайшоу у новы спис
        (while (/= druk_v nil) 
          (if (equal nomer_s (car (car druk_v))) 
            (progn 
              (setq xt11 nil
                    xt22 nil
              )
              (setq xt11 (nth 1 (car druk_v)))
              (setq xt22 (nth 2 (car druk_v)))
              (if 
                (and (= (nth 0 xt1) (nth 0 xt11)) 
                     (= (nth 1 xt1) (nth 1 xt11))
                     (= (nth 0 xt2) (nth 0 xt22))
                     (= (nth 1 xt2) (nth 1 xt22))
                )
                (setq druk_v (cdr druk_v)) ;знишчэнне першага
                (progn 
                  (setq nov_spis (cons 
                                   (cons 
                                     (strcat nomer_s 
                                             "+"
                                             (rtos nlist 2 0)
                                     )
                                     (cdr (car druk_v))
                                   )
                                   nov_spis
                                 )
                  )
                  (setq nlist (1+ nlist))
                  (setq druk_v (cdr druk_v)) ;знишчэнне першага
                )
              ) ;end - progn
            )
            (progn 
              (if (/= druk_v1 nil) 
                (setq druk_v1 (cons (car druk_v) druk_v1))
                (setq druk_v1 (list (car druk_v)))
              )
              (setq druk_v (cdr druk_v)) ;знишчэнне першага
            )
          ) ;end if
        ) ;end while
        (setq druk_v druk_v1)
      ) ;end while
      (if (and (/= druk_v nil) (= (last druk_v) nil)) 
        (setq nov_spis (append 
                         nov_spis
                         (list (cons (car druk_v) (cdr druk_v)))
                       )
        )
      )
      (setq druk_v nov_spis)
    ) ;end progn
    (progn 
      (if (/= druk_v nil) 
        (setq druk_v (cons (car druk_v) (cdr druk_v)))
      )
    )
  ) ;end if
)					;end defun vuznach povtor

;;---------------------прастауленне невызначаных нумароу----------------------
(defun zad_n (/ nlist) 
  (setq nlist 1)
  (while (/= druk_n nil) 
    (if (/= druk_v nil) 
      (if (/= (last druk_n) nil) 
        (setq druk_v (append druk_v 
                             (list 
                               (cons (strcat (rtos nlist 2 0) "~") 
                                     (cdr (car druk_n))
                               )
                             )
                     )
        )
        (setq druk_v (cons (cons (strcat (rtos nlist 2 0) "~") (cdr druk_n)) 
                           druk_v
                     )
        )
      ) ;end if
      (if (/= (last druk_n) nil) 
        (setq druk_v (list 
                       (cons (strcat (rtos nlist 2 0) "~") 
                             (cdr (car druk_n))
                       )
                     )
        )
        (setq druk_v (cons (strcat (rtos nlist 2 0) "~") (cdr druk_n)))
      ) ;end if
    ) ;end if ///
    (setq nlist (1+ nlist))
    (if (/= (last druk_n) nil) 
      (setq druk_n (cdr druk_n))
      (setq druk_n nil)
    )
  )
)					;end zadфту num

;=================================================================

;=================================================================

;=================================================================

;;---------------------вызначэнне фармату----------------------
(defun v_formats (/ dis1 dis2) 
  ;нормализация координат
  (normal_points)

  ;определения длины сторон
  (SETQ dis1 (ABS (- (CAR x1) (CAR x2))))
  (SETQ dis2 (ABS (- (CADR x2) (CADR x1))))

  (if (not mash) 
    (progn 
      ;;вызначэнне маштабу 1-1 або 1-100
      (if (> dis1 dis2) 
        (if (< dis1 27000) 
          (SETQ mash 1)
          (progn 
            (SETQ dis1 (/ dis1 100))
            (SETQ dis2 (/ dis2 100))
            (SETQ mash 0.01)
          )
        )
        (if (< dis2 27000) 
          (SETQ mash 1)
          (progn 
            (SETQ dis1 (/ dis1 100))
            (SETQ dis2 (/ dis2 100))
            (SETQ mash 0.01)
          )
        )
      )
    )
    (progn 
      (SETQ dis1 (* dis1 mash))
      (SETQ dis2 (* dis2 mash))
    )
  )

  ; подфункция определения//отклонения от стандарта
  (Defun ff1 (dis11 dis22 x1 x2) 
    (if (and (equal dis11 x1 2.01) (equal dis22 x2 2.01)) 
      T
      nil
    )
  )

  (if (= format nil) 
    (if (> dis1 dis2) 
      (progn 
        (SETQ poloz "А")
        (SETQ format (COND 
                       ((ff1 dis1 dis2 630 297) "А4х3 (630.00 x 297.00 мм)")
                       ((ff1 dis1 dis2 841 297) "А4х4 (841.00 x 297.00 мм)")
                       ((ff1 dis1 dis2 1051 297) "А4х5 (1051.00 x 297.00 мм)")
                       ((ff1 dis1 dis2 1261 297) "А4х6 (1261.00 x 297.00 мм)")
                       ((ff1 dis1 dis2 1471 297) "А4х7 (1471.00 x 297.00 мм)")
                       ((ff1 dis1 dis2 1682 297) "А4х8 (1682.00 x 297.00 мм)")
                       ((ff1 dis1 dis2 1892 297) "А4х9 (1892.00 x 297.00 мм)")
                       ((ff1 dis1 dis2 891 420) "А3х3 (891.00 x 420.00 мм)")
                       ((ff1 dis1 dis2 1189 420) "А3х4 (1189.00 x 420.00 мм)")
                       ((ff1 dis1 dis2 1486 420) "А3х5 (1486.00 x 420.00 мм)")
                       ((ff1 dis1 dis2 1783 420) "А3х6 (1783.00 x 420.00 мм)")
                       ((ff1 dis1 dis2 2080 420) "А3х7 (2080.00 x 420.00 мм)")
                       ((ff1 dis1 dis2 1261 594) "А2х3 (1261.00 x 594.00 мм)")
                       ((ff1 dis1 dis2 1682 594) "А2х4 (1682.00 x 594.00 мм)")
                       ((ff1 dis1 dis2 2102 594) "А2х5 (2102.00 x 594.00 мм)")
                       ((ff1 dis1 dis2 1783 841) "А1х3 (1783.00 x 841.00 мм)")
                       ((ff1 dis1 dis2 2378 841) "А1х4 (2378.00 x 841.00 мм)")
                       ((ff1 dis1 dis2 1682 1189) "А0х2 (1682.00 x 1189.00 мм)")
                       ((ff1 dis1 dis2 2523 1189) "А0х3 (2523.00 x 1189.00 мм)")
                       ((ff1 dis1 dis2 1189 841) "А0 (1189.00 x 841.00 мм)")
                       ((ff1 dis1 dis2 841 594) "А1 (841.00 x 594.00 мм)")
                       ((ff1 dis1 dis2 594 420) "А2 (594.00 x 420.00 мм)")
                       ((ff1 dis1 dis2 420 297) "А3 (420.00 x 297.00 мм)")
                       ((ff1 dis1 dis2 297 210) "А4 (297.00 x 210.00 мм)")
                       (T nil)
                     )
        )
      ) ;то
      (progn 
        (SETQ poloz "К")
        (SETQ format (COND 
                       ((ff1 dis2 dis1 630 297) "А4х3 (297.00 x 630.00 мм)")
                       ((ff1 dis2 dis1 841 297) "А4х4 (297.00 x 841.00 мм)")
                       ((ff1 dis2 dis1 1051 297) "А4х5 (297.00 x 1051.00 мм)")
                       ((ff1 dis2 dis1 1261 297) "А4х6 (297.00 x 1261.00 мм)")
                       ((ff1 dis2 dis1 1471 297) "А4х7 (297.00 x 1471.00 мм)")
                       ((ff1 dis2 dis1 1682 297) "А4х8 (297.00 x 1682.00 мм)")
                       ((ff1 dis2 dis1 1892 297) "А4х9 (297.00 x 1892.00 мм)")
                       ((ff1 dis2 dis1 891 420) "А3х3 (420.00 x 891.00 мм)")
                       ((ff1 dis2 dis1 1189 420) "А3х4 (420.00 x 1189.00 мм)")
                       ((ff1 dis2 dis1 1486 420) "А3х5 (420.00 x 1486.00 мм)")
                       ((ff1 dis2 dis1 1783 420) "А3х6 (420.00 x 1783.00 мм)")
                       ((ff1 dis2 dis1 2080 420) "А3х7 (420.00 x 2080.00 мм)")
                       ((ff1 dis2 dis1 1261 594) "А2х3 (594.00 x 1261.00 мм)")
                       ((ff1 dis2 dis1 1682 594) "А2х4 (594.00 x 1682.00 мм)")
                       ((ff1 dis2 dis1 2102 594) "А2х5 (594.00 x 2102.00 мм)")
                       ((ff1 dis2 dis1 1783 841) "А1х3 (841.00 x 1783.00 мм)")
                       ((ff1 dis2 dis1 2378 841) "А1х4 (841.00 x 2378.00 мм)")
                       ((ff1 dis2 dis1 1682 1189) "А0х2 (1189.00 x 1682.00 мм)")
                       ((ff1 dis2 dis1 2523 1189) "А0х3 (1189.00 x 2523.00 мм)")
                       ((ff1 dis2 dis1 1189 841) "А0 (841.00 x 1189.00 мм)")
                       ((ff1 dis2 dis1 841 594) "А1 (594.00 x 841.00 мм)")
                       ((ff1 dis2 dis1 594 420) "А2 (420.00 x 594.00 мм)")
                       ((ff1 dis2 dis1 420 297) "А3 (297.00 x 420.00 мм)")
                       ((ff1 dis2 dis1 297 210) "А4 (210.00 x 297.00 мм)")
                       (T nil)
                     )
        )
        ;;закрывает иначе
      ) ;закрывает сам иф
    ) ;закрывает верх иф
  )
)

; нормализация координат
(defun normal_points (/ xtemp) 
  ;нормализация координат
  (if (and (= 3 (length x1)) (= 3 (length x2)) (/= 0 (length x1))) 
    (progn 
      (setq x1 (list (car x1) (nth 1 x1)))
      (setq x2 (list (car x2) (nth 1 x2)))
    )
  )
  ;сканчэнне нармализации каардынат
  (if (> (nth 1 x2) (nth 1 x1)) 
    (progn 
      (setq xtemp (nth 1 x1))
      (setq x1 (list (car x1) (nth 1 x2)))
      (setq x2 (list (car x2) xtemp))
    )
  )
  (if (> (nth 0 x1) (nth 0 x2)) 
    (progn 
      (setq xtemp (nth 0 x1))
      (setq x1 (list (car x2) (nth 1 x1)))
      (setq x2 (list xtemp (nth 1 x2)))
    )
  )
)

;=================================================================

;=================================================================

;=================================================================
(defun try (namefunc lists / catchit) 
  (setq catchit (vl-catch-all-apply namefunc lists))
  (if (vl-catch-all-error-p catchit) 
    (progn 
      (print 
        (strcat "Памылка у функціі: " 
                (vl-symbol-name namefunc)
                " - "
                (vl-catch-all-error-message catchit)
        )
      )
      (princ)
    )
  )
)
;вызначэнне фармату полилилиний
(defun polyline_format (nameobj / vla-nameobj property) 
  (setq nameris     nil
        zapret_name nil
        format      nil
  )
  (if (= 'LIST (type nameobj)) 
    (progn 
      (setq nameobj (car nameobj))
    )
  )
  (if (/= nil (setq property (entget nameobj))) 
    (progn 
      (setq model (cdr (assoc 410 property))) ;model-list
      (setq vla-nameobj (vlax-ename->vla-object nameobj))
      (vla-GetBoundingBox vla-nameobj 'x1 'x2)
      (setq x1 (vlax-safearray->list x1))
      (setq x2 (vlax-safearray->list x2))
      (if (and (/= x1 nil) (/= x2 nil)) 
        (progn 
          (normal_points)
          (v_formats)
        )
      )
    )
  ) ;end if
)					;канец вызначэнне формата

;;-----------------------------друк поліліній-------------------------
(defun poli (/ x3 len nabor_poly list_pol x1 x2 format mash poloz i i1 x3 model) 
  (setq zapret_name nil)
  (vl-load-com)
  (if (= true_explode nil) 
    (setq nabor_poly (ssget "_X" '((0 . "LWPOLYLINE"))))
    (setq nabor_poly (ssget "_p" '((0 . "LWPOLYLINE"))))
  )

  (if (null nabor_poly) 

    (progn 
      (princ "\nНе составлен список полініній. ")
      ; сообщение об отсутствии
      (princ) ; тихий выход
    ) ; конец progn

    (progn 
      (setq i   -1
            len (sslength nabor_poly)
      )
      (repeat len 
        (setq i (1+ i))

        ; Выбор следующего примитива и получение его списка
        (setq x1 nil
              x2 nil
              x3 nil
        )

        (setq mash 1)
        (try 'polyline_format (list (ssname nabor_poly i))) ;имя примитива - формат
        ;(polyline_format (ssname nabor_poly i))

        (if (= format nil) 
          (progn 
            (setq mash 100)
            (try 'polyline_format (list (ssname nabor_poly i))) ;имя примитива - формат
            ;(polyline_format (ssname nabor_poly i))
          )
        )

        (if (/= format nil) 
          (progn 
            (find-stamp-blocks)
            (utvar numa)
            (setq nameris nil
                  numa    nil
                  format  nil
            )
          )
        )
      )
      ;;конец repeat
    ) ;конец progn
  )
  ;;конец if
)					;канец дефана полі

;=================================================================
;=================================================================
;=================================================================
;вызначэнне параметрау блоку нумар-имя
;;-----------------------------друк блокау-------------------------
(defun blocks (/ x3 len nabor_blocks list_pol x1 x2 x3 format mash poloz model 
               list_block i i1 model1 temp_lm true_explode obj len1 list_b1 list_b2 
               osmode_old_
              ) 
  (vl-load-com)
  ;break osmode
  (setq osmode_old_ (getvar "osmode"))
  (setvar "osmode" 0)
  ;выбор блоков
  (setq nabor_blocks (ssget "_X" 
                            (list (cons 0 "INSERT") 
                                  (cons 100 "AcDbBlockReference")
                            )
                     )
  )
  (if (null nabor_blocks) 

    (progn 
      (princ "\nНе составлен список блоков. ")
      ; сообщение об отсутствии
      (princ) ; тихий выход
    ) ; конец progn

    (progn 
      (setq i   -1
            len (sslength nabor_blocks)
      )
      (repeat len 
        (setq i (1+ i)) ; Выбор следующего примитива и получение его списка
        (SETQ name (ssname nabor_blocks i))
        (if (/= nil (setq property (entget name))) 
          (progn 
            (setq mash (/ 1 (cdr (assoc 41 property))))
            (try 'polyline_format (list name)) ;имя примитива - формат
            ;--------------------------------------------------
            (if (/= format nil) 
              (progn 
                (try 'get-block-att (list name))
                (find-stamp-blocks)
                (utvar numa)
                (setq nameris nil
                      numa    nil
                      format  nil
                )
              )
            )
            ;end if
          )
        )

        ;;конец repeat
      )
    )
  )
  (setvar "osmode" osmode_old_)
)					;канец дефана блокау



;=================================================================

;=================================================================

;;---------------------вызначэнне СПДС формат----------------------
(defun v_format_spds (/ f_temp numa name1 name2 name3 is-big shifr xtemp) 
  (setq format nil)
  ;;вызначэнне маштабу
  (if (/= (assoc 40 list1) nil) 
    (setq mash (/ 1 (cdr (assoc 40 list1))))
    (setq mash 1)
  )
  ;нормализация координат
  (if (and (= 3 (length x1)) (= 3 (length x2)) (/= 0 (length x1))) 
    (progn 
      (setq x1 (list (car x1) (nth 1 x1)))
      (setq x2 (list (car x2) (nth 1 x2)))
    )
  )
  (if (> (last x2) (last x1)) 
    (progn 
      (setq xtemp (last x1))
      (setq x1 (list (car x1) (last x2)))
      (setq x2 (list (car x2) xtemp))
    )
  )
  ;;вызначэнне положения
  (if 
    (> (abs (- (car x2) (car x1))) 
       (abs (- (last x1) (last x2)))
    )
    (SETQ poloz "А")
    (SETQ poloz "К")
  )

  ;вызначэнне нумару спдс
  (setq numa (cdr (assoc 300 (member '(301 . "Sheet") list1))))
  (if numa (setq numa (norma_n2 numa)))

  ;получение формата
  (setq f_temp (cdr (assoc 300 (member '(301 . "Format") list1))))
  (if f_temp 
    (progn 
      (setq f_temp (vl-string-subst "А" "A" f_temp))
      (setq f_temp (vl-string-subst "х" "x" f_temp))
    )
  )

  (if (= poloz "А") 
    (SETQ format (COND 
                   ((= "А4х3" f_temp) "А4х3 (630.00 x 297.00 мм)")
                   ((= "А4х4" f_temp) "А4х4 (841.00 x 297.00 мм)")
                   ((= "А4х5" f_temp) "А4х5 (1051.00 x 297.00 мм)")
                   ((= "А4х6" f_temp) "А4х6 (1261.00 x 297.00 мм)")
                   ((= "А4х7" f_temp) "А4х7 (1471.00 x 297.00 мм)")
                   ((= "А4х8" f_temp) "А4х8 (1682.00 x 297.00 мм)")
                   ((= "А4х9" f_temp) "А4х9 (1892.00 x 297.00 мм)")
                   ((= "А3х3" f_temp) "А3х3 (891.00 x 420.00 мм)")
                   ((= "А3х4" f_temp) "А3х4 (1189.00 x 420.00 мм)")
                   ((= "А3х5" f_temp) "А3х5 (1486.00 x 420.00 мм)")
                   ((= "А3х6" f_temp) "А3х6 (1783.00 x 420.00 мм)")
                   ((= "А3х7" f_temp) "А3х7 (2080.00 x 420.00 мм)")
                   ((= "А2х3" f_temp) "А2х3 (1261.00 x 594.00 мм)")
                   ((= "А2х4" f_temp) "А2х4 (1682.00 x 594.00 мм)")
                   ((= "А2х5" f_temp) "А2х5 (2102.00 x 594.00 мм)")
                   ((= "А1х3" f_temp) "А1х3 (1783.00 x 841.00 мм)")
                   ((= "А1х4" f_temp) "А1х4 (2378.00 x 841.00 мм)")
                   ((= "А0х2" f_temp) "А0х2 (1682.00 x 1189.00 мм)")
                   ((= "А0х3" f_temp) "А0х3 (2523.00 x 1189.00 мм)")
                   ((= "А0" f_temp) "А0 (1189.00 x 841.00 мм)")
                   ((= "А1" f_temp) "А1 (841.00 x 594.00 мм)")
                   ((= "А2" f_temp) "А2 (594.00 x 420.00 мм)")
                   ((= "А3" f_temp) "А3 (420.00 x 297.00 мм)")
                   ((= "А4" f_temp) "А4 (297.00 x 210.00 мм)")
                   (T nil)
                 )
    ) ;то
    (SETQ format (COND 
                   ((= "А4х3" f_temp) "А4х3 (297.00 x 630.00 мм)")
                   ((= "А4х4" f_temp) "А4х4 (297.00 x 841.00 мм)")
                   ((= "А4х5" f_temp) "А4х5 (297.00 x 1051.00 мм)")
                   ((= "А4х6" f_temp) "А4х6 (297.00 x 1261.00 мм)")
                   ((= "А4х7" f_temp) "А4х7 (297.00 x 1471.00 мм)")
                   ((= "А4х8" f_temp) "А4х8 (297.00 x 1682.00 мм)")
                   ((= "А4х9" f_temp) "А4х9 (297.00 x 1892.00 мм)")
                   ((= "А3х3" f_temp) "А3х3 (420.00 x 891.00 мм)")
                   ((= "А3х4" f_temp) "А3х4 (420.00 x 1189.00 мм)")
                   ((= "А3х5" f_temp) "А3х5 (420.00 x 1486.00 мм)")
                   ((= "А3х6" f_temp) "А3х6 (420.00 x 1783.00 мм)")
                   ((= "А3х7" f_temp) "А3х7 (420.00 x 2080.00 мм)")
                   ((= "А2х3" f_temp) "А2х3 (594.00 x 1261.00 мм)")
                   ((= "А2х4" f_temp) "А2х4 (594.00 x 1682.00 мм)")
                   ((= "А2х5" f_temp) "А2х5 (594.00 x 2102.00 мм)")
                   ((= "А1х3" f_temp) "А1х3 (841.00 x 1783.00 мм)")
                   ((= "А1х4" f_temp) "А1х4 (841.00 x 2378.00 мм)")
                   ((= "А0х2" f_temp) "А0х2 (1189.00 x 1682.00 мм)")
                   ((= "А0х3" f_temp) "А0х3 (1189.00 x 2523.00 мм)")
                   ((= "А0" f_temp) "А0 (841.00 x 1189.00 мм)")
                   ((= "А1" f_temp) "А1 (594.00 x 841.00 мм)")
                   ((= "А2" f_temp) "А2 (420.00 x 594.00 мм)")
                   ((= "А3" f_temp) "А3 (297.00 x 420.00 мм)")
                   ((= "А4" f_temp) "А4 (210.00 x 297.00 мм)")
                   (T nil)
                 )
    )
    ;;закрывает иначе
  )

  (if (/= format nil) 
    (progn 
      ;; Большой или малый
      (setq is-big (= 1 (cdr (assoc 290 (member '(301 . "First sheet") list1)))))

      ;; Шифр
      (setq shifr (cdr (assoc 300 (member '(301 . "Designation") list1))))
      (if (not shifr) 
        (setq shifr (cdr (assoc 300 (member '(301 . "Name") list1))))
      )
      (if shifr (setq shifr (vl-string-trim " " shifr)) (setq shifr ""))
      (if (and shifr (= (type shifr) 'STR)) (setq shifr (clear-mtext shifr)))

      ;; Fallback Шифр
      (if (or (= shifr "") (= shifr nil)) 
        (progn 
          (normal_points)
          (setq shifr (nameshifr is-big))
          (if (= shifr "") (setq shifr (get-shifr-from-blocks is-big)))
        )
      )

      ;; Название (только для большого штампа)
      (if is-big 
        (progn 
          (setq nameris "")
          (setq name1 (cdr (assoc 300 (member '(301 . "Drawing type") list1))))
          (setq name2 (cdr (assoc 300 (member '(301 . "Drawing type1") list1))))
          (setq name3 (cdr (assoc 300 (member '(301 . "Drawing type2") list1))))
          (if name1 (setq nameris (strcat nameris name1)))
          (if name2 (setq nameris (strcat nameris name2)))
          (if name3 (setq nameris (strcat nameris name3)))
          (setq nameris (vl-string-trim " " nameris))
          (if (and nameris (= (type nameris) 'STR)) 
            (setq nameris (clear-mtext nameris))
          )

          ;; Fallback Название
          (if (or (= nameris "") (= nameris nil)) 
            (progn 
              (normal_points)
              (setq nameris (namelist))
            )
          )
        )
        (setq nameris "")
      )

      ;; Fallback Номер (numa)
      (if (or (= numa nil) (= numa 0) (= numa "") (= numa "0")) 
        (progn 
          (normal_points)
          (setq numa (numar_s))
        )
      )


      (if (or (= numa nil) (= numa 0) (= numa "") (= numa "0")) 
        (if (/= druk_n nil) 
          (setq druk_n (cons (list 0 x1 x2 format mash poloz model nameris nil shifr) 
                             druk_n
                       )
          )
          (setq druk_n (list (list 0 x1 x2 format mash poloz model nameris nil shifr)))
        )
        (if (/= druk_v nil) 
          (setq druk_v (cons 
                         (list numa x1 x2 format mash poloz model nameris nil shifr)
                         druk_v
                       )
          )
          (setq druk_v (list 
                         (list numa x1 x2 format mash poloz model nameris nil shifr)
                       )
          )
        )
      )
    )
  )
)
;канец вызначэнне формата

;------------------------СПДС вызначэнне-------------------------------
(defun spds1 (/ x1 x2 format mash poloz model i1 i nabor len list1 list2) 
  (setq nabor (ssget "X"))
  (vl-load-com)
  (if (null nabor) 
    (progn 
      (princ "\nНе составлен список. ") ; сообщение об отсутствии
      (princ) ; тихий выход
    ) ; конец progn
    (progn 
      (setq i   -1
            len (sslength nabor)
      )
      (repeat len 
        (setq i (1+ i))
        ; Выбор следующего примитива и получение его списка
        (setq list1 (entget (ssname nabor i)))
        (if (= "SPDSFORMAT" (strcase (cdr (assoc 0 list1)))) 
          (progn 
            (setq list2 (vlax-ename->vla-object (ssname nabor i)))
            (vla-GetBoundingBox list2 'x1 'x2)
            (setq x1 (vlax-safearray->list x1))
            (setq x2 (vlax-safearray->list x2))

            (setq model (cdr (assoc 410 list1))) ;model-list

            (if (and (/= x1 nil) (/= x2 nil)) 
              (v_format_spds)
            )
          )
        ) ;);end if spdsFormat
      )
      ;;конец repeat
    ) ;конец progn
  )
  ;;конец if
)

;; Замена ВСЕХ вхождений подстроки (без зацикливания)
(defun vl-string-subst-all (str old new / pos)
  (setq pos 0)
  (while (setq pos (vl-string-search old str pos))
    (setq str (vl-string-subst new old str pos))
    (setq pos (+ pos (strlen new)))
  )
  str
)

;; Экранирование спецсимволов для строки file_all
(defun escape-delimiters (str / result)
  (if (or (= str nil) (= str "")) 
    ""
    (progn
      (setq result str)
      ;; Сначала экранируем сам escape-символ
      (setq result (vl-string-subst-all result "*" "*s"))
      ;; Потом остальные разделители
      (setq result (vl-string-subst-all result "&" "*a"))
      (setq result (vl-string-subst-all result "?" "*q"))
      (setq result (vl-string-subst-all result ">" "*g"))
      (setq result (vl-string-subst-all result "<" "*l"))
      result
    )
  )
)

;---------------------print_s-------------------
(defun print_s (spis / x1 x2 format mash poloz model temp_lm list_n fileuser fi0 cmd 
                nameris ctb_file shifr entry
               ) 
  ;

  (setq temp_lm (getvar "ctab")) ;атрыманне ліста або мадэлі дзе знаходзіца карыстальнік
  (setq list_n (car spis))
  (if (/= list_n nil) (setq list_n1 list_n))
  (setq spis (cdr spis))
  (setq x1 (nth 0 spis))
  (setq x2 (nth 1 spis))
  (setq format (nth 2 spis))
  (setq mash (nth 3 spis))
  (setq poloz (nth 4 spis))
  (setq model (nth 5 spis))
  (setq nameris (nth 6 spis)) ;назва рысынка, у "наступных" не вызначаецца
  (setq ctb_file (if (or (= acad_color 1) (= (nth 7 spis) T)) 
                   "acad.ctb"
                   "monochrome.ctb"
                 )
  )

  (setvar "ctab" model) ;пераход на патрэбны ліст або мадель
  (setq x1 (trans x1 0 1))
  (setq x2 (trans x2 0 1))

  (if (and (/= format nil) (/= (GETVAR "LOCALE") "ENG")) 
    (progn 
      ;имя рысунка
      (if (/= list_n nil) 
        (SETQ fileuser (STRCAT 
                         (SUBSTR (GETVAR "dwgname") 
                                 1
                                 (- (STRLEN (GETVAR "dwgname")) 4)
                         )
                         "_"
                         list_n
                       )
        )
        (SETQ fileuser (SUBSTR (GETVAR "dwgname") 
                               1
                               (- (STRLEN (GETVAR "dwgname")) 4)
                       )
        )
      )
      (setq shifr (nth 8 spis))
      (if (= shifr nil) (setq shifr ""))
      (setq orig_fileuser fileuser)
      (setq current_fileuser orig_fileuser)
      (setq fileuser_path (strcat (getvar "dwgprefix") current_fileuser ".pdf"))

      ; --- Цикл проверки блокировки файла ---
      (setq counter -1)
      (setq fi0 (open fileuser_path "w"))
      (while (and (= fi0 nil) (< counter 9)) 
        (setq counter (1+ counter))
        (setq current_fileuser (strcat orig_fileuser "-" (itoa counter)))
        (setq fileuser_path (strcat (getvar "dwgprefix") current_fileuser ".pdf"))
        (setq fi0 (open fileuser_path "w"))
      )

      (setq fileuser fileuser_path) ; Полный путь для дальнейшей работы

      ; --- Формируем строку для слияния с актуальным (возможно измененным) именем ---
      (setq entry (strcat current_fileuser 
                          ".pdf"
                          ">"
                          (escape-delimiters shifr)
                          ">"
                          (escape-delimiters (if nameris nameris ""))
                          ">"
                          (escape-delimiters (if list_n1 list_n1 ""))
                  )
      )
      (if (equal f_temp "") 
        (setq f_temp entry)
        (setq f_temp (strcat f_temp "?" entry))
      )

      (if (= fi0 nil) 
        (progn 
          (print 
            "Файл ужо адчыненны іншай праграмай (даже с суффиксами 0-9),\nабо немагчыма запісаць у гэты каталог"
          )
          (princ) ; тихий выход
        ) ;канец прогона
        (progn 
          (close fi0)
          (if (or (wcmatch format "А0х2*") (wcmatch format "А0х3*")) 
            (alert "Даннй формат(А0х2 или А0х2) будет перевен в pdf,\n но технически его невозможно будет распечатать")
          )
          (vl-file-delete fileuser)
          (setq cmd (GETVAR "cmdecho"))
          (SETVAR "cmdecho" 0)
            (if (/= model "Model") 
              (command "_plot" ;Сама команда
                       "_y" ;Выполнить детальное задание конфигурации?:
                       "" ;Имя листа или <печать>:
                       "_DWG в PDF ЭТО(Silent).pc3" ;Имя устройства вывода:
                       format ;Формат листа бумаги
                       "_Millimeters" 
                       ;;Единицы измерения размеров листа :
                       poloz 
                       ;;Ориентация чертежа [Книжная/Альбомная]:
                       "_N" 
                       ;;Перевернуть чертеж? :
                       "Рамка" 
                       ;;Печатаемая область [Экран/Границы/Лист/Вид/Рамка]:
                       x1 
                       ;;першая коордыната
                       x2 
                       ;;другая коордяната
                       mash 
                       ;;Масштаб печати :
                       "_center" 
                       ;;Смещение от начала(x,y)или[Центрировать]:
                       "_y" 
                       ;;Учитывать стили печати? [Да/Нет]:
                       ctb_file 
                       ;;Имя таблицы стилей печати:
                       "_y" 
                       ;;Учитывать веса линий? [Да/Нет]:
                       "_N" 
                       ;;Масштабировать веса линий?:
                       "_N" 
                       ;;Печатать объекты листа первыми?:
                       "_N" 
                       ;;Скрывать объекты листа?:
                       fileuser 
                       ;;Введите имя файла :
                       "_n" 
                       ;;Сохранить изменения параметров листа
                       "_y" 
                       ;;Перейти к печати [Да/Нет]
              )
              ;;калі модель
              (command "_plot" ;Сама команда
                       "_y" ;Выполнить детальное задание конфигурации?:
                       "" ;Имя листа или <печать>:
                       "_DWG в PDF ЭТО(Silent).pc3" ;Имя устройства вывода:
                       format ;Формат листа бумаги
                       "_Millimeters" 
                       ;;Единицы измерения размеров листа :
                       poloz 
                       ;;Ориентация чертежа [Книжная/Альбомная]:
                       "_N" 
                       ;;Перевернуть чертеж? :
                       "Рамка" 
                       ;;Печатаемая область [Экран/Границы/Лист/Вид/Рамка]:
                       x1 
                       ;;першая коордыната
                       x2 
                       ;;другая коордяната
                       mash 
                       ;;Масштаб печати :
                       "_center" 
                       ;;Смещение от начала(x,y)или[Центрировать]:
                       "_y" 
                       ;;Учитывать стили печати? [Да/Нет]:
                       ctb_file 
                       ;;Имя таблицы стилей печати:
                       "_y" 
                       ;;Учитывать веса линий? [Да/Нет]:
                       "_N" 
                       ;;Режим вывода тонированных ВЭ:
                       fileuser 
                       ;;Введите имя файла :
                       "_n" 
                       ;;Сохранить изменения параметров листа
                       "_y" 
                       ;;Перейти к печати [Да/Нет]
              )
            )
          (SETVAR "cmdecho" cmd)

          ; Запись диагностики для вывода в консоль
          (setq cur_diag (vl-bb-ref 'current_file_diagnostics))
          (setq diag_item (list (strcat (vl-filename-base fileuser) ".pdf") 
                                nameris
                                shifr
                                list_n1
                                (if (= ctb_file "acad.ctb") "Цвет" "Ч/Б")
                                format
                          )
          )
          (vl-bb-set 'current_file_diagnostics (cons diag_item cur_diag))
        )
      )
    )
  ) ;канец ифа

  (setvar "ctab" temp_lm) ;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
);канец принта


;------------------------ПЕЧАТЬ---------------------------------
(defun peshat (/ list_n1) 
  (if (and (/= druk_v nil) (= (length druk_v) 1)) 
    (progn 
      (if (= (setq list_n1 (caar druk_v)) nil) 
        (setq list_n1 "")
      )
      (setq druk_v (list (cons nil (cdr (car druk_v)))))
    )
  )
  (if (/= druk_v nil) 
    (if (/= (last druk_v) nil) 
      (progn 
        (while (and (/= druk_v nil) (/= (last druk_v) nil)) 
          (print_s (car druk_v))
          (setq druk_v (cdr druk_v))
        ) ;end while
        (if (and (/= druk_v nil) (= (last druk_v) nil)) 
          (progn 
            (print_s druk_v)
            (setq druk_v nil)
          )
        )
      ) ;end progn
      (print_s druk_v)
    ) ;end if
  ) ;end if
)

;------------------------параунанне каардынат--------------------------
(defun kordinat (x1 x2 x4 x5 / xtemp) 

  ;нармалізація групп каардынат
  (if (and (= 3 (length x1)) (= 3 (length x2)) (/= 0 (length x1))) 
    (progn 
      (setq x1 (list (car x1) (nth 1 x1)))
      (setq x2 (list (car x2) (nth 1 x2)))
    )
  )

  (if (> (last x2) (last x1)) 
    (progn 
      (setq xtemp (last x1))
      (setq x1 (list (car x1) (last x2)))
      (setq x2 (list (car x2) xtemp))
    )
  )

  (if (and (= 3 (length x4)) (= 3 (length x5)) (/= 0 (length x4))) 
    (progn 
      (setq x4 (list (car x4) (nth 1 x4)))
      (setq x5 (list (car x5) (nth 1 x5)))
    )
  )

  (if (> (last x5) (last x4)) 
    (progn 
      (setq xtemp (last x4))
      (setq x4 (list (car x4) (last x5)))
      (setq x5 (list (car x5) xtemp))
    )
  )
  ;сканчэнне нармалізаціі

  (if 
    (and (equal (car x1) (car x4) 0.0001) 
         (equal (last x1) (last x4) 0.0001)
         (equal (car x2) (car x5) 0.0001)
         (equal (last x2) (last x5) 0.0001)
    )
    T
    nil
  )
)
;number list
(defun len (lst / q) 
  (if (AND (/= lst nil) (listp druk_n)) 
    (cond 
      ((null lst) 0)
      (t (+ 1 (len (cdr lst))))
    )
    (setq q 0)
  )
)

;------------------------знішчэнне дулікатау--------------------------
(defun del_dubl (/ temp_car temp1 temp2 temp3 druk1 druk2) 
  (princ 
    (strcat "\nКолькасць да знішчэнне дулікатаў рысункаў: " 
            (rtos (+ (len druk_n) (len druk_v)))
    )
  )
  ;апрацоука нявызначанных
  (while (/= nil druk_n) 
    (setq temp_car (car druk_n))
    (setq temp1 (cdr druk_n))
    (while (/= nil temp1) 
      (if 
        (and 
          (kordinat 
            (nth 1 temp_car)
            (nth 2 temp_car)
            (nth 1 (car temp1))
            (nth 2 (car temp1))
          )
          (= (nth 6 (car temp1)) (nth 6 temp_car))
        )
        (setq temp1 (cdr temp1))
        (progn 
          (if (= nil temp2) 
            (setq temp2 (list (car temp1)))
            (setq temp2 (append temp2 (list (car temp1))))
          ) ;end if
          (setq temp1 (cdr temp1))
        ) ;end progn
      )
    ) ;end pod while

    ;перанос каров в масів
    (if (= nil temp3) 
      (setq temp3 (list temp_car))
      (setq temp3 (append temp3 (list temp_car)))
    ) ;end if

    ;сброс значэннеу
    (setq druk_n temp2)
    (setq temp1 nil
          temp2 nil
    )
  ) ;end while
  (setq druk_n temp3)
  (setq temp1 nil
        temp2 nil
        temp3 nil
  )

  ;апрацоука вызначанных
  (while (/= nil druk_v) 
    (setq temp_car (car druk_v))
    (setq temp1 (cdr druk_v))
    (while (/= nil temp1) 
      (if 
        (and 
          (kordinat 
            (nth 1 temp_car)
            (nth 2 temp_car)
            (nth 1 (car temp1))
            (nth 2 (car temp1))
          )
          (= (nth 6 (car temp1)) (nth 6 temp_car))
        )
        (setq temp1 (cdr temp1))
        (progn 
          (if (= nil temp2) 
            (setq temp2 (list (car temp1)))
            (setq temp2 (append temp2 (list (car temp1))))
          ) ;end if
          (setq temp1 (cdr temp1))
        ) ;end progn
      )
    ) ;end pod while

    ;перанос каров в масів
    (if (= nil temp3) 
      (setq temp3 (list temp_car))
      (setq temp3 (append temp3 (list temp_car)))
    ) ;end if

    ;сброс значэннеу
    (setq druk_v temp2)
    (setq temp1 nil
          temp2 nil
    )
  ) ;end while
  (setq druk_v temp3)
  (setq temp1 nil
        temp2 nil
        temp3 nil
  )

  ;удаленіе еслі в ненумерованном спіске совпадает в нумерованным
  (setq druk1 druk_n
        druk2 druk_v
        bool  nil
  )
  (while (and (/= nil druk1) (/= nil druk2)) 
    (setq temp_car (car druk1))
    (setq temp1 druk2)
    (while (/= nil temp1) 
      (if 
        (and (= bool nil) 
             (kordinat 
               (nth 1 temp_car)
               (nth 2 temp_car)
               (nth 1 (car temp1))
               (nth 2 (car temp1))
             )
             (= (nth 6 (car temp1)) (nth 6 temp_car))
        )
        (setq bool T)
        (setq temp1 (cdr temp1))
      )
    ) ;end pod while

    (if (= bool nil) 
      ;перанос каров в масів
      (if (= nil temp3) 
        (setq temp3 (list temp_car))
        (setq temp3 (append temp3 (list temp_car)))
      ) ;end if
    )

    ;сброс значэннеу
    (setq druk1 (cdr druk1))
    (setq temp1 nil
          bool  nil
    )
  )

  (if (/= temp3 nil) 
    (setq druk_n temp3)
  )

  (princ 
    (strcat "\nКолькасць пасля знішчэнне дулікатаў рысункаў: " 
            (rtos (+ (len druk_n) (len druk_v)))
            "\n___________________________________________________________\n"
    )
  )
)


;;---------------------утварэнне списау druk_v druk_n----------------------
(defun utvar (numa0 / numa is-big shifr) 
  (normal_points)
  (if (and (/= numa0 nil) (/= numa0 0) (/= numa0 "0")) 
    (setq numa numa0)
    (setq numa (numar_s))
  )
  ;сканчэнне нармализации каардынат
  (if (and (OR (= nameris "") (= nameris nil)) (/= zapret_name T)) 
    (setq nameris (namelist))
  )

  (setq is-big (is-big-stamp-p))
  (if (not is-big) (setq nameris ""))
  (setq shifr "")
  (setq shifr (nameshifr is-big))
  (if (= shifr nil) (setq shifr ""))
  (if (= shifr "") 
    (progn 
      (setq shifr (get-shifr-from-blocks is-big))
      (if (= shifr nil) (setq shifr ""))
    )
  )

  ; Очистка текста от форматирования MTEXT
  (if (and nameris (= (type nameris) 'STR)) (setq nameris (clear-mtext nameris)))
  (if (and shifr (= (type shifr) 'STR)) (setq shifr (clear-mtext shifr)))
  (if (and numa (= (type numa) 'STR)) (setq numa (clear-mtext numa)))

  ; [ОТЛАДКА] Вывод диагностики
  (princ 
    (strcat "\n--- ДИАГНОСТИКА ШТАМПА ---" 
            "\nШтамп: "
            (if is-big "Большой" "Малый")
            "\nНазвание (nameris): "
            (if nameris nameris "nil")
            "\nШифр (shifr): "
            (if shifr shifr "nil")
            "\nНомер (numa): "
            (if numa (vl-princ-to-string numa) "nil")
            "\n--------------------------\n"
    )
  )

  (if (or (= numa nil) (= numa 0)) 
    (if (/= druk_n nil) 
      (setq druk_n (cons (list 0 x1 x2 format mash poloz model nameris nil shifr) 
                         druk_n
                   )
      )
      (setq druk_n (list (list 0 x1 x2 format mash poloz model nameris nil shifr)))
    ) ;end if
    (if (/= druk_v nil) 
      (setq druk_v (cons (list numa x1 x2 format mash poloz model nameris nil shifr) 
                         druk_v
                   )
      )
      (setq druk_v (list (list numa x1 x2 format mash poloz model nameris nil shifr)))
    ) ;end if
  )
)


;------------------------утварэнне спісау друку іх нумерацыя,ад дыялогу---------------------------------
(defun zapusk_druk (/ f_temp ris_temp color_indices temp_druk_v idx item old_cdr 
                    updated_cdr skip_print
                   ) 

  ;друк полилиний-блокау-спдс
  (poli)
  (blocks)
  (spds1)

  ;знішчэнне дублікатау

  (del_dubl)
  (prin_numar) ;прастаука нумароу вызначанага спису
  (zad_n) ;прастаука нумароу невызначанага спису

  (setq skip_print nil)
  (if (and (= acad_color 2) (/= druk_v nil)) 
    (progn 
      (if (> (length druk_v) 1) 
        (setq color_indices (ShowSheetColorDialog druk_v))
        (setq color_indices (list 0))
      )
      (if (= color_indices 'cancel) 
        (setq skip_print T)
        (progn 
          (setq temp_druk_v nil)
          (setq idx 0)
          (foreach item druk_v 
            (if (member idx color_indices) 
              (progn 
                (setq old_cdr (cdr item))
                (setq updated_cdr (list 
                                    (nth 0 old_cdr)
                                    (nth 1 old_cdr)
                                    (nth 2 old_cdr)
                                    (nth 3 old_cdr)
                                    (nth 4 old_cdr)
                                    (nth 5 old_cdr)
                                    (nth 6 old_cdr)
                                    T
                                  )
                )
                (setq temp_druk_v (cons (cons (car item) updated_cdr) temp_druk_v))
              )
              (setq temp_druk_v (cons item temp_druk_v))
            )
            (setq idx (1+ idx))
          )
          (setq druk_v (reverse temp_druk_v))
        )
      )
    )
  )

  (if (not skip_print) 
    (progn 
      (setq f_temp "")
      (peshat)

      (vl-bb-set 
        'file_all
        (strcat (vl-bb-ref 'file_all) "&" f_temp)
      )
    )
  )
  (princ)
);end function


;------------------------апрацоука  палей диалогу---------------------------------
(defun PrintAutoDcl (/ dcl_file file_handle dcl_id) 
  (setq dcl_file (vl-filename-mktemp "print_spds.dcl"))
  (setq file_handle (open dcl_file "w"))
  (write-line "prindcl: dialog{label=\"Автоматическая печать\";" file_handle)
  (write-line ": boxed_column { label = \"Настройка:\";" file_handle)
  (write-line ": radio_column {" file_handle)
  (write-line ": radio_button {key = \"Radio1\"; label = \"текущий чертеж\"; }" 
              file_handle
  )
  (write-line ": radio_button {key = \"Radio2\"; label = \"текущую папку чертежа\"; value = 1;}" 
              file_handle
  )
  (write-line "}" file_handle)
  (write-line "}" file_handle)
  (write-line ": popup_list {" file_handle)
  (write-line "    label = \"Цвет:\";" file_handle)
  (write-line "    key = \"color_select\";" file_handle)
  (write-line "    list = \"монохромный\\nвсе в цвете\\nзапрашивать цвет\";" 
              file_handle
  )
  (write-line "    value = \"0\";" file_handle)
  (write-line "  }" file_handle)
  (write-line ":popup_list{label=\"\";" file_handle)
  (write-line "list=\"не склеивать\\nсклеить по файлам\\nсклеить в общий файл\";value=\"2\";key=\"sfile_merge\";}" 
              file_handle
  )
  (write-line "ok_cancel;" file_handle)
  (write-line "}" file_handle)
  (close file_handle)
  (setq dcl_id (load_dialog dcl_file))
  (vl-file-delete dcl_file)
  dcl_id
)

(defun palja () 
  ;теукщий чертеж или папку
  (if (= (atoi (get_tile "Radio1")) 1) 
    (setq rys 1)
    (setq rys 0)
  )
  ;склеить как
  (if (= 0 (atoi (get_tile "sfile_merge"))) 
    (setq sfile     0
          sfile_all 0
    )
  )
  (if (= 1 (atoi (get_tile "sfile_merge"))) 
    (setq sfile     1
          sfile_all 0
    )
  )
  (if (= 2 (atoi (get_tile "sfile_merge"))) 
    (setq sfile     0
          sfile_all 1
    )
  )
  (setq acad_color (atoi (get_tile "color_select")))
)

;------------------------выклік діалогу---------------------------------
(defun c:dil_spds (/ dcl_id pdf rys ddi druk_n druk_v done file_all sfile_all sfile 
                   lik_open data stor data1 nameris acad_color old_ctab old_viewctr 
                   old_viewsize
                  )  ;numar
  (setq acad_color 0)
  (PRINC "\n-------------------------------------------------------------------------------------\n")
  (PRINC "Праграмма распрацавана на lisp, prajdziswet-ам (Косавам Уладзімірам) у 2014(ред.2026) годзе\n")
  (PRINC "Праграма прызначана для аўтаматычнай PDF файлаў, а таксама для друку\n")
  (PRINC "Праграма працуе з файламі СПДС, блоками, полірыскамі\n")
  (PRINC "-------------------------------------------------------------------------------------\n")
  ;----------------------------------------------------------------------
  ;вызначэнне версии акада-трошки недакладна
  (setq vers (substr (vl-bb-ref 'dirpol) 
                     (+ (vl-string-search "AUTODESK" (strcase (vl-bb-ref 'dirpol))) 
                        18
                     )
                     4
             )
  )

  (vl-load-com)
  (setq old_ctab (getvar "ctab"))
  (setq old_viewctr (getvar "viewctr"))
  (setq old_viewsize (getvar "viewsize"))
  (vl-bb-set 'lik_open 0)
  ;if auto
  (if (/= auto T) 
    (progn 
      (setq done nil)
      (setq dcl_id (PrintAutoDcl))
      (if (not (new_dialog "prindcl" dcl_id)) 
        (exit)
      )
      (action_tile "accept" "(palja)(done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq ddi (start_dialog))
      (unload_dialog dcl_id)
    )
    ;progn auto
    (progn 
      (setq rys       1
            sfile_all 1
            sfile     0
            ddi       1
      ) ;numar 1 pdf 1
    )
  ) ;end if auto

  (if (= ddi 1) 
    (progn 
      (vl-bb-set 'acad_color acad_color)
      ;усталека первічных параметрау
      (vl-bb-set 
        'file_all
        (strcat (GETVAR "dwgprefix") 
                "&"
                (if (equal sfile 1) "true" "false")
                "&"
                (if (equal sfile_all 1) "true" "false")
        )
      )
      (vl-bb-set 'file_ris nil) ;обнуление
      (if (or (/= rys 0) (peshat-spds-file-prepare)) 
        (progn 
          (vl-bb-set 'current_file_diagnostics nil) ; обнуляем перед началом печати активного
          (zapusk_druk)

          (princ (strcat "\nТэчка файлаў: " (getvar "dwgprefix")))
          (print_diagnostics_to_console (GETVAR "dwgname"))

          (if (= rys 0) 
            (peshat-spds-file-run)
          )

          (if (= needAdd nil) (setq needAdd 0))

          (if (OR (equal sfile_all 1) (equal sfile 1)) 
            ;адпраука файлау на сліяніе пдф
            (progn 
              (setq count_pdf 0)
              (setq temp_str (strcase (vl-bb-ref 'file_all)))
              (while (vl-string-search ".PDF" temp_str) 
                (setq count_pdf (1+ count_pdf))
                (setq temp_str (substr temp_str 
                                       (+ (vl-string-search ".PDF" temp_str) 5)
                               )
                )
              )
              (if (> count_pdf 1) 
                (progn 
                  ; Добавляем маркер нового формата в конец всей строки!
                  (setq final_str (strcat (vl-bb-ref 'file_all) "<"))
                  (princ 
                    (strcat "\n___________________________________________________________\n[ОТЛАДКА] Сформированная строка file_all:\n" 
                            final_str
                            "\n"
                    )
                  )
                  (startapp
                    ;"__prog\\exe\\WPFMergeExe\\WPFMergeExe.exe"
                    "e:\\praca-proect\\_program\\_Програм\\__скончаные\\__скончаные\\_Autocad\\WPFMergeExe\\WPFMergeExe\\bin\\Debug\\net48\\WPFMergeExe.exe"
                    (strcat "\"" final_str "\"")
                  )
                  (command)
                )
              )
            )
          )
        )
      )
    )
  ) ;end if
  (if old_ctab (setvar "ctab" old_ctab))
  (if (and old_viewctr old_viewsize) 
    (vl-cmdf "_zoom" "_C" old_viewctr old_viewsize)
  )
  (vl-bb-set 'lik_open nil)
  (princ)
)
;
(print "загружена автоматичская печать")
(princ)

(defun print_diagnostics_to_console (filename / diag_list) 
  (setq diag_list (vl-bb-ref 'current_file_diagnostics))
  (if diag_list 
    (progn 
      (princ (strcat "\n=== Обработан файл: " filename " ==="))
      (foreach diag (reverse diag_list) 
        (princ 
          (strcat "\n  PDF: " 
                  (nth 0 diag)
                  "\n  Назв: "
                  (if (nth 1 diag) (nth 1 diag) "nil")
                  " | Шифр: "
                  (if (nth 2 diag) (nth 2 diag) "nil")
                  " | Лист: "
                  (if (nth 3 diag) (vl-princ-to-string (nth 3 diag)) "nil")
                  " | "
                  (if (nth 4 diag) (nth 4 diag) "")
                  " | "
                  (if (nth 5 diag) (nth 5 diag) "")
          )
        )
      )
      (vl-bb-set 'current_file_diagnostics nil)
    )
  )
  (princ)
)

(defun peshat-spds-file-prepare (/ peshat-files result selected_files color_mode) 
  ;перадача аргументау дыялогу
  (setq peshat-files (vl-directory-files (GETVAR "dwgprefix") "*.dwg" 1)) ;выбар файлау

  (if (and (= rys 0) (= acad_color 2) (> (length peshat-files) 1)) 
    (progn 
      (setq result (ShowFileListDialog peshat-files))
      (if result 
        (progn 
          (setq selected_files (car result))
          (setq color_mode (cdr result))
          (vl-bb-set 'folder_color_mode color_mode)
          (vl-bb-set 'folder_color_files selected_files)
          (if (member (GETVAR "dwgname") selected_files) 
            (setq acad_color color_mode)
            (setq acad_color 0)
          )
          T
        )
        nil
      )
    )
    (progn 
      (vl-bb-set 'folder_color_mode acad_color)
      (vl-bb-set 'folder_color_files peshat-files)
      T
    )
  )
)


(defun peshat-spds-file-run (/ peshat-files peshat-file selected_files color_mode 
                             current_color
                            ) 
  (vl-bb-set 'current_file_diagnostics nil) ; Обнуляем перед запуском пакета
  (setq peshat-files (vl-directory-files (GETVAR "dwgprefix") "*.dwg" 1)) ;выбар файлау
  (setq selected_files (vl-bb-ref 'folder_color_files))
  (setq color_mode (vl-bb-ref 'folder_color_mode))
  (if (= color_mode nil) (setq color_mode acad_color))

  (if peshat-files 
    (progn 
      (while (/= peshat-files nil) 
        (if (/= (GETVAR "dwgname") (car peshat-files)) 
          (progn 
            (if (member (car peshat-files) selected_files) 
              (setq current_color color_mode)
              (setq current_color 0)
            )
            (vl-bb-set 'acad_color current_color)
            (vl-bb-set 'directory-p 1) ;запуск у іншых адчыняем дакумент
            (setq current_dwg_name (car peshat-files))
            (setq peshat-file (strcat (GETVAR "dwgprefix") current_dwg_name)) ;адчыняемы файл
            (setq peshat-files (cdr peshat-files)) ;обрезка
            (setq peshat-file (vla-Open 
                                (vla-get-Documents (vlax-get-acad-object))
                                peshat-file
                                :flax-true
                                ""
                              )
            ) ;адчыненне
            (vla-Close peshat-file :vlax-false) ;зачыненне

            (print_diagnostics_to_console current_dwg_name)
          ) ;end progn
          (setq peshat-files (cdr peshat-files)) ;обрезка
        )
      ) ;end while
      (vl-bb-set 'directory-p nil) ;забарона друку у іншых файлах
    )
  )
)

;------------------------выклік діалогу з рэдактара---------------------------------
;stor убрана, оставлена для совемстимости верстий
(defun dil_spds_new (needAdd stor / auto) 
  (setq auto T)
  (c:dil_spds)
)


;============================================================================
;------------------------друк у іншых файлах---------------------------------
;============================================================================
(defun open-p ()  ;numar
  ;задаванне параметрау выбару
  (if (= (vl-bb-ref 'directory-p) 1) 
    (progn 
      (vl-bb-set 'directory-p nil)
      (if (vl-bb-ref 'acad_color) 
        (setq acad_color (vl-bb-ref 'acad_color))
      )
      ;друк
      (zapusk_druk)
    )
  )
)
(defun ShowSheetColorDialog (sheets / dcl_file file_handle dcl_id result 
                             selected_indices selected_sheets read_list index item 
                             display_list name
                            ) 
  (setq dcl_file (vl-filename-mktemp "sheet_color.dcl"))
  (setq file_handle (open dcl_file "w"))
  (write-line "sheet_color_dcl : dialog {" file_handle)
  (write-line "  label = \"Печать в цвете\";" file_handle)
  (write-line "  : text { label = \"Выберите чертежи для печати в цвете:\"; }" 
              file_handle
  )
  (write-line "  : text { label = \"* Чертежи, которые НЕ выделены в списке, распечатаются в монохроме.\"; }" 
              file_handle
  )
  (write-line "  : text { label = \"* Подсказка: используйте Ctrl/Shift (+мышь или ↑ ↓) для выделения нескольких чертежей.\"; }" 
              file_handle
  )
  (write-line "  : list_box {" file_handle)
  (write-line "    key = \"sheets_list\";" file_handle)
  (write-line "    width = 60;" file_handle)
  (write-line "    height = 15;" file_handle)
  (write-line "    multiple_select = true;" file_handle)
  (write-line "  }" file_handle)
  (write-line "  ok_cancel;" file_handle)
  (write-line "}" file_handle)
  (close file_handle)

  (setq display_list nil)
  (foreach item sheets 
    (setq name (nth 6 (cdr item)))
    (if (and name (/= name "")) 
      (setq display_list (cons (strcat (car item) " - " name) display_list))
      (setq display_list (cons (car item) display_list))
    )
  )
  (setq display_list (reverse display_list))

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "sheet_color_dcl" dcl_id)) 
    (setq result 0)
    (progn 
      (start_list "sheets_list")
      (foreach item display_list 
        (add_list item)
      )
      (end_list)
      (action_tile "accept" 
                   "(setq selected_indices (get_tile \"sheets_list\")) (done_dialog 1)"
      )
      (action_tile "cancel" "(done_dialog 0)")
      (setq result (start_dialog))
      (unload_dialog dcl_id)
      (vl-file-delete dcl_file)
    )
  )
  (if (= result 1) 
    (if (and selected_indices (/= selected_indices "")) 
      (read (strcat "(" selected_indices ")"))
      nil
    )
    'cancel
  )
)


(defun ShowFileListDialog (file_list / dcl_file file_handle dcl_id result 
                           selected_indices selected_files read_list color_mode
                          ) 
  (setq dcl_file (vl-filename-mktemp "file_list.dcl"))
  (setq file_handle (open dcl_file "w"))
  (write-line "file_list_dcl : dialog {" file_handle)
  (write-line "  label = \"Список файлов для печати\";" file_handle)
  (write-line "  : boxed_column { label = \"Режим цветной печати (для выбранных файлов):\";" 
              file_handle
  )
  (write-line "    : radio_column {" file_handle)
  (write-line "      : radio_button {key = \"file_color_all\"; label = \"печать всех чертежей в файле/-ах в цвете\"; value = 1;}" 
              file_handle
  )
  (write-line "      : radio_button {key = \"file_color_ask\"; label = \"запросить конкретные чертежи в файле/ах\";}" 
              file_handle
  )
  (write-line "    }" file_handle)
  (write-line "  }" file_handle)
  (write-line "  : text { label = \"Выберите файлы для цветной печати:\"; }" 
              file_handle
  )
  (write-line "  : text { label = \"* Файлы, которые НЕ выделены в списке, распечатаются в монохроме.\"; }" 
              file_handle
  )
  (write-line "  : text { label = \"* Подсказка: используйте Ctrl/Shift (+мышь или ↑ ↓) для выделения нескольких файлов.\"; }" 
              file_handle
  )
  (write-line "  : list_box {" file_handle)
  (write-line "    key = \"files_list\";" file_handle)
  (write-line "    width = 50;" file_handle)
  (write-line "    height = 12;" file_handle)
  (write-line "    multiple_select = true;" file_handle)
  (write-line "  }" file_handle)
  (write-line "  ok_cancel;" file_handle)
  (write-line "}" file_handle)
  (close file_handle)

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "file_list_dcl" dcl_id)) 
    (setq result 0)
    (progn 
      (start_list "files_list")
      (foreach item file_list 
        (add_list item)
      )
      (end_list)
      (action_tile "accept" 
                   "(setq selected_indices (get_tile \"files_list\")) (setq color_mode (if (= (atoi (get_tile \"file_color_all\")) 1) 1 2)) (done_dialog 1)"
      )
      (action_tile "cancel" "(done_dialog 0)")
      (setq result (start_dialog))
      (unload_dialog dcl_id)
      (vl-file-delete dcl_file)
    )
  )
  (if (= result 1) 
    (progn 
      (setq selected_files nil)
      (if (and selected_indices (/= selected_indices "")) 
        (progn 
          (setq read_list (read (strcat "(" selected_indices ")")))
          (foreach idx read_list 
            (setq selected_files (cons (nth idx file_list) selected_files))
          )
          (setq selected_files (reverse selected_files))
        )
      )
      (cons selected_files color_mode)
    )
    nil
  )
)


(defun peshat-spds-file (/ peshat-files peshat-file files_to_print) 
  ;перадача аргументау дыялогу
  (setq peshat-files (vl-directory-files (GETVAR "dwgprefix") "*.dwg" 1)) ;выбар файлау

  (if (and (= rys 0) (= acad_color 2) (> (length peshat-files) 1)) 
    (setq files_to_print (ShowFileListDialog peshat-files))
    (setq files_to_print peshat-files)
  )

  (if files_to_print 
    (progn 
      (while (/= files_to_print nil) 
        (if (/= (GETVAR "dwgname") (car files_to_print)) 
          (progn 
            (vl-bb-set 'directory-p 1) ;запуск у іншых адчыняем дакумент
            (setq peshat-file (strcat (GETVAR "dwgprefix") (car files_to_print))) ;адчыняемы файл
            (setq files_to_print (cdr files_to_print)) ;обрезка
            (setq peshat-file (vla-Open 
                                (vla-get-Documents (vlax-get-acad-object))
                                peshat-file
                                :flax-true
                                ""
                              )
            ) ;адчыненне
            (vla-Close peshat-file :vlax-false) ;зачыненне
          ) ;end progn
          (setq files_to_print (cdr files_to_print)) ;обрезка
        )
      ) ;end while and if
      (vl-bb-set 'directory-p nil) ;забарона друку у іншых файлах
    )
  )
)					;--

					;запуск в открываемых файлах
(if (/= (vl-bb-ref 'directory-p) nil) 
  (open-p)
)
