;вяртанне назову рысунка
(defun namelist (/ x_temp1 x_temp2 nabor_s temp_all text n len i text temp_lm) 
  (setq x_temp1 (list (- (car x2) (/ 125 mash)) (+ (cadr x2) (/ 20 mash))))
  (setq x_temp2 (list (- (car x2) (/ 55 mash)) (+ (cadr x2) (/ 5 mash))))


  (setq text "")
  (setq temp_lm (getvar "ctab"))
  (if (and model (/= model temp_lm)) (setvar "ctab" model))
  (vl-cmdf "_zoom" "_W" x_temp1 x_temp2) ;зумаванне акна нумара
  (setq nabor_s (ssget "_W" x_temp1 x_temp2 '((0 . "*EXT"))))
  (if (and (/= nil nabor_s) (= (sslength nabor_s) 1)) 
    ;progn
    (progn 
      (setq temp_all (entget (ssname nabor_s 0)))
      (if (= (cdr (assoc 0 temp_all)) "MTEXT") 
        (progn 
          (command "_.Explode" (ssname nabor_s 0)) ;получение текста
          (setq temp_all (ssget "_p" '((0 . "TEXT"))))
          (setq i   0
                len (sslength temp_all)
          )
          (while (< i len) 
            (setq text (strcat text (cdr (assoc 1 (entget (ssname temp_all i))))))
            (setq i (+ 1 i))
          )

          (command "_u")
        )
        (if (= (cdr (assoc 0 temp_all)) "TEXT") 
          (setq text (strcat text (cdr (assoc 1 temp_all))))
        )
      ) ;end if
    ) ;end progn
    (if (and (/= nil nabor_s) (> (sslength nabor_s) 1)) 
      (progn 
        (setq i   0
              len (sslength nabor_s)
        )
        (while (< i len) 
          (setq text (strcat text (cdr (assoc 1 (entget (ssname nabor_s i))))))
          (setq i (+ 1 i))
        )
      )
    )
  ) ;end if
  (command "_u")
  (if (and model (/= model temp_lm)) (setvar "ctab" temp_lm))
  (if (= text nil) (setq text ""))
  (setq text text)
);end defun

;вяртанне шифру
(defun nameshifr (temp / x_temp1 x_temp2 nabor_s temp_all text n len i text temp_lm)  ;temp nil-верхняя шапка, инакш-нижняя
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
  (setq temp_lm (getvar "ctab"))
  (if (and model (/= model temp_lm)) (setvar "ctab" model))

  (princ 
    (strcat "\n[nameshifr] Зона поиска шифра: dX: " 
            (rtos (- (car x_temp1) (car x2)) 2 2)
            ".."
            (rtos (- (car x_temp2) (car x2)) 2 2)
            ", dY: "
            (rtos (- (cadr x_temp1) (cadr x2)) 2 2)
            ".."
            (rtos (- (cadr x_temp2) (cadr x2)) 2 2)
    )
  )
  ;(getstring "\nНажмите Пробел или Enter, чтобы продолжить (посмотрите на зеленую рамку поиска шифра)...")

  (setq nabor_s (ssget "_C" 
                       (trans x_temp1 0 1)
                       (trans x_temp2 0 1)
                       '((0 . "*EXT"))
                )
  )

  (if (and (/= nil nabor_s) (= (sslength nabor_s) 1)) 
    ;progn
    (progn 
      (setq temp_all (entget (ssname nabor_s 0)))
      (if (= (cdr (assoc 0 temp_all)) "MTEXT") 
        (progn 
          (command "_.Explode" (ssname nabor_s 0)) ;получение текста
          (setq temp_all (ssget "_p" '((0 . "TEXT"))))
          (setq i   0
                len (sslength temp_all)
          )
          (while (< i len) 
            (setq text (strcat text (cdr (assoc 1 (entget (ssname temp_all i))))))
            (setq i (+ 1 i))
          )

          (command "_u")
        )
        (if (= (cdr (assoc 0 temp_all)) "TEXT") 
          (setq text (strcat text (cdr (assoc 1 temp_all))))
        )
      ) ;end if
    ) ;end progn
    (if (and (/= nil nabor_s) (> (sslength nabor_s) 1)) 
      (progn 
        (setq i   0
              len (sslength nabor_s)
        )
        (while (< i len) 
          (setq text (strcat text (cdr (assoc 1 (entget (ssname nabor_s i))))))
          (setq i (+ 1 i))
        )
      )
    )
  ) ;end if
  (command "_u")
  (if (and model (/= model temp_lm)) (setvar "ctab" temp_lm))
  (setq text text)
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


  ; Секущая рамка — захватываем ВСЁ что пересекает зону (TEXT, MTEXT, INSERT)
  (setq nabor_s (ssget "_C" 
                       (trans det-min 0 1)
                       (trans det-max 0 1)
                       '((0 . "TEXT,MTEXT,INSERT"))
                )
  )

  ;(command "_u")

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
                         (and (assoc 60 cur-data) (= (cdr (assoc 60 cur-data)) 1))
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
                         (setq wy (+ ins-y (* sx lx (sin rot)) (* sy ly (cos rot))))
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
                              tag
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
            (if 
              (and (< bminX smaxX) (> bmaxX sminX) (< bminY smaxY) (> bmaxY sminY))
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
                        (setq x1t (nth 0 pt)
                              y1t (nth 1 pt)
                        )
                        (setq x2t (nth 0 x2)
                              y2t (nth 1 x2)
                        )

                        (setq in-zone nil)
                        (if is-big 
                          ; Зона большого штампа (шифр наверху)
                          (setq in-zone (and (< (- x2t (* mash 125)) x1t) 
                                             (> (- x2t (* mash 5)) x1t)
                                             (> y1t (+ (* mash 50) y2t))
                                             (< y1t (+ (* mash 60) y2t))
                                        )
                          )
                          ; Зона малого штампа
                          (setq in-zone (and (< (- x2t (* mash 125)) x1t) 
                                             (> (- x2t (* mash 15)) x1t)
                                             (> y1t (+ (* mash 5) y2t))
                                             (< y1t (+ (* mash 20) y2t))
                                        )
                          )
                        )
                        (if in-zone 
                          (setq shifr-found (vla-get-TextString tag))
                        )
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

  ; Если имя или номер не найдены в атрибутах, попробуем взорвать копию блока и найти обычный текст
  (if (or (= numa nil) (= nameris nil)) 
    (progn 
      (setq copy_obj (vla-Copy vla-nameobj))
      (setq catchit (vl-catch-all-apply 'vla-Explode (list copy_obj)))
      (if (not (vl-catch-all-error-p catchit)) 
        (progn 
          (setq exploded_objs (vlax-safearray->list (vlax-variant-value catchit)))
          (foreach tag exploded_objs 
            (if 
              (or (= (vla-get-ObjectName tag) "AcDbText") 
                  (= (vla-get-ObjectName tag) "AcDbMText")
              )
              (progn 
                (setq catchbox (vl-catch-all-apply 'vla-GetBoundingBox 
                                                   (list tag 'minpt 'maxpt)
                               )
                )
                (if (not (vl-catch-all-error-p catchbox)) 
                  (progn 
                    (setq point (vlax-safearray->list minpt))
                    (setq x1temp (nth 0 point)
                          y1temp (nth 1 point)
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
                       (setq numa (vla-get-TextString tag))
                      )
                      ((and (= numa nil) 
                            (< (- x2temp (* mash 15)) x1temp)
                            (> (- x2temp (* mash 5)) x1temp)
                            (> y1temp (+ (* mash 5) y2temp))
                            (< y1temp (+ (* mash 13) y2temp))
                       )
                       (setq numa (vla-get-TextString tag))
                      )
                      ((and (= nameris nil) 
                            (< (- x2temp (* mash 125)) x1temp)
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
            (vla-Delete tag)
          )
        )
      )
      (vla-Delete copy_obj)
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


;выхад з пдф
(defun exit_pdf () 
  (startapp "__prog\\exe\\pdf_exit.exe")
  (command)
);выхад з пдф




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
  (setq str (vl-string-trim " " str))
  (if 
    (wcmatch str 
             "#,##,###,#`.#,##`.#,###`.#,#`.##,##`.##,###`.##,#`.###,##`.###,###`.###"
    )
    (princ str)
    (princ nil)
  ) ;end if
)


;;---------------------вызначэнне № старонки----------------------
(defun numar_s (/ x_temp1 x_temp2 nabor_s xtemp temp_lm) 
  (setq temp_lm (getvar "ctab")) ;атрыманне ліста або мадэлі дзе знаходзіца карыстальнік

  (setq zapret_nomer nil)

  (setq x_temp1 (list (- (car x2) (/ 40 mash)) (+ (last x2) (/ 30 mash))))
  (setq x_temp2 (list (- (car x2) (/ 25 mash)) (+ (last x2) (/ 20 mash))))
  (if (and model (/= model temp_lm)) 
    (setvar "ctab" model) ;пераход на патрэбны ліст або мадель
  )
  (vl-cmdf "_zoom" "_W" x_temp1 x_temp2) ;зумаванне акна нумара
  (setq nabor_s (ssget "_C" x_temp1 x_temp2 '((0 . "*EXT"))))
  (command "_u")
  ;(vl-cmdf "_zoom" "_p" x_temp1 x_temp2) ;вяртанне зумавання
  (if (and model (/= model temp_lm)) 
    (setvar "ctab" temp_lm) ;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
  )

  (if (and (/= nabor_s nil) (= (sslength nabor_s) 1)) 
    (norma_n)
    (progn 
      ;праверка наяунасти "листов"----------------------------
      (setq x_temp1 (list (- (car x2) (/ 25 mash)) 
                          (+ (last x2) (/ 30 mash))
                    )
      )
      (setq x_temp2 (list (- (car x2) (/ 5 mash)) (+ (last x2) (/ 20 mash))))
      (if (and model (/= model temp_lm)) 
        (setvar "ctab" model) ;пераход на патрэбны ліст або мадель
      )
      (vl-cmdf "_zoom" "_W" x_temp1 x_temp2) ;зумаванне акна нумара
      (setq nabor_s (ssget "_C" x_temp1 x_temp2 '((0 . "*EXT"))))
      (command "_u")
      ;(vl-cmdf "_zoom" "_p" x_temp1 x_temp2) ;вяртанне зумавання
      (if (and model (/= model temp_lm)) 
        (setvar "ctab" temp_lm) ;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
      )

      (if (and (/= nabor_s nil) (= (sslength nabor_s) 1)) 
        (setq zapret_nomer (norma_n))
      )
      ;---------------сканчэнне листов-------------------------
      (if (= nil zapret_nomer) 
        (progn 
          ;калі малы штамп
          (setq zapret_name T)

          (setq x_temp1 (list (- (car x2) (/ 15 mash)) 
                              (+ (last x2) (/ 13 mash))
                        )
          )
          (setq x_temp2 (list (- (car x2) (/ 5 mash)) (+ (last x2) (/ 5 mash))))

          (if (and model (/= model temp_lm)) 
            (setvar "ctab" model) ;пераход на патрэбны ліст або мадель
          )
          (vl-cmdf "_zoom" "_W" x_temp1 x_temp2) ;зумаванне акна нумара
          (setq nabor_s (ssget "_W" x_temp1 x_temp2 '((0 . "*ext"))))
          (command "_u")
          ;(vl-cmdf "_zoom" "_p" x_temp1 x_temp2) ;вяртанне зумавання
          (if (and model (/= model temp_lm)) 
            (setvar "ctab" temp_lm) ;пераход на папярэдні ліст дзе знаходзіуся карыстальнік
          )

          (if (and (/= nabor_s nil) (= (sslength nabor_s) 1)) 
            (setq zapret_nomer (norma_n))
          ) ;end if
        )
        (princ zapret_nomer)
      ) ;end if с запретом zapret_nomer-номер листов
    ) ;end progn
  ) ;end if
)					;канец вызначэнне старонки

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
  (print nabor_blocks)
  (princ)
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
(defun v_format_spds (/ i z f_temp numa) 

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
  (setq i 0
        z 0
  )
  (while (< i 10) 
    (if (= "Info" (cdr (nth (+ 10 z) list1))) 
      (progn 
        (setq i 10)
        (setq z (1+ z))
      )
      (progn 
        (setq z (1+ z))
        (setq i (1+ i))
      )
    )
  ) ;end while

  (if (/= z 10) 
    (progn 
      (setq f_temp (vl-string-trim " " (cdr (nth (+ 10 z) list1))))
      (setq f_temp (substr f_temp 
                           (+ (vl-string-search "\t" f_temp) 2)
                           (strlen f_temp)
                   )
      )
      (setq f_temp (substr f_temp 
                           (+ (vl-string-search "\t" f_temp) 2)
                           (strlen f_temp)
                   )
      )
      ;вызначэнне нумару спдс
      (setq numa (norma_n2 
                   (substr f_temp (+ (vl-string-search "\t" f_temp) 2) 10)
                 )
      )
      (setq f_temp (substr f_temp 1 (vl-string-search "\n" f_temp)))
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
    (utvar numa)
  )
)
;канец вызначэнне формата

;------------------------СПДС вызначэнне-------------------------------
(defun spds1 (/ x1 x2 format mash poloz model i1 i nabor len list1 list2 name1 name2 
              name3
             ) 
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
            (setq nameris     nil
                  zapret_name nil
            )
            (setq list2 (vlax-ename->vla-object (ssname nabor i)))
            (vla-GetBoundingBox list2 'x1 'x2)
            (setq x1 (vlax-safearray->list x1))
            (setq x2 (vlax-safearray->list x2))

            (setq model (cdr (assoc 410 list1))) ;model-list
            (if (<= (length list1) 95) 
              (if (/= (vl-position (cons 301 "Drawing type") list1) nil) 
                (setq zapret_name T
                      nameris     nil
                )
                (setq zapret_name nil
                      nameris     nil
                )
              )
              (progn 
                (setq name1 (cdr 
                              (nth 
                                (+ 1 (vl-position (cons 301 "Drawing type") list1))
                                list1
                              )
                            )
                )
                (if (/= nil (vl-position (cons 301 "Drawing type1") list1)) 
                  (setq name2 (cdr 
                                (nth 
                                  (+ 1 
                                     (vl-position (cons 301 "Drawing type1") list1)
                                  )
                                  list1
                                )
                              )
                  )
                )
                (if (/= nil (vl-position (cons 301 "Drawing type1") list1)) 
                  (setq name3 (cdr 
                                (nth 
                                  (+ 1 
                                     (vl-position (cons 301 "Drawing type2") list1)
                                  )
                                  list1
                                )
                              )
                  )
                )
                (if (and (/= name2 nil) (/= name3 nil)) 
                  (setq nameris (vl-string-trim " " (strcat name1 name2 name3)))
                  (setq nameris (vl-string-trim " " name1))
                )
              )
            ) ;nd progn if
            ;вызначэнне назову рысынка

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

;---------------------print_s-------------------
(defun print_s (spis / x1 x2 format mash poloz model temp_lm list_n fileuser fi0 cmd 
                nameris ctb_file
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
      ; Формируем: имяфайла.pdf>шифр>название>номер
      (setq entry (strcat fileuser 
                          ".pdf"
                          ">"
                          shifr
                          ">"
                          (if nameris nameris "")
                          ">"
                          (if list_n1 list_n1 "")
                  )
      )
      (if (equal f_temp "") 
        (setq f_temp entry)
        (setq f_temp (strcat f_temp "?" entry))
      )
      ;утварэнне пути с файлом
      (SETQ fileuser (STRCAT (GETVAR "dwgprefix") fileuser ".pdf"))

      ;закрытие файла если он открыт тут (на этом компе
      (if (= (setq fi0 (open fileuser "w")) nil) 
        (exit_pdf)
        (close fi0)
      )

      (if (= (setq fi0 (open fileuser "w")) nil) 
        (progn 
          (print 
            "Файл ужо адчыненны іншай праграмай,\nабо немагчыма запісаць у гэты каталог (няма праў)"
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
          (if nil  ; [ОТЛАДКА] Отключено реальное печатание
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
            (princ "\n[ОТЛАДКА] Функция _plot отключена.\n")
          )
          (SETVAR "cmdecho" cmd)
          (princ (STRCAT "\nФайл захаваны: " fileuser "\n"))
        )
      )
    )
  ) ;канец ифа

  (setvar "ctab" temp_lm) ;пераход на папярэдні ліст дзе знаходзіуся карыстальнік

  ;закрытие пдф
  (exit_pdf)
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
        (strcat (vl-bb-ref 'file_all) "&" f_temp "<")
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
                   lik_open data stor data1 nameris acad_color
                  )  ;numar
  (setq acad_color 0)
  (PRINC "\n---------------------------------------------------------------------------\n")
  ; (PRINC "Праграмма распрацавана на lisp, prajdziswet-ам (Косау Уладзимир) у 2014 годзе\n")
  (PRINC "-----------------------------------------------------------------------------\n")
  (PRINC "\n")

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
                (if (equal sfile 1) 
                  (princ "true")
                  (princ "false")
                )
                "&"
                (if (equal sfile_all 1) 
                  (princ "true")
                  (princ "false")
                )
        )
      )
      (vl-bb-set 'file_ris nil) ;обнуление
      (if (or (/= rys 0) (peshat-spds-file-prepare)) 
        (progn 
          (zapusk_druk)
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
                  (princ 
                    (strcat "\n___________________________________________________________\n[ОТЛАДКА] Сформированная строка file_all:\n" 
                            (vl-bb-ref 'file_all)
                            "\n"
                    )
                  )
                  ;(startapp
                  ;  ;"__prog\\exe\\WPFMergeExe\\WPFMergeExe.exe"
                  ;  "e:\\praca-proect\\_program\\_Програм\\__скончаные\\__скончаные\\_Autocad\\WPFMergeExe\\WPFMergeExe\\bin\\Debug\\net48\\WPFMergeExe.exe"
                  ;  (strcat "\"" (vl-bb-ref 'file_all) "\"")
                  ;)
                  ;(command)
                  (princ "\n[ОТЛАДКА] Вызов WPFMergeExe отключен.\n")
                )
              )
            )
          )
        )
      )
    )
  ) ;end if
  (vl-bb-set 'lik_open nil)
  (princ)
)
;
(print "загружена автоматичская печать")
(princ)

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
            (setq peshat-file (strcat (GETVAR "dwgprefix") (car peshat-files))) ;адчыняемы файл
            (setq peshat-files (cdr peshat-files)) ;обрезка
            (setq peshat-file (vla-Open 
                                (vla-get-Documents (vlax-get-acad-object))
                                peshat-file
                                :flax-true
                                ""
                              )
            ) ;адчыненне
            (vla-Close peshat-file :vlax-false) ;зачыненне
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
