;вяртанне назову рысунка

;вяртанне шифру
;сканчэнне вяртанне шифру

; для выбранного многострочного текста очищает форматирование.=============================================================================|;
(defun clear-mtext1 (string-to-normalize / sub_string sub_pos left_string 
                     right_string
                    ) 
  (if 
    (or 
      (setq sub_pos (vl-string-search "{f" string-to-normalize))
      (setq sub_pos (vl-string-search "{\\" string-to-normalize))
      (setq sub_pos (vl-string-search "\\f" string-to-normalize))
      (setq sub_pos (vl-string-search "{\\f" string-to-normalize))
    ) ;_ end of or
    (progn 
      (setq left_string ;все, что до "{"
                        (vl-string-trim 
                          "{"
                          (substr 
                            string-to-normalize
                            1
                            (vl-string-position 
                              (ascii "\\")
                              string-to-normalize
                              sub_pos
                            ) ;_ end of vl-string-position
                          ) ;_ end of substr
                        ) ;_ end of vl-string-trim
      ) ;_ end of setq
      ;; Вот здесь была ошибка при некоторых условиях
      (if 
        (vl-string-position 
          (ascii ";")
          string-to-normalize
          sub_pos
        ) ;_ end of vl-string-position
        (setq right_string ;все, что между {f и ;
                           (substr 
                             string-to-normalize
                             (+ 
                               (vl-string-position 
                                 (ascii ";")
                                 string-to-normalize
                                 sub_pos
                               ) ;_ end of vl-string-position
                               2
                             ) ;_ end of +
                           ) ;_ end of substr
        ) ;_ end of setq
        (setq right_string "")
      ) ;_ end of if
      (clear-mtext1 (strcat left_string right_string))
    ) ;_ end of progn
    ;; Старый вариант попытки снесения "}"
    ;;(vl-string-trim "}" string-to-normalize)
    ;; Новый вариант снесения "}"
    (vl-list->string 
      (vl-remove 
        (ascii "}")
        (vl-string->list string-to-normalize)
      ) ;_ end of vl-remove
    ) ;_ end of vl-list->string
  ) ;_ end of if
) ;_ end of defun
(defun clear-mtext (string-to-normalize / poz) 
  (setq string-to-normalize (clear-mtext1 string-to-normalize))
  (if (wcmatch string-to-normalize "\\*;*") 
    (progn 
      (setq poz (+ 
                  (vl-string-search ";" 
                                    string-to-normalize
                                    (vl-string-search "\\" string-to-normalize 0)
                  )
                  2
                )
      )
      (substr string-to-normalize poz (- (strlen string-to-normalize) poz -1))
    )
    (setq string-to-normalize string-to-normalize)
  )
)
;----------------------------------------------------

;--------------функция атрыманне с дин.блоку тексту----------------
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
    str
    nil
  )
)

;;---------------------нормализация № старонки(спдс)----------------------
(defun norma_n2 (str) 
  (setq str (vl-string-trim " " str))
  (if 
    (wcmatch str 
             "#,##,###,#`.#,##`.#,###`.#,#`.##,##`.##,###`.##,#`.###,##`.###,###`.###"
    )
    str
    nil
  )
)


;;---------------------вызначэнне № старонки----------------------

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
(defun v_formats (/ dis1 dis2 mash) 
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

;;-----------------------------друк поліліній-------------------------

;=================================================================

;=================================================================

;=================================================================

;вызначэнне параметрау блоку нумар-имя


;;-----------------------------друк блокау-------------------------



;=================================================================

;=================================================================

;=================================================================

;;---------------------вызначэнне СПДС формат----------------------
(defun v_format_spds (/ i z f_temp) 

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
)
;канец вызначэнне формата

;------------------------СПДС вызначэнне-------------------------------

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
      (if (equal f_temp "") 
        (setq f_temp (strcat fileuser ".pdf"))
        (setq f_temp (strcat f_temp "?" fileuser ".pdf"))
      )

      (if (equal ris_temp "") 
        (if (/= nil nameris) 
          (setq ris_temp (strcat fileuser ".pdf" "?" nameris "?" list_n1))
          (setq ris_temp (strcat fileuser ".pdf" "?" "" "?" list_n1))
        )
        (if (/= nil nameris) 
          (setq ris_temp (strcat ris_temp "|" fileuser ".pdf" "?" nameris "?" list_n1))
          (setq ris_temp (strcat ris_temp "|" fileuser ".pdf" "?" "" "?" list_n1))
        )
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
          (if (/= model "Model") 
            (command "_plot" ;Сама команда
                     "_y" ;Выполнить детальное задание конфигурации?:
                     "" ;Имя листа или <печать>:
                     "_DWG в PDF ЭТО.pc3" ;Имя устройства вывода:
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
                     "_DWG в PDF ЭТО.pc3" ;Имя устройства вывода:
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
          (princ (STRCAT "Файл захаваны: " fileuser "\n"))
        )
      )
    )
  ) ;канец ифа

  (setvar "ctab" temp_lm) ;пераход на папярэдні ліст дзе знаходзіуся карыстальнік

  ;закрытие пдф
  (exit_pdf)
);канец принта


;------------------------ПЕЧАТЬ---------------------------------

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
    (strcat "\nАгульная колькасць рысункаў да знішчэння дублікатаў: " 
            (rtos (+ (len druk_n) (len druk_v)))
            "\n"
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
    (strcat "Агульная колькасць рысункаў пасля знішчэння дублікатаў: " 
            (rtos (+ (len druk_n) (len druk_v)))
            "\n\n"
    )
  )
)


;;---------------------утварэнне списау druk_v druk_n----------------------


;------------------------утварэнне спісау друку іх нумерацыя,ад дыялогу---------------------------------


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
  (write-line "    list = \"монохромный\\nвсе в цвете\\nвыбрать чертежи\";" 
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

;------------------------ObjectDBX Helpers---------------------------------
(defun get-opened-docs (/ acadObj docs docList) 
  (setq acadObj (vlax-get-acad-object))
  (setq docs (vla-get-documents acadObj))
  (setq docList nil)
  (vlax-for doc docs 
    (setq docList (cons doc docList))
  )
  (reverse docList)
)

(defun get-doc-by-name (fullname / docs res) 
  (setq docs (get-opened-docs))
  (setq res nil)
  (foreach doc docs 
    (if 
      (or (= (strcase (vla-get-fullname doc)) (strcase fullname)) 
          (= (strcase (vla-get-name doc)) (strcase fullname))
      )
      (setq res doc)
    )
  )
  res
)

(defun create-dbx-doc (/ acadver dbx) 
  (setq acadver (substr (getvar "ACADVER") 1 2))
  (setq dbx (vl-catch-all-apply 
              'vlax-create-object
              (list 
                (if (< (atoi acadver) 16) 
                  "ObjectDBX.AxDbDocument"
                  (strcat "ObjectDBX.AxDbDocument." acadver)
                )
              )
            )
  )
  (if (vl-catch-all-error-p dbx) 
    nil
    dbx
  )
)

;------------------------DBX BoundingBox Text Finder------------------------
(defun dbx-get-text-in-box (layout-block pt1 pt2 / text obj-name point x y txt atts 
                            x1temp y1temp
                           ) 
  (setq text "")
  (vlax-for obj layout-block 
    (setq obj-name (strcase (vla-get-ObjectName obj)))
    (cond 
      ((or (= obj-name "ACDBTEXT") (= obj-name "ACDBMTEXT"))
       (setq point (vl-catch-all-apply 'vlax-get (list obj 'InsertionPoint)))
       (if (not (vl-catch-all-error-p point)) 
         (progn 
           (setq x (car point)
                 y (cadr point)
           )
           (if 
             (and (> x (min (car pt1) (car pt2))) 
                  (< x (max (car pt1) (car pt2)))
                  (> y (min (cadr pt1) (cadr pt2)))
                  (< y (max (cadr pt1) (cadr pt2)))
             )
             (progn 
               (setq txt (vla-get-TextString obj))
               (if (= obj-name "ACDBMTEXT") (setq txt (clear-mtext txt)))
               (setq text (strcat text txt))
             )
           )
         )
       )
      )
      ((= obj-name "ACDBBLOCKREFERENCE")
       (if (= (vla-get-HasAttributes obj) :vlax-true) 
         (progn 
           (setq atts (vlax-safearray->list (vlax-variant-value (vla-GetAttributes obj))))
           (foreach tag atts 
             (if (= (vla-get-visible tag) :vlax-true) 
               (progn 
                 (setq point (vl-catch-all-apply 'vlax-get 
                                                 (list tag 'TextAlignmentPoint)
                             )
                 )
                 (if 
                   (or (vl-catch-all-error-p point) (equal point '(0.0 0.0 0.0)))
                   (setq point (vl-catch-all-apply 'vlax-get 
                                                   (list tag 'InsertionPoint)
                               )
                   )
                 )
                 (if (not (vl-catch-all-error-p point)) 
                   (progn 
                     (setq x1temp (car point)
                           y1temp (cadr point)
                     )
                     (if 
                       (and (> x1temp (min (car pt1) (car pt2))) 
                            (< x1temp (max (car pt1) (car pt2)))
                            (> y1temp (min (cadr pt1) (cadr pt2)))
                            (< y1temp (max (cadr pt1) (cadr pt2)))
                       )
                       (progn 
                         (setq txt (vla-get-TextString tag))
                         (setq text (strcat text (clear-mtext txt)))
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
  text
)

;------------------------DBX Core logic---------------------------------
(defun dbx-utvar (layout-block numa0 / numa x_temp1 x_temp2 zapret_nomer) 
  (normal_points)
  (if (and (/= numa0 nil) (/= numa0 0) (/= numa0 "0")) 
    (setq numa numa0)
    (progn 
      (setq x_temp1 (list (- (car x2) (/ 40 mash)) (+ (last x2) (/ 30 mash))))
      (setq x_temp2 (list (- (car x2) (/ 25 mash)) (+ (last x2) (/ 20 mash))))
      (setq numa (norma_n2 (dbx-get-text-in-box layout-block x_temp1 x_temp2)))
      (if (or (= numa nil) (= numa "")) 
        (progn 
          (setq x_temp1 (list (- (car x2) (/ 25 mash)) (+ (last x2) (/ 30 mash))))
          (setq x_temp2 (list (- (car x2) (/ 5 mash)) (+ (last x2) (/ 20 mash))))
          (setq zapret_nomer (norma_n2 
                               (dbx-get-text-in-box layout-block x_temp1 x_temp2)
                             )
          )
          (if (or (= zapret_nomer nil) (= zapret_nomer "")) 
            (progn 
              (setq zapret_name T)
              (setq x_temp1 (list (- (car x2) (/ 15 mash)) 
                                  (+ (last x2) (/ 13 mash))
                            )
              )
              (setq x_temp2 (list (- (car x2) (/ 5 mash)) (+ (last x2) (/ 5 mash))))
              (setq zapret_nomer (norma_n2 
                                   (dbx-get-text-in-box 
                                     layout-block
                                     x_temp1
                                     x_temp2
                                   )
                                 )
              )
            )
          )
          (setq numa zapret_nomer)
        )
      )
    )
  )
  (if (and (OR (= nameris "") (= nameris nil)) (/= zapret_name T)) 
    (progn 
      (setq x_temp1 (list (- (car x2) (/ 125 mash)) (+ (last x2) (/ 20 mash))))
      (setq x_temp2 (list (- (car x2) (/ 55 mash)) (+ (last x2) (/ 5 mash))))
      (setq nameris (dbx-get-text-in-box layout-block x_temp1 x_temp2))
    )
  )
  (if (or (= numa nil) (= numa 0) (= numa "")) 
    (setq druk_n (cons 
                   (list 0 x1 x2 format mash poloz model nameris nil 
                         dbx-current-filename
                   )
                   druk_n
                 )
    )
    (setq druk_v (cons 
                   (list numa x1 x2 format mash poloz model nameris nil 
                         dbx-current-filename
                   )
                   druk_v
                 )
    )
  )
)

(defun dbx-get-block-att (obj / atts tag point x1temp y1temp x2temp y2temp txt) 
  (if (= (vla-get-HasAttributes obj) :vlax-true) 
    (progn 
      (setq atts (vlax-safearray->list (vlax-variant-value (vla-GetAttributes obj))))
      (foreach tag atts 
        (if (= (vla-get-visible tag) :vlax-true) 
          (progn 
            (setq point (vl-catch-all-apply 'vlax-get 
                                            (list tag 'TextAlignmentPoint)
                        )
            )
            (if (or (vl-catch-all-error-p point) (equal point '(0.0 0.0 0.0))) 
              (setq point (vl-catch-all-apply 'vlax-get (list tag 'InsertionPoint)))
            )
            (if (not (vl-catch-all-error-p point)) 
              (progn 
                (setq x1temp (car point)
                      y1temp (cadr point)
                )
                (setq x2temp (car x2)
                      y2temp (cadr x2)
                )
                (setq txt (vla-get-TextString tag))
                (cond 
                  ((and (< (- x2temp (* mash 40)) x1temp) 
                        (> (- x2temp (* mash 25)) x1temp)
                        (> y1temp (+ (* mash 20) y2temp))
                        (< y1temp (+ (* mash 30) y2temp))
                   )
                   (setq numa txt)
                  )
                  ((and (< (- x2temp (* mash 15)) x1temp) 
                        (> (- x2temp (* mash 5)) x1temp)
                        (> y1temp (+ (* mash 5) y2temp))
                        (< y1temp (+ (* mash 13) y2temp))
                   )
                   (setq numa txt)
                  )
                  ((and (< (- x2temp (* mash 125)) x1temp) 
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
      )
    )
  )
)

(defun dbx-scan-document (doc filename is-dbx / active-ms obj-array copied list1 
                          obj-name layout-name layout-block name1 name2 name3 f_temp z 
                          i dbx-current-filename
                         ) 
  (setq active-ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq obj-array (vlax-make-safearray vlax-vbObject '(0 . 0)))
  (setq dbx-current-filename filename)

  (vlax-for layout (vla-get-Layouts doc) 
    (setq layout-name (vla-get-Name layout))
    (setq layout-block (vla-get-Block layout))

    (vlax-for obj layout-block 
      (setq obj-name (strcase (vla-get-ObjectName obj)))

      ; 1. SPDSFORMAT
      (if (= obj-name "MCSDBOBJECTFORMAT") 
        (progn 
          (setq nameris     nil
                zapret_name nil
                format      nil
                numa        nil
          )
          (if 
            (not 
              (vl-catch-all-error-p 
                (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'x1 'x2))
              )
            )
            (progn 
              (setq x1 (vlax-safearray->list x1)
                    x2 (vlax-safearray->list x2)
              )
              (setq model layout-name)
              (if is-dbx 
                (progn 
                  (vlax-safearray-put-element obj-array 0 obj)
                  (setq copied (vla-CopyObjects (vla-get-Database doc) 
                                                obj-array
                                                active-ms
                               )
                  )
                  (setq copied (vlax-safearray-get-element (vlax-variant-value copied) 
                                                           0
                               )
                  )
                  (setq list1 (entget (vlax-vla-object->ename copied)))
                  (vla-Delete copied)
                )
                (setq list1 (entget (vlax-vla-object->ename obj)))
              )
              (if (<= (length list1) 95) 
                (if (/= (vl-position (cons 301 "Drawing type") list1) nil) 
                  (setq zapret_name T)
                )
                (progn 
                  (setq name1 (cdr 
                                (nth 
                                  (+ 1 
                                     (vl-position (cons 301 "Drawing type") list1)
                                  )
                                  list1
                                )
                              )
                  )
                  (setq name2 nil
                        name3 nil
                  )
                  (if (/= nil (vl-position (cons 301 "Drawing type1") list1)) 
                    (setq name2 (cdr 
                                  (nth 
                                    (+ 1 
                                       (vl-position (cons 301 "Drawing type1") 
                                                    list1
                                       )
                                    )
                                    list1
                                  )
                                )
                    )
                  )
                  (if (/= nil (vl-position (cons 301 "Drawing type2") list1)) 
                    (setq name3 (cdr 
                                  (nth 
                                    (+ 1 
                                       (vl-position (cons 301 "Drawing type2") 
                                                    list1
                                       )
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
              )
              (v_format_spds)
              (if (/= format nil) (dbx-utvar layout-block numa))
            )
          )
        )
      )

      ; 2. LWPOLYLINE
      (if (= obj-name "ACDBPOLYLINE") 
        (progn 
          (setq nameris     nil
                zapret_name nil
                format      nil
                numa        nil
                mash        1
          )
          (if 
            (not 
              (vl-catch-all-error-p 
                (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'x1 'x2))
              )
            )
            (progn 
              (setq x1 (vlax-safearray->list x1)
                    x2 (vlax-safearray->list x2)
              )
              (setq model layout-name)
              (normal_points)
              (v_formats)
              (if (= format nil) 
                (progn (setq mash 100) (v_formats))
              )
              (if (/= format nil) (dbx-utvar layout-block nil))
            )
          )
        )
      )

      ; 3. BLOCKREFERENCE
      (if (= obj-name "ACDBBLOCKREFERENCE") 
        (progn 
          (setq nameris     nil
                zapret_name nil
                format      nil
                numa        nil
          )
          (if 
            (not 
              (vl-catch-all-error-p 
                (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'x1 'x2))
              )
            )
            (progn 
              (setq mash (/ 1 (vla-get-XScaleFactor obj)))
              (setq x1 (vlax-safearray->list x1)
                    x2 (vlax-safearray->list x2)
              )
              (setq model layout-name)
              (normal_points)
              (v_formats)
              (if (/= format nil) 
                (progn 
                  (dbx-get-block-att obj)
                  (dbx-utvar layout-block numa)
                )
              )
            )
          )
        )
      )
    )
  )
)

;------------------------выклік діалогу---------------------------------
(defun c:dil_spds (/ dcl_id pdf rys ddi done file_all sfile_all sfile lik_open data 
                   stor data1 nameris acad_color peshat-files active-doc open-doc 
                   dbx-doc idx item color_indices temp_druk_v
                  ) 
  (setq acad_color 0)
  (PRINC "\n-----выкананне аўтаматчынага друку-----\n")

  (setq vers (substr (vl-bb-ref 'dirpol) 
                     (+ (vl-string-search "AUTODESK" (strcase (vl-bb-ref 'dirpol))) 
                        18
                     )
                     4
             )
  )
  (vl-load-com)
  (vl-bb-set 'lik_open 0)

  (if (/= auto T) 
    (progn 
      (setq done nil)
      (setq dcl_id (PrintAutoDcl))
      (if (not (new_dialog "prindcl" dcl_id)) (exit))
      (action_tile "accept" "(palja)(done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq ddi (start_dialog))
      (unload_dialog dcl_id)
    )
    (progn 
      (setq rys       1
            sfile_all 1
            sfile     0
            ddi       1
      )
    )
  )

  (if (= ddi 1) 
    (progn 
      (vl-bb-set 'acad_color acad_color)
      (vl-bb-set 
        'file_all
        (strcat (GETVAR "dwgprefix") 
                "&"
                (if (equal sfile 1) "true" "false")
                "&"
                (if (equal sfile_all 1) "true" "false")
        )
      )
      (vl-bb-set 'file_ris nil)

      ; DBX SCAN PHASE
      (setq druk_n nil
            druk_v nil
      )
      (if (= rys 1) 
        (progn 
          (setq active-doc (vla-get-ActiveDocument (vlax-get-acad-object)))
          (dbx-scan-document active-doc (getvar "dwgname") nil)
        )
        (progn 
          (setq peshat-files (vl-directory-files (GETVAR "dwgprefix") "*.dwg" 1))
          (foreach f peshat-files 
            (setq open-doc (get-doc-by-name (strcat (getvar "dwgprefix") f)))
            (if open-doc 
              (dbx-scan-document open-doc f nil)
              (progn 
                (setq dbx-doc (create-dbx-doc))
                (if dbx-doc 
                  (progn 
                    (if 
                      (not 
                        (vl-catch-all-error-p 
                          (vl-catch-all-apply 'vla-Open 
                                              (list dbx-doc 
                                                    (strcat (getvar "dwgprefix") f)
                                              )
                          )
                        )
                      )
                      (dbx-scan-document dbx-doc f T)
                    )
                    (vlax-release-object dbx-doc)
                  )
                )
              )
            )
          )
        )
      )

      (del_dubl)
      (prin_numar)
      (zad_n)

      (if druk_v 
        (setq druk_v (vl-sort druk_v 
                              '(lambda (a b) 
                                 (< (strcase (nth 9 a)) (strcase (nth 9 b)))
                               )
                     )
        )
      )
      (if druk_n 
        (setq druk_n (vl-sort druk_n 
                              '(lambda (a b) 
                                 (< (strcase (nth 9 a)) (strcase (nth 9 b)))
                               )
                     )
        )
      )

      ; COLOR DIALOG PHASE (Form 3)
      (if (and (= acad_color 2) (/= druk_v nil)) 
        (progn 
          (setq color_indices (ShowHierarchicalColorDialog druk_v))
          (if (= color_indices 'CANCEL) 
            (setq druk_v nil)
            (progn 
              (setq temp_druk_v nil
                    idx         0
              )
              (foreach item druk_v 
                (if (member idx color_indices) 
                  (setq temp_druk_v (cons 
                                      (list (nth 0 item) 
                                            (nth 1 item)
                                            (nth 2 item)
                                            (nth 3 item)
                                            (nth 4 item)
                                            (nth 5 item)
                                            (nth 6 item)
                                            (nth 7 item)
                                            T
                                            (nth 9 item)
                                      )
                                      temp_druk_v
                                    )
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

      ; PRINTING PHASE
      (dbx-peshat druk_v)

      (if (and (OR (equal sfile_all 1) (equal sfile 1)) (> (length druk_v) 1)) 
        (if (boundp 'add_edit) 
          (add_edit needAdd (vl-bb-ref 'file_all))
          (progn 
            (startapp 
              "__prog\\exe\\FormMerge\\FormMergeExe.exe"
              (strcat "\"" (vl-bb-ref 'file_all) "\"")
            )
            (command)
          )
        )
      )
    )
  )
  (princ "------аўтаматычны друк скончаны-----\n")
  (vl-bb-set 'lik_open nil)
  (princ)
)

(defun dil_spds_new (needAdd stor / auto) 
  (setq auto T)
  (c:dil_spds)
)

(defun ShowHierarchicalColorDialog (sheets / dcl_file file_handle dcl_id result 
                                    selected_indices display_list display_map item 
                                    name fname f_group idx map_val read_list 
                                    file_selected i result_indices
                                   ) 
  (setq dcl_file (vl-filename-mktemp "sheet_color.dcl"))
  (setq file_handle (open dcl_file "w"))
  (write-line "sheet_color_dcl : dialog {" file_handle)
  (write-line "  label = \"Печать в цвете\";" file_handle)
  (write-line "  : text { label = \"Выберете чертежи или файлы.(##*) ..\"; }" 
              file_handle
  )
  (write-line "  : list_box {" file_handle)
  (write-line "    key = \"sheets_list\";" file_handle)
  (write-line "    width = 80; height = 25; multiple_select = true;" file_handle)
  (write-line "  }" file_handle)
  (write-line "  ok_cancel;" file_handle)
  (write-line "}" file_handle)
  (close file_handle)

  (setq display_list nil
        display_map  nil
        f_group      ""
        idx          0
  )
  (foreach item sheets 
    (setq fname (nth 9 item))
    (if (/= fname f_group) 
      (progn 
        (setq f_group fname)
        (setq display_list (cons (strcat "##" fname ":") display_list))
        (setq display_map (cons "FILE" display_map))
      )
    )
    (setq name (nth 7 item))
    (if (and name (/= name "")) 
      (setq display_list (cons (strcat "  " (car item) " " name) display_list))
      (setq display_list (cons (strcat "  " (car item)) display_list))
    )
    (setq display_map (cons idx display_map))
    (setq idx (1+ idx))
  )
  (setq display_list (reverse display_list))
  (setq display_map (reverse display_map))

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "sheet_color_dcl" dcl_id)) 
    (setq result 0)
    (progn 
      (start_list "sheets_list")
      (foreach item display_list (add_list item))
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

  (cond 
    ((= result 0) 'CANCEL)
    ((and (= result 1) selected_indices (/= selected_indices ""))
     (setq result_indices nil)
     (setq read_list (read (strcat "(" selected_indices ")")))
     (setq file_selected nil
           i             0
     )
     (foreach map_val display_map 
       (if (= map_val "FILE") 
         (if (member i read_list) (setq file_selected T) (setq file_selected nil))
         (if (or file_selected (member i read_list)) 
           (setq result_indices (cons map_val result_indices))
         )
       )
       (setq i (1+ i))
     )
     result_indices
    )
    (t nil)
  )
)

(defun dbx-execute-print-and-return (/ group f_temp ris_temp item bb_f bb_ris 
                                     acad_color
                                    ) 
  (setq group (vl-bb-ref 'dbx_print_group))
  (setq acad_color (vl-bb-ref 'acad_color))
  (setq f_temp "")
  (setq ris_temp "")

  (foreach item group 
    (print_s item)
  )

  (setq bb_f (vl-bb-ref 'dbx_accumulated_f))
  (if (and bb_f (/= bb_f "")) 
    (vl-bb-set 'dbx_accumulated_f (strcat bb_f "&" f_temp))
    (vl-bb-set 'dbx_accumulated_f f_temp)
  )

  (setq bb_ris (vl-bb-ref 'dbx_accumulated_ris))
  (if (and bb_ris (/= bb_ris "")) 
    (vl-bb-set 'dbx_accumulated_ris (strcat bb_ris "&" ris_temp))
    (vl-bb-set 'dbx_accumulated_ris ris_temp)
  )

  (vl-bb-set 'dbx_print_group nil)
  (princ)
)

(defun dbx-peshat (druk-list / acadObj active-doc current-fname f_group temp 
                   doc-opened f_temp ris_temp bb_f bb_ris
                  ) 
  (setq acadObj (vlax-get-acad-object))
  (setq active-doc (vla-get-ActiveDocument acadObj))
  (setq f_temp   ""
        ris_temp ""
  )
  (vl-bb-set 'dbx_accumulated_f nil)
  (vl-bb-set 'dbx_accumulated_ris nil)

  (while druk-list 
    (setq current-fname (nth 9 (car druk-list)))
    (setq f_group nil
          temp    nil
    )
    (foreach item druk-list 
      (if (= (nth 9 item) current-fname) 
        (setq f_group (cons item f_group))
        (setq temp (cons item temp))
      )
    )
    (setq f_group (reverse f_group))
    (setq druk-list (reverse temp))

    (if (= (strcase current-fname) (strcase (vla-get-Name active-doc))) 
      (progn 
        (princ (strcat "\nДрукуецца бягучы файл: " current-fname "...\n"))
        (foreach item f_group (print_s item))
        (princ 
          (strcat "Раздрукаваны бягучы файл: " 
                  current-fname
                  ", рысункаў: "
                  (itoa (length f_group))
                  "\n"
          )
        )
      )
      (progn 
        (vl-bb-set 'dbx_print_group f_group)
        (setq doc-opened (vl-catch-all-apply 'vla-Open 
                                             (list (vla-get-Documents acadObj) 
                                                   (strcat (getvar "dwgprefix") 
                                                           current-fname
                                                   )
                                                   :vlax-false
                                                   ""
                                             )
                         )
        )
        (if (not (vl-catch-all-error-p doc-opened)) 
          (progn 
            (vla-Close doc-opened :vlax-false)
            (princ 
              (strcat "Раздрукаваны вонкавы файл: " 
                      current-fname
                      ", рысункаў: "
                      (itoa (length f_group))
                      "\n"
              )
            )
          )
        )
      )
    )
  )

  (setq bb_f (vl-bb-ref 'dbx_accumulated_f))
  (setq bb_ris (vl-bb-ref 'dbx_accumulated_ris))

  (if (and bb_f (/= bb_f "")) 
    (if (equal f_temp "") 
      (setq f_temp bb_f)
      (setq f_temp (strcat f_temp "&" bb_f))
    )
  )
  (if (and bb_ris (/= bb_ris "")) 
    (if (equal ris_temp "") 
      (setq ris_temp bb_ris)
      (setq ris_temp (strcat ris_temp "&" bb_ris))
    )
  )

  (if (/= (vl-bb-ref 'file_ris) nil) 
    (vl-bb-set 'file_ris (strcat (vl-bb-ref 'file_ris) "&" ris_temp))
    (vl-bb-set 'file_ris ris_temp)
  )
  (vl-bb-set 'file_all (strcat (vl-bb-ref 'file_all) "&" f_temp))

  (princ 
    (strcat "\nАдпраўлена ў вонкавую праграму зліцця: " (vl-bb-ref 'file_all) "\n")
  )

  (vl-bb-set 'dbx_accumulated_f nil)
  (vl-bb-set 'dbx_accumulated_ris nil)
)

(print "загружена автоматічская печать (ObjectDBX)")
(princ)

(defun dil_spds_new (needAdd stor / auto) 
  (setq auto T)
  (c:dil_spds)
)

(if (vl-bb-ref 'dbx_print_group) 
  (dbx-execute-print-and-return)
)

(defun c:view_formats (/ active-doc old-ctab old-viewctr old-viewsize all-sheets total-sheets idx item model 
                       x1 x2 num format-name msg druk_n druk_v
                      ) 
  (vl-load-com)
  (PRINC "\n-----Аналіз файла для прагляду фарматак-----\n")
  (setq old-ctab (getvar "ctab"))
  (setq old-viewctr (getvar "VIEWCTR"))
  (setq old-viewsize (getvar "VIEWSIZE"))
  (setq active-doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (setq druk_n nil
        druk_v nil
  )
  (dbx-scan-document active-doc (getvar "dwgname") nil)
  (del_dubl)
  (prin_numar)
  (zad_n)

  (setq all-sheets (append druk_v druk_n))

  (if (not all-sheets) 
    (princ "\nФарматкі не знойдзены.\n")
    (progn 
      (setq total-sheets (length all-sheets)
            idx          1
      )
      (foreach item all-sheets 
        (print item)
        (setq num         (nth 0 item)
              x1          (nth 1 item)
              x2          (nth 2 item)
              format-name (nth 3 item)
              model       (nth 6 item)
        )

        (if (/= (getvar "ctab") model) 
          (setvar "ctab" model)
        )

        (command "_.zoom" "_w" x1 x2)

        (setq msg (strcat "\nПрагляд фарматкі " 
                          (itoa idx)
                          " з "
                          (itoa total-sheets)
                          " ["
                          format-name
                          " / Ліст: "
                          model
                          " / №: "
                          (vl-princ-to-string num)
                          "]. Націсніце Enter або Прабел для наступнай... "
                  )
        )
        (getstring msg)
        (setq idx (1+ idx))
      )
      (if (/= (getvar "ctab") old-ctab) 
        (setvar "ctab" old-ctab)
      )
      (command "_.zoom" "_c" old-viewctr old-viewsize)
      (princ "\nПрагляд фарматак завершаны.\n")
    )
  )
  (princ)
)


