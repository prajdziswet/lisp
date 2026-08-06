;---- формы зліцця --------
(defun c:StartForm () 
  (if (boundp 'add_edit) 
    (command "new_edit")
    ;(startapp "__prog\\exe\\WPFMergeExe\\WPFMergeExe.exe")
    ;(startapp "__prog\\exe\\WPFMergeExe\\WPFMergeExe.exe")
    (startapp 
      "e:\\praca-proect\\_program\\_Програм\\__скончаные\\__скончаные\\_Autocad\\WPFMergeExe\\WPFMergeExe\\bin\\Debug\\net48\\WPFMergeExe.exe"
    )
  )
)

;---- дадавання --------
(defun ask_pdf_print (default_name / dcl_file file_handle dcl_id result fname cprint) 
  (setq dcl_file (vl-filename-mktemp "ask_pdf.dcl"))
  (setq file_handle (open dcl_file "w"))
  (write-line "ask_pdf_dcl : dialog {" file_handle)
  (write-line "  label = \"Одиночная печать PDF\";" file_handle)
  (write-line "  : toggle {" file_handle)
  (write-line "    key = \"cprint\";" file_handle)
  (write-line "    label = \"Цветная печать\";" file_handle)
  (write-line "  }" file_handle)
  (write-line "  : text { value = \"Название файла pdf\"; }" file_handle)
  (write-line "  : text { value = \"(нажмите ок или 00 - заменяет\"; }" 
              file_handle
  )
  (write-line "  : text { value = \"название текущего файла):\"; }" file_handle)
  (write-line "  : edit_box {" file_handle)
  (write-line "    key = \"fname\";" file_handle)
  (write-line "    label = \"\";" file_handle)
  (write-line "    width = 50;" file_handle)
  (write-line "    allow_accept = true;" file_handle)
  (write-line "  }" file_handle)
  (write-line "  ok_cancel;" file_handle)
  (write-line "}" file_handle)
  (close file_handle)

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "ask_pdf_dcl" dcl_id)) 
    (setq result "")
    (progn 
      (if default_name (set_tile "fname" default_name))
      (set_tile "cprint" "0")
      (action_tile "fname" "(setq fname $value)")
      (action_tile "cprint" "(setq cprint $value)")
      (action_tile "accept" 
                   "(setq fname (get_tile \"fname\")) (setq cprint (get_tile \"cprint\")) (done_dialog 1)"
      )
      (action_tile "cancel" "(done_dialog 0)")
      (setq result (start_dialog))
      (unload_dialog dcl_id)
      (vl-file-delete dcl_file)
    )
  )
  (if (= result 1) 
    (if (= cprint "1") 
      (strcat "+" fname)
      fname
    )
    nil
  )
)


(defun ask_dialog (zapros1 zapros2 zapros3 btn1 btn2 / dcl_file file_handle dcl_id result) 
  (setq dcl_file (vl-filename-mktemp "ask_dialog.dcl"))
  (setq file_handle (open dcl_file "w"))
  (write-line "ask_dcl : dialog {" file_handle)
  (write-line "  label = \"Запрос\";" file_handle)
  (write-line "  : text {" file_handle)
  (write-line (strcat "    label = \"" zapros1 "\";") file_handle)
  (write-line "    alignment = centered;" file_handle)
  (write-line "  }" file_handle)
  (if (and zapros2 (/= zapros2 ""))
    (progn
      (write-line "  : text {" file_handle)
      (write-line (strcat "    label = \"" zapros2 "\";") file_handle)
      (write-line "    alignment = centered;" file_handle)
      (write-line "  }" file_handle)
    )
  )
  (if (and zapros3 (/= zapros3 ""))
    (progn
      (write-line "  : text {" file_handle)
      (write-line (strcat "    label = \"" zapros3 "\";") file_handle)
      (write-line "    alignment = centered;" file_handle)
      (write-line "  }" file_handle)
    )
  )
  (write-line "  : spacer { height = 1; }" file_handle)
  (write-line "  : row {" file_handle)
  (write-line "    fixed_width = true;" file_handle)
  (write-line "    alignment = centered;" file_handle)
  (write-line "    : button {" file_handle)
  (write-line "      key = \"btn1\";" file_handle)
  (write-line (strcat "      label = \"" btn1 "\";") file_handle)
  (write-line "      is_default = true;" file_handle)
  (write-line "      width = 12;" file_handle)
  (write-line "    }" file_handle)
  (write-line "    : button {" file_handle)
  (write-line "      key = \"btn2\";" file_handle)
  (write-line (strcat "      label = \"" btn2 "\";") file_handle)
  (write-line "      width = 12;" file_handle)
  (write-line "    }" file_handle)
  (write-line "    : button {" file_handle)
  (write-line "      key = \"cancel\";" file_handle)
  (write-line "      label = \"Выход\";" file_handle)
  (write-line "      is_cancel = true;" file_handle)
  (write-line "      width = 12;" file_handle)
  (write-line "    }" file_handle)
  (write-line "  }" file_handle)
  (write-line "}" file_handle)
  (close file_handle)

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "ask_dcl" dcl_id)) 
    (setq result "No")
    (progn 
      (action_tile "btn1" "(done_dialog 1)")
      (action_tile "btn2" "(done_dialog 2)")
      (setq result (start_dialog))
      (unload_dialog dcl_id)
      (vl-file-delete dcl_file)
      (cond 
        ((= result 1) "Yes")
        ((= result 2) "No")
        (T nil)
      )
    )
  )
)


(defun c:AddForm (/ kword) 
  (if (boundp 'add_edit) 
    (command "add_dwg")
    (progn 
      (setq kword (ask_dialog "Нажмите 'Да' - для ручного выбора" "или 'Нет' - для автоматического," "или закройте окно" "Да" "Не"))
      (cond 
        ((= kword "Yes") (pri_new 1))
        ((= kword "No") (dil_spds_new 1 T))
      )
    )
  )
)

;---------------------------функция хибы--------------------------------------------------
(Defun ff1 (dis11 dis22 x1 x2) 
  (if 
    (and (>= dis11 (- x1 3.0001)) 
         (<= dis11 (+ x1 3.0001))
         (>= dis22 (- x2 3.0001))
         (<= dis22 (+ x2 3.0001))
    )
    T
    nil
  )
)

(Defun ff2 (dis11 dis22 x1 x2) 
  (if 
    (and (>= dis11 (- x1 0.0001)) 
         (<= dis11 (+ x1 0.0001))
         (>= dis22 (- x2 0.0001))
         (<= dis22 (+ x2 0.0001))
    )
    T
    nil
  )
)

;-------------------функция праверки фарматау--------------------------------------------
(defun vformat () 
  (if (> dis1 dis2) 
    (progn 
      (SETQ list1 "А")
      (SETQ format (COND 
                     ((ff2 dis1 dis2 630 297) "А4х3 (630.00 x 297.00 мм)")
                     ((ff2 dis1 dis2 841 297) "А4х4 (841.00 x 297.00 мм)")
                     ((ff2 dis1 dis2 1051 297) "А4х5 (1051.00 x 297.00 мм)")
                     ((ff2 dis1 dis2 1261 297) "А4х6 (1261.00 x 297.00 мм)")
                     ((ff2 dis1 dis2 1471 297) "А4х7 (1471.00 x 297.00 мм)")
                     ((ff2 dis1 dis2 1682 297) "А4х8 (1682.00 x 297.00 мм)")
                     ((ff2 dis1 dis2 1892 297) "А4х9 (1892.00 x 297.00 мм)")
                     ((ff2 dis1 dis2 891 420) "А3х3 (891.00 x 420.00 мм)")
                     ((ff2 dis1 dis2 1189 420) "А3х4 (1189.00 x 420.00 мм)")
                     ((ff2 dis1 dis2 1486 420) "А3х5 (1486.00 x 420.00 мм)")
                     ((ff2 dis1 dis2 1783 420) "А3х6 (1783.00 x 420.00 мм)")
                     ((ff2 dis1 dis2 2080 420) "А3х7 (2080.00 x 420.00 мм)")
                     ((ff2 dis1 dis2 1261 594) "А2х3 (1261.00 x 594.00 мм)")
                     ((ff2 dis1 dis2 1682 594) "А2х4 (1682.00 x 594.00 мм)")
                     ((ff2 dis1 dis2 2102 594) "А2х5 (2102.00 x 594.00 мм)")
                     ((ff2 dis1 dis2 1783 841) "А1х3 (1783.00 x 841.00 мм)")
                     ((ff2 dis1 dis2 2378 841) "А1х4 (2378.00 x 841.00 мм)")
                     ((ff2 dis1 dis2 1682 1189) "А0х2 (1682.00 x 1189.00 мм)")
                     ((ff2 dis1 dis2 2523 1189) "А0х3 (2523.00 x 1189.00 мм)")
                     ((ff2 dis1 dis2 1189 841) "А0 (1189.00 x 841.00 мм)")
                     ((ff2 dis1 dis2 841 594) "А1 (841.00 x 594.00 мм)")
                     ((ff2 dis1 dis2 594 420) "А2 (594.00 x 420.00 мм)")
                     ((ff2 dis1 dis2 420 297) "А3 (420.00 x 297.00 мм)")
                     ((ff2 dis1 dis2 297 210) "А4 (297.00 x 210.00 мм)")
                     (T nil)
                   )
      )
    ) ;то
    (progn 
      (SETQ list1 "К")
      (SETQ format (COND 
                     ((ff2 dis2 dis1 630 297) "А4х3 (297.00 x 630.00 мм)")
                     ((ff2 dis2 dis1 841 297) "А4х4 (297.00 x 841.00 мм)")
                     ((ff2 dis2 dis1 1051 297) "А4х5 (297.00 x 1051.00 мм)")
                     ((ff2 dis2 dis1 1261 297) "А4х6 (297.00 x 1261.00 мм)")
                     ((ff2 dis2 dis1 1471 297) "А4х7 (297.00 x 1471.00 мм)")
                     ((ff2 dis2 dis1 1682 297) "А4х8 (297.00 x 1682.00 мм)")
                     ((ff2 dis2 dis1 1892 297) "А4х9 (297.00 x 1892.00 мм)")
                     ((ff2 dis2 dis1 891 420) "А3х3 (420.00 x 891.00 мм)")
                     ((ff2 dis2 dis1 1189 420) "А3х4 (420.00 x 1189.00 мм)")
                     ((ff2 dis2 dis1 1486 420) "А3х5 (420.00 x 1486.00 мм)")
                     ((ff2 dis2 dis1 1783 420) "А3х6 (420.00 x 1783.00 мм)")
                     ((ff2 dis2 dis1 2080 420) "А3х7 (420.00 x 2080.00 мм)")
                     ((ff2 dis2 dis1 1261 594) "А2х3 (594.00 x 1261.00 мм)")
                     ((ff2 dis2 dis1 1682 594) "А2х4 (594.00 x 1682.00 мм)")
                     ((ff2 dis2 dis1 2102 594) "А2х5 (594.00 x 2102.00 мм)")
                     ((ff2 dis2 dis1 1783 841) "А1х3 (841.00 x 1783.00 мм)")
                     ((ff2 dis2 dis1 2378 841) "А1х4 (841.00 x 2378.00 мм)")
                     ((ff2 dis2 dis1 1682 1189) "А0х2 (1189.00 x 1682.00 мм)")
                     ((ff2 dis2 dis1 2523 1189) "А0х3 (1189.00 x 2523.00 мм)")
                     ((ff2 dis2 dis1 1189 841) "А0 (841.00 x 1189.00 мм)")
                     ((ff2 dis2 dis1 841 594) "А1 (594.00 x 841.00 мм)")
                     ((ff2 dis2 dis1 594 420) "А2 (420.00 x 594.00 мм)")
                     ((ff2 dis2 dis1 420 297) "А3 (297.00 x 420.00 мм)")
                     ((ff2 dis2 dis1 297 210) "А4 (210.00 x 297.00 мм)")
                     (T nil)
                   )
      )
      ;;закрывает иначе
    ) ;закрывает сам иф
  ) ;закрывает верх иф

  (if (= format nil) 
    (if (> dis1 dis2) 
      (progn 
        (SETQ list1 "А")
        (SETQ formatfalse T)
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
        (SETQ list1 "К")
        (SETQ formatfalse T)
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



;-------------------надрукаванне уручную--------------------------------------------------
(DEFUN c:pri (/ formatfalse cmd dis1 dis2 mashrus point1 point2 color struser fi0 
              list1 data data1 filename2 file2
             ) 
  (if (= (getvar "BLOCKEDITOR") 1)
    (progn
      (alert "Пожалуйста, закройте Редактор блоков перед печатью!")
      (exit)
    )
  )
  (SETQ formatfalse nil)
  (setq nameris nil shifr nil numa nil)
  ; праверка даты и выхад если надо
  ;;;  (setq data (atoi (rtos (getvar "CDATE") 2 0)))
  ;;;  (setq data1 (atoi (rtos (- (getvar "CDATE") (- (getvar "DATE") (getvar "TDUUPDATE"))) 2 0)))
  ;;;;;;  (if (or (> data 20200101) (> data1 20200101))
  ;;;;;;    (progn
  ;;;;;;      (alert "Тэрмін дзеянне праграммы скончыуся 01-05-2018\nзвярніцесь да вытворцы(за больш актуальнай версіяй)")
  ;;;;;;      (exit)
  ;;;;;;     )
  ;;;;;;  )
  ;;;  ;енд
  (if (not *pri-banner-shown*)
    (progn
      (PRINC "\n---------------------------------------------------------------------------\n")
      (PRINC "Праграма распрацавана на lisp, prajdziswet-ам (Косаў Уладзімір) у 2014 годзе\n")
      (PRINC "-----------------------------------------------------------------------------\n")
      (PRINC "\n")
      (setq *pri-banner-shown* T)
    )
  )
  (SETQ cmd (GETVAR "cmdecho"))
  (SETVAR "cmdecho" 1)
  (SETQ point1 (GETPOINT "Першая кропка рамкі: "))
  (PRINC point1)
  (SETQ point2 (GETCORNER point1 "\nДругая кропка рамкі(па ўскосіне): "))
  (PRINC point2)
  (PRINC "\n")
  (SETQ dis1 (ABS (- (CAR point1) (CAR point2))))
  (SETQ dis2 (ABS (- (CADR point2) (CADR point1))))

  ;;вызначэнне маштабу 1-1 або 1-100
  (if (> dis1 dis2) 
    (if (< dis1 27000) 
      (SETQ mashrus 1)
      (progn 
        (SETQ dis1 (/ dis1 100))
        (SETQ dis2 (/ dis2 100))
        (SETQ mashrus 0.01)
      )
    )
    (if (< dis2 27000) 
      (SETQ mashrus 1)
      (progn 
        (SETQ dis1 (/ dis1 100))
        (SETQ dis2 (/ dis2 100))
        (SETQ mashrus 0.01)
      )
    )
  )

  (if (and (/= mash1 0) (/= mash1 nil)) 
    (setq mashrus mash1)
  )

  (setq nameris nil
        numa    nil
        shifr   nil
        is-big  nil
  )
  (if (and point1 point2) 
    (progn 
      ;; Для определения штампа нужно использовать координаты WCS, 
      ;; так как vla-GetBoundingBox возвращает WCS
      (setq wcs-pt1 (trans point1 1 0))
      (setq wcs-pt2 (trans point2 1 0))
      
      (setq x2 (list (max (car wcs-pt1) (car wcs-pt2)) 
                     (min (cadr wcs-pt1) (cadr wcs-pt2))
               )
      )
      (setq x1 (list (min (car wcs-pt1) (car wcs-pt2)) 
                     (max (cadr wcs-pt1) (cadr wcs-pt2))
               )
      )
      
      ;; В ручной печати mashrus равен 0.01 для масштаба 1:100.
      ;; В функциях поиска штампа (из dil_print19) mash должен совпадать с mashrus
      (setq mash mashrus)
      
      (if (boundp 'find-stamp-blocks) 
        (progn 
          ;; Зумируем к выбранной рамке, чтобы ssget "_C" смог найти текст на экране
          (vl-cmdf "_zoom" "_W" point1 point2)
          
          ;; 1. Определяем большой/малый штамп
          (if (boundp 'is-big-stamp-p) 
            (setq is-big (is-big-stamp-p))
          )

          ;; 2. Поиск в блоках (стандартный)
          (if (boundp 'find-stamp-blocks) 
            (find-stamp-blocks)
          )

          ;; 3. СПДС fallback — если поля пустые, ищем SPDSFORMAT в выделенной области
          (if (or (= shifr nil) (= shifr "")
                  (= nameris nil) (= nameris "")
                  (= numa nil) (= numa 0) (= numa ""))
            (progn
              (setq spds-nabor (ssget "_X"
                                 (list (cons 0 "SPDSFORMAT")
                                       (cons 410 (getvar "ctab"))
                                 )
                               )
              )
              (if spds-nabor
                (progn
                  (setq spds-i 0)
                  (while (and (< spds-i (sslength spds-nabor))
                              (or (= shifr nil) (= shifr "")
                                  (= nameris nil) (= nameris "")
                                  (= numa nil) (= numa 0) (= numa ""))
                         )
                    (setq spds-ename (ssname spds-nabor spds-i))
                    (setq spds-vla (vlax-ename->vla-object spds-ename))
                    (setq spds-catch (vl-catch-all-apply 'vla-GetBoundingBox
                                       (list spds-vla 'spds-minpt 'spds-maxpt)
                                     )
                    )
                    (if (not (vl-catch-all-error-p spds-catch))
                      (progn
                        (setq spds-min (vlax-safearray->list spds-minpt))
                        (setq spds-max (vlax-safearray->list spds-maxpt))
                        ;; Проверка пересечения BoundingBox СПДС с выделенной областью
                        (if (not (or (< (car spds-max) (min (car wcs-pt1) (car wcs-pt2)))
                                     (> (car spds-min) (max (car wcs-pt1) (car wcs-pt2)))
                                     (< (cadr spds-max) (min (cadr wcs-pt1) (cadr wcs-pt2)))
                                     (> (cadr spds-min) (max (cadr wcs-pt1) (cadr wcs-pt2)))
                                )
                            )
                          (progn
                            ;; Извлечение xdata из СПДС рамки
                            (setq spds-list1 (entget spds-ename))
                            
                            ;; Обновляем координату x2 (правый нижний угол) точно по рамке СПДС,
                            ;; чтобы функции поиска текста (nameshifr, namelist) искали в правильном месте
                            (setq x2 (list (car spds-max) (cadr spds-min)))
                            
                            ;; Номер листа
                            (if (or (= numa nil) (= numa 0) (= numa ""))
                              (progn
                                (setq spds-numa (cdr (assoc 300 (member '(301 . "Sheet") spds-list1))))
                                (if (and spds-numa (boundp 'norma_n2))
                                  (setq numa (norma_n2 spds-numa))
                                )
                              )
                            )
                            
                            ;; Шифр
                            (if (or (= shifr nil) (= shifr ""))
                              (progn
                                (setq spds-shifr (cdr (assoc 300 (member '(301 . "Designation") spds-list1))))
                                (if (not spds-shifr)
                                  (setq spds-shifr (cdr (assoc 300 (member '(301 . "Name") spds-list1))))
                                )
                                (if spds-shifr
                                  (setq shifr (vl-string-trim " " spds-shifr))
                                )
                              )
                            )
                            
                            ;; Большой/малый штамп (из СПДС)
                            (setq is-big (= 1 (cdr (assoc 290 (member '(301 . "First sheet") spds-list1)))))
                            
                            ;; Название (только если штамп большой)
                            (if (and is-big (or (= nameris nil) (= nameris "")))
                              (progn
                                (setq spds-name1 (cdr (assoc 300 (member '(301 . "Drawing type") spds-list1))))
                                (setq spds-name2 (cdr (assoc 300 (member '(301 . "Drawing type1") spds-list1))))
                                (setq spds-name3 (cdr (assoc 300 (member '(301 . "Drawing type2") spds-list1))))
                                (setq spds-nameris "")
                                (if spds-name1 (setq spds-nameris (strcat spds-nameris spds-name1)))
                                (if spds-name2 (setq spds-nameris (strcat spds-nameris spds-name2)))
                                (if spds-name3 (setq spds-nameris (strcat spds-nameris spds-name3)))
                                (setq spds-nameris (vl-string-trim " " spds-nameris))
                                (if (/= spds-nameris "")
                                  (setq nameris spds-nameris)
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                    (setq spds-i (1+ spds-i))
                  )
                )
              )
            )
          )

          ;; 4. Поиск в тексте (наложенный текст имеет приоритет над атрибутами СПДС)
          (if (and is-big (boundp 'namelist)) 
            (progn
              (setq tmp-nameris (namelist))
              (if (and tmp-nameris (/= tmp-nameris "")) (setq nameris tmp-nameris))
            )
          )
          (if (not is-big) (setq nameris nil))
          
          (if (boundp 'numar_s) 
            (progn
              (setq tmp-num (numar_s))
              (if (and tmp-num (/= tmp-num "") (/= tmp-num 0)) (setq numa tmp-num))
            )
          )
          
          (if (boundp 'nameshifr) 
            (progn
              (setq tmp-shifr (nameshifr is-big))
              (if (and tmp-shifr (/= tmp-shifr "")) (setq shifr tmp-shifr))
            )
          )
          
          (if (or (= shifr nil) (= shifr ""))
            (progn
              (if (boundp 'get-shifr-from-blocks) 
                (setq shifr (get-shifr-from-blocks is-big))
              )
              (if (= shifr nil) (setq shifr ""))
            )
          )

          ; Очистка текста от форматирования MTEXT (если функция доступна)
          (if (boundp 'clear-mtext)
            (progn
              (if (and nameris (= (type nameris) 'STR)) (setq nameris (clear-mtext nameris)))
              (if (and shifr (= (type shifr) 'STR)) (setq shifr (clear-mtext shifr)))
              (if (and numa (= (type numa) 'STR)) (setq numa (clear-mtext numa)))
            )
          )

          ; Сбор диагностики для вывода в конце
          (if (not (boundp '*pri-final-diagnostics*)) (setq *pri-final-diagnostics* nil))
          (setq *pri-final-diagnostics* 
                (append *pri-final-diagnostics* 
                        (list (strcat "  Назв: " (if nameris nameris "nil")
                                      " | Шифр: " (if shifr (if (= shifr "") "nil" shifr) "nil")
                                      " | Номер: " (if numa (vl-princ-to-string numa) "nil")
                              )
                        )
                )
          )
        )
      )
    )
  )

  (vformat)

  (if (and (/= format nil) (/= (GETVAR "LOCALE") "ENG")) 
    (progn 
      (if (= formatfalse T) 
        (alert 
          "Фармат рысунка адрозніваецца менш чым 3 мм \n(або менш 300 мм для 100 маштабу).\nНа запыт назвы рысунка адкажыце на пытанне
або націсніце \"Esc\"-каб адмовіцца ад друку"
        )
      )

      ;задаванне назова малюнка
      (if (= AddDwgBool nil) 
        (SETQ struser (ask_pdf_print ""))
        (setq struser (strcat (if (= colordwg 1) (princ "+") (princ "")) 
                              namememory
                      )
        )
      )
      (if (= struser nil) 
        (progn 
          (SETVAR "cmdecho" cmd)
          (princ "\nОтменено пользователем.")
          (exit)
        )
      )
      (if (wcmatch struser "+*") 
        (progn 
          (setq struser (SUBSTR struser 2))
          (setq color "acad")
        )
        (setq color "monochrome.ctb")
      )
      (COND 
        ((and (= (STRLEN struser) 2) (wcmatch struser "00*"))
         (SETQ fileuser (SUBSTR (GETVAR "dwgname") 
                                1
                                (- (STRLEN (GETVAR "dwgname")) 4)
                        )
         )
        )
        ((and (> (STRLEN struser) 2) (wcmatch struser "00*"))
         (SETQ fileuser (STRCAT 
                          (SUBSTR (GETVAR "dwgname") 
                                  1
                                  (- (STRLEN (GETVAR "dwgname")) 4)
                          )
                          (SUBSTR struser 3 (- (STRLEN struser) 2))
                        )
         )
        )
        ((= struser "")
         (SETQ fileuser (SUBSTR (GETVAR "dwgname") 
                                1
                                (- (STRLEN (GETVAR "dwgname")) 4)
                        )
         )
        )
        (T (SETQ fileuser struser))
      )

      (setq pure_filename fileuser)
      (setq filename2 (STRCAT (GETVAR "dwgprefix") fileuser "+1.pdf"))
      (SETQ fileuser (STRCAT (GETVAR "dwgprefix") fileuser ".pdf"))

      ;закрытие файла если он открыт тут (на этом компе
      (if (= (setq fi0 (open fileuser "w")) nil) 
        ;(exit_pdf)
        (alert "Дадзены pdf файл адчынены, калі ласка зачыніце і пасля націсніце ОК")
      )
      (if (/= nil fi0) (close fi0))

      ;проверка закрытия файла, если всетаки еще открыт "+1"
      (if (= (setq fi0 (open fileuser "w")) nil) 
        ;(exit_pdf)
        (progn 
          (setq file2    T
                fileuser filename2
          )
        )
      )
      (if (/= nil fi0) (close fi0))

      (if (= (setq fi0 (open fileuser "w")) nil) 
        ((alert "Файл ужо адчыненны іншай праграмай,\nабо немагчыма запісаць у гэты каталог (няма праў)") 
          (if (/= nil fi0) (close fi0))
        )
        (progn 
          (if (/= nil fi0) (close fi0))
          (if (or (wcmatch format "А0х2*") (wcmatch format "А0х3*")) 
            (alert "Даннй формат(А0х2 или А0х2) будет перевен в pdf,\n но технически его невозможно будет распечатать")
          )
          (vl-file-delete fileuser)
          (SETVAR "cmdecho" 0)
          (if (= (GETVAR "tilemode") 0)
            (command "_plot" ;Сама команда
                     "_y" ;Выполнить детальное задание конфигурации?:
                     "" ;Имя листа или <печать>:
                     "_DWG в PDF ЭТО(Silent).pc3" ;Имя устройства вывода:
                     format ;Формат листа бумаги
                     "_Millimeters" ;;Единицы измерения размеров листа :
                     list1 ;;Ориентация чертежа [Книжная/Альбомная]:
                     "_N" ;;Перевернуть чертеж? :
                     "Рамка" ;;Печатаемая область [Экран/Границы/Лист/Вид/Рамка]:
                     point1 ;;першая коордыната
                     point2 ;;другая коордяната
                     mashrus ;;Масштаб печати :
                     "_center" ;;Смещение от начала(x,y)или[Центрировать]:
                     "_y" ;;Учитывать стили печати? [Да/Нет]:
                     color ;;Имя таблицы стилей печати:
                     "_y" ;;Учитывать веса линий? [Да/Нет]:
                     "_N" ;;Масштабировать веса линий?:
                     "_N" ;;Печатать объекты листа первыми?:
                     "_N" ;;Скрывать объекты листа?:
                     fileuser ;;Введите имя файла :
                     "_n" ;;Сохранить изменения параметров листа
                     "_y" ;;Перейти к печати [Да/Нет]
            )
            ;;калі модель
            (command "_plot" ;Сама команда
                     "_y" ;Выполнить детальное задание конфигурации?:
                     "" ;Имя листа или <печать>:
                     "_DWG в PDF ЭТО(Silent).pc3" ;Имя устройства вывода:
                     format ;Формат листа бумаги
                     "_Millimeters" ;;Единицы измерения размеров листа :
                     list1 ;;Ориентация чертежа [Книжная/Альбомная]:
                     "_N" ;;Перевернуть чертеж? :
                     "Рамка" ;;Печатаемая область [Экран/Границы/Лист/Вид/Рамка]:
                     point1 ;;першая коордыната
                     point2 ;;другая коордяната
                     mashrus ;;Масштаб печати :
                     "_center" ;;Смещение от начала(x,y)или[Центрировать]:
                     "_y" ;;Учитывать стили печати? [Да/Нет]:
                     color ;;Имя таблицы стилей печати:
                     "_y" ;;Учитывать веса линий? [Да/Нет]:
                     "_N" ;;Режим вывода тонированных ВЭ:
                     fileuser ;;Введите имя файла :
                     "_n" ;;Сохранить изменения параметров листа
                     "_y" ;;Перейти к печати [Да/Нет]
            )
          )
          (SETVAR "cmdecho" cmd)
          (setq printtrue T)
          (if (findfile fileuser)
            (setq *pri-final-diagnostics* (append *pri-final-diagnostics* (list (STRCAT "  Файл захаваны: " fileuser))))
            (setq *pri-final-diagnostics* (append *pri-final-diagnostics* (list (STRCAT "  Файл не быў захаваны (магчыма, адключана печать): " fileuser))))
          )
          (princ)
        )
      )
    )
    (alert "Недакладны формат - \nтакога фармату неіснуе або памылка\n(праграмма дзенічае ў маштабе 1:1\n або 1:100 і ў русскай версіі аўтакада)...")
  )
  (if (= printtrue T) 
    (progn 
      ;(exit_pdf)
      (if (OR (= nil sufix) (and (/= nil sufix) (/= (substr sufix 1 1) "r"))) 
        (progn 
          (if (= T file2) 
            (alert "Надрукавана з суфіксам +1, так як іншы файл быу адчынены")
            (progn 
              (print "пдф-нанадрукован, глядзі у тэчке,але наўсяляк праверце дату ")
              (princ)
            )
          )

          (setq single-ret (strcat pure_filename 
                                   ".pdf"
                                   ">"
                                   (if (boundp 'escape-delimiters) 
                                     (escape-delimiters (if shifr shifr ""))
                                     (if shifr shifr "")
                                   )
                                   ">"
                                   (if (boundp 'escape-delimiters) 
                                     (escape-delimiters (if nameris nameris ""))
                                     (if nameris nameris "")
                                   )
                                   ">"
                                   (if (boundp 'escape-delimiters) 
                                     (escape-delimiters 
                                       (if numa (vl-princ-to-string numa) "")
                                     )
                                     (if numa (vl-princ-to-string numa) "")
                                   )
                                   "<"
                           )
          )
          (setq single-ret (strcat (GETVAR "dwgprefix") "&false&false&" single-ret))
          (if (boundp 'add_edit) 
            (add_edit nil single-ret)
            (progn 
              (princ 
                (strcat "\n--- СТРОКА ДЛЯ СЛИЯНИЯ ---\n" 
                        single-ret
                        "\n--------------------------\n"
                )
              )
              (startapp
                "e:\\praca-proect\\_program\\_Програм\\__скончаные\\__скончаные\\_Autocad\\WPFMergeExe\\WPFMergeExe\\bin\\Debug\\net48\\WPFMergeExe.exe"
                (strcat "\"" single-ret "\"")
              )
            )
          )
        )
        ;(alert "пдф-нанадрукован, глядзі у тэчке")
      )
    )
  ) ;end if
)

(DEFUN c:зкш () 
  (c:pri)
)

;--------------------------функция перадачы праз разлучонную памяць--------------------------------------------
(defun PrintNewDcl (/ dcl_file file_handle dcl_id) 
  (setq dcl_file (vl-filename-mktemp "print_new.dcl"))
  (setq file_handle (open dcl_file "w"))
  (write-line "printnew: dialog{label=\"Ручная печать\";" file_handle)
  (write-line ":text{label=\"Для выбора рамки - Ок, для завершения - Отмена:\";key=\"txt1\";fixed_width=true;}" 
              file_handle
  )
  (write-line ":toggle{label=\"Цветная печать\";key=\"colordwg\";value=\"0\";}" 
              file_handle
  )
  (write-line ":edit_box {label = \"Маштаб (0 => 1:1 или 1:100):\"; key = \"mash\"; edit_width=10; value = \"0\";}" 
              file_handle
  )
  (write-line ": button { key = \"btn1\";label = \"Переключить модель/лист(-ы) чертежа\";}" 
              file_handle
  )
  (write-line "ok_cancel;" file_handle)
  (write-line "}" file_handle)
  (close file_handle)
  (setq dcl_id (load_dialog dcl_file))
  (vl-file-delete dcl_file)
  dcl_id
)


(defun pri_new (AddDwgBool / temp_start sufix filereturn filename namememory dcl_id 
                ddi colordwg printtrue mash1 spislist mod vers proc shifr nameris numa 
                is-big
               ) 
  ;----------------------------------------------------------------------
  (setq temp_start T
        filename   (SUBSTR (GETVAR "dwgname") 1 (- (STRLEN (GETVAR "dwgname")) 4))
        mash1      0
  )
  (setq i 1)
  (vl-load-com)
  (setq mod (getvar "ctab"))
  (setq spislist (append '("Model") (layoutlist)))
  (setq listpp (vl-position mod spislist)) ;
  (setq *pri-final-diagnostics* nil) ; Очистка перед новым запуском
  ;while
  (while temp_start 
    ;dialog  d:\\Косов\\prog\\_Програм\\_acad\\для панельки\\
    (setq dcl_id (PrintNewDcl))
    (if (not (new_dialog "printnew" dcl_id)) 
      (exit)
    )
    (setq pp nil)
    (set_tile "mash" (rtos mash1))
    (action_tile "btn1" "(setq listpp (1+ listpp) pp T) (done_dialog 2)")
    (action_tile "accept" 
                 "(setq mash1 (atof (get_tile \"mash\"))) (setq colordwg (atoi (get_tile \"colordwg\"))) (done_dialog 1)"
    )
    (action_tile "cancel" "(setq temp_start nil) (done_dialog 0)")
    (setq ddi (start_dialog))
    (unload_dialog dcl_id)
    ;end dialog
    (if (>= listpp (length spislist)) (setq listpp 0))
    (if (= pp T) 
      (progn 
        (setq mod (nth listpp spislist))
        (if (/= (getvar "ctab") mod) (setvar "ctab" mod))
      )
    )
    (if (and (= temp_start T) (/= pp T)) 
      (progn 
        (setq sufix     (strcat "r" (itoa i))
              printtrue nil
        )
        (setq namememory (strcat filename sufix))
        (setq i (1+ i))
        (c:pri)
        (if (/= printtrue nil) 
          (exit_pdf)
        )
        ;вызов діалога

        (if (/= filereturn nil) 
          (if (/= printtrue nil) 
            (setq filereturn (strcat filereturn 
                                     "?"
                                     namememory
                                     ".pdf"
                                     ">"
                                     (if (boundp 'escape-delimiters)
                                       (escape-delimiters (if shifr shifr ""))
                                       (if shifr shifr "")
                                     )
                                     ">"
                                     (if (boundp 'escape-delimiters)
                                       (escape-delimiters (if nameris nameris ""))
                                       (if nameris nameris "")
                                     )
                                     ">"
                                     (if (boundp 'escape-delimiters)
                                       (escape-delimiters
                                         (if numa (vl-princ-to-string numa) "")
                                       )
                                       (if numa (vl-princ-to-string numa) "")
                                     )
                             )
            )
          )
          (if (/= printtrue nil) 
            (setq filereturn (strcat namememory 
                                     ".pdf"
                                     ">"
                                     (if (boundp 'escape-delimiters)
                                       (escape-delimiters (if shifr shifr ""))
                                       (if shifr shifr "")
                                     )
                                     ">"
                                     (if (boundp 'escape-delimiters)
                                       (escape-delimiters (if nameris nameris ""))
                                       (if nameris nameris "")
                                     )
                                     ">"
                                     (if (boundp 'escape-delimiters)
                                       (escape-delimiters
                                         (if numa (vl-princ-to-string numa) "")
                                       )
                                       (if numa (vl-princ-to-string numa) "")
                                     )
                             )
            )
          )
        )
      )
    ) ;end if
  ) ;end while

  (if (/= filereturn nil) 
    (progn 
      (if (and (boundp '*pri-final-diagnostics*) *pri-final-diagnostics*)
        (progn
          (princ "\n\n=== ИТОГИ ПЕЧАТИ ===")
          (foreach diag *pri-final-diagnostics*
            (princ (strcat "\n" diag))
          )
          (princ "\n====================\n")
          (setq *pri-final-diagnostics* nil)
        )
      )
      
      (SETQ filereturn (STRCAT (GETVAR "dwgprefix") "&false&true&" filereturn "<"))

      ;адпраука файлау у редактор
      (if (boundp 'add_edit) 
        (add_edit AddDwgBool filereturn)
        (progn 
          (princ 
            (strcat "\n--- СТРОКА ДЛЯ СЛИЯНИЯ ---\n" 
                    filereturn
                    "\n--------------------------\n"
            )
          )
          (startapp
            "e:\\praca-proect\\_program\\_Програм\\__скончаные\\__скончаные\\_Autocad\\WPFMergeExe\\WPFMergeExe\\bin\\Debug\\net48\\WPFMergeExe.exe"
            (strcat "\"" filereturn "\"")
          )
          (command)
        )
      ) ;vers
    )
  ) ;end if
  (setq AddDwgBool nil)
)

;выхад з пдф
(defun exit_pdf () 
  (startapp "__prog\\exe\\pdf_exit.exe")
  (command)
);выхад з пдф




