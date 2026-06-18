(vl-load-com)

(defun check-vlx-update (/ dirpol temp modul dataupdate linestr server-vlx local-vlx) 
  ; 1. Вызначэнне dirpol (шляху), калі ён не зададзены глабальна ці праз vl-bb-set
  (setq dirpol (vl-bb-ref 'dirpol))
  (if (not dirpol) 
    (progn 
      (setq temp (getvar "ACADPREFIX"))
      (while 
        (and temp 
             (not 
               (wcmatch (substr temp 1 (vl-string-search ";" temp 0)) 
                        "*Users*upport*"
               )
             )
        )
        (setq temp (substr temp 
                           (+ 1 (vl-string-search ";" temp 0))
                           (- (strlen temp) (+ 1 (vl-string-search ";" temp 0)))
                   )
        )
      )
      (if (and temp (/= (vl-string-search ";" temp 0) nil)) 
        (setq dirpol (strcat (substr temp 1 (vl-string-search ";" temp 0)) "\\"))
      )
      (if (or (= dirpol nil) (= dirpol "\\")) 
        (setq dirpol (strcat "C:/Users/" 
                             (getvar "LOGINNAME")
                             "/AppData/Roaming/Autodesk/AutoCAD "
                             "/R"
                             (substr (getvar "ACADVER") 1 4)
                             "/rus/Support/"
                     )
        )
      )
    )
  )

  ; 2. Дапаможная функцыя праверкі даты файла
  (defun checkdata (data1 data2 / temp1 temp2 t1 t2 t3 t4) 
    (if (< (nth 1 data1) 10) 
      (setq t1 (strcat "0" (rtos (nth 1 data1))))
      (setq t1 (rtos (nth 1 data1)))
    )
    (if (< (nth 3 data1) 10) 
      (setq t2 (strcat "0" (rtos (nth 3 data1))))
      (setq t2 (rtos (nth 3 data1)))
    )
    (if (< (nth 4 data1) 10) 
      (setq t3 (strcat "0" (rtos (nth 4 data1))))
      (setq t3 (rtos (nth 4 data1)))
    )
    (if (< (nth 5 data1) 10) 
      (setq t4 (strcat "0" (rtos (nth 5 data1))))
      (setq t4 (rtos (nth 5 data1)))
    )
    (setq temp1 (strcat (rtos (nth 0 data1)) t1 t2 t3 t4))

    (if (< (nth 1 data2) 10) 
      (setq t1 (strcat "0" (rtos (nth 1 data2))))
      (setq t1 (rtos (nth 1 data2)))
    )
    (if (< (nth 3 data2) 10) 
      (setq t2 (strcat "0" (rtos (nth 3 data2))))
      (setq t2 (rtos (nth 3 data2)))
    )
    (if (< (nth 4 data2) 10) 
      (setq t3 (strcat "0" (rtos (nth 4 data2))))
      (setq t3 (rtos (nth 4 data2)))
    )
    (if (< (nth 5 data2) 10) 
      (setq t4 (strcat "0" (rtos (nth 5 data2))))
      (setq t4 (rtos (nth 5 data2)))
    )
    (setq temp2 (strcat (rtos (nth 0 data2)) t1 t2 t3 t4))

    (> (atof temp1) (atof temp2))
  )

  ; 3. Чытанне Update.txt і праверка абнаўлення acad.vlx
  (if (setq modul (open (strcat dirpol "__prog/Update.txt") "r")) 
    (progn 
      (setq dataupdate (vl-string-trim " " (read-line modul))
            linestr    (read-line modul)
      )
      (close modul)

      (if (and linestr (/= (vl-string-trim " " linestr) "")) 
        (progn 
          (setq server-vlx (strcat (vl-string-trim " " linestr) "acad.vlx"))
          (setq local-vlx (strcat dirpol "acad.vlx"))
          (if 
            (and (findfile server-vlx) 
                 (findfile local-vlx)
                 (checkdata 
                   (vl-file-systime server-vlx)
                   (vl-file-systime local-vlx)
                 )
            )
            (alert 
              (strcat "Даступна абнаўленне загрузчыка (acad.vlx)!\n\n" 
                      "Скапіруйце файл:\n" server-vlx "\n\nУ тэчку:\n" dirpol
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

; Запуск функцыі
(check-vlx-update)
(princ "\nСкрыпт праверкі acad.vlx паспяхова выкананы.")
(princ)
