(vl-load-com)
(setq layerListRestore nil)
;--------------пробы--------------------------
(defun try (func input / result)
  (if (vl-catch-all-error-p (setq result (vl-catch-all-apply func (list input))))
    (print (strcat "\nОшибка обработки: " (vl-catch-all-error-message result)))
  )
)

(defun macro (msg) (setvar "MODEMACRO" (if msg msg "")))

;-
;переключеніе
(defun viewswitch (/ acadObj doc activeVport)
  
  ;; Получаем доступ к приложению и текущему документу
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  
  ;; Получаем объект текущего видового экрана
  (setq activeVport (vla-get-ActiveViewport doc))

  ;; ----------------- ШАГ 1: ВИД СБОКУ -----------------
  
  ;; Задаем вектор направления (Direction). 
  ;; Функция vlax-3d-point переводит список в нужный для VLA формат массива (SafeArray)
  (vla-put-Direction activeVport (vlax-3d-point '(-1.0 0.0 0.0)))
  
  ;; ВАЖНО: Применяем изменения к экрану (без этой строки экран не повернется!)
  (vla-put-ActiveViewport doc activeVport)
  
  ;; Делаем зум по границам, чтобы чертеж не улетел за край экрана
  (vla-ZoomExtents acadObj)
  
  (princ "\nВид: Сбоку")
  
  ;; Ждем реакции пользователя. Если нажмет Esc - выполнение скрипта прервется здесь
  (getstring "\nНажмите Enter клавишу для вида сверху или Esc если хотите остаться...")

  ;; ----------------- ШАГ 2: ВИД СВЕРХУ -----------------
  
  ;; Меняем вектор на вид сверху (Z = 1)
  (vla-put-Direction activeVport (vlax-3d-point '(0.0 0.0 1.0)))
  
  ;; Снова применяем изменения
  (vla-put-ActiveViewport doc activeVport)
  
  ;; Снова делаем зум
  (vla-ZoomExtents acadObj)
  
  (princ "\nВид: Сверху")
  (princ)
)
;; Командная версия с префиксом "c:", доступная из командной строки
(defun c:viewswitch (/)
  (viewswitch)
  (princ)
)
;использованием команд-с
(defun command-new (args / cmd-func cmd-args res)
  ;; Выбор функции
  (setq cmd-func (if command-s 'command-s 'command)
        cmd-args args)

  ;; Очистка ВСЕХ "" в конце списка специально для command-s
  (if command-s
    (while (and cmd-args (= (last cmd-args) ""))
      (setq cmd-args (reverse (cdr (reverse cmd-args))))
    )
  )

  ;; Запуск команды
  (setq res (vl-catch-all-apply cmd-func cmd-args))

  ;; Если ошибка — выводим сообщение и ПРЕРЫВАЕМ всё
  (if (vl-catch-all-error-p res)
    (progn
      (princ (strcat "\n\n[КРИТИЧЕСКАЯ ОШИБКА] Команда не выполнена: " (vl-prin1-to-string args)))
      (princ "\nВыполнение скрипта остановлено.")
      (exit) ; Полная остановка LISP
    )
	(BackToText)
  )
  res
)

;----------------------------------------------------------------------
;; ИСПРАВЛЕННАЯ ФУНКЦИЯ: RestoreLayersDefault
;; Назначение:
;;   Восстанавливает ВСЕ исходные состояния слоёв из списка layerList,
;;   включая комбинации состояний (выключен+заморожен, выключен+заблокирован,
;;   выключен+заморожен+заблокирован и т.д.)
;;   Использует VLA методы 
;;----------------------------------------------------------------------
(defun RestoreLayersDefaultLock (layerList / layers layer layer1 layerName
                            flag62 flag70 freezeState lockState visibleState)
  
 (setq layers (vla-get-Layers doc))

    ;; НАЧИНАЕМ ТРАНЗАКЦИЮ ТОЛЬКО ДЛЯ ИЗМЕНЕНИЙ СЛОЕВ
  (vlax-for layer layers
    (setq layerName (vla-get-Name layer) result nil)
(setq result
  (vl-some
    (function (lambda (item)
      (if (equal (car item) layerName)
        (cdr item)  ; возвращаем весь элемент: 
        nil
      )
    ))
    layerList
  )
)
    
    ;; Проверяем, существует ли слой и он входит в список восстаовления
    (if (and result (tblsearch "LAYER" layerName))
      (progn
        
        ;; Извлекаем оригинальные значения флагов
        (setq flag62 (cdr (assoc 62 result))
              flag70 (cdr (assoc 70 result)))
        
     
        ;; Определяем состояния из оригинальных флагов
        (setq freezeState (or (= flag70 1) (= flag70 5))  ; заморожен если flag70 = 1 или 5
              lockState (or (= flag70 4) (= flag70 5))    ; заблокирован если flag70 = 4 или 5
              visibleState (< flag62 0))                 ; видимый если flag62 <= 0
        
        ;; Восстанавливаем состояния в правильном порядке
        ;; Сначала видимость, потом заморозка, потом блокировка
	(if visibleState
	(vla-put-LayerOn layer :vlax-false)
	  )

	(if freezeState
	(vla-put-Freeze layer :vlax-true)
	  )
                
        (if lockState
	(vla-put-Lock layer :vlax-true)
	  )
        
      );end progn
    );end if
  );vlax-for
;(vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports)

)

;; Мини-функция для возврата фокуса на текстовое окно (только для режима Ленты)
(defun BackToText ()
  (if (= is_ribbon 1)
    ;; Если документ свернут, возвращаем текст на передний план
      (textscr)
  )
)


;restore layers
(defun C:RestoreLayersLock (/ acadApp doc)
    (setq acadApp (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acadApp))
  (if layerListRestore
  (RestoreLayersDefaultLock layerListRestore))
  )
  
;__________________________________________________Main function--------------------------------------
(defun c:SuperFlatten ( / proverkanabor *error* acadApp doc cnt ss expm locklst blocks layouts jpro
                      views mspace mspacecnt lst blknamelst patlst hpa NeedsFlattening 
                      templayout blkdef inoutlst actlayout notflatlst filtered_ss 
                      expblklst expblkcnt renameflag newname newnamelst 
                      notrenamedlst optans presufstr templst orig  
                      renameans validlst testlst obj objname proxylst 
                      proxyreport pksty slu proxyerror version elevms elevps 
                      sscol WNFflag UCSflag
                      TestNormal TestZNormal ZZeroPoint ZZeroCoord 
                      ProcessList SF:MakeLWPolyline GetBlock PointList 
                      RotateToNormal ApplyProps FlatMText FlatText 
                      FlatPointObj FlatLine FlatACE FlatCircle FlatArc 
                      FlatPline FlatSpline FlatDimension FlatXref FlatShape 
                      FlatHatch FlatSolid FlatTrace FlatRayOrXline 
                      FlatWipeoutOrRaster FlatMline FlatTable FlatTolerance 
                      FlatRegion FlatPolyFaceMesh FlatCoordinates FlatMInsert 
                      AttributesToText ExpBlockMethod CommandExplode 
                      ModBlockScale SF:TraceObject CheckRename PrefixSuffix 
                      Spinbar LstACADPAT UnlockLayers RelockLayers 
                      SSVLAList Round GetNestedNames ValidItem 
                      FlatMLeader CheckBlock DeleteBlockProxies SF:GetFields 
                      SF:SymbolString WMFOutIn old_osmode old_highlight old_qaflags is_ribbon old_cliprinc old_cleanscreen
					  old_ucsfollow old_REGENMODE old_explmode old_pickstyle savevar restorevar)
                      ;; Глобальные переменные: *expans* *overkillans* *proxyans*

  ;1 захаваць налады
(defun savevar ()

  ;; Сохраняем настройки
  (setq old_osmode (getvar "OSMODE"))
  (setq old_highlight (getvar "HIGHLIGHT"))
  (setq old_qaflags (getvar "QAFLAGS")) 
  (setq old_ucsfollow (getvar "ucsfollow"))
  (setq old_REGENMODE (getvar "REGENMODE"))
  (setq old_explmode (getvar "explmode"))
  (setq old_pickstyle (getvar "pickstyle"))

  (vla-StartUndoMark doc)

  ;; ПРОВЕРКА ЛЕНТЫ
  (setq is_ribbon (if (getvar "RIBBONSTATE") (getvar "RIBBONSTATE") 0))
  
  (if (= is_ribbon 0)
    ;; РЕЖИМ КЛАССИКА: Сворачиваем само приложение целиком
    (progn
     (textscr)
      (if acadApp (vl-catch-all-apply 'vla-put-WindowState (list acadApp 2)))
      (textscr)
    )
    ;; РЕЖИМ ЛЕНТА: Сворачиваем только активный чертеж + включаем чистый экран
      (progn
  (vl-catch-all-apply 'vl-cmdf (list "_.COMMANDLINEHIDE")) 
  (if (= old_cleanscreen 0) (vl-cmdf "_.CLEANSCREENON"))
;;;  (textscr)
  ;(if doc (vl-catch-all-apply 'vla-put-WindowState (list doc 2)))
  (textscr) ;; Теперь 100% откроется отдельное окно!

	  )
  )

  ;; Отключаем тормозящие факторы
  (setvar "OSMODE" 0)      
  (setvar "HIGHLIGHT" 0)   
  (setvar "QAFLAGS" 0)     
  (setvar "REGENMODE" 0)  
  (setvar "ucsfollow" 0)
  (setvar "explmode" 1)
  (setvar "pickstyle" 0)
  (setvar "CMDECHO" 0)
  (setvar "NOMUTT" 0)
  (setvar "EXPERT" 5)
)
;2 аднавіць налады
(defun restorevar ()
   (macro nil)

  ;; ВОССТАНОВЛЕНИЕ ОКОН В ЗАВИСИМОСТИ ОТ РЕЖИМА
  (if (= is_ribbon 0)
    ;; РЕЖИМ КЛАССИКА: Разворачиваем приложение обратно
    (if acadApp (vl-catch-all-apply 'vla-put-WindowState (list acadApp 3)))
	(progn
    (vl-cmdf "_.CLEANSCREENOFF")
	(if doc (vl-catch-all-apply 'vla-put-WindowState (list doc 3)))
	 (graphscr) ;; Скрываем текстовое окно и возвращаемся к чертежу
  ;; ====================================================
  ;; ХИРУРГИЯ: Возвращаем командную строку на место!
  (vl-catch-all-apply 'vl-cmdf (list "_.COMMANDLINE"))
  ;; ====================================================
	)
  )
  
  (graphscr) ;; Скрываем текстовое окно и возвращаемся к чертежу
  
  ;; Классическое и быстрое восстановление переменных (с проверкой на nil)
  (if old_osmode (setvar "OSMODE" old_osmode))
  (if old_highlight (setvar "HIGHLIGHT" old_highlight))
  (if old_qaflags (setvar "QAFLAGS" old_qaflags))
  (if old_REGENMODE (setvar "REGENMODE" old_REGENMODE))
  (if old_ucsfollow (setvar "ucsfollow" old_ucsfollow))
  (if old_explmode (setvar "explmode" old_explmode))
  (if old_pickstyle (setvar "pickstyle" old_pickstyle))
  
  (setvar "CMDECHO" 1)
  (setvar "NOMUTT" 0)
  (setvar "EXPERT" 0)
)


;3 обработчик ошибок
  (defun *error* (msg / i)
    (cond
      ((not msg))
      ((wcmatch (strcase msg) "*QUIT*,*CANCEL*")
        (if blknamelst
          (princ "\n ** ОТМЕНЕНО - РЕКОМЕНДУЕТСЯ ОТМЕНИТЬ **\n")
        )
      )
      (T (princ (strcat "\nОшибка: " msg)))
    )
    
    ;; Безопасное удаление временного макета (глухой перехват ошибки)
    (if (and templayout (= (type templayout) 'VLA-OBJECT))
      (vl-catch-all-apply 'vla-delete (list templayout))
    )

    ;; СВЕРХБЫСТРОЕ восстановление блоков: без текста, без проверок на erased.
    ;; Если блок жив - свойство вернется. Если нет - ошибка мгновенно поглотится.
    (if expblklst
      (foreach x expblklst
        (vl-catch-all-apply 'vlax-put (list x 'Explodable acFalse))
      )
    )

    ;; Безопасное переключение пространства обратно в модель
    (if doc
      (vl-catch-all-apply 
        '(lambda () 
           (if (= 0 (vlax-get doc 'ActiveSpace))
             (vlax-put doc 'ActiveSpace 1)
           )
         )
      )
    )
    
    ;; Безопасное восстановление системных переменных
    (if hpa (vl-catch-all-apply 'setvar (list "hpassoc" hpa)))
    (if slu (vl-catch-all-apply 'setvar (list "showlayerusage" slu)))
    (if doc
      (progn
        (if elevms (vl-catch-all-apply 'vlax-put (list doc 'ElevationModelSpace elevms)))
        (if elevps (vl-catch-all-apply 'vlax-put (list doc 'ElevationPaperSpace elevps)))
      )
    )
	
	(if layerListRestore (vl-catch-all-apply 'RestoreLayersDefaultLock (list layerListRestore)))

    ;; Закрытие транзакции Undo
    (if doc (vl-catch-all-apply 'vla-EndUndoMark (list doc)))

    ;; ==============================================================
    ;; ЕДИНСТВЕННАЯ РЕГЕНЕРАЦИЯ (только активный экран)
    ;; ==============================================================
    (if doc
      (vl-catch-all-apply 'vla-Regen (list doc acActiveViewport))
    )

    ;; Восстановление переменных (разворачивание окна в самом конце)
    (vl-catch-all-apply 'restorevar)

    (princ)
  ) 
 ;end error 
  
  ;; Вспомогательная функция проверки: нужно ли плющить объект?
;; Вынесена отдельно, чтобы позже использовать и для первичного выбора.
(defun NeedsFlattening (vla-obj / minPt maxPt zMin zMax res)
  ;; По умолчанию считаем, что нужно (на случай ошибок получения BoundingBox, лучей и бесконечных линий)
  (setq res T)
  
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list vla-obj 'minPt 'maxPt))))
    (progn
      (setq zMin (caddr (vlax-safearray->list minPt))
            zMax (caddr (vlax-safearray->list maxPt)))
            
      ;; Если объект полностью лежит в плоскости Z=0 (с учетом погрешности 1e-8)
      (if (and (< (abs zMin) 1e-8) (< (abs zMax) 1e-8))
        (setq res nil) ; Плющить не нужно, он уже плоский!
      )
    )
  )
  res
)
  
  
  ;; Добавлено 1/27/2011. Изменено 2/23/2011.
  ;; Аргумент: vla-объект.
  ;; Команда zoom to object была добавлена в 2005, которая является версией 16.1.
  ;; Вложенная функция вызывает эту команду.
  (defun WMFOutIn (obj / doc space tmp tmpwmf blkref space lay clr lt  
                         explst pts newobj UpperLeft ActiveXSS 2DPoints)
    (setq doc (vla-get-activedocument (vlax-get-acad-object))
          space (vlax-get (vla-get-ActiveLayout doc) 'Block)
    )
    ;; Возвращает координаты верхнего левого угла текущего
    ;; вида в виде 3D точки. Изменено 2/24/2011.
    (defun UpperLeft ( / scrn ang vsiz vcen pt d)
      (setq scrn (getvar "screensize")
            ang (angle (list (car scrn) 0.0 0.0) (list 0.0 (cadr scrn) 0.0))
            vsiz (/ (getvar "viewsize") 2.0)
            vcen (getvar "viewctr")
            ;; Верхняя средняя точка вида.
            pt (polar vcen (/ pi 2.0) vsiz)
            d (distance pt vcen)
            ;; Выполнить тригонометрические вычисления. Получить расстояние от центра 
            ;; вида до верхнего левого угла.
            d (/ d (sin (- pi ang)))
      )
      ;; Точка в верхнем левом углу экрана.
      (polar vcen ang d)
    ) ; end UpperLeft
    ;; Аргумент: vla-объект
    ;; Возвращает: объект ActiveX selection set.
    (defun ActiveXSS (obj / ssobj i)
      (if (setq i (ValidItem sscol "tempss"))
        (vla-delete i)
      )
      (setq ssobj (vlax-invoke sscol 'Add "tempss"))
      (vlax-invoke ssobj 'AddItems (list obj))
      (vla-item sscol "tempss")
    ) ; end ActiveXSS
    ;; Удалить каждый третий элемент из плоского списка 3D точек.
    (defun 2DPoints (coord / lst)
      (repeat (/ (length coord) 3)
        (setq lst (cons (car coord) lst)
              lst (cons (cadr coord) lst)
              coord (cdddr coord)
        )
      )
      (reverse lst)
    ) ; end 2DPoints
    ;; Добавлена проверка на модельное пространство 2/9/2011. Избегает незначительного сообщения об ошибке
    ;; в командной строке относительно отсутствия активного вьюпорта модели при
    ;; выравнивании определения блока в пространстве листа.
    (if (= 1 (vlax-get doc 'ActiveSpace))
      (command-new (list "._view" "_orthographic" "_top"))
    )
    (if
      (and
        ;; получить временное имя файла
        (setq tmp (vl-filename-mktemp nil nil nil))
        (setq tmpwmf (strcat tmp ".WMF"))
      )
      (progn
        ;; WMFout
        (setq lay (vlax-get obj 'Layer)
              clr (vlax-get obj 'Color)
              lt (vlax-get obj 'Linetype)
        )
        ;; Требуется 2005 или новее для команды zoom object.
        (command "._zoom" "_object" (vlax-vla-object->ename obj) "") 
        (vla-update obj)
        (vlax-invoke doc 'Export tmp "WMF" (ActiveXSS obj))
		(BackToText)
        ;; WMFin
        (if (not (vl-catch-all-error-p
          (setq blkref (vl-catch-all-apply 'vlax-invoke 
            (list doc 'Import tmpwmf (UpperLeft) 2.0)))))
          (progn
            (princ "\rВыравнивание сплошных объектов, пожалуйста, не отменяйте... ")
            (print)            
            (setq name (vlax-get blkref 'Name))
            (setq explst (vlax-invoke blkref 'Explode))
            (vla-delete blkref)
            (command "._purge" "_blocks" name "_no") (BackToText)
            (foreach x explst 
              ;; Преобразовать тяжелые полилинии из WMFin в линии или легкие полилинии.
              (setq pts (vlax-get x 'Coordinates))
              (vla-delete x)
              (if
                (or
                  ;; Преобразовать двухточечный объект в линию.
                  (= 6 (length pts))
                  ;; Учитывая трехточечный объект и если первая и третья точки
                  ;; совпадают, преобразовать в линию.
                  (and
                    (= 9 (length pts))
                    (equal (car pts) (nth 6 pts) 1e-8)
                    (equal (cadr pts) (nth 7 pts) 1e-8)
                  )
                )
                (setq newobj (vlax-invoke space 'AddLine
                  (list (car pts) (cadr pts) 0.0)
                  (list (nth 3 pts) (nth 4 pts) 0.0)
                ))
                ;; Иначе преобразовать в lwpline.
                (setq newobj (vlax-invoke space 'AddLightWeightPolyline (2DPoints pts)))
              )             
              (vlax-put newobj 'Layer lay)
              (vlax-put newobj 'Color clr)
              (vlax-put newobj 'Linetype lt)
            )
            (vla-delete obj)
            ;; Есть небольшая плата за производительность за выполнение восстановления вида здесь внутри 
            ;; функции. Причина, по которой она здесь, а не в функции ошибки,
            ;; заключается в том, что когда она находится в функции ошибки и пользователь отменяет выполнение,
            ;; возникает ошибка в функции ошибки. Я не уверен, почему.
            (if 
              (and
                (= 1 (vlax-get doc 'ActiveSpace)) ;; модельное пространство
                (tblobjname "view" "SFview")
              )
              (command-new (list "._view" "_restore" "SFview"))
            )
          )
          ;; иначе
          (CommandExplode obj)
        )
        (vl-file-delete tmpwmf)
      )
    )
  )  ; end WMFOutIn
  ;; Аргумент: ename или vla-объект.
  ;; Возвращает T, если нормаль (0.0 0.0 1.0) или (0.0 0.0 -1.0) в пределах fuzz.
  (defun TestNormal (obj / n)
    (if (= (type obj) 'VLA-object)
      (setq n (vlax-get obj 'Normal))
      (setq n (cdr (assoc 210 (entget obj))))
    )
    (or
      (equal n '(0.0 0.0 1.0) 1e-8)
      (equal n '(0.0 0.0 -1.0) 1e-8)
    )
  ) ;end
  ;; Изменено 9/8/2007.
  ;; Пример: окружность с нормалью вида (0.819152 -0.573576 0.0)
  ;; выглядит как линия в верхнем виде. Преобразовать в линию с использованием 
  ;; ограничивающих точек.
  (defun TestZNormal (obj / n mn mx newobj)
    (if (= (type obj) 'VLA-object)
      (setq n (vlax-get obj 'Normal))
      (setq n (cdr (assoc 210 (entget obj)))
            obj (vlax-ename->vla-object obj)
      )
    )
    (if (equal 0.0 (caddr n) 1e-8)
      (progn
        (vla-GetBoundingBox obj 'mn 'mx)
        (setq mn (ZZeroPoint (vlax-safearray->list mn))
              mx (ZZeroPoint (vlax-safearray->list mx))
        )
        (cond
          ((or             
             ;; Назад (0.0 1.0 0.0) или Вправо (1.0 0.0 0.0).
             (equal 1.0 (apply '+ n) 1e-8)
             ;; Включает Влево (-1.0 0.0 0.0).
             (and
               (minusp (car n))
               (not (minusp (cadr n)))
             )
             ;; Включает Вперед (0.0 -1.0 0.0).
             (and
               (not (minusp (car n)))
               (minusp (cadr n))
             )
            )
            (setq newobj (vlax-invoke (GetBlock) 'AddLine mn mx))
          )
          ;; Или использовать другие два угла ограничивающей рамки.
          (T
            (setq newobj 
              (vlax-invoke (GetBlock) 'AddLine 
                (list (car mx) (cadr mn) 0.0)
                (list (car mn) (cadr mx) 0.0)
              )
            )
          )
        )
        (ApplyProps obj newobj)
        ;; Возврат T для условных вызовов.
        (setq renameflag T)
      ) ;progn
    ) ;if
  ) ;end
  ;Аргумент: одиночный список 3D точек.
  (defun ZZeroPoint (pt)
    (if pt
        (list (car pt) (cadr pt) 0.0)
        '(0.0 0.0 0.0)
    )
) ;end
  ;; Аргумент: плоский список 3D координат.
  ;; (setq l '(414.576 572.128 0.0 494.558 637.135 20.0 562.58 575.117 30.0))
  ;; Возвращает:
  ;; (414.576 572.128 0.0 494.558 637.135 0.0 562.58 575.117 0.0)
  (defun ZZeroCoord (coord / lst)
    (repeat (/ (length coord) 3)
      (setq lst (cons (car coord) lst)
            lst (cons (cadr coord) lst)
            lst (cons 0.0 lst)
            coord (cdddr coord)
      )
    )
    (reverse lst)
  ) ;end ZZeroCoord
  (defun GetBlock ()
    (vlax-get (vla-get-ActiveLayout doc) 'Block)
  ) ;end
  ;; Вызывается во вложенной функции ProcessList.
  ;; Автор неизвестен.
  (defun Spinbar (sbar) 
    (cond ((= sbar "\\") "|")
          ((= sbar "|") "/")
          ((= sbar "/") "-")
          (t "\\")
    )
  ) ;end Spinbar
  ;; Аргументы: существующее значение и тестовое значение.
  ;; Порядок передаваемых аргументов не важен.
  ;; Определяет, нужно ли переименовать определение блока
  ;; или нет, устанавливая переменную renameflag.
  (defun CheckRename (exval testval)
    (if (and renameans presufstr)
      (or 
        (equal exval testval 1e-6)
        (setq renameflag T)
      )
    )
  ) ;end CheckRename
  ;; Проверка элемента в коллекции, Дуг Брод.
  (defun ValidItem (collection item / res)
    (vl-catch-all-apply
      '(lambda ()
        (setq res (vla-item collection item))))
    res
  )
  ;; Аргумент: строка "prefix" или "suffix".
  ;; Вызывается из параметров программы.
  ;; snvalid возвращает nil при передаче строки с 
  ;; ведущими или завершающими пробелами.
  (defun PrefixSuffix (argstr / str StripSpaces)
    ;Удалить ведущие и завершающие пробелы для проверки snvalid.
    (defun StripSpaces (str)
      (vl-string-right-trim " " (vl-string-left-trim " " str))
    )

    (setq str (getstring T (strcat "Имя блока " argstr ": ")))
    (if (eq argstr "prefix")
      (setq str (vl-string-left-trim " " str))
      (setq str (vl-string-right-trim " " str))
    )
    (cond
      ((eq "" str)
        (princ "\nБлоки не будут переименованы. ")
      )
      ((not (snvalid (StripSpaces str) 0))
        (while
          (and 
            (not (eq "" str))
            (not 
              (snvalid
                (setq str (StripSpaces (getstring T (strcat "\nНеверный " argstr ": ")))) 0
              )
            )
          )
        )
      )
    )
    (if (not (eq "" str))
      str
    )
  ) ;end PrefixSuffix
  ;; Entmake lwpline. 
  ;; Возвращает lwpline vla-объект, если успешно.
  (defun SF:MakeLWPolyline (ptlst width)
    (if 
      (and
        (> (length ptlst) 1)
        (apply 'and ptlst)
      )
      (if (entmake
            (append
              (list
                '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity")
                '(100 . "AcDbPolyline")
		'(38 . 0)
                 (cons 90 (length ptlst))
                 (cons 43 width)
              )
              (mapcar '(lambda (x) (cons 10 x)) ptlst)
            )
          )
        (progn
          (setq renameflag T)
          (vlax-ename->vla-object (entlast))
        )
      )
    )
  ) ;end SF:MakeLWPolyline
  ;; Аргументы: два vla-объекта.
  ;; Применить свойства старого объекта к новому объекту 
  ;; и удалить старый объект. Также установить renameflag T.
  (defun ApplyProps (old new)
    (if 
      (and 
        old 
        new
        (not (vlax-erased-p old))
        (not (vlax-erased-p new))
      )
      (progn
        (mapcar '(lambda (x) (vlax-put new x (vlax-get old x)))
          '("Color" "Layer" "Linetype" "LinetypeScale" "Lineweight")
        )
        (vl-catch-all-apply
          '(lambda () 
            (vlax-put new 'LinetypeGeneration (vlax-get old 'LinetypeGeneration))
          )
        )
        (vla-delete old)
        (setq renameflag T)
      )
    )
  ) ;end ApplyProps
  ;; Возвращает вложенный список точек из плоского списка точек.
  (defun PointList (obj / coord lst)
    (setq coord (vlax-get obj 'Coordinates))
    (cond
      ((eq "AcDbPolyline" (vlax-get obj 'ObjectName))
        (repeat (/ (length coord) 2)
          (setq lst (cons (list (car coord) (cadr coord)) lst)
                coord (cddr coord)
          )
        )
      )
      (T
        (repeat (/ (length coord) 3)
          (setq lst (cons (list (car coord) (cadr coord) (caddr coord)) lst)
                coord (cdddr coord)
          )
        )
      )
    )
    (reverse lst)
  ) ;end PointList
  ;; Изменено 1/10/2010.
  ;; Преобразовать список объектов атрибутов в список объектов текста.
  ;; Атрибуты могут быть текстовыми или многострочными атрибутами.
  ;; Возвращает: список текстовых и/или mtext vla-объектов.
  ;; Примечание: вызывающая функция должна проверить переданный объект
  ;; не находится ли он на заблокированном слое. В противном случае эти функции
  ;; вызовут ошибки "On locked layer".
  ;; Функция не удаляет переданный объект. Вызывающая
  ;; функция должна сделать это.
  (defun AttributesToText (attlst / n elst str obj res AlignMtext UCSAng)
    ;; Аргумент: текст или атрибут, включая мультистрочный, vla-объект.
    ;; Возвращает: соответствующее свойство AttachmentPoint для mtext объекта.
    ;; На основе кода Ли Макдональда.
    (defun AlignMtext (obj / align1)
      (setq align1 (vlax-get obj 'Alignment))
      (cond 
        ((<= 0 align1 2) (1+ align1))
        ((<= 3 align1 5) 1)
        (T (- align1 5))
      )
    ) ;end
    ;; Угол Mtext (код 50) выражается в UCS.
    ;; Поворот текста выражается в WCS.
    ;; Преобразование угла из WCS в UCS, Джон Уден.
    (defun UCSAng (ang)
      (angle
        (trans '(0 0 0) 0 1)
        (trans (polar '(0 0 0) ang 1) 0 1)
      )
    )
    (foreach attobj attlst 
      (setq n (vlax-get attobj 'Normal))
      (setq elst (entget (vlax-vla-object->ename attobj)))
      (setq str (SF:GetFields attobj))
      (if 
        (and
          (vlax-property-available-p attobj 'MTextAttribute)
          (= -1 (vlax-get attobj 'MTextAttribute))
        )
        ;; многострочный атрибут
        (progn
          (if
            (entmake
              (list
                '(0 . "MTEXT")
                '(100 . "AcDbEntity")
                '(100 . "AcDbMText")
                (cons 1 (vlax-get attobj 'TextString))
                ;(cons 1 str)
                (cons 7 (vlax-get attobj 'StyleName))
                (cons 8 (vlax-get attobj 'Layer))
                ;(cons 10 (vlax-get x 'InsertionPoint))
                (cons 10 (vlax-get attobj 'TextAlignmentPoint))
                ;; это свойство AttachmnetPoint
                (cons 71 (AlignMtext attobj))
                (cons 40 (vlax-get attobj 'Height))
                (cons 50 (UCSAng (vlax-get attobj 'Rotation)))
                ;(cons 50 (vlax-get attobj 'Rotation))
                (cons 62 (vlax-get attobj 'Color))
                ;; Добавлено в 2.3 12/10/2009 ширина mtext
                (cons 41 (vlax-get attobj 'MTextBoundaryWidth))
                ;(cons 210 (vlax-get attobj 'Normal))
                (assoc 410 elst)
              )
            ) ;make
            (progn
              (if (assoc 90 elst)
                (entmod 
                  (append 
                    (entget (entlast))
                    (vl-member-if '(lambda (x) (= 90 (car x))) elst)
                  )
                )
              )
              (setq obj (vlax-ename->vla-object (entlast)))
              (vlax-put obj 'Normal n)
              (setq res (cons (FlatMText obj) res))
            )
          )
        ) ; progn
        ;; стандартный текстовый атрибут
        (progn
          (if
            (entmake
              (list
                '(0 . "TEXT")
                (cons 1 (vlax-get attobj 'TextString))
                (cons 7 (vlax-get attobj 'StyleName))
                (cons 8 (vlax-get attobj 'Layer))
                (cons 10 (vlax-get attobj 'InsertionPoint))
                (cons 11 (vlax-get attobj 'TextAlignmentPoint))
                (cons 40 (vlax-get attobj 'Height))
                (cons 41 (vlax-get attobj 'ScaleFactor))
                (cons 50 (vlax-get attobj 'Rotation))
                (cons 51 (vlax-get attobj 'ObliqueAngle))
                (cons 62 (vlax-get attobj 'Color))
                ;; Добавлено 10/29/2009
                ;(cons 210 (vlax-get attobj 'Normal))
                (cons 67 (cdr (assoc 67 elst)))
                (cons 71 (cdr (assoc 71 elst)))
                (cons 72 (cdr (assoc 72 elst)))
                ;; Изменено 10/29/2009
                ;; Тип вертикального выравнивания текста
                ;; DXF код 74 в атрибуте и
                ;; код 73 в текстовом объекте.
                ;(cons 73 (cdr (assoc 73 elst)))
                (cons 73 (cdr (assoc 74 elst)))
                (assoc 410 elst)
              )
            ) ; make
            (progn
              (setq obj (vlax-ename->vla-object (entlast)))
              (vlax-put obj 'Normal n)
              (if (= 0 (vlax-get obj 'Alignment))
                (vlax-put obj 'InsertionPoint 
                  (vlax-get attobj 'InsertionPoint)
                )
                (vlax-put obj 'TextAlignmentPoint 
                  (vlax-get attobj 'TextAlignmentPoint)
                )
              )
              (setq res (cons (FlatText obj) res))
            )
          )
        )
      )
      ;; Сохранить символы и поля.
      (if (and str obj) (vlax-put obj 'TextString str))
    ) ;foreach
    res
  ) ;end AttributesToText
  ;; Аргумент: ename или vla-объект.
  ;; Типы объектов: mtext, атрибут, mleader или размер.
  ;; Возвращает: строку с символами без изменений.
  (defun SF:SymbolString (obj / e typ str name String blocks)
    ;; Многострочный атрибут может содержать два кода DXF 1 и несколько
    ;; кодов DXF 3. В любом случае первый код 1 должен игнорироваться
    ;; поскольку он содержит строку, которая не отображается на экране.
    ;; По-видимому, это странное условие возникает, когда текст вставляется поверх
    ;; существующего текста. Старый текст хранится в первом DXF коде 1
    ;; а текст, отображаемый на экране, хранится во втором DXF коде 1.
    (defun String (ename / str lst)
      (setq str "")
      (setq lst
        (vl-remove-if-not
          '(lambda (x) (or (= 3 (car x)) (= 1 (car x)))) (entget ename)
        )
      )
      (if (and (< 1 (length lst)) (= 1 (caar lst)))
        (setq lst (cdr lst))
      )
      (foreach x lst
        (setq str (strcat str (cdr x)))
      )
    ) ; end String
    (if (= (type obj) 'VLA-OBJECT)
      (setq e (vlax-vla-object->ename obj))
      (progn
        (setq e obj)
        (setq obj (vlax-ename->vla-object obj))
      )
    )
    (setq typ (vlax-get obj 'ObjectName))
    (cond
      ((or
         (eq typ "AcDbMText")
         (eq typ "AcDbAttribute")
        )
        (setq str (String e))
      )
      ((eq typ "AcDbMLeader")
        (setq str (cdr (assoc 304 (entget e))))
      )
      ((and
         (wcmatch typ "*Dimension*")
         (setq name (cdr (assoc 2 (entget e))))
         (wcmatch name "`*D*")
         (setq blocks 
           (vla-get-blocks
             (vla-get-activedocument
               (vlax-get-acad-object)
             )
           )
         )
       )
       (vlax-for x (vla-item blocks name)
         (if (eq "AcDbMText" (vlax-get x 'ObjectName))
           (progn
             (setq str (String (vlax-vla-object->ename x)))
             ;; Сохранить символы. Это также обновляет строку, возвращаемую
             ;; методом FieldCode, чтобы она содержала символы.
             (vlax-put x 'TextString str)
             ;; Модифицированная версия стандартной функции SymbolString
             ;; для сохранения полей при взрыве размера
             ;; с использованием функции CommandExplode.
             (setq str (vlax-invoke x 'FieldCode))
           )
         )
       )
     )
    )
    str
  ) ; end SF:SymbolString
  ;; Аргумент: исходный vla-объект.
  ;; Возвращает: ту же строку, что и метод FieldCode, но работает с 
  ;; полями в атрибутах и mleaders. FieldCode не работает с 
  ;; атрибутами или mleaders. Возвращает исходную текстовую строку, если поля 
  ;; не найдены в атрибуте или mleader.
  (defun SF:GetFields (obj / srcdict srcdictename srcTEXTdict
                             srcfieldename targdict targdictename
                             fieldelst fielddict dicts actlay
                             tempobj lockflag res)

    (cond 
      ;; Если у объекта нет расширенного словаря или
      ;; словарь можно удалить, потому что он пуст.
      ((or
         (= 0 (vlax-get obj 'HasExtensionDictionary))
         (not 
           (vl-catch-all-error-p
             (vl-catch-all-apply 'vlax-invoke
               (list (vlax-invoke obj 'GetExtensionDictionary) 'Delete)
             )
           )
         )
       )
       (setq res (SF:SymbolString obj))
      )
      ;; Источник - атрибут или mleader, и он содержит одно или несколько полей.
      ;; Создать новый временный mtext объект. Применить исходные словари полей
      ;; к нему. Затем получить FieldCode из временного объекта и удалить его.
      ;; По какой-то причине mtext объекты в таблицах ведут себя неправильно, когда
      ;; словарь добавляется. На самом деле, команда updatefield, кажется,
      ;; разрушает такие поля. Этот метод избегает этой проблемы.
      ((and
        (= -1 (vlax-get obj 'HasExtensionDictionary))
        (setq srcdict (vlax-invoke obj 'GetExtensionDictionary))
        (setq srcdictename (vlax-vla-object->ename srcdict))
        (setq srcTEXTdict (dictsearch srcdictename "ACAD_FIELD"))
        (setq srcfieldename (cdr (assoc 360 srcTEXTdict)))
       )
        ;; Проверить, заблокирован ли активный слой.
        (setq actlay (vlax-get doc 'ActiveLayer))
        (if (= -1 (vlax-get actlay 'Lock))
          (progn
            (vlax-put actlay 'Lock 0)
            (setq lockflag T)
          )
        )
        (setq tempobj (vlax-invoke (GetBlock) 'AddMText '(0.0 0.0 0.0) 0.0 "xx"))
        (setq targdict (vlax-invoke tempobj 'GetExtensionDictionary)
              targdictename (vlax-vla-object->ename targdict)
              fieldelst (entget srcfieldename)
              ;; не уверен в необходимости этого
              fieldelst (vl-remove (assoc 5 fieldelst) fieldelst)
              fieldelst (vl-remove (assoc -1 fieldelst) fieldelst)
              fieldelst (vl-remove (assoc 102 fieldelst) fieldelst)
              fieldelst (vl-remove-if '(lambda (x) (= 330 (car x))) fieldelst)
        )
        (foreach x fieldelst
          (if (= 360 (car x))
            (progn
              (setq dicts (cons (cdr x) dicts))
            )
          )
        )
        ;; удалить все 360 из fieldelst
        (setq fieldelst (vl-remove-if '(lambda (x) (= 360 (car x))) fieldelst))  
        (foreach x (reverse dicts)
          (setq fieldelst (append fieldelst (list (cons 360 (entmakex (entget x))))))
        )
        (setq fielddict
          (dictadd targdictename "ACAD_FIELD"
            (entmakex
              '(
                (0 . "DICTIONARY")
                (100 . "AcDbDictionary")
                (280 . 1)
                (281 . 1)
              )
            )
          )
        )
        (dictadd fielddict "TEXT" 
          (entmakex fieldelst)
        )
        (vlax-put tempobj 'TextString (SF:SymbolString tempobj))
        (setq res (vlax-invoke tempobj 'FieldCode))
        (vla-delete tempobj)
        (if lockflag (vlax-put actlay 'Lock -1))
      )
      ;; Это на самом деле не нужно, учитывая первое условие, но оставить.
      (T (setq res (SF:SymbolString obj)))
    )
    res
  ) ;end SF:GetFields
  ;; Изменить масштабные коэффициенты X, Y и Z блока
  ;; ссылки, если они близки к равным, чтобы использовать
  ;; метод взрыва.
  ;; Возвращает T, если успешно, иначе nil.
  ;; Если nil, вызвать CommandExplode.
  ;; Обратите внимание, метод взрыва работает, если блок имеет, например,
  ;;/// отрицательный масштабный коэффициент X. Блок был зеркально отражен.
  ;; Я не уверен, является ли это полным решением в терминах
  ;; других возможных отрицательных значений.
  (defun ModBlockScale (blk / xsf ysf zsf)
    (setq xsf (vlax-get blk 'XScaleFactor)
          ysf (vlax-get blk 'YScaleFactor)
          zsf (vlax-get blk 'ZScaleFactor)
    )
    (if
      (and
        (or
          (equal xsf ysf 1e-3)
          (equal (- xsf) ysf 1e-3)
        )
        (equal ysf zsf 1e-3)
	(/= (Round xsf 1e-3) 0)
	(/= (Round ysf 1e-3) 0)
	(/= (Round zsf 1e-3) 0)
      )
      (progn
        (vlax-put blk 'XScaleFactor (Round xsf 1e-3))
        (vlax-put blk 'YScaleFactor (Round ysf 1e-3))
        (vlax-put blk 'ZScaleFactor (Round zsf 1e-3))
        T
      )
    )
  ) ;end ModBlockScale

;; Мини-функция для безопасного изменения точки вставки и диагностики
(defun SF:SafePutIP (blk ip / res blkName layName)
  ;; Пробуем изменить координату и перехватываем ошибку
  (setq res (vl-catch-all-apply 'vlax-put (list blk 'InsertionPoint (list (car ip) (cadr ip) 0.0))))
  
  ;; Если произошла ошибка
  (if (vl-catch-all-error-p res)
    (progn
      (setq layName (vlax-get blk 'Layer))
      ;; Проверяем, не является ли слой системным (*ADSK_...). Символ ` экранирует звездочку.
      (if (not (wcmatch (strcase layName) "`*ADSK*"))
        (progn
          (setq blkName (if (vlax-property-available-p blk 'EffectiveName) (vlax-get blk 'EffectiveName) (vlax-get blk 'Name)))
          (princ (strcat "\n! ОШИБКА Z-координаты: Блок [" blkName "] на слое [" layName "]. Описание: " (vl-catch-all-error-message res)))
        )
      )
    )
  )
)
  ;; Взрывать ссылку на блок с использованием метода Explode.
  ;; Если ссылка проходит проверку TestNormal, она не взрывается.
(defun ExpBlockMethod (blkref / ip blkdef flag lay attlst exlst is_dynamic)
    (setq blkdef (vla-item blocks (vlax-get blkref 'Name)))
    
    ;; Проверка, является ли блок динамическим
    (setq is_dynamic (vl-catch-all-apply 'vlax-get-property (list blkref 'IsDynamicBlock)))
    
    (if
        (or
            (not (vlax-property-available-p blkdef 'Explodable))
            (eq acTrue (vlax-get blkdef 'Explodable))
        )
        (setq flag T)
    )
    
    (cond
        ;; Если блок динамический - ТОЛЬКО ВЫРОВНЯТЬ, НЕ ВЗРЫВАТЬ!
        ((and (not (vl-catch-all-error-p is_dynamic)) 
              (eq :vlax-true is_dynamic))
            (setq ip (vlax-get blkref 'InsertionPoint))
            (CheckRename ip (list (car ip) (cadr ip) 0.0))
            
            ;; Обнуляем Z-координату точки вставки
            (SF:SafePutIP blkref ip)
            
            ;; Обязательно вытягиваем и выравниваем атрибуты
            (setq attlst (vlax-invoke blkref 'GetAttributes))
            (if attlst
                (foreach x attlst (FlatText x))
            )
            
            (setq renameflag T)
            (princ "\rДинамический блок выровнен без взрыва: ")
            ;; Для динамических блоков корректнее выводить исходное имя (EffectiveName), а не анонимное *U...
            (princ (if (vlax-property-available-p blkref 'EffectiveName)
                       (vlax-get blkref 'EffectiveName)
                       (vlax-get blkref 'Name)))
        )
        
        ;; Стандартная обработка блоков с нормальной нормалью
        ((TestNormal blkref)
            (setq ip (vlax-get blkref 'InsertionPoint))
            (CheckRename ip (list (car ip) (cadr ip) 0.0))
            ;; Только обнуляем Z-координату точки вставки
            ;(vlax-put blkref 'InsertionPoint (list (car ip) (cadr ip) 0.0))
	    (SF:SafePutIP blkref ip)
            (setq attlst (vlax-invoke blkref 'GetAttributes))
            (foreach x attlst (FlatText x))
        )
        
        ;; Блок можно взорвать стандартным методом
        ((and flag (ModBlockScale blkref))
            (setq lay (vlax-get blkref 'Layer)
                  attlst (vlax-invoke blkref 'GetAttributes)
                  exlst (vlax-invoke blkref 'Explode)
            )
            (if exlst
                (progn
                    (setq renameflag T)
                    (setq expblkcnt (1+ expblkcnt))
                    (AttributesToText attlst)
                    (vla-delete blkref)
                    (foreach x exlst
                        (if (eq "AcDbAttributeDefinition" (vlax-get x 'ObjectName))
                            (vla-delete x)
                        )
                    )
                    (setq exlst (vl-remove-if 'vlax-erased-p exlst))
                    (foreach x exlst
                        (if (eq "0" (vlax-get x 'Layer))
                            (vlax-put x 'Layer lay)
                        )
                        (if (zerop (vlax-get x 'Color))
                            (vlax-put x 'Color 256)
                        )
                    )
                    (ProcessList exlst)
                )
                (progn
                    (setq cnt (1+ cnt))
                    (if (not (vl-position "AcDbBlockReference" notflatlst))
                        (setq notflatlst (cons "AcDbBlockReference" notflatlst))
                    )
                )
            )
        )
        
        ;; Для остальных случаев использовать командный взрыв
        (T (CommandExplode blkref))
    )
) ;end ExpBlockMethod

 ;; Типы объектов, которые можно взорвать с помощью этой функции:
  ;; Штриховка с нечетной нормалью, которую нельзя воссоздать, потому что
  ;;/// шаблон штриховки недоступен.
  ;; Размеры с нечетной нормалью.
  ;;/// AcDb3dSolid и AcDbBody объекты. Некоторые можно взорвать, другие нет.
  ;;/// Ссылки на блоки, когда функция ExpBlockMethod выше не может этого сделать 
  ;;/// потому что ссылка не масштабируется равномерно.
  (defun CommandExplode (obj / lay mark objname attlst name exlst)
    (setq mark (entlast)
          objname (vlax-get obj 'ObjectName)
    )
    (cond
      ((or 
         (eq "AcDb3dSolid" objname)
         ;; Объекты Body не могут быть взорваны.
         ;; сфера взрывается дважды = body
         ;; Удалено 2/7/2011.
         ;; (eq "AcDbBody" objname)
         (eq "AcDbSurface" objname)
         (eq "AcDbHatch" objname)
         (eq "AcDbHelix" objname)
         (eq "AcDbZombieEntity" objname)
         (eq "AcDbPlaneSurface" objname)
	 (wcmatch (strcase objname) "MCSDBOBJECT*")
	 (wcmatch (strcase objname) "MCSPSEUDO*")
        )
        (command-new (list "._explode" (vlax-vla-object->ename obj)))
        (if (not (eq mark (entlast)))
          (setq exlst (SSVLAList (ssget "_p")))
        )
      )
      ;; Добавлено 1/11/2010.
      ((wcmatch objname "*Dimension*")
        (setq str (SF:SymbolString obj))
        (command-new (list "._explode" (vlax-vla-object->ename obj)))
        (if 
          (and 
            (not (eq mark (entlast)))
            (setq exlst (SSVLAList (ssget "_p")))
          )
          (foreach x exlst
            (if (eq "AcDbMText" (vlax-get x 'ObjectName))
              (vlax-put x 'TextString str)
            )
          )
        )
      )
      ;; Добавлено 1/12/2010.
      ((eq "AcDbMLeader" objname)
        (command-new (list "._explode" (vlax-vla-object->ename obj)))
        (if 
          (and 
            (not (eq mark (entlast)))
            (setq exlst (SSVLAList (ssget "_p")))
          )
          (foreach x exlst
            (if (eq "AcDbSolid" (vlax-get x 'ObjectName))
              (progn
                (vla-delete x)
                (setq exlst (vl-remove x exlst))
              )
            )
          )
        )
      )
      ((eq "AcDbBlockReference" objname)
         (setq lay (vlax-get obj 'Layer)
               attlst (vlax-invoke obj 'GetAttributes)
         )
         (command-new (list "._explode" (vlax-vla-object->ename obj)))
         ;; Были проблемы с блоками, которые нельзя взорвать.
         ;; Следующая проверка, кажется, исправляет это.
         (if 
           (and 
             (not (eq mark (entlast)))
             (setq exlst (SSVLAList (ssget "_p")))
           )
           (progn
             (setq expblkcnt (1+ expblkcnt))
             (AttributesToText attlst)
             (foreach x exlst
               (if (eq "AcDbAttributeDefinition" (vlax-get x 'ObjectName))
                 (vla-delete x)
               )
             )
             (setq exlst (vl-remove-if 'vlax-erased-p exlst))
             ;Если взорванный объект находится на слое 0, поместить его на
             ;слой взорванного объекта. Если его цвет по умолчанию, 
             ;изменить цвет на по слою.
             (foreach x exlst
               (if (eq "0" (vlax-get x 'Layer))
                 (vlax-put x 'Layer lay)
               )
               (if (zerop (vlax-get x 'Color))
                 (vlax-put x 'Color 256)
               )
             )
           )
         )
       )
    ) ;cond    
    (cond
      (exlst
        (setq renameflag T)
        (ProcessList exlst)
      )
      ;; Проверка на пустой объект, например, proxy.
      ;; Если список взрыва nil и объект был удален 
      ;;/// командой "explode", то это был пустой объект.
      ;; Поэтому не считать его и не добавлять в notflatlst.
      ;; Изменено 8/27/2007.
      ((not (vlax-erased-p obj))
        (setq cnt (1+ cnt))
        (if (eq "AcDbZombieEntity" objname)
          (setq objname "DbProxy")
        )
        (if (not (vl-position objname notflatlst))
          (setq notflatlst (cons objname notflatlst))
        )
      )
    )
  ) ;end CommandExplode
  ;; Аргументы: vla-объект и вектор нормали.
  ;; Вызывается из FlatXref и FlatShape.
  ;; Проверка на приближение Z значения нормали к 1 или -1, потому что
  ;;/// в этом диапазоне отображение объекта просто
  ;;/// показывает его вращение. Есть пример этого в
  ;;/// тестовом файле Кена Лука из клиентских файлов.
  ;; Обратите внимание, put rotation может быть таким.
  ;; (vlax-put obj 'Rotation (+ (* pi 0.5) (atan (cadr n) (car n))))
  (defun RotateToNormal (obj n)
    (if
      (and
        (not (equal 1.0 (caddr n) 1e-5))
        (not (equal -1.0 (caddr n) 1e-5))
      )
      (vlax-put obj 'Rotation 
        (+ (vlax-get obj 'Rotation) (+ (* pi 0.5) (angle '(0 0) n)))
      )
    )
  ) ;end RotateToNormal
  ;;; TRACE ;;;
  (defun SF:TraceObject (obj / typlst typ ZZeroList TracePline TraceACE 
                               TraceType1Pline TraceType23Pline)
    ;;;; начать вложенные функции трассировки ;;;;
    ;; Аргумент: 2D или 3D список точек.
    ;; Возвращает: 3D список точек с нулевыми Z значениями.
    (defun ZZeroList (lst)
      (mapcar '(lambda (p) (list (car p) (cadr p) 0.0)) lst)
    )
    ;; Аргумент: vla-объект, тяжелая или легкая полилиния.
    ;; Возвращает: WCS список точек, если успешно.
    ;; Примечания: Дублирующиеся соседние точки удаляются.
    ;; Последняя закрывающая точка включена для замкнутой полилинии.
    (defun TracePline (obj / param endparam anginc tparam pt blg 
                             ptlst delta inc arcparam flag)
      (setq param (vlax-curve-getStartParam obj)
            endparam (vlax-curve-getEndParam obj)
            anginc (* pi (/ 7.5 180.0))
      )
      (while (<= param endparam)
        (setq pt (vlax-curve-getPointAtParam obj param))
        ;Избегать дублирующихся точек между началом и концом.
        (if (not (equal pt (car ptlst) 1e-12))
          (setq ptlst (cons pt ptlst))
        )
        ;Замкнутая полилиния возвращает ошибку (invalid index) 
        ;при запросе bulge параметра конца.
        (if 
          (and 
            (/= param endparam)
            (setq blg (abs (vlax-invoke obj 'GetBulge param)))
            (/= 0 blg)
          )
          (progn
            (setq delta (* 4 (atan blg)) ;включенный угол
                  inc (/ 1.0 (1+ (fix (/ delta anginc))))
                  arcparam (+ param inc)
            )
            (while (< arcparam (1+ param))
              (setq pt (vlax-curve-getPointAtParam obj arcparam)
                    ptlst (cons pt ptlst)
                    arcparam (+ inc arcparam)
              )
            )
          )
        )
        (setq param (1+ param))
      ) ;while
      (if (> (length ptlst) 1)
        (progn
          (setq ptlst (vl-remove nil ptlst))
          (ZZeroList (reverse ptlst))
        )
      )
    ) ;end
    ;; Аргумент: vla-объект, дуга, окружность или эллипс.
    ;; Возвращает: WCS список точек, если успешно.
    (defun TraceACE (obj / startparam endparam anginc 
                           delta div inc pt ptlst)
      ;начальные и конечные углы
      ;у окружностей нет свойств StartAngle и EndAngle.
      (setq startparam (vlax-curve-getStartParam obj)
            endparam (vlax-curve-getEndParam obj)
            ;anginc (* pi (/ 7.5 180.0))
            anginc (* pi (/ 2.5 180.0))
      )
      (if (equal endparam (* pi 2) 1e-6)
        (setq delta endparam)
        ;добавлен abs 6/23/2007, тестирование
        (setq delta (abs (- endparam startparam)))
      )
      ;Разделить delta (включенный угол) на равное количество частей.
      (setq div (1+ (fix (/ delta anginc)))
            inc (/ delta div)
      )
      ;Или утверждение позволяет использовать последнюю точку открытого эллипса
      ;а не использовать (<= startparam endparam), что иногда
      ;не возвращает последнюю точку. Не уверен, почему.
      (while
        (or
          (< startparam endparam)
          (equal startparam endparam 1e-12)
          ;(equal startparam endparam)
        )
        (setq pt (vlax-curve-getPointAtParam obj startparam)
              ptlst (cons pt ptlst)
              startparam (+ inc startparam)
        )
      )
      (ZZeroList (reverse ptlst))
    ) ;end
    ;; Взорвать кривую fit pline и собрать список точек из дуг.
    ;; Эта вложенная функция удаляет объекты.
    (defun TraceType1Pline (obj / ptlst objlst lst)
      (setq ptlst (list (vlax-curve-getStartPoint obj))
            objlst (vlax-invoke obj 'Explode)
      )
      (foreach x objlst 
        (setq lst (TraceACE x))
        (if (not (equal (car lst) (last ptlst) 1e-8))
          (setq lst (reverse lst))
        )
        (setq ptlst (append ptlst (cdr lst)))
        (vla-delete x)
      )
      (ZZeroList ptlst)
    ) ;end
    ;; Взорвать квадратичные и кубические plines и собрать список точек из линий.
    ;; Производит точную трассировку.
    ;; Эта вложенная функция удаляет объекты.
    (defun TraceType23Pline (obj / objlst ptlst lastpt)
      (setq objlst (vlax-invoke obj 'Explode)
            lastpt (vlax-get (last objlst) 'EndPoint)
      )
      (foreach x objlst
        (setq ptlst (cons (vlax-get x 'StartPoint) ptlst))
        (vla-delete x)
      )
      (ZZeroList (reverse (cons lastpt ptlst)))
    ) ;end
    ;;;; конец вложенных функций трассировки ;;;;
    ;;;; основная функция трассировки ;;;;
    (setq typlst '("AcDb2dPolyline" "AcDbPolyline"  
                   "AcDbCircle" "AcDbArc" "AcDbEllipse")
    )
    (or 
      (eq (type obj) 'VLA-OBJECT)
      (setq obj (vlax-ename->vla-object obj))
    )
    (setq typ (vlax-get obj 'ObjectName))
    (if (vl-position typ typlst)
      (cond
         ((or (eq typ "AcDb2dPolyline") (eq typ "AcDbPolyline")) 
            (cond
              ((or
                 (not (vlax-property-available-p obj 'Type))
                 (= 0 (vlax-get obj 'Type))
                )
                (TracePline obj)
              )
              ((or (= 3 (vlax-get obj 'Type)) (= 2 (vlax-get obj 'Type)))
                (TraceType23Pline obj)
              )
              ((= 1 (vlax-get obj 'Type))
                (TraceType1Pline obj)
              )
            )
         )
         ((or (eq typ "AcDbCircle") (eq typ "AcDbArc") (eq typ "AcDbEllipse"))
           (TraceACE obj)
         )
      )
    )
  ) ;end SF:TraceObject
  ;;; TRACE ;;;
  ;; На основе кода Луиса Эскивеля.
  ;; Возвращает список имен шаблонов из acad.pat.
  (defun LstACADPAT ( / file line tmp lst )
    (setq file (open (findfile "acad.pat") "r"))
    (while (setq line (read-line file))
      (setq tmp (cons line tmp))
    )
    (close file)
    (setq tmp (reverse tmp))
    (setq lst (vl-remove-if-not
      '(lambda (string)
        (if (eq (substr string 1 1) "*") string)) tmp))
    (mapcar
      '(lambda (string)
        (substr string 2 (- (vl-string-search "," string) 1))) lst)
  ) ;end LstACADPAT
  (defun UnlockLayers (doc / laylst)
    (vlax-for x (vla-get-Layers doc)
      ;отфильтровать слои xref
      (if 
        (and 
          (not (vl-string-search "|" (vlax-get x 'Name)))
          (eq :vlax-true (vla-get-lock x))
        )
        (progn
          (setq laylst (cons x laylst))
          (vla-put-lock x :vlax-false)
        )
      )
    )
    laylst
  ) ;end UnlockLayers
  (defun RelockLayers (lst)
    (foreach x lst
      (vl-catch-all-apply 'vla-put-lock (list x :vlax-true))
    )
  ) ;end UnlockLayers
  (defun SSVLAList (ss / obj lst i)
    (setq i 0)
    (if ss
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i))
              lst (cons obj lst)
              i (1+ i)
        )
      )
    )
    (reverse lst)
  ) ;end

  ;; Аргументы: коллекция блоков и строка имени блока.
  ;; Возвращает список имен вложенных блоков в blkname, если есть.
  (defun GetNestedNames (blkcol blkname / name namelst temp1 temp2)
    ;первый вложенный уровень
    (vlax-for x (vla-item blkcol blkname)
      (if 
        (and
          (= "AcDbBlockReference" (vlax-get x 'ObjectName))
          (not (vl-position (setq name (vlax-get x 'Name)) namelst))
        )
        (setq namelst (cons name namelst))
      )
    )
    ;вложенность глубже
    (setq temp1 namelst)
    (while temp1
      (foreach x temp1
        (vlax-for x (vla-item blkcol x)
          (if 
            (and
              (= "AcDbBlockReference" (vlax-get x 'ObjectName))
              (not (vl-position (setq name (vlax-get x 'Name)) namelst))
            )
            (setq namelst (cons name namelst)
                  temp2 (cons name temp2)
            )
          )
        )
      )
      (setq temp1 temp2 temp2 nil)
    )
    (reverse namelst)
  ) ;end
  ;; Джо Берк 2/23/03
  (defun Round (value to)
    (if (zerop to) value
      (* (atoi (rtos (/ (float value) to) 2 0)) to)))
  ;; Добавлено 9/22/2007. Проверить определение блока на тип объекта.
  ;; Возвращает T, если тип объекта найден.
  ;; Пример: (CheckBlock <blkname> "AcDbZombieEntity")
  (defun CheckBlock (blkname objtyp / flag)
    (setq objtyp (strcase objtyp))
    (vlax-for x (vla-item blocks blkname)
      (if (eq objtyp (strcase (vlax-get x 'ObjectName)))
        (setq flag T)
      )
    )
    flag
  )
  ;; Добавлено 9/22/2007.
  ;; Работает в сочетании с функцией CheckBlock выше.
  ;; Пример:
  ;; (if (CheckBlock <blkname> "AcDbZombieEntity") 
  ;;   (DeleteBlockProxies <blkname>)
  ;; )
  ;; Основная причина этой функции в том, что proxy-объект нельзя удалить
;;;  /// из определения блока с помощью (vla-delete <proxy obj>). Происходит ошибка.
  ;; Если блок содержит только proxy-объекты, удалить все ссылки (вставки) 
;;;  /// включая вложенные и удалить определение блока.
  ;; Если блок содержит proxy-объекты и другие типы объектов, скопировать объекты 
;;;  /// во временный блок. Proxy-объекты игнорируются в процессе. Затем изменить
;;;  /// имя всех ссылок на блок на имя временного блока. Удалить
;;;  /// исходное определение блока, что удаляет proxy-объекты. 
;;;  /// Затем изменить имя временного блока обратно на исходное имя блока.
  (defun DeleteBlockProxies (blkname / blkdef tempname org copylst tempblk)
    (setq blkdef (vla-item blocks blkname))
    (vlax-for x blkdef (setq copylst (cons x copylst)))
    ;; Это может быть if...
    (cond 
      ;; Блок содержит только proxy-объекты.
      ((vl-every 
        '(lambda (x) 
           (eq "AcDbZombieEntity" (vlax-get x 'ObjectName))
         )
         copylst
        )
        (vlax-for x blocks
          (if (eq acFalse (vlax-get x 'IsXref))
            (vlax-for i x
              (if 
                (and 
                  (eq "AcDbBlockReference" (vlax-get i 'ObjectName))
                  (eq (strcase blkname) (strcase (vlax-get i 'Name)))
                )
                (vla-delete i)
              )
            )
          )
        )
        (if (vl-catch-all-error-p 
              (vl-catch-all-apply 'vla-delete (list blkdef))
            )
          (setq proxyerror T)
        )
      )
      ;; Блок содержит proxy-объекты и другие типы объектов.
      (T 
        (if
          (and
            (setq tempname (strcat "%%%" blkname))
            (not (ValidItem blocks tempname))
            (setq org (vlax-get blkdef 'Origin))
            (setq tempblk (vlax-invoke blocks 'Add org tempname))
          )
          (progn
            (vlax-invoke doc 'CopyObjects (reverse copylst) tempblk)
            ;; Должно поймать все вложенные блоки.
            (vlax-for x blocks
              (if (eq acFalse (vlax-get x 'IsXref))
                (vlax-for i x
                  (if 
                    (and 
                      (eq "AcDbBlockReference" (vlax-get i 'ObjectName))
                      (eq (strcase blkname) (strcase (vlax-get i 'Name)))
                    )
                    (vlax-put i 'Name tempname)
                  )
                )
              )
            )
            (if 
              (not 
                (vl-catch-all-error-p 
                  (vl-catch-all-apply 'vla-delete (list blkdef))
                )
              )
              (vlax-put tempblk 'Name blkname)
              (setq proxyerror T)
            )
          )
        ) ;if
      ) ;cond T
    ) ;cond
  ) ;end DeleteBlockProxies
  ;;;;;;;; НАЧАЛО ВЛОЖЕННЫХ ФУНКЦИЙ ВЫРАВНИВАНИЯ ;;;;;;;;;
  (defun FlatPointObj (obj / coord)
    (if (not (TestNormal obj))
      (vlax-put obj 'Normal '(0.0 0.0 1.0))
    )
    (setq coord (vlax-get obj 'Coordinates))
    (CheckRename coord (ZZeroPoint coord))
    (vlax-put obj 'Coordinates (ZZeroPoint coord))
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
  ) ;end
  (defun FlatLine (obj / stpt enpt)
    (if (not (TestNormal obj))
      (progn
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (setq renameflag T)
      )
    )
    (setq stpt (vlax-get obj 'StartPoint))
    (CheckRename stpt (ZZeroPoint stpt))
    (vlax-put obj 'StartPoint (ZZeroPoint stpt))
    (setq enpt (vlax-get obj 'EndPoint))
    (CheckRename enpt (ZZeroPoint enpt))
    (vlax-put obj 'EndPoint (ZZeroPoint enpt))
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    ;; Если выравнивание сделало длину очень короткой, удалить линию. 
    (if (equal 0.0 (vlax-get obj 'Length) 1e-6)
      (progn
        (vla-delete obj)
        (setq renameflag T)
      )
    )
  ) ;end
  ;; Изменено 8/19/2007.
  (defun FlatMText (obj / ip apt ang ip1 ip2)
    (setq ip (vlax-get obj 'InsertionPoint))
    (CheckRename ip (ZZeroPoint ip))
    (if (TestNormal obj)
      (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
      (progn
        (setq apt (vlax-get obj 'AttachmentPoint))
        (vlax-put obj 'AttachmentPoint 1)
        (setq ip1 (vlax-get obj 'InsertionPoint))
        (vlax-put obj 'AttachmentPoint 2)
        (setq ip2 (vlax-get obj 'InsertionPoint))
        (setq ang (angle ip1 ip2))
        (vlax-put obj 'Normal '(0.0 0.0 1.0))       
        (vlax-put obj 'Rotation ang)
        (vlax-put obj 'AttachmentPoint apt)
        (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
        (setq renameflag T)
      )
    )
  ) ;end
  ;; Изменено 8/19/2007.
  (defun FlatText (obj / pt ip ap algn ang)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (if (TestNormal obj)
      (if (= 0 (vlax-get obj 'Alignment))
        (progn
          (setq ip (vlax-get obj 'InsertionPoint))
          (CheckRename ip (ZZeroPoint ip))
          (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
        )
        (progn
          (setq ap (vlax-get obj 'TextAlignmentPoint))
          (CheckRename ap (ZZeroPoint ap))
          (vlax-put obj 'TextAlignmentPoint (ZZeroPoint ap))
        )
      )
      ;; Если у текстового объекта нечетная нормаль.
      (progn
        (setq algn (vlax-get obj 'Alignment))
        (if (= 0 algn)
          (setq pt (vlax-get obj 'InsertionPoint))
          (setq pt (vlax-get obj 'TextAlignmentPoint))
        )
        ; Центральное выравнивание для получения угла.
        (vlax-put obj 'Alignment 1)
        (setq ang 
          (angle 
            (vlax-get obj 'InsertionPoint)
            (vlax-get obj 'TextAlignmentPoint)
          )
        )
        ; Восстановить предыдущее выравнивание.
        (vlax-put obj 'Alignment algn)
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (if (= 0 algn)
          (vlax-put obj 'InsertionPoint (ZZeroPoint pt))
          (vlax-put obj 'TextAlignmentPoint (ZZeroPoint pt))
        )
        (vlax-put obj 'Rotation ang)
        (setq renameflag T)
      ) ;progn нечетная нормаль
    ) ;if
    ;; Изменено, чтобы возвращать результирующий объект 9/3/2007.
    obj
  ) ;end FlatText
  ;; Преобразовать окружность с нечетной нормалью в эллипс или иначе.
  (defun FlatCircle (obj / ratio cen pt rad newobj)
    (cond
      ((TestNormal obj)
        (setq cen (vlax-get obj 'Center))
        (CheckRename cen (ZZeroPoint cen))
        (vlax-put obj 'Center (ZZeroPoint cen))
        (CheckRename (vlax-get obj 'Thickness) 0)
        (vlax-put obj 'Thickness 0.0)
      )
      ((TestZNormal obj))
      (T
        (setq ratio (abs (caddr (vlax-get obj 'Normal)))
              cen (ZZeroPoint (vlax-get obj 'Center))
              pt (ZZeroPoint (vlax-curve-getPointAtParam obj 0))
              rad (vlax-get obj 'Radius)
        )
        (cond
          ((equal ratio 0.0 1e-4)
            (FlatACE obj)
          ) 
          ((equal ratio 1.0 1e-4)
            (if (setq newobj (vlax-invoke (GetBlock) 'AddCircle cen rad))
              (ApplyProps obj newobj)
            )
          )
          (T
            (setq newobj (vlax-invoke (GetBlock) 
              'AddEllipse cen (mapcar '- cen pt) (abs ratio))
            )
            (ApplyProps obj newobj)
          )
        )
      )
    )
  ) ;end FlatCircle
  ;; Преобразовать дугу с нечетной нормалью в эллипс или иначе.
  (defun FlatArc (obj / ratio cen pt stpt enpt pt rad
                        newobj stparam enparam flag)
    (cond
      ((TestNormal obj)
        (setq cen (vlax-get obj 'Center))
        (CheckRename cen (ZZeroPoint cen))
        (vlax-put obj 'Center (ZZeroPoint cen))
        (CheckRename (vlax-get obj 'Thickness) 0)
        (vlax-put obj 'Thickness 0.0)
      )
      ((TestZNormal obj))
      (T
        (setq ratio (caddr (vlax-get obj 'Normal))
              cen (ZZeroPoint (vlax-get obj 'Center))
              stpt (ZZeroPoint (vlax-get obj 'StartPoint))
              enpt (ZZeroPoint (vlax-get obj 'EndPoint))
              rad (vlax-get obj 'Radius)
        )
        (if (minusp ratio)
          (setq ratio (abs ratio) flag T)
        )
        (cond
          ((< ratio 1e-4)
            (FlatACE obj)
          )
          ((equal ratio 1.0 1e-4)
            (if
              ;; Изменено 9/14/2007, чтобы удалить аргумент "stpt" по
              ;; комментарию Марко на theswamp.
              (setq newobj (vlax-invoke (GetBlock)
                'AddArc cen rad (angle cen stpt) (angle cen enpt))
              )
              (ApplyProps obj newobj)
            )
          )
          (T
            (vlax-put obj 'StartAngle 0.0)
            (setq pt (ZZeroPoint (vlax-curve-getStartPoint obj)))
            (setq newobj (vlax-invoke (GetBlock)
              'AddEllipse cen (mapcar '- cen pt) ratio)
            )
            ;; Эта идея из BreakMethod, кажется, работает.
            (setq pt (vlax-curve-getClosestPointTo newobj stpt)
                  stparam (vlax-curve-getParamAtPoint newobj pt)
                  pt (vlax-curve-getClosestPointTo newobj enpt)
                  enparam (vlax-curve-getParamAtPoint newobj pt)
            )
            ;; Если отношение (последнее значение нормали) 
            ;;/// было отрицательным, то параметр меняется местами.
            (if flag
              (progn
                (vlax-put newobj 'StartParameter enparam)
                (vlax-put newobj 'EndParameter stparam)
              )
              (progn
                (vlax-put newobj 'StartParameter stparam)
                (vlax-put newobj 'EndParameter enparam)
              )
            )
            (ApplyProps obj newobj)
          )
        ) ;cond
      ) ;progn
    ) ;if
  ) ;end FlatArc
  ;; Изменено 7/27/2007. Эллипс с нечетной нормалью, который не проходит
;;;  /// первые два условия, трассируется и преобразуется в полилинию.
;;;  /// Это позволяет избежать потенциальных ошибок "degenerate geometry", которые
;;;  /// не стоят риска, связанного с попыткой сохранить эллипс.
  (defun FlatEllipse (obj / cen) 
    (cond
      ((TestNormal obj)
        (setq cen (vlax-get obj 'Center))
        (CheckRename cen (ZZeroPoint cen))
        (vlax-put obj 'Center (ZZeroPoint cen))
      )
      ((TestZNormal obj))
      (T (FlatACE obj))
    )
  ) ;end
  ;; Изменено 7/26/2007. Трассировать объект, когда
  ;;/// нет другого безопасного способа выравнивания его.
  (defun FlatACE (obj / ptlst newobj objname)
    (setq ptlst (SF:TraceObject obj))
    (if (setq newobj (SF:MakeLWpolyline ptlst 0.0))
      (ApplyProps obj newobj)
      (progn
        (setq objname (vlax-get obj 'ObjectName)
              cnt (1+ cnt)
        )
        (if (not (vl-position objname notflatlst))
          (setq notflatlst (cons objname notflatlst))
        )
      )
    )
  ) ;end
  ;; Тяжелые и легкие полилинии.
  ;; Тяжелая полилиния преобразуется в легкую, если она
;;;  /// трассируется с использованием SF:TraceObject.
  (defun FlatPline (obj / width ptlst newobj)
    (cond 
      ((TestNormal obj)
        (CheckRename (vlax-get obj 'Elevation) 0)
        (vlax-put obj 'Elevation 0.0)
        (CheckRename (vlax-get obj 'Thickness) 0)
        (vlax-put obj 'Thickness 0.0)
      )
      ((TestZNormal obj))
      (T   
        ;; Если у полилинии были различные ширины, новая ширина равна нулю.
        ;; Кажется, ничего нельзя сделать с этим.
        (if 
          (vl-catch-all-error-p 
            (setq width 
              (vl-catch-all-apply 'vlax-get (list obj 'ConstantWidth))
            )
          )
          (setq width 0.0)
        )
        (setq ptlst (SF:TraceObject obj))
        (if (setq newobj (SF:MakeLWpolyline ptlst width))
          (ApplyProps obj newobj)
        )
      )
    )
  ) ;end
  ;; PolyFaceMesh объект с одним лицом нужно взорвать в
;;;  /// 3D Face перед тем, как FlatCoordinates ниже сможет обработать его.
  (defun FlatPolyFaceMesh (obj / mark)
    (if (/= 1 (vlax-get obj 'NumberOfFaces))
      (FlatCoordinates obj)
      (progn
        (setq mark (entlast))
        (command-new (list "._explode" (vlax-vla-object->ename obj)))
        (if (not (eq mark (entlast)))
          (FlatCoordinates (vlax-ename->vla-object (entlast)))
        )
      )
    )
  ) ;end
  ;; Добавленная функция 9/11/2007. См. историю версий 1.2b.
  (defun FlatMLeader (obj / mn mx n1 n2)
    (vla-GetBoundingBox obj 'mn 'mx)
    (setq n1 (caddr (vlax-safearray->list mn)))
    (setq n2 (caddr (vlax-safearray->list mx)))
    (cond
      ;; Не нуждается в выравнивании.
      ((and 
         (equal 0 n1 1e-8)
         (equal 0 n2 1e-8)
       )
      )
      ;; Переместить, если параллельно WCS.
      ((equal n1 n2 1e-8)
        (vlax-invoke obj 'Move (list 0.0 0.0 n1) '(0.0 0.0 0.0))
      )
      ;; Это должно происходить, но есть ошибка в 2008 с SP1
;;;      /// которая вызывает вылет стрелок лидера в пространство.
      ;; Изменено 1/12/2010, чтобы позволить выравнивание mleader объектов.
      ;; Обратите внимание, внутри функции CommandExplode
;;;      /// стрелки-тела, которые ведут себя неправильно, удаляются. Я думаю, что это лучшая альтернатива
;;;      /// чем не выравнивать mleaders, как раньше.
      (T (CommandExplode obj))
      ;(T
      ;  (setq cnt (1+ cnt)
      ;        objname (vlax-get obj 'ObjectName)
      ;  )
      ;  (if (not (vl-position objname notflatlst))
      ;    (setq notflatlst (cons objname notflatlst))
      ;  )
      ;)
    )
  ) ;end FlatMLeader
  ;; ObjectName "AcDbMLeader"
  ;; Аргумент: mleader vla-объект. Попытка изменения координат
;;;  /// не работает по неизвестной причине.
  ;(defun FlatMLeader (obj / idx vert)
  ;  (setq idx 0)
  ;  (repeat (vlax-get obj 'LeaderCount)
  ;    (setq vert (vlax-invoke obj 'GetLeaderLineVertices idx))
  ;    (vlax-invoke obj 'SetLeaderLineVertices idx (ZZeroCoord vert))
  ;    (setq idx (1+ idx))
  ;  )
  ;)
  ;; Добавленная функция 8/29/2007. См. историю версий 1.2a.
  (defun FlatLeader (obj / coord zlst n)
    (setq coord (vlax-get obj 'Coordinates))
    (repeat (/ (length coord) 3)
      (setq zlst (cons (caddr coord) zlst)
            coord (cdddr coord)
      )
    )
    (setq n (car zlst))
    (cond
      ((vl-every '(lambda (z) (equal 0.0 z 1e-6)) zlst))
      ((vl-every '(lambda (z) (equal n z 1e-6)) (cdr zlst))
        (vlax-invoke obj 'Move (list 0.0 0.0 n) '(0.0 0.0 0.0))
      )
      (T 
        (FlatCoordinates obj)
      )
    )
  ) ;end
  ;; Обрабатывает 3DPoly, 3DFace, PolyFaceMesh, PolygonMesh и Leader объекты.
  ;; PolyFaceMesh объект может нуждаться в предварительной обработке. См. функцию выше.
  ;; Обратите внимание, изменение координат лидера, когда не
;;;  /// в WCS может изменить его нормаль. Одна из причин, почему программа выравнивается в WCS.
  (defun FlatCoordinates (obj / coord objname)
    (setq coord (vlax-get obj 'Coordinates))
    (if 
      (vl-catch-all-error-p
        (vl-catch-all-apply
          '(lambda () (vlax-put obj 'Coordinates (ZZeroCoord coord)))
        )
      )
      (progn
        (setq cnt (1+ cnt)
              objname (vlax-get obj 'ObjectName)
        )
        (if (not (vl-position objname notflatlst))
          (setq notflatlst (cons objname notflatlst))
        )
      )
    )
    (CheckRename (vlax-get obj 'Coordinates) coord)
  ) ;end
  ;; Нормаль сплайна не открыта под Active X.
;;;  /// Нет нормали, если сплайн не плоский.
;;;  /// См. свойство IsPlanar. Так что если он плоский и
;;;  /// списки координат равны с fuzz, сплайн
;;;  /// не нужно изменять.
;;;  /// Если он изменен, сплайн не должен изменить форму.
;;;  /// Что произойдет, так это то, что любые точки привязки могут быть потеряны.
;;;  /// Кажется, нормально, так как точки привязки иногда удаляются во время
;;;  /// операций редактирования сплайна. Или он может не иметь 
;;;  /// точек привязки изначально.
  (defun FlatSpline (obj / ctrlpts testpts kts)
    (setq ctrlpts (vlax-get obj 'ControlPoints)
          testpts (ZZeroCoord ctrlpts)         
    )
    ;; Изменено 8/17/2007 - исправление ошибки.
    (if 
      (or 
        (eq acFalse (vlax-get obj 'IsPlanar))
        (not (equal ctrlpts testpts 1e-8))
      )
      (progn
        (setq kts (vlax-get obj 'Knots))
        (vlax-put obj 'ControlPoints testpts)
        (vlax-put obj 'Knots kts)
        (setq renameflag T)
      )
    )
  ) ;end
  ;|
  ;; Не используется в настоящее время.
  ;; Аргумент: ename размера
  ;; Пример для вращенного или выровненного размера
  ;; Возвращает: 1, если первая точка размера ассоциативна
  ///          2, если вторая точка размера ассоциативна
  ///          3, если обе точки ассоциативны
  ///          nil, если размер не ассоциативен
  ;; Примечание: возвращаемое значение - это битовая сумма флага ассоциативности.
  /// 1 = Ссылка на первую точку
  /// 2 = Ссылка на вторую точку
  /// 4 = Ссылка на третью точку
  /// 8 = Ссылка на четвертую точку
  (defun DimAssoc (dim / elst dict)
    (if (= (type dim) 'VLA-OBJECT)
      (setq dim (vlax-vla-object->ename dim))
    )
    (and 
      (setq elst (entget dim))
      (setq dict (cdr (assoc 360 elst))) ;dictionary ename
      (setq elst (entget dict))
      (setq elst (entget (cdr (assoc 360 elst)))) ;DIMASSOC elst
    )
    (cdr (assoc 90 elst))
  ) ;end
  |;
  ;; Метод взрыва не работает с размерными объектами.
  ;; Провел некоторое время с этим. Взрыв размеров с нечетными нормалями
;;;  /// кажется лучшим подходом на данный момент.
(defun FlatDimension (obj / z pt proplst e elst dxf13 dxf14 dxf10 dxf15 objname)
  (if (TestNormal obj)
    (progn
      ;; Безопасное отсоединение
      (vl-catch-all-apply 'vl-cmdf (list "._dimdisassociate" (vlax-vla-object->ename obj) ""))
      
      ;; Классический безопасный сдвиг размера
      (setq z (caddr (vlax-get obj 'TextPosition)))
      (CheckRename z 0)
      (if (not (zerop z))
        (vlax-invoke obj 'Move (list 0.0 0.0 z) '(0.0 0.0 0.0))
      )
      
      ;; Безопасный перебор стандартных точек
      (setq proplst '("ExtLine1Point" "ExtLine2Point" "ExtLine1StartPoint" 
                      "ExtLine2StartPoint" "ExtLine1EndPoint" "ExtLine2EndPoint"
                      "Center" "ChordPoint" "FarChordPoint" "AngleVertex" "ArcPoint")
      )
      (foreach p proplst
        (if (vlax-property-available-p obj p)
          (progn
            (setq pt (vlax-get obj p))
            (CheckRename pt (ZZeroPoint pt))
            (vl-catch-all-apply 'vlax-put (list obj p (ZZeroPoint pt)))
          )
        )
      )
      
      (setq e (vlax-vla-object->ename obj))
      (setq elst (entget e))
      
      ;; 1. Оригинальный код автора для ЛИНЕЙНЫХ размеров (DXF 13 и 14)
      (if (setq dxf13 (assoc 13 elst))
        (progn
          (setq dxf13 (list 13 (cadr dxf13) (caddr dxf13) 0.0))
          (entmod (subst dxf13 (assoc 13 elst) elst))
          (setq elst (entget e))
        )
      )
      (if (setq dxf14 (assoc 14 elst))
        (progn
          (setq dxf14 (list 14 (cadr dxf14) (caddr dxf14) 0.0))
          (entmod (subst dxf14 (assoc 14 elst) elst))
          (setq elst (entget e))
        )
      )
      
      ;; 2. НАША ЗАПЛАТКА: Работает ТОЛЬКО для радиальных/диаметральных размеров (DXF 10 и 15)
      ;; Линейные размеры сюда не попадают, поэтому они больше не будут ломаться!
      (setq objname (vlax-get obj 'ObjectName))
      (if (wcmatch objname "*Radial*,*Diametric*")
        (progn
          (if (setq dxf10 (assoc 10 elst))
            (progn
              (setq dxf10 (list 10 (cadr dxf10) (caddr dxf10) 0.0))
              (entmod (subst dxf10 (assoc 10 elst) elst))
              (setq elst (entget e))
            )
          )
          (if (setq dxf15 (assoc 15 elst))
            (progn
              (setq dxf15 (list 15 (cadr dxf15) (caddr dxf15) 0.0))
              (entmod (subst dxf15 (assoc 15 elst) elst))
            )
          )
        )
      )
    )
    ;; Иначе: взрываем
    (CommandExplode obj)
  )
)

  ;end FlatDimension
  ;; Сначала изменить нормаль, затем IP.
  (defun FlatXref (obj / ip nrml)
    (setq ip (vlax-get obj 'InsertionPoint)
          nrml (vlax-get obj 'Normal)
    )
    (if (not (TestNormal obj))
      (progn 
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (RotateToNormal obj nrml)
      )
    )
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
  ) ;end
  (defun FlatTolerance (obj / ip nrml)
    (setq ip (vlax-get obj 'InsertionPoint)
          nrml (vlax-get obj 'Normal)
    ) 
    (if (not (TestNormal obj))
      (progn
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (setq renameflag T)
      )
    )
    (CheckRename ip (ZZeroPoint ip))
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
  ) ;end
  ;; Изменено 9/4/2007. 
;;;  /// Выравнивание minsert с нечетной нормалью на основе кода Джейсона Пирса.
;;;  /// Такие объекты обрабатываются так же, как обычный блок с нечетной нормалью.
;;;  /// Взорвать и выровнять результаты.
  (defun FlatMInsert (obj / ip attlst colcnt rowcnt colspc rowspc collst 
                            rowlst xfac yfac inspt rot bname blknum index 
                            inslst attlst txtlst vlaip newip nrml newblk 
                            newobj blklst)
    (if (TestNormal obj)
      (progn
        (setq ip (vlax-get obj 'InsertionPoint))
        (CheckRename ip (ZZeroPoint ip))
        (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
        (setq attlst (vlax-invoke obj 'GetAttributes))
        (foreach x attlst
          (CheckRename (vlax-get x 'Thickness) 0)
          (vlax-put x 'Thickness 0.0)         
          ;; FlatText проверяет на переименование.
          (FlatText x)
        )
      )
      (progn
        (setq colcnt (vlax-get obj 'Columns)
              rowcnt (vlax-get obj 'Rows)
        )
        (if (or (> colcnt 1) (> rowcnt 1))
          (progn 
            (setq bname (vlax-get obj 'Name) 
                  lay (vlax-get obj 'Layer)
                  xfac (vlax-get obj 'XScaleFactor) 
                  yfac (vlax-get obj 'YScaleFactor) 
                  rot (vlax-get obj 'Rotation) 
                  colspc (vlax-get obj 'ColumnSpacing) 
                  rowspc (vlax-get obj 'RowSpacing)
                  nrml (vlax-get obj 'Normal)
                  blknum (* rowcnt colcnt)
            )
            (if (setq attlst (vlax-invoke obj 'GetAttributes))
              (setq txtlst (AttributesToText attlst))
            )
            ;; Точка вставки minsert, возвращаемая entget, не такая же,
            ;;/// как показывает свойства и что возвращает этот (vlax-get obj 
            ;;/// 'InsertionPoint), когда у объекта нечетная нормаль.
            ;; Но это (inspt) - точка, которая работает. Не уверен, почему.
            (setq inspt (cdr (assoc 10 (entget (vlax-vla-object->ename obj))))
                  vlaip (ZZeroPoint (vlax-get obj 'InsertionPoint))
            )
            (setq index 1)
            (repeat (1- colcnt)
              (setq collst (cons (polar inspt rot (* index colspc)) collst))
              (setq index (1+ index)) 
            )
            (setq collst (cons inspt collst))
            (setq index 1)
            (foreach x collst 
              (progn 
                (repeat (1- rowcnt) 
                  (progn 
                    (setq rowlst (cons (polar x (+ rot (* pi 0.5)) (* index rowspc)) rowlst)) 
                    (setq index (1+ index))
                  ) 
                ) 
                (setq index 1)
              ) 
            ) 
            (foreach x rowlst (setq inslst (cons x inslst)))
            (foreach x collst (setq inslst (cons x inslst)))
            (setq index 0)
            (repeat blknum 
              (entmake (list '(0 . "INSERT") 
                             '(100 . "AcDbEntity") 
                             (cons 8 lay) 
                             '(100 . "AcDbBlockReference") 
                             (cons 2 bname) 
                             (cons 10 (nth index inslst)) 
                             (cons 41 xfac) 
                             (cons 42 yfac) 
                             (cons 50 rot)                    
                             (cons 210 nrml)
                       )
              )
              (setq newblk (vlax-ename->vla-object (entlast))
                    blklst (cons newblk blklst)
                    newip (ZZeroPoint (vlax-get newblk 'InsertionPoint))
              )
              (foreach i txtlst
                (setq newobj (vlax-invoke i 'Copy))
                (vlax-invoke newobj 'Move vlaip newip)
              )
              (setq index (1+ index))
            ) ;repeat
            ;; Удалить объект minsert.
            (vla-delete obj)
            ;; Удалить текстовые объекты, созданные изначально.
            (mapcar 'vla-delete txtlst)
            (setq renameflag T)
            ;; Передать новые блочные объекты в ProcessList для выравнивания.
            ;; Новые текстовые объекты, если есть, уже плоские.
            (ProcessList blklst)
          ) ;progn
        ) ;progn
      ) ;if
    ) ;if
  ) ;end FlatMInsert

;замена acet-ss-remove
  (defun ss-remove-alt (ss-base ss-target / i ent result-ss)
  (setq result-ss (ssadd))
  (if (and ss-base ss-target)
    (progn
      (setq i 0)
      (repeat (sslength ss-target)
        (setq ent (ssname ss-target i))
        (if (null (ssmemb ent ss-base))
          (ssadd ent result-ss)
        )
        (setq i (1+ i))
      )
    )
  )
  result-ss
)
  ;; Метод взрыва не работает с объектами штриховки.
  ;; Переменная patlst локальная в основной функции.
  ;; Если штриховка имеет нечетную нормаль и имя шаблона штриховки
;;;  /// AR-CONC или AR-SAND, штриховка воссоздается для выравнивания.
;;;  /// В противном случае штриховка взрывается и результат выравнивается
;;;  /// для сохранения визуальной целостности.
  ;; Свойство HatchObjectType определяет обычную
;;;  /// штриховку (ноль) или градиентную штриховку (один). Градиентная штриховка
;;;  /// была новой в ACAD 2004. Как сплошная штриховка, градиентная штриховка
;;;  /// не может быть взорвана. Если у градиентной штриховки нечетная нормаль
;;;  /// она преобразуется в сплошную штриховку для выравнивания.
  ;; Команда -hatchedit не работает с градиентной штриховкой.
  ;; Градиенты должны редактироваться с использованием диалога hatchedit.
  ;; Стиль HatchStyle определяет Normal, Outer, Ignore.
  ;; Стиль аргументного объекта применяется к новой штриховке
;;;  /// если она создается.
  ;; Исправлена проблема здесь, когда интервал штриховки существующей
;;;  /// штриховки слишком плотный, поэтому создание новой штриховки (или воссоздание границы) не удается.
;;;  /// Из-за того, что кто-то изменил определение стандартного шаблона штриховки?
  (defun FlatHatch (obj / rtd patname mark ss sset newobj 
                      mn mx scale GetHatchScale ss-before ss-after)
  ;; Сохраняем текущий набор всех объектов в пространстве листа
  (setq ss-before (ssget "_X" (list (cons 410 (getvar "ctab")))))
  
  ;; радианы в градусы
  (defun rtd (radians)
     (/ (* radians 180.0) pi)
  ) ;end
  
  ;; Добавлено 9/14/2007.
  ;; Аргумент: hatch vla-объект.
  ;; Возвращает исправленное значение масштаба, когда объект штриховки был создан
  ;; с использованием нестандартного определения штриховки.
  (defun GetHatchScale (obj / stdoffsetlst patname elst scl ang 
                            xoff yoff yspacing offset)
    ;; Пример ("ANSI31" 0.125 3.175)
    (setq stdoffsetlst 
      '(("AR-SAND" 1.567 39.8018)
        ("AR-CONC" -5.89789472 -149.807)
        ("ANSI31" 0.125 3.175)
        ("ANSI32" 0.375 9.525)
        ("ANSI33" 0.25 6.35)
        ("ANSI34" 0.75 19.05)
        ("ANSI35" 0.25 6.35)
        ("ANSI36" 0.125 3.175)
        ("ANSI37" 0.125 3.175)
        ("ANSI38" 0.125 3.175))
    )
    (setq patname (vlax-get obj 'PatternName))
    (if (vl-position patname (mapcar 'car stdoffsetlst))
      (progn
        (setq
          scl (vlax-get obj 'PatternScale)
          elst (entget (vlax-vla-object->ename obj))
          ang (cdr (assoc 53 elst))
          xoff (cdr (assoc 45 elst))
          yoff (cdr (assoc 46 elst))
          yspacing (/ (- (* yoff (cos ang)) (* xoff (sin ang))) scl)
        )
        (if (zerop (getvar "measurement"))
          (setq offset (car (cdr (assoc patname stdoffsetlst))))
          (setq offset (cadr (cdr (assoc patname stdoffsetlst))))
        )
        (abs (* scl (/ yspacing offset)))
      )
    )
  ) ;end
  
  (cond 
    ((TestNormal obj)
      (CheckRename (vlax-get obj 'Elevation) 0)
      (vlax-put obj 'Elevation 0.0)
    )
    ((TestZNormal obj))
    ;; Градиентная штриховка может быть изменена на сплошную.
    ((and
       (vlax-property-available-p obj 'HatchObjectType)
       (= 1 (vlax-get obj 'HatchObjectType))
      )
      (vlax-put obj 'HatchObjectType 0)
      (ProcessList (list obj))
    )     
    ;; Попытка воссоздать границу и создать новую штриховку.
    ((and
       ;; Recreate boundary введено в 2006.
       (>= (atof (getvar "AcadVer")) 16.2)
       (or patlst (setq patlst (LstACADPAT)))
       (setq patname (vlax-get obj 'PatternName))
       (vl-position patname patlst)
       (or 
         (if (eq "SOLID" patname) (setq scale 1.0))
         (setq scale (GetHatchScale obj))
       )
       ;; Избежать сообщения об отключении ассоциативности границы штриховки.
       (not (vlax-put obj 'AssociativeHatch 0))
       (setq mark (entlast))
       (not (command "._hatchedit" (vlax-vla-object->ename obj) "_b" "_p" "_n"))
       ;; Получаем все объекты после выполнения команды
       (setq ss-after (ssget "_X" (list (cons 410 (getvar "ctab")))))
       ;; Находим новые объекты (разница между ss-after и ss-before)
       (if (not (vl-symbol-value 'acet-ss-remove-dups))
	 (setq sset (ss-remove-alt ss-before ss-after))
	 (setq sset (acet-ss-remove ss-before ss-after)))
       ;; Избежать проблем с увеличением
       (not (command "._zoom" "_object" sset ""))
       (if hpa (setvar "hpassoc" 0))
       (not (command "._hatch" patname scale
              (rtd (vlax-get obj 'PatternAngle)) "_s" sset ""
            )
       )
       ;; Восстановить предыдущее увеличение.
       (not (command "._zoom" "_previous"))
       ;; Удалить объекты границы
       (if sset
         (mapcar 'vla-delete (SSVLAList sset))
       )
       (setq newobj (vlax-ename->vla-object (entlast)))
       (eq "AcDbHatch" (vlax-get newobj 'ObjectName))
       ;; Обновляет штриховку.
       (not (vl-catch-all-error-p 
         (vl-catch-all-apply 'vlax-invoke 
           (list newobj 'Evaluate))))
     ) ;and
      (vlax-put newobj 'HatchStyle (vlax-get obj 'HatchStyle))
      (ApplyProps obj newobj)
    )
    (T (CommandExplode obj))
  ) ;cond
) ;end FlatHatch 
  ;; AcDbSolid 
  ;; Добавлено 9/1/2007. Разделено из предыдущей функции FlatSolidOrTrace.
;;;  /// Хотя свойства показывают значение Elevation для сплошного объекта,
;;;  /// vla-объект не имеет свойства elevation.
;;;  /// Сначала получить координаты, затем изменить нормаль, затем установить zzerocoord.
  (defun FlatSolid (obj / coord)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (setq coord (vlax-get obj 'Coordinates))
    (cond
      ((TestNormal obj)
        (CheckRename coord (ZZeroCoord coord))
        (vlax-put obj 'Coordinates (ZZeroCoord coord))
      )
      ((TestZNormal obj))
      (T
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (vlax-put obj 'Coordinates (ZZeroCoord coord))
        (setq renameflag T)
      )
    )
  ) ;end FlatSolid
  ;; AcDbTrace
  ;; Добавлено 9/1/2007. Разделено из предыдущей функции FlatSolidOrTrace.             
  (defun FlatTrace (obj / coord)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (setq coord (vlax-get obj 'Coordinates))
    (cond
      ((TestNormal obj)
        (CheckRename coord (ZZeroCoord coord))
        (vlax-put obj 'Coordinates (ZZeroCoord coord))
      )
      (T
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (vlax-put obj 'Coordinates (ZZeroCoord coord))
        (setq renameflag T)
      )
    )
  ) ;end
  (defun FlatShape (obj / ip nrml)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (setq ip (vlax-get obj 'InsertionPoint)
          nrml (vlax-get obj 'Normal)
    )
    (if (not (TestNormal obj))
      (progn 
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (RotateToNormal obj nrml)
        (setq renameflag T)
      )
    )
    (CheckRename ip (ZZeroPoint ip))
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
  ) ;end
  (defun FlatRayOrXline (obj / bp sp dv)
    (setq bp (vlax-get obj 'BasePoint))
    (CheckRename bp (ZZeroPoint bp))
    (vlax-put obj 'BasePoint (ZZeroPoint bp))
    (setq sp (vlax-get obj 'SecondPoint))
    (CheckRename sp (ZZeroPoint sp))
    (vlax-put obj 'SecondPoint (ZZeroPoint sp))
    (setq dv (vlax-get obj 'DirectionVector))
    (CheckRename dv (ZZeroPoint dv))
    (vlax-put obj 'DirectionVector (ZZeroPoint dv))
  ) ;end
  ;; AcDbRasterImage или AcDbWipeout
  ;; Выравнивание растровых (изображений) объектов и вытеснений, которые не 
;;;  /// параллельны WCS. Открыто случайно, изменение свойства вращения 
;;;  /// выравнивает те, которые не параллельны WCS.
  (defun FlatWipeoutOrRaster (obj / org)
    (vlax-put obj 'Rotation (vlax-get obj 'Rotation))
    (setq org (vlax-get obj 'Origin))
    (CheckRename org (ZZeroPoint org))
    (vlax-put obj 'Origin (ZZeroPoint org))
  ) ;end
  ;; Как объект Table, свойство Normal не открыто.
;;;  /// Изменить нормаль с помощью entmod.
  (defun FlatMline (obj / ename elst mark lst ptlst pts z line)
    (setq ename (vlax-vla-object->ename obj))
    (cond
      ((TestZNormal ename))
      ;; Выравнивание mline с нечетной нормалью.
      ((not (TestNormal ename))
        (setq elst (entget ename))
        (entmod (subst (cons 210 '(0.0 0.0 1.0)) (assoc 210 elst) elst))
        ;; Это нужно для выравнивания, хотя и не на Z ноль. Странно.
        (vlax-put obj 'Coordinates (ZZeroCoord (vlax-get obj 'Coordinates)))
        ;; Все Z значения должны быть одинаковыми на этом этапе.
        (setq z (caddr (vlax-get obj 'Coordinates)))
        (if (not (zerop z))
          (vlax-invoke obj 'Move (list 0.0 0.0 z) '(0.0 0.0 0.0))
        )
        (setq renameflag T)
      )
      ;; Выравнивание на Z ноль координат. Переместить, потому что это
;;;      /// (vlax-put obj 'Coordinates (ZZeroCoord (vlax-get obj 'Coordinates)))
;;;      /// не работает для изменения Z значений.
      (T
        (setq z (caddr (vlax-get obj 'Coordinates)))
        (CheckRename z 0)
        (if (not (zerop z))
          (vlax-invoke obj 'Move (list 0.0 0.0 z) '(0.0 0.0 0.0))
        )
      )
    )
  ) ;end FlatMline
  ;; Свойство Direction похоже на свойство угла или вращения.
;;;  /// Нормаль не открыта под Active X, но
;;;  /// доступна с использованием entget. Использовать entmod для изменения.
  (defun FlatTable (obj / ename elst nrml ip dir)
    (setq ename (vlax-vla-object->ename obj)
          elst (entget ename)
          nrml (cdr (assoc 210 elst))
          ;Исходная ip для случая, когда нормаль изменяется.
          ip (vlax-get obj 'InsertionPoint)
          dir (vlax-get obj 'Direction)
    )
    (if 
      (not
        (or
          ;; удалено fuzz 6/6/2007
          (equal nrml '(0.0 0.0 1.0))
          (equal nrml '(0.0 0.0 -1.0))
        )
      )
      (progn
        (entmod (subst (cons 210 '(0.0 0.0 1.0)) (assoc 210 elst) elst))
        (setq renameflag T)
      )
    )
    (CheckRename ip (ZZeroPoint ip))
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
    (CheckRename dir (ZZeroPoint dir))
    ;; Исправить случай, когда Z значение направления такое (0.569751 0.0 0.821818).
    (vlax-put obj 'Direction (ZZeroPoint dir))
    (vlax-invoke obj 'RecomputeTableBlock acTrue)
  ) ;end
  ;; Области взрываются. Кажется, нет другого способа справиться с ними.
  ;; Изменено 9/9/2007 - добавлено условие.
  ;; Изменено 1/18/2011 с благодарностью Питеру Б. на свопе за его сообщение об ошибке
;;;  /// о том, что некоторые области нельзя взорвать.
;;;  /// Это вызывало ошибку в версии SF 1.2e.
  (defun FlatRegion (obj / lst)
    (cond
      ((TestZNormal obj))
      (T 
       (if
         (not (vl-catch-all-error-p 
           (setq lst (vl-catch-all-apply 'vlax-invoke (list obj 'Explode)))))
          (progn
            (vla-delete obj)
            (setq renameflag T)
            (ProcessList lst)
          )
          ;Иначе отправить объект в WMFOutIn, если 2005 или новее.
          (if (>= version 16.2)
            (WMFOutIn obj)
            ;; Иначе считать как не выровненный.
            (progn
              (setq cnt (1+ cnt))
              (if (not (vl-position "AcDbRegion" notflatlst))
                (setq notflatlst (cons "AcDbRegion" notflatlst))
              )
            )
          )
        )
      )
    )
  ) ;end
  ;;;;;;;; КОНЕЦ ВЛОЖЕННЫХ ФУНКЦИЙ ВЫРАВНИВАНИЯ ;;;;;;;;;;;;;
  ;; ProcessList
  ;; Аргумент: список vla-объектов.
  ;; Его основная функция - отправлять vla-объекты в другие вложенные функции
;;;  /// для обработки. Все объекты проходят через эту функцию.
  ;; Она обрабатывает индикатор прогресса Spinbar. 
;;;  /// Также проверяет наличие объектов, которые могут быть удалены в другом месте.
;;;  /// И делает предварительную обработку для проверки очень маленьких объектов,
;;;  /// которые обнаружены.
  (defun ProcessList (lst / objname)
    (setq jpro (+ jpro 1))
    ;; Сообщить, выравниваются ли определения блоков 
;;;    /// или набор выбора.
       (if (or inoutlst WMFflag)
      (princ 
        (strcat "\rВыравнивание сложных объектов, пожалуйста, не отменяйте... " 
          (setq *sbar (Spinbar *sbar)) "\t"
       (itoa jpro)
		)
      ))

;;;    (if lst
;;;    (setq i (length lst))
;;;     )
    
    
    (foreach x lst
      ;проверочный код проверкі
;;;      (princ 
;;;          (strcat "\nProcessList (выравнивание выборки)... "
;;;		  (itoa (setq i (- i 1))) " "
;;;            (setq *sbar (Spinbar *sbar))
;;;	    (itoa jpro)
;;;	    (vlax-get x 'ObjectName)
;;;		  )
;;;        )
;;;      
;;;      (if (and (not (vlax-erased-p x))
;;;	       (= i 0)
;;;	       (= jpro 99)
;;;	       )
;;;	(progn 
;;;	  (princ (vlax-get x 'ObjectName)) (princ)
;;;	  (princ (itoa jpro)) (princ)
;;;	  )
;;;	)
      (if  (and 
            (not (vlax-erased-p x))
            ;; Проверяем: если у объекта нет свойства Layer, пропускаем проверку. Если есть - фильтруем *ADSK*
            (or (not (vlax-property-available-p x 'Layer))
                (not (wcmatch (strcase (vlax-get x 'Layer)) "`*ADSK*")))
	    ;; ХИРУРГИЧЕСКАЯ ВСТАВКА: Пропускаем объекты, которые УЖЕ лежат строго на Z=0
            (NeedsFlattening x)
          )
        (progn
          (setq objname (vlax-get x 'ObjectName))
	  (cond
	  	        ((and 
                (eq "AcDbBlockReference" objname)
                (vlax-property-available-p x 'Path)
              )
              (FlatXref x)
				)
	           ((eq "AcDbBlockReference" objname)
              (vlax-for y (vla-item blocks (vlax-get x 'Name)) 
                (setq templst (cons y templst))
              )
              (if 
                (and 
                  (vl-every '(lambda (z)
                    (eq "AcDbBlockReference" (vlax-get z 'ObjectName))) 
                      templst)
                  (ModBlockScale x)
                )
                (progn
                  ;; Изменено 1/10/2008. Добавлена проверка слоя, обычно
;;;                  /// выполняемая в ExpBlockMethod и CommandExplode.
                  (setq lay (vlax-get x 'Layer))
                  (setq templst (vlax-invoke x 'Explode))
                  (foreach i templst
                    (if (eq "0" (vlax-get i 'Layer))
                      (vlax-put i 'Layer lay)
                    )
                    (if (zerop (vlax-get x 'Color))
                      (vlax-put x 'Color 256)
                    )
                  )
                  (ProcessList templst)
                  (vla-delete x)
                )
                (ExpBlockMethod x)
              ) ;if
              (setq templst nil lay nil)
            )  
			((or
                (eq "AcDbPolyline" objname)
                (eq "AcDb2dPolyline" objname)
              )
              (if (< (vlax-curve-getDistAtParam x (vlax-curve-getEndParam x)) 1e-6)
                (vla-delete x)
                (FlatPLine x)
              )
            )
            ((eq "AcDbLine" objname)
              (if (< (vlax-get x 'Length) 1e-6)
                (vla-delete x)
                (FlatLine x)
              )
            )
			((or
               (eq "AcDbText" objname)
               (eq "AcDbAttribute" objname)
               (eq "AcDbAttributeDefinition" objname)
              )
              (FlatText x)
            )
            ((eq "AcDbMText" objname)
              (FlatMText x)
            )
            ;; Изменено 7/16/2007 - взорвать блок, который только
;;;            /// содержит другие блоки. Это не сработает, если ссылка на блок
;;;            /// NUS за пределами того, что делает ModBlockScale.
 			
            ((eq "AcDbCircle" objname)
              (if (< (vlax-get x 'Radius) 1e-6)
                (vla-delete x)
                (FlatCircle x)
              )
            )
            ((eq "AcDbArc" objname)
              (if 
                (or
                  (< (vlax-get x 'TotalAngle) 1e-6)
                  (< (vlax-get x 'Radius) 1e-6)
                )
                (vla-delete x)
                (FlatArc x)
              )
            )
            ((eq "AcDbEllipse" objname)
              (if
                (and
                  (< (vlax-get x 'MajorRadius) 1e-6)
                  (< (vlax-get x 'MinorRadius) 1e-6)
                )
                (vla-delete x)
                (FlatEllipse x)
              )
            )
            ((eq "AcDbSpline" objname)
              (if (< (vlax-curve-getEndParam x) 1e-6)
                (vla-delete x)
                (FlatSpline x)
              )
            )
            ;; Добавлено 9/11/2007.
            ((eq "AcDbMLeader" objname) 
              (FlatMLeader x)
            )
            ;; Добавлено 8/29/2007.
            ((eq "AcDbLeader" objname)
              (FlatLeader x)
            )
            ((or
                (eq "AcDb3dPolyline" objname)
                (eq "AcDbFace" objname)
                (eq "AcDbPolygonMesh" objname)
              )
              (FlatCoordinates x)
            )
            ((eq "AcDbPolyFaceMesh" objname)
              (FlatPolyFaceMesh x)
            )
            ((eq "AcDbPoint" objname)
              (FlatPointObj x)
            )   
            ((eq "AcDbTable" objname)
              (FlatTable x)
            )
            ((eq "AcDbHatch" objname)
              (FlatHatch x)
            )
            ((wcmatch objname "*Dimension*")
              (FlatDimension x)
            )
            ((eq "AcDbRegion" objname)
              (FlatRegion x)
            )
            ;; Имейте в виду команду 2008 "flatshot" 
;;;            /// для выравнивания 3D сплошных объектов.
            ((or
               (eq "AcDb3dSolid" objname) 
               (eq "AcDbSurface" objname)
               ;; Добавлено 2/4/2011.
               (eq "AcDbPlaneSurface" objname)
              )
              ;; Добавлено 2/6/2011. Использовать WMF out in с 2005 или новее.
              ;; В противном случае использовать функцию CommandExplode.
              (if (>= version 16.1)
                (WMFOutIn x)
                (CommandExplode x)
              )
            )
            ;; Добавлено 2/7/2011.
            ;; Объекты Body не могут быть взорваны.
            ((eq "AcDbBody" objname)
              (if (>= version 16.1)
                (WMFOutIn x)
                (progn
                  (setq cnt (1+ cnt))
                  (if (not (vl-position "AcDbBody" notflatlst))
                    (setq notflatlst (cons "AcDbBody" notflatlst))
                  )
                )
              )
            )
            ((eq "AcDbShape" objname)
              (if (>= version 16.1)
                (WMFOutIn x)
                (CommandExplode x)
              )
            )
            ;; Добавлено 9/1/2007.
            ((eq "AcDbSolid" objname)
              (FlatSolid x)
            )
            ;; Добавлено 9/1/2007.
            ((eq "AcDbTrace" objname)
              (FlatTrace x)
            )
            ((or
               (eq "AcDbRay" objname)
               (eq "AcDbXline" objname)
              )
              (FlatRayOrXline x)
            )
            ;; Не может быть взорван.
            ((eq "AcDbMInsertBlock" objname)
              (FlatMInsert x)
            )
            ((eq "AcDbMline" objname)
              (FlatMline x)
            )
            ((or
               (eq "AcDbWipeout" objname)   
               (eq "AcDbRasterImage" objname)
              )
              (FlatWipeoutOrRaster x)
            )
            ((eq "AcDbFcf" objname)
              (FlatTolerance x)
            )
            ;; Удалена поддержка AEC объектов 8/25/2007. 
            ((wcmatch (strcase objname) "AEC*")
              (setq cnt (1+ cnt))
              (if (not (vl-position "DbAecObject" notflatlst))
                (setq notflatlst (cons "DbAecObject" notflatlst))
              )
            )
            ;; Добавлено 9/9/2007. Взрывается в сплайн.
            ((eq "AcDbHelix" objname)
              (CommandExplode x)
            )
            ;; Добавлена опция proxy 8/25/2007.
            ((eq "AcDbZombieEntity" objname)
              (CommandExplode x)
            )
	    ;; proxy при спдс-энебл
            ((or (wcmatch (strcase objname) "MCSDBOBJECT*") (wcmatch (strcase objname) "MCSPSEUDO*"))
              (CommandExplode x)
            )
            ;; Изменено 2/4/2011. Оба типа объектов удалены.
            ((or
               (eq "AcDbLight" objname)
               (eq "AcDbCamera" objname)
               (eq "AcDbSun" objname)
             )
              (vla-delete x)
            )
            ;; Игнорировать вьюпорты.
            ((eq "AcDbViewport" objname))
            ;; Любой объект, не включенный выше.
            (T 
              (setq cnt (1+ cnt))
              (if (not (vl-position objname notflatlst))
                (setq notflatlst (cons objname notflatlst))
              )
            )
          ) ;cond
        ) ;progn
      ) ;if not erased
    ) ;foreach
	
	(gc) ; Очищаем память после обработки списка
  ) ;end ProcessList

  ;; Функція загрузкі  Экспресс Тулс
  ;; Загрузка Экпрес тулс изменилась с 2025
      (defun LoadExpressTools (/ acad_app acad_doc version)
  "Загружает Express Tools в AutoCAD 2025 и совместимых версиях"
  
  (setq acad_app (vlax-get-acad-object)
        acad_doc (vla-get-ActiveDocument acad_app)
        version (atof (getvar "acadver")))
  
  ;; 1. Проверяем наличие функции через vl-symbol-value (работает в 2025)
  (if (not (vl-symbol-value 'acet-ss-remove-dups))
    (progn
      (princ "\rПопытка загрузить Express Tools...")
      
      ;; 2. Пробуем стандартную команду
      (vl-catch-all-apply 
        '(lambda () (command "_.EXPRESSTOOLS"))
      )
	  (BackToText)
      
      ;; 3. Проверяем через atoms-family (работает во всех версиях)
      (if (not (vl-symbol-value 'acet-ss-remove-dups))
        (if (vl-position "ACETTEST" 
            (mapcar 'strcase (mapcar 'vl-filename-base (atoms-family 1))))
          (princ "\r Express Tools загружены через atoms-family")
        )
      )
      
      ;; 4. Если все еще не загружены, ищем файлы вручную
      (if (not (vl-symbol-value 'acet-ss-remove-dups))
        (progn
          (princ "\rПоиск файлов Express Tools вручную...")
          
          ;; Поиск в стандартных путях для 2025
          (setq et_paths
            (list
              (strcat (getenv "ProgramFiles") "\\Autodesk\\AutoCAD 2025\\Express")
              (strcat (getenv "ProgramFiles") "\\Autodesk\\AutoCAD 2025\\Support\\Express")
              (strcat (vl-filename-directory (findfile "acad.exe")) "\\Express")
            )
          )
          
          (foreach path et_paths
            (if (and path (vl-file-directory-p path))
              (progn
                (princ (strcat "\rНайдена папка: " path))
                
                ;; Загружаем основные файлы
                (foreach file '("acettest.fas" "acetutil.fas")
                  (if (findfile (strcat path "\\" file))
                    (vl-catch-all-apply 
                      '(lambda () (load (strcat path "\\" file)))
                    )
                  )
                )
                
                ;; Прерываем цикл после первого найденного пути
                (setq et_paths nil)
              )
            )
          )
        )
      )
    )
  )
  
  ;; 5. Финальная проверка
  (if (vl-symbol-value 'acet-ss-remove-dups)
    (progn
      (princ "\r Express Tools успешно загружены!")
      T
    )
    (progn
      (princ "\n Express Tools не загружены. Попробуйте:")
      (princ "\n Добавить папку Express в Trusted Paths")
      nil
    )
  )
);end load Express Tools

	;________________________________
  ;установка какие слои обрабатывать
  ;________________________________
(defun layersmode (/ s c k d)
  (setq s 7) ; 1+2+4 = заморожен+выключен+заблокирован
  (while (not (equal c "Продолжить"))
    (setq k (strcat "Продолжить"
             (if (= (logand s 2) 2) " Включить" " Отключить")
             (if (= (logand s 1) 1) " Разморозить" " Заморозить")
             (if (= (logand s 4) 4) " Разблокировать" " Заблокировать")))
    (setq d (vl-string-translate " " "/" k))
    (initget 0 k)
    (setq c (getkword (strcat "\nНе Обрабатывать: " ( itoa s) " (обработка"
                     (if (= (logand s 2) 2) "выкл " "вкл ")
                     (if (= (logand s 1) 1) "замор " "разм ")
                     (if (= (logand s 4) 4) "блок" "разбл") ")"
                     "\n[" d "]: ")))
    (cond ((= c "Включить") (setq s (- s 2)))
          ((= c "Отключить") (setq s (+ s 2)))
          ((= c "Разморозить") (setq s (- s 1)))
          ((= c "Заморозить") (setq s (+ s 1)))
          ((= c "Разблокировать") (setq s (- s 4)))
          ((= c "Заблокировать") (setq s (+ s 4))))
    (setq s (min 7 (max 0 s)))
    (if (not c) (setq c "Продолжить"))
  )
  s
)
  ;установка какие слои обрабатывать  
  (defun layersmodesimple (/ c)
  (initget "Продолжить Заблокировать Разблокировать")
  (setq c (getkword "\n[Продолжить/Заблокировать/Разблокировать] <П>: "))
  (if (= c "Разблокировать") 0 4)
)
 ;----------------------------------------------------------------------
;; Функция: GetLayerStatesByModelayer (битовая версия с логическим ИЛИ)
;; Назначение:
;;   Собирает список всех слоев, текущее состояние которых
;;   СООТВЕТСТВУЕТ ЛЮБОМУ из установленных битов в переменной `modelayer`.
;;   Состояние слоя вычисляется с помощью битовых операций:
;;     Бит 1 (значение 1): заморожен (frozen)
;;     Бит 2 (значение 2): выключен (off) 
;;     Бит 4 (значение 4): заблокирован (locked)
;;
;;   modelayer = 7 (111): выбирать ВСЕ проблемные слои (выключенные ИЛИ замороженные ИЛИ заблокированные)
;;   modelayer = 1 (001): выбирать только замороженные слои
;;   modelayer = 2 (010): выбирать только выключенные слои  
;;   modelayer = 4 (100): выбирать только заблокированные слои
;;   modelayer = 3 (011): выбирать выключенные ИЛИ замороженные слои
;;   modelayer = 5 (101): выбирать замороженные ИЛИ заблокированные слои
;;   modelayer = 6 (110): выбирать выключенные ИЛИ заблокированные слои
;;----------------------------------------------------------------------
(defun GetLayerStatesByModelayer (/ layerTbl layerName flag62 flag70 state result)
  (setq layerTbl (tblnext "LAYER" T))
  (setq result '())
  (while layerTbl
    (setq layerName (cdr (assoc 2 layerTbl)))
    (setq flag62 (cdr (assoc 62 layerTbl)))
    (setq flag70 (cdr (assoc 70 layerTbl)))
    
    ;; Вычисляем состояние с помощью битовых операций
    (setq state 0)
    ;; Бит 1 (значение 2): выключен, если flag62 < 0
    (if (< flag62 0) (setq state (logior state 2)))
    ;; Биты из flag70: 
    ;;   Бит 0 (значение 1): заморожен (flag70 = 1 или 5)
    ;;   Бит 2 (значение 4): заблокирован (flag70 = 4 или 5)
    (if (or (= flag70 1) (= flag70 5)) (setq state (logior state 1)))
    (if (or (= flag70 4) (= flag70 5)) (setq state (logior state 4)))
    
    ;; Сравниваем с modelayer с использованием логического ИЛИ
    (if (/= (logand state modelayer) 0)
      (setq result (cons (cons layerName layerTbl) result))
    )
    
    (setq layerTbl (tblnext "LAYER"))
  )
  result
)


;;----------------------------------------------------------------------
;; Функция: ApplyLayerChangesByModelayer
;; Назначение:
;;   Применяет изменения к слоям из списка layerList,
;;   которые соответствуют значению переменной modelayer.
;;----------------------------------------------------------------------
(defun ApplyLayerChangesByModelayer (/ layerName layers layer  
                         )
  
 (setq layers (vla-get-Layers doc))

    ;; НАЧИНАЕМ ТРАНЗАКЦИЮ ТОЛЬКО ДЛЯ ИЗМЕНЕНИЙ СЛОЕВ
  
  (vlax-for layer layers
    (setq layerName (vla-get-Name layer))

   ;общая проверка слоя
        (if (tblsearch "LAYER" layerName)
(progn
    ;; Размораживаем все замороженные слои
    (if (and (= (vla-get-Freeze layer) :vlax-true)
	    (or (= modelayer 7) (= modelayer 1) (= modelayer 2) (= modelayer 5))
	     )
      (vla-put-Freeze layer :vlax-false)
      )
;; Разблокируем все заблокированные слои
        (if (and 
	     (= (vla-get-Lock layer) :vlax-true)
	    (or (= modelayer 7) (= modelayer 4) (= modelayer 5) (= modelayer 6))
	     )
      (vla-put-Lock layer :vlax-false)
      )
  ;; Включаем все выключенные слои
            (if (and 
	     (= (vla-get-LayerOn layer) :vlax-false)
	    (or (= modelayer 7) (= modelayer 2) (= modelayer 3) (= modelayer 6))
	     )
      (vla-put-LayerOn layer :vlax-true)
      )
	  ));end if
  );end vla-for
  ;(vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports)
);;;;ApplyLayerChangesByModelayer


  ;________________________________
  ;настройка программы Flatten
  ;________________________________
  (Defun SetupFlatten ()
          ;;; ----- НАЧАЛО ПАРАМЕТРОВ ПРОГРАММЫ ----- ;;;
      ;; Добавлены параметры программы 7/10/2007.
      ;; Спасибо Стиву Доману за его помощь с интерфейсом.
      ;; Изменено для proxy-объектов 7/25/2007.
      (or *expans* (setq *expans* "Нет")); делает блоки взрываемые
      (or *overkillans* (setq *overkillans* "Да"));очищать
      (or *proxyans* (setq *proxyans* "Да"));апрацоуваць прокси
      (setq optans T)
      (cond
        ;; до 2006 и ET не загружен
	((< version 16.2)
	 (alert "Данная версия Автокад не поддерживается (<2006), воспользуйтесь предыдущей версией SuperFlaten")
	)
 ;; 2006 или новее (объединенная версия)
((>= version 16.2)
  (while optans            
    (if presufstr 
      (princ (strcat "\nТекущие параметры: Переименовать=" renameans "> " presufstr))
      (princ (strcat "\nТекущие параметры: Переименовать=Не указано"))
    )            
    
    ;; Объединенный вывод параметров - Overkill отображается только если есть Express Tools
    (setq param-str (strcat "  Взрываемый=" *expans*
                           (if acet-ss-remove-dups (strcat "  Overkill=" *overkillans*) "")
                           "  Proxies=" *proxyans*))
    (princ param-str)
    
    (if (not proxyreport)
      (progn
        (if proxylst 
          (princ (strcat "\n  " (itoa (length proxylst)) " proxies выбраны. "))
          (princ "Нет выбранных proxies. ")
        )
        (setq proxyreport T)
      )
    )
    
    ;; Добавляем "Продолжить" как первый пункт в меню
    ;; Объединенное меню - Overkill добавляется только если есть Express Tools
    (if acet-ss-remove-dups
      (initget "Продолжить Переименовать Взрываемый Overkill Proxies Слои")
      (initget "Продолжить Переименовать Взрываемый Proxies Слои")
    )
    
    (setq optans
      (getkword 
        (if acet-ss-remove-dups
          "\nПараметры SuperFlatten [Продолжить/Переименовать блоки/Взрываемые блоки/Overkill/Proxies/Слои] <Продолжить>: "
          "\nПараметры SuperFlatten [Продолжить/Переименовать блоки/Взрываемые блоки/Proxies/Слои] <Продолжить>: "
        )
      )
    )
    
    (cond 
      ;; Новый пункт - Продолжить выполнение с текущими настройками
      ((or (eq optans "Продолжить") (not optans))
        (setq optans nil)  ; Выход из цикла while
      )
      ((eq optans "Переименовать")
        (initget "Префикс Суффикс")
        (setq renameans 
          (getkword "\nПараметры имен блоков: [Префикс/Суффикс] <С>: ")
        )
        (if (not renameans) (setq renameans "Суффикс"))
        (cond
          ((eq renameans "Prefix")
            (setq presufstr (PrefixSuffix "prefix"))
          )
          ((eq renameans "Suffix")
            (setq presufstr (PrefixSuffix "suffix"))
          )
        )
      )
      ((eq optans "Взрываемый")
        (initget "Да Нет")
        (setq *expans* 
          (getkword "\nВременно сделать все блоки взрываемыми? [Да/Нет] <Н>: ")
        )
        (if (not *expans*) (setq *expans* "Нет"))
      )
      ;; Overkill обрабатывается только если есть Express Tools
      ((and acet-ss-remove-dups (eq optans "Overkill"))
        (initget "Да Нет")
        (setq *overkillans* 
          (getkword "\nЗапустить Overkill после выравнивания? [Да/Нет] <Н>: ")
        )
        (if (not *overkillans*) (setq *overkillans* "Нет"))
      )
      ((eq optans "Proxies")
        (initget "Да Нет")
        (setq *proxyans* 
          (getkword "\nОбработать proxy-объекты? [Да/Нет] <Н>: ")
        )
        (if (not *proxyans*) (setq *proxyans* "Нет"))
      )
      ((eq optans "Слои")
        (if proverkanabor
        (setq modelayer (layersmode));какие слои обрабатывать
	(setq modelayer (layersmodesimple));какие слои обрабатывать
	 ) 
      )
    )
  )
)       
        
      ) ; end options cond

   );end SetupFlatten
;;; ----- КОНЕЦ ПАРАМЕТРОВ ПРОГРАММЫ SetupFlatten----- ;;;
  

;; Автоматическая загрузка при запуске SuperFlatten
(if (not (vl-symbol-value 'acet-ss-remove-dups))
  (LoadExpressTools)			;загрузка ЕТ
)


;;;;;;; КОНЕЦ ВЛОЖЕННЫХ ФУНКЦИЙ SuperFlatten ;;;;;;;
  
  ;;;;;;; НАЧАЛО ОСНОВНОЙ ПРОГРАММЫ ;;;;;;;
  ;;;;;;; НАЧАЛО ПЕРЕД ВЫБОРОМ ;;;;;;;
   ;;;;;;; НАЧАЛО ОСНОВНОЙ ПРОГРАММЫ ;;;;;;;
  ;;;;;;; НАЧАЛО ПЕРЕД ВЫБОРОМ ;;;;;;;
    ;;;;;;; НАЧАЛО ОСНОВНОЙ ПРОГРАММЫ ;;;;;;;
  ;;;;;;; НАЧАЛО ПЕРЕД ВЫБОРОМ ;;;;;;;
    ;;;;;;; НАЧАЛО ОСНОВНОЙ ПРОГРАММЫ ;;;;;;;
  ;;;;;;; НАЧАЛО ПЕРЕД ВЫБОРОМ ;;;;;;;
    ;;;;;;; НАЧАЛО ОСНОВНОЙ ПРОГРАММЫ ;;;;;;;
  ;;;;;;; НАЧАЛО ПЕРЕД ВЫБОРОМ ;;;;;;;
(setq version (atof (getvar "AcadVer")))

;получение vla
  (setq 
     acadApp (vlax-get-acad-object)
     doc (vla-get-ActiveDocument acadApp)
     views (vla-get-Views doc)
     modelayer 7
  )
  

  
(if (ssget "_I") ; Проверяем уже выделенные объекты
    (setq proverkanabor T)
    (setq proverkanabor nil)
    ;(setq ss (ssget "_X")) ; Если ничего не выделено - выбираем все объекты
)

(princ (strcat 
  (if proverkanabor 
    "Вы выбрали объекты - программа проработает данные объекты. \n\n" 
    "Вы невыбрали объекты - программа выберет объекты отличные от 0 \"Все\".\n\n"
  )
  "Настройки по умолчанию:\n"
  "  Переименование блоков - откл.\n"
  "  Проход по Прокси-объектам (в том числе СПДС-CS) - вкл.\n"
  (if proverkanabor 
    "  Проход осуществляется и по заблокированным слоям.\n" 
    "  Проход осуществляется и по заблокированным-замороженным-отключенным слоям.\n"
  )
  "(настройки по умолчанию можете изменить в окне после этого сообщения...)\n"
  "___________________________________________________________\n"
  "п.с. Если программа не отработала до конца, она не восстановила слои, тогда вызовите принудительно команду \"RestoreLayersLock\".\n"
  "п.с.2 команда viewswitch - просмотр сбоку и возврат"
)) (princ)
  
;вызов настроек программы
(SetupFlatten)

 (macro "Для ускоренение работы программы окно свернуто, дождитесь завершение программы")
 (alert "Окно чертежа будет свернуто (для ускорения),\nсообщения могут не отображаться, дождитесь окончание работы....")
 (savevar)
;(textscr)
;(vla-put-WindowState acadApp 2) ; 2 = Свернуть окно (acMin)


(if start-timer
(start-timer "SupperFlatten"))
  (if (not jpro) (setq jpro 0))
  
;получить слои
(setq layerListRestore (GetLayerStatesByModelayer))
;Приненеие параметров раздблокировки включения и т.д
(ApplyLayerChangesByModelayer)

(setq ss (ssget "_I"))  
(if proverkanabor ; Проверяем уже выделенные объекты
    (setq ss (ssget (list (cons 410 (getvar "ctab")))))
    (setq ss (ssget "_X")) ; Если ничего не выделено - выбираем все объекты
)
  
;; ФИЛЬТРАЦИЯ: Оставляем только объекты, имеющие Z != 0
  (if (and ss (> (sslength ss) 0))
    (progn
      (princ "\nПредварительная фильтрация плоских объектов... ")
      (setq filtered_ss (ssadd)
            i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq vla_obj (vlax-ename->vla-object ent))
        
        ;; Если объект нуждается в сплющивании — добавляем в новый набор
        (if (NeedsFlattening vla_obj)
          (ssadd ent filtered_ss)
        )
        (setq i (1+ i))
      )
      
      (setq ss filtered_ss) ; Заменяем исходный набор на отфильтрованный

 ;; Если после фильтрации ничего не осталось — выходим
      (if (= (sslength ss) 0)
        (progn
         (graphscr) ; Скрываем текстовое окно (чтобы вы случайно не нажали Enter)
          (alert "\nВсе выбранные объекты уже лежат на Z=0. Обработка не требуется.")
          (exit)     ; Сразу отправляет скрипт в *error*, где выполнится restorevar
        )
      )
      (princ (strcat "Отобрано для обработки: " (itoa (sslength ss))))
    )
  )
  ;; --- КОНЕЦ БЛОКА ВЫБОРА ---
  
  ;; Удалить SFview, если он существует.
  (if (setq i (ValidItem views "SFview"))
    (vl-catch-all-apply 'vla-delete (list i))
  )

  ;; Убедиться, что модельное пространство активно.
  (if (= 0 (vlax-get doc 'ActiveSpace))
    (progn
      (princ "\r SupperFlatten предназначен для работы в модельном пространстве. Переключение в модель. \n")
      (vlax-put doc 'ActiveSpace 1)
    )
  )
  ;;;;;;; КОНЕЦ ПЕРЕД ВЫБОРОМ ;;;;;;;

  
  ;;;;;;; ПОЛУЧИТЬ НАБОР ВЫБОРА ;;;;;;
  ;====================================================;

      ;; 2004 или новее.
      (if (>= version 16)
        (setq hpa (getvar "hpassoc"))
      )
      ;; 2006 или новее. Добавлено 9/14/2007.
      (if (>= version 16.2)
        (progn
          (setq slu (getvar "showlayerusage"))
          (setvar "showlayerusage" 0)
        )
      )
      (setq  
         blocks (vla-get-Blocks doc)
         layouts (vla-get-Layouts doc)
         sscol (vla-get-SelectionSets doc)
         mspace (vla-get-ModelSpace doc)
         mspacecnt (vlax-get mspace 'Count)
         elevms (vlax-get doc 'ElevationModelSpace)
         elevps (vlax-get doc 'ElevationPaperSpace)
	 ;locklst (UnlockLayers doc)
         expm (getvar "explmode")
         pksty (getvar "pickstyle")
         cnt 0
         expblkcnt 0
      )
      (vlax-put doc 'ElevationModelSpace 0.0)
      (vlax-put doc 'ElevationPaperSpace 0.0)

      ;; UCSflag используется в конце процедуры для восстановления предыдущей UCS
      ;;/// если она изначально не была мировой.
      (if (= 0 (getvar "worlducs"))
        (setq UCSflag T)
      )
      ;; Выравнивание должно выполняться в WCS из-за того, как некоторые
      ;;/// объекты ведут себя при взаимодействии с кодом.
      (command-new (list "._ucs" "_world"))
      ;; Преобразовать набор выбора в список vla-объектов.
      (setq lst (SSVLAList ss))
      ;; Перемещено сюда 8/25/2007.
      ;; Сделать список имен блоков, включая вложенные имена.
      ;; Также проверить на proxy-объекты.
      (setq testlst lst)
      (while testlst
        (princ 
          (strcat "\rАнализ выборки... "
		  (itoa (- (length testlst) 1)) " "
            (setq *sbar (Spinbar *sbar)))
        )        
        (setq obj (car testlst)
              objname (vlax-get obj 'ObjectName)
        )
        (if 
          (or 
            (eq "AcDb3dSolid" objname) 
            (eq "AcDbBody" objname)
            (eq "AcDbSurface" objname)
            (eq "AcDbPlaneSurface" objname)
          )
          (setq WMFflag T)
        )
        (if 
          (and 
            (eq "AcDbBlockReference" objname)
            (not (vlax-property-available-p obj 'Path))
          )
          (progn
            (setq name (vlax-get obj 'Name))
            (if (not (vl-position name blknamelst))
              (setq blknamelst (cons name blknamelst))
            )
            (foreach i (GetNestedNames blocks name)
              (if (not (vl-position i blknamelst))
                (setq blknamelst (cons i blknamelst))
              )
            )
          )
        )
        (if (eq "AcDbZombieEntity" objname)
          (setq proxylst (cons obj proxylst))
        )        
        (setq testlst (cdr testlst))        
      ) ;while
      ;; Добавлено 9/22/2007 для работы с proxy-объектами в блоках.
      (foreach x blknamelst
        (if (CheckBlock x "AcDbZombieEntity") 
          (DeleteBlockProxies x)
        )
        ;; Добавлено 2/9/2011. Проверить на эти типы объектов в выбранных блоках.      
        (if (not WMFflag)
          (if 
            (or
              (CheckBlock x "AcDb3dSolid")
              (CheckBlock x "AcDbBody")
              (CheckBlock x "AcDbSurface")
              (CheckBlock x "AcDbPlaneSurface")
            )
            (setq WMFflag T)
          )
        )
      )
      ;; Добавлено здесь 2/19/2011. SFview нужен только при выравнивании 3D сплошных 
      ;;/// и аналогичных типов объектов. Если SFview существовал ранее, он был удален выше.
      (if WMFflag
        (progn
          (command-new (list "._view" "_save" "SFview"))
          ;; Визуальные стили были добавлены в 2007. Две причины для следующих
;;;          /// вызовов команд. Во-первых, установить стиль вида на 2d wireframe.
          ;; Во-вторых, в некоторых случаях бета-тестирование показало, что если вид установлен на
;;;          /// 2d wireframe, а затем вид сохранен, сохраненный вид не
;;;          /// 2d wireframe, как ожидалось. Кажется, ошибка. В любом случае, это исправляет
;;;          /// эту проблему и устраняет необходимость для пользователя устанавливать вид
;;;          /// на 2d wireframe перед запуском процедуры. 
          ;; Пример файла: 2008 Sample folder Visualization - Conference Room.dwg.
          (if (>= version 17)
            (progn
              (command "._view" "_settings" "_visual" "SFview" "2d wireframe"
                       "_background" "SFview" "_none" "" "")
              (command-new (list "._view" "_restore" "SFview"))
			  (BackToText)
            )
          )
        )
      )     
      (cond
        ((not WMFflag))
        ((and
            WMFflag
           (not (>= version 16.1))
         )
         (princ "\r Выравнивание 3D сплошных с использованием WMF out/in требует 2005 или новее. ")
        )
        ((and
           WMFflag
           (>= version 16.1)
         )
         (princ "\r  Выравнивание 3D сплошных с использованием WMF out/in. ")
        )
      )


      
      ;(starttimer)
      ;; Добавлено 6/25/2007 для работы с опцией 2006 Explodable blocks.
      ;; Возможно, было бы умнее проверить
      ;; выбранные блоки и затем решить, нужно ли что-то делать.
      ;; Но это кажется чрезмерным, так как в большинстве случаев
     ;;/// пользователь выберет все. Следующее кажется достаточным.
      (if (eq "Да" *expans*)
        (vlax-for x blocks 
          (if 
            (and
              (vlax-property-available-p x 'Explodable)
              (eq acFalse (vlax-get x 'Explodable))
            )
            (progn
              (setq expblklst (cons x expblklst))
              (vlax-put x 'Explodable acTrue)
            )
          )
        )
      )
      ;; Удалить proxy-объекты из списка выбора, если proxyans равно Нет.
      (if (eq "Нет" *proxyans*)
        (foreach x proxylst
          (setq lst (vl-remove x lst))
        )
      )
;; Обработать выбранные объекты до выравнивания определений БЛОКОВ.
       ;; Перемещено сюда 7/19/2007. Должно быть здесь, а не
       ;; после выравнивания определений, чтобы условие ссылок на блоки
       ;;/// в PROCESSLIST делало то, что должно. Взорвать блоки, которые только
       ;;/// содержат другие блоки.
      (ProcessList lst)
      ;; Добавлено 8/23/2007.
      ;; Проверить, являются ли имена в blknamelst все еще действительными элементами
      ;;/// в коллекции блоков.
      (foreach x blknamelst
        (if (ValidItem blocks x)
          (setq validlst (cons x validlst))
        )
      )
      (setq blknamelst (reverse validlst))
;; Проверить на пустые блоки.
       ;; Вызывает ошибку с объектами копирования.
       ;; Найден пустой блок под названием "круглая лестница" в файле Кен Люк
       ;;/// клиентские файлы пример файла.
      (if blknamelst
        (progn
          ;; Удалить временный макет, если он уже существует.
          (vl-catch-all-apply '(lambda () (vla-delete (vla-item layouts "SuperFlatten layout"))))
          (setq actlayout (vlax-get doc 'ActiveLayout)
                templayout (vlax-invoke layouts 'Add  "SuperFlatten layout")
                layoutblk (vlax-get templayout 'Block)
          )
          (vlax-put doc 'ActiveLayout templayout)
          (foreach x blknamelst
            (setq blkdef (vla-item blocks x)
                  inoutlst nil
                  renameflag nil
            )
            ;; добавлено 7/16/2007 - из большого примера файла Стива:
            ;; определение блока
            ;; Name = "3D-BASE-STREET-TREES"
            ;; Origin = (66965.5 13010.2 -354.0)
            (setq orig (vlax-get blkdef 'Origin))
            (CheckRename orig (ZZeroPoint orig))
            (vlax-put blkdef 'Origin (ZZeroPoint (vlax-get blkdef 'Origin)))
            ;; Список объектов в исходном блоке и отфильтровать вьюпорты.
            (vlax-for i blkdef
              (or
                (eq "AcDbViewport" (vlax-get i 'ObjectName))
                (setq inoutlst (cons i inoutlst))
              )
            )
            (if inoutlst
              (progn
                ;; Копировать список в блок макета.
                (setq inoutlst (vlax-invoke doc 'CopyObjects inoutlst layoutblk))
                ;; Очистить исходный блок, кроме вьюпортов.
                (vlax-for i blkdef
                  (or
                    (eq "AcDbViewport" (vlax-get i 'ObjectName))
                    (vl-catch-all-apply 'vla-delete (list i))
                  )
                )
                ;; Выровнять объекты в макете.
                (ProcessList inoutlst)                
                (setq inoutlst nil)
                ;; Список выровненных объектов, отфильтровать вьюпорты.
                (vlax-for i layoutblk
                  (or
                    (eq "AcDbViewport" (vlax-get i 'ObjectName))
                    (setq inoutlst (cons i inoutlst))
                  )
                )
                ;; Копировать выровненные объекты в макете обратно в
                ;;/// определение блока и удалить объекты в макете.
                (if inoutlst
                  (progn
                    (vlax-invoke doc 'CopyObjects inoutlst blkdef)
                    ;; БЕЗОПАСНОЕ УДАЛЕНИЕ: перехватываем ошибки заблокированных слоев
                    (foreach obj inoutlst 
                      (vl-catch-all-apply 'vla-delete (list obj))
                    )
                  )
                )
              )
            )
            (if 
              (and 
                ;; Нельзя переименовать анонимные блоки.
                (not (vl-string-search "*" x))
                renameflag 
                renameans
                presufstr
              )
              (cond
                ((and
                   (eq renameans "Prefix")
                   (setq newname (strcat presufstr x))
                  )
                  ;; Добавлена проверка существующего имени блока 8/9/2007.
                  (if (ValidItem blocks newname)
                    (setq notrenamedlst (cons x notrenamedlst))
                    (progn
                      (vlax-put blkdef 'Name newname)
                      (setq newnamelst (cons newname newnamelst))
                    )
                  )
                )
                ((and
                   (eq renameans "Suffix")
                   (setq newname (strcat x presufstr))
                  )
                  ;; Добавлена проверка существующего имени блока 8/9/2007.
                  (if (ValidItem blocks newname)
                    (setq notrenamedlst (cons x notrenamedlst))
                    (progn
                      (vlax-put blkdef 'Name newname)
                      (setq newnamelst (cons newname newnamelst))
                    )
                  )
                )
              )
            )
          ) ;foreach
          (vlax-put doc 'ActiveLayout actlayout)
          ;templayout удаляется в обработчике ошибок.
        ) ;progn
      ) ;if blknamelst
;;;      (if blknamelst
;;;        (vla-regen doc acActiveViewport)
;;;      )
      (if 
        (and 
          (eq "Да" *overkillans*)
          (setq ss 
            (cadr 
              (acet-ss-remove-dups 
                (ssget "_x" '((410 . "Model"))) 1e-6 nil)
            )
          )
        )
		(progn
        (command (list "._erase" ss ""))
		(BackToText))
      )
      (if (or newnamelst notrenamedlst)
        (textscr)
      )
      (if newnamelst
        (progn
          (princ "\nСледующие блоки были переименованы: ")
          (foreach x newnamelst
            (print x)
          )
        )
      )
      (if notrenamedlst
        (progn
          (princ "\nСледующие блоки не были переименованы из-за конфликта с существующим именем блока: ")
          (foreach x notrenamedlst
            (print x)
          )
        )
      )
      (if (> expblkcnt 0)
        (princ (strcat "\nКоличество взорванных блоков: " (itoa expblkcnt)))
      )
      (princ 
        (strcat "\nКоличество объектов в модельном пространстве до: " 
          (itoa mspacecnt) " после: " (itoa (vlax-get mspace 'Count)) " \n"
        )
      )
      (if (> cnt 0)
        (progn
          (princ 
            (strcat "\nКоличество объектов не обработанных или не выровненных: " (itoa cnt) " \n")
          )
          (if notflatlst
            (progn
              (princ "\nТипы бъектов не выровнены: ")
              (foreach x notflatlst
                (setq pos (+ 3 (vl-string-search "Db" x)))
                (princ (strcat (substr x pos) " "))
              )
              (print)
            )
          )
        )
      )
      (if proxyerror (princ "\nВозникла проблема с proxies внутри блоков. "))
      (if UCSflag (command-new (list "._ucs" "_previous")))
      (if (setq i (ValidItem views "SFview"))
        (vl-catch-all-apply 'vla-delete (list i))
      )
   
      ;; 1. Восстанавливаем исходные состояния слоев (пока окно свернуто)
      (RestoreLayersDefaultLock layerListRestore)

      ;; 2. Останавливаем и печатаем таймеры
      (if start-timer
        (progn
          (end-timer "SupperFlatten")    
          (start-timer "возрат настроек")
        )
      )
      
      ;; 3. ФИНАЛЬНАЯ ОЧИСТКА: Разворачиваем окно, включаем UNDO, возвращаем настройки
      ;; Вызываем явно, так как штатная работа закончена.
      (*error* nil)
  
      (if start-timer
        (progn
      (end-timer "возрат настроек")
      (print-timers)
      ))

      ;; 4. ПЕРЕКЛЮЧЕНИЕ ВИДА (В "Песочнице")
      ;; Окно уже развернуто, пользователь видит чертеж.
      ;; Если он нажмет Esc, скрипт просто тихо завершится, не вызывая *error* повторно.
      (vl-catch-all-apply 'viewswitch nil)
  

      (princ) ; Тихий выход из SuperFlatten

) ;; end SuperFlatten
;;;;;;; КОНЕЦ ОСНОВНОЙ ПРОГРАММЫ ;;;;;;;;
;shortcut
(defun c:SF () (c:SuperFlatten))