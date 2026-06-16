printnew: dialog{label="Ручная печать";
:text{label="Для продолжения нажмите ОК,для завершения - Отмена";key="txt1";}
:toggle{label="Цветная печать";key="colordwg";value="0";}
:edit_box {label = "Маштаб (0 => 1:1 или 1:100):"; key = "mash"; edit_width=10; value = "0";}
: button { key = "btn1";label = "Переключить лист"; }
ok_cancel;
}