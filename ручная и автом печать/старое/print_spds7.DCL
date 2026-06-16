prindcl: dialog{label="Автоматическая печать";
: boxed_column { label = "Настройка:";
: radio_column {
: radio_button {key = "Radio1"; label = "текущий чертеж"; }
: radio_button {key = "Radio2"; label = "текущую папку чертежа"; value = 1;}
}
:text{label="";}
:toggle{label="сторонние блоки";key="stor";value="0";}
:text{label="      (обычно не требуется)";}
}
:popup_list{label="";
list="не склеивать\nсклеить по файлам\nсклеить в общий файл";value="2";key="sfile_merge";}
ok_cancel;
}