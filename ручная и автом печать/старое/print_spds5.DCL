prindcl: dialog{label="Автоматическая печать";
:text{label="Настройки:";key="txt1";}
:row{
:toggle{label="Закрыть все PDF**";key="pdf";value="1";}
}
: radio_column {
: radio_button {key = "Radio1"; label = "текущий чертеж"; value = 1;}
: radio_button {key = "Radio2"; label = "текущую папку"; }
}
:row{
:toggle{label="сторонние блоки";key="stor";value="0";}
}

:text{label="";}
:toggle{label="склеить по файлам**";key="sfile";value="0";}
:toggle{label="склеить в общий файл**";key="sfile_all";value="0";}
ok_cancel;
}