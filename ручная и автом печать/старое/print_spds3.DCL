prindcl: dialog{label="Автоматическая печать";
:text{label="Настройки:";key="txt1";}
:row{
:toggle{label="Закрыть PDF**";key="pdf";value="0";}
}
:row{
:toggle{label="текущий чертеж";key="rys";value="1";}
}
:text{label="** - ! закрывает все pdf-файлы";key="txt5";}
:text{label="";}
:toggle{label="склеить по файлам**";key="sfile";value="0";}
:toggle{label="склеить в общий файл**";key="sfile_all";value="0";}
ok_cancel;
}