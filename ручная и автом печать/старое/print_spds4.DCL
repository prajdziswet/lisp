prindcl: dialog{label="Автоматическая печать";
:text{label="Настройки:";key="txt1";}
:row{
:toggle{label="Закрыть все PDF**";key="pdf";value="1";}
}
:row{
:toggle{label="текущий чертеж ";key="rys";value="1";}
}
:text{label="(снять галочку => папку чертежа)";key="txt5";}
:row{
:toggle{label="сторонние блоки";key="stor";value="0";}
}

:text{label="";}
:toggle{label="склеить по файлам**";key="sfile";value="0";}
:toggle{label="склеить в общий файл**";key="sfile_all";value="0";}
ok_cancel;
}