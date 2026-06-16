prindcl: dialog{label="Автоматическая печать";
:text{label="Обрабатывать:";key="txt1";}
:row{
:toggle{label="spds-рамки        ";key="spds";value="1";}
:toggle{label="рамки-Володи*";key="kos";value="0";}}
:row{
:toggle{label="прямоугольники";key="polir";value="0";}
:toggle{label="Закрыть PDF**";key="pdf";value="0";}
}
:row{
:toggle{label="№ страниц            ";key="numar";value="1";}
:toggle{label="текущий чертеж";key="rys";value="1";}
}
:text{label="* - динамич. блоки (необязательно)";key="txt4";}
:text{label="** - ! закрывает все pdf-файлы";key="txt5";}
:toggle{label="склеить по файлам**";key="sfile";value="0";}
:toggle{label="склеить в общий файл**";key="sfile_all";value="0";}
ok_cancel;
}