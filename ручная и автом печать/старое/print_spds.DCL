prindcl: dialog{label="Аўтаматычны друк";
:text{label="Апрацоўваць:";key="txt1";}
:row{
:toggle{label="spdsFormat";key="spds";value="1";}
:toggle{label="рамкі-блокі*";key="kos";value="0";}}
:row{
:toggle{label="полірыскі             ";key="polir";value="0";}
:toggle{label="Зачыніць Adobe**";key="pdf";value="0";}
}
:row{
:toggle{label="№ старонак      ";key="numar";value="1";}
:toggle{label="бягучы рысунак";key="rys";value="1";}
}
:text{label="* - дынаміч. блокі на аснове прастакутнікаў";key="txt4";}
:text{label="** - ! зачыняе усе pdf-файлы";key="txt5";}
:toggle{label="склеіць па файлах**";key="sfile";value="0";}
:toggle{label="склеіць у агульны файл**";key="sfile_all";value="0";}
ok_cancel;
}