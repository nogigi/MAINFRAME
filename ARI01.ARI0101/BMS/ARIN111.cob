ARIN111  DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,TIOAPFX=YES
* MAP MENU
ARIM111  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1,CTRL=(FREEKB,FRSET)
         DFHMDF POS=(01,01),LENGTH=39,ATTRB=(ASKIP,BRT),               *
               INITIAL='FORMATION CICS -        ESTIAC INSTITUT'
         DFHMDF POS=(01,62),LENGTH=06,ATTRB=(ASKIP),                   *
               INITIAL='DATE :'
MDATE    DFHMDF POS=(01,69),LENGTH=10,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(02,21),LENGTH=32,ATTRB=(ASKIP),                   *
               INITIAL='===================================='
         DFHMDF POS=(02,58),LENGTH=10,ATTRB=(ASKIP),                   *
               INITIAL='TERMINAL :'
MTERM    DFHMDF POS=(02,69),LENGTH=04,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(03,61),LENGTH=07,ATTRB=(ASKIP),                   *
               INITIAL='TACHE :'
MTASK    DFHMDF POS=(03,69),LENGTH=07,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(04,59),LENGTH=09,ATTRB=(ASKIP),                   *
               INITIAL='TRANSID :'
MTRAN    DFHMDF POS=(04,69),LENGTH=04,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(05,21),LENGTH=36,ATTRB=(ASKIP,BRT),               *
               INITIAL='APPLICATION GESTART - MENU PRINCIPAL'
         DFHMDF POS=(06,21),LENGTH=36,ATTRB=(ASKIP),                   *
               INITIAL='------------------------------------'
         DFHMDF POS=(10,20),LENGTH=41,ATTRB=(ASKIP),                   *
               INITIAL='1 ==> CONSULTATION DIRECT (CODE ARTICLE)'
         DFHMDF POS=(11,20),LENGTH=46,ATTRB=(ASKIP),                   *
               INITIAL='2 ==> CONSULTATION SEQUENTIELLE (CODE ARTICLE)'
         DFHMDF POS=(12,20),LENGTH=48,ATTRB=(ASKIP),                   *
               INITIAL='3 ==> CONSULTATION SEQUENTIELLE (CODE CATEGORIE*
               )'
         DFHMDF POS=(13,20),LENGTH=27,ATTRB=(ASKIP),                   *
               INITIAL='4 ==> CREATION D''ARTICLE'
         DFHMDF POS=(14,20),LENGTH=31,ATTRB=(ASKIP),                   *
               INITIAL='5 ==> MODIFICATION D''UN ARTICLE'
         DFHMDF POS=(15,20),LENGTH=30,ATTRB=(ASKIP),                   *
               INITIAL='6 ==> SUPPRESSION D''UN ARTICLE'
         DFHMDF POS=(17,29),LENGTH=18,ATTRB=(ASKIP),                   *
               INITIAL='SAISIE DU CHOIX =>'
MCHOIX   DFHMDF POS=(17,48),LENGTH=01,ATTRB=(BRT,IC)
         DFHMDF POS=(17,50),LENGTH=02,ATTRB=(ASKIP),                   *
               INITIAL='<='
         DFHMDF POS=(21,01),LENGTH=09,ATTRB=(ASKIP),                   *
               INITIAL='MESSAGE :'
MMSG     DFHMDF POS=(22,01),LENGTH=79,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(24,12),LENGTH=55,ATTRB=(ASKIP,BRT),               *
               INITIAL='ENTREE => ENVOI SELECTION     PF3 => FIN DE TRA*
               NSACTION'
         DFHMSD TYPE=FINAL
         END
