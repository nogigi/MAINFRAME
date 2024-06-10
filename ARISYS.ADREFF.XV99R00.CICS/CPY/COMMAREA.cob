      *----------------------------------------------------------------*
      * DESCRIPTION DE LA ZONE DE COMMUNICATION COBOL-CICS             *
      *----------------------------------------------------------------*
       01  WS-COMMAREA.
           05 WS-AIG                   PIC 9.
              88 LOOP-MENU                VALUE 0.
              88 LOOP-SPG                 VALUE 1 THRU 6.
           05 WS-TAFF                  PIC X.
              88 INIT-TRT                 VALUE LOW-VALUE.
              88 AFF-MAP                  VALUE 'M'.
              88 AFF-AIDE                 VALUE 'A'.
              88 CONFIRMATION             VALUE 'C'.
              88 AFF-FOU                  VALUE 'F'.
              88 LOCK-MOD                 VALUE 'L'.
              88 AFF-CAT                  VALUE 'C'.
      *
      *
      *----------------------------------------------------------------*
      * ZONE MODIFIABLE SELON LES BESOINS (TAILLE MAX. = 4094 OCTETS)  *
      *----------------------------------------------------------------*
           05 WS-TASK                  PIC X(5).
           05 WS-TEST                  PIC 9(4).
           05 FILLER                   PIC X.
              88 SAVE-MAP                 VALUE 'S'.
              88 RESET-MAP                VALUE LOW-VALUE.
           05 WS-CHOIX                 PIC X.
           05 WS-MDATE                 PIC X(10).
           05 WS-ENR-SAV.
              10 WS-CODE               PIC X(5).
                 88 CODE-VIDE             VALUE SPACES.
                 88 CODE-INEX             VALUE LOW-VALUE.
              10 WS-LIB                PIC X(20).
              10 WS-CATEG              PIC X(5).
              10 WS-FOUR               PIC 9(5).
              10 WS-APPRO              PIC 99.
              10 WS-QTE                PIC 9(6).
              10 WS-ALERT              PIC 9(5).
              10 WS-NB-LOT             PIC 9.
              10 FILLER.
                 15 WS-TLOT            OCCURS 5.
                    20 WS-TLOT-NUM     PIC X(6).
                    20 WS-TLOT-QTE     PIC 9(5).
                    20 WS-TLOT-PXU     PIC 9(5)V99.
              10 FILLER                PIC X.
           05 FILLER.
               10 WS-NUMLOT            PIC X(6) OCCURS 5.
           05 WS-CHA-QTE               PIC 9(5).
           05 WS-CPT-PAGE              PIC S9(4)  COMP.
           05 WS-NB-PAGE               PIC S9(4)  COMP.
              88 NO-Q                     VALUE 0.
              88 NO-SCROLL                VALUE 1.
           05 WS-PF7                   PIC X.
              88 PF7-DEBFIN               VALUE 'A'.
              88 PF7-DEB                  VALUE 'Z'.
              88 PF7-INACTIVE             VALUE LOW-VALUE.
              88 PF7-ACTIVE               VALUE HIGH-VALUE.
           05 WS-PF8                   PIC X.
              88 PF8-DEBFIN               VALUE 'A'.
              88 PF8-FIN                  VALUE 'Z'.
              88 PF8-INACTIVE             VALUE LOW-VALUE.
              88 PF8-ACTIVE               VALUE HIGH-VALUE.
           05 WS-POSITION-CLE          PIC X.
              88 DEBUT                    VALUE 'D'.
              88 MILIEU                   VALUE 'M'.
              88 FIN                      VALUE 'F'.
           05 WS-DER-POSTE-TS          PIC 9.
           05 WS-CPT                   PIC S9(3) COMP-3.
           05 WS-CPT-ART               PIC S9(4) COMP.
           05 WS-MCDE                  PIC X.
           05 WS-MLOTM                 PIC X(6).
           05 WS-MQTE                  PIC 9(5).
           05 WS-MQTE-CARA REDEFINES WS-MQTE
                                       PIC X(5).
           05 WS-COMM-MSG              PIC X(79).
           05 WS-NB-LIGNE              PIC S9(4) COMP.
           05 WS-CA-LAST-CMD           PIC X.
              88 CMD-VALID                VALUE '1'.
              88 CMD-INVALID              VALUE '0'.
