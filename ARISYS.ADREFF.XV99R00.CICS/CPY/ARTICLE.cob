      *----------------------------------------------------------------*
      * DESCRIPTION DE L'ENREGISTREMENT DU FICHIER ARTICLE             *
      *----------------------------------------------------------------*
      *
      *    ----TABLE----
      *
       01  WS-ART-ENR.
           05  WS-ART-CODE           PIC X(5).
           05  WS-ART-LIBEL          PIC X(20).
           05  WS-ART-CATEG          PIC X(5).
           05  WS-ART-FOU            PIC 9(5).
           05  WS-ART-DELAI          PIC 99.
           05  WS-ART-QTE            PIC 9(6).
           05  WS-ART-ALERT          PIC 9(5).
           05  WS-ART-NB-LOT         PIC 9.
           05  WS-ART-TLOT.
               10  WS-ART-LOT         OCCURS 5.
                  15  WS-ART-LOT-NUM  PIC X(6).
                  15  WS-ART-LOT-QTE  PIC 9(5).
                  15  WS-ART-LOT-PXU  PIC 9(5)V99.
           05  WS-FILLER             PIC X.
      *
      *    ----INDICE----
      *
       01  WS-IND                    PIC S9(4) COMP.
