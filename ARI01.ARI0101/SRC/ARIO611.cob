      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO4611                                   *
      *  NOM DU REDACTEUR : BAUDELET                                  *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 23/04/2024                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * EXERCICE SUR LES TABLEAUX                                     *
      * T1 EST UN TABLEAU DE 3 DIM DE TAILLE 10X10X10 TYPE PIC 9(2)   *
      * T2 EST UN TABLEAU DE 1 DIM DE TAILLE 30 TYPE PIC 9(5) COMP-3  *
      * T1 EST INIT AVEC LA SOMME DES 3 INDICE I J K                  *
      * T2 EST INIT AVEC LE NOMBRE D OCURENCE DE LA VALEUR DE L INCIDE*
      * CORRESPONDANT AU VALEUR DE T1                                 *
      *PUIS LE PROGRAMME PRINT LES POS (I J K) DE TOUT LES OCCURENCE  *
      * MAXIMALE DANS T2                                              *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * JJ/MM/SSAA    !                                               *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO611.
      *
      *                  ==============================               *
      *=================<  ENVIRONMENT      DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      **********************
       ENVIRONMENT DIVISION.
      **********************
      *
      *======================
       CONFIGURATION SECTION.
      *======================
      *
      *--------------
       SPECIAL-NAMES.
      *--------------
           DECIMAL-POINT IS COMMA.
      *
      *=====================
       INPUT-OUTPUT SECTION.
      *=====================
      *
      *-------------
       FILE-CONTROL.
      *-------------
      *
      *                      -------------------------------------------
      *                      F-CPTE-E: FICHIER DES COMPTE EN ENTRER
      *                      -------------------------------------------
      *
      *
      *                  ==============================               *
      *=================<       DATA        DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      ***************
       DATA DIVISION.
      ***************
      *
      *=============
       FILE SECTION.
      *=============
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
      *MY WS SECTION
       01  WS-TAB1.
         05 FILLER              OCCURS 10.
           10 FILLER            OCCURS 10.
              15 FILLER         OCCURS 10.
                 20 WS-ELEM     PIC 9(2).
      * INDICE
       01  WS-T1-I              PIC S9(4)    COMP   VALUE ZERO.
       01  WS-T1-J              PIC S9(4)    COMP   VALUE ZERO.
       01  WS-T1-K              PIC S9(4)    COMP   VALUE ZERO.
      * INDICE ED
       01  WS-T1-I-ED           PIC 99.
       01  WS-T1-J-ED           PIC 99.
       01  WS-T1-K-ED           PIC 99.
      * TD2
       01  WS-TAB2.
         05 FILLER              OCCURS 30.
            10 WS-ELEM2         PIC 9(5)     COMP-3 VALUE ZERO.
       01  WS-TAB2-ED.
         05 FILLER              OCCURS 30.
            10 WS-ELEM2-ED      PIC 9(2).
      *INDICE
       01  WS-T2-I              PIC S9(4)    COMP   VALUE ZERO.
      *INDICE ED
       01  WS-T2-I-ED           PIC 99.
      *PRG LOGIC
       01  WS-STEP              PIC 9               VALUE 1.
       01  WS-STRING            PIC X(80).
       01  WS-COUNT             PIC 9(1).
       01  WS-MAX-OCCUR-T2      PIC 9(2).
       01  WS-MAX-OCCCUR-VALUE  PIC 9(2).
       01  WS-CALC              PIC 9(2).
       01  WS-TEMP-ED           PIC 9(4).
      *
      *
      *                  ==============================               *
      *=================<   PROCEDURE       DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
       PROCEDURE           DIVISION.
      *
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE ALGORITHMIQUE DU PROGRAMME      *
      *---------------------------------------------------------------*
      *                                                               *
      *    1 : LES COMPOSANTS DU DIAGRAMME SONT CODES A L'AIDE DE     *
      *        DEUX PARAGRAPHES  XXXX-COMPOSANT-DEB                   *
      *                          XXYY-COMPOSANR-FIN                   *
      *                                                               *
      *    2 : XX REPRESENTE LE NIVEAU HIERARCHIQUE                   *
      *        YY DIFFERENCIE LES COMPOSANTS DE MEME NIVEAU           *
      *                                                               *
      *    3 : TOUT COMPOSANT EST PRECEDE D'UN CARTOUCHE DE           *
      *        COMMENTAIRE QUI EXPLICITE LE ROLE DU COMPOSANT         *
      *                                                               *
      *                                                               *
      *===============================================================*
      *===============================================================*
      *
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME              *
      *               ==================================              *
      *---------------------------------------------------------------*
      *
       0000-PROGRAMME-DEB.
      *INIT TAB1 ET TAB2
           PERFORM 7000-INIT-TAB-DEB THRU 7000-INIT-TAB-FIN
                   VARYING WS-T1-I  FROM 1 BY 1
                   UNTIL WS-T1-I > 10
                   AFTER WS-T1-J    FROM 1 BY 1
                   UNTIL WS-T1-J > 10
                   AFTER WS-T1-K    FROM 1 BY 1
                   UNTIL WS-T1-K > 10.
      *CONVERTE T2 TO DISPLAY
           PERFORM 7010-T2-TO-DISPLAY-DEB
              THRU 7010-T2-TO-DISPLAY-FIN
                   VARYING WS-T2-I  FROM 1 BY 1
                   UNTIL WS-T2-I > 30.
      *PRINT T1 ET T2
           PERFORM 8000-PRINT-DEB
              THRU 8000-PRINT-FIN.
      *FIND POS AND DISPLAY
           PERFORM 7020-FIND-POS-DEB
              THRU 7020-FIND-POS-FIN
                   VARYING WS-T2-I  FROM 1 BY 1
                   UNTIL WS-T2-I > 30.
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
       0000-PROGRAMME-FIN.
           STOP RUN.
      *===============================================================*
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
      *---------------------------------------------------------------*
      *                                                               *
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *                                                               *
      *===============================================================*
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *                                                               *
      *DEBUT OPEN
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *DEBUT 7000
       7000-INIT-TAB-DEB.
           COMPUTE WS-CALC = WS-T1-I + WS-T1-J + WS-T1-K.
           MOVE WS-CALC TO WS-ELEM(WS-T1-I, WS-T1-J, WS-T1-K).
           ADD  1       TO WS-ELEM2(WS-CALC).
       7000-INIT-TAB-FIN.
           EXIT.
      *CONVERTE T2 T2 DISPLAY
       7010-T2-TO-DISPLAY-DEB.
           MOVE WS-ELEM2(WS-T2-I) TO WS-ELEM2-ED(WS-T2-I).
           IF WS-ELEM2(WS-T2-I) > WS-MAX-OCCUR-T2
              MOVE WS-ELEM2(WS-T2-I) TO WS-MAX-OCCUR-T2
              MOVE WS-T2-I           TO WS-MAX-OCCCUR-VALUE
           END-IF.
       7010-T2-TO-DISPLAY-FIN.
           EXIT.
      * BOUCLE IMBRIQUER SUR T2 ET T1
       7020-FIND-POS-DEB.
           IF WS-ELEM2(WS-T2-I) = WS-MAX-OCCUR-T2
              ADD 1                    TO WS-COUNT
              PERFORM 8010-HEADER-DEB
                 THRU 8010-HEADER-FIN
              PERFORM 7030-DISPLAY-POS-DEB THRU 7030-DISPLAY-POS-FIN
                   VARYING WS-T1-I  FROM 1 BY 1
                   UNTIL WS-T1-I > 10
                   AFTER WS-T1-J    FROM 1 BY 1
                   UNTIL WS-T1-J > 10
                   AFTER WS-T1-K    FROM  1 BY 1
                   UNTIL WS-T1-K > 10
           END-IF.
       7020-FIND-POS-FIN.
           EXIT.
       7030-DISPLAY-POS-DEB.
           IF WS-ELEM(WS-T1-I, WS-T1-J, WS-T1-K) = WS-T2-I
              PERFORM 8020-POS-DEB
                 THRU 8020-POS-FIN
           END-IF.
       7030-DISPLAY-POS-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *8000 SECTION
      *
       8010-HEADER-DEB.
      * LIGNE 1
           MOVE SPACES                 TO WS-STRING.
           MOVE WS-T2-I                TO WS-T2-I-ED.
           DISPLAY ' '.
           STRING 'ETAPE '
           WS-STEP
           ' - VALEUR LA PLUS FREQUENTE '
           WS-COUNT
           ' : '
           WS-T2-I-ED
           DELIMITED BY SIZE
           INTO WS-STRING.
           DISPLAY WS-STRING.
      *   LIGNE 2
           MOVE SPACES                 TO WS-STRING.
           STRING 'TROUVEE '
           WS-MAX-OCCUR-T2
           ' FOIS DANS LES POSTES :'
           DELIMITED BY SIZE
           INTO WS-STRING.
           DISPLAY WS-STRING.
       8010-HEADER-FIN.
           EXIT.
       8020-POS-DEB.
           MOVE SPACES                 TO WS-STRING.
           MOVE WS-T1-I                TO WS-T1-I-ED.
           MOVE WS-T1-J                TO WS-T1-J-ED.
           MOVE WS-T1-K                TO WS-T1-K-ED.
           STRING WS-T1-I-ED
           ' , '
           WS-T1-J-ED
           ' , '
           WS-T1-K-ED
           DELIMITED BY SIZE
           INTO WS-STRING.
           DISPLAY WS-STRING.
       8020-POS-FIN.
           EXIT.
       8000-PRINT-DEB.
           DISPLAY 'ETAPE 1 - TABLEAU-1 GLOBAL :'
           DISPLAY WS-TAB1.
           DISPLAY ' '.
           DISPLAY 'ETAPE 1 - TABLEAU-2 GLOBAL :'
           DISPLAY WS-TAB2-ED.
       8000-PRINT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *---------------------------------------------------------------*
      *
      *9000-APPEL-SP-DEB.
      *
      *9000-APPEL-SP-FIN.
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO611         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO611        *'.
           DISPLAY '*==============================================*'.
           MOVE 12                     TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.


