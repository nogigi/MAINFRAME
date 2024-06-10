      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO4711                                  *
      *  NOM DU REDACTEUR : BAUDELET                                  *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 23/04/2024                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * EXERCICE SUR LES TABLEAUX                                     *
      * 711 PREND EN ENTRER UNE COMMANDE SYSIN DE 2 OCT               *
      * LA FIN SYSIN EST CODER '$$'                                   *
      * T1 EST UN TABLEAU DE 3 DIM DE TAILLE 10X10X10 TYPE PIC 9(2)   *
      * T3 EST UN TABLEAU DE 1 DIM DE TAILLE 100 TYPE PIC 9(2)        *
      * T1 EST INIT AVEC LA SOMME DES 3 INDICE I J K                  *
      * T3 EST PASSER AU SOUS PRG 811 . IL CONTIENT TOUT LES POS DES  *
      *OCCURENCE E DANS T1                                            *
      * CORRESPONDANT AU VALEUR DE T1                                 *
      *PUIS LE PROGRAMME PRINT LES POS (I J K) DE TOUT LES OCCURENCES *
      * MAXIMALE DANS T3 POUR UNE COMMANDE DONNER E                   *
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
       PROGRAM-ID.      ARIO711.
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
      *-----SHARED VARIABLE------------
      *-----PRIVATE
       01  WS-TAB1.
         05 FILLER              OCCURS 10.
           10 FILLER            OCCURS 10.
              15 FILLER         OCCURS 10.
                 20 WS-ELEM     PIC 9(2).
       01  WS-E                 PIC 9(2).
      *-------PUBLIC
       01  WS-TAB3.
         05 FILLER              OCCURS 100.
           10 WS-ELEM-I         PIC 9(2).
           10 WS-ELEM-J         PIC 9(2).
           10 WS-ELEM-K         PIC 9(2).
      *S TO RETURN
       01  WS-S                 PIC 9(2).
      *--------LOCAL VARIABLE----------------
       01  WS-SYSIN.
           05  WS-DEM           PIC XX.
               88 FIN-SYSIN                         VALUE '$$'.
           05  FILLER           PIC X(78).
      * INDICE
       01  WS-T1-I              PIC S9(4)    COMP   VALUE ZERO.
       01  WS-T1-J              PIC S9(4)    COMP   VALUE ZERO.
       01  WS-T1-K              PIC S9(4)    COMP   VALUE ZERO.
       01  WS-T3-I              PIC S9(4)    COMP   VALUE ZERO.
      *PRG LOGIC
       01  WS-COUNT             PIC 9(2)            VALUE ZERO.
       01  WS-COUNT-ED          PIC Z9.
       01  WS-STRING            PIC X(80).
       01  WS-CALC              PIC 9(2).
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
      *gauche
           ACCEPT WS-SYSIN FROM SYSIN.
           PERFORM 7000-INIT-TAB-DEB THRU 7000-INIT-TAB-FIN
                   VARYING WS-T1-I  FROM 1 BY 1
                   UNTIL WS-T1-I > 10
                   AFTER WS-T1-J    FROM 1 BY 1
                   UNTIL WS-T1-J > 10
                   AFTER WS-T1-K    FROM 1 BY 1
                   UNTIL WS-T1-K > 10.
           PERFORM 8000-PRINT-T1-DEB
              THRU 8000-PRINT-T1-FIN.
      *IETRATIVE
           PERFORM 1000-TRT-SYSIN-DEB
              THRU 1000-TRT-SYSIN-FIN
             UNTIL FIN-SYSIN.
      *    droite
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
       0000-PROGRAMME-FIN.
           STOP RUN.
       1000-TRT-SYSIN-DEB.
           PERFORM 7010-INIT-E-DEB
              THRU 7010-INIT-E-FIN.
      *gauche
           PERFORM 9000-PRG8-DEB
              THRU 9000-PRG8-FIN.
      *droite
           PERFORM 7020-ADD-COUNT-DEB
              THRU 7020-ADD-COUNT-FIN.
      *header
           IF WS-S = 0
              PERFORM 8030-PRINT-ZERO-DEB
                 THRU 8030-PRINT-ZERO-FIN
           ELSE
              PERFORM 8010-PRINT-HEADER-DEB
                 THRU 8010-PRINT-HEADER-FIN
           END-IF.
      *print t3
           PERFORM 8020-PRINT-POS-DEB
              THRU 8020-PRINT-POS-FIN
                   VARYING WS-T3-I  FROM 1 BY 1
                   UNTIL WS-T3-I > WS-S.
           ACCEPT WS-SYSIN FROM SYSIN.
       1000-TRT-SYSIN-FIN.
           EXIT.
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
       7000-INIT-TAB-FIN.
           EXIT.
       7010-INIT-E-DEB.
           MOVE WS-DEM               TO WS-E.
       7010-INIT-E-FIN.
           EXIT.
       7020-ADD-COUNT-DEB.
           ADD 1                    TO WS-COUNT.
       7020-ADD-COUNT-FIN.
           EXIT.
      * BOUCLE IMBRIQUER SUR T2 ET T1
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *8000 SECTION
      *
       8000-PRINT-T1-DEB.
           DISPLAY 'ETAPE 2 - TABLEAU-1 GLOBAL :'
           DISPLAY WS-TAB1.
           DISPLAY ' '.
       8000-PRINT-T1-FIN.
           EXIT.
       8010-PRINT-HEADER-DEB.
           MOVE SPACES                 TO WS-STRING.
           MOVE WS-COUNT               TO WS-COUNT-ED.
           DISPLAY ' '.
           STRING 'ETAPE 2  - VALEUR RECHERCHEE'
           WS-COUNT-ED
           ' : '
           WS-E
           DELIMITED BY SIZE
           INTO WS-STRING.
           DISPLAY WS-STRING.
      *ligne 2
           MOVE SPACES                 TO WS-STRING.
           STRING 'TROUVEE '
           WS-S
           ' FOIS DANS LES POSTES :'
           DELIMITED BY SIZE
           INTO WS-STRING.
           DISPLAY WS-STRING.
       8010-PRINT-HEADER-FIN.
           EXIT.
       8020-PRINT-POS-DEB.
           MOVE SPACES                 TO WS-STRING.
           STRING WS-ELEM-I(WS-T3-I)
           ' , '
           WS-ELEM-J(WS-T3-I)
           ' , '
           WS-ELEM-K(WS-T3-I)
           DELIMITED BY SIZE
           INTO WS-STRING.
           DISPLAY WS-STRING.
       8020-PRINT-POS-FIN.
           EXIT.
       8030-PRINT-ZERO-DEB.
           MOVE SPACES                 TO WS-STRING.
           MOVE WS-COUNT               TO WS-COUNT-ED.
           DISPLAY ' '.
           STRING 'ETAPE 2  - VALEUR RECHERCHEE'
           WS-COUNT-ED
           ' : '
           WS-E
           DELIMITED BY SIZE
           INTO WS-STRING.
           DISPLAY WS-STRING.
      *ligne 2
           MOVE SPACES                 TO WS-STRING.
           STRING 'AUCCUNE OCCURENCE'
           DELIMITED BY SIZE
           INTO WS-STRING.
           DISPLAY WS-STRING.
       8030-PRINT-ZERO-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *---------------------------------------------------------------*
      *
       9000-PRG8-DEB.
           CALL 'ARIO811'
           USING BY CONTENT WS-TAB1   BY CONTENT WS-E
                 BY REFERENCE WS-TAB3 BY REFERENCE WS-S.
       9000-PRG8-FIN.
           EXIT.
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
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO711         *'.
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


