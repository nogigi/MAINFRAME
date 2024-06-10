CBL MAP, LIST, SSRANGE, TEST
      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIT111                                   *
      *  NOM DU REDACTEUR : FORMATEUR                                 *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 03/12/2020                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *                                                               *
      *                                                               *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      *               !                                               *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIT111.
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
       01 TABLE9.
           05 WS-AA OCCURS 16.
              10 WS-BB OCCURS 16 PIC S9(5) COMP-3.
       01 TABLEX REDEFINES TABLE9 PIC X(768).
      *
      *========================
       LINKAGE SECTION.
      *========================
       01 LS-TRUC.
          05 FILLER              PIC X(5).
          05 LS-A                PIC 9(2).
          05 LS-B                PIC 9(2) COMP.
          05 LS-ZUT              PIC X(3).
      *                  ==============================               *
      *=================<   PROCEDURE       DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
       PROCEDURE           DIVISION USING LS-TRUC.
      *
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE ALGORITHMIQUE DU PROGRAMME      *
      *---------------------------------------------------------------*
      *                                                               *
      *    1 : LES COMPOSANTS DU DIAGRAMME SONT CODES A L'AIDE DE     *
      *        DEUX PARAGRAPHES  XXXX-COMPOSANT-DEB                   *
      *                          XXYY-COMPOSANT-FIN                   *
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
      *
           PERFORM 1000-TABLEAU-DEB
              THRU 1000-TABLEAU-FIN
      *
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *
       0000-PROGRAMME-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *
       1000-TABLEAU-DEB.
      *
           EVALUATE LS-B
              WHEN  1 PERFORM 7020-CALCUL-DEB
                         THRU 7020-CALCUL-FIN
              WHEN  2 PERFORM 7021-CALCUL-DEB
                         THRU 7021-CALCUL-FIN
              WHEN  3 PERFORM 7022-CALCUL-DEB
                         THRU 7022-CALCUL-FIN
              WHEN  4 PERFORM 7023-CALCUL-DEB
                         THRU 7023-CALCUL-FIN
              WHEN  5 PERFORM 7024-CALCUL-DEB
                         THRU 7024-CALCUL-FIN
              WHEN  6 PERFORM 7025-CALCUL-DEB
                         THRU 7025-CALCUL-FIN
              WHEN  7 PERFORM 7026-CALCUL-DEB
                         THRU 7026-CALCUL-FIN
              WHEN  8 PERFORM 7027-CALCUL-DEB
                         THRU 7027-CALCUL-FIN
              WHEN  9 PERFORM 7028-CALCUL-DEB
                         THRU 7028-CALCUL-FIN
              WHEN 10 PERFORM 7029-CALCUL-DEB
                         THRU 7029-CALCUL-FIN
              WHEN 11 PERFORM 7030-CALCUL-DEB
                         THRU 7030-CALCUL-FIN
              WHEN 12 PERFORM 7031-CALCUL-DEB
                         THRU 7031-CALCUL-FIN
              WHEN 13 PERFORM 7032-CALCUL-DEB
                         THRU 7032-CALCUL-FIN
              WHEN 14 PERFORM 7033-CALCUL-DEB
                         THRU 7033-CALCUL-FIN
              WHEN 15 PERFORM 7034-CALCUL-DEB
                         THRU 7034-CALCUL-FIN
            END-EVALUATE.
      *
       1000-TABLEAU-FIN.
           EXIT.
      *
      *
      *
      *===============================================================*
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
      *---------------------------------------------------------------*
      *                                                               *
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *   9XXX-  : PROTECTION FIN DE PROGRAMME                        *
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *                                                               *
      *===============================================================*
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *
       7020-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7020-CALCUL-FIN.
           EXIT.
      *
       7021-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7021-CALCUL-FIN.
           EXIT.
      *
       7022-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7022-CALCUL-FIN.
           EXIT.
      *
       7023-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7023-CALCUL-FIN.
           EXIT.
      *
       7024-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7024-CALCUL-FIN.
           EXIT.
      *
       7025-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7025-CALCUL-FIN.
           EXIT.
      *
       7026-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7026-CALCUL-FIN.
           EXIT.
      *
       7027-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7027-CALCUL-FIN.
           EXIT.
      *
       7028-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7028-CALCUL-FIN.
           EXIT.
      *
       7029-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7029-CALCUL-FIN.
           EXIT.
      *
       7030-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7030-CALCUL-FIN.
           EXIT.
      *
       7031-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7031-CALCUL-FIN.
           EXIT.
      *
       7032-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7032-CALCUL-FIN.
           EXIT.
      *
       7033-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7033-CALCUL-FIN.
           EXIT.
      *
       7034-CALCUL-DEB.
      *
           ADD 1 TO WS-BB(LS-A, LS-B).
      *
       7034-CALCUL-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
      * 8999-STATISTIQUES-DEB.
      *
      *     DISPLAY '************************************************'
      *     DISPLAY '*     STATISTIQUES DU PROGRAMME TESTCOBS       *'
      *     DISPLAY '*     ==================================       *'
      *     DISPLAY '************************************************'.
      *
      * 8999-STATISTIQUES-FIN.
      *     EXIT.
      *
      *---------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *---------------------------------------------------------------*
      *
      *9000-APPEL-SP-DEB.
      *
      *9000-APPEL-SP-FIN.
      *     EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIT111         *'
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           STOP RUN.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIT111        *'
           DISPLAY '*==============================================*'.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.
