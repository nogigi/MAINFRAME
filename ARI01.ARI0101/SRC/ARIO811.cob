      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO811                                   *
      *  NOM DU REDACTEUR : BAUDELET                                  *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 24/04/2024                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *SOUS PROGRAMME QUI TROUVE TOUT LES OCCURENCE E DANS T1         *
      *ET ECRIT LES POS I J K DE TOUT LES OCCURENCE DANS T3           *
      *RETOURNE S TAILLE DE T3 / NOMBRE D OCCURENCE DANS T1           *
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
       PROGRAM-ID.      ARIO811 IS INITIAL.
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
      * INDICE T LOCAL
       01  WS-T1-I              PIC S9(4)    COMP   VALUE ZERO.
       01  WS-T1-J              PIC S9(4)    COMP   VALUE ZERO.
       01  WS-T1-K              PIC S9(4)    COMP   VALUE ZERO.

      *========================
       LINKAGE SECTION.
      *TABLEAU TO GET
       01  LS-TAB1.
         05 FILLER              OCCURS 10.
           10 FILLER            OCCURS 10.
              15 FILLER         OCCURS 10.
                 20 LS-ELEM     PIC 9(2).
      *E VARAIBLE TO SEARCH
       01  LS-E                 PIC 9(2).
       01  LS-TAB3.
         05 FILLER              OCCURS 100.
           10 LS-ELEM-I         PIC 9(2).
           10 LS-ELEM-J         PIC 9(2).
           10 LS-ELEM-K         PIC 9(2).
      *S TO RETURN
       01  LS-S                 PIC 9(2).

      *========================
      *
      *
      *                  ==============================               *
      *=================<   PROCEDURE       DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
       PROCEDURE           DIVISION USING LS-TAB1 LS-E LS-TAB3 LS-S.
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
      *SEARCH IN T1 ALL E SET T3 ALL POS GET MAX OCCUR
           PERFORM 7000-T3-BUILD-DEB
              THRU 7000-T3-BUILD-FIN.
      *RETURN T3 AND S MAX OCCUR
       0000-PROGRAMME-FIN.
           EXIT PROGRAM.
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
       7000-T3-BUILD-DEB.
           MOVE ZERO                   TO LS-S.
           PERFORM 7010-SEARCH-T1-DEB THRU 7010-SEARCH-T1-FIN
                   VARYING WS-T1-I  FROM 1 BY 1
                   UNTIL WS-T1-I > 10
                   AFTER WS-T1-J    FROM 1 BY 1
                   UNTIL WS-T1-J > 10
                   AFTER WS-T1-K    FROM 1 BY 1
                   UNTIL WS-T1-K > 10.
       7000-T3-BUILD-FIN.
           EXIT.
       7010-SEARCH-T1-DEB.
           IF LS-ELEM(WS-T1-I, WS-T1-J, WS-T1-K) = LS-E
              ADD 1                    TO LS-S
              MOVE WS-T1-I             TO LS-ELEM-I(LS-S)
              MOVE WS-T1-J             TO LS-ELEM-J(LS-S)
              MOVE WS-T1-K             TO LS-ELEM-K(LS-S)
           END-IF.
       7010-SEARCH-T1-FIN.
           EXIT.

      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *8000 SECTION
      *
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
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO811         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO811        *'.
           DISPLAY '*==============================================*'.
           MOVE 12                     TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.
