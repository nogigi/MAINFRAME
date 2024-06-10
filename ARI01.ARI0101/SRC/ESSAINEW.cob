CBL    MAP,SSRANGE,LIST,NOOFFSET
CBL    MAP,SSRANGE,LIST,NOOFFSET
      *    ===========================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIOTgu                                   *
      *  NOM DU REDACTEUR : FORMATEUR                                 *
      *  SOCIETE          : ESTIAC INSTITUT                           *
      *    ===========================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIOTgu                                   *
      *  NOM DU REDACTEUR : FORMATEUR                                 *
      *  SOCIETE          : ESTIAC INSTITUT                           *
      *  DATE DE CREATION : 22/02/2023                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * CE PROGRAMME CORRESPOND A UN EXERCICE TD COMMENTE POUR        *
      * APPRENDRE COBOL  (AVANT CORRECTION)                           *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   §          NATURE DE LA MODIFICATION           *
      *---------------------------------------------------------------*
      * JJ/MM/SSAA    §                                              *
      *               §                                              *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIOTgu.
      *
      *                  ==============================               *
      *=================<   ENVIRONMENT     DIVISION   >==============*
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
      *             -------------------------------------------
      *             F-NBR-E: Fichier des nombres (NBR)
      *             -------------------------------------------
      * NOM DE FICHIER INTERNE : F-NBR-E
      * DDNAME                 : INP001
      *             -------------------------------------------
           SELECT F-NBR- ASSIGN TO INP001
           FILE STATUS IS WS-FS-NBR-E.
      *
      *                  ==============================               *
      *=================<   DATA            DIVISION   >==============*
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
      *---------------- FICHIER NOMBRE EN ENTREE----------------------*
      * LONGUEUR ENREGISTREMENT = 50
      *---------------------------------------------------------------*
       FD  F-NBR-E
           RECORDING MODE IS F.
      *
      *---------------- DESCRIPTION DE L'ENREGISTREMENT---------------*
       01  FS-ENR-NBR                       PIC X(50).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
       01  WS-FS-NBR-E                      PIC XX.
       01  WS-ENR-NBR.
           05  WS-ENR-NBR-NB                PIC 9(6).
           05  FILLER                       PIC X(44).
       01  WS-PG-NB                         PIC 9(6)     VALUE ZERO.
       01  WS-CPT-NB                        PIC S9(6).
      *
      *                  ==============================               *
      *=================<    PROCEDURE      DIVISION   >==============*
      *                  ==============================               *
      *
      *===============================================================*
      *
      *********************
       PROCEDURE  DIVISION.
      *********************
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
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME              *
      *               ==================================              *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      * DEBUT DU PROGRAMME                                            *
      *---------------------------------------------------------------*
       0000-TRT-PRINCIPAL-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
           PERFORM 6000-OPEN-NBR-DEB.
              THRU 6000-OPEN-NBR-FIN.
      *
           PERFORM 6010-READ-NBR-DEB
              THRU 6010-READ-NBR-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
           PERFORM 1000-TRT-FICHIER-DEB
              THRU 1000-TRT-FICHIER-FIN
             UNTIL WS-FS-NBR-E = '10'.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)                         *
      *---------------------------------------------------------------*
           PERFORM 8000-EDITION-NOMBRE-DEB
              THRU 8000-EDITION-NOMBRE-FIN.
      *
           PERFORM 6030-CLOSE-NBR-DEB
              THRU 6030-CLOSE-NBR-FIN.
      *
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *
      *---------------------------------------------------------------*
      * FIN DU PROGRAMME                                              *
      *---------------------------------------------------------------*
       0000-TRT-PRINCIPAL-FIN.
           STOP RUN.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT TRAITEMENT             *
      *               ===================================             *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT                                           *
      *---------------------------------------------------------------*
      *
       1000-TRT-FICHIER-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7000-INCREM-CPT-DEB
              THRU 7000-INCREM-CPT-FIN.
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE SIMPLE)               *
      *---------------------------------------------------------------*
           IF WS-ENR-NBR-NB > WS-PG-NB
             PERFORM 2000-TRT-NOMBRE-DEB
                THRU 2000-TRT-NOMBRE-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)                         *
      *---------------------------------------------------------------*
           PERFORM 6010-READ-NBR-DEB
              THRU 6010-READ-NBR-FIN.
      *
       1000-TRT-FICHIER-FIN.
           EXIT.
      *
       2000-TRT-NOMBRE-DEB.
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DE PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
           PERFORM 7010-TRANSFERT-NBR-DEB
              THRU 7010-TRANSFERT-NBR-FIN.
      *
       2000-TRT-NOMBRE-FIN.
           EXIT.
      *
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
      *6000-ORDRE-FICHIER-DEB.
      *
      *6000-ORDRE-FICHIER-FIN.
      *     EXIT.
      *
       6000-OPEN-NBR-DEB.
           OPEN INPUT F-NBR-E.
           IF WS-FS-NBR-E NOT = '00'
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-NBR-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-NBR-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-OPEN-NBR-FIN.
            EXIT.
      *
       6010-READ-NBR-DEB.
           READ F-NBR-E INTO WS-ENR-NBR.
           IF NOT (WS-FS-NBR-E = '00' OR '10')
              DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-NBR-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-NBR-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-READ-NBR-FIN.
      *
       6030-CLOSE-NBR-DEB.
           CLOSE F-NBR-E.
           IF WS-FS-NBR-E NOT = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-NBR-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-NBR-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6030-CLOSE-NBR-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
      *7000-ORDRE-CALCUL-DEB.
      *
      *7000-ORDRE-CALCUL-FIN.
      *     EXIT.
      *
       7000-INCREM-CPT-DEB.
           ADD 1              TO WS-CPT-NB.
       7000-INCREM-CPT-FIN.
           EXIT.
      *
       7010-TRANSFERT-NBR-DEB.
           MOVE WS-ENR-NBR-NB TO WS-PG-NB.
       7010-TRANSFERT-NBR-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
      *8000-ORDRE-EDITION-DEB.
      *
      *8000-ORDRE-EDITION-FIN.
      *    EXIT.
      *
       8000-EDITION-NOMBRE-DEB.
           DISPLAY 'PLUS GRAND NOMBRE LU : ' WS-PG-NB.
       8000-EDITION-NOMBRE-FIN.
           EXIT.
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
           DISPLAY '*============================================*'.
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIOTgu       *'.
           DISPLAY '*============================================*'.
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
           DISPLAY '*============================================*.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE         *.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIOTgu      *.
           DISPLAY '*============================================*.
           MOVE 12 TO RETURN-CODE.
       9999-ERREUR-PROGRAME-FIN.
           STOP RUN.
