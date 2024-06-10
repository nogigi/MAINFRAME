      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO111                                   *
      *  NOM DU REDACTEUR : BAUDELET                                  *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 27/03/2024                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * …DITE LES BALANCES DES COMPTES CLIENT ET LES ERREURS DE       *
      * MOUVEMENT DANS LA SORTIE STANDARD                             *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * XX/XX/XXXX    !                                               *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO111.
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
      *                      F-MVTS-E : FICHIER DES MOUVEMENTS
      *                      -------------------------------------------
           SELECT  F-MVTS-E        ASSIGN TO INP001
                   FILE STATUS         IS WS-FS-MVTS-E.
      *                      -------------------------------------------
      *
      *
      *                  ==============================               *
      *=================<       DATA        DIVISION   >==============*
      *                  ==============================               *
      *   DDNAME :INP001                                              *
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
       FD  F-MVTS-E
           RECORDING MODE IS F.
       01  FS-ENRG-F-MVTS-F      PIC X(50).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      * DEB WS F-MVTS-E
       77  WS-FS-MVTS-E          PIC X(2).
       01  WS-ENRG-F-MVTS.
           05  WS-MVTS-CPTE      PIC 9(10).
           05  WS-MVTS-DATE.
              10  WS-MVTS-ANNEE.
                  15  WS-MVTS-SS PIC 99.
                  15  WS-MVTS-AA PIC 99.
              10  WS-MVTS-MM     PIC 99.
              10  WS-MVTS-JJ     PIC 99.
           05  WS-MVTS-CODE      PIC X.
           05  WS-MVTS-MT        PIC 9(8)V99.
           05  FILLER            PIC X(21).
      * FIN DU WS F-MVTS-E
      * =======================
      * DEB WS ETAT DES OPERATIONS
       01  WS-LASTER         PIC X(45) VALUE ALL '*'.
       01  WS-LCPTE.
           05  FILLER        PIC X(28) VALUE 'NUMERO DE COMPTE'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-OCPT       PIC 9(10).
       01  WS-LTIRET         PIC X(45) VALUE ALL '-'.
       01  WS-LCB.
           05  FILLER        PIC X(28) VALUE 'CUMUL CARTE-BLEUE'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-OCB        PIC 9(10)V99.
       01  WS-LRDAB.
           05  FILLER        PIC X(28) VALUE 'CUMUL RETRAIT DAB'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-ORDAB      PIC 9(10)V99.
       01  WS-LDGUI.
           05  FILLER        PIC X(28) VALUE 'CUMUL DEPOT GUICHET'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-ODGUI      PIC 9(10)V99.
       01  WS-LBAL.
           05  FILLER        PIC X(12) VALUE 'BALANCE DES '.
           05  FILLER        PIC X(16) VALUE 'OPERATIONS'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-OBAL       PIC S9(10)V99.
      * FIN DU WS ETAT DES OPERATIONS
      * DEB WS ETAT DES ERREURS
       01  WS-LECPT.
           05  FILLER        PIC X(7)  VALUE 'ERREUR '.
           05  FILLER        PIC X(18) VALUE 'POUR LE COMPTE'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-ECPT       PIC 9(10).
       01  WS-LEMVT.
           05  FILLER        PIC X(25) VALUE 'CODE MOUVEMENT'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-EMVT       PIC X.
       01  WS-LEMT.
           05  FILLER        PIC X(25) VALUE 'MONTANT'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-EMT        PIC 9(8)V99.
      * FIN WS ETAT DES ERREURS
      * DEB WS COMPTE RENDU D'EXECUTION
       01  WS-LCLIENT.
           05  FILLER        PIC X(30) VALUE 'NOMBRE DE CLIENTS'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-CCLI       PIC 9(3)  VALUE ZERO.
       01  WS-LCMVT.
           05  FILLER        PIC X(30) VALUE 'NOMBRE DE MOUVEMENTS'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-CMVT       PIC 9(3)  VALUE ZERO.
       01  WS-LCERR.
           05  FILLER        PIC X(10) VALUE 'NOMBRE DE '.
           05  FILLER        PIC X(20) VALUE 'MOUVEMENTS ERRONES'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-CERR       PIC 9(3).
       01  WS-LRETRAIT.
           05  FILLER        PIC X(30) VALUE 'NOMBRE DE RETRAITS'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-CRET       PIC 9(3)  VALUE ZERO.
       01  WS-LCCB.
           05  FILLER        PIC X(10) VALUE 'NOMBRE DE '.
           05  FILLER        PIC X(20) VALUE 'CARTES BLEUES'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-CCB        PIC 9(3)  VALUE ZERO.
       01  WS-LDEP.
           05  FILLER        PIC X(30) VALUE 'NOMBRE DE DEPOTS'.
           05  FILLER        PIC X(3)  VALUE ':'.
           05  WS-CDEP       PIC 9(3)  VALUE ZERO.
      * FIN WS COMPTE RENDU D'EXECUTION
      *
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
      * 0000 TRT PRINCIPAL
      * TRAITEMENT PRINCIPALE OUVRE FERME ET PERFORM LA BOUCLE        *
      * PRINCIPALE
       0000-TRT-PRINCIPAL-DEB.
      *
      *---------------------------------------------------------------*
      *             TRAITEMENT OREILLETTE GAUCHE
      *---------------------------------------------------------------*
      *
           PERFORM  6000-OPEN-F-MVTS-E-DEB
              THRU  6000-OPEN-F-MVTS-E-FIN.
      *
           PERFORM  6010-READ-F-MVTS-E-DEB
              THRU  6010-READ-F-MVTS-E-FIN.
      *
           IF WS-FS-MVTS-E = '10'
              DISPLAY 'F-MVTS-E VIDE'
           END-IF.
      *---------------------------------------------------------------*
      *             TRAITEMENT ITERATIVE
      *---------------------------------------------------------------*
      *
           PERFORM  1000-TRT-CLIENTS-DEB
              THRU  1000-TRT-CLIENTS-FIN
             UNTIL  WS-FS-MVTS-E = '10'.
      *
      *---------------------------------------------------------------*
      *             TRAITEMENT OREILLETTE DROITE
      *---------------------------------------------------------------*
      *
      *
           PERFORM  7060-TRT-NB-MVTS-DEB
              THRU  7060-TRT-NB-MVTS-FIN.
      *
           PERFORM  6020-CLOSE-F-MVTS-E-DEB
              THRU  6020-CLOSE-F-MVTS-E-FIN.
      *
           PERFORM  8999-STATISTIQUES-DEB
              THRU  8999-STATISTIQUES-FIN.
      *
           PERFORM  9999-FIN-PROGRAMME-DEB
              THRU  9999-FIN-PROGRAMME-FIN.
      *
       0000-TRT-PRINCIPAL-FIN.
           STOP RUN.
      *
      *-----------------------------------------------------
      * TRT 1000 CUMULE TOUT LES MOUVEMENT POUR UN 1 COMPTE
      *----------------------------------------------------
       1000-TRT-CLIENTS-DEB.
      *---------------------------------------------------
      *GAUCHE
      *------------------
           PERFORM  7000-TRT-VAR-CLIENT-DEB
              THRU  7000-TRT-VAR-CLIENT-FIN.
      *-------------------------------------------
      *  ITERATIVE
      *--------------------------
           PERFORM  2000-TRT-MVTS-DEB
              THRU  2000-TRT-MVTS-FIN
             UNTIL  WS-FS-MVTS-E = '10' OR WS-MVTS-CPTE NOT = WS-OCPT.
      *-------------------------------------------
      * DROITE
      *---------------------
           IF WS-OCB > 0 OR WS-ORDAB > 0 OR WS-ODGUI > 0
              PERFORM  7050-TRT-BALANCE-DEB
                 THRU  7050-TRT-BALANCE-FIN
      *
              PERFORM  8010-EDITION-ETAT-OP-DEB
                 THRU  8010-EDITION-ETAT-OP-FIN
           END-IF.
      *
       1000-TRT-CLIENTS-FIN.
           EXIT.
      *---------------------------------------------------
      * 2000 TRT GERE LES DIFFÈRENTS CODE MOUVEMENT BANCAIRE
      *----------------------------------------------------
       2000-TRT-MVTS-DEB.
      *-----------------------------------
      * GAUCHE
      *-------------------------
      *
      *---------------------
      * ALTERNATIVE MULTIPLE
      *--------------------------
            EVALUATE  WS-MVTS-CODE
                WHEN  'R'  PERFORM 3000-TRT-CODE-R-DEB
                              THRU 3000-TRT-CODE-R-FIN
      *
                WHEN  'C'  PERFORM 3010-TRT-CODE-C-DEB
                              THRU 3010-TRT-CODE-C-FIN
      *
                WHEN  'D'  PERFORM 3020-TRT-CODE-D-DEB
                              THRU 3020-TRT-CODE-D-FIN
      *
                WHEN OTHER PERFORM 3030-TRT-CODE-OTHER-DEB
                              THRU 3030-TRT-CODE-OTHER-FIN
      *
            END-EVALUATE.
      *-----------------------------------
      * DROITE
      *--------------------------------
            PERFORM 6010-READ-F-MVTS-E-DEB
               THRU 6010-READ-F-MVTS-E-FIN.
      *
       2000-TRT-MVTS-FIN.
            EXIT.
      *---------------------------------------
      *EXECUTE LE CALCULE DU CODE MOUVEMENT RETRAIT
      *----------------------------------------
       3000-TRT-CODE-R-DEB.
      *
            PERFORM 7010-CALC-R-DEB
               THRU 7010-CALC-R-FIN.
      *
       3000-TRT-CODE-R-FIN.
            EXIT.
      *---------------------------------------
      *EXECUTE LE CALCULE DU CODE MOUVEMENT CRÈDIT
      *------------------------------------------
       3010-TRT-CODE-C-DEB.
      *
            PERFORM 7020-CALC-C-DEB
               THRU 7020-CALC-C-FIN.
      *
       3010-TRT-CODE-C-FIN.
            EXIT.
      *------------------------------------
      *EXECUTE LE CALCULE DU CODE MOUVEMENT DEBIT
      *------------------------------------------
       3020-TRT-CODE-D-DEB.
      *
            PERFORM 7030-CALC-D-DEB
               THRU 7030-CALC-D-FIN.
      *
       3020-TRT-CODE-D-FIN.
            EXIT.
      *-------------------------------------
      *EXECUTE LE CALCULE ET L EDITION CODE FAUX
      *-----------------------------------------
       3030-TRT-CODE-OTHER-DEB.
      *
            PERFORM 7040-CALC-OTHER-DEB
               THRU 7040-CALC-OTHER-FIN.
      *
            PERFORM 8000-EDITION-ETAT-ANO-DEB
               THRU 8000-EDITION-ETAT-ANO-FIN.
      *
       3030-TRT-CODE-OTHER-FIN.
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
      *6000-ORDRE-FICHIER-DEB.
      *
      *6000-ORDRE-FICHIER-FIN.
      *    EXIT.
      *
      *6000 EXECUTE L OPEN DU FICHIER FMVTS-E
      *----------------------------------------
       6000-OPEN-F-MVTS-E-DEB.
      *
            OPEN INPUT F-MVTS-E.
            IF WS-FS-MVTS-E NOT = '00'
               DISPLAY 'PROBLEME D''OUVERTURE FICHIER F-MVTS-E'
               DISPLAY 'VALEUR DU FS= ' WS-FS-MVTS-E
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
      *
            END-IF.
      *
       6000-OPEN-F-MVTS-E-FIN.
            EXIT.
      *-------------------------------------
      *6010 EXECUTE LA LECTURE SUR FMVTS-E
      *------------------------------------
       6010-READ-F-MVTS-E-DEB.
      *
            READ F-MVTS-E INTO WS-ENRG-F-MVTS.
            IF NOT (WS-FS-MVTS-E = '00' OR '10')
               DISPLAY 'PROBLEMME DE LECTURE DU FICHIER F-MVTS-E'
               DISPLAY 'VALEUR DU FS= ' WS-FS-MVTS-E
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
      *
            END-IF.
      *
       6010-READ-F-MVTS-E-FIN.
            EXIT.
      *-------------------------------------
      *6020 EXECUTE LA FERMTURE SUR FMVTS-E
      *------------------------------------
       6020-CLOSE-F-MVTS-E-DEB.
      *
            CLOSE F-MVTS-E.
            IF WS-FS-MVTS-E NOT = '00'
               DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-MVTS-E'
               DISPLAY 'VALEUR DU FS= ' WS-FS-MVTS-E
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
      *
            END-IF.
      *
       6020-CLOSE-F-MVTS-E-FIN.
            EXIT.
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
      *7000-ORDRE-CALCUL-DEB.
      *
      *7000-ORDRE-CALCUL-FIN.
      *    EXIT.
      *
      *-------------------------------------
      *7000 INTITIALISE LES VARIBLES POUR UN NOUVEAU COMPTE
      *------------------------------------
       7000-TRT-VAR-CLIENT-DEB.
      *
            MOVE  WS-MVTS-CPTE TO WS-OCPT
            MOVE  WS-MVTS-CPTE TO WS-ECPT
            MOVE  ZERO         TO WS-ORDAB WS-ODGUI WS-OBAL WS-OCB
            ADD   1            TO WS-CCLI.
      *
       7000-TRT-VAR-CLIENT-FIN.
            EXIT.
      *-------------------------------------
      *7010 ADDITIONNE LE CUMULE DES RETRAITS
      *------------------------------------
       7010-CALC-R-DEB.
      *
            ADD  WS-MVTS-MT    TO WS-ORDAB
            ADD  1             TO WS-CRET.
      *
       7010-CALC-R-FIN.
            EXIT.
      *-------------------------------------
      *7020 ADDITIONNE LE CUMULE DES CARTE BLEU
      *------------------------------------
       7020-CALC-C-DEB.
      *
            ADD  WS-MVTS-MT    TO WS-OCB
            ADD  1             TO WS-CCB.
      *
       7020-CALC-C-FIN.
            EXIT.
      *-------------------------------------
      *7030 ADDITIONNE LE CUMULE DEPOT GUICHET
      *------------------------------------
       7030-CALC-D-DEB.
      *
            ADD  WS-MVTS-MT    TO WS-ODGUI
            ADD  1             TO WS-CDEP.
      *
       7030-CALC-D-FIN.
            EXIT.
      *-------------------------------------
      *7040 ADDITIONNE LE CUMULE DES MVT FAUX
      *------------------------------------
       7040-CALC-OTHER-DEB.
      *
            MOVE WS-MVTS-CODE  TO WS-EMVT
            MOVE WS-MVTS-MT    TO WS-EMT
            ADD  1             TO WS-CERR.
      *
       7040-CALC-OTHER-FIN.
            EXIT.
      *-------------------------------------
      *7050 FAIT LE CALCULE DE LA BALANCE MVT CLIENT
      *------------------------------------
       7050-TRT-BALANCE-DEB.
      *
            COMPUTE WS-OBAL = WS-ODGUI - WS-OCB - WS-ORDAB.
      *
       7050-TRT-BALANCE-FIN.
            EXIT.
      *-------------------------------------
      *7060 FAIT LE CUMULE DE TOUT LES MOUVEMENT
      *------------------------------------
       7060-TRT-NB-MVTS-DEB.
      *
            COMPUTE WS-CMVT = WS-CERR + WS-CRET + WS-CCB + WS-CDEP.
      *
       7060-TRT-NB-MVTS-FIN.
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
      *-------------------------------------
      *8000 AFFICHE LES ANNOMALIES
      *------------------------------------
       8000-EDITION-ETAT-ANO-DEB.
      *
            DISPLAY WS-LASTER.
            DISPLAY WS-LECPT.
            DISPLAY WS-LEMVT.
            DISPLAY WS-LEMT.
            DISPLAY WS-LASTER.
      *
       8000-EDITION-ETAT-ANO-FIN.
            EXIT.
      *-------------------------------------
      *8010 AFFICHE LES ETAT DES OPERATIONS
      *------------------------------------
      *
       8010-EDITION-ETAT-OP-DEB.
      *
            DISPLAY WS-LASTER.
            DISPLAY WS-LCPTE.
            DISPLAY WS-LTIRET.
            DISPLAY WS-LCB.
            DISPLAY WS-LRDAB.
            DISPLAY WS-LDGUI.
            DISPLAY WS-LTIRET.
            DISPLAY WS-LBAL.
            DISPLAY WS-LASTER.
      *
       8010-EDITION-ETAT-OP-FIN.
            EXIT.
      *-------------------------------------
      *8010 AFFICHE LE COMPTE RENDU D EXECUTION
      *------------------------------------
       8999-STATISTIQUES-DEB.
      *
            DISPLAY '************************************************'.
            DISPLAY '*      STATISTIQUE DU PROGRAMME ARIO111        *'.
            DISPLAY '*      ================================        *'.
            DISPLAY '************************************************'.
            DISPLAY WS-LASTER.
            DISPLAY WS-LCLIENT.
            DISPLAY WS-LCMVT.
            DISPLAY WS-LCERR.
            DISPLAY WS-LRETRAIT.
            DISPLAY WS-LCCB.
            DISPLAY WS-LDEP.
            DISPLAY WS-LASTER.
      *
       8999-STATISTIQUES-FIN.
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
            DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO111         *'.
            DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
            EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
            DISPLAY '*==============================================*'.
            DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
            DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO111        *'.
            DISPLAY '*==============================================*'.
            MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
            STOP RUN.
