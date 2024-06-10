      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO211                                   *
      *  NOM DU REDACTEUR : BAUDELET                                  *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 02/04/2024                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * …DITE LES BALANCES DES COMPTES CLIENT ET LES ERREURS DE       *
      * MOUVEMENT DANS F-ETATCLI-S ET F-ETATANO-S                     *
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
       PROGRAM-ID.      ARIO211.
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
      *                      F-ETATCLI-S: FICHIER SORTIE DES ETAT CLIENT
      *                      F-ETATANO-S: FICHIER SORTIE DES ANOMALIES
      *                      -------------------------------------------
           SELECT  F-MVTS-E        ASSIGN TO INP001
                   FILE STATUS         IS WS-FS-MVTS-E.
      *
           SELECT  F-ETATCLI-S     ASSIGN TO ETATCLI
                   FILE STATUS         IS WS-FS-ETATCLI.
      *
           SELECT  F-ETATANO-S    ASSIGN TO ETATANO
                   FILE STATUS         IS WS-FS-ETATANO.
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
      *INPUT FILE
       FD  F-MVTS-E
           RECORDING MODE IS F.
       01  FS-ENRG-F-MVTS-F      PIC X(50).
      *OUTPUT FILE
       FD F-ETATCLI-S
           RECORDING MODE IS F.
       01  FS-ETATCLI-BUFFER     PIC X(80).
       FD F-ETATANO-S
           RECORDING MODE IS F.
       01  FS-ETATANO-BUFFER     PIC X(80).
      *========================
       WORKING-STORAGE SECTION.
      *========================
      * DEB WS F-ETATCLI
       77  WS-FS-ETATCLI         PIC X(2).
       77  WS-FS-ETATANO         PIC X(2).
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
       01  WS-LETAT-ASTER    PIC X(78) VALUE ALL '*'.
       01  WS-LETAT-ENT.
           05  FILLER        PIC X(21) VALUE '* NUMERO DE COMPTE : '.
           05  WS-LETAT-AST-CPTE-ED    PIC 9(10).
           05  FILLER        PIC X(32) VALUE ALL SPACE.
           05  FILLER        PIC X(3)  VALUE 'LE '.
           05  WS-LETAT-AST-DATE-ED.
               10  WS-DD     PIC X(2).
               10  FILLER    PIC X     VALUE '/'.
               10  WS-MM     PIC X(2).
               10  FILLER    PIC X     VALUE '/'.
               10  WS-YYYY   PIC X(4).
           05  FILLER        PIC X(2)  VALUE ' *'.
       01  WS-LETAT-TITRE.
           05  FILLER        PIC X(9)  VALUE '* LIBELLE'.
           05  FILLER        PIC X(34) VALUE ALL SPACE.
           05  FILLER        PIC X(18) VALUE '*      DEBIT     *'.
           05  FILLER        PIC X(20) VALUE '     CREDIT     *'.
       01  WS-LETAT-DETAIL.
           05  FILLER        PIC X(2)  VALUE '* '.
           05  WS-LETAT-DET-MVT-ED     PIC X(13).
           05  FILLER        PIC X(28) VALUE ALL SPACE.
           05  FILLER        PIC X(5)  VALUE '*'.
           05  WS-LETAT-DET-MTDB-ED    PIC ZZZZZZZ9,99 BLANK WHEN ZERO.
           05  FILLER        PIC X(6)  VALUE ' * '.
           05  WS-LETAT-DET-MTCR-ED    PIC ZZZZZZZ9,99 BLANK WHEN ZERO.
           05  FILLER        PIC X(2)  VALUE ' *'.
       01  WS-LETAT-TOTAL.
           05  FILLER        PIC X(7)  VALUE '* TOTAL'.
           05  FILLER        PIC X(36) VALUE ALL SPACE.
           05  FILLER        PIC X(3)  VALUE '*'.
           05  WS-LETAT-TOT-MTDB-ED    PIC ZZZZZZZZZ9,99
               BLANK WHEN ZERO.
           05  FILLER        PIC X(4)  VALUE ' *  '.
           05  WS-LETAT-TOT-MTCR-ED    PIC ZZZZZZZZZ9,99
               BLANK WHEN ZERO.
           05  FILLER        PIC X(2)  VALUE ' *'.
      * FIN DU WS ETAT DES OPERATIONS
      * DEB WS ETAT DES ERREURS
       01  WS-LANO-L1.
           05  FILLER        PIC X(1)  VALUE '*'.
           05  FILLER        PIC X(53) VALUE ALL '-'.
           05  FILLER        PIC X(1)  VALUE '*'.
       01  WS-LANO-TITRE.
           05  FILLER        PIC X(55) VALUE '|  NR COMPTE  |  CODE MOUV
      -        'EMENT  |    MONTANT         |'.
       01  WS-LANO-L3.
           05  FILLER        PIC X(1)  VALUE '|'.
           05  FILLER        PIC X(53) VALUE ALL '-'.
           05  FILLER        PIC X(1)  VALUE '|'.
       01  WS-LANO-DETAIL.
           05  FILLER        PIC X(2)  VALUE '|'.
           05  WS-LANO-DET-CPT-ED      PIC 9(10).
           05  FILLER        PIC X(11) VALUE '  |'.
           05  WS-LANO-DET-MVT-ED      PIC X.
           05  FILLER        PIC X(16) VALUE '         |'.
           05  WS-LANO-DET-MT-ED       PIC ZZZZZZZ9,99 BLANK WHEN ZERO.
           05  FILLER        PIC X(4)  VALUE '   |'.
       01  WS-LANO-TOTAL.
           05  FILLER        PIC X(40) VALUE '| MONTANT TOTAL DES ANOMAL
      -        'IES    |      '.
           05  WS-LANO-TOT-MT-ED       PIC ZZZZZZZ9,99.
           05  FILLER         PIC X(4) VALUE '   |'.
      * FIN WS ETAT DES ERREURS
      * DEB WS COMPTE RENDU D'EXECUTION
       01  WS-LCRE-ASTER     PIC X(45) VALUE ALL '*'.
       01  WS-LCRE-TITRE     PIC X(45) VALUE '*    COMPTE RENDU D''EXECU
      -       'TION (ARIO211)     *'.
       01  WS-LCRE-DETAIL.
           05  FILLER        PIC X(3)  VALUE '*'.
           05  WS-LCRE-DET-LIB-ED      PIC X(28).
           05  FILLER        PIC X(6)  VALUE '  :   '.
           05 WS-LCRE-DET-TOT-ED       PIC ZZZ.
           05  FILLER        PIC X(5)  VALUE '    *'.
      * FIN WS COMPTE RENDU D'EXECUTION
      * DEB WS CALC
      *   CUM DE ETAT CLIENT
       77  WS-NB-DEBIT       PIC 9(10)V99 VALUE ZERO.
       77  WS-NB-CREDIT      PIC 9(10)V99 VALUE ZERO.
      *    CUM DE ETAT ANO
       77  WS-NB-ANO         PIC 9(10)V99 VALUE ZERO.
      *    CUM DE RENDU EXEC
       77  WS-NB-CLIENT      PIC 9(3)     VALUE ZERO.
       77  WS-NB-MVT         PIC 9(3)     VALUE ZERO.
       77  WS-NB-MVT-ERR     PIC 9(3)     VALUE ZERO.
       77  WS-NB-RETRAIT     PIC 9(3)     VALUE ZERO.
       77  WS-NB-CB          PIC 9(3)     VALUE ZERO.
       77  WS-NB-DEPOT       PIC 9(3)     VALUE ZERO.
      * FIN WS CALC
      * DEB TRANSFORMATION DATE US TO EU DATE
       01  WS-DATE-US.
           05  WS-YYYY   PIC 9(4).
           05  WS-MM     PIC 9(2).
           05  WS-DD     PIC 9(2).
      * FIN TRANSFORMATION DATE
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
           PERFORM  6030-READ-F-MVTS-E-DEB
              THRU  6030-READ-F-MVTS-E-FIN.
      *
           IF WS-FS-MVTS-E = '10'
              DISPLAY 'F-MVTS-E VIDE'
           END-IF.
      *
           PERFORM  6010-OPEN-F-ETAT-CLI-S-DEB
              THRU  6010-OPEN-F-ETAT-CLI-S-FIN.
      *
           PERFORM  6020-OPEN-F-ETAT-ANO-S-DEB
              THRU  6020-OPEN-F-ETAT-ANO-S-FIN.
      * SET LA DATE DU JOURS
           PERFORM  7060-CALC-DATE-DEB
              THRU  7060-CALC-DATE-FIN.
      *
      *
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
           PERFORM  7050-CALC-TOT-MVT-DEB
              THRU  7050-CALC-TOT-MVT-FIN.
      * SI ANO > 0 WRITE FOOTER OF ANO FICHIER
           IF WS-NB-MVT-ERR > 0
              PERFORM  8010-FOOTER-ANO-DEB
                 THRU  8010-FOOTER-ANO-FIN
           END-IF
      * CLOSE DES FICHIER I/O
           PERFORM  6060-CLOSE-F-MVTS-E-DEB
              THRU  6060-CLOSE-F-MVTS-E-FIN.
      *
           PERFORM  6070-CLOSE-F-ETAT-CLI-S-DEB
              THRU  6070-CLOSE-F-ETAT-CLI-S-FIN.
      *
           PERFORM  6080-CLOSE-F-ETAT-ANO-S-DEB
              THRU  6080-CLOSE-F-ETAT-ANO-S-FIN.
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
             UNTIL  WS-FS-MVTS-E = '10' OR WS
      -                    -MVTS-CPTE NOT = WS-LETAT-AST-CPTE-ED.
      *-------------------------------------------
      * DROITE
      *---------------------
           IF WS-NB-DEBIT > 0 OR WS-NB-CREDIT > 0
      *
              PERFORM  8070-FOOTER-ETAT-CLI-DEB
                 THRU  8070-FOOTER-ETAT-CLI-FIN
      *
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
            IF (WS-MVTS-CODE = 'R' OR WS-MVTS-CODE = 'C'
                     OR WS-MVTS-CODE = 'D') AND
                           (WS-NB-DEBIT = 0 AND WS-NB-CREDIT = 0)
      *EDITE L'HEADER DE ETAT CLI SI CODE MVT VALIDE
               PERFORM 8030-HEADER-ETAT-CLI-DEB
                  THRU 8030-HEADER-ETAT-CLI-FIN
            END-IF.
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
            PERFORM 6030-READ-F-MVTS-E-DEB
               THRU 6030-READ-F-MVTS-E-FIN.
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
            PERFORM 8040-LIGNE-CLI-DEB
               THRU 8040-LIGNE-CLI-FIN.
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
            PERFORM 8040-LIGNE-CLI-DEB
               THRU 8040-LIGNE-CLI-FIN.
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
            PERFORM 8040-LIGNE-CLI-DEB
               THRU 8040-LIGNE-CLI-FIN.
      *
       3020-TRT-CODE-D-FIN.
            EXIT.
      *-------------------------------------
      *EXECUTE LE CALCULE ET L EDITION CODE FAUX
      *-----------------------------------------
       3030-TRT-CODE-OTHER-DEB.
      *
            IF WS-NB-MVT-ERR = 0
               PERFORM 8000-HEADER-ANO-DEB
                  THRU 8000-HEADER-ANO-FIN
            END-IF.
      *
            PERFORM 7040-CALC-OTHER-DEB
               THRU 7040-CALC-OTHER-FIN.
      *
            PERFORM 8020-LIGNE-ANO-DEB
               THRU 8020-LIGNE-ANO-FIN.
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
      *
       6010-OPEN-F-ETAT-CLI-S-DEB.
            OPEN OUTPUT F-ETATCLI-S
            IF WS-FS-ETATCLI NOT = '00'
               DISPLAY 'PROBLEME D''OUVERTURE FICHIER F-ETATCLI'
               DISPLAY 'VALEUR DU FS= ' WS-FS-ETATCLI
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
      *
            END-IF.
       6010-OPEN-F-ETAT-CLI-S-FIN.
            EXIT.
      *
       6020-OPEN-F-ETAT-ANO-S-DEB.
            OPEN OUTPUT F-ETATANO-S
            IF WS-FS-ETATANO NOT = '00'
               DISPLAY 'PROBLEME D''OUVERTURE FICHIER F-ETATANO'
               DISPLAY 'VALEUR DU FS= ' WS-FS-ETATANO
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6020-OPEN-F-ETAT-ANO-S-FIN.
            EXIT.
      *
      *READ LE FICHIER F MVTS E
       6030-READ-F-MVTS-E-DEB.
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
       6030-READ-F-MVTS-E-FIN.
            EXIT.
      *ECRITURE ETAT ANO
       6040-WRITE-ETAT-ANO-DEB.
            WRITE FS-ETATANO-BUFFER.
            IF WS-FS-ETATANO NOT = '00'
               DISPLAY 'PROBLEMME ECRITURE FICHIER F-ETAT-ANO-S'
               DISPLAY 'VALEUR DU FS= ' WS-FS-ETATANO
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6040-WRITE-ETAT-ANO-FIN.
            EXIT.
      *ECRITURE ETAT CLI
       6050-WRITE-ETAT-CLI-DEB.
            WRITE FS-ETATCLI-BUFFER.
            IF WS-FS-ETATCLI NOT = '00'
               DISPLAY 'PROBLEMME ECRITURE FICHIER F-ETAT-CLI-S'
               DISPLAY 'VALEUR DU FS= ' WS-FS-ETATCLI
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6050-WRITE-ETAT-CLI-FIN.
            EXIT.
      *ECRITURE ETAT CLI AVEC SAUT DE PAGE
       6090-WRITE-ETAT-CLI-PAGE-DEB.
      *
            WRITE FS-ETATCLI-BUFFER AFTER PAGE.
            IF WS-FS-ETATCLI NOT = '00'
               DISPLAY 'PROBLEMME ECRITURE FICHIER F-ETAT-CLI-S'
               DISPLAY 'VALEUR DU FS= ' WS-FS-ETATCLI
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6090-WRITE-ETAT-CLI-PAGE-FIN.
            EXIT.
      *ECRITURE ETAT ANO AVEC SAUT DE PAGE
       6100-WRITE-ETAT-ANO-PAGE-DEB.
      *
            WRITE FS-ETATANO-BUFFER AFTER PAGE.
            IF NOT (WS-FS-ETATANO = '00' OR '10')
               DISPLAY 'PROBLEMME ECRITURE FICHIER F-ETAT-ANO$S'
               DISPLAY 'VALEUR DU FS= ' WS-FS-ETATANO
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6100-WRITE-ETAT-ANO-PAGE-FIN.
            EXIT.
      *-------------------------------------
      *6020 EXECUTE LA FERMTURE SUR FMVTS-E
      *------------------------------------
       6060-CLOSE-F-MVTS-E-DEB.
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
       6060-CLOSE-F-MVTS-E-FIN.
            EXIT.
      *
       6070-CLOSE-F-ETAT-CLI-S-DEB.
      *
            CLOSE F-ETATCLI-S.
            IF WS-FS-ETATCLI NOT = '00'
               DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETATCLI-S'
               DISPLAY 'VALEUR DU FS= ' WS-FS-ETATCLI
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
      *
            END-IF.
       6070-CLOSE-F-ETAT-CLI-S-FIN.
            EXIT.
      *
       6080-CLOSE-F-ETAT-ANO-S-DEB.
      *
            CLOSE F-ETATANO-S.
            IF WS-FS-ETATANO NOT = '00'
               DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETATCLI-S'
               DISPLAY 'VALEUR DU FS= ' WS-FS-ETATANO
      *
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
      *
       6080-CLOSE-F-ETAT-ANO-S-FIN.
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
            MOVE  ZERO TO WS-NB-DEBIT WS-NB-CREDIT.
            MOVE  WS-MVTS-CPTE   TO WS-LETAT-AST-CPTE-ED.
            ADD   1              TO WS-NB-CLIENT.
      *
       7000-TRT-VAR-CLIENT-FIN.
            EXIT.
      *-------------------------------------
      *7010 ADDITIONNE LE CUMULE DES RETRAITS
      *------------------------------------
       7010-CALC-R-DEB.
      *
            ADD  WS-MVTS-MT      TO WS-NB-DEBIT.
            ADD  1               TO WS-NB-RETRAIT.
            MOVE 'RETRAIT DAB'   TO WS-LETAT-DET-MVT-ED.
            MOVE WS-MVTS-MT      TO WS-LETAT-DET-MTDB-ED.
            MOVE ZERO            TO WS-LETAT-DET-MTCR-ED.
      *
       7010-CALC-R-FIN.
            EXIT.
      *-------------------------------------
      *7020 ADDITIONNE LE CUMULE DES CARTE BLEU
      *------------------------------------
       7020-CALC-C-DEB.
      *
            ADD  WS-MVTS-MT      TO WS-NB-DEBIT.
            ADD  1               TO WS-NB-CB.
            MOVE 'CARTE BLEUE'   TO WS-LETAT-DET-MVT-ED.
            MOVE WS-MVTS-MT      TO WS-LETAT-DET-MTDB-ED.
            MOVE ZERO            TO WS-LETAT-DET-MTCR-ED.
      *
       7020-CALC-C-FIN.
            EXIT.
      *-------------------------------------
      *7030 ADDITIONNE LE CUMULE DEPOT GUICHET
      *------------------------------------
       7030-CALC-D-DEB.
      *
            ADD  WS-MVTS-MT      TO WS-NB-CREDIT.
            ADD  1               TO WS-NB-DEPOT.
            MOVE 'DEPOT GUICHET' TO WS-LETAT-DET-MVT-ED.
            MOVE WS-MVTS-MT      TO WS-LETAT-DET-MTCR-ED.
            MOVE ZERO            TO WS-LETAT-DET-MTDB-ED.
      *
       7030-CALC-D-FIN.
            EXIT.
      *-------------------------------------
      *7040 ADDITIONNE LE CUMULE DES MVT FAUX
      *------------------------------------
       7040-CALC-OTHER-DEB.
      *
            ADD  WS-MVTS-MT    TO WS-NB-ANO.
            ADD  1             TO WS-NB-MVT-ERR.
      *
       7040-CALC-OTHER-FIN.
            EXIT.
      *-------------------------------------
      *7050 FAIT LE TOTAL MVT
      *------------------------------------
       7050-CALC-TOT-MVT-DEB.
      *
            COMPUTE WS-NB-MVT = WS-NB-MVT-ERR + WS-NB-RETRAIT +
                                WS-NB-CB + WS-NB-DEPOT.
      *
       7050-CALC-TOT-MVT-FIN.
            EXIT.
      * TRANSFORME LA DATE US EN DATE EU
       7060-CALC-DATE-DEB.
            ACCEPT WS-DATE-US FROM DATE YYYYMMDD.
            MOVE WS-DD   OF WS-DATE-US
              TO WS-DD   OF WS-LETAT-AST-DATE-ED.
            MOVE WS-MM   OF WS-DATE-US
              TO WS-MM   OF WS-LETAT-AST-DATE-ED.
            MOVE WS-YYYY OF WS-DATE-US
              TO WS-YYYY OF WS-LETAT-AST-DATE-ED.
       7060-CALC-DATE-FIN.
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
       8000-HEADER-ANO-DEB.
      *
            MOVE WS-LANO-L1  TO FS-ETATANO-BUFFER.
            PERFORM 6100-WRITE-ETAT-ANO-PAGE-DEB
               THRU 6100-WRITE-ETAT-ANO-PAGE-FIN.
      *
            MOVE WS-LANO-TITRE  TO FS-ETATANO-BUFFER.
            PERFORM 6040-WRITE-ETAT-ANO-DEB
               THRU 6040-WRITE-ETAT-ANO-FIN.
      *
            MOVE WS-LANO-L3  TO FS-ETATANO-BUFFER.
            PERFORM 6040-WRITE-ETAT-ANO-DEB
               THRU 6040-WRITE-ETAT-ANO-FIN.
      *
       8000-HEADER-ANO-FIN.
            EXIT.
      *-------------------------------------
      *8010 PIED DE PAGE ETAT ANO
      *------------------------------------
      *
       8010-FOOTER-ANO-DEB.
      *
            MOVE WS-NB-ANO      TO WS-LANO-TOT-MT-ED.
            MOVE WS-LANO-L3     TO FS-ETATANO-BUFFER.
            PERFORM 6040-WRITE-ETAT-ANO-DEB
               THRU 6040-WRITE-ETAT-ANO-FIN.
      *
            MOVE WS-LANO-TOTAL  TO FS-ETATANO-BUFFER.
            PERFORM 6040-WRITE-ETAT-ANO-DEB
               THRU 6040-WRITE-ETAT-ANO-FIN.
      *
            MOVE WS-LANO-L1     TO FS-ETATANO-BUFFER.
            PERFORM 6040-WRITE-ETAT-ANO-DEB
               THRU 6040-WRITE-ETAT-ANO-FIN.
      *
       8010-FOOTER-ANO-FIN.
            EXIT.
      * LIGNE DETAIL DU FICHIER ANO
       8020-LIGNE-ANO-DEB.
      *
            MOVE WS-MVTS-CPTE    TO WS-LANO-DET-CPT-ED.
            MOVE WS-MVTS-CODE    TO WS-LANO-DET-MVT-ED.
            MOVE WS-MVTS-MT      TO WS-LANO-DET-MT-ED.
            MOVE WS-LANO-DETAIL  TO FS-ETATANO-BUFFER.
            PERFORM 6040-WRITE-ETAT-ANO-DEB
               THRU 6040-WRITE-ETAT-ANO-FIN.
      *
       8020-LIGNE-ANO-FIN.
            EXIT.
       8030-HEADER-ETAT-CLI-DEB.
      *
            MOVE WS-LETAT-ASTER  TO FS-ETATCLI-BUFFER.
            PERFORM 6090-WRITE-ETAT-CLI-PAGE-DEB
               THRU 6090-WRITE-ETAT-CLI-PAGE-FIN.
      *
            MOVE WS-LETAT-ENT    TO FS-ETATCLI-BUFFER.
            PERFORM 6050-WRITE-ETAT-CLI-DEB
               THRU 6050-WRITE-ETAT-CLI-FIN.
      *
            MOVE WS-LETAT-ASTER  TO FS-ETATCLI-BUFFER.
            PERFORM 6050-WRITE-ETAT-CLI-DEB
               THRU 6050-WRITE-ETAT-CLI-FIN.
      *
            MOVE WS-LETAT-TITRE  TO FS-ETATCLI-BUFFER.
            PERFORM 6050-WRITE-ETAT-CLI-DEB
               THRU 6050-WRITE-ETAT-CLI-FIN.
      *
            MOVE WS-LETAT-ASTER  TO FS-ETATCLI-BUFFER.
            PERFORM 6050-WRITE-ETAT-CLI-DEB
               THRU 6050-WRITE-ETAT-CLI-FIN.
       8030-HEADER-ETAT-CLI-FIN.
            EXIT.
      *
       8040-LIGNE-CLI-DEB.
            MOVE WS-LETAT-DETAIL TO FS-ETATCLI-BUFFER.
            PERFORM 6050-WRITE-ETAT-CLI-DEB
               THRU 6050-WRITE-ETAT-CLI-FIN.
       8040-LIGNE-CLI-FIN.
            EXIT.
      *
       8070-FOOTER-ETAT-CLI-DEB.
            MOVE WS-LETAT-ASTER  TO FS-ETATCLI-BUFFER.
            PERFORM 6050-WRITE-ETAT-CLI-DEB
               THRU 6050-WRITE-ETAT-CLI-FIN.
      *
            MOVE WS-NB-DEBIT     TO WS-LETAT-TOT-MTDB-ED
            MOVE WS-NB-CREDIT    TO WS-LETAT-TOT-MTCR-ED
            MOVE WS-LETAT-TOTAL  TO FS-ETATCLI-BUFFER.
            PERFORM 6050-WRITE-ETAT-CLI-DEB
               THRU 6050-WRITE-ETAT-CLI-FIN.
      *
            MOVE WS-LETAT-ASTER  TO FS-ETATCLI-BUFFER.
            PERFORM 6050-WRITE-ETAT-CLI-DEB
               THRU 6050-WRITE-ETAT-CLI-FIN.
       8070-FOOTER-ETAT-CLI-FIN.
            EXIT.
      *-------------------------------------
      *8010 AFFICHE LE COMPTE RENDU D EXECUTION
      *------------------------------------
       8999-STATISTIQUES-DEB.
      *
            DISPLAY '************************************************'.
            DISPLAY '*      STATISTIQUE DU PROGRAMME ARIO211        *'.
            DISPLAY '*      ================================        *'.
            DISPLAY '************************************************'.
      *
            DISPLAY  WS-LCRE-ASTER.
            DISPLAY  WS-LCRE-TITRE.
            DISPLAY  WS-LCRE-ASTER.
            MOVE 'NOMBRE DE CLIENTS'    TO WS-LCRE-DET-LIB-ED.
            MOVE     WS-NB-CLIENT       TO WS-LCRE-DET-TOT-ED.
            DISPLAY  WS-LCRE-DETAIL.
            MOVE 'NOMBRE DE MOUVEMENTS'         TO WS-LCRE-DET-LIB-ED.
            MOVE     WS-NB-MVT                  TO WS-LCRE-DET-TOT-ED.
            DISPLAY  WS-LCRE-DETAIL.
            MOVE 'NOMBRE DE MOUVEMENTS ERRONES' TO WS-LCRE-DET-LIB-ED.
            MOVE     WS-NB-MVT-ERR              TO WS-LCRE-DET-TOT-ED.
            DISPLAY  WS-LCRE-DETAIL.
            MOVE 'NOMBRE DE RETRAITS'           TO WS-LCRE-DET-LIB-ED.
            MOVE     WS-NB-RETRAIT              TO WS-LCRE-DET-TOT-ED.
            DISPLAY  WS-LCRE-DETAIL.
            MOVE 'NOMBRE DE CARTES BLEUES'      TO WS-LCRE-DET-LIB-ED.
            MOVE     WS-NB-CB                   TO WS-LCRE-DET-TOT-ED.
            DISPLAY  WS-LCRE-DETAIL.
            MOVE 'NOMBRE DE DEPOTS'             TO WS-LCRE-DET-LIB-ED.
            MOVE     WS-NB-DEPOT                TO WS-LCRE-DET-TOT-ED.
            DISPLAY  WS-LCRE-DETAIL.
            DISPLAY  WS-LCRE-ASTER.
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
            DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO211         *'.
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
