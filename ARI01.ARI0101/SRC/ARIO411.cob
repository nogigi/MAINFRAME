      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO411                                   *
      *  NOM DU REDACTEUR : BAUDELET                                  *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 15/04/2024                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *MISE A JOUR DU FICHIER DES COMPTES CLIENTS A PARTIR DES        *
      *MOUVEMENTS BANCAIRE.(F-MVTS) SUR LE FICHIER (F-CPTES EN IO)    *
      *EDITION DES OPERATIONS BANCAIRES  (ETAT-CLI-S)                 *
      *EDITION DES MOUVEMENT INVALIDE (F-ANO)                         *
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
       PROGRAM-ID.      ARIO411.
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
      *                      F-MVTS-E: FICHIER DES MVT EN ENTRER
      *                      -------------------------------------------
           SELECT  F-MVTS-E            ASSIGN TO INP001
                   FILE STATUS         IS WS-FS-MVTS-E.
      *                      -------------------------------------------
      *                      -------------------------------------------
      *                      F-CPTE-ES: FICHIER DES COMPTE EN ENTRER
      *                                 IO
      *                      -------------------------------------------
           SELECT  F-CPTE-ES           ASSIGN TO IO001
                   ORGANIZATION        IS INDEXED
                   ACCESS MODE         IS RANDOM
                   RECORD KEY          IS FS-KEY
                   FILE STATUS         IS WS-FS-CPTE-ES.
      *                      -------------------------------------------
      *                      -------------------------------------------
      *                      F-ETAT-CLI-S: SYSOUT DES ETAT CLIENT EN S
      *                      -------------------------------------------
           SELECT  F-ETAT-CLI-S        ASSIGN TO ETATCLI
                   FILE STATUS         IS WS-FS-ETAT-CLI-S.
      *                      -------------------------------------------
      *                      -------------------------------------------
      *                      F-ETAT-ANO : SYSOUT DES ANOMALIE MVTS
      *                      -------------------------------------------
           SELECT  F-ETAT-ANO-S        ASSIGN TO ETATANO
                   FILE STATUS         IS WS-FS-ETAT-ANO-S.
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
      *FICHIER MVTS E
       FD  F-MVTS-E
           RECORDING MODE IS F.
       01  FS-ENRG-F-MVTS-E      PIC X(50).
      *FICHIER CPTE ES
       FD  F-CPTE-ES
           RECORD CONTAINS 50 CHARACTERS.
       01  FS-BUFF-F-CPTE-ES.
           05  FS-KEY            PIC X(10).
           05  FILLER            PIC X(40).
      *SYSOUT  ETAT CLI SORTIE
       FD  F-ETAT-CLI-S
           RECORDING MODE IS F.
       01  FS-BUFF-F-ETAT-CLI-S  PIC X(80).
      *SYSOUT  ETAT ANO SORTIE
       FD  F-ETAT-ANO-S
           RECORDING MODE IS F.
       01  FS-BUFF-F-ETAT-ANO-S  PIC X(80).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
       COPY TP4LEDIT.
       COPY TP4MVTS.
       COPY TP4CPTES.
      *MY WS SECTION
      *FILE STATUS
       01  WS-FS-MVTS-E     PIC X(2).
           88 MVTS-OK                            VALUE '00'.
           88 MVTS-FIN                           VALUE '10'.
       01  WS-FS-CPTE-ES    PIC X(2).
           88 CPTES-OK                           VALUE '00'.
           88 CPTES-INTROUVABLE                  VALUE '23'.
       01  WS-FS-ETAT-CLI-S PIC X(2).
           88 CLI-OK                             VALUE '00'.
       01  WS-FS-ETAT-ANO-S PIC X(2).
           88 ANO-OK                             VALUE '00'.
      *VARIABLE DE CALULE CUMULE
       01  WS-CUM-CRED      PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-CUM-DEB       PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-RENDU-CUM-ANO PIC S9(9)V99  COMP-3 VALUE ZERO.
       01  WS-NEW-SOLD      PIC S9(11)V99 COMP-3 VALUE ZERO.
      *COMPTEUR RENDU EXEC
       01  WS-NB-CLI        PIC S9(4)     COMP   VALUE ZERO.
       01  WS-NB-CLI-NEW    PIC S9(4)     COMP   VALUE ZERO.
       01  WS-NB-CLI-STAND  PIC S9(4)     COMP   VALUE ZERO.
       01  WS-NB-CLI-CLOSE  PIC S9(4)     COMP   VALUE ZERO.
       01  WS-NB-MVT        PIC S9(4)     COMP   VALUE ZERO.
       01  WS-NB-ANO        PIC S9(4)     COMP   VALUE ZERO.
       01  WS-NB-RETRAIT    PIC S9(4)     COMP   VALUE ZERO.
       01  WS-NB-CB         PIC S9(4)     COMP   VALUE ZERO.
       01  WS-NB-DEP        PIC S9(4)     COMP   VALUE ZERO.
       01  WS-NB-CLOSE      PIC S9(4)     COMP   VALUE ZERO.
      *GESTION PAGE
       01  WS-FLAG-CLOSE    PIC S9(4)     COMP   VALUE ZERO.
       01  WS-LIGNE-COUNT   PIC S9(4)     COMP   VALUE ZERO.
       01  WS-PAGE-COUNT    PIC S9(4)     COMP   VALUE ZERO.
      *GESTION DATE
       01  WS-DATE-J.
           05  WS-YYYY.
               10 WS-SS     PIC 9(2).
               10 WS-AA     PIC 9(2).
           05  WS-MM        PIC 9(2).
           05  WS-DD        PIC 9(2).
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
      *GAUCHE
      *    OPEN FICHIER ENTRER
           PERFORM 6000-OPEN-F-MVTS-E-DEB
              THRU 6000-OPEN-F-MVTS-E-FIN.
           PERFORM 6010-OPEN-F-CPTE-ES-DEB
              THRU 6010-OPEN-F-CPTE-ES-FIN.
      *    OPEN FICHIER SORTIE
           PERFORM 6030-OPEN-F-ETAT-CLI-S-DEB
              THRU 6030-OPEN-F-ETAT-CLI-S-FIN.
           PERFORM 6040-OPEN-F-ETAT-ANO-S-DEB
              THRU 6040-OPEN-F-ETAT-ANO-S-FIN.
      *    READ FILE
           PERFORM 6050-READ-F-MVTS-E-DEB
              THRU 6050-READ-F-MVTS-E-FIN.
           IF MVTS-FIN
              DISPLAY 'F-MVTS VIDE'
           END-IF.
      *    INIT DATE
           PERFORM 7000-INIT-DATE-DEB
              THRU 7000-INIT-DATE-FIN.
           PERFORM 8140-FRONT-CLI-DEB
              THRU 8140-FRONT-CLI-FIN.
           PERFORM 8150-FRONT-ANO-DEB
              THRU 8150-FRONT-ANO-FIN.
      *ITERATIVE
           PERFORM 1000-TRT-CPT-DEB
              THRU 1000-TRT-CPT-FIN
             UNTIL MVTS-FIN.
      *DROITE
      *    CALC COMPTE RENDU EXECUTION NB CLIEN ET NB MVT TOT
           PERFORM 7110-CALC-RENDU-EXEC-DEB
              THRU 7110-CALC-RENDU-EXEC-FIN.
      *    RENDU ANO SANS ANO DETECTER
           IF WS-RENDU-CUM-ANO = 0
              PERFORM 8100-LANO-OK-DEB
                 THRU 8100-LANO-OK-FIN
           ELSE
              PERFORM 7230-PREP-FOOTER-ANO-DEB
                 THRU 7230-PREP-FOOTER-ANO-DEB
              PERFORM 8110-FOOTER-ANO-DEB
                 THRU 8110-FOOTER-ANO-FIN
           END-IF.
      *    CLOSE FICHIER ENTRER
           PERFORM 6070-CLOSE-F-MVTS-E-DEB
              THRU 6070-CLOSE-F-MVTS-E-FIN.
           PERFORM 6080-CLOSE-F-CPTE-ES-DEB
              THRU 6080-CLOSE-F-CPTE-ES-FIN.
      *    CLOSE FICHIER SORTIE
           PERFORM 6100-CLOSE-F-ETAT-CLI-S-DEB
              THRU 6100-CLOSE-F-ETAT-CLI-S-FIN.
           PERFORM 6110-CLOSE-F-ETAT-ANO-S-DEB
              THRU 6110-CLOSE-F-ETAT-ANO-S-FIN.
           PERFORM 8999-STATISTIQUES-DEB
              THRU 8999-STATISTIQUES-FIN.
      *    FIN PROGRAME
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *
       0000-PROGRAMME-FIN.
           STOP RUN.
       1000-TRT-CPT-DEB.
      *GAUCHE
           PERFORM 6060-READ-F-CPTE-ES-DEB
              THRU 6060-READ-F-CPTE-ES-FIN.
      *AS
           IF CPTES-INTROUVABLE
              PERFORM 2010-TRT-CPT-NEW-DEB
                 THRU 2010-TRT-CPT-NEW-FIN
           ELSE
              PERFORM 2000-TRT-CPT-EXIST-DEB
                 THRU 2000-TRT-CPT-EXIST-FIN
           END-IF.
      *DROITE
       1000-TRT-CPT-FIN.
           EXIT.
      * GESTION DES CPT EXISTANT ET MISE A JOUR DANS FICHIER CPTES
       2000-TRT-CPT-EXIST-DEB.
      *GAUCHE
           PERFORM 7090-INIT-CPT-EXIST-DEB
              THRU 7090-INIT-CPT-EXIST-FIN.
      *ITERATIVE
           PERFORM 3000-TRT-AVEC-CPTE-DEB
              THRU 3000-TRT-AVEC-CPTE-FIN
             UNTIL (WS-MVTS-CPTE NOT = FS-KEY) OR MVTS-FIN.
      *DROITE
           PERFORM 7080-SOLD-STAND-DEB
              THRU 7080-SOLD-STAND-FIN.
           IF WS-FLAG-CLOSE = 1
              PERFORM 7300-COUNT-CLOSE-DEB
                 THRU 7300-COUNT-CLOSE-FIN
              PERFORM 6170-DELET-F-CPTE-ES-DEB
                 THRU 6170-DELET-F-CPTE-ES-FIN
              PERFORM 7240-PREP-ETAT-CLI-CLOSE-DEB
                 THRU 7240-PREP-ETAT-CLI-CLOSE-FIN
           ELSE
              PERFORM 7270-PREP-REWRITE-DEB
                 THRU 7270-PREP-REWRITE-FIN
              PERFORM 6170-REWRITE-F-CPTE-ES-DEB
                 THRU 6170-REWRITE-F-CPTE-ES-FIN
           END-IF
           IF WS-CUM-DEB > 0 OR WS-CUM-CRED > 0
              PERFORM 7220-PREP-NEW-SOLD-DEB
                 THRU 7220-PREP-NEW-SOLD-FIN
              PERFORM 8080-FOOTER-ETAT-CLI-DEB
                 THRU 8080-FOOTER-ETAT-CLI-FIN
           END-IF.
       2000-TRT-CPT-EXIST-FIN.
           EXIT.
      *GESTION DES CPT INEXISTANT ET CREATION DANS FICHIER CPTES
       2010-TRT-CPT-NEW-DEB.
      *GAUCHE
           PERFORM 7120-INIT-CPT-NEW-DEB
              THRU 7120-INIT-CPT-NEW-FIN.
      *ITERATIVE
           PERFORM 3010-TRT-SANS-CPTE-DEB
              THRU 3010-TRT-SANS-CPTE-FIN
             UNTIL (WS-MVTS-CPTE NOT = FS-KEY) OR MVTS-FIN.
      *DROITE
           PERFORM 7100-SOLD-NEW-DEB
              THRU 7100-SOLD-NEW-FIN.
           IF WS-CUM-DEB > 0 OR WS-CUM-CRED > 0
              IF WS-FLAG-CLOSE = 1
                 PERFORM 7240-PREP-ETAT-CLI-CLOSE-DEB
                    THRU 7240-PREP-ETAT-CLI-CLOSE-FIN
                 PERFORM 7220-PREP-NEW-SOLD-DEB
                    THRU 7220-PREP-NEW-SOLD-FIN
                 PERFORM 8080-FOOTER-ETAT-CLI-DEB
                    THRU 8080-FOOTER-ETAT-CLI-FIN
              ELSE
                 PERFORM 7220-PREP-NEW-SOLD-DEB
                    THRU 7220-PREP-NEW-SOLD-FIN
                 PERFORM 8080-FOOTER-ETAT-CLI-DEB
                    THRU 8080-FOOTER-ETAT-CLI-FIN
                 PERFORM 7290-PREP-WRITE-DEB
                    THRU 7290-PREP-WRITE-FIN
                 PERFORM 6120-WRITE-F-CPTE-ES-DEB
                    THRU 6120-WRITE-F-CPTE-ES-FIN
              END-IF
           END-IF.
       2010-TRT-CPT-NEW-FIN.
           EXIT.
      * GESTION DES MOUVEMENT AVEC CPT EXISTANT
       3000-TRT-AVEC-CPTE-DEB.
      *GAUCHE
           IF (RETRAIT OR CB OR DEPOT)
              IF WS-LIGNE-COUNT = 5
      *FOOTER INTERMEDIAIRE
                 PERFORM 7260-PREP-INTER-SOLD-DEB
                    THRU 7260-PREP-INTER-SOLD-FIN
                 PERFORM 7080-SOLD-STAND-DEB
                    THRU 7080-SOLD-STAND-FIN
                 PERFORM 8080-FOOTER-ETAT-CLI-DEB
                    THRU 8080-FOOTER-ETAT-CLI-FIN
                 PERFORM 7200-PREP-HEADER-DEB
                    THRU 7200-PREP-HEADER-FIN
                 PERFORM 8020-HEADER-ETAT-CLI-DEB
                    THRU 8020-HEADER-ETAT-CLI-FIN
              ELSE IF (WS-LIGNE-COUNT = 0)
                 PERFORM 7200-PREP-HEADER-DEB
                    THRU 7200-PREP-HEADER-FIN
                 PERFORM 8020-HEADER-ETAT-CLI-DEB
                    THRU 8020-HEADER-ETAT-CLI-FIN
              END-IF
           END-IF.
      *AM
           EVALUATE TRUE
               WHEN RETRAIT
                    PERFORM 4000-TRT-CODE-R-DEB
                       THRU 4000-TRT-CODE-R-FIN
               WHEN CB
                    PERFORM 4010-TRT-CODE-C-DEB
                       THRU 4010-TRT-CODE-C-FIN
               WHEN DEPOT
                    PERFORM 4020-TRT-CODE-D-DEB
                       THRU 4020-TRT-CODE-D-FIN
               WHEN CLOTURE
                    PERFORM 4040-TRT-CLOSE-DEB
                       THRU 4040-TRT-CLOSE-FIN
               WHEN OTHER
                    PERFORM 4030-TRT-OTHER-DEB
                       THRU 4030-TRT-OTHER-FIN
           END-EVALUATE.
      *DROITE
           PERFORM 6050-READ-F-MVTS-E-DEB
              THRU 6050-READ-F-MVTS-E-FIN.
       3000-TRT-AVEC-CPTE-FIN.
           EXIT.
      * GESTION DES MOUVEMENT SANS COMPTE EXISTANT
       3010-TRT-SANS-CPTE-DEB.
      *GAUCHE
           PERFORM 7190-PREP-HEADER-NEW-DEB
              THRU 7190-PREP-HEADER-NEW-FIN.
           IF (RETRAIT OR CB OR DEPOT)
              IF WS-LIGNE-COUNT = 5
                 PERFORM 7260-PREP-INTER-SOLD-DEB
                    THRU 7260-PREP-INTER-SOLD-FIN
                 PERFORM 7100-SOLD-NEW-DEB
                    THRU 7100-SOLD-NEW-FIN
                 PERFORM 8080-FOOTER-ETAT-CLI-DEB
                    THRU 8080-FOOTER-ETAT-CLI-FIN
                 PERFORM 7200-PREP-HEADER-DEB
                    THRU 7200-PREP-HEADER-FIN
                 PERFORM 7190-PREP-HEADER-NEW-DEB
                    THRU 7190-PREP-HEADER-NEW-FIN
                 PERFORM 8020-HEADER-ETAT-CLI-DEB
                    THRU 8020-HEADER-ETAT-CLI-FIN
              ELSE IF (WS-LIGNE-COUNT = 0)
                 PERFORM 7280-PREP-DMAJ-DEB
                    THRU 7280-PREP-DMAJ-FIN
                 PERFORM 7200-PREP-HEADER-DEB
                    THRU 7200-PREP-HEADER-FIN
                 PERFORM 7190-PREP-HEADER-NEW-DEB
                    THRU 7190-PREP-HEADER-NEW-FIN
                 PERFORM 8020-HEADER-ETAT-CLI-DEB
                    THRU 8020-HEADER-ETAT-CLI-FIN
              END-IF
           END-IF.
      *AM
           EVALUATE TRUE
               WHEN RETRAIT
                    PERFORM 4000-TRT-CODE-R-DEB
                       THRU 4000-TRT-CODE-R-FIN
               WHEN CB
                    PERFORM 4010-TRT-CODE-C-DEB
                       THRU 4010-TRT-CODE-C-FIN
               WHEN DEPOT
                    PERFORM 4020-TRT-CODE-D-DEB
                       THRU 4020-TRT-CODE-D-FIN
               WHEN CLOTURE
                    PERFORM 4040-TRT-CLOSE-DEB
                       THRU 4040-TRT-CLOSE-FIN
               WHEN OTHER
                    PERFORM 4030-TRT-OTHER-DEB
                       THRU 4030-TRT-OTHER-FIN
           END-EVALUATE.
      *DROITE
           PERFORM 6050-READ-F-MVTS-E-DEB
              THRU 6050-READ-F-MVTS-E-FIN.
       3010-TRT-SANS-CPTE-FIN.
           EXIT.
      *    4000
      * GESTION CODE MVT ET CALCULE LES CUMULES EDITE LES F-ETAT-CLI
      * ET F-ETAT-ANO
       4000-TRT-CODE-R-DEB.
           PERFORM 7040-CALC-R-DEB
              THRU 7040-CALC-R-FIN.
           PERFORM 7210-PREP-DATE-MVT-DEB
              THRU 7210-PREP-DATE-MVT-FIN.
           PERFORM 8040-ETAT-CLI-DEB
              THRU 8040-ETAT-CLI-FIN.
       4000-TRT-CODE-R-FIN.
           EXIT.
       4010-TRT-CODE-C-DEB.
           PERFORM 7050-CALC-C-DEB
              THRU 7050-CALC-C-FIN.
           PERFORM 7210-PREP-DATE-MVT-DEB
              THRU 7210-PREP-DATE-MVT-FIN.
           PERFORM 8040-ETAT-CLI-DEB
              THRU 8040-ETAT-CLI-FIN.
       4010-TRT-CODE-C-FIN.
           EXIT.
       4020-TRT-CODE-D-DEB.
           PERFORM 7060-CALC-D-DEB
              THRU 7060-CALC-D-FIN.
           PERFORM 7210-PREP-DATE-MVT-DEB
              THRU 7210-PREP-DATE-MVT-FIN.
           PERFORM 8040-ETAT-CLI-DEB
              THRU 8040-ETAT-CLI-FIN.
       4020-TRT-CODE-D-FIN.
           EXIT.
       4030-TRT-OTHER-DEB.
           IF WS-NB-ANO = 0
              PERFORM 8060-HEADER-ANO-DEB
                 THRU 8060-HEADER-ANO-FIN
           END-IF.
           PERFORM 7070-CALC-OTHER-DEB
              THRU 7070-CALC-OTHER-FIN.
           PERFORM 8130-L-ETAT-ANO-S-DEB
              THRU 8130-L-ETAT-ANO-S-FIN.
       4030-TRT-OTHER-FIN.
           EXIT.
       4040-TRT-CLOSE-DEB.
           PERFORM 7250-CALC-CLOSE-DEB
              THRU 7250-CALC-CLOSE-FIN.
       4040-TRT-CLOSE-FIN.
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
       6000-OPEN-F-MVTS-E-DEB.
           OPEN INPUT F-MVTS-E.
           IF NOT MVTS-OK
              DISPLAY 'PROBLEME D''OUVERTURE FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FS= ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-OPEN-F-MVTS-E-FIN.
           EXIT.
       6010-OPEN-F-CPTE-ES-DEB.
           OPEN I-O F-CPTE-ES.
           IF NOT CPTES-OK
              DISPLAY 'PROBLEME D''OUVERTURE FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6010-OPEN-F-CPTE-ES-FIN.
           EXIT.
       6030-OPEN-F-ETAT-CLI-S-DEB.
           OPEN OUTPUT F-ETAT-CLI-S.
           IF NOT CLI-OK
              DISPLAY 'PROBLEME D''OUVERTURE FICHIER F-ETAT-CLI-S'
              DISPLAY 'VALEUR DU FS= ' WS-FS-ETAT-CLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6030-OPEN-F-ETAT-CLI-S-FIN.
           EXIT.
       6040-OPEN-F-ETAT-ANO-S-DEB.
           OPEN OUTPUT F-ETAT-ANO-S.
           IF NOT ANO-OK
              DISPLAY 'PROBLEME D''OUVERTURE FICHIER F-ETAT-ANO-S'
              DISPLAY 'VALEUR DU FS= ' WS-FS-ETAT-ANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-OPEN-F-ETAT-ANO-S-FIN.
           EXIT.
      * DEBUT DES READ
       6050-READ-F-MVTS-E-DEB.
           READ F-MVTS-E INTO WS-ENRG-F-MVTS.
           IF NOT (MVTS-OK OR MVTS-FIN)
              DISPLAY 'PROBLEMME LECTURE FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FS= ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6050-READ-F-MVTS-E-FIN.
           EXIT.
       6060-READ-F-CPTE-ES-DEB.
           MOVE WS-MVTS-CPTE               TO FS-KEY.
           READ F-CPTE-ES INTO WS-ENRG-F-CPTES.
           IF NOT (CPTES-OK OR CPTES-INTROUVABLE)
              DISPLAY 'PROBLEMME LECTURE FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6060-READ-F-CPTE-ES-FIN.
           EXIT.
      * DEBUT DES CLOSE
       6070-CLOSE-F-MVTS-E-DEB.
           CLOSE F-MVTS-E.
           IF NOT MVTS-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FS= ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6070-CLOSE-F-MVTS-E-FIN.
           EXIT.
       6080-CLOSE-F-CPTE-ES-DEB.
           CLOSE F-CPTE-ES.
           IF NOT CPTES-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6080-CLOSE-F-CPTE-ES-FIN.
           EXIT.
       6100-CLOSE-F-ETAT-CLI-S-DEB.
           CLOSE F-ETAT-CLI-S.
           IF NOT CLI-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETAT-CLI-S'
              DISPLAY 'VALEUR DU FS= ' WS-FS-ETAT-CLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6100-CLOSE-F-ETAT-CLI-S-FIN.
           EXIT.
       6110-CLOSE-F-ETAT-ANO-S-DEB.
           CLOSE F-ETAT-ANO-S.
           IF NOT ANO-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETAT-ANO-S'
              DISPLAY 'VALEUR DU FS= ' WS-FS-ETAT-ANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6110-CLOSE-F-ETAT-ANO-S-FIN.
           EXIT.
      *DEBUT WRITE
       6120-WRITE-F-CPTE-ES-DEB.
           WRITE FS-BUFF-F-CPTE-ES.
           IF NOT CPTES-OK
              DISPLAY 'PROBLEME DE ECRITURE DU FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6120-WRITE-F-CPTE-ES-FIN.
           EXIT.
       6170-REWRITE-F-CPTE-ES-DEB.
           REWRITE FS-BUFF-F-CPTE-ES.
           IF NOT CPTES-OK
              DISPLAY 'PROBLEME DE RE-ECRITURE DU FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6170-REWRITE-F-CPTE-ES-FIN.
           EXIT.
       6130-WRITE-F-ETAT-ANO-S-DEB.
           WRITE FS-BUFF-F-ETAT-ANO-S.
           IF NOT ANO-OK
              DISPLAY 'PROBLEMME ECRITURE FICHIER F-ETAT-ANO-S'
              DISPLAY 'VALEUR DU FS= ' WS-FS-ETAT-ANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6130-WRITE-F-ETAT-ANO-S-FIN.
           EXIT.
       6140-WRITE-F-ETAT-CLI-S-DEB.
           WRITE FS-BUFF-F-ETAT-CLI-S.
           IF NOT CLI-OK
              DISPLAY 'PROBLEMME ECRITURE FICHIER F-ETAT-CLI-S'
              DISPLAY 'VALEUR DU FS= ' WS-FS-ETAT-CLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6140-WRITE-F-ETAT-CLI-S-FIN.
           EXIT.
      * DEBUT WRITE AVEC SAUT DE PAGE
       6150-WRITE-F-ETAT-ANO-P-DEB.
           WRITE FS-BUFF-F-ETAT-ANO-S AFTER PAGE.
           IF NOT ANO-OK
              DISPLAY 'PROBLEMME ECRITURE FICHIER F-ETAT-ANO-S'
              DISPLAY 'VALEUR DU FS= ' WS-FS-ETAT-ANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6150-WRITE-F-ETAT-ANO-P-FIN.
           EXIT.
       6160-WRITE-F-ETAT-CLI-P-DEB.
           WRITE FS-BUFF-F-ETAT-CLI-S AFTER PAGE.
           IF NOT CLI-OK
              DISPLAY 'PROBLEMME ECRITURE FICHIER F-ETAT-CLI-S'
              DISPLAY 'VALEUR DU FS= ' WS-FS-ETAT-CLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6160-WRITE-F-ETAT-CLI-P-FIN.
           EXIT.
       6170-DELET-F-CPTE-ES-DEB.
           DELETE F-CPTE-ES.
           IF NOT CPTES-OK
              DISPLAY 'PROBLEME DE SUPPRESSION DU FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6170-DELET-F-CPTE-ES-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *DEBUT 7000
      * INITIALISE LA DATE DU JOUR
       7000-INIT-DATE-DEB.
           ACCEPT WS-DATE-J FROM DATE YYYYMMDD.
      *DATE FRONT PAGE ETAT CLI
           MOVE WS-DD   OF WS-DATE-J    TO WS-L7-JJ-ED.
           MOVE WS-MM   OF WS-DATE-J    TO WS-L7-MM-ED.
           MOVE WS-SS   OF WS-DATE-J    TO WS-L7-SS-ED.
           MOVE WS-AA   OF WS-DATE-J    TO WS-L7-AA-ED.
      *DATE ETAT CLI RELEVE  DU XXXXX
           MOVE WS-MM   OF WS-DATE-J    TO WS-LETAT-MM-ED.
           MOVE WS-DD   OF WS-DATE-J    TO WS-LETAT-JJ-ED.
           MOVE WS-SS   OF WS-DATE-J    TO WS-LETAT-SS-ED.
           MOVE WS-AA   OF WS-DATE-J    TO WS-LETAT-AA-ED.
       7000-INIT-DATE-FIN.
           EXIT.
      *CALCULE LE SOLDE POUR UN COMPTE AVEC MOUVEMENT
       7080-SOLD-STAND-DEB.
           COMPUTE WS-NEW-SOLD = WS-CPTES-SOLDE - WS-CUM-DEB
                                 + WS-CUM-CRED.
           MOVE WS-NEW-SOLD             TO WS-LETAT-SOLD-ED.
           MOVE WS-DATE-J               TO WS-CPTES-DMAJ.
           MOVE WS-ENRG-F-CPTES         TO FS-BUFF-F-CPTE-ES.
       7080-SOLD-STAND-FIN.
           EXIT.
      *CALCULE TRAITMENT CODE R
       7040-CALC-R-DEB.
           COMPUTE WS-CUM-DEB = WS-CUM-DEB + WS-MVTS-MT.
           ADD     1                    TO WS-NB-RETRAIT.
           ADD 1                        TO WS-LIGNE-COUNT.
           MOVE 'RETRAIT DAB'           TO WS-LETAT-OP-LIB-ED.
           MOVE WS-MVTS-MT              TO WS-LETAT-OP-DEBIT-ED.
           MOVE ZERO                    TO WS-LETAT-OP-CREDIT-ED.
       7040-CALC-R-FIN.
           EXIT.
      *CALCULE TRAITMENT CODE C
       7050-CALC-C-DEB.
           COMPUTE WS-CUM-DEB = WS-CUM-DEB + WS-MVTS-MT.
           ADD     1                    TO WS-NB-CB.
           ADD 1                        TO WS-LIGNE-COUNT.
           MOVE 'CARTE BLEUE'           TO WS-LETAT-OP-LIB-ED.
           MOVE WS-MVTS-MT              TO WS-LETAT-OP-DEBIT-ED.
           MOVE ZERO                    TO WS-LETAT-OP-CREDIT-ED.
       7050-CALC-C-FIN.
           EXIT.
      *CALCULE TRAITMENT CODE D
       7060-CALC-D-DEB.
           COMPUTE WS-CUM-CRED = WS-CUM-CRED + WS-MVTS-MT.
           ADD     1                    TO WS-NB-DEP.
           ADD 1                        TO WS-LIGNE-COUNT.
           MOVE 'DEPOT GUICHET'         TO WS-LETAT-OP-LIB-ED.
           MOVE ZERO                    TO WS-LETAT-OP-DEBIT-ED.
           MOVE WS-MVTS-MT              TO WS-LETAT-OP-CREDIT-ED.
       7060-CALC-D-FIN.
           EXIT.
      *CALCULE TRAITMENT CODE ERREUR
       7070-CALC-OTHER-DEB.
           COMPUTE WS-RENDU-CUM-ANO = WS-RENDU-CUM-ANO + WS-MVTS-MT.
           ADD     1                    TO WS-NB-ANO.
           MOVE WS-MVTS-CPTE            TO WS-LANO-NUMCPT-ED.
           MOVE WS-MVTS-CODE            TO WS-LANO-CODEMVT-ED.
           MOVE WS-MVTS-MT              TO WS-LANO-MONTANT-ED.
       7070-CALC-OTHER-FIN.
           EXIT.
      *INITALISE LES VARAIBLE POUR LE TRT MVT AVEC CPT
       7090-INIT-CPT-EXIST-DEB.
           ADD  1                       TO WS-NB-CLI-STAND.
           MOVE ZERO                    TO WS-CUM-CRED.
           MOVE ZERO                    TO WS-CUM-DEB.
           MOVE ZERO                    TO WS-LIGNE-COUNT.
           MOVE ZERO                    TO WS-PAGE-COUNT.
           MOVE ZERO                    TO WS-FLAG-CLOSE.
           MOVE WS-DATE-J               TO WS-CPTES-DMAJ.
           MOVE ' '                     TO WS-LETAT-CLOSE-ED.
           ADD  1                       TO WS-NB-CLI.
       7090-INIT-CPT-EXIST-FIN.
           EXIT.
      *INITALISE LES VARAIBLE POUR LE TRT MVT SANS CPT
       7120-INIT-CPT-NEW-DEB.
           MOVE ZERO                    TO WS-CUM-CRED.
           MOVE ZERO                    TO WS-CUM-DEB.
           MOVE ZERO                    TO WS-LIGNE-COUNT.
           MOVE ZERO                    TO WS-PAGE-COUNT.
           MOVE ZERO                    TO WS-FLAG-CLOSE.
           MOVE WS-DATE-J               TO WS-CPTES-DMAJ.
           MOVE ' '                     TO WS-LETAT-CLOSE-ED.
           ADD 1                        TO WS-NB-CLI.
       7120-INIT-CPT-NEW-FIN.
           EXIT.
      *CALCULE LE SOLDE POUR LA CREATION D UN COMPTE
      *PREPARE L ENREGITREMENT A ECRIRE
       7100-SOLD-NEW-DEB.
           COMPUTE WS-NEW-SOLD = 0 - WS-CUM-DEB
                                 + WS-CUM-CRED.
           MOVE WS-DATE-J               TO WS-CPTES-DMAJ.
           MOVE WS-ENRG-F-CPTES         TO FS-BUFF-F-CPTE-ES.
       7100-SOLD-NEW-FIN.
           EXIT.
      *CALCULE LES TOT POUR LE COMPTE RENDU D EXEC
      *PREPARE L ENREGITREMENT A ECRIRE
       7110-CALC-RENDU-EXEC-DEB.
           COMPUTE WS-NB-MVT = WS-NB-RETRAIT + WS-NB-DEP + WS-NB-CB
                   + WS-NB-ANO + WS-NB-CLOSE.
           MOVE WS-NB-RETRAIT           TO WS-LCRE-RET-TOT-ED.
           MOVE WS-NB-CLI               TO WS-LCRE-CPT-TRT-TOT-ED.
           MOVE WS-NB-DEP               TO WS-LCRE-DEP-TOT-ED.
           MOVE WS-NB-CLI-NEW           TO WS-LCRE-CPT-CRE-TOT-ED.
           MOVE WS-NB-CLI-STAND         TO WS-LCRE-CPT-STD-TOT-ED.
           MOVE WS-NB-CLI-CLOSE         TO WS-LCRE-CPT-CLR-TOT-ED.
           MOVE WS-NB-CB                TO WS-LCRE-CBS-TOT-ED.
           MOVE WS-NB-ANO               TO WS-LCRE-ANOM-TOT-ED.
           MOVE WS-NB-MVT               TO WS-LCRE-MVTS-TOT-ED.
       7110-CALC-RENDU-EXEC-FIN.
           EXIT.
      *MISE A JOUR DE LA DATE POUR LE PREMIER MOUVEMENT VALIDE
      *DANS LE CAS D UN MOUVEMENT SANS COMPTE
       7190-PREP-HEADER-NEW-DEB.
           MOVE WS-DATE-J               TO WS-CPTES-DMAJ.
           MOVE WS-CPTES-CPTE           TO WS-LETAT-NUMCPT-ED.
           MOVE WS-PAGE-COUNT           TO WS-LETAT-PAGE-ED.
           MOVE 'CREATION DE COMPTE'
                                        TO WS-LETAT-OPEN-ED.
           MOVE 'ANCIEN SOLDE'
                                        TO WS-LETAT-LIB-ED.
           MOVE ZERO                    TO WS-LETAT-SOLD-ED.
       7190-PREP-HEADER-NEW-FIN.
           EXIT.
       7200-PREP-HEADER-DEB.
           MOVE ZERO                    TO WS-LIGNE-COUNT
           ADD  1                       TO WS-PAGE-COUNT
           MOVE WS-CPTES-CPTE           TO WS-LETAT-NUMCPT-ED
           MOVE WS-PAGE-COUNT           TO WS-LETAT-PAGE-ED.
           MOVE '                  '    TO WS-LETAT-OPEN-ED.
           MOVE 'ANCIEN SOLDE'          TO WS-LETAT-LIB-ED.
           MOVE ' '                     TO WS-LETAT-CLOSE-ED.
           MOVE WS-CPTES-SOLDE          TO WS-LETAT-SOLD-ED.
       7200-PREP-HEADER-FIN.
           EXIT.
       7210-PREP-DATE-MVT-DEB.
           MOVE WS-MVTS-SS              TO WS-LETAT-OP-SS-ED.
           MOVE WS-MVTS-AA              TO WS-LETAT-OP-AA-ED.
           MOVE WS-MVTS-JJ              TO WS-LETAT-OP-JJ-ED.
           MOVE WS-MVTS-MM              TO WS-LETAT-OP-MM-ED.
       7210-PREP-DATE-MVT-FIN.
           EXIT.
       7220-PREP-NEW-SOLD-DEB.
           MOVE 'TOTAL DES OPERATIONS'
                                        TO WS-LETAT-TOT-LIB-ED.
           MOVE 'NOUVEAU SOLDE'         TO WS-LETAT-LIB-ED.
           MOVE WS-CUM-CRED             TO WS-LETAT-TOTCR-ED.
           MOVE WS-CUM-DEB              TO WS-LETAT-TOTDB-ED.
           MOVE WS-NEW-SOLD             TO WS-LETAT-SOLD-ED.
       7220-PREP-NEW-SOLD-FIN.
           EXIT.
       7260-PREP-INTER-SOLD-DEB.
           MOVE 'SOUS TOTAL DES OPERATIONS'
                                        TO WS-LETAT-TOT-LIB-ED.
           MOVE 'SOLDE INTERMEDIAIRE'   TO WS-LETAT-LIB-ED.
           MOVE WS-CUM-CRED             TO WS-LETAT-TOTCR-ED.
           MOVE WS-CUM-DEB              TO WS-LETAT-TOTDB-ED.
           MOVE WS-NEW-SOLD             TO WS-LETAT-SOLD-ED.
           MOVE ZERO                    TO WS-LIGNE-COUNT.
       7260-PREP-INTER-SOLD-FIN.
           EXIT.
       7230-PREP-FOOTER-ANO-DEB.
           MOVE WS-RENDU-CUM-ANO        TO WS-LANO-TOTAL-ED.
       7230-PREP-FOOTER-ANO-FIN.
           EXIT.
       7240-PREP-ETAT-CLI-CLOSE-DEB.
           MOVE 'CLOTURE DE COMPTE'     TO WS-LETAT-CLOSE-ED.
       7240-PREP-ETAT-CLI-CLOSE-FIN.
           EXIT.
       7250-CALC-CLOSE-DEB.
           MOVE 1                       TO WS-FLAG-CLOSE.
           ADD 1                        TO WS-NB-CLOSE.
       7250-CALC-CLOSE-FIN.
           EXIT.
       7290-PREP-WRITE-DEB.
           ADD  1                       TO WS-NB-CLI-NEW.
           MOVE WS-NEW-SOLD             TO WS-CPTES-SOLDE.
           MOVE WS-DATE-J               TO WS-CPTES-DMAJ.
           MOVE WS-ENRG-F-CPTES         TO FS-BUFF-F-CPTE-ES.
       7290-PREP-WRITE-FIN.
           EXIT.
       7270-PREP-REWRITE-DEB.
           MOVE WS-NEW-SOLD             TO WS-CPTES-SOLDE.
           MOVE WS-DATE-J               TO WS-CPTES-DMAJ.
           MOVE WS-ENRG-F-CPTES         TO FS-BUFF-F-CPTE-ES.
       7270-PREP-REWRITE-FIN.
           EXIT.
       7280-PREP-DMAJ-DEB.
           MOVE WS-MVTS-DATE            TO WS-CPTES-DCREA.
           MOVE WS-MVTS-CPTE            TO WS-CPTES-CPTE.
       7280-PREP-DMAJ-FIN.
           EXIT.
       7300-COUNT-CLOSE-DEB.
           ADD     1                    TO WS-NB-CLI-CLOSE.
       7300-COUNT-CLOSE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *8000 SECTION
      *ECRIT L PAGE DE GARDE DE ETAT CLI
       8140-FRONT-CLI-DEB.
           MOVE WS-ENTETE-L1            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6160-WRITE-F-ETAT-CLI-P-DEB
              THRU 6160-WRITE-F-ETAT-CLI-P-FIN.
           MOVE WS-ENTETE-L2            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L3            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L4            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L2            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L2            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L5            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L6            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L2            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L7            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L8            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L2            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L1            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
       8140-FRONT-CLI-FIN.
           EXIT.
      *ECRIT LA PAGE DE GARDE DE ETAT ANO
       8150-FRONT-ANO-DEB.
           MOVE WS-ENTETE-L1            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6150-WRITE-F-ETAT-ANO-P-DEB
              THRU 6150-WRITE-F-ETAT-ANO-P-FIN.
           MOVE WS-ENTETE-L2            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-ENTETE-L3       TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-ENTETE-L4       TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-ENTETE-L2            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-ENTETE-L5       TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-ENTETE-L6       TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-ENTETE-L2            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-ENTETE-L7            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-ENTETE-L8            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-ENTETE-L2            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-ENTETE-L2            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-ENTETE-L1            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
       8150-FRONT-ANO-FIN.
           EXIT.
      *ECRIT LE HEADER POUR LE FICHIER ETAT ANO
       8060-HEADER-ANO-DEB.
           MOVE WS-LANO-L1              TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6150-WRITE-F-ETAT-ANO-P-DEB
              THRU 6150-WRITE-F-ETAT-ANO-P-FIN.
           MOVE WS-LANO-TITRES          TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-L3              TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
       8060-HEADER-ANO-FIN.
           EXIT.
      *ECRIT PAS D ANO
       8100-LANO-OK-DEB.
           MOVE WS-LANO-OK              TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
       8100-LANO-OK-FIN.
           EXIT.
       8130-L-ETAT-ANO-S-DEB.
           MOVE WS-LANO-DETAIL          TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
       8130-L-ETAT-ANO-S-FIN.
           EXIT.
      *ECRIT LE FOOTER DES ANO
       8110-FOOTER-ANO-DEB.
           MOVE WS-LANO-L3              TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-TOTAL           TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-L1              TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
       8110-FOOTER-ANO-FIN.
           EXIT.
      *ECRIT LE HEADER ETAT CLI POUR UN NOUVEAU CLIENT
       8120-HEADER-ETAT-CLI-NEW-DEB.
           MOVE WS-ENTETE-L1            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6160-WRITE-F-ETAT-CLI-P-DEB
              THRU 6160-WRITE-F-ETAT-CLI-P-FIN.
           MOVE WS-LETAT-DATE-PAGE      TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-NUMCPT         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TIRETS         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-SOLD-OP        TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TIRETS         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TITRES         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TIRETS         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
       8120-HEADER-ETAT-CLI-NEW-FIN.
           EXIT.
      *ECRIT LE HEADER ETAT CLI POUR UN COMPTE EXISTANT
       8020-HEADER-ETAT-CLI-DEB.
           MOVE WS-ENTETE-L1            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6160-WRITE-F-ETAT-CLI-P-DEB
              THRU 6160-WRITE-F-ETAT-CLI-P-FIN.
           MOVE WS-LETAT-DATE-PAGE      TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-NUMCPT         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TIRETS         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-SOLD-OP        TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TIRETS         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TITRES         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TIRETS         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
       8020-HEADER-ETAT-CLI-FIN.
           EXIT.
      *ECRIT LA LIGNE DES MOUVEMENT DANS ETAT CLI
       8040-ETAT-CLI-DEB.
           MOVE WS-LETAT-DETAIL-OP      TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
       8040-ETAT-CLI-FIN.
           EXIT.
      *ECRIT LE FOOTER ETAT CLI
       8080-FOOTER-ETAT-CLI-DEB.
           MOVE WS-LETAT-TIRETS         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TOT-OP         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TIRETS         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-SOLD-OP        TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-ENTETE-L1            TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
       8080-FOOTER-ETAT-CLI-FIN.
           EXIT.
      *ECRIT LE COMPTE RENDU D EXECUTION
       8999-STATISTIQUES-DEB.
           DISPLAY WS-LCRE-ASTER.
           DISPLAY WS-LCRE-TITRE.
           DISPLAY WS-LCRE-ASTER.
           DISPLAY WS-LCRE-CPT-TRT-ED.
           DISPLAY WS-LCRE-CPT-CRE-ED.
           DISPLAY WS-LCRE-CPT-STD-ED.
           DISPLAY WS-LCRE-CPT-CLR-ED.
           DISPLAY WS-LCRE-MVTS-ED.
           DISPLAY WS-LCRE-ANOM-ED.
           DISPLAY WS-LCRE-RET-ED.
           DISPLAY WS-LCRE-CBS-ED.
           DISPLAY WS-LCRE-DEP-ED.
           DISPLAY WS-LCRE-ASTER.

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
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO411         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO411        *'.
           DISPLAY '*==============================================*'.
           MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.

