      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARID111                                   *
      *  NOM DU REDACTEUR : BAUDELET                                  *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 07/05/2024                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *PROJET DE MIGRATION DU TP4.                                    *
      *MIGRATION DES FICHIER MVTS ET CPTES                            *
      *EN TABLE DB2 TMVTS ET TCPTES                                   *
      *---------------------------------------------------------------*
      *MISE A JOUR DES COMPTES CLIENTS A PARTIR DE LA TABLE           *
      *MOUVEMENTS BANCAIRE.(TMVTS) SUR LA TABLE (TCPTES)              *
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
       PROGRAM-ID.      ARID111.
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
      * COPY
       COPY TP4LEDIT.
      *SQL INCLUDE
           EXEC SQL
               INCLUDE FCPTES
           END-EXEC.
           EXEC SQL
               INCLUDE FMVTS
           END-EXEC.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
             DECLARE CUR-MVTS
             CURSOR FOR
               SELECT NUMCPTE,
                      DMVTS,
                      CMVTS,
                      MTMVTS
               FROM TMVTS
           END-EXEC.
      *SQL VARIABLE
       01 WS-DB2-ERR-CPTES.
          02 WS-CPTES-SQLCODE
                            PIC S9(9) COMP       VALUE ZERO.
             88 CPTES-FIN                        VALUE 100.
             88 CPTES-OK                         VALUE 0.
          02 WS-CPTES-ERR   PIC X(70)            VALUE SPACE.
       01 WS-DB2-ERR-MVTS.
          02 WS-MVTS-SQLCODE
                            PIC S9(9) COMP       VALUE ZERO.
             88 MVTS-FIN                         VALUE 100.
             88 MVTS-OK                          VALUE 0.
          02 WS-MVTS-ERR    PIC X(70)            VALUE SPACE.
      *MY WS SECTION
       01  WS-NUM-KEY       PIC X(10).
      *FILE STATUS
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
       01 WS-DB2-DATE.
          05 WS-DD          PIC 9(2).
          05 FILLER         PIC X.
          05 WS-MM          PIC 9(2).
          05 FILLER         PIC X.
          05 WS-YYYY.
             10 WS-SS       PIC 9(2).
             10 WS-AA       PIC 9(2).
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
      *    OPEN CUR TABLES
           PERFORM 6000-OPEN-CUR-MVTS-DEB
              THRU 6000-OPEN-CUR-MVTS-FIN.
      *    OPEN FICHIER SORTIE
           PERFORM 6030-OPEN-F-ETAT-CLI-S-DEB
              THRU 6030-OPEN-F-ETAT-CLI-S-FIN.
           PERFORM 6040-OPEN-F-ETAT-ANO-S-DEB
              THRU 6040-OPEN-F-ETAT-ANO-S-FIN.
      *    READ FILE
           PERFORM 6020-READ-CUR-MVTS-DEB
              THRU 6020-READ-CUR-MVTS-FIN.
           IF MVTS-FIN
              DISPLAY 'TABLE F-MVTS VIDE'
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
      *    CLOSE CUR TABLE
           PERFORM 6010-CLOSE-CUR-MVTS-DEB
              THRU 6010-CLOSE-CUR-MVTS-FIN.
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
           PERFORM 6060-EXIST-TCPTES-DEB
              THRU 6060-EXIST-TCPTES-FIN.
      *AS
           IF CPTES-FIN
              PERFORM 2010-TRT-CPT-NEW-DEB
                 THRU 2010-TRT-CPT-NEW-FIN
           ELSE
              PERFORM 2000-TRT-CPT-EXIST-DEB
                 THRU 2000-TRT-CPT-EXIST-FIN
           END-IF.
      *DROITE
       1000-TRT-CPT-FIN.
           EXIT.
      * GESTION DES CPT EXISTANT ET MISE A JOUR DANS LA TABLE CPTES
       2000-TRT-CPT-EXIST-DEB.
      *GAUCHE
           PERFORM 7090-INIT-CPT-EXIST-DEB
              THRU 7090-INIT-CPT-EXIST-FIN.
      *ITERATIVE
           PERFORM 3000-TRT-AVEC-CPTE-DEB
              THRU 3000-TRT-AVEC-CPTE-FIN
             UNTIL (NUMCPTE NOT = WS-NUM-KEY) OR MVTS-FIN.
      *DROITE
           PERFORM 7080-SOLD-STAND-DEB
              THRU 7080-SOLD-STAND-FIN.
           IF WS-FLAG-CLOSE = 1
              PERFORM 7300-COUNT-CLOSE-DEB
                 THRU 7300-COUNT-CLOSE-FIN
              PERFORM 6050-DELETE-TCPTES-DEB
                 THRU 6050-DELETE-TCPTES-FIN
              PERFORM 7240-PREP-ETAT-CLI-CLOSE-DEB
                 THRU 7240-PREP-ETAT-CLI-CLOSE-FIN
           ELSE
              PERFORM 7270-PREP-REWRITE-DEB
                 THRU 7270-PREP-REWRITE-FIN
              PERFORM 6030-UPDATE-TCPTES-DEB
                 THRU 6030-UPDATE-TCPTES-FIN
           END-IF
           IF WS-CUM-DEB > 0 OR WS-CUM-CRED > 0
              PERFORM 7220-PREP-NEW-SOLD-DEB
                 THRU 7220-PREP-NEW-SOLD-FIN
              PERFORM 8080-FOOTER-ETAT-CLI-DEB
                 THRU 8080-FOOTER-ETAT-CLI-FIN
           END-IF.
       2000-TRT-CPT-EXIST-FIN.
           EXIT.
      *GESTION DES CPT INEXISTANT ET CREATION DANS LA TABLES TCPTES
       2010-TRT-CPT-NEW-DEB.
      *GAUCHE
           PERFORM 7120-INIT-CPT-NEW-DEB
              THRU 7120-INIT-CPT-NEW-FIN.
      *ITERATIVE
           PERFORM 3010-TRT-SANS-CPTE-DEB
              THRU 3010-TRT-SANS-CPTE-FIN
             UNTIL (NUMCPTE NOT = WS-NUM-KEY) OR MVTS-FIN.
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
                 PERFORM 6040-INSERT-TCPTES-DEB
                    THRU 6040-INSERT-TCPTES-FIN
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
           PERFORM 6020-READ-CUR-MVTS-DEB
              THRU 6020-READ-CUR-MVTS-FIN.
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
           PERFORM 6020-READ-CUR-MVTS-DEB
              THRU 6020-READ-CUR-MVTS-FIN.
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
      *
       6000-OPEN-CUR-MVTS-DEB.
           EXEC SQL OPEN CUR-MVTS
           END-EXEC.
           MOVE SQLCODE                 TO WS-MVTS-SQLCODE.
           IF NOT MVTS-OK AND NOT MVTS-FIN
              DISPLAY 'ERREUR OUVERTURE CURSEUR MVTS : ' WS-MVTS-SQLCODE
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-OPEN-CUR-MVTS-FIN.
           EXIT.
       6010-CLOSE-CUR-MVTS-DEB.
           EXEC SQL CLOSE CUR-MVTS
           END-EXEC.
           MOVE SQLCODE                 TO WS-MVTS-SQLCODE.
           IF NOT MVTS-OK AND NOT MVTS-FIN
              DISPLAY 'ERREUR FERMETURE CURSEUR MVTS : ' WS-MVTS-SQLCODE
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-CLOSE-CUR-MVTS-FIN.
           EXIT.
       6020-READ-CUR-MVTS-DEB.
           EXEC SQL FETCH CUR-MVTS
                    INTO :NUMCPTE,
                         :DMVTS,
                         :CMVTS,
                         :MTMVTS
           END-EXEC.
           MOVE DMVTS                   TO WS-DB2-DATE.
           MOVE SQLCODE                 TO WS-MVTS-SQLCODE.
           IF NOT MVTS-OK AND NOT MVTS-FIN
              DISPLAY 'ERREUR LECTURE CURSEUR MVTS : ' WS-MVTS-SQLCODE
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-READ-CUR-MVTS-FIN.
           EXIT.
       6030-UPDATE-TCPTES-DEB.
           EXEC SQL UPDATE TCPTES
                    SET SLDCPTES    = :SLDCPTES,
                        DMJCPTES    = :DMJCPTES
                    WHERE NUMCPTES  = :NUMCPTES
           END-EXEC.
           MOVE SQLCODE                 TO WS-DB2-ERR-CPTES.
           IF NOT CPTES-OK AND NOT CPTES-FIN
              DISPLAY 'ERREUR UPDATE TCPTES : ' WS-CPTES-SQLCODE
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6030-UPDATE-TCPTES-FIN.
           EXIT.
       6040-INSERT-TCPTES-DEB.
           EXEC SQL INSERT INTO TCPTES
                    (NUMCPTES,DCRCPTES,SLDCPTES,DMJCPTES)
           VALUES (:NUMCPTES,:DCRCPTES,:SLDCPTES,:DMJCPTES)

           END-EXEC.
           MOVE SQLCODE                 TO WS-DB2-ERR-CPTES.
           IF NOT CPTES-OK AND NOT CPTES-FIN
              DISPLAY 'ERREUR INSERT TCPTES : ' WS-CPTES-SQLCODE
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-INSERT-TCPTES-FIN.
           EXIT.
       6050-DELETE-TCPTES-DEB.
           EXEC SQL DELETE FROM TCPTES
                    WHERE NUMCPTES = :NUMCPTES
           END-EXEC.
           MOVE SQLCODE                 TO WS-DB2-ERR-CPTES.
           IF NOT CPTES-OK AND NOT CPTES-FIN
              DISPLAY 'ERREUR DELETE TCPTES : ' WS-CPTES-SQLCODE
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6050-DELETE-TCPTES-FIN.
           EXIT.
       6060-EXIST-TCPTES-DEB.
           EXEC SQL SELECT * INTO :NUMCPTES , :DCRCPTES,
                                  :SLDCPTES , :DMJCPTES
                    FROM TCPTES
                    WHERE NUMCPTES = :NUMCPTE
           END-EXEC.
           MOVE NUMCPTE                 TO WS-NUM-KEY.
           MOVE SQLCODE                 TO WS-DB2-ERR-CPTES.
           IF NOT CPTES-OK AND NOT CPTES-FIN
              DISPLAY 'ERREUR SELECT TCPTES : ' WS-DB2-ERR-CPTES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6060-EXIST-TCPTES-FIN.
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
      * DEBUT DES CLOSE
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
           COMPUTE WS-NEW-SOLD = SLDCPTES - WS-CUM-DEB
                                 + WS-CUM-CRED.
           MOVE WS-NEW-SOLD             TO WS-LETAT-SOLD-ED.
           MOVE CORR WS-DATE-J          TO WS-DB2-DATE.
           MOVE WS-DB2-DATE             TO DMJCPTES.
       7080-SOLD-STAND-FIN.
           EXIT.
      *CALCULE TRAITMENT CODE R
       7040-CALC-R-DEB.
           COMPUTE WS-CUM-DEB = WS-CUM-DEB + MTMVTS.
           ADD     1                    TO WS-NB-RETRAIT.
           ADD     1                    TO WS-LIGNE-COUNT.
           MOVE 'RETRAIT DAB'           TO WS-LETAT-OP-LIB-ED.
           MOVE MTMVTS                  TO WS-LETAT-OP-DEBIT-ED.
           MOVE ZERO                    TO WS-LETAT-OP-CREDIT-ED.
           MOVE DMVTS                   TO WS-DB2-DATE.
       7040-CALC-R-FIN.
           EXIT.
      *CALCULE TRAITMENT CODE C
       7050-CALC-C-DEB.
           COMPUTE WS-CUM-DEB = WS-CUM-DEB + MTMVTS.
           ADD     1                    TO WS-NB-CB.
           ADD     1                    TO WS-LIGNE-COUNT.
           MOVE 'CARTE BLEUE'           TO WS-LETAT-OP-LIB-ED.
           MOVE MTMVTS                  TO WS-LETAT-OP-DEBIT-ED.
           MOVE ZERO                    TO WS-LETAT-OP-CREDIT-ED.
           MOVE DMVTS                   TO WS-DB2-DATE.
       7050-CALC-C-FIN.
           EXIT.
      *CALCULE TRAITMENT CODE D
       7060-CALC-D-DEB.
           COMPUTE WS-CUM-CRED = WS-CUM-CRED + MTMVTS.
           ADD     1                    TO WS-NB-DEP.
           ADD     1                    TO WS-LIGNE-COUNT.
           MOVE 'DEPOT GUICHET'         TO WS-LETAT-OP-LIB-ED.
           MOVE ZERO                    TO WS-LETAT-OP-DEBIT-ED.
           MOVE MTMVTS                  TO WS-LETAT-OP-CREDIT-ED.
           MOVE DMVTS                   TO WS-DB2-DATE.
       7060-CALC-D-FIN.
           EXIT.
      *CALCULE TRAITMENT CODE ERREUR
       7070-CALC-OTHER-DEB.
           COMPUTE WS-RENDU-CUM-ANO = WS-RENDU-CUM-ANO + MTMVTS.
           ADD     1                    TO WS-NB-ANO.
           MOVE NUMCPTE                 TO WS-LANO-NUMCPT-ED.
           MOVE CMVTS                   TO WS-LANO-CODEMVT-ED.
           MOVE MTMVTS                  TO WS-LANO-MONTANT-ED.
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
           MOVE CORR WS-DATE-J          TO WS-DB2-DATE.
           MOVE WS-DB2-DATE             TO DMJCPTES.
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
           MOVE CORR WS-DATE-J          TO WS-DB2-DATE.
           MOVE WS-DB2-DATE             TO DMJCPTES.
           MOVE ' '                     TO WS-LETAT-CLOSE-ED.
           ADD 1                        TO WS-NB-CLI.
       7120-INIT-CPT-NEW-FIN.
           EXIT.
      *CALCULE LE SOLDE POUR LA CREATION D UN COMPTE
      *PREPARE L ENREGITREMENT A ECRIRE
       7100-SOLD-NEW-DEB.
           COMPUTE WS-NEW-SOLD = 0 - WS-CUM-DEB
                                 + WS-CUM-CRED.
           MOVE CORR WS-DATE-J          TO WS-DB2-DATE.
           MOVE WS-DB2-DATE             TO DMJCPTES.
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
           MOVE CORR WS-DATE-J          TO WS-DB2-DATE.
           MOVE WS-DB2-DATE             TO DMJCPTES.
           MOVE NUMCPTES                TO WS-LETAT-NUMCPT-ED.
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
           MOVE NUMCPTES           TO WS-LETAT-NUMCPT-ED
           MOVE WS-PAGE-COUNT           TO WS-LETAT-PAGE-ED.
           MOVE '                  '    TO WS-LETAT-OPEN-ED.
           MOVE 'ANCIEN SOLDE'          TO WS-LETAT-LIB-ED.
           MOVE ' '                     TO WS-LETAT-CLOSE-ED.
           MOVE SLDCPTES                TO WS-LETAT-SOLD-ED.
       7200-PREP-HEADER-FIN.
           EXIT.
       7210-PREP-DATE-MVT-DEB.
           MOVE WS-SS                   OF WS-DB2-DATE
                                        TO WS-LETAT-OP-SS-ED.
           MOVE WS-AA                   OF WS-DB2-DATE
                                        TO WS-LETAT-OP-AA-ED.
           MOVE WS-DD                   OF WS-DB2-DATE
                                        TO WS-LETAT-OP-JJ-ED.
           MOVE WS-MM                   OF WS-DB2-DATE
                                        TO WS-LETAT-OP-MM-ED.
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
           MOVE WS-NEW-SOLD             TO SLDCPTES.
           MOVE CORR WS-DATE-J          TO WS-DB2-DATE.
           MOVE WS-DB2-DATE             TO DMJCPTES.
       7290-PREP-WRITE-FIN.
           EXIT.
       7270-PREP-REWRITE-DEB.
           MOVE WS-NEW-SOLD             TO SLDCPTES.
           MOVE CORR WS-DATE-J          TO WS-DB2-DATE.
           MOVE WS-DB2-DATE             TO DMJCPTES.
       7270-PREP-REWRITE-FIN.
           EXIT.
       7280-PREP-DMAJ-DEB.
           MOVE DMVTS          TO DCRCPTES.
           MOVE NUMCPTE        TO NUMCPTES.
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
           EXEC SQL
               COMMIT
           END-EXEC.
           DISPLAY '*==============================================*'.
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARID111         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           EXEC SQL
               ROLLBACK
           END-EXEC.
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARID111        *'.
           DISPLAY '*==============================================*'.
           MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.


