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
      *EDITION DES COMPTES CLIENT PAR PLAGE DE CLEF                   *
      *SOIS PAR PLAGE DE NUMERO DE COMPTE                             *
      *SOIS PAR PLAGE DE NOM DE COMPTE                                *
      *SYSIN -> COMMANDE :                                            *
      *[NATURE CODE A/B][NOM DEMANDEUR][PLAGE A][PLAGE B]             *
      *EDITE UN FICHIER ETAT CLI EN SORTIE  (RESULTAT)                *
      *EDITE UN FICHIER ETAT ANO EN SORTIE  (ANOMALIE)                *
      *SYSOUT -> COMPTE RENDU D EXEC                                  *
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
       PROGRAM-ID.      ARIO511.
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
           SELECT  F-CPTE-E            ASSIGN TO INP001
                  ORGANIZATION         IS INDEXED
                  ACCESS MODE          IS DYNAMIC
                  RECORD KEY           IS FS-KEY-CPTE
                  ALTERNATE RECORD KEY IS FS-KEY-CLI WITH DUPLICATES
                  FILE STATUS          IS WS-FS-CPTE-E.
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
      *FICHIER CPTE ES
       FD  F-CPTE-E
           RECORD CONTAINS 50 CHARACTERS.
       01  FS-BUFF-F-CPTE-E.
           05  FS-KEY-CPTE       PIC X(10).
           05  FS-KEY-CLI        PIC X(20).
           05  FILLER            PIC X(20).
      *FICHIER  ETAT CLI SORTIE
       FD  F-ETAT-CLI-S
           RECORDING MODE IS F.
       01  FS-BUFF-F-ETAT-CLI-S  PIC X(80).
      *FICHIER ETAT ANO SORTIE
       FD  F-ETAT-ANO-S
           RECORDING MODE IS F.
       01  FS-BUFF-F-ETAT-ANO-S  PIC X(80).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
       COPY TP5LEDIT.
       COPY TP5CPTE.
       COPY TP5SYSIN.
      *MY WS SECTION
       01  WS-LETAT-EMPTY.
           05 FILLER        PIC X                VALUE  '|'.
           05 FILLER        PIC X(23)            VALUE  SPACES.
           05 FILLER        PIC X(53)            VALUE  ' PLAGE VIDE'.
           05 FILLER        PIC X                VALUE  '|'.
      *FILE STATUS
       01  WS-FS-CPTE-E     PIC X(2).
           88 CPTE-OK                            VALUE '00'.
           88 CPTE-FIN                           VALUE '10'.
           88 CPTE-INEXISTANT                    VALUE '23'.
           88 CPTE-DOUBLE                        VALUE '02'.
       01  WS-FS-ETAT-CLI-S PIC X(2).
           88 CLI-OK                             VALUE '00'.
       01  WS-FS-ETAT-ANO-S PIC X(2).
           88 ANO-OK                             VALUE '00'.
      *COMPTEUR RENDU EXEC
       01  WS-NB-DEM        PIC S9(4)     COMP   VALUE ZERO.
       01  WS-NB-DEM-ERR    PIC S9(4)     COMP   VALUE ZERO.
      *LOGIC PRG
       01  WS-COUNT-LINE    PIC S9(4)     COMP   VALUE ZERO.
       01  WS-MIN-NUM       PIC 9(10).
       01  WS-MIN-ALPHA     PIC X(20).
       01  WS-ERR-CODE      PIC 9(2).
           88 NO-ERREUR-CODE                     VALUE 0.
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
      *    OPEN FICHIER SORTIE
           PERFORM 6030-OPEN-F-ETAT-CLI-S-DEB
              THRU 6030-OPEN-F-ETAT-CLI-S-FIN.
           PERFORM 6040-OPEN-F-ETAT-ANO-S-DEB
              THRU 6040-OPEN-F-ETAT-ANO-S-FIN.
           PERFORM 6010-OPEN-F-CPTE-E-DEB
              THRU 6010-OPEN-F-CPTE-E-FIN.
           ACCEPT WS-SYSIN FROM SYSIN.
      *    GET MIN ALPHA
           PERFORM 7000-LV-TO-NUMKEY-DEB
              THRU 7000-LV-TO-NUMKEY-FIN.
           PERFORM 6170-POINTER-FCPTE-NUM-DEB
              THRU 6170-POINTER-FCPTE-NUM-FIN.
           PERFORM 6190-READ-F-CPTE-E-DEB
              THRU 6190-READ-F-CPTE-E-FIN.
           PERFORM 7010-GET-MIN-NUM-DEB
              THRU 7010-GET-MIN-NUM-FIN.
      *    GET MIN NUM
           PERFORM 7020-LV-TO-ALPHA-DEB
              THRU 7020-LV-TO-ALPHA-FIN.
           PERFORM 6180-POINTER-FCPTE-ALPHA-DEB
              THRU 6180-POINTER-FCPTE-ALPHA-FIN.
           PERFORM 6190-READ-F-CPTE-E-DEB
              THRU 6190-READ-F-CPTE-E-FIN.
           PERFORM 7030-GET-MIN-ALPHA-DEB
              THRU 7030-GET-MIN-ALPHA-FIN.

      *ITERATIVE
           PERFORM 1000-TRT-SYSIN-DEB
              THRU 1000-TRT-SYSIN-FIN
             UNTIL FIN-SYSIN.
      *DROITE
           PERFORM 8999-STATISTIQUES-DEB
              THRU 8999-STATISTIQUES-FIN.
           IF WS-NB-DEM-ERR > 0
      *    FOOTER ANO
              PERFORM 8050-FOOTER-ETAT-ANO-DEB
                 THRU 8050-FOOTER-ETAT-ANO-FIN
           END-IF
      *    CLOSE FICHIER ENTRER
           PERFORM 6190-CLOSE-F-CPTE-E-DEB
              THRU 6190-CLOSE-F-CPTE-E-FIN.
      *    CLOSE FICHIER SORTIE
           PERFORM 6100-CLOSE-F-ETAT-CLI-S-DEB
              THRU 6100-CLOSE-F-ETAT-CLI-S-FIN.
           PERFORM 6110-CLOSE-F-ETAT-ANO-S-DEB
              THRU 6110-CLOSE-F-ETAT-ANO-S-FIN.
      *    FIN PROGRAME
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *
       0000-PROGRAMME-FIN.
           STOP RUN.
      * TRT SYSIN
       1000-TRT-SYSIN-DEB.
           PERFORM 7110-COUNT-DEM-DEB
              THRU 7110-COUNT-DEM-FIN.
      *AM
           EVALUATE TRUE
               WHEN A
                    PERFORM 2000-CODE-A-DEB
                       THRU 2000-CODE-A-FIN
               WHEN B
                    PERFORM 2010-CODE-B-DEB
                       THRU 2010-CODE-B-FIN
               WHEN OTHER
                    PERFORM 2020-INVALIDE-CODE-DEB
                       THRU 2020-INVALIDE-CODE-FIN
           END-EVALUATE.

      *DROITE
           ACCEPT WS-SYSIN FROM SYSIN.
       1000-TRT-SYSIN-FIN.
           EXIT.
      *TRT DU CODE A POUR CMD NUMERIQUE
       2000-CODE-A-DEB.
      *GAUCHE
      *  INIT VAR TO 0
           PERFORM 7070-INIT-LOGIC-DEB
              THRU 7070-INIT-LOGIC-FIN.
      *GET LA MIN VALUE NUMERIQUE DE LA PLAGE DE COMPTE
           PERFORM 7130-PREP-POINT-NUM-DEB
              THRU 7130-PREP-POINT-NUM-FIN.
           PERFORM 6170-POINTER-FCPTE-NUM-DEB
              THRU 6170-POINTER-FCPTE-NUM-FIN.
      *TEST LA VALIDITER DE LA COMMANDE EN SYSIN
           EVALUATE TRUE
               WHEN WS-DEM-NOM = SPACES OR WS-DEM-CPT-DEB = SPACES
                    OR WS-DEM-CPT-FIN = SPACES
                    PERFORM 7040-ERR-VIDE-DEB
                       THRU 7040-ERR-VIDE-FIN
               WHEN WS-DEM-NOM IS NOT ALPHABETIC
                    PERFORM 7050-ERR-ALPHA-DEB
                       THRU 7050-ERR-ALPHA-FIN
               WHEN (WS-DEM-CPT-DEB IS NOT NUMERIC)
                                   OR (WS-DEM-CPT-FIN IS NOT NUMERIC)
                    PERFORM 7150-ERR-NUM-DEB
                       THRU 7150-ERR-NUM-DEB
               WHEN WS-DEM-CPT-DEB > WS-DEM-CPT-FIN
                    PERFORM 7060-ERR-INTERVAL-DEB
                       THRU 7060-ERR-INTERVAL-FIN
               WHEN CPTE-INEXISTANT OR WS-DEM-CPT-FIN < WS-MIN-NUM
                    PERFORM 7070-ERR-INEXISTANT-DEB
                       THRU 7070-ERR-INEXISTANT-FIN
           END-EVALUATE.
      *AS
      *    SI IL Y AS UNE ERREUR
           IF NOT NO-ERREUR-CODE
              PERFORM 3020-TRT-INVALIDE-DEB
                 THRU 3020-TRT-INVALIDE-FIN
           ELSE
              PERFORM 3000-TRT-VALIDE-A-DEB
                 THRU 3000-TRT-VALIDE-A-FIN
           END-IF.
      *DROITE
       2000-CODE-A-FIN.
           EXIT.
       2010-CODE-B-DEB.
      *GAUCHE
      *INIT
           PERFORM 7070-INIT-LOGIC-DEB
              THRU 7070-INIT-LOGIC-FIN.
      *GET LA MIN VALUE DE LA PLAGE ALPHABETIQUE
           PERFORM 7140-PREP-POINT-ALPHA-DEB
              THRU 7140-PREP-POINT-ALPHA-FIN.
           PERFORM 6180-POINTER-FCPTE-ALPHA-DEB
              THRU 6180-POINTER-FCPTE-ALPHA-FIN.
      *TEST LA VALIDITER DE LA COMMANDE EN SYSIN
           EVALUATE TRUE
               WHEN WS-DEM-NOM = SPACES OR WS-DEM-CLI-DEB = SPACES
                    OR WS-DEM-CLI-FIN = SPACES
                    PERFORM 7040-ERR-VIDE-DEB
                       THRU 7040-ERR-VIDE-FIN
               WHEN (WS-DEM-NOM IS NOT ALPHABETIC)
                                   OR (WS-DEM-CLI-DEB IS NOT ALPHABETIC)
                                   OR (WS-DEM-CLI-FIN IS NOT ALPHABETIC)
                    PERFORM 7050-ERR-ALPHA-DEB
                       THRU 7050-ERR-ALPHA-FIN
               WHEN WS-DEM-CLI-DEB > WS-DEM-CLI-FIN
                    PERFORM 7060-ERR-INTERVAL-DEB
                       THRU 7060-ERR-INTERVAL-FIN
               WHEN CPTE-INEXISTANT OR WS-DEM-CLI-FIN < WS-MIN-ALPHA
                    PERFORM 7070-ERR-INEXISTANT-DEB
                       THRU 7070-ERR-INEXISTANT-FIN
           END-EVALUATE.
      *AS
      *SI IL A UNE ERREUR DANS LA CMD
           IF NOT NO-ERREUR-CODE
              PERFORM 3020-TRT-INVALIDE-DEB
                 THRU 3020-TRT-INVALIDE-FIN
           ELSE
              PERFORM 3010-TRT-VALIDE-B-DEB
                 THRU 3010-TRT-VALIDE-B-FIN
           END-IF.
      *DROITE
       2010-CODE-B-FIN.
           EXIT.
       2020-INVALIDE-CODE-DEB.
           PERFORM 7120-COUNT-ERR-DEB
              THRU 7120-COUNT-ERR-FIN.
           IF WS-NB-DEM-ERR = 1
      *    HEADER ANO
              PERFORM 8030-HEADER-ETAT-ANO-DEB
                 THRU 8030-HEADER-ETAT-ANO-FIN
           END-IF.
      * CODE WS-PEM-TYPE FAUX
           PERFORM 7080-ERR-CODE-DEB
              THRU 7080-ERR-CODE-FIN.
           PERFORM 8040-MAIN-ETAT-ANO-DEB
              THRU 8040-MAIN-ETAT-ANO-FIN.
       2020-INVALIDE-CODE-FIN.
           EXIT.
       3000-TRT-VALIDE-A-DEB.
      *GAUCHE
           PERFORM 7150-INIT-COUNT-LINE-DEB
              THRU 7150-INIT-COUNT-LINE-FIN.
      * HEADER ETATCLI
           PERFORM 6190-READ-F-CPTE-E-DEB
              THRU 6190-READ-F-CPTE-E-FIN.
           PERFORM 7080-PREP-HEADER-ETATCLI-A-DEB
              THRU 7080-PREP-HEADER-ETATCLI-A-FIN.
           PERFORM 8000-HEADER-ETAT-CLI-DEB
              THRU 8000-HEADER-ETAT-CLI-FIN.
      *IERATIVE
           PERFORM 4000-TRT-DETAIL-DEB
              THRU 4000-TRT-DETAIL-FIN
             UNTIL CPTE-FIN OR WS-CPTE-CPTE > WS-DEM-CPT-FIN.
      *DROITE
      *FOOTER ETATCLI
           IF WS-COUNT-LINE = 0
              PERFORM 8060-EMPTY-ETAT-CLI-DEB
                 THRU 8060-EMPTY-ETAT-CLI-FIN
           END-IF.
           PERFORM 8020-FOOTER-ETAT-CLI-DEB
              THRU 8020-FOOTER-ETAT-CLI-FIN.
       3000-TRT-VALIDE-A-FIN.
           EXIT.
       3010-TRT-VALIDE-B-DEB.
      *GAUCHE
           PERFORM 7150-INIT-COUNT-LINE-DEB
              THRU 7150-INIT-COUNT-LINE-FIN.
      * HEADER ETATCLI
           PERFORM 6190-READ-F-CPTE-E-DEB
              THRU 6190-READ-F-CPTE-E-FIN.
           PERFORM 7090-PREP-HEADER-ETATCLI-B-DEB
              THRU 7090-PREP-HEADER-ETATCLI-B-FIN.
           PERFORM 8000-HEADER-ETAT-CLI-DEB
              THRU 8000-HEADER-ETAT-CLI-FIN.
      *IERATIVE
           PERFORM 4000-TRT-DETAIL-DEB
              THRU 4000-TRT-DETAIL-FIN
             UNTIL CPTE-FIN OR WS-CPTE-NOM > WS-DEM-CLI-FIN.
      *DROITE
      *FOOTER ETATCLI
           IF WS-COUNT-LINE = 0
              PERFORM 8060-EMPTY-ETAT-CLI-DEB
                 THRU 8060-EMPTY-ETAT-CLI-FIN
           END-IF.
           PERFORM 8020-FOOTER-ETAT-CLI-DEB
              THRU 8020-FOOTER-ETAT-CLI-FIN.
       3010-TRT-VALIDE-B-FIN.
           EXIT.
       3020-TRT-INVALIDE-DEB.
           PERFORM 7120-COUNT-ERR-DEB
              THRU 7120-COUNT-ERR-FIN.
           IF WS-NB-DEM-ERR = 1
              PERFORM 8030-HEADER-ETAT-ANO-DEB
                 THRU 8030-HEADER-ETAT-ANO-FIN
           END-IF.
      *8000 LIGNE ETATANO
           PERFORM 8040-MAIN-ETAT-ANO-DEB
              THRU 8040-MAIN-ETAT-ANO-FIN.
       3020-TRT-INVALIDE-FIN.
           EXIT.
       4000-TRT-DETAIL-DEB.
      *READ  FCPTE
           PERFORM 7100-PREP-MAIN-ETATCLI-DEB
              THRU 7100-PREP-MAIN-ETATCLI-FIN.
           PERFORM 8010-MAIN-ETAT-CLI-DEB
              THRU 8010-MAIN-ETAT-CLI-FIN.
           PERFORM 6190-READ-F-CPTE-E-DEB
              THRU 6190-READ-F-CPTE-E-FIN.
           PERFORM 7160-COUNT-LINE-DEB
              THRU 7160-COUNT-LINE-FIN.
      *PRINT LA LIGNE DANS ETATCLI
       4000-TRT-DETAIL-FIN.
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
       6010-OPEN-F-CPTE-E-DEB.
           OPEN INPUT F-CPTE-E.
           IF NOT CPTE-OK
              DISPLAY 'PROBLEME D''OUVERTURE FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6010-OPEN-F-CPTE-E-FIN.
           EXIT.
      * DEBUT DES READ
      * DEBUT DES CLOSE
       6190-CLOSE-F-CPTE-E-DEB.
           CLOSE  F-CPTE-E.
           IF NOT CPTE-OK
              DISPLAY 'PROBLEME D''FERMETURE FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6190-CLOSE-F-CPTE-E-FIN.
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
       6080-CLOSE-F-CPTE-E-DEB.
           CLOSE F-CPTE-E.
           IF NOT CPTE-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6080-CLOSE-F-CPTE-E-FIN.
           EXIT.
      *DEBUT READ
       6190-READ-F-CPTE-E-DEB.
           READ F-CPTE-E NEXT INTO WS-ENRG-F-CPTE.
           IF NOT (CPTE-OK OR CPTE-FIN OR CPTE-DOUBLE)
              DISPLAY 'PROBLEMME LECTURE FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6190-READ-F-CPTE-E-FIN.
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
       6170-POINTER-FCPTE-NUM-DEB.
      *     MOVE WS-DEM-CPT-DEB TO FS-KEY-CPTE.
           START F-CPTE-E
             KEY >= FS-KEY-CPTE
           END-START.
           IF NOT (CPTE-OK OR CPTE-INEXISTANT)
              DISPLAY 'ENREGISTREMENT INEXISTANT POUR NUM:' FS-KEY-CPTE
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6170-POINTER-FCPTE-NUM-FIN.
           EXIT.
       6180-POINTER-FCPTE-ALPHA-DEB.
      *     MOVE WS-DEM-CLI-DEB TO FS-KEY-CLI.
           START F-CPTE-E
             KEY >= FS-KEY-CLI
           END-START.
           IF NOT (CPTE-OK OR CPTE-INEXISTANT)
              DISPLAY 'ENREGISTREMENT INEXISTANT POUR ALPHA:' FS-KEY-CLI
              DISPLAY 'VALEUR DU FS= ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6180-POINTER-FCPTE-ALPHA-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *DEBUT 7000
       7000-LV-TO-NUMKEY-DEB.
           MOVE LOW-VALUES              TO FS-KEY-CPTE.
       7000-LV-TO-NUMKEY-FIN.
           EXIT.
       7010-GET-MIN-NUM-DEB.
           MOVE WS-CPTE-CPTE            TO WS-MIN-NUM.
       7010-GET-MIN-NUM-FIN.
           EXIT.
       7020-LV-TO-ALPHA-DEB.
           MOVE LOW-VALUES              TO FS-KEY-CLI.
       7020-LV-TO-ALPHA-FIN.
           EXIT.
       7030-GET-MIN-ALPHA-DEB.
           MOVE WS-CPTE-NOM             TO WS-MIN-ALPHA.
       7030-GET-MIN-ALPHA-FIN.
           EXIT.
       7040-ERR-VIDE-DEB.
      * MOVE ERREUR CHAMP VIDE SYSIN
           MOVE 2                       TO WS-ERR-CODE.
           MOVE WS-ERR-CODE             TO WS-LANO-NUM-ED.
           MOVE 'CHAMP VIDE'            TO WS-LANO-TYP-ED.
           MOVE WS-SYSIN                TO WS-LANO-ENR-ED.
       7040-ERR-VIDE-FIN.
           EXIT.
       7050-ERR-ALPHA-DEB.
      * MOVE ERREUR NON ALPHA
           MOVE 3                       TO WS-ERR-CODE.
           MOVE WS-ERR-CODE             TO WS-LANO-NUM-ED.
           MOVE 'CHAMP NON ALPHABETIQUE'
                                        TO WS-LANO-TYP-ED.
           MOVE WS-SYSIN                TO WS-LANO-ENR-ED.
       7050-ERR-ALPHA-FIN.
           EXIT.
       7060-ERR-INTERVAL-DEB.
           MOVE 4                       TO WS-ERR-CODE.
           MOVE WS-ERR-CODE             TO WS-LANO-NUM-ED.
           MOVE 'BORNE A SUPPERIEUR A BORNE B'
                                        TO WS-LANO-TYP-ED.
           MOVE WS-SYSIN                TO WS-LANO-ENR-ED.
      * ERREUR BORNE B SUP BORNE A
       7060-ERR-INTERVAL-FIN.
           EXIT.
       7070-ERR-INEXISTANT-DEB.
      * HORS LIMITE
           MOVE 5                       TO WS-ERR-CODE.
           MOVE WS-ERR-CODE             TO WS-LANO-NUM-ED.
           MOVE 'HORS LIMITE'           TO WS-LANO-TYP-ED.
           MOVE WS-SYSIN                TO WS-LANO-ENR-ED.
       7070-ERR-INEXISTANT-FIN.
           EXIT.
       7080-ERR-CODE-DEB.
           MOVE 1                       TO WS-ERR-CODE.
           MOVE WS-ERR-CODE             TO WS-LANO-NUM-ED.
           MOVE 'CODE INVALIDE'         TO WS-LANO-TYP-ED.
           MOVE WS-SYSIN                TO WS-LANO-ENR-ED.
       7080-ERR-CODE-FIN.
           EXIT.
       7150-ERR-NUM-DEB.
           MOVE 6                       TO WS-ERR-CODE.
           MOVE WS-ERR-CODE             TO WS-LANO-NUM-ED.
           MOVE 'CHAMP NON NUMIERIQUE'  TO WS-LANO-TYP-ED.
           MOVE WS-SYSIN                TO WS-LANO-ENR-ED.
       7150-ERR-NUM-FIN.
           EXIT.
       7070-INIT-LOGIC-DEB.
      * RESET LES VARIABLE DE LOGIC À 0
           MOVE 0                       TO WS-ERR-CODE.
       7070-INIT-LOGIC-FIN.
           EXIT.
       7080-PREP-HEADER-ETATCLI-A-DEB.
      * PREP HEADER
           MOVE WS-DEM-NOM              TO WS-LETAT-NOMD-ED.
           MOVE WS-NB-DEM               TO WS-LETAT-NUM-ED.
           MOVE 1                       TO WS-LETAT-PAGE-ED.
           MOVE 'NUMERO DE COMPTE'      TO WS-LETAT-TYPE-ED.
           MOVE WS-DEM-CPT-DEB          TO WS-LETAT-REFDEB-ED.
           MOVE WS-DEM-CPT-FIN          TO WS-LETAT-REFFIN-ED.
       7080-PREP-HEADER-ETATCLI-A-FIN.
           EXIT.
       7090-PREP-HEADER-ETATCLI-B-DEB.
      * PREP HEADER
           MOVE WS-DEM-NOM              TO WS-LETAT-NOMD-ED.
           MOVE WS-NB-DEM               TO WS-LETAT-NUM-ED.
           MOVE 1                       TO WS-LETAT-PAGE-ED.
           MOVE 'NOM DE CLIENT'         TO WS-LETAT-TYPE-ED.
           MOVE WS-DEM-CLI-DEB          TO WS-LETAT-REFDEB-ED.
           MOVE WS-DEM-CLI-FIN          TO WS-LETAT-REFFIN-ED.
       7090-PREP-HEADER-ETATCLI-B-FIN.
           EXIT.
       7100-PREP-MAIN-ETATCLI-DEB.
      * PREP HEADER
           MOVE WS-CPTE-CPTE            TO WS-LETAT-NUMCPT-ED.
           MOVE WS-CPTE-DCREA-JJ        TO WS-LETAT-DCREA-JJ-ED.
           MOVE WS-CPTE-DCREA-MM        TO WS-LETAT-DCREA-MM-ED.
           MOVE WS-CPTE-DCREA-SS        TO WS-LETAT-DCREA-SS-ED.
           MOVE WS-CPTE-DCREA-AA        TO WS-LETAT-DCREA-AA-ED.
           MOVE WS-CPTE-DMAJ-JJ         TO WS-LETAT-DMAJ-JJ-ED.
           MOVE WS-CPTE-DMAJ-MM         TO WS-LETAT-DMAJ-MM-ED.
           MOVE WS-CPTE-DMAJ-SS         TO WS-LETAT-DMAJ-SS-ED.
           MOVE WS-CPTE-DMAJ-AA         TO WS-LETAT-DMAJ-AA-ED.
           MOVE WS-CPTE-SOLDE           TO WS-LETAT-SOLDE-ED.
           MOVE WS-CPTE-NOM             TO WS-LETAT-NOMC-ED.
       7100-PREP-MAIN-ETATCLI-FIN.
           EXIT.
       7110-COUNT-DEM-DEB.
           ADD 1                        TO WS-NB-DEM.
       7110-COUNT-DEM-FIN.
           EXIT.
       7120-COUNT-ERR-DEB.
           ADD 1                        TO WS-NB-DEM-ERR.
       7120-COUNT-ERR-FIN.
           EXIT.
       7130-PREP-POINT-NUM-DEB.
           MOVE WS-DEM-CPT-DEB          TO FS-KEY-CPTE.
       7130-PREP-POINT-NUM-FIN.
           EXIT.
       7140-PREP-POINT-ALPHA-DEB.
           MOVE WS-DEM-CLI-DEB          TO FS-KEY-CLI.
       7140-PREP-POINT-ALPHA-FIN.
           EXIT.
       7150-INIT-COUNT-LINE-DEB.
           MOVE ZERO                    TO WS-COUNT-LINE.
       7150-INIT-COUNT-LINE-FIN.
           EXIT.
       7160-COUNT-LINE-DEB.
           ADD 1                        TO WS-COUNT-LINE.
       7160-COUNT-LINE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *8000 SECTION
      *
       8000-HEADER-ETAT-CLI-DEB.
           MOVE WS-LETAT-TIRET          TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6160-WRITE-F-ETAT-CLI-P-DEB
              THRU 6160-WRITE-F-ETAT-CLI-P-FIN.
           MOVE WS-LETAT-ENTETE         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-BLANC          TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-TITRE          TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-BLANC          TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-REFDEB         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-REFFIN         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-BLANC          TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-INTITULE       TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
           MOVE WS-LETAT-BLANC          TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
       8000-HEADER-ETAT-CLI-FIN.
           EXIT.
       8010-MAIN-ETAT-CLI-DEB.
           MOVE WS-LETAT-DETAIL         TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
       8010-MAIN-ETAT-CLI-FIN.
           EXIT.
       8020-FOOTER-ETAT-CLI-DEB.
           MOVE WS-LETAT-TIRET          TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
       8020-FOOTER-ETAT-CLI-FIN.
           EXIT.
       8060-EMPTY-ETAT-CLI-DEB.
           MOVE WS-LETAT-EMPTY          TO FS-BUFF-F-ETAT-CLI-S.
           PERFORM 6140-WRITE-F-ETAT-CLI-S-DEB
              THRU 6140-WRITE-F-ETAT-CLI-S-FIN.
       8060-EMPTY-ETAT-CLI-FIN.
           EXIT.
       8030-HEADER-ETAT-ANO-DEB.
           MOVE WS-LANO-ASTER           TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6150-WRITE-F-ETAT-ANO-P-DEB
              THRU 6150-WRITE-F-ETAT-ANO-P-FIN.
           MOVE WS-LANO-TITRE           TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-ASTER           TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
       8030-HEADER-ETAT-ANO-FIN.
           EXIT.
       8040-MAIN-ETAT-ANO-DEB.
           MOVE WS-LANO-ERREUR          TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-ENR1            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-ENR2            TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
           MOVE WS-LANO-INTERL          TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
       8040-MAIN-ETAT-ANO-FIN.
           EXIT.
       8050-FOOTER-ETAT-ANO-DEB.
           MOVE WS-LANO-ASTER           TO FS-BUFF-F-ETAT-ANO-S.
           PERFORM 6130-WRITE-F-ETAT-ANO-S-DEB
              THRU 6130-WRITE-F-ETAT-ANO-S-FIN.
       8050-FOOTER-ETAT-ANO-FIN.
           EXIT.

      *ECRIT LE COMPTE RENDU D EXECUTION
       8999-STATISTIQUES-DEB.
           DISPLAY WS-LCRE-ASTER.
           DISPLAY WS-LCRE-TITRE.
           DISPLAY WS-LCRE-ASTER.
           MOVE 'NOMBRE TOTAL DE DEMANDES'
                                        TO WS-LCRE-DET-LIB-ED.
           MOVE WS-NB-DEM               TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE DEMANDES ERRONEES'
                                        TO WS-LCRE-DET-LIB-ED.
           MOVE WS-NB-DEM-ERR           TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
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
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO511         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO511        *'.
           DISPLAY '*==============================================*'.
           MOVE 12                      TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.


