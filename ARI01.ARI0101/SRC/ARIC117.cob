
      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIC117                                   *
      *  NOM DU REDACTEUR : BAUDELET WILLIAM                          *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 03/06/2024                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * PROGRAMME DE GESTION DU MENU 6                                *
      * AFFICHAGE ET LECTURE DES MAPS                                 *
      * GESTION DES COMPORTEMENTS PF3,CLEAR-SCREEN,CHAMP NON RENSEIGNE*
      * GESTION DU CHAMP MCODE POUR delet UN enregistrement dans le   *
      * fichier ARTICLE                                               *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   �          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * JJ/MM/SSAA    �                                               *
      *               �                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID. ARIC117.
      *
      *===============================================================*
      *           NE PAS MODIFIER LA PARTIE ENCADREE DU CODE          *
      *===============================================================*
      *
      *                  ==============================               *
      *=================<    ENVIRONMENT    DIVISION   >==============*
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
           DECIMAL-POINT IS COMMA.
      *--------------
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
      *=================<         DATA      DIVISION   >==============*
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
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
      *===============================================================*
      *             COPY - INSERTION DE SEQUENCES DE SOURCE           *
      *===============================================================*
      * TEST DES TOUCHES FONCTION
           COPY DFHAID.
      * MODIFICATION DYNAMIQUE DES ATTRIBUTS DE MAP
           COPY DFHBMSCA.
      * VARIABLES DE LA MAP
           COPY ARIN117.
      * VARIABLE MSG
           COPY TABMSG.
      * VARIABLE COMMAREA
           COPY COMMAREA.
      * DESCRIPTION ENREGISTREMENT ARTICLE
           COPY ARTICLE.
      *GESTION DATE
       01 WS-DATEJ                     PIC X(10).
      *
       01 WS-DATE-CICS                 PIC S9(15)     COMP-3.
      *ENG VARIABLE ED
      *VARIABLE GESTION CICS
       01 WS-RESP                      PIC S9(4)      COMP.
       01 WS-MAP                       PIC X(7)       VALUE 'ARIM117'.
       01 WS-MAPSET                    PIC X(7)       VALUE 'ARIN117'.
       01 WS-MSG-ERR                   PIC X(80).
      *================
       LINKAGE SECTION.
      *================
      *
       01 DFHCOMMAREA                  PIC X(4096).
      *
      *                  ==============================               *
      *=================<    PROCEDURE      DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      ********************
       PROCEDURE DIVISION.
      ********************
      *
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE ALGORITHMIQUE DU PROGRAMME      *
      *---------------------------------------------------------------*
      *    1 : LES COMPOSANTS DU DIAGRAMME SONT CODES A L'AIDE DE     *
      *        DEUX PARAGRAPHES  XXYY-COMPOSANT-DEB                   *
      *                          XXYY-COMPOSANT-FIN                   *
      *    2 : XX REPRESENTE LE NIVEAU HIERARCHIQUE                   *
      *        YY DIFFERENCIE LES COMPOSANTS DE MEME NIVEAU           *
      *    3 : TOUT COMPOSANT EST PRECEDE D'UN CARTOUCHE DE           *
      *        COMMENTAIRE QUI EXPLICITE LE ROLE DU COMPOSANT         *
      *===============================================================*
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME              *
      *---------------------------------------------------------------*
      *   COMPOSANT PRINCIPAL    0000                                 *
      *---------------------------------------------------------------*
       0000-PROGRAMME-DEB.
      * GAUCHE
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7140-INIT-COMMAREA-DEB
              THRU 7140-INIT-COMMAREA-FIN.
      * AS 1000/1010 | SI PREMIERE EXEC -> INIT
           EVALUATE TRUE
               WHEN INIT-TRT
                  PERFORM 1000-INIT-SCREEN-DEB
                     THRU 1000-INIT-SCREEN-FIN
               WHEN AFF-AIDE
                  PERFORM 1020-HELP-DEB
                     THRU 1020-HELP-FIN
               WHEN AFF-MAP
                  PERFORM 1010-N-FOIS-DEB
                     THRU 1010-N-FOIS-FIN
               when CONFIRMATION
                  PERFORM 1030-CONFIRMATION-DEB
                     THRU 1030-CONFIRMATION-FIN
           END-EVALUATE.
      * DROITE
           PERFORM 9999-FIN-RTRANSID-DEB
              THRU 9999-FIN-RTRANSID-FIN.
       0000-PROGRAMME-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   1000       *
      *---------------------------------------------------------------*
      * INITIALISE LES CHAMPS TEXTE A LEUR VALEUR                     *
      * SEND LA MAP POUR LA 1ER FOIS                                  *
      *---------------------------------------------------------------*
       1000-INIT-SCREEN-DEB.
      * INIT LA DATE
           PERFORM 7010-INIT-DATE-DEB
              THRU 7010-INIT-DATE-FIN.
      *INIT LE TEXTE
           PERFORM 7020-INIT-TEXT-DEB
              THRU 7020-INIT-TEXT-FIN.
      *SEND LA MAP
           PERFORM 6000-SEND-MAP-DEB
              THRU 6000-SEND-MAP-FIN.
       1000-INIT-SCREEN-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   1010       *
      *---------------------------------------------------------------*
      * GESTION DES INPUT UTILISATEUR PF3 ENTER CLEAR ET AUTRE        *
      *---------------------------------------------------------------*
       1010-N-FOIS-DEB.
      * AM 2000
           EVALUATE EIBAID
               WHEN DFHPF3
               PERFORM 2020-PF3-DEB
                  THRU 2020-PF3-FIN
               WHEN DFHENTER
               PERFORM 2000-ENTER-DEB
                  THRU 2000-ENTER-FIN
               WHEN DFHPF1
               PERFORM 2040-PF1-DEB
                  THRU 2040-PF1-FIN
               WHEN DFHCLEAR
               PERFORM 2010-CLEAR-DEB
                  THRU 2010-CLEAR-FIN
               WHEN OTHER
               PERFORM 2030-OTHER-DEB
                  THRU 2030-OTHER-FIN
           END-EVALUATE.
       1010-N-FOIS-FIN.
           EXIT.

       1020-HELP-DEB.
           PERFORM 7010-INIT-DATE-DEB
              THRU 7010-INIT-DATE-FIN.
      *INIT LE TEXTE
           PERFORM 7020-INIT-TEXT-DEB
              THRU 7020-INIT-TEXT-FIN.
           PERFORM 7210-LOAD-ENG-DEB
              THRU 7210-LOAD-ENG-FIN.
      * Si WS-ENG-SAVE EST VIDE ne pas afficher les champ
           IF MCODEO = space
              PERFORM 7160-CLEAR-CHAMP-DEB
                 THRU 7160-CLEAR-CHAMP-FIN
           ELSE
              PERFORM 8000-DISP-ENG-ART-DEB
                 THRU 8000-DISP-ENG-ART-FIN
           END-IF.
           PERFORM 6000-SEND-MAP-DEB
              THRU 6000-SEND-MAP-FIN.
       1020-HELP-FIN.
           EXIT.
       1030-CONFIRMATION-DEB.
           EVALUATE EIBAID
               WHEN DFHPF3
               PERFORM 2040-PF3-CANCEL-DEB
                  THRU 2040-PF3-CANCEL-FIN
               WHEN DFHENTER
               PERFORM 2050-CONFIRM-DELET-DEB
                  THRU 2050-CONFIRM-DELET-FIN
               WHEN OTHER
               PERFORM 2060-CONFIRM-OTHER-DEB
                  THRU 2060-CONFIRM-OTHER-FIN
           END-EVALUATE.
       1030-CONFIRMATION-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   2000       *
      *---------------------------------------------------------------*
      * GESTION INPUT :  ENTER                                        *
      *---------------------------------------------------------------*
       2040-PF3-CANCEL-DEB.
           PERFORM 7010-INIT-DATE-DEB
              THRU 7010-INIT-DATE-FIN.
           PERFORM 7020-INIT-TEXT-DEB
              THRU 7020-INIT-TEXT-FIN.
           PERFORM 7200-trt-pf3-c-deb
              THRU 7200-trt-pf3-c-FIN.
           PERFORM 6000-SEND-MAP-DEB
              THRU 6000-SEND-MAP-FIN.
       2040-PF3-CANCEL-FIN.
           EXIT.
       2050-CONFIRM-DELET-DEB.
           PERFORM 7010-INIT-DATE-DEB
              THRU 7010-INIT-DATE-FIN.
           PERFORM 7020-INIT-TEXT-DEB
              THRU 7020-INIT-TEXT-FIN.
           PERFORM 6050-DELET-DEB
              THRU 6050-DELET-FIN.
           PERFORM 7210-trt-confirm-del-deb
              THRU 7210-trt-confirm-del-FIN.
           PERFORM 6000-SEND-MAP-DEB
              THRU 6000-SEND-MAP-FIN.
       2050-CONFIRM-DELET-FIN.
           EXIT.
       2060-CONFIRM-OTHER-DEB.
           PERFORM 7010-INIT-DATE-DEB
              THRU 7010-INIT-DATE-FIN.
           PERFORM 7020-INIT-TEXT-DEB
              THRU 7020-INIT-TEXT-FIN.
           PERFORM 7220-trt-confirm-other-deb
              THRU 7220-trt-confirm-other-fin.
           MOVE WS-CODE                          TO MCODEO.
           MOVE WS-MSG(29)                       TO MMSGO.
           PERFORM 6000-SEND-MAP-DEB
              THRU 6000-SEND-MAP-FIN.
       2060-CONFIRM-OTHER-FIN.
           EXIT.
       2000-ENTER-DEB.
           PERFORM 6010-INPUT-START-DEB
              THRU 6010-INPUT-START-FIN.
      * INIT LA DATE
           PERFORM 7010-INIT-DATE-DEB
              THRU 7010-INIT-DATE-FIN.
           PERFORM 7020-INIT-TEXT-DEB
              THRU 7020-INIT-TEXT-FIN.
      * *AS MAPFAIL
           IF WS-RESP = DFHRESP(MAPFAIL)
                  PERFORM 3000-MAPFAIL-DEB
                     THRU 3000-MAPFAIL-FIN
           ELSE
                  PERFORM 3010-CHOIX-OK-DEB
                     THRU 3010-CHOIX-OK-FIN
           END-IF.
       2000-ENTER-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   2010       *
      *---------------------------------------------------------------*
      * GESTION INPUT CLEAR (ALT+C)                                   *
      *---------------------------------------------------------------*
       2010-CLEAR-DEB.
      * INIT LA DATE & MAP
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7010-INIT-DATE-DEB
              THRU 7010-INIT-DATE-FIN.
           PERFORM 7020-INIT-TEXT-DEB
              THRU 7020-INIT-TEXT-FIN.
      * ADD MSG "NE PAS CLEAR"
           PERFORM 7210-LOAD-ENG-DEB
              THRU 7210-LOAD-ENG-FIN.
      * Si WS-ENG-SAVE EST VIDE ne pas afficher les champ
           IF MCODEO = space
              PERFORM 7160-CLEAR-CHAMP-DEB
                 THRU 7160-CLEAR-CHAMP-FIN
           ELSE
              PERFORM 8000-DISP-ENG-ART-DEB
                 THRU 8000-DISP-ENG-ART-FIN
           END-IF.
           PERFORM 7040-MSG-CLEAR-DEB
              THRU 7040-MSG-CLEAR-FIN.
      * SEND MAP
           PERFORM 6000-SEND-MAP-DEB
              THRU 6000-SEND-MAP-FIN.
       2010-CLEAR-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   2020       *
      *---------------------------------------------------------------*
      * GESTION INPUT PF3 (F3) RETURN                                 *
      *---------------------------------------------------------------*
       2020-PF3-DEB.
           PERFORM 7170-RETURN-PGM-1-DEB
              THRU 7170-RETURN-PGM-1-FIN.
           PERFORM 9000-APPEL-SPG-DEB
              THRU 9000-APPEL-SPG-FIN.
       2020-PF3-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   2030       *
      *---------------------------------------------------------------*
      * GESTION INPUT AUTRE                                           *
      *---------------------------------------------------------------*
       2030-OTHER-DEB.
           PERFORM 7030-MSG-OTHER-2000-DEB
              THRU 7030-MSG-OTHER-2000-FIN.
           PERFORM 6020-SEND-MAP-DATAONLY-DEB
              THRU 6020-SEND-MAP-DATAONLY-FIN.
       2030-OTHER-FIN.
           EXIT.
       2040-PF1-DEB.
           PERFORM 7190-SET-A-CODE-DEB
              THRU 7190-SET-A-CODE-DEB.
           PERFORM 6040-SEND-HELP-DEB
              THRU 6040-SEND-HELP-FIN.
       2040-PF1-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   3000       *
      *---------------------------------------------------------------*
      * GESTION EN CAS DE MAPFAIL                                     *
      *---------------------------------------------------------------*
       3000-MAPFAIL-DEB.
           PERFORM 7160-CLEAR-CHAMP-DEB
              THRU 7160-CLEAR-CHAMP-FIN.
           PERFORM 7100-RESET-SAVE-DEB
              THRU 7100-RESET-SAVE-FIN.
           PERFORM 7050-MSG-CHAMP-VIDE-DEB
              THRU 7050-MSG-CHAMP-VIDE-FIN.
           PERFORM 6020-SEND-MAP-DATAONLY-DEB
              THRU 6020-SEND-MAP-DATAONLY-FIN.
       3000-MAPFAIL-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   3010       *
      *---------------------------------------------------------------*
      * GESTION INPUT VALIDE  (ENTER)                                 *
      *---------------------------------------------------------------*
       3010-CHOIX-OK-DEB.
           PERFORM 6030-READ-DEB
              THRU 6030-READ-FIN.
           PERFORM 7200-SAVE-ENG-DEB
              THRU 7200-SAVE-ENG-FIN.
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                PERFORM 4000-FOUND-DEB
                   THRU 4000-FOUND-FIN
               WHEN DFHRESP(NOTFND)
                PERFORM 4010-NOT-FOUND-DEB
                   THRU 4010-NOT-FOUND-FIN
           END-EVALUATE.
       3010-CHOIX-OK-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   4000       *
      *---------------------------------------------------------------*
      * GESTION CHAMP MCHOIX (VALEUR VALIDE ENTRE 1 ET 6)             *
      *---------------------------------------------------------------*
       4000-FOUND-DEB.
           PERFORM 8000-DISP-ENG-ART-DEB
              THRU 8000-DISP-ENG-ART-FIN.
           PERFORM 7180-MSG-FOUND-DEB
              THRU 7180-MSG-FOUND-FIN.
           PERFORM 6020-SEND-MAP-DATAONLY-DEB
              THRU 6020-SEND-MAP-DATAONLY-FIN.
       4000-FOUND-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   4010       *
      *---------------------------------------------------------------*
      * VALEUR MCHOIX NON COMPRIS ENTRE 1 ET 6                        *
      *---------------------------------------------------------------*
       4010-NOT-FOUND-DEB.
           PERFORM 7160-CLEAR-CHAMP-DEB
              THRU 7160-CLEAR-CHAMP-FIN.
           PERFORM 7100-RESET-SAVE-DEB
              THRU 7100-RESET-SAVE-FIN.
           PERFORM 6020-SEND-MAP-DATAONLY-DEB
              THRU 6020-SEND-MAP-DATAONLY-FIN.
       4010-NOT-FOUND-FIN.
           EXIT.
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS PROGRAMMES         *
      *   9999-  : FIN DE PROGRAMME                                   *
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   6000       *
      *---------------------------------------------------------------*
      * SEND LA MAP ET ERASE TOUT (ERASE)                             *
      *---------------------------------------------------------------*
       6000-SEND-MAP-DEB.
           EXEC CICS
                SEND MAP     (WS-MAP)
                     MAPSET  (WS-MAPSET)
                     FROM    (ARIM117O)
                     ERASE
                     RESP    (WS-RESP)
           END-EXEC.
           IF WS-RESP NOT = DFHRESP(NORMAL)
                MOVE "6000 ERREUR SEND MAP"      TO WS-MSG-ERR
                PERFORM 9999-ERREUR-PROGRAMME-DEB
                   THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-SEND-MAP-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   6010       *
      *---------------------------------------------------------------*
      * RECEIVE LA MAP UTILISATEUR  ARIM111I (INPUT)                  *
      *---------------------------------------------------------------*
       6010-INPUT-START-DEB.
           EXEC CICS
              RECEIVE MAP     (WS-MAP)
                      MAPSET  (WS-MAPSET)
                      INTO    (ARIM117I)
                      RESP    (WS-RESP)
           END-EXEC.
           IF WS-RESP NOT = (DFHRESP(NORMAL) AND DFHRESP(MAPFAIL))
                MOVE '6010 - ERREUR RECEVE MAP'  TO WS-MSG-ERR
                PERFORM 9999-ERREUR-PROGRAMME-DEB
                   THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-INPUT-START-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   6020       *
      *---------------------------------------------------------------*
      * SEND LA MAP ET ERASE SEULEMENT LES ELEMENTS UNPROTECTED       *
      *  (ERASEAUP)                                                   *
      *---------------------------------------------------------------*
       6020-SEND-MAP-DATAONLY-DEB.
           EXEC CICS
                SEND MAP     (WS-MAP)
                     MAPSET  (WS-MAPSET)
                     FROM    (ARIM117O)
                     ERASEAUP
                     DATAONLY
                     RESP    (WS-RESP)
           END-EXEC.
           IF WS-RESP NOT = DFHRESP(NORMAL)
                MOVE '6020 - ERREUR SEND MAP DATAONLY'
                                                 TO WS-MSG-ERR
                PERFORM 9999-ERREUR-PROGRAMME-DEB
                   THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-SEND-MAP-DATAONLY-FIN.
           EXIT.
       6030-READ-DEB.
           EXEC CICS
               READ FILE('   ')
                    RIDFLD(MCODEO)
                    INTO(WS-ART-ENR)
                    RESP(WS-RESP)
           END-EXEC.
           IF NOT (WS-RESP = DFHRESP(NORMAL) OR DFHRESP(NOTFND))
                MOVE '6030 - ERREUR CICS READ :'
                                                 TO WS-MSG-ERR
                PERFORM 9999-ERREUR-PROGRAMME-DEB
                   THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6030-READ-FIN.
           EXIT.
       6050-DELET-DEB.
           EXEC CICS
               DELET FILE('ART0101')
                    RIDFLD(ws-code)
                    RESP(WS-RESP)
           END-EXEC.
           IF NOT (WS-RESP = DFHRESP(NORMAL) OR DFHRESP(NOTFND))
                MOVE '6050 - ERREUR CICS DELET :'
                                                 TO WS-MSG-ERR
                PERFORM 9999-ERREUR-PROGRAMME-DEB
                   THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6050-DELET-FIN.
           EXIT.
       6040-SEND-HELP-DEB.
           EXEC CICS SEND MAP    ('ARIMHP2')
                          MAPSET ('ARIN112')
                          FROM   (ARIM117O)
                          ERASE
                          RESP   (WS-RESP)
           END-EXEC.
           IF NOT (WS-RESP = DFHRESP(NORMAL))
                MOVE '6040 - ERREUR CICS SEND HELP'
                                                 TO WS-MSG-ERR
                PERFORM 9999-ERREUR-PROGRAMME-DEB
                   THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-SEND-HELP-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
       7200-SAVE-ENG-DEB.
      *SAVE WS ART TO COMMAREA
           MOVE WS-ART-ENR                       TO WS-ENR-SAV.
       7200-SAVE-ENG-FIN.
           EXIT.
       7210-LOAD-ENG-DEB.
      *SAVE WS ART TO COMMAREA
           MOVE WS-ENR-SAV                       TO WS-ART-ENR.
       7210-LOAD-ENG-FIN.
           EXIT.
       7100-RESET-SAVE-DEB.
           MOVE LOW-VALUE                        TO WS-ENR-SAV.
           MOVE space                            TO MCODEO.
           MOVE ZERO                             TO WS-FOUR.
           PERFORM 7160-CLEAR-CHAMP-DEB
              THRU 7160-CLEAR-CHAMP-FIN.
       7100-RESET-SAVE-FIN.
           EXIT.
       7000-INIT-MAP-DEB.
           MOVE LOW-VALUE                        TO ARIM117O.
       7000-INIT-MAP-FIN.
           EXIT.
       7010-INIT-DATE-DEB.
           EXEC CICS
               ASKTIME ABSTIME(WS-DATE-CICS)
           END-EXEC.
           EXEC CICS
               FORMATTIME ABSTIME  (WS-DATE-CICS)
                          YYYYMMDD (WS-DATEJ)
                          DATESEP
           END-EXEC.
       7010-INIT-DATE-FIN.
           EXIT.
       7020-INIT-TEXT-DEB.
           MOVE EIBTRMID                         TO MTERMO.
           MOVE EIBTRNID                         TO MTRANO.
           MOVE EIBTASKN                         TO MTASKO.
           MOVE WS-MSG(3)                        TO MMSGO.
           MOVE 'M'                              TO WS-TAFF.
           MOVE WS-DATEJ                         TO MDATEO.
       7020-INIT-TEXT-FIN.
           EXIT.
       7030-MSG-OTHER-2000-DEB.
           MOVE EIBTASKN                         TO MTASKO.
           MOVE WS-MSG(1)                        TO MMSGO.
       7030-MSG-OTHER-2000-FIN.
           EXIT.
       7040-MSG-CLEAR-DEB.
           MOVE WS-MSG(2)                        TO MMSGO.
       7040-MSG-CLEAR-FIN.
           EXIT.
       7050-MSG-CHAMP-VIDE-DEB.
           MOVE EIBTASKN                         TO MTASKO.
           PERFORM 7160-CLEAR-CHAMP-DEB
              THRU 7160-CLEAR-CHAMP-FIN.
           MOVE WS-MSG(6)                        TO MMSGO.
       7050-MSG-CHAMP-VIDE-FIN.
           EXIT.
      * 7000 AM
       7060-MSG-OTHER-4000-DEB.
           MOVE WS-MSG(25)                       TO MMSGO.
       7060-MSG-OTHER-4000-FIN.
           EXIT.
       7170-RETURN-PGM-1-DEB.
           MOVE '1'                              TO WS-AIG.
       7170-RETURN-PGM-1-FIN.
           EXIT.
       7140-INIT-COMMAREA-DEB.
           MOVE DFHCOMMAREA                      TO WS-COMMAREA.
           MOVE SPACE                            TO MMSGO.
       7140-INIT-COMMAREA-FIN.
           EXIT.
       7150-F1-DEB.
           MOVE 'A'                              TO WS-TAFF.
       7150-F1-FIN.
           EXIT.
       7180-MSG-FOUND-DEB.
           MOVE  'C'                             TO WS-TAFF.
           MOVE DFHBMPRO                         TO MCODEA.
           MOVE DFHBLUE                          TO MCODEA.
           MOVE WS-MSG(29)                       TO MMSGO.
       7180-MSG-FOUND-FIN.
           EXIT.
       7160-CLEAR-CHAMP-DEB.
           MOVE SPACE                            TO MCODEO.
           MOVE WS-MSG(27)                       TO MMSGO.
       7160-CLEAR-CHAMP-FIN.
           EXIT.
       7190-SET-A-CODE-DEB.
           MOVE 'A'                              TO WS-TAFF.
       7190-SET-A-CODE-FIN.
           EXIT.
       7200-trt-pf3-c-deb.
           MOVE WS-MSG(31)                       TO MMSGO.
           MOVE SPACE                            TO WS-CODE.
       7200-trt-pf3-c-fin.
           EXIT.
       7210-trt-confirm-del-deb.
           MOVE SPACE                            TO WS-CODE.
           MOVE WS-MSG(30)                       TO MMSGO.
       7210-trt-confirm-del-fin.
           EXIT.
       7220-trt-confirm-other-deb.
           MOVE DFHBMPRO                         TO MCODEA.
           MOVE DFHBLUE                          TO MCODEA.
           MOVE 'C'                              TO WS-TAFF.
       7220-trt-confirm-other-fin.
           EXIT.
      *DEBUT 8000
       8000-DISP-ENG-ART-DEB.
           MOVE WS-CODE                          TO MCODEO.
       8000-DISP-ENG-ART-FIN.
           EXIT.


      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT APPEL-SPG              *
      *---------------------------------------------------------------*
      * COMPOSANT EXECUTE LORS DE CHAQUE APPEL D'UN SOUS PROGRAMME    *
      * (APRES SAISIE D'UN CHOIX OU A CHAQUE BOUCLE SUR UN DES        *
      * TRAITEMENTS DEPENDANTS)                                       *
      * IL PERMET :                                                   *
      * ==> D'INITIALISER LE NOM DU SOUS PROGRAMME A APPELER          *
      * ==> DE DONNER DYNAMIQUEMENT LE CONTROLE PROGRAMME             *
      *     CORRESPONDANT EN LUI TRANSMETTANT UNE COMMAREA QUI PERMET *
      *     DE SAUVEGARDER LES DONNEES NECCESSAIRES A LA POURSUITE    *
      *     DU TRAITEMENT (PROGRAMMATION PSEUDO CONVERSATIONNELLE)    *
      *---------------------------------------------------------------*
      *
      *
      *---------------------------------------------------------------*
      *   9999-  : FIN DE PROGRAMME                                   *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
           EXEC CICS SEND
                     FROM (WS-MSG(26))
                     ERASE
           END-EXEC.
           EXEC CICS RETURN
           END-EXEC.
       9999-FIN-PROGRAMME-FIN.
           EXIT.
       9999-ERREUR-PROGRAMME-DEB.
           EXEC CICS SEND
                     FROM (WS-MSG-ERR)
                     ERASE
           END-EXEC.
           EXEC CICS RETURN
           END-EXEC.
       9999-ERREUR-PROGRAMME-FIN.
           EXIT.
       9000-APPEL-SPG-DEB.
           EXEC CICS XCTL PROGRAM('ARIC111')
          END-EXEC.
       9000-APPEL-SPG-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT FIN-RTRANSID           *
      *---------------------------------------------------------------*
      * COMPOSANT EXECUTE APRES CHAQUE AFFICHAGE POUR TERMINER LA     *
      * TRANSACTION DE FACON TEMPORAIRE.                              *
      * L'OPTION TRANSID INDIQUE LE CODE TRANSACTION QUI SERA UTILISE *
      * PAR CICS POUR REINITIALISER LA TRANSACTION (EIBTRNID CONTIENT *
      * LE DERNIER CODE UTILISE).                                     *
      * L'OPTION COMMAREA PERMET DE TRANSMETTRE UNE ZONE QUI PERMET   *
      * SAUVEGARDER DES DONNEES QUI SERONT RECUPEREES PAR LE PROGRAMME*
      * POUR LE COMPTE DE LA TRANSACTION QUI SERA REACTIVEE.          *
      *---------------------------------------------------------------*
      *
       9999-FIN-RTRANSID-DEB.
           EXEC CICS RETURN
                     TRANSID(EIBTRNID)
                     COMMAREA(WS-COMMAREA)
           END-EXEC.
       9999-FIN-RTRANSID-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ABEND-PRG              *
      *---------------------------------------------------------------*
      * COMPOSANT EXECUTE QUAND UNE ERREUR EST DETECTEE LORS DU       *
      * TEST SUR LE CONTEXTE D'EXECUTION (CODE DIFFERENT D'UNE BOUCLE *
      * SUR LA GESTION DU MENU OU SUR UN DES TRAITEMENTS DEPENDANTS). *
      *                                                               *
      *                           ATTENTION !                         *
      * AUCUN CODE (ABCODE) N'EST UTILISE POUR IDENTIFIE L'ABEND      *
      * (UNE SEULE CONDITION D'ABEND) ET L'OPTION NODUMP PERMET DE    *
      * SUPPRIMER L'IMPRESSION PAR DEFAUT D'UN DUMP.                  *
      *---------------------------------------------------------------*
      *
       9999-ABEND-PRG-DEB.
           EXEC CICS ABEND
                     NODUMP
           END-EXEC.
       9999-ABEND-PRG-FIN.
           EXIT.
