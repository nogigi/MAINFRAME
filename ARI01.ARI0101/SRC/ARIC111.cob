      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIC111                                   *
      *  NOM DU REDACTEUR : BAUDELET WILLIAM                          *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 22/05/2024                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * PROGRAMME DE GESTION DU MENU PRINCIPALE                       *
      * AFFICHAGE ET LECTURE DES MAPS                                 *
      * GESTION DES COMPORTEMENTS PF3,CLEAR-SCREEN,CHAMP NON RENSEIGNE*
      * GESTION DU CHAMP MCHOIX POUR UN FUTUR APPEL DE SOUS PROGRAMME *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   §          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * JJ/MM/SSAA    §                                               *
      *               §                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID. ARIC111.
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
           COPY ARIN111.
      * VARIABLE MSG
           COPY TABMSG.
      * VARIABLE COMMAREA
           COPY COMMAREA.
      *GESTION DATE
       01 WS-DATEJ                     PIC X(10).
      *
       01 WS-DATE-CICS                 PIC S9(15)     COMP-3.
      *VARIABLE GESTION CICS
       01 WS-RESP                      PIC S9(4)      COMP.
       01 WS-MAP                       PIC X(7)       VALUE 'ARIM111'.
       01 WS-MAPSET                    PIC X(7)       VALUE 'ARIN111'.
       01 WS-MSG-ERR                   PIC X(80).
      *TABLEAU DE PGM
       01 WS-SS-PRG.
          05 WS-NAME                   PIC X(6)       VALUE 'ARIC11'.
          05 WS-NUMBER                 PIC X          VALUE '1'.
          05 FILLER                    PIC X          VALUE SPACE.
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
      *   composant principal    0000                                 *
      *---------------------------------------------------------------*
       0000-PROGRAMME-DEB.
      * GAUCHE
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
      * AS 1000/1010 | SI PREMIERE EXEC -> INIT
           IF EIBCALEN = 0
                  PERFORM 1000-INIT-SCREEN-DEB
                     THRU 1000-INIT-SCREEN-FIN
           ELSE
                  PERFORM 1010-N-FOIS-DEB
                     THRU 1010-N-FOIS-FIN
           END-IF.
      * DROITE
           PERFORM 9999-FIN-RTRANSID-DEB
              THRU 9999-FIN-RTRANSID-FIN.
       0000-PROGRAMME-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   1000       *
      *---------------------------------------------------------------*
      * INITIALISE LES CHAMPS TEXTE A LEUR VALEUR                     *
      * SEND LA MAP POUR LA 1er FOIS                                  *
      *---------------------------------------------------------------*
       1000-INIT-SCREEN-DEB.
      * INIT LA DATE
           MOVE LOW-VALUE                        TO WS-COMMAREA.
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
           IF WS-AIG >= 2 AND WS-AIG <= 7
      * CALL LE WS-SS-PRG(WS-AIG)
              PERFORM 7140-AIG-DEB
                 THRU 7140-AIG-DEB
              PERFORM 9000-APPEL-SPG-DEB
                 THRU 9000-APPEL-SPG-FIN
           ELSE
              PERFORM 2000-MAP-1-DEB
                 THRU 2000-MAP-1-FIN
           END-IF.
       1010-N-FOIS-FIN.
           EXIT.
       2000-MAP-1-DEB.
           EVALUATE EIBAID
               WHEN DFHPF3
               PERFORM 3020-PF3-DEB
                  THRU 3020-PF3-FIN
               WHEN DFHENTER
               PERFORM 3000-ENTER-DEB
                  THRU 3000-ENTER-FIN
               WHEN DFHCLEAR
               PERFORM 3010-CLEAR-DEB
                  THRU 3010-CLEAR-FIN
               WHEN OTHER
               PERFORM 3030-OTHER-DEB
                  THRU 3030-OTHER-FIN
           END-EVALUATE.
       2000-MAP-1-FIN.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   2000       *
      *---------------------------------------------------------------*
      * gestion input :  ENTER                                        *
      *---------------------------------------------------------------*
       3000-ENTER-DEB.
           PERFORM 6010-INPUT-START-DEB
              THRU 6010-INPUT-START-FIN.
      * INIT LA DATE
           PERFORM 7010-INIT-DATE-DEB
              THRU 7010-INIT-DATE-FIN.
           PERFORM 7020-INIT-TEXT-DEB
              THRU 7020-INIT-TEXT-FIN.
      * *AS MAPFAIL
           IF WS-RESP = DFHRESP(MAPFAIL)
                  PERFORM 4000-MAPFAIL-DEB
                     THRU 4000-MAPFAIL-FIN
           ELSE
                  PERFORM 4010-CHOIX-OK-DEB
                     THRU 4010-CHOIX-OK-FIN
           END-IF.
       3000-ENTER-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   2010       *
      *---------------------------------------------------------------*
      * GESTION INPUT CLEAR (alt+c)                                   *
      *---------------------------------------------------------------*
       3010-CLEAR-DEB.
      * INIT LA DATE & MAP
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7010-INIT-DATE-DEB
              THRU 7010-INIT-DATE-FIN.
           PERFORM 7020-INIT-TEXT-DEB
              THRU 7020-INIT-TEXT-FIN.
      * ADD MSG "NE PAS CLEAR"
           PERFORM 7040-MSG-CLEAR-DEB
              THRU 7040-MSG-CLEAR-FIN.
      * SEND MAP
           PERFORM 6000-SEND-MAP-DEB
              THRU 6000-SEND-MAP-FIN.
       3010-CLEAR-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   2020       *
      *---------------------------------------------------------------*
      * GESTION INPUT PF3 (F3) RETURN                                 *
      *---------------------------------------------------------------*
       3020-PF3-DEB.
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
       3020-PF3-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   2030       *
      *---------------------------------------------------------------*
      * GESTION INPUT AUTRE                                           *
      *---------------------------------------------------------------*
       3030-OTHER-DEB.
           PERFORM 7030-MSG-OTHER-2000-DEB
              THRU 7030-MSG-OTHER-2000-FIN.
           PERFORM 6020-SEND-MAP-DATAONLY-DEB
              THRU 6020-SEND-MAP-DATAONLY-FIN.
       3030-OTHER-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   3000       *
      *---------------------------------------------------------------*
      * GESTION EN CAS DE MAPFAIL                                     *
      *---------------------------------------------------------------*
       4000-MAPFAIL-DEB.
           PERFORM 7050-MSG-CHAMP-VIDE-DEB
              THRU 7050-MSG-CHAMP-VIDE-FIN.
           PERFORM 6020-SEND-MAP-DATAONLY-DEB
              THRU 6020-SEND-MAP-DATAONLY-FIN.
       4000-MAPFAIL-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   3010       *
      *---------------------------------------------------------------*
      * GESTION INPUT VALIDE  (ENTER)                                 *
      *---------------------------------------------------------------*
       4010-CHOIX-OK-DEB.
      *SI MCHOIXI EST ENTRE 1 ET 6
           IF  MCHOIXI >= '1' AND MCHOIXI <= '6'
               PERFORM 5000-CHOIX-VALIDE-DEB
                  THRU 5000-CHOIX-VALIDE-FIN
           ELSE
      * SINON ERREUR
               PERFORM 5010-OTHER-DEB
                  THRU 5010-OTHER-FIN
           END-IF.
           IF ws-AIG >= 2 AND ws-AIG <= 7
              PERFORM 9000-APPEL-SPG-DEB
                 THRU 9000-APPEL-SPG-FIN
           end-if.
           PERFORM 6020-SEND-MAP-DATAONLY-DEB
              THRU 6020-SEND-MAP-DATAONLY-FIN.
       4010-CHOIX-OK-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   4000       *
      *---------------------------------------------------------------*
      * GESTION CHAMP MCHOIX (valeur valide entre 1 et 6)             *
      *---------------------------------------------------------------*
       5000-CHOIX-VALIDE-DEB.
           EVALUATE MCHOIXI
              WHEN  '1'
              PERFORM 7080-CODE-1-DEB
                 THRU 7080-CODE-1-FIN
              WHEN  '2'
              PERFORM 7090-CODE-2-DEB
                 THRU 7090-CODE-2-FIN
              WHEN  '3'
              PERFORM 7100-CODE-3-DEB
                 THRU 7100-CODE-3-FIN
              WHEN  '4'
              PERFORM 7110-CODE-4-DEB
                 THRU 7110-CODE-4-DEB
              WHEN  '5'
              PERFORM 7120-CODE-5-DEB
                 THRU 7120-CODE-5-DEB
              WHEN  '6'
              PERFORM 7130-CODE-6-DEB
                 THRU 7130-CODE-6-DEB
           END-EVALUATE.
           EXIT.
       5000-CHOIX-VALIDE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME   4010       *
      *---------------------------------------------------------------*
      * VALEUR MCHOIX non compris entre 1 et 6                        *
      *---------------------------------------------------------------*
       5010-OTHER-DEB.
           PERFORM 7060-MSG-OTHER-4000-DEB
              THRU 7060-MSG-OTHER-4000-FIN.
       5010-OTHER-FIN.
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
                     FROM    (ARIM111O)
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
                      INTO    (ARIM111I)
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
                     FROM    (ARIM111O)
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
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
       7000-INIT-MAP-DEB.
            MOVE DFHCOMMAREA                     TO WS-COMMAREA.
            MOVE LOW-VALUE                       TO ARIM111O.
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
           MOVE EIBTASKN                         TO MTASKO.
           MOVE EIBTRNID                         TO MTRANO.
           MOVE EIBTRMID                         TO MTERMO.
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
           MOVE LOW-VALUE                        TO ARIM111O.
           MOVE EIBTASKN                         TO MTASKO.
           MOVE WS-MSG(24)                       TO MMSGO.
       7050-MSG-CHAMP-VIDE-FIN.
           EXIT.
       7070-RESET-MCHOIXO-DEB.
           MOVE LOW-VALUE                        TO MCHOIXO.
       7070-RESET-MCHOIXO-FIN.
           EXIT.
      * 7000 AM
       7060-MSG-OTHER-4000-DEB.
           MOVE LOW-VALUE                        TO MCHOIXO.
           MOVE WS-MSG(25)                       TO MMSGO.
       7060-MSG-OTHER-4000-FIN.
           EXIT.
       7080-CODE-1-DEB.
           MOVE LOW-VALUE                        TO MCHOIXO.
           MOVE 2                                TO WS-AIG.
           MOVE WS-AIG                           TO WS-NUMBER.
           MOVE 'MCHOIX : 1'                     TO MMSGO.
       7080-CODE-1-FIN.
           EXIT.
       7090-CODE-2-DEB.
           MOVE LOW-VALUE                        TO MCHOIXO.
           MOVE 3                                TO WS-AIG.
           MOVE WS-AIG                           TO WS-NUMBER.
           MOVE 'MCHOIX : 2'                     TO MMSGO.
       7090-CODE-2-FIN.
           EXIT.
       7100-CODE-3-DEB.
           MOVE 4                                TO WS-AIG.
           MOVE WS-AIG                           TO WS-NUMBER.
           MOVE LOW-VALUE                        TO MCHOIXO.
           MOVE 'MCHOIX : 3'                     TO MMSGO.
       7100-CODE-3-FIN.
           EXIT.
       7110-CODE-4-DEB.
           MOVE 5                                TO WS-AIG.
           MOVE WS-AIG                           TO WS-NUMBER.
           MOVE LOW-VALUE                        TO MCHOIXO.
           MOVE 'MCHOIX : 4'                     TO MMSGO.
       7110-CODE-4-FIN.
           EXIT.
       7120-CODE-5-DEB.
           MOVE 6                                TO WS-AIG.
           MOVE WS-AIG                           TO WS-NUMBER.
           MOVE LOW-VALUE                        TO MCHOIXO.
           MOVE 'MCHOIX : 5'                     TO MMSGO.
       7120-CODE-5-FIN.
           EXIT.
       7130-CODE-6-DEB.
           MOVE 7                                TO WS-AIG.
           MOVE WS-AIG                           TO WS-NUMBER.
           MOVE LOW-VALUE                        TO MCHOIXO.
           MOVE 'MCHOIX : 6'                     TO MMSGO.
       7130-CODE-6-FIN.
           EXIT.
       7140-AIG-DEB.
           MOVE WS-AIG                           TO WS-NUMBER.
       7140-AIG-FIN.
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
       9000-APPEL-SPG-DEB.
           EXEC CICS XCTL PROGRAM(WS-SS-PRG)
                          COMMAREA(WS-COMMAREA)
           END-EXEC.
       9000-APPEL-SPG-FIN.
           EXIT.
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
