       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAJASSUR.

      *===============================================================*
      *  PROGRAMME : MAJASSUR                                         *
      *  ROLE      :                                                  *
      *    - Lire les mouvements d'un fichier d'entrée (ESDS)         *
      *    - Mettre à jour le fichier des assurés (KSDS)              *
      *    - Produire un état des anomalies (ETATANO)                 *
      *    - Produire un fichier de statistiques (STAT)               *
      *===============================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

      *---------------------------------------------------------------*
      * Indicateur de fin de lecture du fichier des mouvements        *
      *---------------------------------------------------------------*
       77  WS-EOF-MV                PIC X VALUE "N".
           88 FIN-MVTS                    VALUE "Y".
           88 PAS-FIN-MVTS                VALUE "N".

      *---------------------------------------------------------------*
      * Indicateur de fin de lecture du fichier des assurés           *
      *---------------------------------------------------------------*
       77  WS-EOF-AS                PIC X VALUE "N".
           88 FIN-ASSURE                  VALUE "Y".
           88 PAS-FIN-ASSURE              VALUE "N".

      *---------------------------------------------------------------*
      * Compteurs statistiques                                         *
      *---------------------------------------------------------------*
       01  WS-STAT-COUNTERS.
           05 WS-NB-ASS-LUS         PIC 9(5) VALUE 0.
           05 WS-NB-MVT-LUS         PIC 9(5) VALUE 0.
           05 WS-NB-ANOM            PIC 9(5) VALUE 0.
           05 WS-NB-CRE             PIC 9(5) VALUE 0.
           05 WS-NB-MOD             PIC 9(5) VALUE 0.
           05 WS-NB-SUP             PIC 9(5) VALUE 0.
           05 WS-NB-ANO-CODE        PIC 9(5) VALUE 0.
           05 WS-NB-ANO-CRE         PIC 9(5) VALUE 0.
           05 WS-NB-ANO-MAJ         PIC 9(5) VALUE 0.
           05 WS-NB-ANO-SUP         PIC 9(5) VALUE 0.

      *---------------------------------------------------------------*
      * Zones d'édition pour affichage / écriture des statistiques    *
      *---------------------------------------------------------------*
       01  WS-STAT-EDIT.
           05 ED-NB-ASS-LUS         PIC Z(4)9.
           05 ED-NB-MVT-LUS         PIC Z(4)9.
           05 ED-NB-ANOM            PIC Z(4)9.
           05 ED-NB-CRE             PIC Z(4)9.
           05 ED-NB-MOD             PIC Z(4)9.
           05 ED-NB-SUP             PIC Z(4)9.
           05 ED-NB-ANO-CODE        PIC Z(4)9.
           05 ED-NB-ANO-CRE         PIC Z(4)9.
           05 ED-NB-ANO-MAJ         PIC Z(4)9.
           05 ED-NB-ANO-SUP         PIC Z(4)9.

      *---------------------------------------------------------------*
      * Table des messages d'anomalie / statistiques                  *
      *---------------------------------------------------------------*
       COPY MESSAGES.

       01  WS-TABLE-MSG-TAB REDEFINES TABLE-MESSAGE.
           05 WS-MSG OCCURS 30 TIMES PIC X(60).

      *---------------------------------------------------------------*
      * Descriptions des enregistrements métier                       *
      *---------------------------------------------------------------*
       COPY WFMVTS.
       COPY WASSURE.

      *---------------------------------------------------------------*
      * Date système courante                                          *
      *---------------------------------------------------------------*
       01  WS-CURR-DT.
           05 WS-CD-YYYY            PIC 9(4).
           05 WS-CD-MM              PIC 9(2).
           05 WS-CD-DD              PIC 9(2).
           05 WS-CD-HH              PIC 9(2).
           05 WS-CD-MN              PIC 9(2).
           05 WS-CD-SS              PIC 9(2).
           05 WS-CD-HS              PIC 9(2).
           05 WS-CD-OFFSIGN         PIC X.
           05 WS-CD-OFFHH           PIC 9(2).
           05 WS-CD-OFFMN           PIC 9(2).

      *---------------------------------------------------------------*
      * Date éditée JJ/MM/AAAA                                         *
      *---------------------------------------------------------------*
       01  WS-DATE-EDIT.
           05 WS-DATE-JJ            PIC 99.
           05 WS-SLASH1             PIC X VALUE "/".
           05 WS-DATE-MM            PIC 99.
           05 WS-SLASH2             PIC X VALUE "/".
           05 WS-DATE-AAAA          PIC 9(4).

      *---------------------------------------------------------------*
      * Entête du fichier ETATANO                                      *
      *---------------------------------------------------------------*
       01  WS-ETATANO-HEADERS.
           05 WS-H-TITRE            PIC X(80)
              VALUE "                ETAT  DES  ANOMALIES".
           05 WS-H-SOUS-TITRE       PIC X(80)
              VALUE "                -------------------".
           05 WS-H-VIDE             PIC X(80)
              VALUE SPACES.
           05 WS-H-TIRETS           PIC X(80)
              VALUE ALL "-".

           05 WS-H-LIGNE-MAJ.
              10 WS-H-LBL-MAJ       PIC X(20)
                 VALUE "MISE A JOUR DU : ".
              10 WS-H-DATE          PIC X(10).

           05 WS-H-LIGNE-USER.
              10 WS-H-LBL-USER      PIC X(20)
                 VALUE "User      : API11".
              10 WS-H-USER          PIC X(8).

       77  WS-ETAT-LINE             PIC X(80) VALUE SPACES.

      *---------------------------------------------------------------*
      * Ligne d'anomalie                                               *
      *---------------------------------------------------------------*
       01  WS-ETAT-LIGNE.
           05 WS-ETAT-MAT           PIC X(06).
           05 FILLER                PIC X     VALUE SPACE.
           05 WS-ETAT-LIB1          PIC X(8)  VALUE "ERREUR :".
           05 FILLER                PIC X(2)  VALUE SPACES.
           05 WS-ETAT-MSG           PIC X(60).
           05 FILLER                PIC X(3)  VALUE SPACES.

       77  WS-STAT-LINE             PIC X(80) VALUE SPACES.

      *---------------------------------------------------------------*
      * Ligne de statistique                                           *
      *---------------------------------------------------------------*
       01  WS-STAT-LIGNE-OUT.
           05 FILLER                PIC X(02) VALUE "I ".
           05 WS-STAT-LIB           PIC X(60).
           05 FILLER                PIC X(03) VALUE " : ".
           05 WS-STAT-VAL           PIC Z(4)9.
           05 FILLER                PIC X(03) VALUE " I".

      *---------------------------------------------------------------*
      * Code d'anomalie courante                                       *
      *---------------------------------------------------------------*
       77  WS-CODE-ANO              PIC 99 VALUE 1.

      *---------------------------------------------------------------*
      * Nom du programme d'accès fichiers + buffer                     *
      *---------------------------------------------------------------*
       01  WS-ACC                   PIC X(8).
       01  WS-BUFFER                PIC X(80) VALUE SPACES.

      *---------------------------------------------------------------*
      * Zone de communication avec le sous-programme ACCFIC            *
      *---------------------------------------------------------------*
       01  Z-COM.
           05 Z-NOM-FICHIER         PIC X(8).
           05 Z-CODE-FONCTION       PIC 99.
           05 Z-CODE-RETOUR         PIC 99.
           05 Z-ENREGISTREMENT      PIC X(80).
           05 FILLER                PIC X(28).

       PROCEDURE DIVISION.

      *===============================================================*
      * PROGRAMME PRINCIPAL                                            *
      *===============================================================*
       MAIN.

           MOVE 00 TO Z-CODE-RETOUR

      *--- Ouverture des fichiers d'entrée / sortie ------------------*
           PERFORM OPEN-MVTSE
           PERFORM OPEN-ASSURE
           PERFORM OPEN-ETATANO
           PERFORM WRITE-ENTETE-ETATANO
           PERFORM OPEN-STAT

      *--- Lecture initiale du fichier des mouvements ----------------*
           PERFORM READ-MVTSE
           IF NOT FIN-MVTS
              MOVE WS-BUFFER TO W-FMVTSE
           END-IF

      *--- Boucle principale de traitement ---------------------------*
           PERFORM UNTIL FIN-MVTS
              ADD 1 TO WS-NB-MVT-LUS

              PERFORM TRAITER-MOUVEMENT

              PERFORM READ-MVTSE
              IF NOT FIN-MVTS
                 MOVE WS-BUFFER TO W-FMVTSE
              END-IF
           END-PERFORM

      *--- Ecriture des statistiques finales -------------------------*
           PERFORM WRITE-STAT-FINAL

      *--- Fermeture des fichiers ------------------------------------*
           PERFORM CLOSE-MVTSE
           PERFORM CLOSE-ASSURE
           PERFORM CLOSE-ETATANO
           PERFORM CLOSE-STAT

           STOP RUN.
           .

      *===============================================================*
      * Analyse du code mouvement                                      *
      *===============================================================*
       TRAITER-MOUVEMENT.

           EVALUATE F-CODE
               WHEN "C"
                   PERFORM TRAITER-CREATION

               WHEN "M"
                   PERFORM TRAITER-MODIF

               WHEN "S"
                   PERFORM TRAITER-SUPPR

               WHEN OTHER
                   ADD 1 TO WS-NB-ANOM
                   ADD 1 TO WS-NB-ANO-CODE
                   MOVE 01 TO WS-CODE-ANO
                   PERFORM WRITE-ANOMALIE
           END-EVALUATE
           .

      *===============================================================*
      * Ouverture fichier mouvements (ESDS)                           *
      *===============================================================*
       OPEN-MVTSE.
           MOVE 'ACCFIC' TO WS-ACC

           MOVE 'ESDS'   TO Z-NOM-FICHIER
           MOVE 01       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'OPEN ERREUR : ' Z-CODE-RETOUR
           END-IF
           .

      *===============================================================*
      * Ouverture fichier assurés (KSDS)                              *
      *===============================================================*
       OPEN-ASSURE.
           MOVE 'ACCFIC' TO WS-ACC

           MOVE 'KSDS'   TO Z-NOM-FICHIER
           MOVE 01       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'OPEN ERREUR : ' Z-CODE-RETOUR
           END-IF
           .

      *===============================================================*
      * Ouverture fichier anomalies                                   *
      *===============================================================*
       OPEN-ETATANO.
           MOVE 'ACCFIC'  TO WS-ACC

           MOVE 'ETATANO' TO Z-NOM-FICHIER
           MOVE 01        TO Z-CODE-FONCTION
           MOVE 00        TO Z-CODE-RETOUR
           MOVE SPACES    TO Z-ENREGISTREMENT

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'OPEN STAT ERREUR : ' Z-CODE-RETOUR
           END-IF
           .

      *===============================================================*
      * Ouverture fichier statistiques                                *
      *===============================================================*
       OPEN-STAT.
           MOVE 'ACCFIC' TO WS-ACC

           MOVE 'STAT'   TO Z-NOM-FICHIER
           MOVE 01       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'OPEN STAT ERREUR : ' Z-CODE-RETOUR
           END-IF
           .

      *===============================================================*
      * Lecture séquentielle du fichier mouvements                    *
      *===============================================================*
       READ-MVTSE.
           MOVE 'ACCFIC' TO WS-ACC

           MOVE 'ESDS'   TO Z-NOM-FICHIER
           MOVE 02       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR = 00
              MOVE Z-ENREGISTREMENT TO WS-BUFFER
              SET PAS-FIN-MVTS TO TRUE
           ELSE
              IF Z-CODE-RETOUR = 10
                  SET FIN-MVTS TO TRUE
              ELSE
                  DISPLAY 'READ MVT ERREUR : ' Z-CODE-RETOUR
              END-IF
           END-IF
           .

      *===============================================================*
      * Lecture d'un assuré par clé                                   *
      *===============================================================*
       READ-ASSURES.
           MOVE 'ACCFIC' TO WS-ACC

           MOVE 'KSDS'   TO Z-NOM-FICHIER
           MOVE 02       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT

           MOVE F-MAT TO Z-ENREGISTREMENT(1:6)

           CALL WS-ACC USING Z-COM

           EVALUATE Z-CODE-RETOUR
               WHEN 00
                   ADD 1 TO WS-NB-ASS-LUS
                   MOVE Z-ENREGISTREMENT TO WS-BUFFER

               WHEN 23
                   CONTINUE

               WHEN 99
                   ADD 1 TO WS-NB-ANOM
           END-EVALUATE
           .

      *===============================================================*
      * Fermeture des fichiers                                         *
      *===============================================================*
       CLOSE-MVTSE.
           MOVE 'ACCFIC' TO WS-ACC

           MOVE 'ESDS'   TO Z-NOM-FICHIER
           MOVE 03       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'CLOSE MVT ERREUR : ' Z-CODE-RETOUR
           END-IF
           .

       CLOSE-ASSURE.
           MOVE 'ACCFIC' TO WS-ACC

           MOVE 'KSDS'   TO Z-NOM-FICHIER
           MOVE 03       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'CLOSE ASSURE ERREUR : ' Z-CODE-RETOUR
           END-IF
           .

       CLOSE-ETATANO.
           MOVE 'ACCFIC'  TO WS-ACC

           MOVE 'ETATANO' TO Z-NOM-FICHIER
           MOVE 03        TO Z-CODE-FONCTION
           MOVE 00        TO Z-CODE-RETOUR
           MOVE SPACES    TO Z-ENREGISTREMENT

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'CLOSE ETATANO ERREUR : ' Z-CODE-RETOUR
           END-IF
           .

       CLOSE-STAT.
           MOVE 'ACCFIC' TO WS-ACC

           MOVE 'STAT'   TO Z-NOM-FICHIER
           MOVE 03       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'CLOSE STAT ERREUR : ' Z-CODE-RETOUR
           END-IF
           .

      *===============================================================*
      * Traitement d'un mouvement de création                         *
      *===============================================================*
       TRAITER-CREATION.

      *--- Vérifier si l'assuré existe déjà --------------------------*
           MOVE 'ACCFIC' TO WS-ACC
           MOVE 'KSDS'   TO Z-NOM-FICHIER
           MOVE 02       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT
           MOVE F-MAT    TO Z-ENREGISTREMENT(1:6)

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR = 00

      *--- Anomalie : création d'un assuré déjà existant ------------*
               ADD 1 TO WS-NB-ANOM
               ADD 1 TO WS-NB-ANO-CRE
               MOVE 02 TO WS-CODE-ANO
               PERFORM WRITE-ANOMALIE

           ELSE
               IF Z-CODE-RETOUR = 23

      *--- L'assuré n'existe pas : on le crée -----------------------*
                   MOVE W-FMVTSE TO W-ASSURE

                   MOVE 'ACCFIC' TO WS-ACC
                   MOVE 'KSDS'   TO Z-NOM-FICHIER
                   MOVE 04       TO Z-CODE-FONCTION
                   MOVE 00       TO Z-CODE-RETOUR
                   MOVE W-ASSURE TO Z-ENREGISTREMENT

                   CALL WS-ACC USING Z-COM

                   IF Z-CODE-RETOUR = 00
                       ADD 1 TO WS-NB-CRE
                   ELSE
                       ADD 1 TO WS-NB-ANOM
                       MOVE 02 TO WS-CODE-ANO
                       PERFORM WRITE-ANOMALIE
                   END-IF

               ELSE
                   ADD 1 TO WS-NB-ANOM
                   MOVE 02 TO WS-CODE-ANO
                   PERFORM WRITE-ANOMALIE
               END-IF
           END-IF
           .

      *===============================================================*
      * Traitement d'un mouvement de modification                     *
      *===============================================================*
       TRAITER-MODIF.

      *--- Vérifier que l'assuré existe ------------------------------*
           MOVE 'ACCFIC' TO WS-ACC
           MOVE 'KSDS'   TO Z-NOM-FICHIER
           MOVE 02       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT
           MOVE F-MAT    TO Z-ENREGISTREMENT(1:6)

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR = 23

      *--- Anomalie : tentative de MAJ d'un assuré inexistant -------*
               ADD 1 TO WS-NB-ANOM
               ADD 1 TO WS-NB-ANO-MAJ
               MOVE 03 TO WS-CODE-ANO
               PERFORM WRITE-ANOMALIE

           ELSE
               IF Z-CODE-RETOUR = 00

      *--- L'assuré existe : on le réécrit --------------------------*
                   MOVE W-FMVTSE TO W-ASSURE

                   MOVE 'ACCFIC' TO WS-ACC
                   MOVE 'KSDS'   TO Z-NOM-FICHIER
                   MOVE 05       TO Z-CODE-FONCTION
                   MOVE 00       TO Z-CODE-RETOUR
                   MOVE W-ASSURE TO Z-ENREGISTREMENT

                   CALL WS-ACC USING Z-COM

                   IF Z-CODE-RETOUR = 00
                       ADD 1 TO WS-NB-MOD
                   ELSE
                       ADD 1 TO WS-NB-ANOM
                   END-IF
               ELSE
                   ADD 1 TO WS-NB-ANOM
               END-IF
           END-IF
           .

      *===============================================================*
      * Traitement d'un mouvement de suppression                      *
      *===============================================================*
       TRAITER-SUPPR.

      *--- Vérifier que l'assuré existe ------------------------------*
           MOVE 'ACCFIC' TO WS-ACC
           MOVE 'KSDS'   TO Z-NOM-FICHIER
           MOVE 02       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR
           MOVE SPACES   TO Z-ENREGISTREMENT
           MOVE F-MAT    TO Z-ENREGISTREMENT(1:6)

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR = 23

      *--- Anomalie : suppression d'un assuré inexistant ------------*
               ADD 1 TO WS-NB-ANOM
               ADD 1 TO WS-NB-ANO-SUP
               MOVE 04 TO WS-CODE-ANO
               PERFORM WRITE-ANOMALIE

           ELSE
               IF Z-CODE-RETOUR = 00

      *--- L'assuré existe : suppression ----------------------------*
                   MOVE 'ACCFIC' TO WS-ACC
                   MOVE 'KSDS'   TO Z-NOM-FICHIER
                   MOVE 06       TO Z-CODE-FONCTION
                   MOVE 00       TO Z-CODE-RETOUR
                   MOVE F-MAT    TO Z-ENREGISTREMENT(1:6)

                   CALL WS-ACC USING Z-COM

                   IF Z-CODE-RETOUR = 00
                       ADD 1 TO WS-NB-SUP
                   ELSE
                       ADD 1 TO WS-NB-ANOM
                   END-IF
               ELSE
                   ADD 1 TO WS-NB-ANOM
               END-IF
           END-IF
           .

      *===============================================================*
      * Ecriture de l'entête du fichier anomalies                     *
      *===============================================================*
       WRITE-ENTETE-ETATANO.

           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DT
           MOVE WS-CD-DD   TO WS-DATE-JJ
           MOVE WS-CD-MM   TO WS-DATE-MM
           MOVE WS-CD-YYYY TO WS-DATE-AAAA
           MOVE WS-DATE-EDIT TO WS-H-DATE

           MOVE WS-H-TITRE      TO WS-ETAT-LINE
           PERFORM WRITE-LIGNE-ETATANO

           MOVE WS-H-SOUS-TITRE TO WS-ETAT-LINE
           PERFORM WRITE-LIGNE-ETATANO

           MOVE WS-H-VIDE       TO WS-ETAT-LINE
           PERFORM WRITE-LIGNE-ETATANO

           MOVE WS-H-LIGNE-MAJ  TO WS-ETAT-LINE
           PERFORM WRITE-LIGNE-ETATANO

           MOVE WS-H-LIGNE-USER TO WS-ETAT-LINE
           PERFORM WRITE-LIGNE-ETATANO

           MOVE WS-H-VIDE       TO WS-ETAT-LINE
           PERFORM WRITE-LIGNE-ETATANO

           MOVE WS-H-TIRETS     TO WS-ETAT-LINE
           PERFORM WRITE-LIGNE-ETATANO
           .

      *===============================================================*
      * Ecriture des statistiques finales                             *
      *===============================================================*
       WRITE-STAT-FINAL.

           MOVE WS-NB-ASS-LUS  TO ED-NB-ASS-LUS
           MOVE WS-NB-MVT-LUS  TO ED-NB-MVT-LUS
           MOVE WS-NB-CRE      TO ED-NB-CRE
           MOVE WS-NB-MOD      TO ED-NB-MOD
           MOVE WS-NB-SUP      TO ED-NB-SUP
           MOVE WS-NB-ANOM     TO ED-NB-ANOM
           MOVE WS-NB-ANO-CODE TO ED-NB-ANO-CODE
           MOVE WS-NB-ANO-CRE  TO ED-NB-ANO-CRE
           MOVE WS-NB-ANO-MAJ  TO ED-NB-ANO-MAJ
           MOVE WS-NB-ANO-SUP  TO ED-NB-ANO-SUP

           MOVE WS-MSG(05)    TO WS-STAT-LIB
           MOVE ED-NB-ASS-LUS TO WS-STAT-VAL
           PERFORM WRITE-1-STAT

           MOVE WS-MSG(06)    TO WS-STAT-LIB
           MOVE ED-NB-MVT-LUS TO WS-STAT-VAL
           PERFORM WRITE-1-STAT

           MOVE WS-MSG(08)    TO WS-STAT-LIB
           MOVE ED-NB-CRE     TO WS-STAT-VAL
           PERFORM WRITE-1-STAT

           MOVE WS-MSG(10)    TO WS-STAT-LIB
           MOVE ED-NB-MOD     TO WS-STAT-VAL
           PERFORM WRITE-1-STAT

           MOVE WS-MSG(11)    TO WS-STAT-LIB
           MOVE ED-NB-SUP     TO WS-STAT-VAL
           PERFORM WRITE-1-STAT

           MOVE WS-MSG(07)    TO WS-STAT-LIB
           MOVE ED-NB-ANOM    TO WS-STAT-VAL
           PERFORM WRITE-1-STAT

           MOVE WS-MSG(12)       TO WS-STAT-LIB
           MOVE ED-NB-ANO-CODE   TO WS-STAT-VAL
           PERFORM WRITE-1-STAT

           MOVE WS-MSG(13)       TO WS-STAT-LIB
           MOVE ED-NB-ANO-CRE    TO WS-STAT-VAL
           PERFORM WRITE-1-STAT

           MOVE WS-MSG(14)       TO WS-STAT-LIB
           MOVE ED-NB-ANO-MAJ    TO WS-STAT-VAL
           PERFORM WRITE-1-STAT

           MOVE WS-MSG(15)       TO WS-STAT-LIB
           MOVE ED-NB-ANO-SUP    TO WS-STAT-VAL
           PERFORM WRITE-1-STAT
           .

      *===============================================================*
      * Ecriture d'une anomalie dans ETATANO                          *
      *===============================================================*
       WRITE-ANOMALIE.

           MOVE SPACES TO WS-ETAT-LIGNE

           MOVE F-MAT               TO WS-ETAT-MAT
           MOVE WS-MSG(WS-CODE-ANO) TO WS-ETAT-MSG

           MOVE WS-ETAT-LIGNE TO Z-ENREGISTREMENT

           MOVE 'ACCFIC'  TO WS-ACC
           MOVE 'ETATANO' TO Z-NOM-FICHIER
           MOVE 04        TO Z-CODE-FONCTION
           MOVE 00        TO Z-CODE-RETOUR

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'WRITE ETATANO ERREUR : ' Z-CODE-RETOUR
           END-IF
           .

      *===============================================================*
      * Ecriture d'une ligne libre dans ETATANO                       *
      *===============================================================*
       WRITE-LIGNE-ETATANO.
           MOVE WS-ETAT-LINE TO Z-ENREGISTREMENT

           MOVE 'ACCFIC'  TO WS-ACC
           MOVE 'ETATANO' TO Z-NOM-FICHIER
           MOVE 04        TO Z-CODE-FONCTION
           MOVE 00        TO Z-CODE-RETOUR

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'WRITE ETATANO ERREUR : ' Z-CODE-RETOUR
           END-IF
           .

      *===============================================================*
      * Ecriture d'une statistique dans STAT                          *
      *===============================================================*
       WRITE-1-STAT.
           MOVE WS-STAT-LIGNE-OUT TO Z-ENREGISTREMENT

           MOVE 'ACCFIC' TO WS-ACC
           MOVE 'STAT'   TO Z-NOM-FICHIER
           MOVE 04       TO Z-CODE-FONCTION
           MOVE 00       TO Z-CODE-RETOUR

           CALL WS-ACC USING Z-COM

           IF Z-CODE-RETOUR NOT = 00
               DISPLAY 'WRITE STAT ERREUR : ' Z-CODE-RETOUR
           END-IF
           .