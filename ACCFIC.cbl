       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCFIC.

      *===============================================================*
      *  PROGRAMME : ACCFIC                                           *
      *  ROLE      : Sous-programme générique de gestion de fichiers  *
      *                                                               *
      *  Fonctions gérées :                                           *
      *    01 = OPEN                                                  *
      *    02 = READ                                                  *
      *    03 = CLOSE                                                 *
      *    04 = WRITE                                                 *
      *    05 = REWRITE                                               *
      *    06 = DELETE                                                *
      *===============================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *---------------------------------------------------------------*
      * Fichier des mouvements : séquentiel                           *
      *---------------------------------------------------------------*
           SELECT F-MVTS ASSIGN TO AS-MVTS
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS FS-MV.

      *---------------------------------------------------------------*
      * Fichier des assurés : indexé                                  *
      *---------------------------------------------------------------*
           SELECT F-ASSURES ASSIGN TO ASSURES
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS AS-KEY
               FILE STATUS  IS FS-AS.

      *---------------------------------------------------------------*
      * Fichier état anomalies : séquentiel                           *
      *---------------------------------------------------------------*
           SELECT F-ETAT-ANO ASSIGN TO ETATANO
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS FS-ANO.

      *---------------------------------------------------------------*
      * Fichier statistiques : séquentiel                             *
      *---------------------------------------------------------------*
           SELECT F-STAT ASSIGN TO STAT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS FS-STAT.

       DATA DIVISION.
       FILE SECTION.

      *---------------------------------------------------------------*
      * Description du fichier mouvements                             *
      *---------------------------------------------------------------*
       FD  F-MVTS
           RECORD CONTAINS 80 CHARACTERS.
       01  MV-REC.
           05 MV-RAW               PIC X(80).

      *---------------------------------------------------------------*
      * Description du fichier assurés                                *
      *---------------------------------------------------------------*
       FD  F-ASSURES
           RECORD CONTAINS 80 CHARACTERS.
       01  AS-REC.
           05 AS-KEY               PIC X(06).
           05 AS-DATA              PIC X(74).

      *---------------------------------------------------------------*
      * Description du fichier anomalies                              *
      *---------------------------------------------------------------*
       FD  F-ETAT-ANO
           RECORD CONTAINS 80 CHARACTERS.
       01  ANO-REC.
           05 ANO-RAW              PIC X(80).

      *---------------------------------------------------------------*
      * Description du fichier statistiques                           *
      *---------------------------------------------------------------*
       FD  F-STAT
           RECORD CONTAINS 80 CHARACTERS.
       01  STAT-REC.
           05 STAT-RAW             PIC X(80).

       WORKING-STORAGE SECTION.

      *---------------------------------------------------------------*
      * File status des fichiers                                      *
      *---------------------------------------------------------------*
       77  FS-MV                   PIC XX VALUE SPACES.
       77  FS-AS                   PIC XX VALUE SPACES.
       77  FS-ANO                  PIC XX VALUE SPACES.
       77  FS-STAT                 PIC XX VALUE SPACES.

       LINKAGE SECTION.

      *---------------------------------------------------------------*
      * Zone de communication avec le programme appelant              *
      *---------------------------------------------------------------*
       01  Z-COM.
           05 Z-NOM-FICHIER        PIC X(8).
           05 Z-CODE-FONCTION      PIC 99.
           05 Z-CODE-RETOUR        PIC 99.
           05 Z-ENREGISTREMENT     PIC X(80).
           05 FILLER               PIC X(28).

       PROCEDURE DIVISION USING Z-COM.

      *===============================================================*
      * Point d'entrée principal                                       *
      *===============================================================*
       MAIN.
           MOVE 00 TO Z-CODE-RETOUR

           EVALUATE Z-CODE-FONCTION
               WHEN 01
                   PERFORM OPEN-FILES

               WHEN 02
                   PERFORM READ-FILES

               WHEN 03
                   PERFORM CLOSE-FILES

               WHEN 04
                   PERFORM WRITE-FILES

               WHEN 05
                   PERFORM REWRITE-FILES

               WHEN 06
                   PERFORM DELETE-FILES

               WHEN OTHER
                   MOVE 99 TO Z-CODE-RETOUR
           END-EVALUATE

           GOBACK
           .

      *===============================================================*
      * Ouverture d'un fichier selon son nom logique                  *
      *===============================================================*
       OPEN-FILES.
           EVALUATE Z-NOM-FICHIER
               WHEN "ESDS"
                   OPEN INPUT F-MVTS
                   IF FS-MV NOT = "00"
                       MOVE 99 TO Z-CODE-RETOUR
                   END-IF

               WHEN "KSDS"
                   OPEN I-O F-ASSURES
                   IF FS-AS NOT = "00"
                       MOVE 99 TO Z-CODE-RETOUR
                   END-IF

               WHEN "ETATANO"
                   OPEN OUTPUT F-ETAT-ANO
                   IF FS-ANO NOT = "00"
                       MOVE 99 TO Z-CODE-RETOUR
                   END-IF

               WHEN "STAT"
                   OPEN OUTPUT F-STAT
                   IF FS-STAT NOT = "00"
                       MOVE 99 TO Z-CODE-RETOUR
                   END-IF

               WHEN OTHER
                   MOVE 99 TO Z-CODE-RETOUR
           END-EVALUATE
           .

      *===============================================================*
      * Lecture d'un fichier                                          *
      *===============================================================*
       READ-FILES.
           EVALUATE Z-NOM-FICHIER

      *--- Lecture séquentielle du fichier mouvements ----------------*
               WHEN "ESDS"
                   READ F-MVTS
                       AT END
                           MOVE 10 TO Z-CODE-RETOUR
                       NOT AT END
                           MOVE MV-RAW TO Z-ENREGISTREMENT
                           MOVE 00 TO Z-CODE-RETOUR
                   END-READ

      *--- Lecture d'un assuré par clé -------------------------------*
               WHEN "KSDS"
                   IF Z-ENREGISTREMENT(1:6) = SPACES
                      OR Z-ENREGISTREMENT(1:6) = LOW-VALUES
                       MOVE 99 TO Z-CODE-RETOUR
                   ELSE
                       MOVE Z-ENREGISTREMENT(1:6) TO AS-KEY

                       READ F-ASSURES KEY IS AS-KEY
                           INVALID KEY
                               MOVE 23 TO Z-CODE-RETOUR
                           NOT INVALID KEY
                               MOVE AS-REC TO Z-ENREGISTREMENT
                               MOVE 00 TO Z-CODE-RETOUR
                       END-READ
                   END-IF

               WHEN OTHER
                   MOVE 99 TO Z-CODE-RETOUR
           END-EVALUATE
           .

      *===============================================================*
      * Fermeture des fichiers                                        *
      *===============================================================*
       CLOSE-FILES.
           EVALUATE Z-NOM-FICHIER
               WHEN "ESDS"
                   CLOSE F-MVTS
                   MOVE 00 TO Z-CODE-RETOUR

               WHEN "KSDS"
                   CLOSE F-ASSURES
                   MOVE 00 TO Z-CODE-RETOUR

               WHEN "ETATANO"
                   CLOSE F-ETAT-ANO
                   MOVE 00 TO Z-CODE-RETOUR

               WHEN "STAT"
                   CLOSE F-STAT
                   MOVE 00 TO Z-CODE-RETOUR

               WHEN OTHER
                   MOVE 99 TO Z-CODE-RETOUR
           END-EVALUATE
           .

      *===============================================================*
      * Ecriture dans un fichier                                      *
      *===============================================================*
       WRITE-FILES.
           EVALUATE Z-NOM-FICHIER

      *--- Ecriture dans le fichier des anomalies --------------------*
               WHEN "ETATANO"
                   MOVE Z-ENREGISTREMENT TO ANO-RAW
                   WRITE ANO-REC
                   IF FS-ANO = "00"
                       MOVE 00 TO Z-CODE-RETOUR
                   ELSE
                       MOVE 99 TO Z-CODE-RETOUR
                   END-IF

      *--- Ecriture dans le fichier des statistiques -----------------*
               WHEN "STAT"
                   MOVE Z-ENREGISTREMENT TO STAT-RAW
                   WRITE STAT-REC
                   IF FS-STAT = "00"
                       MOVE 00 TO Z-CODE-RETOUR
                   ELSE
                       MOVE 99 TO Z-CODE-RETOUR
                   END-IF

      *--- Ecriture dans le fichier indexé des assurés --------------*
               WHEN "KSDS"
                   MOVE Z-ENREGISTREMENT TO AS-REC
                   WRITE AS-REC
                       INVALID KEY
                           MOVE 99 TO Z-CODE-RETOUR
                       NOT INVALID KEY
                           MOVE 00 TO Z-CODE-RETOUR
                   END-WRITE

               WHEN OTHER
                   MOVE 99 TO Z-CODE-RETOUR
           END-EVALUATE
           .

      *===============================================================*
      * Réécriture d'un enregistrement assuré                         *
      *===============================================================*
       REWRITE-FILES.
           EVALUATE Z-NOM-FICHIER
               WHEN "KSDS"
                   MOVE Z-ENREGISTREMENT(1:6) TO AS-KEY

                   READ F-ASSURES KEY IS AS-KEY
                       INVALID KEY
                           MOVE 23 TO Z-CODE-RETOUR
                       NOT INVALID KEY
                           MOVE Z-ENREGISTREMENT TO AS-REC
                           REWRITE AS-REC
                           IF FS-AS = "00"
                               MOVE 00 TO Z-CODE-RETOUR
                           ELSE
                               MOVE 99 TO Z-CODE-RETOUR
                           END-IF
                   END-READ

               WHEN OTHER
                   MOVE 99 TO Z-CODE-RETOUR
           END-EVALUATE
           .

      *===============================================================*
      * Suppression d'un enregistrement assuré                        *
      *===============================================================*
       DELETE-FILES.
           EVALUATE Z-NOM-FICHIER
               WHEN "KSDS"
                   MOVE Z-ENREGISTREMENT(1:6) TO AS-KEY

                   READ F-ASSURES KEY IS AS-KEY
                       INVALID KEY
                           MOVE 23 TO Z-CODE-RETOUR
                       NOT INVALID KEY
                           DELETE F-ASSURES
                           IF FS-AS = "00"
                               MOVE 00 TO Z-CODE-RETOUR
                           ELSE
                               MOVE 99 TO Z-CODE-RETOUR
                           END-IF
                   END-READ

               WHEN OTHER
                   MOVE 99 TO Z-CODE-RETOUR
           END-EVALUATE
           .