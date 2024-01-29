       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-CONVERT.
       AUTHOR. DAVID LONG BIN.
       DATE-WRITTEN. 18/01/24.
       DATE-COMPILED.
       SECURITY. Standard.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FICHE-EMP ASSIGN TO "FICHEMP.DAT"
           ORGANISATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FICHE-EMP-STATUS.

           SELECT FICHE-COMPT ASSIGN TO "FICHE-COMPT.DAT"
           ORGANISATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FICHE-COMPT-STATUS.

   
       DATA DIVISION.
       FILE SECTION.
       FD  FICHE-EMP.
       01  EMPDETAILS.
           
           05 EMP-INFOS.
               10 EMPLOYEID PIC 9(7).
               10 EMPLOYENAME.
                   15 NOM PIC X(10).
                   15 PRENOM PIC X(10).
               10 DATEDENAISSANCE.
                   15 ANNEE PIC 9(4).
                   15 MOIS PIC 9(2).
                   15 JOUR PIC 9(2).
               10 HEURESTRAVAIL PIC 9(3).
               10 SALAIRE PIC 9(4)V99.
           05 DEPARTEMENT PIC X(30).
           05 GENRE PIC X.


       FD  FICHE-COMPT.
       01  FS-BUF-COMPT PIC X(72).                

       WORKING-STORAGE SECTION.
       01         PIC X VALUE X"00".
           88 FIN-FICHIER-EMP VALUE X"01" THRU X"FF".
       01  WS-FICHE-EMP-STATUS PIC X(2).
       01  WS-FICHE-COMPT-STATUS PIC X(2).

       01  WS-EMP-INFOS.     
           05 WS-ID-EMP PIC X(7).
           05 FILLER PIC XXX VALUE SPACE.
           05 WS-PRENOM-EMP PIC X(10).
           05 WS-NOM-EMP PIC X(10).
           05 WS-SALAIRE-EMP PIC 9(4)V99.
           05 FILLER PIC XXX VALUE SPACE.
           05 WS-DEPARTEMENT-EMP PIC X(30).  

       01  WS-STARS-FILLER PIC X(70) VALUE ALL "*".
       01  WS-TIRET-FILLER PIC X(70) VALUE ALL "-".

       01  WS-HEADER.
           05 FILLER PIC X(7) VALUE "   ID  ".    
           05 FILLER PIC X(3) VALUE SPACE.
           05 FILLER PIC X(10) VALUE "PRENOM".
           05 FILLER PIC X(10) VALUE "NOM".
           05 FILLER PIC X(7) VALUE "SALARY".
           05 FILLER PIC X(3) VALUE SPACES.
           05 FILLER PIC X(30) VALUE "DEPARTEMENT".
    




       PROCEDURE DIVISION.

       0000-MAIN-MODULE SECTION.

           PERFORM 1000-OPEN-FILES.
           PERFORM 1100-WRITE-HEADER.
           PERFORM 2010-READ-AND-WRITE-LINES.
           PERFORM 3000-CLOSE-FILE.
           
         



       1000-OPEN-FILES SECTION.
           OPEN-FILES-START.

           OPEN INPUT FICHE-EMP.
           IF WS-FICHE-EMP-STATUS NOT = "00"
               DISPLAY "Erreur lors de l'ouverture du fichier."
               DISPLAY "Le code erreur est : " WS-FICHE-EMP-STATUS
           END-IF.

           OPEN OUTPUT FICHE-COMPT.
           IF WS-FICHE-COMPT-STATUS NOT = "00"
               DISPLAY "Erreur lors de l'ecriture du fichier de sortie."
               DISPLAY "Le code erreur est : " WS-FICHE-COMPT-STATUS
           END-IF.


           OPEN-FILES-END.
               EXIT.

       1100-WRITE-HEADER SECTION.

           WRITE-HEADER-START.

           MOVE WS-STARS-FILLER TO FS-BUF-COMPT.
           WRITE FS-BUF-COMPT.
           MOVE WS-HEADER TO FS-BUF-COMPT.
           WRITE FS-BUF-COMPT.
 
           WRITE-HEADER-END.
               EXIT.         

       2010-READ-AND-WRITE-LINES SECTION.
           READ-AND-WRITE-START.

           *> première lecture du fichier 
           PERFORM 2020-READ-FILE
               IF FIN-FICHIER-EMP
                   DISPLAY "Le fichier est vide"
               END-IF.

           *>LECTURE EN BOUCLE 

           PERFORM UNTIL FIN-FICHIER-EMP
                  MOVE EMPLOYEID TO WS-ID-EMP
                  MOVE PRENOM TO WS-PRENOM-EMP
                  MOVE NOM TO WS-NOM-EMP
                  MOVE SALAIRE TO WS-SALAIRE-EMP
                  MOVE DEPARTEMENT TO WS-DEPARTEMENT-EMP

                    MOVE WS-EMP-INFOS TO FS-BUF-COMPT
                    WRITE FS-BUF-COMPT
                    MOVE WS-TIRET-FILLER TO FS-BUF-COMPT
                    WRITE FS-BUF-COMPT

                 PERFORM 2020-READ-FILE   

           END-PERFORM.      

           READ-AND-WRITE-END.
               EXIT.           
          

       2020-READ-FILE SECTION. 

           READ-FILE-START.
           
               READ FICHE-EMP
                   AT END 
                       SET FIN-FICHIER-EMP TO TRUE 
                   NOT AT END
                       IF WS-FICHE-EMP-STATUS NOT = ZERO
                           DISPLAY "Probleme lecture fichier"
                           DISPLAY "code retour" WS-FICHE-EMP-STATUS
                       END-IF
               END-READ.               
           
           READ-FILE-END.
               EXIT.    


       3000-CLOSE-FILE SECTION.
           CLOSE-FILE-START.
           CLOSE FICHE-EMP.
           CLOSE FICHE-COMPT.

           CLOSE-FILE-END.
               EXIT.

       9999-FIN-PROGRAMME.
       *>---------------------------------------------------------------
       STOP RUN.
       END PROGRAM FILE-CONVERT. 



           
