       IDENTIFICATION DIVISION.
       PROGRAM-ID. BankingApp.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AccountFile ASSIGN TO "accounts.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD AccountFile.
       01 AccountLine            PIC X(100).
 
       WORKING-STORAGE SECTION.
       01 WS-FirstName           PIC X(30).
       01 WS-LastName            PIC X(30).
       01 WS-BirthDate           PIC X(10).
       01 WS-Balance             PIC X(20).
       01 WS-UserInput           PIC X(60).
       01 WS-Input-FirstName     PIC X(30).
       01 WS-Input-LastName      PIC X(30).
       01 Line-End               PIC X VALUE LOW-VALUES.
       01 Name-Match             PIC X VALUE "N".
 
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "Bitte geben Sie Vor- und Nachnamen an: "
           ACCEPT WS-UserInput

           PERFORM PARSE-USER-INPUT

           OPEN INPUT AccountFile
           PERFORM UNTIL Line-End = HIGH-VALUES
              READ AccountFile
                  AT END
                      MOVE HIGH-VALUES TO Line-End
                  NOT AT END
                      PERFORM PARSE-ACCOUNT-LINE
                      IF WS-Input-FirstName = WS-FirstName
                         AND WS-Input-LastName = WS-LastName
                          DISPLAY "Der Kontostand von "
                              WS-FirstName SPACE WS-LastName
                              " lautet: " WS-Balance
                          MOVE "Y" TO Name-Match
                          MOVE HIGH-VALUES TO Line-End
                      END-IF
              END-READ
           END-PERFORM
           CLOSE AccountFile

           IF Name-Match = "N"
              DISPLAY "Kein Konto für " WS-UserInput " gefunden."
           END-IF

           STOP RUN.

       PARSE-USER-INPUT.
           UNSTRING WS-UserInput
              DELIMITED BY SPACE
              INTO WS-Input-FirstName
                   WS-Input-LastName
          .

       PARSE-ACCOUNT-LINE.
           UNSTRING AccountLine
              DELIMITED BY ","
              INTO WS-FirstName
                   WS-LastName
                   WS-BirthDate
                   WS-Balance
          .
