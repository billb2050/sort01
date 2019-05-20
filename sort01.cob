       IDENTIFICATION DIVISION.
       PROGRAM-ID. sort01.
      * A GnuCOBOL program
      * On: 03/11/2019
      * By: Bill Blasingim      
      *
      * This program uses the COBOL SORT.
      * Sorts name in last, first order.
      *
       Environment Division.      
       Input-Output Section.
       File-Control.
       Select InFile Assign to
         "/home/bill/Mystuff/COBOL/data/customer-fixed2.txt"
         Line Sequential.
       Select OutFile Assign to
         "./customer.srt".
      *   Line Sequential.
       SELECT SORT-FILE  ASSIGN TO "./srtwork.fil".
       Data Division.
       File Section.
       FD InFile.
         01 InRec.
           05 Account		pic x(18).
           05 Filler		pic x(2).
           05 Gender		pic x.
           05 I-Name.
             10 I-First		pic x(15).
             10 I-Middle	pic x(15).
             10 I-Last		pic x(20).           
           05 I-Birthday.
              10 yyyy		pic x(4).
              10 Filler		pic x.
              10 mm			pic x(2).
              10 Filler		pic x.              
              10 dd			pic x(2).              
            05 I-Address    pic x(25).
            05 City		    pic x(20).
            05 State	    pic x(2).
            05 Zip		    pic x(5). 
      *      05 Filler		pic x.                                              

       FD OutFile.
         01 OutRec.
           05 O-Name.   
             10 O-Last			pic x(20).                  
             10 O-First			pic x(15).
             10 O-Middle		pic x(15).
           05 O-Birthday.
              10 o-yyyy			pic x(4).
              10 Filler         pic x.
              10 o-mm			pic x(2).
              10 Filler         pic x.
              10 o-dd          pic x(2). 
            05 o-eol           BINARY-CHAR.             

       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SRT-NAME.
             10 s-First     	pic x(15).
             10 s-Middle		pic x(15).
             10 s-Last			pic x(20).           
           05  SRT-Birthday     PIC X(10).

       Working-Storage Section.
         01 Misc.
           05        Pic X
             Value "N".
           88 EOF     Value "Y".    
      *    Linux end of line [line feed]
           05 eol    BINARY-CHAR UNSIGNED value 10.  
           05  EOF-FLAG                     PIC X(01) VALUE 'N'.
               88  EOF2                                VALUE 'Y'.           

       PROCEDURE DIVISION.

           SORT SORT-FILE
                ASCENDING KEY  s-Last, s-first
                INPUT PROCEDURE SRT-INPUT-PROCEDURE
                OUTPUT PROCEDURE SRT-OUTPUT-PROCEDURE.


         Close InFile, OutFile.
         STOP RUN.                

       SRT-INPUT-PROCEDURE SECTION.
           OPEN INPUT InFile.

           PERFORM READ-RTN THRU READ-EXIT.
           PERFORM PROCESS-RTN THRU PROCESS-EXIT
               UNTIL EOF.

       END-INPUT SECTION.

       READ-RTN.
           Read InFile
             At End
               Set EOF to True
           End-Read.            

      *    EXHIBIT NAMED IN-NAME.
       READ-EXIT.
           EXIT.       

       PROCESS-RTN.
           MOVE I-NAME TO SRT-NAME.
           MOVE I-Birthday to SRT-Birthday.

           RELEASE SORT-RECORD.

           PERFORM READ-RTN THRU READ-EXIT.

       PROCESS-EXIT.
           EXIT.           

       SRT-OUTPUT-PROCEDURE SECTION.

           MOVE 'N' TO EOF-FLAG.
           RETURN SORT-FILE RECORD AT END
             MOVE 'Y' TO EOF-FLAG.

           Open Output OutFile.

           PERFORM WRITE-RTN THRU WRITE-RTN-EXIT
               UNTIL EOF2.

       END-OUTPUT SECTION.

       WRITE-RTN.
           MOVE S-last TO o-last.
           MOVE S-first TO o-first.
           Move spaces to O-Middle.
           MOVE SRT-Birthday to o-Birthday.
           MOVE eol to o-eol.

           WRITE OutRec.

           RETURN SORT-FILE RECORD AT END
             MOVE 'Y' TO EOF-FLAG.
       WRITE-RTN-EXIT.
           EXIT.
