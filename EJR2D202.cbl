      ******************************************************************
      *                    IDENTIFICATION DIVISION                     *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJR2D202.
       AUTHOR. R2D2.
       DATE-WRITTEN. 27/08/2024.

      ******************************************************************
      *                      EVIRONMENT DIVISION                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
      *ARCHIVO DE LECTURA
       SELECT FILE-ENT1
           ASSIGN TO "/home/thisdarkcrow/cobol/ejemplo2/datosemp1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WFS-ENT1.

      *ARCHIVO DE IMPORTES MAYORES A X
       SELECT FILE-ENT2
           ASSIGN TO "/home/thisdarkcrow/cobol/ejemplo2/datosemp2.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WFS-ENT2.

      *ARCHIVO DE IMPORTES MENORES A X
       SELECT FILE-SAL1
           ASSIGN TO "/home/thisdarkcrow/cobol/ejemplo2/datossal1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WFS-SAL1.


      ******************************************************************
      *                         DATA DIVISION                          *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  FILE-ENT1
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REG-EMP1.
       01 REG-EMP1 PIC X(179).

       FD  FILE-ENT2
           RECORDING  MODE  IS  F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REG-EMP2.
       01 REG-EMP2 PIC X(101).

       FD  FILE-SAL1
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REG-SAL1.
       01 REG-SAL1 PIC X(123).
      
       WORKING-STORAGE SECTION.
       01 WSS-ENT1-EMP1.
           05 ENT1-NUM-EMPLEADO   PIC X(08).
           05 ENT1-NOMBRE         PIC X(20).
           05 ENT1-PATERNO        PIC X(20).
           05 ENT1-MATERNO        PIC X(20).
           05 ENT1-DIRECCION      PIC X(30).
           05 ENT1-CP             PIC 9(05).
           05 ENT1-TELEFONO       PIC 9(10).
           05 ENT1-PUESTO         PIC X(20).
           05 ENT1-DEPARTAMENTO   PIC X(15).
           05 ENT1-RFC            PIC X(13).
           05 ENT1-CURP           PIC X(18).
    
       01 WSS-ENT2-EMP2.
           05 ENT2-RFC            PIC X(13).
           05 ENT2-NOMBRE         PIC X(20).
           05 ENT2-CP             PIC 9(05).
           05 ENT2-DIRECCION      PIC X(30).
           05 ENT2-DEPARTAMENTO   PIC X(15).
           05 ENT2-TELEFONO       PIC 9(10).
           05 ENT2-NUM-EMPLEADO   PIC X(08).

       01 WSS-SAL1.
           05 SAL1-NUM-EMPLEADO   PIC X(08).
           05 SAL1-NOMBRE         PIC X(20).
           05 SAL1-PATERNO        PIC X(20).
           05 SAL1-MATERNO        PIC X(20).
           05 SAL1-TELEFONO       PIC 9(10).
           05 SAL1-DIRECCION      PIC X(30).
           05 SAL1-DEPARTAMENTO   PIC X(15).

       01 WSS-FILE-STATUS.
           05 WFS-ENT1            PIC X(2).
           05 WFS-ENT2            PIC X(2).
           05 WFS-SAL1            PIC X(2).
        
       01 WSS-ACUMULADORES.
           05 WAC-ENT1 PIC 9(6) VALUE ZEROES.
           05 WAC-ENT2 PIC 9(6) VALUE ZEROES.
           05 WAC-SAL1 PIC 9(6) VALUE ZEROES.
       
       01 WSS-SWITCHES.
           05 WSW-ENT1 PIC X(2) VALUE SPACE.
           05 WSW-ENT2 PIC X(2) VALUE SPACE.

       01 LLAVE-EMPLEADO1.
           05 LLAVE-ENT1-EMPLEADO PIC X(08).
           05 LLAVE-ENT1-RFC      PIC X(13).

       01 LLAVE-EMPLEADO2.
           05 LLAVE-ENT2-EMPLEADO PIC X(08).
           05 LLAVE-ENT2-RFC      PIC X(13).

           
      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-INCIO
           PERFORM 2000-PROCESO
      *         UNTIL LLAVE-EMPLEADO1 EQUAL LLAVE-EMPLEADO2
               UNTIL WSW-ENT1 EQUAL 's' OR
                     WSW-ENT2 EQUAL 's'
           PERFORM 3000-FIN
           .

       1000-INCIO.
           PERFORM 1100-ABRIR-ARCHIV.
           PERFORM 1200-LEER-ENT1.
           PERFORM 1300-LEER-ENT2.

       1100-ABRIR-ARCHIV.
           OPEN INPUT FILE-ENT1
                      FILE-ENT2
                OUTPUT FILE-SAL1

           IF WFS-ENT1 EQUAL '00' AND
              WFS-ENT2 EQUAL '00' AND
              WFS-SAL1 EQUAL '00'
              CONTINUE
           ELSE
               DISPLAY 'WFS-ENT1: 'WFS-ENT1
               DISPLAY 'WFS-ENT2: 'WFS-ENT2
               DISPLAY 'WFS-SAL1: 'WFS-SAL1
               PERFORM 1110-FIN-PROG
           END-IF
           .

       1110-FIN-PROG.
           STOP RUN.

       1200-LEER-ENT1.
           READ FILE-ENT1 INTO WSS-ENT1-EMP1
           IF WFS-ENT1 = '00'
               MOVE ENT1-NUM-EMPLEADO TO LLAVE-ENT1-EMPLEADO
               MOVE ENT1-RFC TO LLAVE-ENT1-RFC
               ADD 1 TO WAC-ENT1
           ELSE
               IF WFS-ENT1 = '10'
                   MOVE 's' TO WSW-ENT1
                   IF WFS-ENT2 = '00'
                       PERFORM 1300-LEER-ENT2
                       DISPLAY 'ARCHIVO ENTRADA 1: 'WFS-ENT1
                   END-IF
               END-IF
           END-IF
           .
        
       1300-LEER-ENT2.
           READ FILE-ENT2 INTO WSS-ENT2-EMP2
           IF WFS-ENT2 = '00'
               MOVE ENT2-NUM-EMPLEADO TO LLAVE-ENT2-EMPLEADO
               MOVE ENT2-RFC TO LLAVE-ENT2-RFC
               ADD 1 TO WAC-ENT2
           ELSE
               IF WFS-ENT2 = '10'
                   MOVE 's' TO WSW-ENT2
                   IF WFS-ENT1 = '00'
                       PERFORM 1200-LEER-ENT1
                       DISPLAY 'ARCHIVO ENTRADA 2: 'WFS-ENT2
                   END-IF
               END-IF
           END-IF
           .

       2000-PROCESO.
           PERFORM 2100-VALIDA-EMPLEADO.

       2100-VALIDA-EMPLEADO.
           DISPLAY 'FILE 1: 'ENT1-RFC' FILE 2: 'ENT2-RFC
           EVALUATE TRUE
               WHEN LLAVE-EMPLEADO1 EQUAL LLAVE-EMPLEADO2
                   PERFORM 2110-LLENA-WSS-SAL1
                   PERFORM 2120-GRABA-REG-SAL1
                   PERFORM 1200-LEER-ENT1
                   PERFORM 1300-LEER-ENT2
               WHEN  OTHER
                   PERFORM 1300-LEER-ENT2
           END-EVALUATE
           .
        
       2110-LLENA-WSS-SAL1.
           MOVE ENT1-NUM-EMPLEADO TO SAL1-NUM-EMPLEADO
           MOVE ENT1-NOMBRE TO SAL1-NOMBRE
           MOVE ENT1-PATERNO TO SAL1-PATERNO
           MOVE ENT1-MATERNO TO SAL1-MATERNO
           MOVE ENT1-TELEFONO TO SAL1-TELEFONO
           MOVE ENT1-DIRECCION TO SAL1-DIRECCION      
           MOVE ENT1-DEPARTAMENTO TO SAL1-DEPARTAMENTO 
           .

       2120-GRABA-REG-SAL1.
           WRITE REG-SAL1 FROM WSS-SAL1
           ADD 1 TO WAC-SAL1
           IF WFS-SAL1 EQUAL '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR DE GRABACIÓN SALIDA 1: 'WFS-SAL1
              PERFORM 1110-FIN-PROG
           END-IF
           .

       3000-FIN.
           PERFORM 3100-CERRAR-ARCHIV.
           PERFORM 3200-CIFRAS-CTRL.
           PERFORM 1110-FIN-PROG.

       3100-CERRAR-ARCHIV.
           CLOSE FILE-ENT1
                 FILE-ENT2
                 FILE-SAL1
           .
           
       3200-CIFRAS-CTRL.
           DISPLAY 'LEIDOS ENTRADA 1 : 'WAC-ENT1
           DISPLAY 'LEIDOS ENTRADA 2 : 'WAC-ENT2
           DISPLAY 'GRABADOS SALIDA 1: 'WAC-SAL1
           .
