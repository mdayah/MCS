DEFINT A-Z
DECLARE SUB Heading ()
DECLARE FUNCTION CodFind (Code$)
TYPE Inventory
    Sto AS STRING * 2
    Typ AS STRING * 1
    Cod AS STRING * 4
    Bra AS STRING * 3
    Des AS STRING * 24
    Sta AS STRING * 1
    Ctr AS STRING * 2
    Mdl AS STRING * 15
    Ser AS STRING * 15
    Pdt AS STRING * 10
    Prc AS STRING * 7
    Dlt AS STRING * 10
    Tim AS STRING * 2
    Tfr AS STRING * 2
    Cus AS STRING * 6
    Dis AS STRING * 10
    Del AS STRING * 1
    Tto AS STRING * 2
    Num AS INTEGER
    Fil AS STRING * 6
    Mdr AS LONG: Ydr AS LONG: Tdr AS LONG 'D-Rented
    Mds AS LONG: Yds AS LONG: Tds AS LONG 'D-Repairs
    Min AS LONG: Yin AS LONG: Tin AS LONG '$-Revenues
    Mde AS LONG: Yde AS LONG: Tde AS LONG '$-Depreciation
    Mre AS LONG: Yre AS LONG: Tre AS LONG '$-Repairs
    END TYPE

TYPE InvIndex
    Grp AS STRING * 2
    Cod AS STRING * 4
    Des AS STRING * 24
    Dis AS STRING * 10
    Mdl AS STRING * 10
    Prc AS STRING * 7
    Rat AS STRING * 3  'rate code
    Dtr AS STRING * 1  'drop terms
    Dfm AS STRING * 1  'drop FMV
    END TYPE

REM $INCLUDE: '..\Include\Codes.Rec'
REM $INCLUDE: '..\Include\Rates.Rec'

    DIM SHARED Inv AS Inventory
    DIM SHARED Inx AS InvIndex
    DIM SHARED Cod AS Codes
    DIM SHARED Rat AS Rates
    
    ON ERROR GOTO ErrTrap

    SHELL "Del Inventor.xxx"
    
    WIDTH 80, 50
    CLS
    PRINT "     This program will print different inventory costs accorindg to: "
    PRINT " code, distributor and model numbers.  You can send the output to the"
    PRINT " printer or the screen.  If you choose the screen for diplaying the  "
    PRINT " report, press any key to see one screenfull at a time.              "
    PRINT " At any time during processing or printing, use [Esc] to suspend.    "
    PRINT
    PRINT " Building inventory index file: ";
 
    OPEN "Inventor.Mas" FOR RANDOM AS #1 LEN = LEN(Inv)
    OPEN "Inventor.xxx" FOR ISAM InvIndex "Index" AS #2
    OPEN "\Rentals\Master\Codes.Mas" FOR RANDOM AS #3 LEN = LEN(Cod)
    OPEN "\Rentals\Master\Rates.Mas" FOR RANDOM AS #4 LEN = LEN(Rat)

    CREATEINDEX 2, "Def", 1, "Grp", "Cod", "Dis", "Mdl", "Prc"
    SETINDEX 2, "Def"

    FOR x = 1 TO LOF(1) \ LEN(Inv)
    IF INKEY$ = CHR$(27) THEN CLOSE : END
    GET #1, x, Inv
    IF CodFind(Inv.Cod) = 0 THEN BEEP
    Inx.Grp = Cod.Grp
    Inx.Cod = Inv.Cod
    Inx.Des = Cod.Des
    Inx.Dis = Inv.Dis
    Inx.Mdl = Inv.Mdl
    Inx.Prc = Inv.Prc
    Inx.Rat = Cod.Rat
    Inx.Dtr = Cod.Dtr
    Inx.Dfm = Cod.Dfm
    LOCATE 7, 43: PRINT x;
    INSERT #2, Inx
    i = i + 1: LOCATE 7, 33: PRINT i;
10  NEXT
    
    CLS
    MOVEFIRST #2
    VIEW PRINT
    PRINT "Press (S) for screen or (P) for printer"
    DO: k$ = INKEY$: LOOP WHILE k$ = ""
    k$ = UCASE$(k$)
    IF k$ <> "S" AND k$ <> "P" THEN 10

    IF k$ = "S" THEN
       LOCATE 1, 1: PRINT "                                                 Model            Rate Drop Drop"
       LOCATE 2, 1: PRINT "Group Code       Description        Distribut    Number     Cost  Code Term FMV "
       LOCATE 3, 1: PRINT "===== ==== ======================== ========== ========== ======= ==== ==== ===="
       VIEW PRINT 4 TO 50
    ELSE
       Heading
    END IF
    
20  RETRIEVE #2, Inx
    IF INKEY$ = CHR$(27) THEN CLOSE : END
    IF k$ = "S" THEN
        l = l + 1: IF l > 46 THEN l = 1: DO: LOOP WHILE INKEY$ = ""
        PRINT USING "  &  & & & & &  &   &   &"; Inx.Grp; Inx.Cod; Inx.Des; Inx.Dis; Inx.Mdl; Inx.Prc; Inx.Rat; Inx.Dtr; Inx.Dfm
    ELSE
       l = l + 1: IF l > 60 THEN l = 1: Heading
       LPRINT USING "  &  & & & & &  &   &   &  "; Inx.Grp; Inx.Cod; Inx.Des; Inx.Dis; Inx.Mdl; Inx.Prc; Inx.Rat; Inx.Dtr; Inx.Dfm;
       IF Inx.Rat > "   " THEN
          GET #4, VAL(Inx.Rat), Rat
          FOR z = 1 TO 3
              LPRINT USING " & &"; Rat.Mtr(z); Rat.Mrt(z); Rat.Wtr(z); Rat.Wrt(z);
          NEXT
       END IF
       LPRINT
    END IF
    MOVENEXT #2
    IF NOT EOF(2) THEN GOTO 20
    CLOSE
    END

ErrTrap:
    IF ERR = 86 THEN RESUME 10
    BEEP: PRINT ERR
    CLOSE
    END

FUNCTION CodFind (Code$)
  b = 1: t = LOF(3) \ LEN(Cod)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #3, m, Cod
      IF Code$ = Cod.Cod THEN CodFind = m: EXIT DO
      IF Code$ > Cod.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

SUB Heading ()
    LPRINT CHR$(12);
    LPRINT CHR$(27); "@";  'Reset Printer
    LPRINT CHR$(27); "M";  'Elite Pitch
    WIDTH LPRINT 255       'Supres Line Fed
    LPRINT "                                                 Model            Rate Drop Drop"
    LPRINT "Group Code       Description        Distribut    Number     Cost  Code Term FMV  Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate"
    LPRINT "===== ==== ======================== ========== ========== ======= ==== ==== ==== ========== ========== ========== ========== ========== =========="
END SUB

