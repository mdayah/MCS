DEFINT A-Z

DECLARE FUNCTION FindCod (f$)

REM $INCLUDE: '..\Include\Codes.REC'
REM $INCLUDE: '..\Include\Inventor.OFF'

COMMON SHARED Cod      AS Codes      '01
COMMON SHARED Inv      AS Inventory  '10
    
    CLS
    OPEN "..\Master\Codes   .Mas" FOR RANDOM AS #1 LEN = LEN(Cod)
    OPEN "..\OfData\Inventor.MAS" FOR ISAM Inventory "Inventory" AS #10

'   rebuild data base, create new file database file
'   recode and create new indexes
'
    ON ERROR GOTO Errors

    OPEN "..\OfData\Inventor.NEW" FOR ISAM Inventory "Inventory" AS #11
 
    SETINDEX 10, "Sto"
    MOVEFIRST #10
    DO: IF INKEY$ = CHR$(27) THEN EXIT DO
        RETRIEVE #10, Inv
        IF FindCod(Inv.Cod) = 0 THEN ERROR 200
        Inv.Typ = Cod.Typ
        Inv.Grp = Cod.Grp
        Inv.Cod = Cod.Cod
        Inv.Rat = Cod.Rat
        INSERT #11, Inv
        UPDATE #10, Inv
        MOVENEXT #10
        i& = i& + 1: PRINT i&
    LOOP UNTIL EOF(10)
    PRINT "Creating Indexes"
    CREATEINDEX #11, "Sto", 1, "Sto", "Typ", "Grp", "Cod", "Mdl", "Ser"
    CREATEINDEX #11, "Ser", 1, "Ser"
    CREATEINDEX #11, "Agr", 1, "Sto", "Agr", "Typ", "Grp", "Cod", "Ser"
    CREATEINDEX #11, "Cod", 0, "Cod"
    CREATEINDEX #11, "Cus", 0, "Sto", "Agr"
    CREATEINDEX #11, "Cst", 0, "Grp", "Cod", "Dis", "Mdl", "Prc"

    CLOSE

    KILL "..\OfData\Inventor.MAS"
    NAME "..\OfData\Inventor.NEW" AS "..\OfData\Inventor.MAS"
    END

Errors:
    CLOSE
    PRINT "Error: "; ERR
    PRINT "Serial Number: "; Inv.Ser
    KILL "..\OfData\Inventor.NEW"
    END

FUNCTION FindCod (f$)
   b = 1: t = LOF(1) \ LEN(Cod)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #1, m, Cod
      IF f$ = Cod.Cod THEN FindCod = m: EXIT DO
      IF f$ > Cod.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

