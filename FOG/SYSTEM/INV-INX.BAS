'temporary program to rebuid inventory index file. Add purchase date
'to index file for the purpose of printing end of year inventory cost
'report for the accountant.


DEFINT A-Z
DECLARE FUNCTION FindCod (f$)

REM $INCLUDE: '..\Include\Codes.REC'
REM $INCLUDE: '..\Include\Inventor.Rec'

DIM SHARED Cod AS Codes
DIM SHARED Inv AS Inventory
DIM SHARED Inx AS InvIndex

    KILL "..\DataFile\Inventor.Inx"
    KILL "..\DataFile\Inventor.Ser"

    OPEN "..\Master\Codes.Mas" FOR RANDOM AS #1 LEN = LEN(Cod)
    OPEN "..\DataFile\Inventor.Mas" FOR RANDOM AS #10 LEN = LEN(Inv)
    OPEN "..\DataFile\Inventor.New" FOR RANDOM AS #11 LEN = LEN(Inx)

'Test on-rent inventory for agreement numbers
    WIDTH 80, 50
    CLS
    FOR x = 1 TO LOF(10) \ LEN(Inv)
        GET #10, x, Inv
        IF Inv.Sta = 2 AND Inv.Agr < "100000" THEN PRINT Inv.Ser, Inv.Agr
    NEXT

FOR x = 1 TO LOF(10) \ LEN(Inv)
    GET #10, x, Inv
    IF FindCod(Inv.Cod) = 0 THEN
        PRINT "ERROR-Code not found"
        PRINT "Type:            "; Inv.Typ
        PRINT "Group:           "; Inv.Grp
        PRINT "Code:            "; Inv.Cod
        PRINT "Brand:           "; Inv.Bra
        PRINT "Status:          "; Inv.Sta
        PRINT "X rent:          "; Inv.Tim
        PRINT "Service center:  "; Inv.Ctr
        PRINT "Model number:    "; Inv.Mdl
        PRINT "Serial number:   "; Inv.Ser
        PRINT "Distributor:     "; Inv.Dis
        PRINT "Date purchased:  "; Inv.Dtp
        PRINT "Date received:   "; Inv.Dtr
        PRINT "Last activity;   "; Inv.Dta
        PRINT "Purchase price:  "; Inv.Prc
        PRINT "Agreement number:"; Inv.Agr
        PRINT "Trasferred (T):  "; Inv.Del
        PRINT "Trasfered form:  "; Inv.Tfr
        PRINT "Transfered to:   "; Inv.Tto
        PRINT "Purchase order:  "; Inv.Pon
        PRINT "Activity Code:   "; Inv.Act
        PRINT "Warranty parts:  "; Inv.Par
        PRINT "Warranty labor:  "; Inv.Lab
        PRINT "Rate code:       "; Inv.Rat
        PRINT "Override rate:   "; Inv.Ovr
        WHILE INKEY$ = "": WEND
    ELSE
        Inx.Typ = Cod.Typ
        Inx.Grp = Cod.Grp
        Inx.Cod = Inv.Cod
        Inx.Mdl = Inv.Mdl
        Inx.Ser = Inv.Ser
        Inx.Dtp = VAL(RIGHT$(Inv.Dtp, 4) + LEFT$(Inv.Dtp, 2) + MID$(Inv.Dtp, 4, 2))
        Inx.Num = x
        PUT #11, x, Inx
        LOCATE 1, 1: PRINT USING "Re-Indexing Invetory number: ##### "; x;
    END IF
NEXT

    CLOSE
    SHELL "Sortf ..\DataFile\Inventor.New ..\DataFile\Inventor.Inx /L33 /+1,27 /Q"
    SHELL "Sortf ..\DataFile\Inventor.Inx ..\DataFile\Inventor.Ser /L33 /+18,10 /Q"
    KILL "..\DataFile\Inventor.New"

FUNCTION FindCod (f$)
   b = 1: t = LOF(1) \ LEN(Cod)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #1, m, Cod
      IF f$ = Cod.Cod THEN FindCod = 1: EXIT DO
      IF f$ > Cod.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

