DEFINT A-Z
DECLARE FUNCTION FindCod (f$)
DECLARE SUB EntryU (r, c, s, e, b, f$)


'$INCLUDE: '..\Include\Codes.rec'
'$INCLUDE: '..\Include\Inventor.rec'

    DIM SHARED Inv AS Inventory
    DIM SHARED Inx AS InvIndex
    DIM SHARED Cod AS Codes

    OPEN "..\Master  \Codes   .MAS" FOR RANDOM AS #1 LEN = LEN(Cod)
    OPEN "..\DataFile\Inventor.MAS" FOR RANDOM AS #2 LEN = LEN(Inv)
    OPEN "..\DataFile\Inventor.Inx" FOR RANDOM AS #3 LEN = LEN(Inx)

10  COLOR 3, 0: CLS
    LOCATE 1, 1: PRINT "1. Recode according to code only                  "
    LOCATE 2, 1: PRINT "2. Recode according to model number & distributor "

    k$ = INPUT$(1)
    IF k$ = "1" THEN 11
    IF k$ = "2" THEN 19
    IF k$ = CHR$(27) THEN 50
    GOTO 10

11  CLS
    LOCATE 2, 1: PRINT "    Old Code:  ";
    LOCATE 3, 1: PRINT "    New Code:  ";
12  EntryU 2, 16, 4, e, b, o$:  IF e THEN 10 ELSE IF b THEN 12
13  EntryU 3, 16, 4, e, b, c$:  IF e THEN 10 ELSE IF b THEN 12
    IF FindCod(c$) = 0 THEN BEEP: GOTO 13

    LOCATE 6, 1: PRINT "Press * to confirm";
    k$ = INPUT$(1)
    LOCATE 6, 1: PRINT "                  ";
    IF k$ <> "*" THEN 13
    
    FOR x = 1 TO LOF(3) \ LEN(Inx)
        GET #3, x, Inx
        IF o$ = Inx.Cod THEN
           GET #2, Inx.Num, Inv
           Inv.Cod = c$
           Inx.Typ = Cod.Typ
           Inx.Grp = Cod.Grp
           Inx.Cod = Inv.Cod
           PUT #2, Inx.Num, Inv
           PUT #3, x, Inx
           Sort = 1
           LPRINT USING "  &"; o$; Inv.Cod; Inv.Mdl; Inv.Ser
        END IF
    NEXT
    GOTO 11

19  CLS
    LOCATE 2, 1: PRINT "Model Number:";
    LOCATE 3, 1: PRINT " Distributor:";
    LOCATE 4, 1: PRINT "    New Code:";

20  EntryU 2, 16, 10, e, b, m$: IF e THEN 10 ELSE IF b THEN 20
30  EntryU 3, 16, 10, e, b, d$: IF e THEN 10 ELSE IF b THEN 20
40  EntryU 4, 16, 4, e, b, c$:  IF e THEN 10 ELSE IF b THEN 30
    IF FindCod(c$) = 0 THEN BEEP: GOTO 40

    LOCATE 6, 1: PRINT "Press * to confirm";
    k$ = INPUT$(1)
    LOCATE 6, 1: PRINT "                  ";
    IF k$ <> "*" THEN 40
    
    FOR x = 1 TO LOF(3) \ LEN(Inx)
    GET #3, x, Inx

    IF Inx.Mdl = m$ THEN
       GET #2, Inx.Num, Inv
       IF Inv.Dis = d$ THEN
          LPRINT USING "  &"; c$; Inv.Cod; Inv.Mdl; Inv.Ser
          Inv.Cod = c$
          Inx.Typ = Cod.Typ
          Inx.Grp = Cod.Grp
          Inx.Cod = Inv.Cod
          PUT #2, Inx.Num, Inv
          PUT #3, x, Inx
          Sort = 1
       END IF
    END IF
    NEXT
    GOTO 19

50  CLS
    LOCATE 2, 1: PRINT "Press [Y] to EXIT or [Any] key to continue."
    IF UCASE$(INPUT$(1)) <> "Y" THEN 10
    IF Sort THEN
       CLOSE
       SHELL "Sortf ..\DataFile\Inventor.Inx ..\DataFile\Inventor.Tmp /L33 /+1,27 /Q"
       SHELL "Sortf ..\DataFile\Inventor.Inx ..\DataFile\Inventor.Ser /L33 /+18,10 /Q"
       KILL "..\DataFile\Inventor.Inx"
       NAME "..\DataFile\Inventor.Tmp" AS "..\DataFile\Inventor.Inx"
    END IF
    CLS
    END

FUNCTION FindCod (f$)
   b = 1: t = LOF(1) \ LEN(Cod)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #1, m, Cod
      IF f$ = Cod.Cod THEN FindCod = 1: EXIT DO
      IF f$ > Cod.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

