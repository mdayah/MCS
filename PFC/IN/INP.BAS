DEFINT A-Z: DEFLNG L
DECLARE FUNCTION Confirm ()
DECLARE SUB OpenFiles ()
DECLARE SUB Heading ()
DECLARE SUB KeyHit (f$)
DECLARE SUB WinMssg (f$)
DECLARE SUB ProDisp (x)
DECLARE SUB ProLine (r, L1, L2, L3, L4)
DECLARE SUB ProSort ()
DECLARE SUB ProEdit (r, n)
DECLARE SUB ProShow ()
DECLARE SUB ProHdng ()
DECLARE SUB ProLite (m, p)
DECLARE SUB ProFile ()

'$INCLUDE: '..\BI\Library.DEC'
'$INCLUDE: '..\BI\Products.REC'
'$INCLUDE: '..\BI\ProIndex.REC'
'$INCLUDE: '..\BI\Material.REC'
'$INCLUDE: '..\BI\MatIndex.REC'
'$INCLUDE: '..\BI\Recipes.REC'

   'record variables
    COMMON SHARED Pro AS Products
    COMMON SHARED Pnx AS ProIndex
    COMMON SHARED Mat AS Material
    COMMON SHARED Mnx AS MatIndex
    COMMON SHARED Rcp AS Recipes
   'common variables
    DIM SHARED Sort    AS INTEGER
    DIM SHARED Top     AS INTEGER
    DIM SHARED Bot     AS INTEGER
    DIM SHARED Menu(6, 6) AS INTEGER
   'Common array
    DATA 1,5, 7,8, 16,7, 24,9, 34,6

DEFINT L
SUB ProDisp (x)
    IF Bot > 0 AND Top > 0 THEN
       LOCATE 3: COLOR 2, 0
       FOR x = Bot TO Top
       GET #2, x, Pnx
       GET #1, Pnx.Num, Pro
       LOCATE , 2: PRINT USING "&³&³&³&³&³&³&³&³&"; Pro.Num; Pnx.Typ; Pnx.Nam; Pro.Fra; Pro.Mes; Pro.Con; Pro.Wat; Pro.Ref; Pro.Prc
       IF CSRLIN = 49 THEN EXIT FOR
       NEXT
    END IF
END SUB

DEFLNG L
SUB Products ()
    CALL MhOpenScreenLib("..\IN\INS.SLB" + CHR$(0), e)
    CALL MhShadowOn(0, 8)
    CALL MhBlinkOff
    ProFile
    CALL MhDisplayByName(0, 0, "ProList", e)
    Bot = 1: Top = LOF(2) \ LEN(Pnx): ProDisp x
    r = 3: n = 1: m = 4: ProLite m, 1
600 IF n > 0 THEN GET #2, n, Pnx: GET #1, Pnx.Num, Pro
    COLOR 10, 0: LOCATE r, 2: PRINT USING "&³&³&³&³&³&³&³&³&"; Pro.Num; Pnx.Typ; Pnx.Nam; Pro.Fra; Pro.Mes; Pro.Con; Pro.Wat; Pro.Ref; Pro.Prc
    KeyHit n$
    COLOR 2, 0:  LOCATE r, 2: PRINT USING "&³&³&³&³&³&³&³&³&"; Pro.Num; Pnx.Typ; Pnx.Nam; Pro.Fra; Pro.Mes; Pro.Con; Pro.Wat; Pro.Ref; Pro.Prc
    IF n$ = CHR$(13) THEN n$ = MID$(" Add   Modify   Erase   Display   List ", 1 + Menu(m, 1), 1)
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        EXIT SUB
    CASE CHR$(0) + "M" 'right
        ProLite m, 0
        m = m + 1: IF m > 5 THEN m = 1
        ProLite m, 1
    CASE CHR$(0) + "K" 'left
        ProLite m, 0
        m = m - 1: IF m < 1 THEN m = 5
        ProLite m, 1
    CASE CHR$(0) + "P" 'Dn
        IF n + 1 > Top THEN 600 ELSE n = n + 1
        IF r + 1 > 48 THEN CALL scroll(2, 3, 79, 48, 1) ELSE r = r + 1
    CASE CHR$(0) + "H" 'Up
        IF n - 1 < 1 THEN 600 ELSE n = n - 1
        IF r - 1 < 3 THEN CALL BkScroll(2, 3, 79, 48, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 43
        IF Bot < 1 THEN Bot = 1
        ProDisp x: n = x + r - 48
    CASE CHR$(0) + "Q" 'PgDn
        Bot = n - r + 49
        IF Bot + 46 > Top THEN Bot = Top - 45: IF Bot < 1 THEN Bot = 1
        ProDisp x: n = x + r - 48
    CASE CHR$(0) + "G" 'Home
        Bot = 1: ProDisp x: n = r - 2
    CASE CHR$(0) + "O" 'End
        Bot = Top - 45: IF Bot < 1 THEN Bot = 1
        ProDisp x: n = Bot + r - 3
    CASE "L"
        ProLite m, 0
        m = 5: ProLite m, 1
        IF Confirm THEN
           PageNum = 0: ProHdng
           FOR y = 1 TO LOF(1) \ LEN(Pro)
           IF INKEY$ = CHR$(27) THEN EXIT FOR
           GET #2, y, Pnx
           GET #1, Pnx.Num, Pro
           IF Pro.Nam > "!" THEN
              LineNum = LineNum + 1: IF LineNum > 60 THEN ProHdng
              LPRINT USING "   &"; Pro.Typ; Pro.Num; Pro.Nam; Pro.Fra; Pro.Mes; Pro.Con;
              LPRINT USING "       &     "; Pro.Wat; Pro.Ref; Pro.Prc
           END IF
           NEXT
        END IF
    CASE "E"
        ProLite m, 0: m = 3: ProLite m, 1
        L1 = Pro.Ybp + Pro.Ypp + Pro.Ysp + Pro.Ywp
        IF L1 <> 0 THEN
           WinMssg "Active item, no erase"
        ELSE
           IF Confirm THEN
              GET #3, Pnx.Num, Rcp
              Rcp.Num = ""
              Rcp.Yld = ""
              ERASE Rcp.Itm, Rcp.Qty
              PUT #3, Pnx.Num, Rcp

              Pnx.Typ = "þþ"
              Pnx.Nam = "þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ"
              PUT #1, Pnx.Num
              PUT #1, Pnx.Num, Pro.Num
              PUT #2, n, Pnx
              ProSort
              Bot = 1: n = 1: r = 3
              ProDisp x
           END IF
        END IF
    CASE "D"
        ProLite m, 0
        m = 4: ProLite m, 1
        ProShow
    CASE "A"
        ProLite m, 0: m = 1: ProLite m, 1: y = 0
        FOR Z = 1 TO Top: GET #2, Z, Pnx                 'see if a deleted
        IF LEFT$(Pnx.Nam, 1) = "þ" THEN y = Z: EXIT FOR  'record is found
        NEXT                                             'in master file
        IF y = 0 THEN Pnx.Num = 1 + LOF(1) \ LEN(Pro): y = Pnx.Num 'append if not found
        IF Pnx.Num > 999 THEN
           WinMssg "Products Master is full"
        ELSE
           GET #1, Pnx.Num, Pro
           COLOR 2, 0: LOCATE r, 2: PRINT USING "###³  ³                                ³    ³     ³    ³      ³       ³       "; Pnx.Num
           ProEdit r, y
        END IF
    CASE "M"
        ProLite m, 0: m = 2: ProLite m, 1
        ProEdit r, n
    CASE ELSE
        WinMssg "Select from highlited keys"
    END SELECT
    GOTO 600
END SUB

DEFINT L
SUB ProEdit (r, n)
    p$ = Pnx.Typ + Pnx.Nam
611 EntryU r, 6, 2, e, b, Pro.Typ:     IF e THEN 620 ELSE IF b THEN 611
    IF Pro.Typ < "!" THEN WinMssg "Type please": GOTO 611
612 EntryS r, 9, 32, e, b, Pro.Nam:    IF e THEN 620 ELSE IF b THEN 611
    IF Pro.Nam < "!" THEN WinMssg "Name must not be blank!!!": GOTO 612
613 EntryN r, 42, 4, 0, e, b, Pro.Fra: IF e THEN 620 ELSE IF b THEN 612
    IF Pro.Fra < "   1" THEN WinMssg "Fraction must not be 0": GOTO 613
614 EntryS r, 47, 5, e, b, Pro.Mes:    IF e THEN 620 ELSE IF b THEN 613
    IF Pro.Mes < "!" THEN WinMssg "How about a measure!!!!!!": GOTO 614
615 EntryS r, 53, 4, e, b, Pro.Con:    IF e THEN 620 ELSE IF b THEN 614
    IF Pro.Con < "!" THEN WinMssg "Dont you want a container": GOTO 615
616 EntryN r, 58, 6, 2, e, b, Pro.Wat: IF e THEN 620 ELSE IF b THEN 615
    IF Pro.Wat < "   .01" THEN WinMssg "Weight Please!!!!!!!!": GOTO 616
617 EntryN r, 65, 7, 3, e, b, Pro.Ref: IF e THEN 620 ELSE IF b THEN 616
    IF Pro.Ref < "  .001" THEN WinMssg "I must have a price!": GOTO 617
618 EntryN r, 73, 7, 2, e, b, Pro.Prc: IF e THEN 620 ELSE IF b THEN 617
    IF Pro.Prc < "   .01" THEN WinMssg "I must have a price!": GOTO 618
    IF Confirm THEN
       Pro.Num = RIGHT$(STR$(1000 + Pnx.Num), 3)
       Pnx.Typ = Pro.Typ
       Pnx.Nam = Pro.Nam
       PUT #2, n, Pnx
       PUT #1, Pnx.Num, Pro
       IF p$ <> Pnx.Typ + Pnx.Nam THEN
          Top = LOF(1) \ LEN(Pro)
          ProSort
          ProDisp x
          n = r - 2
       END IF
    ELSE
       GOTO 611
    END IF
620
END SUB

DEFSNG A-Z
SUB ProFile ()
    CLOSE
    OPEN "..\IN\Products.MAS" FOR RANDOM AS #1 LEN = LEN(Pro)
    OPEN "..\IN\Products.INX" FOR RANDOM AS #2 LEN = LEN(Pnx)
    OPEN "..\IN\Recipes .MAS" FOR RANDOM AS #3 LEN = LEN(Rcp)
END SUB

SUB ProHdng ()
    Heading
    LPRINT TAB(72); "PRODUCT LISTING": LPRINT
    LPRINT "  Type  Num            Description              Units per Container     Case Weight       Unit Cost      Case Price    "
    LPRINT "  ====  ===   ==============================   =====================   ==============   =============  ================"
END SUB

DEFINT A-Z
SUB ProInit ()
    FOR y = 1 TO 5
    FOR x = 1 TO 2
        READ Menu(y, x)
    NEXT
    NEXT
END SUB

DEFLNG L
DEFSNG A-K, M-Q, S-Z
SUB ProLine (r, L1, L2, L3, L4)
    LOCATE r, 21: PRINT USING "#######.##"; L1 / 100;
    LOCATE r, 32: PRINT USING "#######.##"; L2 / 100;
    LOCATE r, 43: IF L1 <> 0 THEN PRINT USING "###.##"; L2 / L1;  ELSE PRINT "      ";
    LOCATE r, 51: PRINT USING "#######.##"; L3 / 100;
    LOCATE r, 62: PRINT USING "#######.##"; L4 / 100;
    LOCATE r, 73: IF L3 <> 0 THEN PRINT USING "###.##"; L4 / L3;  ELSE PRINT "      ";
END SUB

DEFINT A-Q, S-Z
SUB ProLite (m, p)
    IF p THEN f = 4: b = 7 ELSE f = 14: b = 1
    LOCATE 50, Menu(m, 1) + 40
    f$ = MID$(" Add   Modify   Erase   Display   List ", Menu(m, 1), Menu(m, 2))
    COLOR f, b: PRINT MID$(f$, 1, 2);
    COLOR f - 3, b: PRINT MID$(f$, 3);
END SUB

DEFSNG A-Z
SUB ProShow ()
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "Product", e)
    
    COLOR 11, 3
    LOCATE 9, 14:  PRINT Pro.Typ; "-"; Pro.Num
    LOCATE 11, 14: PRINT Pro.Nam
    LOCATE 13, 14: PRINT LTRIM$(Pro.Fra); " "; RTRIM$(Pro.Mes); "/"; Pro.Con
    LOCATE 9, 65:  PRINT USING " ###.##"; VAL(Pro.Ref) * VAL(Pro.Fra);
    LOCATE 10, 65: PRINT Pro.Prc;
    LOCATE 11, 66: PRINT Pro.Wat;
    
    COLOR 14, 3: ProLine 20, Pro.Moq, 0, Pro.Yoq, 0
    COLOR 14, 3: ProLine 21, Pro.Msq, Pro.Msp, Pro.Ysq, Pro.Ysp
    COLOR 14, 3: ProLine 22, Pro.Msq, Pro.Msc, Pro.Ysq, Pro.Ysc
    COLOR 15, 1: ProLine 24, Pro.Msq, Pro.Msp - Pro.Msc, Pro.Ysq, Pro.Ysp - Pro.Ysc
  
    COLOR 14, 3: ProLine 31, Pro.Mbq, Pro.Mbp, Pro.Ybq, Pro.Ybp
    COLOR 14, 3: ProLine 32, Pro.Mpq, Pro.Mpp, Pro.Ypq, Pro.Ypp
    COLOR 15, 1: ProLine 34, Pro.Mbq + Pro.Mpq, Pro.Mbp + Pro.Mpp, Pro.Ybq + Pro.Ypq, Pro.Ybp + Pro.Ypp
    COLOR 14, 3: ProLine 36, Pro.Msq, Pro.Msc, Pro.Ysq, Pro.Ysc
    COLOR 14, 3: ProLine 37, Pro.Mwq, Pro.Mwp, Pro.Ywq, Pro.Ywp
    COLOR 14, 3: ProLine 38, 0, 0, Pro.Ygq, Pro.Ygp
    COLOR 15, 1: ProLine 40, Pro.Msq + Pro.Mwq, Pro.Msc + Pro.Mwp, Pro.Ysq + Pro.Ywq + Pro.Ygq, Pro.Ysc + Pro.Ywp + Pro.Ygp
    COLOR 14, 3: ProLine 42, Pro.Mbq + Pro.Mpq - Pro.Msq - Pro.Mwq, Pro.Mbp + Pro.Mpp - Pro.Msc - Pro.Mwp, Pro.Ybq + Pro.Ypq - Pro.Ysq - Pro.Ywq - Pro.Ygq, Pro.Ybp + Pro.Ypp - Pro.Ysc - Pro.Ywp - Pro.Ygp
    
    KeyHit f$: PCOPY 1, 0
END SUB

SUB ProSort ()
    CLOSE
    LOCATE 1
    SHELL "SortF ..\IN\Products.Inx ..\IN\Products /L36 /+1,34 /Q"
    KILL "..\IN\Products.Inx"
    NAME "..\IN\Products" AS "..\IN\Products.INX"
    ProFile
END SUB

