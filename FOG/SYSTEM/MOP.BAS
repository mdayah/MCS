DEFINT A-Z: DEFLNG L
DECLARE FUNCTION FindCmb (f$)
DECLARE FUNCTION FindCod (f$)
DECLARE FUNCTION FindCom (f$)
DECLARE FUNCTION FindIni (f$)
DECLARE FUNCTION FindPas (f$)
DECLARE FUNCTION FindPrc (f$)
DECLARE FUNCTION ChkCode (c$)
DECLARE FUNCTION ChkPric (c$)
DECLARE FUNCTION PassWord (f$)
DECLARE FUNCTION Confirm ()

DECLARE SUB DispCmb (x)
DECLARE SUB DispCod (x)
DECLARE SUB DispCtr (x)
DECLARE SUB DispPas (x)
DECLARE SUB DispPrc (x)
DECLARE SUB DispRat (x)
DECLARE SUB DispSto (x)
DECLARE SUB HCenter ()
DECLARE SUB HCodes ()
DECLARE SUB HCombo ()
DECLARE SUB Heading ()
DECLARE SUB HPrices ()
DECLARE SUB HRates ()
DECLARE SUB HStores ()
DECLARE SUB HWords ()
DECLARE SUB HitKey (f$)
DECLARE SUB KeyHit (f$)
DECLARE SUB OpenFiles ()

DECLARE SUB ShowAmt (r, L1, L2)
DECLARE SUB ShowCtr ()
DECLARE SUB ShowSto ()
DECLARE SUB SortPas ()
DECLARE SUB SortRat ()
DECLARE SUB TimeDate ()
DECLARE SUB WinCode (t$)
DECLARE SUB WinMssg (f$)
DECLARE SUB KeyPres (f$)

DECLARE FUNCTION FJulian$ (J&)
DECLARE FUNCTION TJulian& (d$)
DECLARE FUNCTION DayWeek$ (d$)
DECLARE FUNCTION DateDif& (d$, t$)
DECLARE FUNCTION DayMath$ (d$, d%)
DECLARE FUNCTION MonMath$ (d$, d%)

DECLARE SUB Picture (r, c, s, f$)
DECLARE SUB EntryX (r, c, e, b, f$)
DECLARE SUB EntryT (r, c, e, b, f$)
DECLARE SUB EntryD (r, c, e, b, f$)
DECLARE SUB EntryU (r, c, s, e, b, f$)
DECLARE SUB EntryS (r, c, s, e, b, f$)
DECLARE SUB EntryN (r, c, s, d, e, b, f$)

REM $INCLUDE: '..\Include\Codes.REC'
REM $INCLUDE: '..\Include\Combos.REC'
REM $INCLUDE: '..\Include\Prices.REC'
REM $INCLUDE: '..\Include\Rates.REC'
REM $INCLUDE: '..\Include\PassWord.REC'
REM $INCLUDE: '..\Include\Stores.REC'
REM $INCLUDE: '..\Include\Centers.REC'

TYPE OldInventory
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

    DIM SHARED Cod      AS Codes      '01
    DIM SHARED Cmb      AS Combos     '02
    DIM SHARED Prc      AS Prices     '03
    DIM SHARED Rat      AS Rates      '04
    DIM SHARED Pas      AS PassWord   '05
    DIM SHARED Sto      AS Stores     '06
    DIM SHARED Ctr      AS Centers    '07
    DIM SHARED Oin      AS OldInventory
    
    DIM SHARED Top      AS INTEGER
    DIM SHARED Bot      AS INTEGER
    DIM SHARED LineNum  AS INTEGER
    DIM SHARED PageNum  AS INTEGER
    
    KEY OFF
    KEY 1, "1"
    KEY 2, "2"
    KEY 3, "3"
    KEY 4, "4"
    KEY 5, "5"
    KEY 6, "6"
    KEY 7, "7"
    KEY 8, "8"
    KEY 9, "9"
    KEY 10, "0"

    ON ERROR GOTO Errors
    SCREEN 0, 0, 0, 0: WIDTH 80, 50
    SCREEN 0, 0, 1, 1: PCOPY 0, 1
    SCREEN 0, 0, 0, 0
    CALL MhOpenScreenLib("MOP.SLB" + CHR$(0), e)
    CALL MhBlinkOff
    CALL MhShadowOn(0, 8)

'ON ERROR RESUME NEXT
'OPEN "..\Master\Password.Nam" FOR RANDOM LOCK WRITE AS #8 LEN = LEN(Pas)
'FOR x = 1 TO LOF(8) \ LEN(Pas)
'GET #8, x, Pas
'PRINT Pas.Pas, Pas.Nam, Pas.Ini, Pas.Lvl
'Pas.Pas = CHR$(ASC(MID$(Pas.Pas, 1, 1)) + 34) + CHR$(ASC(MID$(Pas.Pas, 2, 1)) + 70) + CHR$(ASC(MID$(Pas.Pas, 3, 1)) + 81) + CHR$(ASC(MID$(Pas.Pas, 4, 1)) + 63)
'PUT #8, x, Pas
'NEXT
'CLOSE
'SortPas
'STOP

    OpenFiles
    FOR x = 1 TO LOF(1) \ LEN(Cod)
    GET #1, x, Cod
    Cod.Dep = "48"
    PUT #1, x, Cod
    NEXT

Menu: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
10  CALL MhDisplayByName(0, 0, "MO-Menu", e)
    CLOSE : HitKey f$
    IF f$ = CHR$(27) THEN GOTO Terminate
    f = VAL(f$): IF f < 1 THEN 10
    ON f GOTO Codes, Combo, Price, Rates, Words, Store, Center, Check, Compress
    GOTO 10
Codes: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    OpenFiles
    CALL MhDisplayByName(0, 0, "Codes", e)
    Bot = 1: Top = LOF(1) \ LEN(Cod): DispCod x
    r = 2: n = 1
100 IF n > 0 THEN GET #1, n, Cod
    COLOR 11, 1: LOCATE r, 2: PRINT USING "  &   ณ&ณ &  ณ &  ณ &  ณ&ณ &ณ&ณ & ณ  &  ณ &  "; Cod.Del; Cod.Cod; Cod.Typ; Cod.Grp; Cod.Cmb; Cod.Des; Cod.Rat; Cod.Fmv; Cod.Dep; Cod.Dtr; Cod.Dfm;
    KeyHit n$
    COLOR 1, 3: : LOCATE r, 2: PRINT USING "  &   ณ&ณ &  ณ &  ณ &  ณ&ณ &ณ&ณ & ณ  &  ณ &  "; Cod.Del; Cod.Cod; Cod.Typ; Cod.Grp; Cod.Cmb; Cod.Des; Cod.Rat; Cod.Fmv; Cod.Dep; Cod.Dtr; Cod.Dfm;
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        GOTO 10
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > Top THEN 100 ELSE n = n + 1
        IF r + 1 > 48 THEN CALL Scroll(2, 2, 79, 48, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 THEN 100 ELSE n = n - 1
        IF r - 1 < 2 THEN CALL BkScroll(2, 2, 79, 48, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 45: IF Bot < 1 THEN Bot = 1
        DispCod x
        n = x - 48 + r
    CASE CHR$(0) + "Q" 'PgDn
        Bot = 49 - r + n
        IF Bot > Top - 46 THEN Bot = Top - 46: n = Top + r - 48 ELSE n = n + 47
        DispCod x
    CASE CHR$(0) + "G" 'Home
        Bot = 1: DispCod x
        n = r - 1
    CASE CHR$(0) + "O" 'End
        Bot = Top - 46: DispCod x
        n = Top + r - 48
    CASE "E"
        IF PassWord(J$) THEN
           Cod.Cod = "þþþþ"
           Cod.Typ = "þ"
           Cod.Des = "þþþþþþþþþþþþþþþþþþþþþþþþ"
           Cod.Rat = "þþþ"
           Cod.Del = "þ"
           Cod.Grp = "þþ"
           Cod.Cmb = "þþ"
           Cod.Fmv = "þþþ"
           Cod.Dtr = "þ"
           Cod.Dfm = "þ"
           Cod.Dep = "þþ"
           Cod.Fil = "þþþþþþ"
           PUT #1, n, Cod
           CLOSE : SHELL "SortF ..\Master\Codes.Mas ..\Master\Codes /L50 /+1,4 /Q"
           CLOSE : SHELL "SortF ..\Master\Codes.Mas ..\Master\Codes.Cmb /L50 /+36,2 /+1,4 /Q"
           KILL "..\Master\Codes.Mas": NAME "..\Master\Codes" AS "..\Master\Codes.Mas"
           OpenFiles
           Bot = n - r + 2
           DispCod x
           GOTO 100
        END IF
    CASE "F"
        COLOR 0, 2: LOCATE 17
        LOCATE , 41: PRINT "ษอออออออออออออออออป"
        LOCATE , 41: PRINT "บ Find Code:      บ"
        LOCATE , 41: PRINT "ศอออออออออออออออออผ"
        COLOR 10, 3: LOCATE 18
        LOCATE , 40: PRINT "Û"
        LOCATE , 40: PRINT "Û"
        LOCATE , 40: PRINT "฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"
105     EntryU 18, 54, 4, e, b, Cod.Cod: IF e THEN 106 ELSE IF b THEN 105
        x = FindCod(Cod.Cod): IF x = 0 THEN WinMssg "Code not found!": GOTO 105
        Bot = x - r + 2
        IF Bot > Top - 46 THEN Bot = Top - 46: r = 48 - Top + x ELSE IF Bot < 1 THEN Bot = 1: r = x + 1
        n = Bot + r - 2
106     DispCod x
    CASE "N"
        Cod.Cod = "": COLOR 3, 0: LOCATE r, 2: PRINT "      ณ    ณ    ณ     ณ     ณ                        ณ    ณ   ณ    ณ     ณ    ";
110     EntryU r, 9, 4, e, b, Cod.Cod:    IF e THEN 100 ELSE IF b THEN 110
        IF FindCod(Cod.Cod) THEN WinMssg "Duplicate Code exists": GOTO 110
        IF Cod.Cod < "!" THEN WinMssg "Code must not be blank": GOTO 110
        Cod.Typ = ""
        Cod.Grp = ""
        Cod.Cmb = ""
        Cod.Des = ""
        Cod.Rat = ""
        Cod.Del = ""
        Cod.Fmv = ""
        Cod.Dtr = ""
        Cod.Dfm = ""
        Cod.Dep = ""
111     EntryU r, 15, 1, e, b, Cod.Typ:    IF e THEN 100 ELSE IF b THEN 110
        IF Cod.Typ < "!" THEN WinMssg "Type must not be blank": GOTO 111
112     EntryU r, 20, 2, e, b, Cod.Grp:    IF e THEN 100 ELSE IF b THEN 111
        IF Cod.Grp < "!" THEN WinMssg "Group must not be blank": GOTO 112
113     EntryU r, 26, 2, e, b, Cod.Cmb:    IF e THEN 100 ELSE IF b THEN 112
114     EntryS r, 31, 24, e, b, Cod.Des:   IF e THEN 100 ELSE IF b THEN 113
        IF Cod.Des < "!" THEN WinMssg "Description must not be blank": GOTO 114
115     EntryN r, 57, 3, 0, e, b, Cod.Rat: IF e THEN 100 ELSE IF b THEN 114
        IF Cod.Rat < "  1" THEN WinMssg "Rate must not be zero": GOTO 115
        IF VAL(Cod.Rat) > LOF(4) \ LEN(Rat) THEN WinMssg "Rate does not exist": GOTO 115
116     EntryN r, 61, 3, 1, e, b, Cod.Fmv: IF e THEN 100 ELSE IF b THEN 115
        IF Cod.Fmv < "0.0" THEN WinMssg "FMV must not be zero": GOTO 116
117     EntryN r, 66, 2, 0, e, b, Cod.Dep: IF e THEN 100 ELSE IF b THEN 116
118     EntryN r, 72, 1, 0, e, b, Cod.Dtr: IF e THEN 100 ELSE IF b THEN 117
119     EntryN r, 77, 1, 0, e, b, Cod.Dfm: IF e THEN 100 ELSE IF b THEN 118
        IF PassWord(J$) THEN
           Top = 1 + LOF(1) \ LEN(Cod)
           PUT #1, Top, Cod
           CLOSE : SHELL "SortF ..\Master\Codes.Mas ..\Master\Codes /L50 /+1,4 /Q"
           CLOSE : SHELL "SortF ..\Master\Codes.Mas ..\Master\Codes.Cmb /L50 /+36,2 /+1,4 /Q"
           KILL "..\Master\Codes.Mas": NAME "..\Master\Codes" AS "..\Master\Codes.Mas"
           GOTO Codes
        ELSE
           GOTO 119
        END IF
    CASE "M"
151     EntryU r, 15, 1, e, b, Cod.Typ:    IF e THEN 100 ELSE IF b THEN 151
        IF Cod.Typ < "!" THEN WinMssg "Type must not be blank": GOTO 151
152     EntryU r, 20, 2, e, b, Cod.Grp:    IF e THEN 100 ELSE IF b THEN 151
        IF Cod.Grp < "!" THEN WinMssg "Group must not be blank": GOTO 152
153     EntryU r, 26, 2, e, b, Cod.Cmb:    IF e THEN 100 ELSE IF b THEN 152
154     EntryS r, 31, 24, e, b, Cod.Des:   IF e THEN 100 ELSE IF b THEN 153
        IF Cod.Des < "!" THEN WinMssg "Description must not be blank": GOTO 154
155     EntryN r, 57, 3, 0, e, b, Cod.Rat: IF e THEN 100 ELSE IF b THEN 154
        IF Cod.Rat < "  1" THEN WinMssg "Rate must not be zero": GOTO 155
        IF VAL(Cod.Rat) > LOF(4) \ LEN(Rat) THEN WinMssg "Rate does not exist": GOTO 155
156     EntryN r, 61, 3, 1, e, b, Cod.Fmv: IF e THEN 100 ELSE IF b THEN 155
        IF Cod.Fmv < "0.0" THEN WinMssg "FMV must not be zero": GOTO 156
157     EntryN r, 66, 2, 0, e, b, Cod.Dep: IF e THEN 100 ELSE IF b THEN 156
158     EntryN r, 72, 1, 0, e, b, Cod.Dtr: IF e THEN 100 ELSE IF b THEN 157
159     EntryN r, 77, 1, 0, e, b, Cod.Dfm: IF e THEN 100 ELSE IF b THEN 158
        IF PassWord(J$) THEN
           PUT #1, n, Cod
           CLOSE : SHELL "SortF ..\Master\Codes.Mas ..\Master\Codes /L50 /+1,4 /Q"
           CLOSE : SHELL "SortF ..\Master\Codes.Mas ..\Master\Codes.Cmb /L50 /+36,2 /+1,4 /Q"
           KILL "..\Master\Codes.Mas": NAME "..\Master\Codes" AS "..\Master\Codes.Mas"
           OpenFiles
           Bot = n - r + 2
           DispCod x
           GOTO 100
        ELSE
           GOTO 159
        END IF
    CASE "P"
        IF Confirm() THEN
           PageNum = 0: HCodes
           FOR x = 1 TO LOF(1) / LEN(Cod)
           GET #1, x, Cod
           IF INKEY$ = CHR$(27) THEN EXIT FOR
           IF LineNum > 60 THEN HCodes
           LineNum = LineNum + 1
           LPRINT USING "&  &    &   &   &   &  &    &    &    &  "; Cod.Cod; Cod.Typ; Cod.Grp; Cod.Des; Cod.Cmb; Cod.Fmv; Cod.Dep; Cod.Dtr; Cod.Dfm; Cod.Rat;
           IF Cod.Rat > "   " THEN
              GET #10, VAL(Cod.Rat), Rat
              FOR z = 1 TO 3
                 LPRINT USING " & &"; Rat.Mtr(z); Rat.Mrt(z);
                 LPRINT USING " & &"; Rat.Wtr(z); Rat.Wrt(z);
              NEXT
           END IF
           LPRINT
           NEXT
           LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(1) / LEN(Cod); "CODES in the master file."
        END IF
    CASE "A"
        WinMssg "Function not implemented yet"
    CASE ELSE
        WinMssg "Select from highlited keys"
    END SELECT
    GOTO 100
Combo: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    OpenFiles
    CALL MhDisplayByName(0, 0, "Combos", e)
    Bot = 1: Top = LOF(2) \ LEN(Cmb): DispCmb x
    r = 2: n = 1
300 IF n > 0 THEN GET #2, n, Cmb
    COLOR 11, 1: LOCATE r, 3: PRINT USING "  & ณ &  ณ   &   ณ  &  ณ  & ณ & ณ & ณ & ณ & "; Cmb.Cod; Cmb.Des; Cmb.Rat; Cmb.Dtr; Cmb.Dfm; Cmb.Cde(1); Cmb.Cde(2); Cmb.Cde(3); Cmb.Cde(4);
    KeyHit n$
    COLOR 1, 3:  LOCATE r, 3: PRINT USING "  & ณ &  ณ   &   ณ  &  ณ  & ณ & ณ & ณ & ณ & "; Cmb.Cod; Cmb.Des; Cmb.Rat; Cmb.Dtr; Cmb.Dfm; Cmb.Cde(1); Cmb.Cde(2); Cmb.Cde(3); Cmb.Cde(4);
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        GOTO 10
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > Top THEN 300 ELSE n = n + 1
        IF r + 1 > 48 THEN CALL Scroll(3, 2, 77, 48, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 THEN 300 ELSE n = n - 1
        IF r - 1 < 2 THEN CALL BkScroll(3, 2, 77, 48, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 45: IF Bot < 1 THEN Bot = 1
        DispCmb x
        n = x - 48 + r
    CASE CHR$(0) + "Q" 'PgDn
        Bot = 49 - r + n
        IF Bot > Top - 46 THEN Bot = Top - 46: n = Top + r - 48 ELSE n = n + 47
        DispCmb x
    CASE CHR$(0) + "G" 'Home
        Bot = 1: DispCmb x: n = r - 1
    CASE CHR$(0) + "O" 'End
        Bot = Top - 46: DispCmb x: n = Top + r - 48
    CASE "F"
        COLOR 0, 2: LOCATE 17
        LOCATE , 41: PRINT "ษออออออออออออออออออป"
        LOCATE , 41: PRINT "บ Find Combo:      บ"
        LOCATE , 41: PRINT "ศออออออออออออออออออผ"
        COLOR 10, 3: LOCATE 18
        LOCATE , 40: PRINT "Û"
        LOCATE , 40: PRINT "Û"
        LOCATE , 40: PRINT "฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"
305     EntryU 18, 55, 4, e, b, Cmb.Cod: IF e THEN 306 ELSE IF b THEN 305
        x = FindCmb(Cmb.Cod): IF x = 0 THEN WinMssg "Combo not found!": GOTO 305
        Bot = x - r + 2
        IF Bot > Top - 46 THEN Bot = Top - 46: r = 48 - Top + x ELSE IF Bot < 1 THEN Bot = 1: r = x + 1
        n = Bot + r - 2
306     DispCmb x
    CASE "N"
        Cmb.Cod = "": COLOR 3, 0: LOCATE r, 3: PRINT "       ณ                           ณ         ณ     ณ    ณ    ณ    ณ    ณ    "
310     EntryU r, 5, 4, e, b, Cmb.Cod:   IF e THEN 300 ELSE IF b THEN 310
        IF FindCmb(Cmb.Cod) THEN WinMssg "Duplicate Combo exists": GOTO 310
        IF Cmb.Cod < "!" THEN WinMssg "Code must not be blank": GOTO 310
        Cmb.Des = ""
        Cmb.Rat = ""
        Cmb.Cde(1) = ""
        Cmb.Cde(2) = ""
        Cmb.Cde(3) = ""
        Cmb.Cde(4) = ""
311     EntryS r, 12, 24, e, b, Cmb.Des:   IF e THEN 300 ELSE IF b THEN 310
        IF Cmb.Des < "!" THEN WinMssg "Description must not be blank": GOTO 311
312     EntryN r, 42, 3, 0, e, b, Cmb.Rat: IF e THEN 300 ELSE IF b THEN 311
        IF Cmb.Rat < "  1" THEN WinMssg "Rate must not be zero": GOTO 312
        IF VAL(Cmb.Rat) > LOF(4) \ LEN(Rat) THEN WinMssg "Rate does not exist": GOTO 312
313     EntryN r, 51, 1, 0, e, b, Cmb.Dtr: IF e THEN 300 ELSE IF b THEN 312
        IF Cmb.Dtr < "1" THEN WinMssg "Drop terms ratio please": GOTO 313
314     EntryN r, 57, 1, 0, e, b, Cmb.Dfm: IF e THEN 300 ELSE IF b THEN 313
        IF Cmb.Dfm < "1" THEN WinMssg "Drop terms ratio please": GOTO 314
315     EntryU r, 61, 2, e, b, Cmb.Cde(1): IF e THEN 300 ELSE IF b THEN 314
        IF Cmb.Cde(1) = "  " THEN 316
        IF FindCom(Cmb.Cde(1)) = 0 THEN WinCode Cmb.Cde(1): GOTO 315
316     EntryU r, 66, 2, e, b, Cmb.Cde(2): IF e THEN 300 ELSE IF b THEN 315
        IF Cmb.Cde(2) = "  " THEN 317
        IF FindCom(Cmb.Cde(2)) = 0 THEN WinCode Cmb.Cde(2): GOTO 316
317     EntryU r, 71, 2, e, b, Cmb.Cde(3): IF e THEN 300 ELSE IF b THEN 316
        IF Cmb.Cde(3) = "  " THEN 318
        IF FindCom(Cmb.Cde(3)) = 0 THEN WinCode Cmb.Cde(3): GOTO 317
318     EntryU r, 76, 2, e, b, Cmb.Cde(4): IF e THEN 300 ELSE IF b THEN 317
        IF Cmb.Cde(4) = "  " THEN 319
        IF FindCom(Cmb.Cde(4)) = 0 THEN WinCode Cmb.Cde(4): GOTO 318
319     IF PassWord(J$) THEN
           Top = 1 + LOF(2) \ LEN(Cmb)
           PUT #2, Top, Cmb
           CLOSE : SHELL "SortF ..\Master\Combos.Mas ..\Master\Combo /L49 /+1,4 /Q"
           KILL "..\Master\Combos.Mas": NAME "..\Master\Combo" AS "..\Master\Combos.Mas"
           GOTO Combo
        ELSE
           GOTO 318
        END IF
    CASE "M"
320     EntryS r, 12, 24, e, b, Cmb.Des:   IF e THEN 300 ELSE IF b THEN 320
        IF Cmb.Des < "!" THEN WinMssg "Description must not be blank": GOTO 320
321     EntryN r, 42, 3, 0, e, b, Cmb.Rat: IF e THEN 300 ELSE IF b THEN 320
        IF Cmb.Rat < "  1" THEN WinMssg "Rate must not be zero": GOTO 321
        IF VAL(Cmb.Rat) > LOF(4) \ LEN(Rat) THEN WinMssg "Rate does not exist": GOTO 321
322     EntryN r, 51, 1, 0, e, b, Cmb.Dtr: IF e THEN 300 ELSE IF b THEN 321
        IF Cmb.Dtr < "1" THEN WinMssg "Drop terms ratio please": GOTO 322
323     EntryN r, 57, 1, 0, e, b, Cmb.Dfm: IF e THEN 300 ELSE IF b THEN 322
        IF Cmb.Dfm < "1" THEN WinMssg "Drop terms ratio please": GOTO 323
324     EntryU r, 61, 2, e, b, Cmb.Cde(1): IF e THEN 300 ELSE IF b THEN 323
        IF Cmb.Cde(1) = "  " THEN 325
        IF FindCom(Cmb.Cde(1)) = 0 THEN WinCode Cmb.Cde(1): GOTO 324
325     EntryU r, 66, 2, e, b, Cmb.Cde(2): IF e THEN 300 ELSE IF b THEN 324
        IF Cmb.Cde(2) = "  " THEN 326
        IF FindCom(Cmb.Cde(2)) = 0 THEN WinCode Cmb.Cde(2): GOTO 325
326     EntryU r, 71, 2, e, b, Cmb.Cde(3): IF e THEN 300 ELSE IF b THEN 325
        IF Cmb.Cde(3) = "  " THEN 327
        IF FindCom(Cmb.Cde(3)) = 0 THEN WinCode Cmb.Cde(3): GOTO 326
327     EntryU r, 76, 2, e, b, Cmb.Cde(4): IF e THEN 300 ELSE IF b THEN 326
        IF Cmb.Cde(4) = "  " THEN 328
        IF FindCom(Cmb.Cde(4)) = 0 THEN WinCode Cmb.Cde(4): GOTO 327
328     IF PassWord(J$) THEN PUT #2, n, Cmb ELSE GOTO 327
    CASE "P"
        IF Confirm() THEN
           PageNum = 0: HCombo
           FOR x = 1 TO LOF(2) / LEN(Cmb)
           GET #2, x, Cmb
           IF INKEY$ = CHR$(27) THEN EXIT FOR
           IF LineNum > 60 THEN HCombo
           LineNum = LineNum + 1
           LPRINT USING "&   &    &     &     &     &    &   "; Cmb.Cod; Cmb.Des; Cmb.Cde(1); Cmb.Cde(2); Cmb.Cde(3); Cmb.Cde(4); Cmb.Rat;
           IF Cmb.Rat > "   " THEN
              GET #10, VAL(Cmb.Rat), Rat
              FOR z = 1 TO 3
                 LPRINT USING " & &"; Rat.Mtr(z); Rat.Mrt(z);
                 LPRINT USING " & &"; Rat.Wtr(z); Rat.Wrt(z);
              NEXT
           END IF
           LPRINT
           NEXT
           LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(2) / LEN(Cmb); "COMBOS in the master file."
        END IF
    CASE "E"
        IF PassWord(J$) THEN
           Cmb.Cod = "þþþþ"
           Cmb.Des = "þþþþþþþþþþþþþþþþþþþþþþþþ"
           Cmb.Rat = "þþþ"
           Cmb.Cde(1) = "þþ"
           Cmb.Cde(2) = "þþ"
           Cmb.Cde(3) = "þþ"
           Cmb.Cde(4) = "þþ"
           Cmb.Grp = "þþ"
           Cmb.Dtr = "þ"
           Cmb.Dfm = "þ"
           PUT #2, n, Cmb
           CLOSE : SHELL "SortF ..\Master\Combos.Mas ..\Master\Combo /L49 /+1,4 /Q"
           KILL "..\Master\Combos.Mas": NAME "..\Master\Combo" AS "..\Master\Combos.Mas"
           OpenFiles
           Bot = n - r + 2
           DispCmb x
           GOTO 300
        END IF
    CASE "A"
        WinMssg "Function not implemented yet"
    CASE ELSE
        WinMssg "Select from highlited keys"
    END SELECT
    GOTO 300
Price: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    OpenFiles
    CALL MhDisplayByName(0, 0, "Prices", e)
    Bot = 1: Top = LOF(3) \ LEN(Prc): DispPrc x
    r = 3: n = 1
200 IF n > 0 THEN GET #3, n, Prc
    COLOR 11, 1: LOCATE r, 2: PRINT USING " & ณ& ณ&ณ&ณ&ณ&ณ&ณ&ณ&ณ & ณ & "; Prc.Del; Prc.Dis; Prc.Mdl; Prc.Cod; Prc.Old; Prc.New; Prc.Dat; Prc.Min; Prc.Max; Prc.Par; Prc.Lab;
    KeyHit n$
    COLOR 1, 3:  LOCATE r, 2: PRINT USING " & ณ& ณ&ณ&ณ&ณ&ณ&ณ&ณ&ณ & ณ & "; Prc.Del; Prc.Dis; Prc.Mdl; Prc.Cod; Prc.Old; Prc.New; Prc.Dat; Prc.Min; Prc.Max; Prc.Par; Prc.Lab;
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        GOTO 10
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > Top THEN 200 ELSE n = n + 1
        IF r + 1 > 48 THEN CALL Scroll(2, 3, 79, 48, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 THEN 200 ELSE n = n - 1
        IF r - 1 < 3 THEN CALL BkScroll(2, 3, 79, 48, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 45: IF Bot < 1 THEN Bot = 1
        DispPrc x: n = x - 48 + r
    CASE CHR$(0) + "Q" 'PgDn
        Bot = 48 - r + n
        IF Bot > Top - 45 THEN Bot = Top - 45
        DispPrc x
        n = x + r - 48
    CASE CHR$(0) + "G" 'Home
        Bot = 1: DispPrc x: n = r - 2
    CASE CHR$(0) + "O" 'End
        Bot = Top - 45: DispPrc x: n = x + r - 48
    CASE "F"
        COLOR 0, 2: LOCATE 17
        LOCATE , 41: PRINT "ษอออออออออออัอออออออออออัออออป"
        LOCATE , 41: PRINT "บDistributorณ Model Nง  ณCodeบ"
        LOCATE , 41: PRINT "บ           ณ           ณ    บ"
        LOCATE , 41: PRINT "ศอออออออออออฯอออออออออออฯออออผ"
        COLOR 10, 3: LOCATE 18
        LOCATE , 40: PRINT "Û"
        LOCATE , 40: PRINT "Û"
        LOCATE , 40: PRINT "Û"
        LOCATE , 40: PRINT "฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"

205     EntryU 19, 42, 10, e, b, Prc.Dis: IF e THEN 208 ELSE IF b THEN 205
206     EntryU 19, 54, 10, e, b, Prc.Mdl: IF e THEN 208 ELSE IF b THEN 205
207     EntryU 19, 66, 4, e, b, Prc.Cod:  IF e THEN 208 ELSE IF b THEN 206
        x = FindPrc(Prc.Dis + Prc.Mdl + Prc.Cod): IF x = 0 THEN WinMssg "Price not found!": GOTO 205
        Bot = x - r + 2
        IF Bot > Top - 46 THEN Bot = Top - 46: r = 48 - Top + x ELSE IF Bot < 1 THEN Bot = 1: r = x + 1
        n = Bot + r - 2
208     DispPrc x
    CASE "N"
        Prc.Cod = ""
        Prc.Dis = ""
        Prc.Mdl = ""
        COLOR 3, 0: LOCATE r, 2: PRINT "   ณ           ณ          ณ    ณ       ณ       ณ          ณ   ณ   ณ     ณ     "
210     EntryU r, 6, 10, e, b, Prc.Dis:   IF e THEN 200 ELSE IF b THEN 210
        IF Prc.Dis < "!" THEN WinMssg "Distributor can't be blank": GOTO 210
211     EntryU r, 18, 10, e, b, Prc.Mdl:  IF e THEN 200 ELSE IF b THEN 210
        IF Prc.Mdl < "!" THEN WinMssg "Model Number must not be blank": GOTO 211
212     EntryU r, 29, 4, e, b, Prc.Cod:   IF e THEN 200 ELSE IF b THEN 211
        IF FindCod(Prc.Cod) = 0 THEN WinMssg "Code not found": GOTO 212
        c$ = Prc.Cod
        d$ = Prc.Dis
        m$ = Prc.Mdl
        IF FindPrc(d$ + m$ + c$) THEN WinMssg "Duplicate Code exists": GOTO 210
        Prc.Cod = c$
        Prc.Dis = d$
        Prc.Mdl = m$
        Prc.Old = ""
        Prc.New = ""
        Prc.Dat = ""
        Prc.Par = ""
        Prc.Lab = ""
213     EntryN r, 34, 7, 2, e, b, Prc.Old: IF e THEN 200 ELSE IF b THEN 212
        IF Prc.Old < "   0.01" THEN WinMssg "Cost must not be zero": GOTO 213
214     EntryN r, 42, 7, 2, e, b, Prc.New: IF e THEN 200 ELSE IF b THEN 213
        IF Prc.New < "   0.01" THEN WinMssg "Cost must not be zero": GOTO 214
215     EntryD r, 50, e, b, Prc.Dat:       IF e THEN 200 ELSE IF b THEN 214
        IF Prc.Dat < "!" THEN WinMssg "Must have a date": GOTO 215
216     EntryN r, 61, 3, 0, e, b, Prc.Min: IF e THEN 200 ELSE IF b THEN 215
        IF Prc.Min < "  0" THEN WinMssg "Reorder point please": GOTO 216
217     EntryN r, 65, 3, 0, e, b, Prc.Max: IF e THEN 200 ELSE IF b THEN 216
        IF Prc.Max < "  0" THEN WinMssg "Reorder quantity please": GOTO 217
218     EntryN r, 70, 3, 0, e, b, Prc.Par: IF e THEN 200 ELSE IF b THEN 217
        IF Prc.Par < "  0" THEN WinMssg "Parts warranty can't be zero": GOTO 218
219     EntryN r, 76, 3, 0, e, b, Prc.Lab: IF e THEN 200 ELSE IF b THEN 218
        IF Prc.Lab < "  0" THEN WinMssg "Labor warranty can't be zero": GOTO 219
        IF PassWord(J$) THEN
           Top = 1 + LOF(3) \ LEN(Prc)
           PUT #3, Top, Prc
           CLOSE : SHELL "SortF ..\Master\Prices.Mas ..\Master\Prices /L70 /+5,20 /+1,4 /Q"
           KILL "..\Master\Prices.Mas"
           NAME "..\Master\Prices" AS "..\Master\Prices.Mas"
           OpenFiles
           Bot = n - r + 3
           DispPrc x
           GOTO 200
        ELSE
           GOTO 219
        END IF
    CASE "M"
220     EntryU r, 6, 10, e, b, Prc.Dis:    IF e THEN 200 ELSE IF b THEN 220
        IF Prc.Dis < "!" THEN WinMssg "Distributor can't be blank": GOTO 220
221     EntryU r, 18, 10, e, b, Prc.Mdl:   IF e THEN 200 ELSE IF b THEN 220
        IF Prc.Mdl < "!" THEN WinMssg "Model Number must not be blank": GOTO 221
222     EntryU r, 29, 4, e, b, Prc.Cod:    IF e THEN 200 ELSE IF b THEN 221
        IF FindCod(Prc.Cod) = 0 THEN WinMssg "Code not found": GOTO 222
223     EntryN r, 34, 7, 2, e, b, Prc.Old: IF e THEN 200 ELSE IF b THEN 222
        IF Prc.Old < "   0.01" THEN WinMssg "Cost must not be zero": GOTO 223
224     EntryN r, 42, 7, 2, e, b, Prc.New: IF e THEN 200 ELSE IF b THEN 223
        IF Prc.New < "   0.01" THEN WinMssg "Cost must not be zero": GOTO 224
225     EntryD r, 50, e, b, Prc.Dat:       IF e THEN 200 ELSE IF b THEN 224
        IF Prc.Dat < "!" THEN WinMssg "Must have a date": GOTO 225
226     EntryN r, 61, 3, 0, e, b, Prc.Min: IF e THEN 200 ELSE IF b THEN 225
        IF Prc.Min < "  0" THEN WinMssg "Reorder point please": GOTO 226
227     EntryN r, 65, 3, 0, e, b, Prc.Max: IF e THEN 200 ELSE IF b THEN 226
        IF Prc.Max < "  0" THEN WinMssg "Reorder quantity please": GOTO 227
228     EntryN r, 70, 3, 0, e, b, Prc.Par: IF e THEN 200 ELSE IF b THEN 227
        IF Prc.Par < "  0" THEN WinMssg "Parts warranty can't be zero": GOTO 228
229     EntryN r, 76, 3, 0, e, b, Prc.Lab: IF e THEN 200 ELSE IF b THEN 228
        IF Prc.Lab < "  0" THEN WinMssg "Labor warranty can't be zero": GOTO 228
        IF PassWord(J$) THEN
           PUT #3, n, Prc
           CLOSE : SHELL "SortF ..\Master\Prices.Mas ..\Master\Prices /L70 /+5,20 /+1,4 /Q"
           KILL "..\Master\Prices.Mas"
           NAME "..\Master\Prices" AS "..\Master\Prices.Mas"
           OpenFiles
           Bot = n - r + 3
           DispPrc x
           GOTO 200
        ELSE
           GOTO 229
        END IF
    CASE "E"
        IF PassWord(J$) THEN
           Prc.Cod = "þþþþ"
           Prc.Dis = "þþþþþþþþþþ"
           Prc.Mdl = "þþþþþþþþþþ"
           Prc.Old = "þþþþþþþ"
           Prc.New = "þþþþþþþ"
           Prc.Dat = "þþþþþþþþþþ"
           Prc.Min = "þþþ"
           Prc.Max = "þþþ"
           Prc.Par = "þþþ"
           Prc.Lab = "þþþ"
           Prc.Del = "þ"
           Prc.Fil = "þþþþþþþþþþþþþþþ"
           PUT #3, n, Prc
           CLOSE : SHELL "SortF ..\Master\Prices.Mas ..\Master\Prices /L70 /+5,20 /+1,4 /Q"
           KILL "..\Master\Prices.Mas"
           NAME "..\Master\Prices" AS "..\Master\Prices.Mas"
           OpenFiles
           Bot = n - r + 3
           DispPrc x
           GOTO 200
        END IF
    CASE "P"
        IF Confirm() THEN
           PageNum = 0: HPrices
           FOR x = 1 TO LOF(3) / LEN(Prc)
           GET #3, x, Prc
           IF INKEY$ = CHR$(27) THEN EXIT FOR
           IF LineNum > 61 THEN HPrices
           LineNum = LineNum + 1
           LPRINT USING "&  "; Prc.Dis; Prc.Mdl; Prc.Cod; Prc.Old; Prc.New; Prc.Dat;
           LPRINT USING " &      &"; Prc.Par; Prc.Lab
           NEXT
           LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(3) / LEN(Prc); "PRICES in the master file."
        END IF
    CASE "A"
        WinMssg "Function not implemented yet"
    CASE ELSE
        WinMssg "Select from highlited keys"
    END SELECT
    GOTO 200
Rates: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    OpenFiles
    CALL MhDisplayByName(0, 0, "Rates", e)
    
    n = 1: r = 5: b = 1: t = LOF(4) \ LEN(Rat): GOSUB DispRat
    COLOR 16, 3: LOCATE 48, 75: PRINT t;

400 GET #4, n, Rat
    COLOR 0, 7: LOCATE r, 2: PRINT USING "&  &"; Rat.Del; Rat.Num;
                LOCATE , 9:  PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(1); Rat.Mrt(1); Rat.Wtr(1); Rat.Wrt(1);
                LOCATE , 33: PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(2); Rat.Mrt(2); Rat.Wtr(2); Rat.Wrt(2);
                LOCATE , 57: PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(3); Rat.Mrt(3); Rat.Wtr(3); Rat.Wrt(3);
    KeyHit n$
    
    SELECT CASE n$
    CASE CHR$(27) 'Esc
         GOTO 10
    CASE CHR$(0) + "M"  '===============Right==
         
    CASE CHR$(0) + "K"  '===============Left===
         
    CASE CHR$(0) + "P"  '===============Down===
         IF n + 1 > t THEN 400 ELSE n = n + 1
         COLOR 16, 2: LOCATE , 2:  PRINT USING "&  &"; Rat.Del; Rat.Num;
         COLOR 16, 7: LOCATE , 9:  PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(1); Rat.Mrt(1); Rat.Wtr(1); Rat.Wrt(1);
                      LOCATE , 33: PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(2); Rat.Mrt(2); Rat.Wtr(2); Rat.Wrt(2);
                      LOCATE , 57: PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(3); Rat.Mrt(3); Rat.Wtr(3); Rat.Wrt(3);
         IF r + 1 > 47 THEN CALL Scroll(1, 5, 79, 47, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"  '===============Up=====
         IF n - 1 < 1 THEN 400 ELSE n = n - 1
         COLOR 16, 2: LOCATE , 2:  PRINT USING "&  &"; Rat.Del; Rat.Num;
         COLOR 16, 7: LOCATE , 9:  PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(1); Rat.Mrt(1); Rat.Wtr(1); Rat.Wrt(1);
                      LOCATE , 33: PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(2); Rat.Mrt(2); Rat.Wtr(2); Rat.Wrt(2);
                      LOCATE , 57: PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(3); Rat.Mrt(3); Rat.Wtr(3); Rat.Wrt(3);
         IF r - 1 < 5 THEN CALL BkScroll(1, 5, 79, 47, 1) ELSE r = r - 1
    CASE CHR$(0) + "I"  '===============PgUp===
         b = n - r - 38: IF b < 1 THEN b = 1
         GOSUB DispRat: n = x - 47 + r
    CASE CHR$(0) + "Q"  '===============PgDn===
         b = n - r + 48: IF b > t - 42 THEN b = t - 42
         GOSUB DispRat: n = x + r - 47
    CASE CHR$(0) + "G"  '===============Home===
         b = 1: GOSUB DispRat: n = r - 4
    CASE CHR$(0) + "O"  '===============End====
         b = t - 42: GOSUB DispRat: n = t + r - 47
    CASE "N"
         s = 1: GOSUB EditRat
    CASE "M"
         s = 0: GOSUB EditRat
    CASE "E"
         IF PassWord(J$) THEN
            n$ = Rat.Num
            GET #4, t + 1, Rat
            Rat.Num = n$
            Rat.Mtr(1) = "þþþ"
            PUT #4, n, Rat
            SortRat
            b = n - r + 5
            GOSUB DispRat
         END IF
    CASE "P"
         PageNum = 0: HRates
         FOR x = 1 TO LOF(4) / LEN(Rat)
            GET #4, x, Rat
            IF INKEY$ = CHR$(27) THEN EXIT FOR
            LPRINT USING "&  &"; Rat.Del; Rat.Num;
            FOR z = 1 TO 3
                LPRINT USING " & &  & &"; Rat.Mtr(z); Rat.Mrt(z); Rat.Wtr(z); Rat.Wrt(z);
            NEXT
            LPRINT
            LineNum = LineNum + 1: IF LineNum > 60 THEN HRates
         NEXT
         LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(4) / LEN(Rat); "RATES in the master file."
    END SELECT
    GOTO 400

EditRat:
    IF s THEN
       m = 1 + LOF(4) \ LEN(Rat)
       GET #4, m, Rat
       Rat.Num = RIGHT$(STR$(1000 + m), 3)
       LOCATE r, 5
       PRINT Rat.Num + "     ณ      ณ    ณ           ณ      ณ    ณ           ณ      ณ    ณ       ";
    ELSE
       m = n
    END IF
    z = 1
412 EntryN r, z * 24 - 14, 3, 0, e, b, Rat.Mtr(z): IF e THEN 400
    IF b THEN IF z > 1 THEN z = z - 1: GOTO 416 ELSE 412
414 EntryN r, z * 24 - 10, 6, 2, e, b, Rat.Mrt(z): IF e THEN 400 ELSE IF b THEN 412
    LOCATE r, z * 24 - 2
    IF Rat.Mtr(z) > "  0" THEN
       PRINT USING "###"; 4.348 * VAL(Rat.Mtr(z));
    ELSE
       PRINT "   ";
    END IF
    Picture r, z * 24 - 2, 3, Rat.Wtr(z)
416 EntryN r, z * 24 + 2, 6, 2, e, b, Rat.Wrt(z): IF e THEN 400 ELSE IF b THEN 414
    z = z + 1: IF z > 3 THEN z = 3 ELSE 412
    IF PassWord(J$) = 0 THEN 416
    PUT #4, m, Rat
    SortRat
    t = LOF(4) \ LEN(Rat)
    COLOR 16, 3: LOCATE 48, 75: PRINT t;
    b = n - r + 5
DispRat:
    LOCATE 5
    FOR x = b TO t
        GET #4, x, Rat
        COLOR 16, 2: LOCATE , 2:  PRINT USING "&  &"; Rat.Del; Rat.Num;
        COLOR 16, 7: LOCATE , 9:  PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(1); Rat.Mrt(1); Rat.Wtr(1); Rat.Wrt(1);
                   : LOCATE , 33: PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(2); Rat.Mrt(2); Rat.Wtr(2); Rat.Wrt(2);
                   : LOCATE , 57: PRINT USING " &ณ&ณ &ณ&"; Rat.Mtr(3); Rat.Mrt(3); Rat.Wtr(3); Rat.Wrt(3)
        IF CSRLIN = 48 THEN EXIT FOR
    NEXT
    RETURN
Words: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    OpenFiles
    CALL MhDisplayByName(0, 0, "Password", e)
    Bot = 1: Top = LOF(8) \ LEN(Pas): GOSUB DispPas
    r = 2: n = 1
700 IF n > 0 THEN GET #8, n, Pas
    COLOR 11, 1: LOCATE r, 10: PRINT USING "    &   ณ   &   ณ & ณ         &        "; Pas.Ini; Pas.Pas; Pas.Nam; Pas.Lvl
    KeyHit n$
    COLOR 1, 3:  LOCATE r, 10: PRINT USING "    &   ณ   &   ณ & ณ         &        "; Pas.Ini; Pas.Pas; Pas.Nam; Pas.Lvl
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        GOTO 10
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > Top THEN 700 ELSE n = n + 1
        IF r + 1 > 48 THEN CALL Scroll(10, 2, 71, 48, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 THEN 700 ELSE n = n - 1
        IF r - 1 < 2 THEN CALL BkScroll(10, 2, 71, 48, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 45: IF Bot < 1 THEN Bot = 1
        GOSUB DispPas
        n = x - 48 + r
    CASE CHR$(0) + "Q" 'PgDn
        Bot = 49 - r + n
        IF Bot > Top - 46 THEN Bot = Top - 46: n = Top + r - 48 ELSE n = n + 47
        GOSUB DispPas
    CASE CHR$(0) + "G" 'Home
        Bot = 1: GOSUB DispPas
        n = r - 1
    CASE CHR$(0) + "O" 'End
        Bot = Top - 46: GOSUB DispPas
        n = Top + r - 48
    CASE "N"
        s = 1: GOSUB EditPas
    CASE "M"
        s = 0: GOSUB EditPas
    CASE "P"
        IF Confirm() THEN
           PageNum = 0: HWords
           FOR x = 1 TO LOF(8) \ LEN(Pas)
           GET #8, x, Pas
           IF INKEY$ = CHR$(27) THEN EXIT FOR
           IF LineNum > 60 THEN HWords
           LineNum = LineNum + 1
           LPRINT USING "  &   &    &       &"; Pas.Ini; Pas.Pas; Pas.Nam; Pas.Lvl
           NEXT
           LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(8) / LEN(Pas); "PASSWORDS in the master file."
        END IF
    CASE "E"
        IF PassWord(J$) THEN
           Pas.Pas = "þþþþ"
           Pas.Nam = "þþþþþþþþþþþþþþþþþþþ"
           Pas.Ini = "þþþ"
           Pas.Lvl = "þ"
           PUT #8, n, Pas
           SortPas
           OpenFiles
           Bot = n - r + 2
           GOSUB DispPas
        END IF
    END SELECT
    GOTO 700
EditPas:
    IF s THEN m = 1 + Top ELSE m = n
    GET #8, m, Pas
    i$ = Pas.Ini: a$ = i$
    IF Pas.Pas < "    " THEN
       p$ = "    "
    ELSE
       p$ = CHR$(ASC(MID$(Pas.Pas, 1)) - 34) + CHR$(ASC(MID$(Pas.Pas, 2)) - 70) + CHR$(ASC(MID$(Pas.Pas, 3)) - 81) + CHR$(ASC(MID$(Pas.Pas, 4)) - 63)
    END IF
    b$ = p$
    n$ = Pas.Nam
    l$ = Pas.Lvl
    IF s THEN
       LOCATE r, 12: PRINT "        ณ          ณ                     ณ            ";
710    EntryU r, 14, 3, e, b, i$: IF e THEN 700 ELSE IF b THEN 710
       IF LEN(RTRIM$(LTRIM$(i$))) < 3 THEN WinMssg "Initials must be full": GOTO 710
       IF a$ <> i$ THEN IF FindIni(i$) THEN WinMssg "Duplicate initials": GOTO 710
    END IF
711 EntryS r, 24, 4, e, b, p$: IF e THEN 700 ELSE IF b THEN IF s THEN 710 ELSE 711
    IF p$ < "!" THEN WinMssg "Password must not be blank": GOTO 711
    IF b$ <> p$ THEN
       o$ = CHR$(ASC(MID$(p$, 1, 1)) + 34) + CHR$(ASC(MID$(p$, 2, 1)) + 70) + CHR$(ASC(MID$(p$, 3, 1)) + 81) + CHR$(ASC(MID$(p$, 4, 1)) + 63)
       IF FindPas(o$) THEN WinMssg "Duplicate Password": GOTO 711
    END IF
712 EntryS r, 33, 19, e, b, n$: IF e THEN 700 ELSE IF b THEN 711
    IF n$ < "!" THEN WinMssg "Employee must not be blank": GOTO 712
713 EntryN r, 63, 1, 0, e, b, l$: IF e THEN 700 ELSE IF b THEN 712
    IF l$ < "0" THEN WinMssg "Security level please": GOTO 713
    IF PassWord(J$) = 0 THEN 713
    Pas.Ini = i$
    Pas.Pas = CHR$(ASC(MID$(p$, 1, 1)) + 34) + CHR$(ASC(MID$(p$, 2, 1)) + 70) + CHR$(ASC(MID$(p$, 3, 1)) + 81) + CHR$(ASC(MID$(p$, 4, 1)) + 63)
    Pas.Nam = n$
    Pas.Lvl = l$
    PUT #8, m, Pas
    SortPas
    Top = LOF(8) \ LEN(Pas)
    b = n - r + 4
DispPas:
    IF Bot > 0 AND Top > 0 THEN
       LOCATE 2: COLOR 1, 3
       FOR x = Bot TO Top
       GET #8, x, Pas
       LOCATE , 10: PRINT USING "    &   ณ   &   ณ & ณ         &        "; Pas.Ini; Pas.Pas; Pas.Nam; Pas.Lvl
       IF CSRLIN = 49 THEN EXIT FOR
       NEXT
    ELSE Bot = 1
    END IF
    RETURN
Store: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    OpenFiles
    CALL MhDisplayByName(0, 0, "Stores", e)
    Bot = 1: Top = LOF(5) / LEN(Sto): DispSto x
    r = 2: n = 1
500 IF n > 0 THEN GET #5, n, Sto
    COLOR 11, 1: LOCATE r, 3: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
    KeyHit n$
    COLOR 1, 3:  LOCATE r, 3: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        GOTO 10
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > Top THEN 500 ELSE n = n + 1
        IF r + 1 > 23 THEN CALL Scroll(3, 2, 31, 23, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 THEN 500 ELSE n = n - 1
        IF r - 1 < 2 THEN CALL BkScroll(3, 2, 31, 23, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 20: IF Bot < 1 THEN Bot = 1
        DispSto x
        n = x - 23 + r
    CASE CHR$(0) + "Q" 'PgDn
        Bot = 24 - r + n
        IF Bot > Top - 21 THEN Bot = Top - 21: n = Top + r - 23 ELSE n = n + 22
        DispSto x
    CASE CHR$(0) + "G" 'Home
        Bot = 1: DispSto x: n = r - 1
    CASE CHR$(0) + "O" 'End
        Bot = Top - 21: DispSto x: n = Top + r - 23
    CASE "F"
        COLOR 0, 2: LOCATE 13
        LOCATE , 8: PRINT "ษอออออออออออออออออป"
        LOCATE , 8: PRINT "บ Find Store:     บ"
        LOCATE , 8: PRINT "ศอออออออออออออออออผ"
        COLOR 10, 3: LOCATE 14
        LOCATE , 7: PRINT "Û"
        LOCATE , 7: PRINT "Û"
        LOCATE , 7: PRINT "฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"
505     EntryN 14, 22, 2, 0, e, b, Sto.Num: IF e THEN 506 ELSE IF b THEN 505
        x = VAL(Sto.Num): IF x = 0 THEN BEEP: GOTO 505 ELSE GET #5, x, Sto
        Bot = x - r + 2
        IF Bot > Top - 21 THEN Bot = Top - 21: r = 23 - Top + x ELSE IF Bot < 1 THEN Bot = 1: r = x + 1
        n = Bot + r - 2
506     ShowSto
        DispSto x
    CASE "D"
        ShowSto
    CASE "M"
        ShowSto
509     EntryS 3, 52, 1, e, b, Sto.Div:     IF e THEN 500 ELSE IF b THEN 509
510     EntryU 4, 52, 3, e, b, Sto.Cod:     IF e THEN 500 ELSE IF b THEN 509
511     EntryS 6, 52, 26, e, b, Sto.Nam:    IF e THEN 500 ELSE IF b THEN 510
512     EntryS 7, 52, 26, e, b, Sto.Add:    IF e THEN 500 ELSE IF b THEN 511
513     EntryS 8, 52, 26, e, b, Sto.Str:    IF e THEN 500 ELSE IF b THEN 512
514     EntryS 9, 52, 17, e, b, Sto.Cty:    IF e THEN 500 ELSE IF b THEN 513
515     EntryU 9, 70, 2, e, b, Sto.Sta:     IF e THEN 500 ELSE IF b THEN 514
516     EntryU 9, 73, 5, e, b, Sto.Zip:     IF e THEN 500 ELSE IF b THEN 515
517     EntryS 11, 52, 16, e, b, Sto.Man:   IF e THEN 500 ELSE IF b THEN 516
518     EntryS 12, 52, 16, e, b, Sto.Acc:   IF e THEN 500 ELSE IF b THEN 517
519     EntryS 13, 52, 16, e, b, Sto.Acm:   IF e THEN 500 ELSE IF b THEN 518
520     EntryS 14, 52, 16, e, b, Sto.Sec:   IF e THEN 500 ELSE IF b THEN 519
521     EntryT 16, 52, e, b, Sto.Tel:       IF e THEN 500 ELSE IF b THEN 520
522     EntryT 17, 52, e, b, Sto.Pho:       IF e THEN 500 ELSE IF b THEN 521
523     EntryD 19, 52, e, b, Sto.Opn:       IF e THEN 500 ELSE IF b THEN 522
524     EntryN 21, 52, 5, 2, e, b, Sto.Stx: IF e THEN 500 ELSE IF b THEN 523
        IF PassWord(J$) THEN PUT #5, n, Sto ELSE 524
    CASE "P"
        IF Confirm() THEN
           PageNum = 0: HStores
           FOR x = 1 TO LOF(5) / LEN(Sto)
           GET #5, x, Sto
           IF INKEY$ = CHR$(27) THEN EXIT FOR
           IF Sto.Nam > "!" THEN
              LineNum = LineNum + 5: IF LineNum > 62 THEN HStores
              LPRINT USING " &  &  &     Store Manager: &   Voice Phone: &   Opening Date: &"; Sto.Div; Sto.Num; Sto.Nam; Sto.Man; Sto.Tel; Sto.Opn
              LPRINT USING "        &   Account Manager: &    Data Phone: &      Sales Tax: & %"; Sto.Add; Sto.Acc; Sto.Pho; Sto.Stx
              LPRINT USING "        &   Account Manager: &"; Sto.Str; Sto.Acm
              LPRINT USING "        & & &         Secretary: &"; Sto.Cty; Sto.Sta; Sto.Zip; Sto.Sec
              LPRINT
           END IF
           NEXT
           LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(5) / LEN(Sto); "STORES in the master file."
        END IF
    CASE "A"
        WinMssg "Function not implemented yet"
    CASE ELSE
        WinMssg "Select from highlited keys"
    END SELECT
    GOTO 500
Center: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    OpenFiles
    CALL MhDisplayByName(0, 0, "Centers", e)
    Bot = 1: Top = LOF(6) / LEN(Ctr): DispCtr x
    r = 2: n = 1
600 IF n > 0 THEN GET #6, n, Ctr
    COLOR 11, 1: LOCATE r, 3: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam
    KeyHit n$
    COLOR 1, 3:  LOCATE r, 3: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        GOTO 10
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > Top THEN 600 ELSE n = n + 1
        IF r + 1 > 23 THEN CALL Scroll(3, 2, 31, 23, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 THEN 600 ELSE n = n - 1
        IF r - 1 < 2 THEN CALL BkScroll(3, 2, 31, 23, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 20: IF Bot < 1 THEN Bot = 1
        DispCtr x
        n = x - 23 + r
    CASE CHR$(0) + "Q" 'PgDn
        Bot = 24 - r + n
        IF Bot > Top - 21 THEN Bot = Top - 21: n = Top + r - 23 ELSE n = n + 22
        DispCtr x
    CASE CHR$(0) + "G" 'Home
        Bot = 1: DispCtr x: n = r - 1
    CASE CHR$(0) + "O" 'End
        Bot = Top - 21: DispCtr x: n = Top + r - 23
    CASE "F"
        COLOR 0, 2: LOCATE 13
        LOCATE , 7: PRINT " ษอออออออออออออออออป "
        LOCATE , 7: PRINT " บ Find Center:    บ "
        LOCATE , 7: PRINT " ศอออออออออออออออออผ "
        COLOR 10, 3: LOCATE 14
        LOCATE , 6: PRINT "Û"
        LOCATE , 6: PRINT "Û"
        LOCATE , 6: PRINT "฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"
605     EntryN 14, 23, 2, 0, e, b, Ctr.Num: IF e THEN 606 ELSE IF b THEN 605
        x = VAL(Ctr.Num): IF x = 0 THEN 606
        GET #6, x, Ctr
        Bot = x - r + 2
        IF Bot > Top - 21 THEN Bot = Top - 21: r = 23 - Top + x ELSE IF Bot < 1 THEN Bot = 1: r = x + 1
        n = Bot + r - 2
        ShowCtr
606     DispCtr x
    CASE "D"
        ShowCtr
    CASE "M"
        ShowCtr
610     EntryS 3, 52, 26, e, b, Ctr.Nam:   IF e THEN 600 ELSE IF b THEN 610
611     EntryS 4, 52, 26, e, b, Ctr.Str:   IF e THEN 600 ELSE IF b THEN 610
612     EntryS 5, 52, 17, e, b, Ctr.Cty:   IF e THEN 600 ELSE IF b THEN 611
613     EntryU 5, 70, 2, e, b, Ctr.Sta:    IF e THEN 600 ELSE IF b THEN 612
614     EntryU 5, 73, 5, e, b, Ctr.Zip:    IF e THEN 600 ELSE IF b THEN 613
615     EntryT 6, 52, e, b, Ctr.Tel:       IF e THEN 600 ELSE IF b THEN 615
616     EntryN 8, 52, 2, 0, e, b, Ctr.Trm: IF e THEN 600 ELSE IF b THEN 615
617     EntryU 10, 52, 1, e, b, Ctr.Typ:   IF e THEN 600 ELSE IF b THEN 616
        IF INSTR("WBF", Ctr.Typ) = 0 THEN WinMssg "Must pick W, B or F": GOTO 617
        IF PassWord(J$) THEN PUT #6, n, Ctr ELSE 617
    CASE "P"
        IF Confirm() THEN
           PageNum = 0: HCenter: L0 = 0: L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0: L6 = 0
           FOR x = 1 TO LOF(6) / LEN(Ctr)
           GET #6, x, Ctr
           IF INKEY$ = CHR$(27) THEN EXIT FOR
           IF Ctr.Nam > "!" THEN
               IF LineNum > 62 THEN HCenter
               LineNum = LineNum + 1: L0 = L0 + 1
               LPRINT USING "& "; Ctr.Num; Ctr.Nam; Ctr.Str; Ctr.Cty; Ctr.Sta;
               LPRINT Ctr.Zip;
               LPRINT USING "#######.##"; Ctr.Mbb / 100; Ctr.Mam / 100; Ctr.Mpd / 100; (Ctr.Mbb + Ctr.Mam - Ctr.Mpd) / 100;
               LPRINT USING "#######.##"; Ctr.Ybb / 100; Ctr.Yam / 100; Ctr.Ypd / 100; (Ctr.Ybb + Ctr.Yam - Ctr.Ypd) / 100
               L1 = L1 + Ctr.Mbb: L2 = L2 + Ctr.Mam: L3 = L3 + Ctr.Mpd
               L4 = L4 + Ctr.Ybb: L5 = L5 + Ctr.Yam: L6 = L6 + Ctr.Ypd
           END IF
           NEXT
           LPRINT : LPRINT "Printed"; L0; "out of"; LOF(6) / LEN(Ctr); "CENTERS in the master file."; TAB(84);
           LPRINT USING "#######.##"; L1 / 100; L2 / 100; L3 / 100; (L1 + L2 - L3) / 100;
           LPRINT USING "#######.##"; L4 / 100; L5 / 100; L6 / 100; (L4 + L5 - L6) / 100
        END IF
    CASE "A"
        WinMssg "Function not implemented yet"
    CASE ELSE
        WinMssg "Select from highlited keys"
    END SELECT
    GOTO 600
Check: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    CALL MhDisplayByName(0, 0, "Check", e)
    IF PassWord(J$) THEN
       COLOR 1, 7
       OpenFiles
       OPEN "\Acctg\IN\Inventor.Mas" FOR RANDOM AS #11 LEN = LEN(Oin)

       FOR x = 1 TO LOF(1) \ LEN(Cod)
       GET #1, x, Cod: Cod.Del = "?": PUT #1, x, Cod
       LOCATE 36, 57: PRINT USING "#####"; x;
       NEXT
       FOR x = 1 TO LOF(3) \ LEN(Prc)
       GET #3, x, Prc: Prc.Del = "?": PUT #3, x, Prc
       LOCATE 40, 57: PRINT USING "#####"; x;
       NEXT

       FOR x = 1 TO LOF(11) \ LEN(Oin)
       GET #11, x, Oin
       m = FindCod(Oin.Cod)
       IF m THEN Cod.Del = " ": PUT #1, m, Cod
       LOCATE 36, 57: PRINT USING "#####"; x;
       m = FindPrc(Oin.Dis + LEFT$(Oin.Mdl, 10) + Oin.Cod)
       IF m THEN Prc.Del = " ": PUT #3, m, Prc
       LOCATE 40, 57: PRINT USING "#####"; x;
       NEXT
       
       FOR x = 1 TO LOF(4) \ LEN(Rat)
       GET #4, x, Rat: Rat.Del = "?": PUT #4, x, Rat
       LOCATE 44, 57: PRINT USING "#####"; x;
       NEXT
       FOR x = 1 TO LOF(1) \ LEN(Cod)
           GET #1, x, Cod
           IF VAL(Cod.Rat) > 0 THEN
              GET #4, VAL(Cod.Rat), Rat
              Rat.Del = " "
              PUT #4, VAL(Cod.Rat), Rat
           END IF
           LOCATE 44, 57: PRINT USING "#####"; x;
       NEXT
       FOR x = 1 TO LOF(2) \ LEN(Cmb)
           GET #2, x, Cmb
           IF VAL(Cmb.Rat) > 0 THEN
              GET #4, VAL(Cmb.Rat), Rat
              Rat.Del = " "
              PUT #4, VAL(Cmb.Rat), Rat
           END IF
           LOCATE 44, 57: PRINT USING "#####"; x;
       NEXT
       CLOSE
       
    END IF
    GOTO 10
Compress: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    CALL MhDisplayByName(0, 0, "Compress", e)
    IF PassWord(J$) THEN
       COLOR 9, 7
       OPEN "..\Master\Codes   .Mas" FOR RANDOM AS #1 LEN = LEN(Cod)
       OPEN "..\Master\Codes   .New" FOR RANDOM AS #2 LEN = LEN(Cod)
       t = LOF(1) \ LEN(Cod)
       LOCATE 36, 48: PRINT USING "#####"; t;
       FOR x = 1 TO t
       GET #1, x, Cod
       IF Cod.Cod = "þþþþ" THEN EXIT FOR
       PUT #2, x, Cod
       LOCATE 36, 58: PRINT USING "#####"; x;
       NEXT
       CLOSE
       KILL "..\Master\Codes.Mas"
       KILL "..\Master\Codes.Cmb"
       SHELL "SortF ..\Master\Codes.New ..\Master\Codes.Mas /L50 /+1,4 /Q"
       SHELL "SortF ..\Master\Codes.New ..\Master\Codes.Cmb /L50 /+36,2 /+1,4 /Q"
       KILL "..\Master\Codes.New"
       
       OPEN "..\Master\Combos  .Mas" FOR RANDOM AS #1 LEN = LEN(Cmb)
       OPEN "..\Master\Combos  .New" FOR RANDOM AS #2 LEN = LEN(Cmb)
       t = LOF(1) \ LEN(Cmb)
       LOCATE 40, 48: PRINT USING "#####"; t;
       FOR x = 1 TO t
       GET #1, x, Cmb
       IF Cmb.Cod = "þþþþ" THEN EXIT FOR
       PUT #2, x, Cmb
       LOCATE 40, 58: PRINT USING "#####"; x;
       NEXT
       CLOSE
       KILL "..\Master\Combos.Mas"
       SHELL "SortF ..\Master\Combos.New ..\Master\Combos.Mas /L49 /+1,4 /Q"
       KILL "..\Master\Combos.New"

       OPEN "..\Master\Prices  .Mas" FOR RANDOM AS #1 LEN = LEN(Prc)
       OPEN "..\Master\Prices  .New" FOR RANDOM AS #2 LEN = LEN(Prc)
       t = LOF(1) \ LEN(Prc)
       LOCATE 44, 48: PRINT USING "#####"; t;
       FOR x = 1 TO t
       GET #1, x, Prc
       IF Prc.Cod = "þþþþ" THEN EXIT FOR
       PUT #2, x, Prc
       LOCATE 44, 58: PRINT USING "#####"; x;
       NEXT
       CLOSE
       KILL "..\Master\Prices.Mas"
       SHELL "SortF ..\Master\Prices.New ..\Master\Prices.Mas /L70 /+5,20 /+1,4 /Q"
       KILL "..\Master\Prices.New"

       OPEN "..\Master\Password.MAS" FOR RANDOM AS #1 LEN = LEN(Pas)
       OPEN "..\Master\Password.New" FOR RANDOM AS #2 LEN = LEN(Pas)
       t = LOF(1) \ LEN(Pas)
       LOCATE 48, 48: PRINT USING "#####"; t;
       FOR x = 1 TO t
       GET #1, x, Pas
       IF Pas.Pas = "þþþþ" THEN EXIT FOR
       PUT #2, x, Pas
       LOCATE 48, 58: PRINT USING "#####"; x;
       NEXT
       CLOSE
       KILL "..\Master\Password.MAS"
       KILL "..\Master\Password.NAM"
       SHELL "SortF ..\Master\Password.New ..\Master\Password.Mas /L27 /+1,4 /Q"
       SHELL "SortF ..\Master\Password.Mas ..\Master\Password.Nam /L27 /+5,19 /C /Q"
       KILL "..\Master\Password.New"
       WinMssg "Press any key to continue"
    END IF
    GOTO 10
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    SELECT CASE ERR
    CASE 70
       WinMssg "Master Files in use": GOTO 10
    CASE ELSE
       WinMssg "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
    END SELECT
    GOTO 10
Terminate: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    CLOSE
    END

DEFINT L
FUNCTION Confirm ()
    PCOPY 0, 2
    COLOR 14, 4: LOCATE 12
    LOCATE , 36: PRINT "                           "
    LOCATE , 36: PRINT " PRESS * to start printing "
    LOCATE , 36: PRINT "                           "
    COLOR 12, 3: LOCATE 13
    LOCATE , 35: PRINT "Û"
    LOCATE , 35: PRINT "Û"
    LOCATE , 35: PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
    SOUND 880, 1: SOUND 770, 2: KeyHit f$
    IF f$ = "*" THEN Confirm = 1
    PCOPY 2, 0
END FUNCTION

SUB DispCmb (x)
    IF Bot > 0 AND Top > 0 THEN
       LOCATE 2: COLOR 1, 3
       FOR x = Bot TO Top
       GET #2, x, Cmb
       LOCATE , 3: PRINT USING "  & ณ &  ณ   &   ณ  &  ณ  & ณ & ณ & ณ & ณ & "; Cmb.Cod; Cmb.Des; Cmb.Rat; Cmb.Dtr; Cmb.Dfm; Cmb.Cde(1); Cmb.Cde(2); Cmb.Cde(3); Cmb.Cde(4)
       IF CSRLIN = 49 THEN EXIT FOR
       NEXT
    ELSE Bot = 1
    END IF
END SUB

SUB DispCod (x)
    IF Bot > 0 AND Top > 0 THEN
       LOCATE 2: COLOR 1, 3
       FOR x = Bot TO Top
       GET #1, x, Cod
       LOCATE , 2: PRINT USING "  &   ณ&ณ &  ณ &  ณ &  ณ&ณ &ณ&ณ & ณ  &  ณ &  "; Cod.Del; Cod.Cod; Cod.Typ; Cod.Grp; Cod.Cmb; Cod.Des; Cod.Rat; Cod.Fmv; Cod.Dep; Cod.Dtr; Cod.Dfm
       IF CSRLIN = 49 THEN EXIT FOR
       NEXT
    ELSE Bot = 1
    END IF
END SUB

SUB DispCtr (x)
    IF Bot > 0 AND Top > 0 THEN
       LOCATE 2: COLOR 1, 3
       FOR x = Bot TO Top
       GET #6, x, Ctr
       LOCATE , 3: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam
       IF CSRLIN = 24 THEN EXIT FOR
       NEXT
    ELSE Bot = 1
    END IF
END SUB

SUB DispPrc (x)
    IF Bot > 0 AND Top > 0 THEN
       LOCATE 3: COLOR 1, 3
       FOR x = Bot TO Top
       GET #3, x, Prc
       LOCATE , 2: PRINT USING " & ณ& ณ&ณ&ณ&ณ&ณ&ณ&ณ&ณ & ณ & "; Prc.Del; Prc.Dis; Prc.Mdl; Prc.Cod; Prc.Old; Prc.New; Prc.Dat; Prc.Min; Prc.Max; Prc.Par; Prc.Lab
       IF CSRLIN = 49 THEN EXIT FOR
       NEXT
    ELSE Bot = 1
    END IF
END SUB

SUB DispSto (x)
    IF Bot > 0 AND Top > 0 THEN
       LOCATE 2: COLOR 1, 3
       FOR x = Bot TO Top
       GET #5, x, Sto
       LOCATE , 3: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
       IF CSRLIN = 24 THEN EXIT FOR
       NEXT
    ELSE Bot = 1
    END IF
END SUB

FUNCTION FindCmb (Comb$)
   b = 1: t = LOF(2) \ LEN(Cmb)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #2, m, Cmb
      IF Comb$ = Cmb.Cod THEN FindCmb = m: EXIT DO
      IF Comb$ > Cmb.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindCod (Code$)
   b = 1: t = LOF(1) \ LEN(Cod)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #1, m, Cod
      IF Code$ = Cod.Cod THEN FindCod = m: EXIT DO
      IF Code$ > Cod.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindCom (Code$)
   b = 1: t = LOF(7) \ LEN(Cod)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #7, m, Cod
      IF Code$ = Cod.Cmb THEN FindCom = m: EXIT DO
      IF Code$ > Cod.Cmb THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindIni (f$)
   b = 1: t = LOF(9) \ LEN(Pas)
   FOR x = 1 TO t
      GET #9, x, Pas
      IF f$ = Pas.Ini THEN FindIni = x: EXIT FOR
   NEXT
END FUNCTION

FUNCTION FindPas (f$)
   b = 1: t = LOF(9) \ LEN(Pas)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #9, m, Pas
      IF f$ = Pas.Pas THEN FindPas = m: EXIT DO
      IF f$ > Pas.Pas THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindPrc (f$)
   b = 1: t = LOF(3) \ LEN(Prc)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #3, m, Prc
      IF f$ = Prc.Dis + Prc.Mdl + Prc.Cod THEN FindPrc = m: EXIT DO
      IF f$ > Prc.Dis + Prc.Mdl + Prc.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

DEFSNG A-Z
SUB HCenter ()
    Heading
    LPRINT TAB(62); "SERVICE  CENTERS": LPRINT
    LPRINT CHR$(28);
    LPRINT "                                                                                    ============== M O N T H ============== =============== Y E A R ==============="
    LPRINT "Nง       Service Center              Street Address            City        ST  Zip   Forward   Charges  Payments   Balance   Forward   Charges  Payments   Balance "
    LPRINT "== ========================== ========================== ================= == ===== ========= ========= ========= ========= ========= ========= ========= ========="
END SUB

SUB HCodes ()
    Heading
    LPRINT TAB(64); "CODES MASTER": LPRINT
    LPRINT CHR$(28);
    LPRINT "                                                        Drop Ratio        =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY=="
    LPRINT "Code Type Group       Description        Combo FMV Life Term  Fmv   Rate  Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate"
    LPRINT "==== ==== ===== ======================== ===== === ==== ==========  ====  ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

SUB HCombo ()
    Heading
    LPRINT TAB(64); "COMBO MASTER": LPRINT
    LPRINT CHR$(28);
    LPRINT "                                                                     =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY=="
    LPRINT "Code         Description          Code   Code   Code   Code   Rate   Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate"
    LPRINT "====   ========================   ====   ====   ====   ====   ====   ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

SUB Heading ()
    LPRINT CHR$(12); CHR$(27); CHR$(24);
    WIDTH LPRINT 255: LPRINT
    PageNum = PageNum + 1: LineNum = 8
    LPRINT DATE$; TAB(60); "F. O. G. ENTERPRISES"; TAB(125); "Page:"; PageNum
    LPRINT TIME$; TAB(60); "Harriman . Tennessee": LPRINT
END SUB

SUB HitKey (f$)
    DO: f$ = INKEY$: LOOP WHILE f$ <> ""
    DO: f$ = INKEY$: TimeDate: LOOP WHILE f$ = ""
END SUB

SUB HPrices ()
    Heading
    LPRINT TAB(64); "PRICE MASTER": LPRINT
    LPRINT "                              =====PRICES=====              ===WARRANTY==="
    LPRINT "Code  Distributor  Model Nง     Old      New       Date     Parts    Labor"
    LPRINT "====  =========== ==========  =======  =======  ==========  =============="
END SUB

SUB HRates ()
    Heading
    LPRINT TAB(64); "RATES MASTER": LPRINT
    LPRINT CHR$(28);
    LPRINT "Rate   =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY=="
    LPRINT "Code   Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate"
    LPRINT "=====  ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

SUB HStores ()
    Heading
    LPRINT TAB(64); "STORE MASTER": LPRINT
    LPRINT "Div Num           Store                         Personnel                      Telephone Number              Other Information    "
    LPRINT "=== === ===========================  =================================   =============================   ========================="
END SUB

SUB HWords ()
    Heading
    LPRINT TAB(62); "PASSWRD SECURITY": LPRINT : LPRINT
    LPRINT "Password     Employee Name     Initials    Security Level  "
    LPRINT "========  ===================  ========  =================="
END SUB

SUB KeyHit (f$)
    DO: f$ = INKEY$: LOOP WHILE f$ <> ""
    DO: f$ = INKEY$: LOOP WHILE f$ = "": f$ = UCASE$(f$)
END SUB

SUB OpenFiles ()
    CLOSE
    OPEN "..\Master\Codes   .Mas" FOR RANDOM LOCK WRITE AS #1 LEN = LEN(Cod)
    OPEN "..\Master\Combos  .Mas" FOR RANDOM LOCK WRITE AS #2 LEN = LEN(Cmb)
    OPEN "..\Master\Prices  .Mas" FOR RANDOM LOCK WRITE AS #3 LEN = LEN(Prc)
    OPEN "..\Master\Rates   .Inx" FOR RANDOM LOCK WRITE AS #4 LEN = LEN(Rat)
    OPEN "..\Master\Stores  .Mas" FOR RANDOM LOCK WRITE AS #5 LEN = LEN(Sto)
    OPEN "..\Master\Centers .Mas" FOR RANDOM LOCK WRITE AS #6 LEN = LEN(Ctr)
    OPEN "..\Master\Codes   .Cmb" FOR RANDOM LOCK WRITE AS #7 LEN = LEN(Cod)
    OPEN "..\Master\Password.Nam" FOR RANDOM LOCK WRITE AS #8 LEN = LEN(Pas)
    OPEN "..\Master\Password.Mas" FOR RANDOM LOCK WRITE AS #9 LEN = LEN(Pas)
    OPEN "..\Master\Rates   .Mas" FOR RANDOM LOCK WRITE AS #10 LEN = LEN(Rat)
END SUB

DEFINT A-Z
FUNCTION PassWord (f$)
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "Confirm", e)
    f$ = "": SOUND 900, 1: SOUND 950, 1
    FOR x = 1 TO 4
    s$ = INPUT$(1): IF s$ = CHR$(27) OR s$ = CHR$(13) THEN EXIT FOR
    f$ = f$ + s$
    NEXT
    IF UCASE$(f$) = "NANY" THEN PassWord = 1
    PCOPY 1, 0
END FUNCTION

DEFLNG L
DEFSNG A-K, M-Q, S-Z
SUB ShowAmt (r, L1, L2)
    LOCATE r, 53: PRINT USING "######,.##"; L1 / 100
    LOCATE r, 68: PRINT USING "######,.##"; L2 / 100
END SUB

DEFSNG L, R
SUB ShowCtr ()
    COLOR 10, 0
    LOCATE 2, 52:  PRINT Ctr.Num;
    LOCATE 3, 52:  PRINT Ctr.Nam;
    LOCATE 4, 52:  PRINT Ctr.Str;
    LOCATE 5, 52:  PRINT Ctr.Cty; " "; Ctr.Sta; " "; Ctr.Zip;
    LOCATE 6, 52:  PRINT Ctr.Tel;
    LOCATE 8, 52:  PRINT Ctr.Trm;
    LOCATE 10, 52: PRINT Ctr.Typ;
    ShowAmt 17, Ctr.Mbb, Ctr.Ybb
    ShowAmt 18, Ctr.Mam, Ctr.Yam
    ShowAmt 20, Ctr.Mbb + Ctr.Mam, Ctr.Ybb + Ctr.Yam
    ShowAmt 21, Ctr.Mpd, Ctr.Ypd
    ShowAmt 23, Ctr.Mbb + Ctr.Mam - Ctr.Mpd, Ctr.Ybb + Ctr.Yam - Ctr.Ypd
END SUB

SUB ShowSto ()
    COLOR 10, 0
    LOCATE 2, 52:  PRINT Sto.Num
    LOCATE 3, 52:  PRINT Sto.Div
    LOCATE 4, 52:  PRINT Sto.Cod
    LOCATE 6, 52:  PRINT Sto.Nam
    LOCATE 7, 52:  PRINT Sto.Add
    LOCATE 8, 52:  PRINT Sto.Str
    LOCATE 9, 52:  PRINT Sto.Cty; " "; Sto.Sta; " "; Sto.Zip
    LOCATE 11, 52: PRINT Sto.Man
    LOCATE 12, 52: PRINT Sto.Acc
    LOCATE 13, 52: PRINT Sto.Acm
    LOCATE 14, 52: PRINT Sto.Sec
    LOCATE 16, 52: PRINT Sto.Tel
    LOCATE 17, 52: PRINT Sto.Pho
    LOCATE 19, 52: PRINT Sto.Opn
    LOCATE 21, 52: PRINT Sto.Stx
END SUB

SUB SortPas ()
    CLOSE
    SHELL "SortF ..\Master\Password.Nam ..\Master\Password.Mas /L27 /+1,4 /Q"
    SHELL "SortF ..\Master\Password.Mas ..\Master\Password.Nam /L27 /+5,19 /C /Q"
    OpenFiles
END SUB

SUB SortRat ()
    CLOSE
    SHELL "sortf ..\Master\Rates.Inx ..\Master\Rates.Mas /L58 /+1,3 /Q"
    SHELL "sortf ..\Master\Rates.Mas ..\Master\Rates.Inx /L58 /+5,54/Q"
    OpenFiles
END SUB

SUB TimeDate ()
 COLOR 16, 14
 LOCATE 21, 24, 0: PRINT LEFT$(DATE$, 6) + RIGHT$(DATE$, 2)
 LOCATE 21, 49: PRINT TIME$
END SUB

DEFINT A-Z
SUB WinCode (t$)
    SCREEN , , 1, 1
    PCOPY 0, 1
    b = 1: t = LOF(7) / LEN(Cod): r = 7: n = 1: GOSUB C2
C1: GET #7, n, Cod
    COLOR 15, 2: LOCATE r, 46: PRINT USING "&ณ&ณ&"; Cod.Cmb; Cod.Cod; Cod.Des
    KeyHit n$
    COLOR 11, 1: LOCATE r, 46: PRINT USING "&ณ&ณ&"; Cod.Cmb; Cod.Cod; Cod.Des
    SELECT CASE n$
    CASE CHR$(13)
        t$ = Cod.Cmb
        SCREEN , , 0, 0
        COLOR 14, 0
        EXIT SUB
    CASE CHR$(27)
        t$ = ""
        SCREEN , , 0, 0
        COLOR 14, 0
        EXIT SUB
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > t GOTO C1 ELSE n = n + 1
        IF r + 1 > 20 THEN CALL Scroll(46, 7, 77, 20, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 GOTO C1 ELSE n = n - 1
        IF r - 1 < 7 THEN CALL BkScroll(46, 7, 77, 20, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        b = n - r - 7: IF b < 1 THEN b = 1
        GOSUB C2: n = b + r - 7
    CASE CHR$(0) + "Q" 'PgDn
        b = 21 - r + n
        IF b > t - 13 THEN b = t - 13: n = t - 20 + r ELSE n = n + 14
        GOSUB C2
    CASE CHR$(0) + "G" 'Home
        b = 1: GOSUB C2
        n = r - 6
    CASE CHR$(0) + "O" 'End
        b = t - 13: GOSUB C2
        n = t - 20 + r
    CASE ELSE
    END SELECT
    GOTO C1
 
C2: COLOR 11, 1: LOCATE 6, 45: PRINT "ษออัออออัออออออออออออออออออออออออป"
    FOR x = b TO t: GET #7, x, Cod
    LOCATE , 45: PRINT USING "บ&ณ&ณ&บ"; Cod.Cmb; Cod.Cod; Cod.Des
    IF CSRLIN > 20 THEN EXIT FOR
    NEXT
    LOCATE , 45: PRINT "ศออฯออออฯออออออออออออออออออออออออผ"
    RETURN
END SUB

DEFSNG A-Z
SUB WinMssg (f$)
    PCOPY 0, 2
    COLOR 20, 7
    LOCATE 12, 36: PRINT STRING$(LEN(f$) + 4, " ");
    LOCATE 13, 36: PRINT "  "; f$; "  ";
    LOCATE 14, 36: PRINT STRING$(LEN(f$) + 4, " ");
    COLOR 7, 7
    LOCATE 13, 35: PRINT "Û";
    LOCATE 14, 35: PRINT "Û";
    LOCATE 15, 35: PRINT "Û"; STRING$(LEN(f$) + 3, "Û");
    SOUND 60, 1: SOUND 80, 1: KeyHit f$
    PCOPY 2, 0
END SUB

