DEFINT A-Z: DEFLNG L
DECLARE FUNCTION FindCom (c$)

DECLARE SUB SortRat ()
DECLARE SUB SortCmb ()
DECLARE SUB SortCod ()
DECLARE SUB SortPas ()
DECLARE SUB SortPrc ()

DECLARE SUB PrntCod ()
DECLARE SUB PrntCtr ()
DECLARE SUB PrntPas ()
DECLARE SUB PrntSto ()
DECLARE SUB PrntCmb ()

DECLARE SUB Heading ()
DECLARE SUB HCodes ()
DECLARE SUB HCombo ()
DECLARE SUB HRates ()
DECLARE SUB HWords ()
DECLARE SUB HPrices ()
DECLARE SUB HCenter ()
DECLARE SUB HStores ()

DECLARE SUB WinCode (t$)
DECLARE SUB WinMssg (f$)
DECLARE SUB ShowSto ()
DECLARE SUB ShowCtr ()
DECLARE SUB ShowAmt (x, l1, L2)
DECLARE SUB SubMenu (x, s$)

DECLARE SUB FindGrp (f$)
DECLARE SUB KeyPres (f$)
DECLARE SUB OpenFiles ()
DECLARE SUB Statlin (f$)
DECLARE SUB WinRate ()

DECLARE SUB qPrint (s$, BYVAL r, BYVAL c, BYVAL p, BYVAL f)
DECLARE SUB xqPRINT (s$, BYVAL r, BYVAL c, BYVAL a, BYVAL p, BYVAL f)

REM $INCLUDE: '..\Include\Config.OFF'

DIM SHARED Menu(7, 2)   AS INTEGER

DEFINT L
FUNCTION FindCom (Code$)
   b = 1: t = LOF(8) \ LEN(Cod)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #8, m, Cod
      IF Code$ = Cod.Cmb THEN FindCom = m: EXIT DO
      IF Code$ > Cod.Cmb THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

DEFSNG A-Z
SUB HCenter ()
    Heading
    LPRINT TAB(62); "SERVICE  CENTERS": LPRINT
    LPRINT CHR$(28);
    LPRINT "                                                                                    ============== M O N T H ============== =============== Y E A R ==============="
    LPRINT "N§       Service Center              Street Address            City        ST  Zip   Forward   Charges  Payments   Balance   Forward   Charges  Payments   Balance "
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

SUB HPrices ()
    Heading
    LPRINT TAB(64); "PRICE MASTER": LPRINT
    LPRINT "                              =====PRICES=====              ===WARRANTY==="
    LPRINT "Code  Distributor  Model N§     Old      New       Date     Parts    Labor"
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

DEFINT A-Z
SUB mMasChk ()
    CALL MhDisplayByName(0, 0, "Validate", e)

    Statlin "Check Master Files"

    FOR x = 1 TO LOF(1) \ LEN(Cod)
        GET #1, x, Cod: Cod.Del = "?": PUT #1, x, Cod
        LOCATE 36, 57: PRINT USING "#####"; x;
    NEXT

    FOR x = 1 TO LOF(3) \ LEN(Prc)
        GET #3, x, Prc: Prc.Del = "?": PUT #3, x, Prc
        LOCATE 40, 57: PRINT USING "#####"; x;
    NEXT

    FOR x = 1 TO LOF(4) \ LEN(Rat)
        GET #4, x, Rat: Rat.Del = "?": PUT #4, x, Rat
        LOCATE 44, 57: PRINT USING "#####"; x;
    NEXT

    SETINDEX 10, "Sto"
    MOVEFIRST #10
    DO: IF INKEY$ = CHR$(27) THEN EXIT DO
        RETRIEVE #10, Inv
        i& = i& + 1
        n = FindCod(Inv.Cod)
        IF n THEN Cod.Del = " ": PUT #1, n, Cod
        LOCATE 36, 57: PRINT USING "#####"; i&;
        n = FindPrc(Inv.Dis + Inv.Mdl + Inv.Cod)
        IF n THEN Prc.Del = " ": PUT #3, n, Prc
        LOCATE 40, 57: PRINT USING "#####"; i&;
        MOVENEXT #10
    LOOP UNTIL EOF(10)

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
END SUB

SUB mMasCmb ()
    CALL MhDisplayByName(0, 0, "Mast-Cmb", e)
    Statlin "Combos Master File"
    SubMenu 1, ""
    r = 5: n = 1: b = 1: t = LOF(2) \ LEN(Cmb): GOSUB DispCmb
300 GET #2, n, Cmb
    COLOR 16, 4: LOCATE r, 2: PRINT USING "  & ³ &  ³   &  ³   &  ³   &  ³ & ³ & ³ & ³ & "; Cmb.Cod; Cmb.Des; Cmb.Rat; Cmb.Dtr; Cmb.Dfm; Cmb.Cde(1); Cmb.Cde(2); Cmb.Cde(3); Cmb.Cde(4);
    KeyPres n$
    IF n$ = CHR$(13) THEN SubMenu 0, n$
    SELECT CASE n$
    CASE CHR$(27)       '==============Esc====
         EXIT SUB
    CASE CHR$(0) + "M"  '==============Right==
         SubMenu 0, n$
    CASE CHR$(0) + "K"  '==============Left===
         SubMenu 0, n$
    CASE CHR$(0) + "P"  '==============Down===
         IF n + 1 > t THEN 300 ELSE n = n + 1
         COLOR 24, 3: LOCATE , 2: PRINT USING "  & ³ &  ³   &  ³   &  ³   &  ³ & ³ & ³ & ³ & "; Cmb.Cod; Cmb.Des; Cmb.Rat; Cmb.Dtr; Cmb.Dfm; Cmb.Cde(1); Cmb.Cde(2); Cmb.Cde(3); Cmb.Cde(4);
         IF r + 1 > 47 THEN CALL Scroll(2, 5, 79, 47, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"  '==============Up=====
         IF n - 1 < 1 THEN 300 ELSE n = n - 1
         COLOR 24, 3: LOCATE , 2: PRINT USING "  & ³ &  ³   &  ³   &  ³   &  ³ & ³ & ³ & ³ & "; Cmb.Cod; Cmb.Des; Cmb.Rat; Cmb.Dtr; Cmb.Dfm; Cmb.Cde(1); Cmb.Cde(2); Cmb.Cde(3); Cmb.Cde(4);
         IF r - 1 < 5 THEN CALL BkScroll(2, 5, 79, 47, 1) ELSE r = r - 1
    CASE CHR$(0) + "I"  '==============PgUp===
         b = n - r - 38: IF b < 1 THEN b = 1
         GOSUB DispCmb: n = x + r - 47
    CASE CHR$(0) + "Q"  '==============PgDn===
         b = n - r + 48: IF b > t - 42 THEN b = t - 42
         GOSUB DispCmb: n = x + r - 47
    CASE CHR$(0) + "G"  '==============Home===
         b = 1: GOSUB DispCmb: n = r - 4
    CASE CHR$(0) + "O"  '==============End====
         b = t - 42: GOSUB DispCmb: n = t + r - 47
    CASE "N"
         SubMenu 0, n$
         s = 1: GOSUB EditCmb
    CASE "M"
         SubMenu 0, n$
         s = 0: GOSUB EditCmb
    CASE "R"
         SubMenu 0, n$
         GET #6, VAL(Cmb.Rat), Rat
         WinRate
    CASE "C"
         SubMenu 0, n$
         PCOPY 0, 2: CALL MhDisplayByName(0, 0, "Combo", e)
         GET #6, VAL(Cmb.Rat), Rat
         qPrint Cmb.Cod + " " + Cmb.Des, 12, 10, 0, 1
 '       InvRate 18, 42
         LOCATE 16
         FOR s = 1 TO 4: FindGrp (Cmb.Cde(s)): NEXT
         KeyPres f$
         PCOPY 2, 0
    CASE "P"
         SubMenu 0, n$
         PrntCmb
    CASE "E"
         SubMenu 0, n$
         IF Confirm(0, "Erase Combo Record") THEN
            GET #2, t + 1, Cmb
            Cmb.Cod = "þþþþ"
            PUT #2, n, Cmb
            SortCmb
         END IF
    END SELECT
    GOTO 300

EditCmb:
    IF s THEN
       m = t + 1
       GET #3, m, Cmb
       qPrint "       ³                           ³        ³      ³      ³    ³    ³    ³    ", r, 2, 0, 1
    ELSE
       m = n
    END IF
    c$ = Cmb.Cod
    p$ = Cmb.Cod
    d$ = Cmb.Des
    r$ = Cmb.Rat
    t$ = Cmb.Dtr
    f$ = Cmb.Dfm
    w$ = Cmb.Cde(1)
    x$ = Cmb.Cde(2)
    y$ = Cmb.Cde(3)
    z$ = Cmb.Cde(4)

310 EntryU r, 4, 4, e, b, c$:     IF e THEN 300 ELSE IF b THEN 310
    IF c$ < "!" THEN WinMssg "Code must not be blank": GOTO 310
    IF c$ <> p$ THEN IF FindCmb(c$) THEN WinMssg "Duplicate Combo exists": GOTO 310
311 EntryS r, 11, 24, e, b, d$:   IF e THEN 300 ELSE IF b THEN 310
    IF d$ < "!" THEN WinMssg "Description must not be blank": GOTO 311
312 EntryN r, 41, 3, 0, e, b, r$: IF e THEN 300 ELSE IF b THEN 311
    IF r$ < "  1" THEN WinMssg "Rate must not be zero": GOTO 312
    IF VAL(r$) > LOF(4) \ LEN(Rat) THEN WinMssg "Rate does not exist": GOTO 312
313 EntryN r, 50, 1, 0, e, b, t$: IF e THEN 300 ELSE IF b THEN 312
    IF t$ < "1" THEN WinMssg "Drop terms ratio please": GOTO 313
314 EntryN r, 57, 1, 0, e, b, f$: IF e THEN 300 ELSE IF b THEN 313
    IF f$ < "1" THEN WinMssg "Drop terms ratio please": GOTO 314
315 EntryU r, 62, 2, e, b, w$:    IF e THEN 300 ELSE IF b THEN 314
    IF w$ = "  " THEN 316
    IF FindCom(w$) = 0 THEN WinCode w$: GOTO 315
316 EntryU r, 67, 2, e, b, x$:    IF e THEN 300 ELSE IF b THEN 315
    IF x$ = "  " THEN 317
    IF FindCom(x$) = 0 THEN WinCode x$: GOTO 316
317 EntryU r, 72, 2, e, b, y$:    IF e THEN 300 ELSE IF b THEN 316
    IF y$ = "  " THEN 318
    IF FindCom(y$) = 0 THEN WinCode y$: GOTO 317
318 EntryU r, 77, 2, e, b, z$:    IF e THEN 300 ELSE IF b THEN 317
    IF z$ = "  " THEN 319
    IF FindCom(z$) = 0 THEN WinCode z$: GOTO 318
319 IF Confirm(0, "Save Combo") = 0 THEN 318
    PUT #2, m, Cmb
    SortCmb

DispCmb:
    LOCATE 5: COLOR 24, 3
    FOR x = b TO t
        GET #2, x, Cmb
        LOCATE , 2: PRINT USING "  & ³ &  ³   &  ³   &  ³   &  ³ & ³ & ³ & ³ & "; Cmb.Cod; Cmb.Des; Cmb.Rat; Cmb.Dtr; Cmb.Dfm; Cmb.Cde(1); Cmb.Cde(2); Cmb.Cde(3); Cmb.Cde(4)
        IF CSRLIN = 48 THEN EXIT FOR
    NEXT
    RETURN
END SUB

SUB mMasCod ()
    CALL MhDisplayByName(0, 0, "Mast-Cod", e)
    Statlin "Codes Master File"
    SubMenu 1, ""
    r = 5: n = 1: m = 1: b = 1: t = LOF(1) \ LEN(Cod): GOSUB DispCod
100 GET #1, n, Cod
    COLOR 17, 4: LOCATE r, 2: PRINT USING "   &   ³&³  & ³ &  ³ &  ³&³ &³&³ & ³  & ³  & "; Cod.Del; Cod.Cod; Cod.Typ; Cod.Grp; Cod.Cmb; Cod.Des; Cod.Rat; Cod.Fmv; Cod.Dep; Cod.Dtr; Cod.Dfm;
    KeyPres n$
    IF n$ = CHR$(13) THEN SubMenu 0, n$
    SELECT CASE n$
    CASE CHR$(27)       '===============Esc====
        EXIT SUB
    CASE CHR$(0) + "M"  '===============right==
        SubMenu 0, n$
    CASE CHR$(0) + "K"  '===============left===
        SubMenu 0, n$
    CASE CHR$(0) + "P"  '===============Down===
        IF n + 1 > t THEN 100 ELSE n = n + 1
        COLOR 1, 7:  LOCATE , 2: PRINT USING "   &   ³&³  & ³ &  ³ &  ³&³ &³&³ & ³  & ³  & "; Cod.Del; Cod.Cod; Cod.Typ; Cod.Grp; Cod.Cmb; Cod.Des; Cod.Rat; Cod.Fmv; Cod.Dep; Cod.Dtr; Cod.Dfm;
        IF r + 1 > 47 THEN CALL Scroll(2, 5, 79, 47, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"  '===============Up=====
        IF n - 1 < 1 THEN 100 ELSE n = n - 1
        COLOR 1, 7:  LOCATE , 2: PRINT USING "   &   ³&³  & ³ &  ³ &  ³&³ &³&³ & ³  & ³  & "; Cod.Del; Cod.Cod; Cod.Typ; Cod.Grp; Cod.Cmb; Cod.Des; Cod.Rat; Cod.Fmv; Cod.Dep; Cod.Dtr; Cod.Dfm;
        IF r - 1 < 5 THEN CALL BkScroll(2, 5, 79, 47, 1) ELSE r = r - 1
    CASE CHR$(0) + "I"  '===============PgUp===
        b = n - r - 38: IF b < 1 THEN b = 1
        GOSUB DispCod:  n = x + r - 47
    CASE CHR$(0) + "Q"  '===============PgDn===
        b = n - r + 48: IF b > t - 42 THEN b = t - 42
        GOSUB DispCod:  n = x + r - 47
    CASE CHR$(0) + "G"  '===============Home===
        b = 1: GOSUB DispCod: n = r - 4
    CASE CHR$(0) + "O"  '===============End====
        b = t - 42: GOSUB DispCod: n = t + r - 47
    CASE "N"
        SubMenu 0, n$
        s = 1: GOSUB EditCod
    CASE "M"
        SubMenu 0, n$
        s = 0: GOSUB EditCod
    CASE "E"
        SubMenu 0, n$
        IF Confirm(0, "") THEN
           GET #1, t + 1, Cod
           Cod.Cod = "þþþþ"
           PUT #1, n, Cod
           SortCod
           b = n - r + 5
           GOSUB DispCod
        END IF
    CASE "R"
        SubMenu 0, n$
        GET #4, VAL(Cod.Rat), Rat
        WinRate
    CASE "C"
        SubMenu 0, n$
        WinCode f$
    CASE "O"
        SubMenu 0, n$
        SETINDEX 10, "Cod"
        FOR x = 1 TO LOF(1) \ LEN(Cod)
        GET #1, x, Cod
        SEEKEQ #10, Cod.Cod
        IF EOF(10) THEN Cod.Del = " ": PUT #1, x, Cod
        NEXT
    CASE "P"
        SubMenu 0, n$
        PrntCod
    END SELECT
    GOTO 100

EditCod:
    IF s THEN m = 1 + t ELSE m = n
    GET #1, m, Cod
    IF s THEN
       m = 1 + LOF(1) \ LEN(Cod)
       GET #1, m, Cod
       qPrint "       ³    ³    ³     ³     ³                        ³    ³   ³    ³    ³    ", r, 2, 0, 1
110    EntryU r, 10, 4, e, b, Cod.Cod: IF e THEN 100 ELSE IF b THEN 110
       IF FindCod(Cod.Cod) THEN WinMssg "Duplicate Code exists": GOTO 110
       IF Cod.Cod < "!" THEN WinMssg "Code must not be blank": GOTO 110
    ELSE
       m = n
    END IF
111 EntryU r, 17, 1, e, b, Cod.Typ:    IF e THEN 100 ELSE IF b THEN IF s THEN 110 ELSE 111
    IF Cod.Typ < "!" THEN WinMssg "Type must not be blank": GOTO 111
112 EntryU r, 21, 2, e, b, Cod.Grp:    IF e THEN 100 ELSE IF b THEN 111
    IF Cod.Grp < "!" THEN WinMssg "Group must not be blank": GOTO 112
113 EntryU r, 27, 2, e, b, Cod.Cmb:    IF e THEN 100 ELSE IF b THEN 112
114 EntryS r, 32, 24, e, b, Cod.Des:   IF e THEN 100 ELSE IF b THEN 113
    IF Cod.Des < "!" THEN WinMssg "Description must not be blank": GOTO 114
115 EntryN r, 58, 3, 0, e, b, Cod.Rat: IF e THEN 100 ELSE IF b THEN 114
    IF Cod.Rat < "  1" THEN WinMssg "Rate must not be zero": GOTO 115
    IF VAL(Cod.Rat) > LOF(4) \ LEN(Rat) THEN WinMssg "Rate does not exist": GOTO 115
116 EntryN r, 62, 3, 1, e, b, Cod.Fmv: IF e THEN 100 ELSE IF b THEN 115
    IF Cod.Fmv < "0.0" THEN WinMssg "FMV must not be zero": GOTO 116
117 EntryN r, 67, 2, 0, e, b, Cod.Dep: IF e THEN 100 ELSE IF b THEN 116
118 EntryN r, 73, 1, 0, e, b, Cod.Dtr: IF e THEN 100 ELSE IF b THEN 117
119 EntryN r, 78, 1, 0, e, b, Cod.Dfm: IF e THEN 100 ELSE IF b THEN 118
    IF Confirm(1, "SAVE Code") = 0 THEN 119
    t = 1 + LOF(1) \ LEN(Cod)
    PUT #1, t, Cod
    SortCod
    RETURN

DispCod:
    LOCATE 5: COLOR 1, 7
    FOR x = b TO t
       GET #1, x, Cod
       LOCATE , 2: PRINT USING "   &   ³&³  & ³ &  ³ &  ³&³ &³&³ & ³  & ³  & "; Cod.Del; Cod.Cod; Cod.Typ; Cod.Grp; Cod.Cmb; Cod.Des; Cod.Rat; Cod.Fmv; Cod.Dep; Cod.Dtr; Cod.Dfm
       IF CSRLIN = 48 THEN EXIT FOR
    NEXT
    RETURN
END SUB

SUB mMasCtr ()
    CALL MhDisplayByName(0, 0, "Mast-Ctr", e)
    Statlin "Service Centers Master"
    SubMenu 1, ""
    r = 3: n = 1: b = 1: t = LOF(7) / LEN(Ctr): GOSUB DispCtr

600 GET #7, n, Ctr
    COLOR 16, 4: LOCATE r, 2: PRINT USING " & ³&"; Ctr.Num; Ctr.Nam;
    KeyPres n$
    IF n$ = CHR$(13) THEN SubMenu 0, n$
    SELECT CASE n$
    CASE CHR$(27)       '===============Esc====
         EXIT SUB
    CASE CHR$(0) + "M"  '===============Right==
         SubMenu 0, n$
    CASE CHR$(0) + "K"  '===============Left===
         SubMenu 0, n$
    CASE CHR$(0) + "P"  '===============Down===
         IF n + 1 > t THEN 600 ELSE n = n + 1
         COLOR 16, 3: LOCATE , 2: PRINT USING " & ³&"; Ctr.Num; Ctr.Nam;
         IF r + 1 > 47 THEN CALL Scroll(3, 3, 32, 47, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"  '===============Up=====
         IF n - 1 < 1 THEN 600 ELSE n = n - 1
         COLOR 16, 3: LOCATE , 2: PRINT USING " & ³&"; Ctr.Num; Ctr.Nam;
         IF r - 1 < 3 THEN CALL BkScroll(3, 3, 32, 47, 1) ELSE r = r - 1
    CASE CHR$(0) + "I"  '===============PgUp===
         b = n - r - 42: IF b < 1 THEN b = 1
         GOSUB DispCtr: n = b + r - 3
    CASE CHR$(0) + "Q"  '===============PgDn===
         b = n - r + 48: IF b > t - 44 THEN b = t - 44
         GOSUB DispCtr: n = b + r - 3
    CASE CHR$(0) + "G"  '===============Home===
         b = 1: GOSUB DispCtr: n = r - 2
    CASE CHR$(0) + "O"  '===============End====
         b = t - 44: GOSUB DispCtr: n = t + r - 47
    CASE "D"
         SubMenu 0, n$
         ShowCtr
    CASE "M"
         SubMenu 0, n$
         GOSUB EditCtr
    CASE "E"
         SubMenu 0, n$
         ShowCtr
         IF Confirm(0, "") THEN
            s$ = Ctr.Num
            GET #7, 100, Ctr
            Ctr.Num = s$
            PUT #7, n, Ctr
         END IF
    CASE "P"
         SubMenu 0, n$
         PrntCtr
    END SELECT
    GOTO 600

EditCtr:
    ShowCtr
610 EntryS 11, 52, 26, e, b, Ctr.Nam:   IF e THEN 600 ELSE IF b THEN 610
611 EntryS 12, 52, 26, e, b, Ctr.Str:   IF e THEN 600 ELSE IF b THEN 610
612 EntryS 13, 52, 17, e, b, Ctr.Cty:   IF e THEN 600 ELSE IF b THEN 611
613 EntryU 13, 70, 2, e, b, Ctr.Sta:    IF e THEN 600 ELSE IF b THEN 612
614 EntryU 13, 73, 5, e, b, Ctr.Zip:    IF e THEN 600 ELSE IF b THEN 613
615 EntryT 18, 52, e, b, Ctr.Tel:       IF e THEN 600 ELSE IF b THEN 614
616 EntryN 23, 52, 2, 0, e, b, Ctr.Trm: IF e THEN 600 ELSE IF b THEN 615
617 EntryU 28, 52, 1, e, b, Ctr.Typ:    IF e THEN 600 ELSE IF b THEN 616
    IF Confirm(0, "") THEN PUT #7, n, Ctr ELSE 617
    GOTO 600

DispCtr:
    LOCATE 3: COLOR 16, 3
    FOR x = b TO t
    GET #7, x, Ctr
    LOCATE , 2: PRINT USING " & ³&"; Ctr.Num; Ctr.Nam
    IF CSRLIN = 48 THEN EXIT FOR
    NEXT
RETURN
END SUB

SUB mMasPak ()
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
END SUB

SUB mMasPas ()
    CALL MhDisplayByName(0, 0, "Mast-Pas", e)
    Statlin "Password Master File"
    SubMenu 1, ""
    r = 5: n = 1: b = 1: t = LOF(5) \ LEN(Pas): GOSUB DispPas
    COLOR 7, 4: LOCATE 48, 75: PRINT t;
700 GET #5, n, Pas
    COLOR 17, 7: LOCATE r, 2: PRINT USING "   &  ³  &  ³ & ³  &  "; Pas.Ini; Pas.Pas; Pas.Nam; Pas.Lvl;
    KeyPres n$
    IF n$ = CHR$(13) THEN SubMenu 0, n$
    SELECT CASE n$
    CASE CHR$(27)       '===============Esc====
         EXIT SUB
    CASE CHR$(0) + "M"  '===============Right==
         SubMenu 0, n$
    CASE CHR$(0) + "K"  '===============Left===
         SubMenu 0, n$
    CASE CHR$(0) + "P"  '===============Down===
         IF n + 1 > t THEN 700 ELSE n = n + 1
         COLOR 1, 7:  LOCATE , 2: PRINT USING "   &  ³  &  ³ & ³  &  "; Pas.Ini; Pas.Pas; Pas.Nam; Pas.Lvl;
         IF r + 1 > 47 THEN CALL Scroll(2, 5, 45, 47, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"  '===============Up=====
         IF n - 1 < 1 THEN 700 ELSE n = n - 1
         COLOR 1, 7:  LOCATE , 2: PRINT USING "   &  ³  &  ³ & ³  &  "; Pas.Ini; Pas.Pas; Pas.Nam; Pas.Lvl;
         IF r - 1 < 5 THEN CALL BkScroll(2, 5, 45, 47, 1) ELSE r = r - 1
    CASE CHR$(0) + "I"  '===============PgUp===
        b = n - r - 38: IF b < 1 THEN b = 1
        GOSUB DispPas: n = x + r - 47
    CASE CHR$(0) + "Q"  '===============PgDn===
        b = n - r + 48: IF b > t - 42 THEN b = t - 42
        GOSUB DispPas: n = x + r - 47
    CASE CHR$(0) + "G"  '===============Home===
        b = 1: GOSUB DispPas: n = r - 4
    CASE CHR$(0) + "O"  '===============End====
        b = t - 42: GOSUB DispPas: n = t + r - 47
    CASE "L"
        SubMenu 0, n$
        l = l + 1: IF l > 3 THEN l = 1
        CALL MhDisplayByName(0, 0, "Level-" + LTRIM$(STR$(l)), e)
    CASE "N"
        SubMenu 0, n$
        s = 1: GOSUB EditPas
    CASE "E"
        SubMenu 0, n$
        s = 0: GOSUB EditPas
    CASE "D"
        SubMenu 0, n$
        IF Confirm(0, "") THEN
           GET #5, t + 1, Pas
           Pas.Nam = CHR$(255)
           Pas.Pas = CHR$(255)
           PUT #5, n, Pas
           SortPas
           b = n - r + 4
           GOSUB DispPas
        END IF
    CASE "P"
        SubMenu 0, n$
        PrntPas
    END SELECT
    GOTO 700
EditPas:
    IF s THEN m = 1 + t ELSE m = n
    GET #5, m, Pas
    i$ = Pas.Ini: a$ = Pas.Ini
    IF p$ < "    " THEN
       p$ = "    "
    ELSE
       p$ = CHR$(ASC(MID$(Pas.Pas, 1, 1)) - 34) + CHR$(ASC(MID$(Pas.Pas, 2, 1)) - 70) + CHR$(ASC(MID$(Pas.Pas, 3, 1)) - 81) + CHR$(ASC(MID$(Pas.Pas, 4, 1)) - 63)
    END IF
    b$ = p$
    n$ = Pas.Nam
    l$ = Pas.Lvl
    IF s THEN
       qPrint "        ³        ³                     ³     ", r, 2, 0, 1
710    EntryU r, 5, 3, e, b, i$:  IF e THEN 700 ELSE IF b THEN 710
       IF LEN(RTRIM$(LTRIM$(i$))) < 3 THEN WinMssg "Initials must be full": GOTO 710
       IF i$ <> a$ THEN IF FindIni(i$) THEN WinMssg "Duplicate initials": GOTO 710
    END IF
711 EntryS r, 13, 4, e, b, p$:    IF e THEN 700 ELSE IF b THEN IF s THEN 710 ELSE 711
    IF p$ < "!" THEN WinMssg "Password must not be blank": GOTO 711
    IF p$ <> b$ THEN
       o$ = CHR$(ASC(MID$(p$, 1, 1)) + 34) + CHR$(ASC(MID$(p$, 2, 1)) + 70) + CHR$(ASC(MID$(p$, 3, 1)) + 81) + CHR$(ASC(MID$(p$, 4, 1)) + 63)
       IF FindPas(o$) THEN WinMssg "Duplicate Password": GOTO 711
    END IF
712 EntryS r, 21, 19, e, b, n$:   IF e THEN 700 ELSE IF b THEN 711
    IF n$ < "!" THEN WinMssg "Employee must not be blank": GOTO 712
713 EntryN r, 44, 1, 0, e, b, l$: IF e THEN 700 ELSE IF b THEN 712
    IF l$ < "0" THEN WinMssg "Security level please": GOTO 713
    IF Confirm(0, "") = 0 THEN 713
    Pas.Pas = CHR$(ASC(MID$(p$, 1, 1)) + 34) + CHR$(ASC(MID$(p$, 2, 1)) + 70) + CHR$(ASC(MID$(p$, 3, 1)) + 81) + CHR$(ASC(MID$(p$, 4, 1)) + 63)
    Pas.Ini = i$
    Pas.Nam = n$
    Pas.Lvl = l$
    PUT #5, m, Pas
    SortPas
    t = LOF(5) \ LEN(Pas)
    COLOR 16, 3: LOCATE 48, 75: PRINT t;
    b = n - r + 4
    
DispPas:
    LOCATE 5: COLOR 1, 7
    FOR x = b TO t
       GET #5, x, Pas
       LOCATE , 2: PRINT USING "   &  ³  &  ³ & ³  &  "; Pas.Ini; Pas.Pas; Pas.Nam; Pas.Lvl
       IF CSRLIN = 48 THEN EXIT FOR
    NEXT
    RETURN
END SUB

SUB mMasPrc ()
    CALL MhDisplayByName(0, 0, "Mast-Prc", e)
    Statlin "Prices Master File"
    SubMenu 1, ""
    r = 5: n = 1: b = 1: t = LOF(3) \ LEN(Prc): GOSUB DispPrc
    COLOR 0, 3: LOCATE 48, 75: PRINT t;
200 GET #3, n, Prc
    COLOR 11, 1: LOCATE r, 2: PRINT USING "   &  ³& ³& ³&³ & ³ & ³&³ & ³ & "; Prc.Del; Prc.Dis; Prc.Mdl; Prc.Cod; Prc.Old; Prc.New; Prc.Dat; Prc.Par; Prc.Lab;
    KeyPres n$
    IF n$ = CHR$(13) THEN SubMenu 0, n$
    SELECT CASE n$
    CASE CHR$(27)       '===============Esc===
         EXIT SUB
    CASE CHR$(0) + "M"  '===============Right=
         SubMenu 0, n$
    CASE CHR$(0) + "K"  '===============Left==
         SubMenu 0, n$
    CASE CHR$(0) + "P"  '===============Down==
         IF n + 1 > t THEN 200 ELSE n = n + 1
         COLOR 7, 6:  LOCATE , 2: PRINT USING "   &  ³& ³& ³&³ & ³ & ³&³ & ³ & "; Prc.Del; Prc.Dis; Prc.Mdl; Prc.Cod; Prc.Old; Prc.New; Prc.Dat; Prc.Par; Prc.Lab;
         IF r + 1 > 47 THEN CALL Scroll(5, 5, 78, 47, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"  '===============Up====
         IF n - 1 < 1 THEN 200 ELSE n = n - 1
         COLOR 7, 6:  LOCATE , 2: PRINT USING "   &  ³& ³& ³&³ & ³ & ³&³ & ³ & "; Prc.Del; Prc.Dis; Prc.Mdl; Prc.Cod; Prc.Old; Prc.New; Prc.Dat; Prc.Par; Prc.Lab;
         IF r - 1 < 5 THEN CALL BkScroll(5, 5, 78, 47, 1) ELSE r = r - 1
    CASE CHR$(0) + "I"  '===============PgUp==
        b = n - r - 38: IF b < 1 THEN b = 1
        GOSUB DispPrc:  n = x + r - 47
    CASE CHR$(0) + "Q"  '===============PgDn==
        b = n - r + 48: IF b > t - 42 THEN b = t - 42
        GOSUB DispPrc:  n = x + r - 47
    CASE CHR$(0) + "G"  '===============Home==
        b = 1: GOSUB DispPrc: n = r - 4
    CASE CHR$(0) + "O"  '===============End===
        b = t - 42: GOSUB DispPrc: n = t + r - 47
    CASE "N"
        SubMenu 0, n$
        s = 1: GOSUB EditPrc
    CASE "M"
        SubMenu 0, n$
        s = 0: GOSUB EditPrc
    CASE "P"
        SubMenu 0, n$
        PageNum = 0: HPrices
        FOR x = 1 TO LOF(4) / LEN(Prc)
        GET #3, x, Prc
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        IF LineNum > 61 THEN HPrices
        LineNum = LineNum + 1
        LPRINT USING "&  "; Prc.Dis; Prc.Mdl; Prc.Cod; Prc.Old; Prc.New; Prc.Dat;
        LPRINT USING " &      &"; Prc.Par; Prc.Lab
        NEXT
        LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(3) / LEN(Prc); "PRICES in the master file."
    END SELECT
    GOTO 200

EditPrc:
    IF s THEN m = 1 + LOF(3) \ LEN(Prc) ELSE m = LOC(4)
    GET #3, m, Prc
    d$ = Prc.Dis
    m$ = Prc.Mdl
    c$ = Prc.Cod
    o$ = Prc.Old
    n$ = Prc.New
    a$ = Prc.Dat
    p$ = Prc.Par
    l$ = Prc.Lab
    IF s THEN
       qPrint "      ³           ³           ³    ³         ³         ³          ³     ³     ", r, 2, 0, 1
210    EntryU r, 9, 10, e, b, d$:  IF e THEN 200 ELSE IF b THEN 210
       IF d$ < "!" THEN WinMssg "Distributor can't be blank": GOTO 210
211    EntryU r, 21, 10, e, b, m$: IF e THEN 200 ELSE IF b THEN 210
       IF m$ < "!" THEN WinMssg "Model Number must not be blank": GOTO 211
212    EntryU r, 33, 4, e, b, c$:  IF e THEN 200 ELSE IF b THEN 211
       IF FindCod(c$) = 0 THEN WinMssg "Code not found": GOTO 212
       IF FindPrc(d$ + m$ + c$) THEN WinMssg "Duplicate price": GOTO 212
    END IF
213 EntryN r, 39, 7, 2, e, b, o$:  IF e THEN 200 ELSE IF b THEN IF s THEN 212 ELSE 213
    IF o$ < "   0.01" THEN WinMssg "Cost must not be zero": GOTO 213
214 EntryN r, 49, 7, 2, e, b, n$:  IF e THEN 200 ELSE IF b THEN 213
    IF n$ < "   0.01" THEN WinMssg "Cost must not be zero": GOTO 214
215 EntryD r, 58, e, b, a$:        IF e THEN 200 ELSE IF b THEN 214
    IF a$ < "!" THEN WinMssg "Must have a date": GOTO 215
217 EntryN r, 70, 3, 0, e, b, p$:  IF e THEN 200 ELSE IF b THEN 215
    IF p$ < "  0" THEN WinMssg "Parts warranty can't be zero": GOTO 217
218 EntryN r, 76, 3, 0, e, b, l$:  IF e THEN 200 ELSE IF b THEN 217
    IF l$ < "  0" THEN WinMssg "Labor warranty can't be zero": GOTO 218
    IF Confirm(0, "") = 0 THEN 218
       Prc.Dis = d$
       Prc.Mdl = m$
       Prc.Cod = c$
       Prc.Old = o$
       Prc.New = n$
       Prc.Dat = a$
       Prc.Par = p$
       Prc.Lab = l$
       PUT #3, m, Prc
       SortPrc
    COLOR 0, 3: LOCATE 48, 75: PRINT t;
    b = n - r + 5
DispPrc:
     LOCATE 5: COLOR 7, 6
     FOR x = b TO t
         GET #3, x, Prc
         LOCATE , 2: PRINT USING "   &  ³& ³& ³&³ & ³ & ³&³ & ³ & "; Prc.Del; Prc.Dis; Prc.Mdl; Prc.Cod; Prc.Old; Prc.New; Prc.Dat; Prc.Par; Prc.Lab
         IF CSRLIN = 48 THEN EXIT FOR
     NEXT
     RETURN
END SUB

SUB mMasRat ()
    CALL MhDisplayByName(0, 0, "Mast-Rat", e)
    Statlin "Rates Master File"
    SubMenu 1, ""
    n = 1: r = 5: b = 1: t = LOF(4) \ LEN(Rat): GOSUB DispRat
    COLOR 16, 3: LOCATE 48, 75: PRINT t;

400 GET #4, n, Rat
    COLOR 0, 7: LOCATE r, 2: PRINT USING "&  &"; Rat.Del; Rat.Num;
                LOCATE , 9:  PRINT USING " &³&³ &³&"; Rat.Mtr(1); Rat.Mrt(1); Rat.Wtr(1); Rat.Wrt(1);
                LOCATE , 33: PRINT USING " &³&³ &³&"; Rat.Mtr(2); Rat.Mrt(2); Rat.Wtr(2); Rat.Wrt(2);
                LOCATE , 57: PRINT USING " &³&³ &³&"; Rat.Mtr(3); Rat.Mrt(3); Rat.Wtr(3); Rat.Wrt(3);
    KeyPres n$
    IF n$ = CHR$(13) THEN SubMenu 0, n$
    SELECT CASE n$
    CASE CHR$(27) 'Esc
         EXIT SUB
    CASE CHR$(0) + "M"  '===============Right==
         SubMenu 0, n$
    CASE CHR$(0) + "K"  '===============Left===
         SubMenu 0, n$
    CASE CHR$(0) + "P"  '===============Down===
         IF n + 1 > t THEN 400 ELSE n = n + 1
         COLOR 16, 2: LOCATE , 2:  PRINT USING "&  &"; Rat.Del; Rat.Num;
         COLOR 16, 7: LOCATE , 9:  PRINT USING " &³&³ &³&"; Rat.Mtr(1); Rat.Mrt(1); Rat.Wtr(1); Rat.Wrt(1);
                      LOCATE , 33: PRINT USING " &³&³ &³&"; Rat.Mtr(2); Rat.Mrt(2); Rat.Wtr(2); Rat.Wrt(2);
                      LOCATE , 57: PRINT USING " &³&³ &³&"; Rat.Mtr(3); Rat.Mrt(3); Rat.Wtr(3); Rat.Wrt(3);
         IF r + 1 > 47 THEN CALL Scroll(1, 5, 79, 47, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"  '===============Up=====
         IF n - 1 < 1 THEN 400 ELSE n = n - 1
         COLOR 16, 2: LOCATE , 2:  PRINT USING "&  &"; Rat.Del; Rat.Num;
         COLOR 16, 7: LOCATE , 9:  PRINT USING " &³&³ &³&"; Rat.Mtr(1); Rat.Mrt(1); Rat.Wtr(1); Rat.Wrt(1);
                      LOCATE , 33: PRINT USING " &³&³ &³&"; Rat.Mtr(2); Rat.Mrt(2); Rat.Wtr(2); Rat.Wrt(2);
                      LOCATE , 57: PRINT USING " &³&³ &³&"; Rat.Mtr(3); Rat.Mrt(3); Rat.Wtr(3); Rat.Wrt(3);
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
    CASE "C"
         SubMenu 0, n$
         IF Confirm(0, "") THEN
            CLOSE 1: OPEN "..\Master\Rates.M" FOR RANDOM AS #1 LEN = LEN(Rat)
            CLOSE 2: OPEN "..\Master\Rates.I" FOR RANDOM AS #2 LEN = LEN(Rtx)
            FOR x = 1 TO t
                y = 0
                GET #4, x, Rat
                IF Rat.Mtr(1) <> "þþþ" THEN
                   y = y + 1
                   Rat.Num = RIGHT$(STR$(1000 + y), 3)
                   PUT #2, y, Rtx: PUT #1, y, Rat
                END IF
            NEXT
            SortRat
            b = n - r + 5
            GOSUB DispRat
         END IF
    CASE "E"
         SubMenu 0, n$
         IF Confirm(0, "") THEN
            n$ = Rat.Num
            GET #5, t + 1, Rat
            Rat.Num = n$
            Rat.Mtr(1) = "þþþ"
            PUT #5, n, Rat
            SortRat
            b = n - r + 5
            GOSUB DispRat
         END IF
    CASE "P"
         SubMenu 0, n$
         PageNum = 0: HRates
         FOR x = 1 TO LOF(5) / LEN(Rat)
            GET #5, x, Rat
            IF INKEY$ = CHR$(27) THEN EXIT FOR
            LPRINT USING "&  &"; Rat.Del; Rat.Num;
            FOR z = 1 TO 3
                LPRINT USING " & &  & &"; Rat.Mtr(z); Rat.Mrt(z); Rat.Wtr(z); Rat.Wrt(z);
            NEXT
            LPRINT
            LineNum = LineNum + 1: IF LineNum > 60 THEN HRates
         NEXT
         LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(5) / LEN(Rat); "RATES in the master file."
    END SELECT
    GOTO 400

EditRat:
    IF s THEN
       m = 1 + LOF(4) \ LEN(Rat)
       GET #4, m, Rat
       Rat.Num = RIGHT$(STR$(1000 + m), 3)
       qPrint Rat.Num + "     ³      ³    ³           ³      ³    ³           ³      ³    ³       ", r, 5, 0, 1
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
    IF Confirm(0, "") = 0 THEN 416
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
        COLOR 16, 7: LOCATE , 9:  PRINT USING " &³&³ &³&"; Rat.Mtr(1); Rat.Mrt(1); Rat.Wtr(1); Rat.Wrt(1);
                   : LOCATE , 33: PRINT USING " &³&³ &³&"; Rat.Mtr(2); Rat.Mrt(2); Rat.Wtr(2); Rat.Wrt(2);
                   : LOCATE , 57: PRINT USING " &³&³ &³&"; Rat.Mtr(3); Rat.Mrt(3); Rat.Wtr(3); Rat.Wrt(3)
        IF CSRLIN = 48 THEN EXIT FOR
    NEXT
    RETURN
END SUB

SUB mMasSto ()
    CALL MhDisplayByName(0, 0, "Mast-Sto", e)
    Statlin "Stores Master File"
    SubMenu 1, ""
    r = 3: n = 1: b = 1: t = LOF(6) \ LEN(Sto): GOSUB DispSto
500 GET #6, n, Sto
    COLOR 27, 4: LOCATE r, 2: PRINT USING " & ³&"; Sto.Num; Sto.Nam;
    KeyPres n$
    IF n$ = CHR$(13) THEN SubMenu 0, n$
    SELECT CASE n$
    CASE CHR$(27)       '===============Esc===
         EXIT SUB
    CASE CHR$(0) + "M"  '===============Right=
         SubMenu 0, n$
    CASE CHR$(0) + "K"  '===============Left==
         SubMenu 0, n$
    CASE CHR$(0) + "P"  '===============Down==
         IF n + 1 > t THEN 500 ELSE n = n + 1
         COLOR 11, 7: LOCATE , 2: PRINT USING " & ³&"; Sto.Num; Sto.Nam;
         IF r + 1 > 47 THEN CALL Scroll(2, 3, 32, 47, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"  '===============Up====
         IF n - 1 < 1 THEN 500 ELSE n = n - 1
         COLOR 11, 7: LOCATE , 2: PRINT USING " & ³&"; Sto.Num; Sto.Nam;
         IF r - 1 < 3 THEN CALL BkScroll(2, 3, 32, 47, 1) ELSE r = r - 1
    CASE CHR$(0) + "I"  '===============PgUp==
         b = n - r - 42: IF b < 1 THEN b = 1
         GOSUB DispSto: n = b + r - 3
    CASE CHR$(0) + "Q"  '===============PgDn==
         b = n - r + 48: IF b > t - 44 THEN b = t - 44
         GOSUB DispSto: n = b + r - 3
    CASE CHR$(0) + "G"  '===============Home==
         b = 1: GOSUB DispSto: n = r - 2
    CASE CHR$(0) + "O"  '===============End===
         b = t - 44: GOSUB DispSto: n = t + r - 47
    CASE "D"
         SubMenu 0, n$
         ShowSto
    CASE "M"
         SubMenu 0, n$
         GOSUB EditSto
    CASE "E"
         SubMenu 0, n$
         ShowSto
         IF Confirm(0, "") THEN
            s$ = Sto.Num
            GET #6, 100, Sto
            Sto.Num = s$
            PUT #6, n, Sto
         END IF
    CASE "P"
         SubMenu 0, n$
         PrntSto
    END SELECT
    GOTO 500

EditSto:
    ShowSto
509 EntryS 12, 52, 1, e, b, Sto.Div:    IF e THEN 500 ELSE IF b THEN 509
510 EntryS 17, 52, 26, e, b, Sto.Nam:   IF e THEN 500 ELSE IF b THEN 509
511 EntryS 18, 52, 26, e, b, Sto.Add:   IF e THEN 500 ELSE IF b THEN 510
512 EntryS 19, 52, 26, e, b, Sto.Str:   IF e THEN 500 ELSE IF b THEN 511
513 EntryS 20, 52, 17, e, b, Sto.Cty:   IF e THEN 500 ELSE IF b THEN 512
514 EntryU 20, 70, 2, e, b, Sto.Sta:    IF e THEN 500 ELSE IF b THEN 513
515 EntryU 20, 73, 5, e, b, Sto.Zip:    IF e THEN 500 ELSE IF b THEN 514
516 EntryS 25, 52, 16, e, b, Sto.Man:   IF e THEN 500 ELSE IF b THEN 515
517 EntryS 26, 52, 16, e, b, Sto.Acc:   IF e THEN 500 ELSE IF b THEN 516
518 EntryS 27, 52, 16, e, b, Sto.Acm:   IF e THEN 500 ELSE IF b THEN 517
519 EntryS 28, 52, 16, e, b, Sto.Sec:   IF e THEN 500 ELSE IF b THEN 518
520 EntryT 33, 52, e, b, Sto.Tel:       IF e THEN 500 ELSE IF b THEN 519
521 EntryT 34, 52, e, b, Sto.Pho:       IF e THEN 500 ELSE IF b THEN 520
522 EntryD 39, 52, e, b, Sto.Opn:       IF e THEN 500 ELSE IF b THEN 521
523 EntryN 44, 52, 5, 2, e, b, Sto.Stx: IF e THEN 500 ELSE IF b THEN 522
    IF Confirm(0, "") THEN PUT #6, n, Sto ELSE 523
    GOTO 500

DispSto:
    LOCATE 3: COLOR 11, 7
    FOR x = b TO t
       GET #6, x, Sto
       LOCATE , 2: PRINT USING " & ³&"; Sto.Num; Sto.Nam
       IF CSRLIN = 48 THEN EXIT FOR
       NEXT
    RETURN
END SUB

SUB mRecDet ()
    CALL MhDisplayByName(0, 0, "Recode-D", e)
    Statlin "Recode Inventory Detail"

20  EntryU 14, 51, 10, e, b, d$: IF e THEN EXIT SUB ELSE IF b THEN 20
30  EntryU 16, 51, 10, e, b, m$: IF e THEN EXIT SUB ELSE IF b THEN 20
40  EntryU 18, 51, 4, e, b, o$:  IF e THEN EXIT SUB ELSE IF b THEN 30
    IF FindCod(o$) = 0 THEN WinMssg "Code not found": GOTO 40
41  EntryU 20, 51, 4, e, b, c$:  IF e THEN EXIT SUB ELSE IF b THEN 40
    IF FindCod(c$) = 0 THEN WinMssg "Code not found": GOTO 41

    IF Confirm(0, "Recode") = 0 THEN 20
    COLOR 18, 0
    SETINDEX 10, "Sto"
    MOVEFIRST #10
DO: RETRIEVE #10, Inv
    IF m$ = Inv.Mdl AND d$ = Inv.Dis AND o$ = Inv.Cod THEN
       Inv.Cod = c$
       Inv.Typ = Cod.Typ
       Inv.Grp = Cod.Grp
       Inv.Cod = Inv.Cod
       IF MID$(Inv.Rmk, 13, 4) = c$ THEN
          Inv.Rmk = SPACE$(69)
       ELSE
          Inv.Rmk = "Recode from " + o$ + " to " + c$ + DATE$
       END IF
       LPRINT USING "  &"; "Changed old code:"; o$; "to new code"; c$; "for item model"; Inv.Mdl; "Serial"; Inv.Ser; "Store"; Inv.Sto
       UPDATE #10, Inv
    END IF
    MOVENEXT #10
    x& = x& + 1: LOCATE 50, 45: PRINT USING "#######"; x&;
LOOP UNTIL EOF(10)
    LPRINT
    m = FindPrc(d$ + m$ + o$)
    IF m THEN
       GET #3, m, Prc
       LPRINT USING "  &"; "Changed price master old code"; o$; "to new code"; n$; "for distributor"; d$; "modle"; m$
       Prc.Cod = c$
       PUT #3, m, Prc
    END IF

    SortPrc
END SUB

SUB mRecSim ()
    CALL MhDisplayByName(0, 0, "Recode-S", e)
    Statlin "Recode Inventory Simple"
12  EntryU 14, 50, 4, e, b, o$:  IF e THEN EXIT SUB ELSE IF b THEN 12
    IF FindCod(o$) = 0 THEN WinMssg "Code not found": GOTO 12
13  EntryU 16, 50, 4, e, b, c$:  IF e THEN EXIT SUB ELSE IF b THEN 12
    IF FindCod(c$) = 0 THEN WinMssg "Code not found": GOTO 13

    IF Confirm(0, "Recode") = 0 THEN 12
    COLOR 18, 0
    SETINDEX 10, "Sto"
    MOVEFIRST #10
DO: RETRIEVE #10, Inv
    IF Inv.Cod = o$ THEN
       Inv.Cod = c$
       Inv.Typ = Cod.Typ
       Inv.Grp = Cod.Grp
       Inv.Cod = Inv.Cod
       IF MID$(Inv.Rmk, 13, 4) = c$ THEN
          Inv.Rmk = SPACE$(69)
       ELSE
          Inv.Rmk = "Recode from " + o$ + " to " + c$ + DATE$
       END IF
       LPRINT USING "  &"; "Changed old code:"; o$; "to new code"; c$; "for item model"; Inv.Mdl; "Serial"; Inv.Ser; "Store"; Inv.Sto
       UPDATE #10, Inv
    END IF
    MOVENEXT #10
    x& = x& + 1: LOCATE 50, 45: PRINT USING "######"; x&;
LOOP UNTIL EOF(10)
    LPRINT
    FOR x = 1 TO LOF(3) \ LEN(Prc)
    GET #3, x, Prc
    IF o$ = Prc.Cod THEN
       Prc.Cod = c$
       PUT #3, x, Prc
       LPRINT USING "  &"; "Changed price master old code"; o$; "to new code"; Prc.Cod; "for distributor"; Prc.Dis; "modle"; Prc.Mdl
    END IF
    NEXT
    SortPrc
END SUB

SUB PrntCmb ()
    PageNum = 0: HCombo
    FOR x = 1 TO LOF(3) / LEN(Cmb)
        GET #3, x, Cmb
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        IF LineNum > 60 THEN HCombo
        LineNum = LineNum + 1
        LPRINT USING "&   &    &     &     &     &    &   "; Cmb.Cod; Cmb.Des; Cmb.Cde(1); Cmb.Cde(2); Cmb.Cde(3); Cmb.Cde(4); Cmb.Rat;
        IF Cmb.Rat > "   " THEN
           GET #3, VAL(Cmb.Rat), Rat
'           f = 0: ERASE Rate
           FOR z = 1 TO 3
           IF Rat.Mtr(1) > "   " THEN
              LPRINT USING " & &"; Rat.Mtr(z); Rat.Mrt(z); Rat.Wtr(z); Rat.Wrt(z); Rat.Mtr(3); Rat.Mrt(3); Rat.Wtr(1); Rat.Wrt(1); Rat.Wtr(2); Rat.Wrt(2); Rat.Wtr(3); Rat.Wrt(3)
           ELSE
              EXIT FOR
           END IF
           NEXT
        END IF
        LPRINT
    NEXT
    LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(2) / LEN(Cmb); "COMBOS in the master file."
END SUB

SUB PrntCod ()
    PageNum = 0: HCodes
    FOR x = 1 TO LOF(1) / LEN(Cod)
        GET #1, x, Cod
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        IF LineNum > 60 THEN HCodes
        LineNum = LineNum + 1
        LPRINT USING "&  &    &   &   &   &  &    &    &    &  "; Cod.Cod; Cod.Typ; Cod.Grp; Cod.Des; Cod.Cmb; Cod.Fmv; Cod.Dep; Cod.Dtr; Cod.Dfm; Cod.Rat;
        IF Cod.Rat > "   " THEN
           GET #4, VAL(Cod.Rat), Rat
           FOR z = 1 TO 3
           IF Rat.Mtr(z) > "   " THEN
              LPRINT USING " & &"; Rat.Mtr(z); Rat.Mrt(z); Rat.Wtr(z); Rat.Wrt(z); Rat.Mtr(3); Rat.Mrt(3); Rat.Wtr(1); Rat.Wrt(1); Rat.Wtr(2); Rat.Wrt(2); Rat.Wtr(3); Rat.Wrt(3)
           ELSE
              EXIT FOR
           END IF
           NEXT
        END IF
        LPRINT
    NEXT
    LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(1) / LEN(Cod); "CODES in the master file."
END SUB

DEFLNG L
SUB PrntCtr ()
    PageNum = 0: HCenter
    FOR x = 1 TO LOF(5) / LEN(Ctr)
        GET #7, x, Ctr
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        IF Ctr.Nam > "!" THEN
           IF LineNum > 62 THEN HCenter
           LineNum = LineNum + 1: L0 = L0 + 1
           LPRINT USING "& "; Ctr.Num; Ctr.Nam; Ctr.Str; Ctr.Cty; Ctr.Sta;
           LPRINT Ctr.Zip;
           LPRINT USING "#######.##"; Ctr.Mbb / 100; Ctr.Mam / 100; Ctr.Mpd / 100; (Ctr.Mbb + Ctr.Mam - Ctr.Mpd) / 100;
           LPRINT USING "#######.##"; Ctr.Ybb / 100; Ctr.Yam / 100; Ctr.Ypd / 100; (Ctr.Ybb + Ctr.Yam - Ctr.Ypd) / 100
           l1 = l1 + Ctr.Mbb: L2 = L2 + Ctr.Mam: L3 = L3 + Ctr.Mpd
           L4 = L4 + Ctr.Ybb: L5 = L5 + Ctr.Yam: L6 = L6 + Ctr.Ypd
        END IF
    NEXT
    LPRINT : LPRINT "Printed"; L0; "out of"; LOF(6) / LEN(Ctr); "CENTERS in the master file."; TAB(84);
    LPRINT USING "#######.##"; l1 / 100; L2 / 100; L3 / 100; (l1 + L2 - L3) / 100;
    LPRINT USING "#######.##"; L4 / 100; L5 / 100; L6 / 100; (L4 + L5 - L6) / 100
END SUB

DEFINT L
SUB PrntPas ()
    PageNum = 0: HWords
    FOR x = 1 TO LOF(5) \ LEN(Pas)
        GET #5, x, Pas
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        IF LineNum > 60 THEN HWords
        LineNum = LineNum + 1
        LPRINT USING "  &   &    &       &"; Pas.Ini; Pas.Pas; Pas.Nam; Pas.Lvl
    NEXT
    LPRINT : LPRINT "Printed"; x - 1; "out of"; LOF(5) / LEN(Pas); "PASSWORDS in the master file."
END SUB

SUB PrntSto ()

    PageNum = 0: HStores
    FOR x = 1 TO LOF(6) / LEN(Sto)
        GET #6, x, Sto
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
END SUB

DEFLNG L
DEFSNG A-K, M-Q, S-Z
SUB ShowAmt (r, l1, L2)
    LOCATE r, 53: PRINT USING "######,.##"; l1 / 100;
    LOCATE r, 68: PRINT USING "######,.##"; L2 / 100;
END SUB

DEFSNG L, R
SUB ShowCtr ()
    qPrint Ctr.Num, 6, 52, 0, 1
    qPrint Ctr.Nam, 11, 52, 0, 1
    qPrint Ctr.Str, 12, 52, 0, 1
    qPrint Ctr.Cty + " " + Ctr.Sta + " " + Ctr.Zip, 13, 52, 0, 1
    qPrint Ctr.Tel, 18, 52, 0, 1
    qPrint Ctr.Trm, 23, 52, 0, 1
    qPrint Ctr.Typ, 28, 52, 0, 1
    ShowAmt 37, Ctr.Mbb, Ctr.Ybb
    ShowAmt 38, Ctr.Mam, Ctr.Yam
    ShowAmt 40, Ctr.Mbb + Ctr.Mam, Ctr.Ybb + Ctr.Yam
    ShowAmt 41, Ctr.Mpd, Ctr.Ypd
    ShowAmt 43, Ctr.Mbb + Ctr.Mam - Ctr.Mpd, Ctr.Ybb + Ctr.Yam - Ctr.Ypd
END SUB

SUB ShowSto ()
    qPrint Sto.Num, 6, 52, 0, 1
    qPrint Sto.Div, 12, 52, 0, 1
    qPrint Sto.Nam, 17, 52, 0, 1
    qPrint Sto.Add, 18, 52, 0, 1
    qPrint Sto.Str, 19, 52, 0, 1
    qPrint Sto.Cty, 20, 52, 0, 1
    qPrint Sto.Sta, 20, 70, 0, 1
    qPrint Sto.Zip, 20, 73, 0, 1
    qPrint Sto.Man, 25, 52, 0, 1
    qPrint Sto.Acc, 26, 52, 0, 1
    qPrint Sto.Acm, 27, 52, 0, 1
    qPrint Sto.Sec, 28, 52, 0, 1
    qPrint Sto.Tel, 33, 52, 0, 1
    qPrint Sto.Pho, 34, 52, 0, 1
    qPrint Sto.Opn, 39, 52, 0, 1
    qPrint Sto.Stx, 44, 52, 0, 1
END SUB

SUB SortCmb ()
    CLOSE
    SHELL "SortF ..\Master\Combos.Mas ..\Master\Combo /L49 /+1,4 /Q"
    KILL "..\Master\Combos.Mas"
    NAME "..\Master\Combo" AS "..\Master\Combos.Mas"
    OpenFiles
END SUB

SUB SortCod ()
    CLOSE
    SHELL "SortF ..\Master\Codes.Mas ..\Master\Codes.Cmb /L50 /+36,2 /+1,4 /Q"
    SHELL "SortF ..\Master\Codes.Cmb ..\Master\Codes.Mas /L50 /+1,4 /Q"
    OpenFiles
END SUB

SUB SortPas ()
    CLOSE
    SHELL "SortF ..\Master\Password.Nam ..\Master\Password.Mas /L27 /+1,4 /Q"
    SHELL "SortF ..\Master\Password.Mas ..\Master\Password.Nam /L27 /+5,19 /C /Q"
    OpenFiles
END SUB

SUB SortPrc ()
    CLOSE
    SHELL "SortF ..\Master\Prices.Mas ..\Master\Prices /L70 /+5,20 /+1,4 /Q"
    KILL "..\Master\Prices.Mas"
    NAME "..\Master\Prices" AS "..\Master\Prices.Mas"
    OpenFiles
END SUB

SUB SortRat ()
    CLOSE
    SHELL "sortf ..\Master\Rates.Inx ..\Master\Rates.Mas /L58 /+1,3 /Q"
    SHELL "sortf ..\Master\Rates.Mas ..\Master\Rates.Inx /L58 /+5,54/Q"
    OpenFiles
END SUB

