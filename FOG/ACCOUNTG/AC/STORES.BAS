DEFINT A-Z: DEFLNG L
DECLARE FUNCTION Confirm (f$)
DECLARE SUB OpenFiles ()
DECLARE SUB DispSto (x)
DECLARE SUB ShowSto ()
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

REM $INCLUDE: 'Stores.REC'

    DIM SHARED Sto      AS Stores
    
    DIM SHARED Top      AS INTEGER
    DIM SHARED Bot      AS INTEGER

    ON ERROR GOTO Errors
    SCREEN 0, 0, 0, 0: WIDTH 80, 50
    SCREEN 0, 0, 1, 1: PCOPY 0, 1
    SCREEN 0, 0, 0, 0
    CALL MhOpenScreenLib("STORES.SLB" + CHR$(0), e)
    CALL MhBlinkOff
    CALL MhShadowOn(0, 8)

Store: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    OpenFiles
    CALL MhDisplayByName(0, 0, "Stores", e)
    Bot = 1: Top = LOF(1) / LEN(Sto): DispSto x
    r = 2: n = 1
500 IF n > 0 THEN GET #1, n, Sto
    COLOR 11, 1: LOCATE r, 3: PRINT USING "&ณ&"; Sto.Num; Sto.Nam;
    KeyPres n$
    COLOR 1, 3:  LOCATE r, 3: PRINT USING "&ณ&"; Sto.Num; Sto.Nam;
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        GOTO Terminate
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
        x = VAL(Sto.Num): IF x = 0 THEN BEEP: GOTO 505 ELSE GET #1, x, Sto
        Bot = x - r + 2
        IF Bot > Top - 21 THEN Bot = Top - 21: r = 23 - Top + x ELSE IF Bot < 1 THEN Bot = 1: r = x + 1
        n = Bot + r - 2
506     ShowSto
        DispSto x
    CASE "D"
        ShowSto
    CASE "E"
        ShowSto
        Sto.Div = SPACE$(1)
        Sto.Nam = SPACE$(26)
        Sto.Add = SPACE$(26)
        Sto.Str = SPACE$(26)
        Sto.Cty = SPACE$(17)
        Sto.Sta = SPACE$(2)
        Sto.Zip = SPACE$(5)
        Sto.Cod = SPACE$(3)
        Sto.AAA = SPACE$(1)
        Sto.Tel = SPACE$(12)
        Sto.BBB = SPACE$(4)
        Sto.Pho = SPACE$(12)
        Sto.Opn = SPACE$(10)
        Sto.Man = SPACE$(16)
        Sto.Acc = SPACE$(16)
        Sto.Acm = SPACE$(16)
        Sto.Sec = SPACE$(16)
        Sto.Stx = SPACE$(5)
        Sto.CCC = SPACE$(16)
        IF Confirm("ERASE Store") THEN PUT #1, n, Sto

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
        IF Confirm("SAVE Store") THEN PUT #1, n, Sto ELSE 524
    END SELECT
    GOTO 500
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    SELECT CASE ERR
    CASE 70
       WinMssg "Master Files in use": GOTO Store
    CASE ELSE
       WinMssg "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
    END SELECT
    GOTO Store
Terminate: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    CLOSE
    END

DEFINT L
FUNCTION Confirm (f$)
    PCOPY 0, 2
    COLOR 14, 4: LOCATE 12
    LOCATE , 36: PRINT "            " + SPACE$(LEN(f$))
    LOCATE , 36: PRINT " PRESS * to " + f$
    LOCATE , 36: PRINT "            " + SPACE$(LEN(f$))
    COLOR 12, 3: LOCATE 13
    LOCATE , 35: PRINT "Û"
    LOCATE , 35: PRINT "Û"
    LOCATE , 35: PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛ" + STRING$(LEN(f$), "Û");
    SOUND 880, 1: SOUND 770, 2: KeyPres f$
    IF f$ = "*" THEN Confirm = 1
    PCOPY 2, 0
END FUNCTION

SUB DispSto (x)
    IF Bot > 0 AND Top > 0 THEN
       LOCATE 2: COLOR 1, 3
       FOR x = Bot TO Top
       GET #1, x, Sto
       LOCATE , 3: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
       IF CSRLIN = 24 THEN EXIT FOR
       NEXT
    ELSE Bot = 1
    END IF
END SUB

DEFSNG A-Z
SUB KeyPres (f$)
    DO: LOOP WHILE INKEY$ > ""
    DO: f$ = INKEY$: LOOP WHILE f$ = ""
    f$ = UCASE$(f$)
END SUB

SUB OpenFiles ()
    CLOSE : OPEN "Stores.Mas" FOR RANDOM LOCK WRITE AS #1 LEN = LEN(Sto)
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
    SOUND 60, 1: SOUND 80, 1: KeyPres f$
    PCOPY 2, 0
END SUB

