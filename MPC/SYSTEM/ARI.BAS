DEFINT A-Z: DEFDBL T: DEFLNG L

DECLARE SUB HiLite (m, p)
DECLARE SUB MatShow ()
DECLARE SUB MatSort ()
DECLARE SUB MatEdit (r, n)
DECLARE SUB MatDisp (x)
DECLARE SUB MatLine (r, L1, L2, L3, L4)

DECLARE SUB OpenFiles ()
DECLARE SUB TimeDate ()
DECLARE SUB Monitor ()
DECLARE SUB KeyHit (f$)
DECLARE SUB HitKey (f$)
DECLARE SUB WinMssg (f$)
DECLARE SUB HMaterial ()
DECLARE SUB Heading ()

DECLARE SUB Picture (r, c, s, f$)
DECLARE SUB EntryS (r, c, s, e, b, f$)
DECLARE SUB EntryU (r, c, s, e, b, f$)
DECLARE SUB EntryN (r, c, s, D, e, b, f$)

'$INCLUDE: '..\Include\Material.REC'

   'record variables
    DIM SHARED Mat AS Material
    DIM SHARED Mnx AS MatIndex
   'common variables
    DIM SHARED LineNum AS INTEGER
    DIM SHARED PageNum AS INTEGER
    DIM SHARED Segment AS INTEGER
    DIM SHARED Sort    AS INTEGER
    DIM SHARED Top     AS INTEGER
    DIM SHARED Bot     AS INTEGER
    DIM SHARED Menu(6, 6) AS INTEGER

    ON ERROR GOTO Errors
    Monitor
    FOR y = 1 TO 5: FOR x = 1 TO 2: READ Menu(y, x): NEXT x, y

Materials: '=================================================================
    OpenFiles
    WIDTH 80, 50
    DEF SEG = Segment: BLOAD "MatList.Scr", 0: DEF SEG
    Bot = 1: Top = LOF(2) \ LEN(Mnx): MatDisp x
    r = 3: n = 1: m = 4: HiLite m, 1
600 IF n > 0 THEN GET #2, n, Mnx: GET #1, Mnx.Num, Mat
    COLOR 0, 7: LOCATE r, 2: PRINT USING "&≥&≥&≥&≥&≥&≥ &≥&≥ & ≥ &"; Mat.Num; Mnx.Typ; Mnx.Nam; Mat.Fra; Mat.Mes; Mat.Con; Mat.Ref; Mat.Prc; Mat.Rpt; Mat.Rqt
    KeyHit n$
    COLOR 2, 0: LOCATE r, 2: PRINT USING "&≥&≥&≥&≥&≥&≥ &≥&≥ & ≥ &"; Mat.Num; Mnx.Typ; Mnx.Nam; Mat.Fra; Mat.Mes; Mat.Con; Mat.Ref; Mat.Prc; Mat.Rpt; Mat.Rqt
    IF n$ = CHR$(13) THEN n$ = MID$(" Add   Modify   Erase   Display   List ", 1 + Menu(m, 1), 1)
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        CLOSE : RUN "ARP.EXE"
    CASE CHR$(0) + "M" 'right
        HiLite m, 0
        m = m + 1: IF m > 5 THEN m = 1
        HiLite m, 1
    CASE CHR$(0) + "K" 'left
        HiLite m, 0
        m = m - 1: IF m < 1 THEN m = 5
        HiLite m, 1
    CASE CHR$(0) + "P" 'Dn
        IF n + 1 > Top THEN 600 ELSE n = n + 1
        IF r + 1 > 48 THEN CALL scroll(2, 3, 79, 48, 1) ELSE r = r + 1
    CASE CHR$(0) + "H" 'Up
        IF n - 1 < 1 THEN 600 ELSE n = n - 1
        IF r - 1 < 3 THEN CALL BkScroll(2, 3, 79, 48, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 43
        IF Bot < 1 THEN Bot = 1
        MatDisp x: n = x + r - 48
    CASE CHR$(0) + "Q" 'PgDn
        Bot = n - r + 49
        IF Bot + 46 > Top THEN Bot = Top - 45: IF Bot < 1 THEN Bot = 1
        MatDisp x: n = x + r - 48
    CASE CHR$(0) + "G" 'Home
        Bot = 1: MatDisp x: n = r - 2
    CASE CHR$(0) + "O" 'End
        Bot = Top - 45: IF Bot < 1 THEN Bot = 1
        MatDisp x: n = Bot + r - 3
    CASE "L"
        HiLite m, 0
        m = 5: HiLite m, 1
        PageNum = 0: HMaterial
        FOR y = 1 TO LOF(1) \ LEN(Mat)
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #2, y, Mnx
        GET #1, Mnx.Num, Mat
        IF Mat.Nam > "!" THEN
           LineNum = LineNum + 1: IF LineNum > 60 THEN HMaterial
           LPRINT USING "   &"; Mat.Typ; Mat.Num; Mat.Nam; Mat.Fra; Mat.Mes; Mat.Con;
           LPRINT USING "       &     "; Mat.Ref; Mat.Prc; Mat.Rpt; Mat.Rqt
        END IF
        NEXT
    CASE "E"
        HiLite m, 0: m = 3: HiLite m, 1
        L1 = Mat.Ybp + Mat.Ypp + Mat.Ysp + Mat.Ykp + Mat.Ywp
        IF L1 <> 0 THEN
           WinMssg "Active item, no erase"
        ELSE
           WinMssg "Hit [*] to erase": KeyHit f$
           IF f$ = "*" THEN
              Mnx.Typ = "˛˛˛"
              Mnx.Nam = "˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛"
              PUT #1, Mnx.Num
              PUT #1, Mnx.Num, Mat.Num
              PUT #2, n, Mnx
              MatSort
              MatDisp x
           END IF
        END IF
    CASE "D"
        HiLite m, 0
        m = 4: HiLite m, 1
        MatShow
    CASE "A"
        HiLite m, 0: m = 1: HiLite m, 1: y = 0
        FOR z = 1 TO Top: GET #2, z, Mnx                 'see if a deleted
        IF LEFT$(Mnx.Nam, 1) = "˛" THEN y = z: EXIT FOR  'record is found
        NEXT                                             'in master file
        IF y = 0 THEN Mnx.Num = 1 + LOF(1) \ LEN(Mat): y = Mnx.Num 'append if not found
        IF Mnx.Num > 999 THEN
           WinMssg "Inventory Master is full"
        ELSE
           GET #1, Mnx.Num, Mat
           COLOR 2, 0: LOCATE r, 2: PRINT USING "###≥   ≥                          ≥    ≥  ≥  ≥        ≥        ≥       ≥      "; Mnx.Num
           MatEdit r, y
        END IF
    CASE "M"
        HiLite m, 0: m = 2: HiLite m, 1
        MatEdit r, n
    CASE ELSE
        WinMssg "Select from highlited keys"
    END SELECT
    GOTO 600
Errors: '====================================================================
    WinMssg "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
    KeyHit f$
    CLOSE
    RUN "ARP"

DATA 1,5, 7,8, 16,7, 24,9, 34,6

DEFSNG A-Z
SUB Heading ()
     LPRINT CHR$(12); CHR$(27); "@";
     WIDTH "LPT1:", 255: LPRINT CHR$(27); "M"
     PageNum = PageNum + 1: LineNum = 13
     LPRINT "Date: "; DATE$; TAB(61); "       McKENRY PRODUCE COMPANY        "; TAB(153); "Page: "; PageNum
     LPRINT "Time: "; TIME$; TAB(61); "707 Willow Avenue, Knoxville, TN 37915"
     LPRINT :         LPRINT TAB(69); "INVENTORY  PURCHASING": LPRINT
END SUB

DEFINT A-Z
SUB HiLite (m, p)
    IF p THEN f = 4: b = 7 ELSE f = 14: b = 1
    LOCATE 50, Menu(m, 1) + 40
    f$ = MID$(" Add   Modify   Erase   Display   List ", Menu(m, 1), Menu(m, 2))
    COLOR f, b: PRINT MID$(f$, 1, 2);
    COLOR f - 3, b: PRINT MID$(f$, 3);
END SUB

DEFSNG A-Z
SUB HitKey (f$)
    DO: f$ = INKEY$: LOOP WHILE f$ <> ""
    DO: f$ = INKEY$: TimeDate: LOOP WHILE f$ = ""
END SUB

SUB HMaterial ()
    Heading
    LPRINT TAB(70); " INVENTORY LISTING ": LPRINT : LPRINT
    LPRINT "  Type   Num          Description           Units  Measure  Container  Reference Cost  Reorder Point  Reorder Quantity"
    LPRINT "  ====   ===   ==========================   =====  =======  =========  ==============  =============  ================"
END SUB

SUB KeyHit (f$)
    DO: f$ = INKEY$: LOOP WHILE f$ = ""
    f$ = UCASE$(f$)
END SUB

DEFINT A-Z
SUB MatDisp (x)
    IF Bot > 0 AND Top > 0 THEN
       LOCATE 3: COLOR 2, 0
       FOR x = Bot TO Top
       GET #2, x, Mnx: GET #1, Mnx.Num, Mat
       LOCATE , 2: PRINT USING "&≥&≥&≥&≥&≥&≥ &≥&≥ & ≥ &"; Mat.Num; Mnx.Typ; Mnx.Nam; Mat.Fra; Mat.Mes; Mat.Con; Mat.Ref; Mat.Prc; Mat.Rpt; Mat.Rqt
       IF CSRLIN = 49 THEN EXIT FOR
       NEXT
    ELSE Bot = 1
    END IF
END SUB

SUB MatEdit (r, n)
    p$ = Mnx.Typ + Mnx.Nam
611 EntryU r, 6, 3, e, b, Mat.Typ:     IF e THEN 620 ELSE IF b THEN 611
    IF Mat.Typ < "!" THEN WinMssg "Type please": GOTO 611
612 EntryS r, 10, 26, e, b, Mat.Nam:   IF e THEN 620 ELSE IF b THEN 611
    IF Mat.Nam < "!" THEN WinMssg "Name must not be blank": GOTO 612
613 EntryN r, 37, 4, 0, e, b, Mat.Fra: IF e THEN 620 ELSE IF b THEN 612
    IF Mat.Fra < "   1" THEN WinMssg "Fraction must not be zero": GOTO 613
614 EntryS r, 42, 2, e, b, Mat.Mes:    IF e THEN 620 ELSE IF b THEN 613
    IF Mat.Mes < "!" THEN WinMssg "How about a measure": GOTO 614
615 EntryS r, 45, 2, e, b, Mat.Con:    IF e THEN 620 ELSE IF b THEN 614
    IF Mat.Con < "!" THEN WinMssg "Dont you want a container": GOTO 615
616 EntryN r, 49, 7, 3, e, b, Mat.Ref: IF e THEN 620 ELSE IF b THEN 615
    IF Mat.Ref < "   .001" THEN WinMssg "I must have a price": GOTO 616
617 EntryN r, 57, 8, 4, e, b, Mat.Prc: IF e THEN 620 ELSE IF b THEN 616
    IF Mat.Prc < "   .01" THEN WinMssg "I must have a price": GOTO 617
618 EntryN r, 67, 5, 0, e, b, Mat.Rpt: IF e THEN 620 ELSE IF b THEN 617
619 EntryN r, 75, 5, 0, e, b, Mat.Rqt: IF e THEN 620 ELSE IF b THEN 618
    WinMssg "Hit [*] to save": KeyHit f$
    IF f$ = "*" THEN
       Mat.Num = RIGHT$(STR$(1000 + Mnx.Num), 3)
       Mnx.Typ = Mat.Typ
       Mnx.Nam = Mat.Nam
       PUT #2, n, Mnx
       PUT #1, Mnx.Num, Mat
       IF p$ <> Mnx.Typ + Mnx.Nam THEN
          MatSort
          MatDisp x
          n = r - 2
          Top = LOF(1) \ LEN(Mat)
       END IF
    ELSE
       GOTO 611
    END IF
620
END SUB

DEFLNG L
DEFSNG A-K, M-Q, S-Z
SUB MatLine (r, L1, L2, L3, L4)
    LOCATE r, 21: PRINT USING "#######.##"; L1 / 100;
    LOCATE r, 32: PRINT USING "#######.##"; L2 / 100;
    LOCATE r, 43: IF L1 <> 0 THEN PRINT USING "###.##"; L2 / L1;  ELSE PRINT "      ";
    LOCATE r, 52: PRINT USING "#######.##"; L3 / 100;
    LOCATE r, 63: PRINT USING "#######.##"; L4 / 100;
    LOCATE r, 74: IF L3 <> 0 THEN PRINT USING "###.##"; L4 / L3;  ELSE PRINT "      ";
END SUB

DEFSNG L, R
SUB MatShow ()
    SCREEN , , 1, 0
    PCOPY 0, 1
    DEF SEG = Segment: BLOAD "Material.SCR", 9536: DEF SEG
    COLOR 14, 6
    LOCATE 10, 14: PRINT Mat.Typ; "-"; Mat.Num
    LOCATE 12, 14: PRINT Mat.Nam
    LOCATE 14, 14: PRINT LTRIM$(Mat.Fra); " "; RTRIM$(Mat.Mes); "/"; Mat.Con
    LOCATE 10, 70: PRINT Mat.Ref; "/"; Mat.Mes
    LOCATE 11, 69: PRINT Mat.Prc; "/"; Mat.Mes
    LOCATE 13, 70: PRINT Mat.Rpt; " "; Mat.Mes
    LOCATE 14, 70: PRINT Mat.Rqt; " "; Mat.Mes
  
    COLOR , 6: MatLine 21, Mat.Msq, Mat.Msp, Mat.Ysq, Mat.Ysp
    COLOR , 6: MatLine 22, Mat.Msq, Mat.Msc, Mat.Ysq, Mat.Ysc
    COLOR , 0: MatLine 24, Mat.Msq, Mat.Msp - Mat.Msc, Mat.Ysq, Mat.Ysp - Mat.Ysc
  
    COLOR , 6: MatLine 31, Mat.Mbq, Mat.Mbp, Mat.Ybq, Mat.Ybp
    COLOR , 6: MatLine 32, Mat.Mpq, Mat.Mpp, Mat.Ypq, Mat.Ypp
    COLOR , 6: MatLine 33, Mat.Mmq, Mat.Mmp, Mat.Ymq, Mat.Ymp
    COLOR , 0: MatLine 35, Mat.Mbq + Mat.Mpq + Mat.Mmq, Mat.Mbp + Mat.Mpp + Mat.Mmp, Mat.Ybq + Mat.Ypq + Mat.Ymq, Mat.Ybp + Mat.Ypp + Mat.Ymp
    COLOR , 6: MatLine 37, Mat.Msq, Mat.Msc, Mat.Ysq, Mat.Ysc
    COLOR , 6: MatLine 38, Mat.Mkq, Mat.Mkp, Mat.Ykq, Mat.Ykp
    COLOR , 6: MatLine 39, Mat.Mwq, Mat.Mwp, Mat.Ywq, Mat.Ywp
    COLOR , 6: MatLine 40, 0, 0, Mat.Ygq, Mat.Ygp
    COLOR , 0: MatLine 42, Mat.Mkq + Mat.Msq + Mat.Mwq, Mat.Mkp + Mat.Msc + Mat.Mwp, Mat.Ykq + Mat.Ysq + Mat.Ywq + Mat.Ygq, Mat.Ykp + Mat.Ysc + Mat.Ywp + Mat.Ygp
    COLOR , 6: MatLine 44, Mat.Mbq + Mat.Mpq + Mat.Mmq - Mat.Mkq - Mat.Msq - Mat.Mwq, Mat.Mbp + Mat.Mpp + Mat.Mmp - Mat.Mkp - Mat.Msc - Mat.Mwp, Mat.Ybq + Mat.Ypq + Mat.Ymq - Mat.Ykq - Mat.Ysq - Mat.Ywq - Mat.Ygq, Mat.Ybp + Mat.Ypp + Mat.Ymp - Mat. _
Ysc - Mat.Ykp - Mat.Ywp - Mat.Ygp
    SCREEN , , 1, 1
    KeyHit f$: SCREEN , , 0, 0
END SUB

SUB MatSort ()
    CLOSE
    SHELL "AC.COM ..\Data\Material.Inx ..\Data\Material /L31 /+1,29 /Q"
    KILL "..\Data\Material.Inx"
    NAME "..\Data\Material" AS "..\Data\Material.INX"
    OpenFiles
END SUB

SUB Monitor ()
    DEF SEG = 0
       Segment = &HB800
    DEF SEG
    KEY 1, "1": KEY 2, "2": KEY 3, "3": KEY 4, "4": KEY 5, "5"
    KEY 6, "6": KEY 7, "7": KEY 8, "8": KEY 9, "9": KEY 10, "0"
END SUB

SUB OpenFiles ()
    CLOSE
    OPEN "..\Data\Material.MAS" FOR RANDOM LOCK READ WRITE AS #1 LEN = LEN(Mat)
    OPEN "..\Data\Material.INX" FOR RANDOM LOCK READ WRITE AS #2 LEN = LEN(Mnx)
END SUB

SUB TimeDate ()
 COLOR 3, 0
 LOCATE 10, 22, 0: PRINT " "; DATE$; " "
 t$ = " " + LEFT$(TIME$, 2)
 IF TIME$ > "11:59:59" THEN f$ = "pm " ELSE f$ = "am "
 IF TIME$ > "12:59:59" THEN t$ = " " + STR$(VAL(t$) - 12)
 LOCATE 10, 47: PRINT t$; MID$(TIME$, 3, 6) + f$
END SUB

SUB WinMssg (f$)
    SCREEN , , 1, 1
    PCOPY 0, 1
    COLOR 14, 4: LOCATE 12
    LOCATE , 36: PRINT " …Õ"; STRING$(LEN(f$), "Õ"); "Õª "
    LOCATE , 36: PRINT " ∫ "; f$; " ∫ "
    LOCATE , 36: PRINT " »Õ"; STRING$(LEN(f$), "Õ"); "Õº "
    COLOR 12, 0: LOCATE 13
    LOCATE , 35: PRINT "€"
    LOCATE , 35: PRINT "€"
    LOCATE , 35: PRINT "ﬂﬂﬂ"; STRING$(LEN(f$) + 2, "ﬂ")
    SOUND 770, .1: SOUND 970, .2: SLEEP 0
    SCREEN , , 0, 0
END SUB

