DEFINT A-Z: DEFLNG L
DECLARE FUNCTION TraStat$ (f$)

DECLARE FUNCTION Confirm% (g%, f$)
DECLARE FUNCTION FindCmb% (f$)
DECLARE FUNCTION FindCod% (f$)
DECLARE FUNCTION FindIni% (f$)
DECLARE FUNCTION FindPas% (f$)
DECLARE FUNCTION FindPrc% (f$)
DECLARE FUNCTION WinItem% (c%, i%)
DECLARE FUNCTION WinCntr% ()
DECLARE FUNCTION WinComb% ()
DECLARE FUNCTION WinPric% ()
DECLARE FUNCTION WinStor% ()
DECLARE FUNCTION WinAgrt% ()
DECLARE FUNCTION WinCust% ()
DECLARE FUNCTION WinRcts% ()

DECLARE SUB WinMssg (f$)
DECLARE SUB FindGrp (f$)
DECLARE SUB KeyPres (f$)
DECLARE SUB OpenFiles ()

DECLARE SUB mMasCol ()
DECLARE SUB mMasRat ()
DECLARE SUB mMasPas ()
DECLARE SUB mMasSto ()
DECLARE SUB mMasCtr ()
DECLARE SUB mMasCmb ()
DECLARE SUB mMasPrc ()
DECLARE SUB mMasCod ()
DECLARE SUB mMasChk ()
DECLARE SUB mMasCus ()
DECLARE SUB mMasInv ()
DECLARE SUB mAgrMas ()
DECLARE SUB mRecSim ()
DECLARE SUB mRecDet ()

DECLARE SUB mAudRpt (s)
DECLARE SUB mExtRpt (s)
DECLARE SUB mStaRpt (s)
DECLARE SUB mInvPrc ()
DECLARE SUB mAgeRpt (s)

DECLARE SUB CusEdit ()
DECLARE SUB Statlin (f$)

DECLARE SUB mTA ()
DECLARE SUB mTM ()

'$INCLUDE: '..\Include\Config.off'

'   ON ERROR GOTO Errors

'   y = SETMEM(-16384)   ' SETMEM in MS-DOS session under
                         ' Windows
    
    SCREEN 0, 0, 0, 0: WIDTH 80, 50
    SCREEN 0, 0, 1, 1: PCOPY 0, 1
    SCREEN 0, 0, 0, 0


    CALL MhOpenScreenLib("RTS.SLB" + CHR$(0), e)
    CALL MhBlinkOff
    CALL MhShadowOff

    DIM Menu(6, 2)     AS STRING
    DIM Loct(6, 2)     AS INTEGER
    DIM Subm(6, 22, 2) AS STRING
    DIM HiLi(6, 22)    AS INTEGER

    FOR m = 1 TO 6
    READ Menu(m, 1), Menu(m, 2), Loct(m, 1), Loct(m, 2)
    FOR c = 1 TO Loct(m, 2)
    READ Subm(m, c, 1), Subm(m, c, 2), HiLi(m, c)
    NEXT
    NEXT

    IF LEN(DIR$("InvInUse")) = 0 THEN       ' Ensure Inventory is not in use
       OPEN "InvInUse" FOR RANDOM AS #1: CLOSE
    ELSE
       WinMssg "Inventory in use, denied": END
    END IF
    OpenFiles
    
    m = 1: c = 1
10  IF c > Loct(m, 2) THEN c = Loct(m, 2)
    CALL MhDisplayByName(0, 0, "Logo", e)
    CALL MhDisplayByName(0, 0, Menu(m, 1), e)
20  COLOR 27, 0: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
    COLOR 28, 0: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
    KeyPres f$
    
30  SELECT CASE f$
    CASE CHR$(27): KILL "InvInUse": END
    CASE CHR$(0) + "M"
         m = m + 1: IF m > 6 THEN m = 1
         GOTO 10
    CASE CHR$(0) + "K"
         m = m - 1: IF m < 1 THEN m = 6
         GOTO 10
    CASE CHR$(0) + "P"
         COLOR 17, 6: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
         COLOR 20, 6: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
         c = c + 1: IF c > Loct(m, 2) THEN c = 1
    CASE CHR$(0) + "H"
         COLOR 17, 6: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
         COLOR 20, 6: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
         c = c - 1: IF c < 1 THEN c = Loct(m, 2)
    CASE CHR$(13)
         COLOR 17, 6: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
         COLOR 20, 6: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);

         SELECT CASE m
              CASE 1
                   SELECT CASE c
                        CASE 1:  CALL mMasCod
                        CASE 2:  CALL mMasCmb
                        CASE 3:  CALL mMasPrc
                        CASE 4:  CALL mMasRat
                        CASE 5:  CALL mMasPas
                        CASE 6:  CALL mMasSto
                        CASE 7:  CALL mMasCtr
                        CASE 8:  CALL mMasChk
                   END SELECT
              CASE 2
                   SELECT CASE c
                        CASE 1:  CALL mTA
                        CASE 2:  CALL mAgrMas
                   END SELECT
              CASE 3
                   SELECT CASE c
                   END SELECT
              CASE 4
                   SELECT CASE c
                        CASE 1:  CALL mTM
                        CASE 2:  CALL mMasInv
                        CASE 3:  CALL mRecSim
                        CASE 4:  CALL mRecDet
                   END SELECT
              CASE 5
                   SELECT CASE c
                        CASE 1:  CALL mAudRpt(1)
                        CASE 2:  CALL mExtRpt(1)
                        CASE 3:  CALL mStaRpt(1)
                        CASE 4:  CALL mAgeRpt(1)
                        CASE 5:  CALL mAudRpt(0)
                        CASE 6:  CALL mExtRpt(0)
                        CASE 7:  CALL mStaRpt(0)
                        CASE 8:  CALL mAgeRpt(0)
                        CASE 9:  CALL mInvPrc
                   END SELECT
              CASE 6
                   SELECT CASE c
                   END SELECT
         END SELECT
         GOTO 10
    CASE ELSE: FOR x = 1 TO Loct(m, 2)
         IF f$ = Subm(m, x, 2) THEN c = x: f$ = CHR$(13): GOTO 30
         NEXT
    END SELECT
    GOTO 20
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    PCOPY 0, 3: CALL MhDisplayByName(0, 0, "ErrMessage", e)
    SELECT CASE ERR
           CASE 200:  Er$ = "Unable to find code master"
           CASE 201:  Er$ = "Unable to find user password"
           CASE 203:  Er$ = "Receipt rent exceeds $999.99"
           CASE 204:  Er$ = "Late charges exceeds $99.99"
           CASE 205:  Er$ = "Not eligible for reinstatment"
           CASE 206:  Er$ = "Invalid agreement number "
           CASE 207:  Er$ = "Store has item as active"
           CASE 208:  Er$ = "Printer {1} is not ready"
           CASE 209:  Er$ = "Printer {2} is not ready"
           CASE 210:  Er$ = "Invalid status, not 3,4,or 5"
           CASE ELSE: Er$ = ERROR$(ERR)
    END SELECT
    qPrint STR$(ERR) + STR$(ERDEV AND &HFF) + STR$((ERDEV AND &HFF00) \ 256), 32, 38, 0, 1
    qPrint Er$, 33, 38, 0, 1
    qPrint ERDEV$, 35, 38, 0, 1
    SOUND 50, 3: KeyPres f$
    PCOPY 3, 0
    RESUME 10
MenuData: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    DATA "Menu-Mas","",2,8
    DATA " Codes     ","C",3
    DATA " Combos    ","B",6
    DATA " Prices    ","P",3
    DATA " Rates     ","R",3
    DATA " Passwords ","W",7
    DATA " Stores    ","S",3
    DATA " Centers   ","T",6
    DATA " Validate  ","V",3

    DATA "Menu-Agr","Agreement",11,2
    DATA " Record Transactions ","R",12
    DATA " Agreements Master   ","A",12

    DATA "Menu-Rct","Receipt",23,7
    DATA " Issue      ","I",24
    DATA " Display    ","D",24
    DATA " Print      ","P",24
    DATA " Void       ","V",24
    DATA " Bad Check  ","B",24
    DATA " Mark Check ","M",24
    DATA " Cash Sale  ","C",24

    DATA "Menu-Inv","",33,4
    DATA " Record Transactions ","R",34
    DATA " Merchandise Master  ","M",34
    DATA " Simple Recode       ","S",34
    DATA " Detailed Recode     ","D",34

    DATA "Menu-Rpt", "",46,9
    DATA " Audit Report [ONE] ","A",47
    DATA " Extinguished [ONE] ","E",47
    DATA " Codes Status [ONE] ","C",47
    DATA " Aging Report [ONE] ","g",48
    DATA " Audit Report [ALL] ","R",53
    DATA " Extinguished [ALL] ","x",48
    DATA " Codes Status [ALL] ","S",53
    DATA " Aging Report [ALL] ","i",49
    DATA " Merchandise Prices ","M",47
    

    DATA "Menu-End","",55,17
    DATA " Day closing   ","D",56
    DATA " Week closing  ","W",56
    DATA " Month closing ","M",56
    DATA " Year closing  ","Y",56
    DATA " Transactions  ","T",56
    DATA " View activity ","V",56
    DATA " Delinquency   ","D",56
    DATA " Communication ","C",56
    DATA " Special menu  ","S",56
    DATA " Recap weekday ","R",56
    DATA " System date   ","S",56
    DATA " Format disk   ","F",56
    DATA " Purge records ","P",56
    DATA " Backup data   ","B",56
    DATA " Test backup   ","T",56
    DATA " Restore data  ","R",56
    DATA " Terminate     ","T",56

DEFINT L
FUNCTION Confirm (g, f$)
    r = 30
    PCOPY 0, 1
    COLOR 17, 6: LOCATE r + 1, 20: PRINT STRING$(15 + LEN(f$), " ");
    COLOR 17, 6: LOCATE r + 2, 20: PRINT "  Press * to "; f$; "  ";
    COLOR 8, 0: Picture r + 2, POS(0), 1, c$: PRINT c$;
    COLOR 17, 6: LOCATE r + 3, 20: PRINT STRING$(15 + LEN(f$), " ");
    COLOR 8, 0: Picture r + 3, POS(0), 1, c$: PRINT c$;
    COLOR 8, 0: Picture r + 4, 21, 15 + LEN(f$), c$: LOCATE r + 4, 21: PRINT c$;
    SOUND 350, .5: KeyPres f$
    IF f$ = "*" THEN Confirm = 1: SOUND 1200, .3: SOUND 1000, .2
    PCOPY 1, 0
END FUNCTION

FUNCTION FindCmb (f$)
   b = 1: t = LOF(3) \ LEN(Cmb)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #3, m, Cmb
      IF f$ = Cmb.Cod THEN FindCmb = 1: EXIT DO
      IF f$ > Cmb.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindCod (f$)
   b = 1: t = LOF(1) \ LEN(Cod)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #1, m, Cod
      IF f$ = Cod.Cod THEN FindCod = m: EXIT DO
      IF f$ > Cod.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

SUB FindGrp (f$)
   IF f$ > "  " THEN
      b = 1: t = LOF(2) \ LEN(Cod)
      DO UNTIL (t < b)
         m = (t + b) \ 2: GET #2, m, Cod
         IF f$ > Cod.Cmb THEN b = m + 1 ELSE t = m - 1
      LOOP
      DO WHILE f$ > Cod.Cmb AND NOT EOF(2)
         GET #2, , Cod
      LOOP
      qPrint f$, CSRLIN, 9, 0, 1
      DO WHILE f$ = Cod.Cmb
         qPrint Cod.Cod + "ณ" + Cod.Des, CSRLIN, 12, 0, 1
         LOCATE CSRLIN + 1
         IF CSRLIN > 46 THEN KeyPres n$
         GET #2, , Cod
         IF f$ <> Cod.Cmb THEN EXIT DO
      LOOP
   END IF
END SUB

FUNCTION FindIni (f$)
   FOR x = 1 TO LOF(5) \ LEN(Pas)
       GET #5, x, Pas
       IF f$ = Pas.Ini THEN FindIni = x: EXIT FOR
   NEXT
END FUNCTION

FUNCTION FindPas (f$)
   FOR x = 1 TO LOF(5) \ LEN(Pas)
       GET #5, x, Pas
       IF f$ = Pas.Pas THEN FindPas = x: EXIT FOR
   NEXT
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
SUB Heading ()
    WIDTH LPRINT 255            'Supress Line Feed
    'SELECT CASE Laser
    'CASE 0 '=====================EPSON PRINTER
    '     LPRINT CHR$(27); "@";  'Reset Printer
    '     LPRINT CHR$(27); "M";  'Elite Pitch
    '     LPRINT CHR$(15)        'Condensed
    'CASE 1 '=====================HP LaserJET
         LPRINT CHR$(27); "E";       'Reset Printer
         LPRINT CHR$(27); "(10U";    'PC-8 character set
         LPRINT CHR$(27); "(s0P";    'Fixed font
         LPRINT CHR$(27); "(s4102T"; 'Letter Gothic
         LPRINT CHR$(27); "(s2B";    'Demi bold
         LPRINT CHR$(27); "(s20H";   'Pitch 20 cpi
         LPRINT CHR$(27); "&l2A";    '8.5" x 11"
         LPRINT CHR$(27); "&l0E";    'Top margin 0 lines
         LPRINT CHR$(27); "&a0L";    'Left margin 0
         LPRINT CHR$(27); "&a164M";  'Right margin
         LPRINT CHR$(27); "&l66F";   'Text length 66 lines
         LPRINT CHR$(27); "&s0C"     'Line wrap
    'END SELECT
    PageNum = PageNum + 1: LineNum = 8
    LPRINT DATE$; TAB(70); "F. O. G. ENTERPRISES"; TAB(150); "Page:"; PageNum
    LPRINT TIME$; TAB(70); "Harriman . Tennessee": LPRINT
    LPRINT
END SUB

SUB KeyPres (f$)
    DO: LOOP WHILE INKEY$ > ""
    DO: f$ = INKEY$: LOOP WHILE f$ = ""
    f$ = UCASE$(f$)
END SUB

SUB OpenFiles ()

    CLOSE 1:  OPEN "..\Master\Codes   .Mas" FOR RANDOM ACCESS READ AS #1 LEN = LEN(Cod)
    CLOSE 2:  OPEN "..\Master\Combos  .Mas" FOR RANDOM ACCESS READ AS #2 LEN = LEN(Cmb)
    CLOSE 3:  OPEN "..\Master\Prices  .Mas" FOR RANDOM ACCESS READ AS #3 LEN = LEN(Prc)
    CLOSE 4:  OPEN "..\Master\Rates   .Mas" FOR RANDOM ACCESS READ AS #4 LEN = LEN(Rat)
    CLOSE 5:  OPEN "..\Master\PassWord.NAM" FOR RANDOM ACCESS READ AS #5 LEN = LEN(Pas)
    CLOSE 6:  OPEN "..\Master\Stores  .Mas" FOR RANDOM ACCESS READ AS #6 LEN = LEN(Sto)
    CLOSE 7:  OPEN "..\Master\Centers .Mas" FOR RANDOM ACCESS READ AS #7 LEN = LEN(Ctr)
    CLOSE 8:  OPEN "..\Master\Codes   .Cmb" FOR RANDOM ACCESS READ AS #8 LEN = LEN(Cod)

    CLOSE 10: OPEN "..\OfData\Inventor.MAS" FOR ISAM Inventory "Inventory" AS #10
'   CLOSE 11: OPEN "..\OfData\Customer.MAS" FOR ISAM Customer "Custom" AS #11
'   CLOSE 12: OPEN "..\OfData\Agreemnt.MAS" FOR ISAM Agreement "Agreem" AS #12
'   CLOSE 13: OPEN "..\OfData\Receipts.MAS" FOR ISAM Receipts "Receip" AS #13

'   CLOSE 14: OPEN "..\OfData\Activity.Mas" FOR RANDOM AS #14 LEN = LEN(Act)
'   CLOSE 15: OPEN "..\OfData\Transact.Hst" FOR RANDOM AS #15 LEN = LEN(Tra)
'   CLOSE 16: OPEN "..\OfData\Transact.Log" FOR APPEND AS #16
'   CLOSE 17: OPEN "LPT2:" FOR OUTPUT AS #17
END SUB

DEFINT A-Z
FUNCTION PassWord (p$)
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "User-ID", e)
    COLOR 28: LOCATE 20, 47
    SOUND 860, .3
    FOR x = 1 TO 4
    KeyPres s$
    PRINT "þ";
    IF s$ = CHR$(27) THEN PCOPY 1, 0: EXIT FUNCTION
    IF s$ = CHR$(13) THEN EXIT FOR
    f$ = f$ + s$
    NEXT
    PCOPY 1, 0
    f$ = CHR$(ASC(MID$(f$, 1, 1)) + 34) + CHR$(ASC(MID$(f$, 2, 1)) + 70) + CHR$(ASC(MID$(f$, 3, 1)) + 81) + CHR$(ASC(MID$(f$, 4, 1)) + 63)
    IF FindPas(f$) THEN
       IF Pas.Lvl >= p$ THEN PassWord = 1 ELSE WinMssg "Permission denied"
    ELSE
       WinMssg "Invalid Password"
    END IF
END FUNCTION

DEFSNG A-Q, S-Z
FUNCTION Response (f$)
    SOUND 800, .2
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, f$, e)
    KeyPres f$
    PCOPY 1, 0
    IF f$ = CHR$(27) THEN Response = 1
END FUNCTION

DEFSNG R
SUB Statlin (f$)
    qPrint SPACE$(25 - LEN(f$)) + f$, 50, 55, 0, 1
END SUB

DEFINT A-Z
STATIC SUB SubMenu (s, n$)
    IF s THEN
       REDIM a$(10), a(10)
       Picture 49, 1, 80, m$
       b = 1
       FOR x = 1 TO 9
           e = INSTR(b, m$, "  ")
           IF e = b THEN EXIT FOR
           a$(x) = MID$(m$, b, 1 + e - b)
           a(x) = b
           b = e + 1
       NEXT
       REDIM PRESERVE a$(x - 1)
       REDIM PRESERVE a(x - 1)
       b = 1
    ELSE
       LOCATE 49, a(b)
       COLOR 10, 0: PRINT MID$(a$(b), 1, 2);
       COLOR 13, 0: PRINT MID$(a$(b), 3);
       SELECT CASE n$
       CASE CHR$(0) + "M"  '===============Right=
            b = b + 1: IF b > UBOUND(a, 1) THEN b = 1
       CASE CHR$(0) + "K"  '===============Left==
            b = b - 1: IF b < 1 THEN b = UBOUND(a, 1)
       CASE CHR$(13)
            n$ = MID$(a$(b), 2, 1): EXIT SUB
       CASE ELSE
            FOR x = 1 TO UBOUND(a, 1)
            IF n$ = MID$(a$(x), 2, 1) THEN b = x: EXIT FOR
            NEXT
       END SELECT
       LOCATE 49, a(b)
       COLOR 18, 14: PRINT MID$(a$(b), 1, 2);
       COLOR 21, 14: PRINT MID$(a$(b), 3);
    END IF
END SUB

DEFSNG A-S, U-Z
DEFSTR T
FUNCTION TraStat (f$)
     SELECT CASE f$
            CASE "AA": TraStat = "A. Paid Out [F]"
            CASE "AB": TraStat = "A. Loaner Back "
            CASE "AC": TraStat = "A. Charge-off  "
            CASE "AE": TraStat = "A. EZA days    "
            CASE "AF": TraStat = "A. Free Days   "
            CASE "AL": TraStat = "A. Loaner      "
            CASE "AM": TraStat = "A. Reroute     "
            CASE "AN": TraStat = "A. New Issue   "
            CASE "AP": TraStat = "A. Promissary  "
            CASE "AR": TraStat = "A. Reinstate   "
            CASE "AS": TraStat = "A. Swap        "
            CASE "AT": TraStat = "A. Terminate   "
            CASE "AV": TraStat = "A. Void        "
            CASE "AW": TraStat = "A. Wavier      "

            CASE "MC": TraStat = "M. Override    "
            CASE "ME": TraStat = "M. Erase Item  "
            CASE "MG": TraStat = "M. Give-away   "
            CASE "MI": TraStat = "M. Service in  "
            CASE "MN": TraStat = "M. Receive new "
            CASE "MM": TraStat = "M. Transfer man"
            CASE "MO": TraStat = "M. Service out "
            CASE "MP": TraStat = "M. Scrap       "
            CASE "MR": TraStat = "M. Transfer in "
            CASE "MS": TraStat = "M. Service site"
            CASE "MT": TraStat = "M. Transfer out"
            CASE "MU": TraStat = "M. Modify Item "

            CASE "RB": TraStat = "R. Bad Check   "
            CASE "RN": TraStat = "R. New Issue   "
            CASE "RP": TraStat = "R. Paid Out [R]"
            CASE "RV": TraStat = "R. Void        "
            CASE "RR": TraStat = "R. Reprint     "

            CASE "CN": TraStat = "C. New         "
            CASE "CM": TraStat = "C. Edit        "
            CASE ELSE: TraStat = "?. Activity    "
     END SELECT
END FUNCTION

DEFINT A-Z
FUNCTION WinAgrt ()
'   CustAgr Cus.Num, f, t
    IF f = 0 THEN
       WinMssg "No agreement for this customer"
       WinAgrt = 1
       EXIT FUNCTION
    END IF
    r = 12: b = f: n = f
    PCOPY 0, 2: CALL MhDisplayByName(0, 0, "List-AGR", e)
    GOSUB a2
a1: GET #18, n, Agx: GET #16, Agx.Num, Agr
    COLOR 15, 5: LOCATE r, 32: PRINT USING "&ณ & ณ&ณ&ณ& "; Agr.Num; Agr.Sta; Agr.Dat; Agr.Cod; Agr.Cmb
    KeyPres f$
    COLOR 17, 5: LOCATE r, 32: PRINT USING "&ณ & ณ&ณ&ณ& "; Agr.Num; Agr.Sta; Agr.Dat; Agr.Cod; Agr.Cmb
    SELECT CASE f$
    CASE CHR$(0) + "P"
         IF n + 1 > t GOTO a1 ELSE n = n + 1
         IF r + 1 > 26 THEN CALL Scroll(32, 12, 62, 26, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"
         IF n - 1 < f GOTO a1 ELSE n = n - 1
         IF r - 1 < 12 THEN CALL BkScroll(32, 12, 62, 26, 1) ELSE r = r - 1
    CASE CHR$(13)
         IF Yes = 2 THEN IF WinRcts GOTO a1
         PCOPY 2, 0
         EXIT FUNCTION
    CASE ELSE
         WinAgrt = 1
         PCOPY 2, 0
         EXIT FUNCTION
    END SELECT
    GOTO a1

a2: COLOR 17, 5: LOCATE 12
    FOR x = b TO t
    GET #18, x, Agx: GET #16, Agx.Num, Agr
    LOCATE , 32: PRINT USING "&ณ & ณ&ณ&ณ& "; Agr.Num; Agr.Sta; Agr.Dat; Agr.Cod; Agr.Cmb
    IF CSRLIN > 26 THEN EXIT FOR
    NEXT
    RETURN
END FUNCTION

FUNCTION WinCntr ()
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-CTR", e)
    b = 1: n = b: r = 5: GOSUB T2
T1: GET #6, n, Ctr
    COLOR 27, 0: LOCATE r, 41: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam
    KeyPres n$
    COLOR 8, 3: LOCATE r, 41: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam
    SELECT CASE n$
    CASE CHR$(13)
        IF Ctr.Nam > "!" THEN
           PCOPY 1, 0
           EXIT FUNCTION
        END IF
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > 99 GOTO T1 ELSE n = n + 1
        IF r + 1 > 19 THEN CALL Scroll(41, 5, 69, 19, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 GOTO T1 ELSE n = n - 1
        IF r - 1 < 5 THEN CALL BkScroll(41, 5, 69, 19, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        b = n - r - 10: IF b < 1 THEN b = 1
        GOSUB T2: n = b + r - 5
    CASE CHR$(0) + "Q" 'PgDn
        b = 20 - r + n
        IF b > 85 THEN b = 85: n = 80 + r ELSE n = n + 15
        GOSUB T2
    CASE CHR$(0) + "G" 'Home
        b = 1: GOSUB T2
        n = r - 4
    CASE CHR$(0) + "O" 'End
        b = 85: GOSUB T2
        n = 80 + r
    CASE ELSE
        WinCntr = 1
        PCOPY 1, 0
        EXIT FUNCTION
    END SELECT
    GOTO T1
  
T2: COLOR 8, 3: LOCATE 5
    FOR x = b TO b + 14: GET #6, x, Ctr
    LOCATE , 41: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam
    NEXT
    RETURN
END FUNCTION

SUB WinCode (t$)
    PCOPY 0, 1
    b = 1: t = LOF(8) / LEN(Cod): r = 7: n = 1: GOSUB C2
C1: GET #8, n, Cod
    COLOR 15, 2: LOCATE r, 46: PRINT USING "&ณ&ณ&"; Cod.Cmb; Cod.Cod; Cod.Des;
    KeyPres n$
    COLOR 11, 1: LOCATE r, 46: PRINT USING "&ณ&ณ&"; Cod.Cmb; Cod.Cod; Cod.Des;
    SELECT CASE n$
    CASE CHR$(13)
        t$ = Cod.Cmb
        PCOPY 1, 0
        EXIT SUB
    CASE CHR$(27)
        t$ = ""
        PCOPY 1, 0
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
    FOR x = b TO t: GET #8, x, Cod
    LOCATE , 45: PRINT USING "บ&ณ&ณ&บ"; Cod.Cmb; Cod.Cod; Cod.Des
    IF CSRLIN > 20 THEN EXIT FOR
    NEXT
    LOCATE , 45: PRINT "ศออฯออออฯออออออออออออออออออออออออผ"
    RETURN
END SUB

FUNCTION WinComb ()
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-CMB", e)
    b = 1: t = LOF(3) \ LEN(Cmb): r = 7: n = 1: GOSUB B2
B1: GET #3, n, Cmb
    COLOR 27, 1: LOCATE r, 43: PRINT USING "&ณ&"; Cmb.Cod; Cmb.Des;
    KeyPres n$
    COLOR 17, 3: LOCATE r, 43: PRINT USING "&ณ&"; Cmb.Cod; Cmb.Des;
    SELECT CASE n$
    CASE CHR$(0) + "P", "2"'--Dn
        IF n + 1 > t GOTO B1 ELSE n = n + 1
        IF r + 1 > 21 THEN CALL Scroll(43, 7, 71, 21, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", "8"'--Up
        IF n - 1 < 1 GOTO B1 ELSE n = n - 1
        IF r - 1 < 7 THEN CALL BkScroll(43, 7, 71, 21, 1) ELSE r = r - 1
    CASE CHR$(0) + "I", "9"'--PgUp
        b = n - r - 8
        IF b < 1 THEN b = 1
        GOSUB B2: n = x + r - 21
    CASE CHR$(0) + "Q", "3"'--PgDn
        b = n - r + 22
        IF b + 14 > t THEN b = t - 14
        GOSUB B2: n = x + r - 21
    CASE CHR$(0) + "G", "7"'--Home
        b = 1: GOSUB B2
        n = r - 6
    CASE CHR$(0) + "O", "1"'--End
        b = t - 14: GOSUB B2
        n = t - 21 + r
    CASE CHR$(27), "5"
        WinComb = 1
        GOTO C3
    CASE CHR$(13)
'       Agr.Cmb = Cmb.Cod
        GOTO C3
    CASE CHR$(0) + "K", CHR$(0) + "M", "4", "6"'--left
        PCOPY 0, 2: CALL MhDisplayByName(0, 0, "Combo", e)
        GET #4, VAL(Cmb.Rat), Rat
        COLOR 10, 0: LOCATE 12, 10: PRINT Cmb.Cod; " "; Cmb.Des
 '      InvRate 18, 42
        LOCATE 16
        FOR s = 1 TO 4: FindGrp (Cmb.Cde(s)): NEXT
        KeyPres f$
        PCOPY 2, 0
    CASE ELSE
    END SELECT
    GOTO B1

B2: COLOR 17, 3: LOCATE 7
    FOR x = b TO t: GET #3, x, Cmb
    LOCATE , 43: PRINT USING "&ณ&"; Cmb.Cod; Cmb.Des
    IF CSRLIN > 21 THEN EXIT FOR
    NEXT
    RETURN
C3: PCOPY 1, 0
END FUNCTION

FUNCTION WinCust ()
    n = LOC(15)
    t = LOF(15) \ LEN(Cnx)
    b = n
    r = 4
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-CUS", e)
    GOSUB w2
w1: GET #15, n, Cnx
    COLOR 15, 1: LOCATE r, 47: PRINT USING "&"; Cnx.Nam;
    KeyPres f$
    COLOR 17, 7: LOCATE r, 47: PRINT USING "&"; Cnx.Nam;
    SELECT CASE f$
    CASE CHR$(0) + "P", "2"
         IF n + 1 > t GOTO w1 ELSE n = n + 1
         IF r + 1 > 18 THEN CALL Scroll(47, 4, 72, 18, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", "8"
         IF n - 1 < 1 GOTO w1 ELSE n = n - 1
         IF r - 1 < 4 THEN CALL BkScroll(47, 4, 72, 18, 1) ELSE r = r - 1
    CASE CHR$(0) + "I", "9"'PgUp
         IF t > 15 THEN
            b = n - r - 11
            IF b < 1 THEN b = 1
            GOSUB w2: n = x + r - 18
         END IF
    CASE CHR$(0) + "Q", "3"'PgDn
         IF t > 15 THEN
            b = 19 - r + n
            IF b + 14 > t THEN b = t - 14
            GOSUB w2: n = x + r - 18
         END IF
    CASE CHR$(0) + "G", "7"'Home
         b = 1: GOSUB w2: n = r - 3
    CASE CHR$(0) + "O", "1"'End
         b = t - 14: IF b < 1 THEN b = 1
         GOSUB w2: n = b + r - 4
    CASE CHR$(13)
         GET #13, Cnx.Num, Cus
         IF Yes THEN IF WinAgrt GOTO w1
         GOTO w3
    CASE "N"
         Cnx.Num = 1 + LOF(15) \ LEN(Cnx)
'        Tra.Typ = "CN"
'        CusEdit
    CASE "M"
'        Tra.Typ = "CM"
'        CusEdit
    CASE ELSE
         GET #13, Cnx.Num, Cus
         WinCust = 1: GOTO w3
    END SELECT
    GOTO w1

w2: COLOR 17, 7: LOCATE 4
    FOR x = b TO t
    GET #15, x, Cnx
    LOCATE , 47: PRINT USING "&"; Cnx.Nam
    IF CSRLIN > 18 THEN EXIT FOR
    NEXT
    RETURN
w3: PCOPY 1, 0
END FUNCTION

FUNCTION WinItem (c, i)
    n = LOC(12)
    t = LOF(12) \ LEN(Inx)
    b = n
    r = 7
    s = 12
    f = 11
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-ITM", e)
    GOSUB i2
i1: GET #f, n, Inx
    COLOR 11, 1: LOCATE r, 43: PRINT USING " & ณ&ณ&ณ& "; Inx.Typ + Inx.Grp; Inx.Cod; Inx.Mdl; Inx.Ser: KeyPres n$
    COLOR 16, 3: LOCATE r, 43: PRINT USING " & ณ&ณ&ณ& "; Inx.Typ + Inx.Grp; Inx.Cod; Inx.Mdl; Inx.Ser
    SELECT CASE n$
    CASE CHR$(0) + "P", "2"
        IF n + 1 > t GOTO i1 ELSE n = n + 1
        IF r + 1 > 21 THEN CALL Scroll(44, 7, 75, 21, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", "8"
        IF n - 1 < 1 GOTO i1 ELSE n = n - 1
        IF r - 1 < 7 THEN CALL BkScroll(44, 7, 75, 21, 1) ELSE r = r - 1
    CASE CHR$(0) + "I", "9"
        b = n - r - 8
        IF b < 1 THEN b = 1
        GOSUB i2: n = x + r - 21
    CASE CHR$(0) + "Q", "3"
        b = n - r + 22
        IF b + 14 > t THEN b = t - 14
        GOSUB i2: n = x + r - 21
    CASE CHR$(0) + "G", "7"
        b = 1: GOSUB i2: n = r - 6
    CASE CHR$(0) + "O", "1"
        b = t - 14: GOSUB i2: n = t - 21 + r
    CASE CHR$(13)
        GET #10, Inx.Num, Inv
        WinItem = 0: GOTO I3
    CASE CHR$(27), "5"
        WinItem = 1: GOTO I3
    CASE "I"
        SWAP f, s
        b = n + 7 - r: GOSUB i2
    CASE CHR$(0) + "K", CHR$(0) + "M", "4", "6"'--left
        PCOPY 0, 2: CALL MhDisplayByName(0, 0, "Inventory", e)
'       InvRtrv
        COLOR 11, 5
        LOCATE 13, 4:  PRINT USING "&ณ&ณ&"; Cod.Typ; Inv.Cod; Cod.Des;
        LOCATE 15, 23: PRINT Status(Inv.Sta);
        LOCATE 16, 23: PRINT Cod.Grp;
        LOCATE 17, 23: PRINT Inv.Mdl;
        LOCATE 18, 23: PRINT Inv.Ser;
        IF Pas.Lvl > "5" THEN
           LOCATE 21, 23: PRINT USING "#######"; Inv.Tdr;
           LOCATE 22, 23: PRINT USING "####.##"; Inv.Tin / 100;
           LOCATE 24, 23: PRINT Inv.Prc;
           LOCATE 25, 26: PRINT Cod.Fmv;
           LOCATE 26, 26: PRINT Cod.Dfm;
           LOCATE 27, 26: PRINT Cod.Dtr;
           LOCATE 28, 25: PRINT Cod.Dep;
        END IF
        LOCATE 30, 24: PRINT Inv.Par;
        LOCATE 31, 24: PRINT Inv.Lab;
        LOCATE 35, 3:  PRINT USING "&ณ&ณ&"; Inv.Dtp; Inv.Dtr; Inv.Dta;
'       InvRate 17, 36
        KeyPres f$
        PCOPY 2, 0
    CASE "F"
        FOR x = n + 1 TO t
        GET #f, x, Inx
        GET #10, Inx.Num, Inv
        IF Inv.Sta = 0 THEN
           SELECT CASE c
           CASE 0 'any idle items
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR
           CASE 1 'for loaners, check group
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR

           CASE 2 'for reinstatement, same value
                
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR
                
           CASE 3 'for a combo item
                IF FindCod(Inv.Cod) = 0 THEN ERROR 200
                
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR
                
           END SELECT
        END IF
        NEXT
    END SELECT
    GOTO i1
i2: COLOR 16, 3: LOCATE 7
    FOR x = b TO t
    GET #f, x, Inx
    LOCATE , 44
    PRINT USING "& ณ&ณ&ณ&"; Inx.Typ + Inx.Grp; Inx.Cod; Inx.Mdl; Inx.Ser
    IF CSRLIN > 21 THEN EXIT FOR
    NEXT
    RETURN
I3: PCOPY 1, 0
END FUNCTION

DEFSNG A-Q, S-Z
SUB WinMssg (f$)
    r = 16
    PCOPY 0, 3
    COLOR 17, 7
    LOCATE r + 1, 31: PRINT STRING$(4 + LEN(f$), " ");
    LOCATE r + 2, 31: PRINT "  "; f$; "  ";
    LOCATE r + 3, 31: PRINT STRING$(4 + LEN(f$), " ");
    COLOR 8, 0
    Picture r + 2, 30, 1, b$: LOCATE r + 2, 30: PRINT b$;
    Picture r + 3, 30, 1, b$: LOCATE r + 3, 30: PRINT b$;
    Picture r + 4, 30, 4 + LEN(f$), b$: LOCATE r + 4, 30: PRINT b$;
    SOUND 350, 3: KeyPres f$
    PCOPY 3, 0
END SUB

DEFINT A-Q, S-Z
FUNCTION WinPric ()
    n = FindPrc(Inv.Dis + Inv.Mdl + Inv.Cod)
    IF n = 0 THEN n = 1
    t = LOF(3) \ LEN(Prc)
    b = n
    r = 7
    IF b > t - 13 THEN b = t - 13: r = 20 - (t - n)
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-PRC", e)
    GOSUB p2
p1: GET #3, n, Prc
    COLOR 15, 3: LOCATE r, 50: PRINT USING "& ณ&ณ&"; Prc.Dis; Prc.Mdl; Prc.Cod;
    KeyPres n$
    COLOR 17, 3: LOCATE r, 50: PRINT USING "& ณ&ณ&"; Prc.Dis; Prc.Mdl; Prc.Cod;
    SELECT CASE n$
    CASE CHR$(13)
        PCOPY 1, 0: EXIT FUNCTION
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > t GOTO p1 ELSE n = n + 1
        IF r + 1 > 20 THEN CALL Scroll(50, 7, 76, 20, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 GOTO p1 ELSE n = n - 1
        IF r - 1 < 7 THEN CALL BkScroll(50, 7, 76, 20, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        b = n - r - 7: IF b < 1 THEN b = 1
        GOSUB p2: n = b + r - 7
    CASE CHR$(0) + "Q" 'PgDn
        b = 21 - r + n
        IF b > t - 13 THEN b = t - 13: n = t - 20 + r ELSE n = n + 14
        GOSUB p2
    CASE CHR$(0) + "G" 'Home
        b = 1: GOSUB p2
        n = r - 6
    CASE CHR$(0) + "O" 'End
        b = t - 13: GOSUB p2
        n = t - 20 + r
    CASE ELSE
        WinPric = 1
        PCOPY 1, 0
        EXIT FUNCTION
    END SELECT
    GOTO p1
  
p2: COLOR 17, 3: LOCATE 7
    FOR x = b TO t: GET #3, x, Prc
    LOCATE , 50: PRINT USING "& ณ&ณ&"; Prc.Dis; Prc.Mdl; Prc.Cod
    IF CSRLIN > 20 THEN EXIT FOR
    NEXT
    RETURN
END FUNCTION

SUB WinRate ()
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "Rates", e)
    FOR z = 1 TO 3
        r = 24 + z
        qPrint Rat.Mtr(z), r, 12, 0, 1
        qPrint Rat.Mrt(z), r, 17, 0, 1
        qPrint Rat.Wtr(z), r, 24, 0, 1
        qPrint Rat.Wrt(z), r, 29, 0, 1
    NEXT
    KeyPres f$
    PCOPY 1, 0
END SUB

FUNCTION WinRcts ()
'   RctAgrt Agx.Agr, f, t
    IF f = 0 THEN
       WinMssg "No receipts for this agreement"
       WinRcts = 1
       EXIT FUNCTION
    END IF
    r = 20: b = f: n = f
    PCOPY 0, 3: CALL MhDisplayByName(0, 0, "List-RCT", e)
    GOSUB m2
M1: GET #21, n, Rcx: GET #19, Rcx.Num, Rct
    COLOR 15, 1: LOCATE r, 23: PRINT USING "&ณ&ณ&ณ&"; Rct.Agr; Rct.Num; Rct.Dat; Rct.Amt
    KeyPres f$
    COLOR 27, 1: LOCATE r, 23: PRINT USING "&ณ&ณ&ณ&"; Rct.Agr; Rct.Num; Rct.Dat; Rct.Amt
    SELECT CASE f$
    CASE CHR$(0) + "P"
         IF n + 1 > t GOTO M1 ELSE n = n + 1
         IF r + 1 > 34 THEN CALL Scroll(23, 20, 54, 34, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"
         IF n - 1 < f GOTO M1 ELSE n = n - 1
         IF r - 1 < 20 THEN CALL BkScroll(23, 20, 54, 34, 1) ELSE r = r - 1
    CASE CHR$(13)
         PCOPY 3, 0
         EXIT FUNCTION
    CASE ELSE
         WinRcts = 1
         PCOPY 3, 0
         EXIT FUNCTION
    END SELECT
    GOTO M1

m2: COLOR 27, 1: LOCATE 20
    FOR x = b TO t
    GET #21, x, Rcx: GET #19, Rcx.Num, Rct
    LOCATE , 23: PRINT USING "&ณ&ณ&ณ&"; Rct.Agr; Rct.Num; Rct.Dat; Rct.Amt
    IF CSRLIN > 34 THEN EXIT FOR
    NEXT
    RETURN
END FUNCTION

FUNCTION WinStor ()
    PCOPY 0, 1: : CALL MhDisplayByName(0, 0, "List-STO", e)
    Bot = 1: Top = 99: r = 5: n = 1: GOSUB S2
s1: GET #6, n, Sto
    COLOR 15, 3: LOCATE r, 41: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
    KeyPres n$
    COLOR 0, 7: LOCATE r, 41: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
    SELECT CASE n$
    CASE CHR$(13)
        IF Sto.Nam > "!" THEN
           PCOPY 1, 0
           EXIT FUNCTION
        END IF
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > 99 GOTO s1 ELSE n = n + 1
        IF r + 1 > 19 THEN CALL Scroll(41, 5, 69, 19, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 GOTO s1 ELSE n = n - 1
        IF r - 1 < 5 THEN CALL BkScroll(41, 5, 69, 19, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 10: IF Bot < 1 THEN Bot = 1
        GOSUB S2: n = Bot + r - 5
    CASE CHR$(0) + "Q" 'PgDn
        Bot = 20 - r + n
        IF Bot > 85 THEN Bot = 85: n = 80 + r ELSE n = n + 15
        GOSUB S2
    CASE CHR$(0) + "G" 'Home
        Bot = 1: GOSUB S2
        n = r - 4
    CASE CHR$(0) + "O" 'End
        Bot = 85: GOSUB S2
        n = 80 + r
    CASE ELSE
        WinStor = 1
        PCOPY 1, 0
        EXIT FUNCTION
    END SELECT
    GOTO s1
   
S2: COLOR 0, 7: LOCATE 5
    FOR x = Bot TO Bot + 14
    GET #6, x, Sto
    LOCATE , 41: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
    NEXT
    RETURN
END FUNCTION

