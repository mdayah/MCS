DEFINT A-Z
DECLARE SUB AC_Init ()
DECLARE SUB AP_Files ()
DECLARE SUB AR_Files ()
DECLARE SUB GL_Files ()
DECLARE SUB PR_Files ()
DECLARE SUB IN_Files ()
DECLARE SUB FS_Files ()
DECLARE FUNCTION WinAcct ()
DECLARE FUNCTION WinStor ()

DECLARE SUB optDate ()
DECLARE SUB optYear ()
DECLARE SUB optMonth ()
DECLARE SUB optModify ()
DECLARE SUB optBackup (s)
DECLARE SUB Terminate ()

DECLARE SUB mapInvNew ()
DECLARE SUB mapInvEdt ()
DECLARE SUB mapChkReg ()
DECLARE SUB mapChkPrt ()
DECLARE SUB mapChkMan ()
DECLARE SUB mapChkMas ()
DECLARE SUB mapVenMas ()
DECLARE SUB mapInvDet ()
DECLARE SUB mapInvSum ()
DECLARE SUB mapSelInv ()
DECLARE SUB mapChkDet ()
DECLARE SUB mapRequir ()
DECLARE SUB mapAging ()
DECLARE SUB mapVenRpt ()
DECLARE SUB mapVerify ()
DECLARE SUB mapPurge ()

DECLARE SUB mprCards ()
DECLARE SUB mprSheet ()
DECLARE SUB mprChecks ()
DECLARE SUB mprChkReg ()
DECLARE SUB mprChkMas ()
DECLARE SUB mprChkMan ()
DECLARE SUB mprChkLst ()
DECLARE SUB mprEmpMas ()
DECLARE SUB mprEmpLst ()
DECLARE SUB mprW2s ()
DECLARE SUB mprMonth ()
DECLARE SUB mprQuarter ()
DECLARE SUB mprYear ()

DECLARE SUB mglJrnNew ()
DECLARE SUB mglJrnEdt ()
DECLARE SUB mglJrnAct ()
DECLARE SUB mglJrnRpt ()
DECLARE SUB mglActMas ()
DECLARE SUB mglJrnMas ()
DECLARE SUB mglActVrf ()
DECLARE SUB mglPurge ()

DECLARE SUB minMatCost ()
DECLARE SUB minMatCount ()
DECLARE SUB minMatMast ()
DECLARE SUB minMatPrice ()
DECLARE SUB minMonth ()
DECLARE SUB minPurchase ()
DECLARE SUB minPurMonth ()
DECLARE SUB minPurNew ()
DECLARE SUB minPurPrice ()
DECLARE SUB minPurVerify ()
DECLARE SUB minPurYear ()
DECLARE SUB minYear ()
DECLARE SUB marCusMast ()
DECLARE SUB marInvoice ()
DECLARE SUB marMonth ()
DECLARE SUB marOrder ()
DECLARE SUB marReceipt ()
DECLARE SUB marYear ()

DECLARE SUB mfsTrial ()
DECLARE SUB mfsIncome ()
DECLARE SUB mfsSheet ()
DECLARE SUB mfsEsheet ()
DECLARE SUB mfsEincome ()

REM $INCLUDE: '..\BI\Library.REC'

    ON ERROR GOTO Errors
    CLS : WIDTH 80, 50

    CALL MhOpenScreenLib("AC.SLB" + CHR$(0), e)
    CALL MhBlinkOff
    CALL MhShadowOn(0, 8)

    DIM Menu(7)        AS STRING
    DIM Loct(7, 2)     AS INTEGER
    DIM Subm(7, 21, 2) AS STRING
    DIM HiLi(7, 21)    AS INTEGER

    FOR m = 1 TO 7
    READ Menu(m)
    READ Loct(m, 1), Loct(m, 2)
    FOR c = 1 TO Loct(m, 2)
    READ Subm(m, c, 1), Subm(m, c, 2)
    READ HiLi(m, c)
    NEXT
    NEXT
    ErrSw(1) = 1
    ErrSw(2) = 1

'$INCLUDE: '..\BI\StartUp.DEC'
    
    AC_Init
    m = 1: c = 1
10  IF c > Loct(m, 2) THEN c = Loct(m, 2)
    CALL MhDisplayByName(0, 0, "AC-Logo", e)
    CALL MhDisplayByName(0, 0, Menu(m), e)
    qPrint Company$, 50, 2, 0, 1
20  CLOSE
    COLOR 27, 0: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
    COLOR 28, 0: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
    KeyPres f$
30  COLOR 17, 6: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
    COLOR 20, 6: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
    SELECT CASE f$
    CASE CHR$(0) + "M"
         m = m + 1: IF m > 7 THEN m = 1
         GOTO 10
    CASE CHR$(0) + "K"
         m = m - 1: IF m < 1 THEN m = 7
         GOTO 10
    CASE CHR$(0) + "P"
         c = c + 1: IF c > Loct(m, 2) THEN c = 1
    CASE CHR$(0) + "H"
         c = c - 1: IF c < 1 THEN c = Loct(m, 2)
    CASE CHR$(13)
    COLOR 27, 0: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
    COLOR 28, 0: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
         SELECT CASE m
              CASE 1
                   GL_Files
                   SELECT CASE c
                          CASE 1:  mglJrnNew
                          CASE 2:  mglJrnEdt
                          CASE 3:  mglJrnRpt
                          CASE 4:  mglJrnAct
                          CASE 5:  mglJrnMas
                          CASE 6:  mglActMas
                          CASE 7:  mglPurge
                          CASE 8:  mglActVrf
                   END SELECT
              CASE 2
                   PR_Files
                   SELECT CASE c
                          CASE 1:  mprCards
                          CASE 2:  mprSheet
                          CASE 3:  mprChkReg
                          CASE 4:  mprChecks
                          CASE 5:  mprChkMan
                          CASE 6:  mprEmpMas
                          CASE 7:  mprChkMas
                          CASE 8:  mprEmpLst
                          CASE 9:  mprChkLst
                          CASE 10: mprW2s
                          CASE 11: mprMonth
                          CASE 12: mprQuarter
                          CASE 13: mprYear
                   END SELECT
              CASE 3
                   AP_Files
                   SELECT CASE c
                          CASE 1: mapInvNew
                          CASE 2: mapInvEdt
                          CASE 3: mapChkReg
                          CASE 4: mapChkPrt
                          CASE 5: mapChkMan
                          CASE 6: mapChkMas
                          CASE 7: mapVenMas
                          CASE 8: mapInvDet
                          CASE 9: mapInvSum
                          CASE 10: mapSelInv
                          CASE 11: mapChkDet
                          CASE 12: mapVenRpt
                          CASE 13: mapAging
                          CASE 14: mapRequir
                          CASE 15: mapPurge
                          CASE 16: mapVerify
                   END SELECT
              CASE 4
                  'AR_Files
                   SELECT CASE c
                   '      CASE 1:  marOrder
                   '      CASE 2:  marInvoice
                   '      CASE 3:  marReceipt
                   '      CASE 4:  marCusMast
                   '      CASE 18: marMonth
                   '      CASE 19: marYear
                   END SELECT
              CASE 5
                  'IN_Files
                   SELECT CASE c
                   '      CASE 1:  minPurchase
                   '      CASE 2:  minPurNew
                   '      CASE 3:  minPurMonth
                   '      CASE 4:  minPurYear
                   '      CASE 5:  minPurPrice
                   '      CASE 7:  minPurVerify
                   '      CASE 7:  minMatCost
                   '      CASE 9:  minMatPrice
                   '      CASE 10: minMatCount
                   '      CASE 11: minMatMast
                   '      CASE 12: minMonth
                   '      CASE 13: minYear
                   END SELECT
              CASE 6
                   FS_Files
                   SELECT CASE c
                          CASE 1: mfsTrial
                          CASE 2: mfsIncome
                          CASE 3: mfsSheet
                          CASE 4: mfsEincome
                          CASE 5: mfsEsheet
                          CASE 6:
                          CASE 7:
                   END SELECT
              CASE 7
                   SELECT CASE c
                          CASE 1: optModify
                          CASE 2: optDate
                          CASE 3: optBackup 1
                          CASE 4: optBackup 2
                          CASE 5: optBackup 3
                          CASE 6: optMonth
                          CASE 7: optYear
                          CASE 8: Terminate
                   END SELECT
         END SELECT
         CLOSE
         GOTO 10
    CASE ELSE
         FOR x = 1 TO Loct(m, 2)
         IF f$ = Subm(m, x, 2) THEN c = x: f$ = CHR$(13): GOTO 30
         NEXT
    END SELECT
    GOTO 20
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    PCOPY 0, 1
    CALL MhDisplayByName(0, 0, "AC-Error", e)
    SELECT CASE ERR
           CASE 200: e$ = "Invalid Store Numer"
           CASE ELSE: e$ = ERROR$(ERR)
    END SELECT
    qPrint STR$(ERR) + STR$(ERDEV AND &HFF) + STR$((ERDEV AND &HFF00) \ 256), 17, 42, 0, 1
    qPrint e$, 18, 42, 0, 1
    qPrint ERDEV$, 19, 42, 0, 1
    
    SOUND 50, 3: KeyPres f$
    PCOPY 1, 0
    RESUME 10
MenuData: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    DATA "GL-Menu",2,8
    DATA " New Entry        ","N",3
    DATA " Edit Entry       ","E",3
    DATA " Print Entries    ","P",3
    DATA " Account Activity ","A",3
    DATA " Journal Master   ","J",3
    DATA " Accounts Master  ","M",12
    DATA " Purge Entries    ","P",3
    DATA " Verify Balances  ","V",3

    DATA "PR-Menu",10,13
    DATA " Update Timecards ","U",11
    DATA " Print Timesheets ","P",11
    DATA " Payroll Register ","R",19
    DATA " Issue Checks     ","I",11
    DATA " MaNual Check     ","N",13
    DATA " Employee Master  ","E",11
    DATA " Checks Master    ","C",11
    DATA " Employee Listing ","L",20
    DATA " CHeck Listing    ","H",12
    DATA " Print W-2 Forms  ","W",17
    DATA " Month Closing    ","M",11
    DATA " Quarter Closing  ","Q",11
    DATA " Year Closing     ","Y",11
    
    DATA "AP-Menu",19,16
    DATA " New Invoice       ","N",20
    DATA " Edit Invoice      ","E",20
    DATA " Check Register    ","R",26
    DATA " Issue Checks      ","I",20
    DATA " Manual Check      ","M",20
    DATA " Checks Master     ","C",20
    DATA " Vendors Master    ","V",20
    DATA " Invoice Detail    ","D",28
    DATA " Invoice Summary   ","S",28
    DATA " Select InvOices   ","O",30
    DATA " List ChecKs       ","K",29
    DATA " List Vendors      ","L",20
    DATA " Aging Report      ","A",20
    DATA " Cash ReQuirements ","Q",27
    DATA " Purge Invoices    ","P",20
    DATA " VeriFy Payables   ","F",24

    DATA "AR-Menu",29,17
    DATA " Sales Orders      ","O",36
    DATA " Sales Invoices    ","I",36
    DATA " Sales Receipts    ","R",36
    DATA " Customer Master   ","C",30
    DATA " Invoice Register  ","I",30
    DATA " Detailes Sales    ","D",30
    DATA " Summary Sales     ","S",30
    DATA " Address Lables    ","A",30
    DATA " Open Invoices     ","O",30
    DATA " Customer Listing  ","C",30
    DATA " Print Statements  ","P",30
    DATA " Reprint Statement ","R",30
    DATA " Statement by Type ","S",30
    DATA " Void Receipts     ","V",30
    DATA " Purge Invoices    ","P",30
    DATA " Purge Receipts    ","P",30
    DATA " Recost Invoices   ","R",30

    DATA "IN-Menu",42,10
    DATA " Update Purchases    ","U",43
    DATA " Print New Entries   ","P",43
    DATA " Print Month Entries ","P",43
    DATA " Print Year Entries  ","P",43
    DATA " Price Analysis      ","P",43
    DATA " Verify Purchases    ","V",43
    DATA " Reference Prices    ","R",43
    DATA " Inventory Count     ","I",43
    DATA " Inventory Master    ","I",43
    DATA " Detail Inventory    ","D",43

    DATA "FS-Menu",53,5
    DATA " Trial Balance    ","T",54
    DATA " Income Statement ","I",54
    DATA " Balance Sheet    ","B",54
    DATA " INcome's Layout  ","N",55
    DATA " Sheet's Layout   ","S",54

    DATA "OP-Menu",65,8
    DATA " Comapny Info  ","C",66
    DATA " System Date   ","S",66
    DATA " Backup Data   ","B",66
    DATA " Verify Backup ","V",66
    DATA " Restore Data  ","R",66
    DATA " Month Closing ","M",66
    DATA " Year Closing  ","Y",66
    DATA " Terminate     ","T",66

ArrayData: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    DATA ""
    DATA "one "
    DATA "two "
    DATA "three "
    DATA "four "
    DATA "five "
    DATA "six "
    DATA "seven "
    DATA "eight "
    DATA "nine "
    DATA ""
    DATA "eleven "
    DATA "twelve "
    DATA "thirteen "
    DATA "fourteen "
    DATA "fifteen "
    DATA "sixteen "
    DATA "seventeen "
    DATA "eighteen "
    DATA "nineteen "
    DATA ""
    DATA "ten "
    DATA "twenty "
    DATA "thirty "
    DATA "forty "
    DATA "fifty "
    DATA "sixty "
    DATA "seventy "
    DATA "eighty "
    DATA "ninety "
 
    DATA "Thursday "
    DATA "Friday   "
    DATA "Saturday "
    DATA "Sunday   "
    DATA "Monday   "
    DATA "Tuesday  "
    DATA "Wednesday"

SUB AC_Init ()
    FOR y = 1 TO 3
    FOR x = 0 TO 9
    READ Digit(y, x)
    NEXT x, y

    FOR y = 0 TO 6
    READ dWeek(y)
    NEXT
    WIDTH "LPT1:", 255
END SUB

FUNCTION Confirm (f$)
    r = 10
    PCOPY 0, 3
    COLOR 17, 6: LOCATE r + 1, 40: PRINT STRING$(15 + LEN(f$), " ");
    COLOR 17, 6: LOCATE r + 2, 40: PRINT "  Press * to "; f$; "  ";
    COLOR 8, 0: Picture r + 2, POS(0), 1, c$: PRINT c$;
    COLOR 17, 6: LOCATE r + 3, 40: PRINT STRING$(15 + LEN(f$), " ");
    COLOR 8, 0: Picture r + 3, POS(0), 1, c$: PRINT c$;
    COLOR 8, 0: Picture r + 4, 41, 15 + LEN(f$), c$: LOCATE r + 4, 41: PRINT c$;
    SOUND 350, .5: KeyPres f$
    IF f$ = "*" THEN Confirm = 1: SOUND 1200, .3: SOUND 1000, .2
    PCOPY 3, 0
END FUNCTION

DEFSNG A-Z
SUB Heading (s$)
     PRINT #11, CHR$(27); "E";       'Reset Printer
     WIDTH "LPT1:", 255
     PRINT #11, CHR$(27); "(10U";    'PC-8 character set
     PRINT #11, CHR$(27); "(s0P";    'Fixed font
     PRINT #11, CHR$(27); "(s4102T"; 'Letter Gothic
     PRINT #11, CHR$(27); "(s2B";    'Demi bold
     PRINT #11, CHR$(27); "(s20H";   'Pitch 20 cpi
     PRINT #11, CHR$(27); "&l2A";    '8.5" x 11"
     PRINT #11, CHR$(27); "&l0E";    'Top margin 0 lines
     PRINT #11, CHR$(27); "&a0L";    'Left margin 0
     PRINT #11, CHR$(27); "&a164M";  'Right margin
     PRINT #11, CHR$(27); "&l66F";   'Text length 66 lines
     PRINT #11, CHR$(27); "&s0C"     'Line wrap
     
     IF s$ = "00" THEN
        Sto.Num = "00"
        Sto.Nam = "Consolidated Stores       "
     ELSE
        GET #12, VAL(s$), Sto
     END IF
     PageNum = PageNum + 1: LineNum = 8
     PRINT #11, "Date: "; DATE$; TAB(81 - LEN(RTRIM$(Company)) \ 2); Company; TAB(150); "Page: "; PageNum
     PRINT #11, "Time: "; TIME$; TAB(81 - LEN(RTRIM$(Street) + " " + RTRIM$(City)) \ 2); RTRIM$(Street) + " " + City
     PRINT #11, TAB(81 - LEN(RTRIM$(Sto.Num + " " + Sto.Nam)) \ 2); Sto.Num + " " + Sto.Nam
END SUB

SUB KeyPres (f$)
    DO: LOOP WHILE INKEY$ > ""
    DO: f$ = INKEY$: LOOP WHILE f$ = ""
    f$ = UCASE$(f$)
END SUB

SUB NumWords (n$, w$)
     w$ = ""
     IF RIGHT$(n$, 2) > "01" THEN w$ = "s." ELSE w$ = "."
     w$ = RIGHT$(n$, 2) + " cent" + w$
     d$ = MID$(n$, 4, 2)
     IF d$ > "10" AND d$ < "20" THEN
        w$ = Digit(2, VAL(RIGHT$(d$, 1))) + "dollars & " + w$
     ELSEIF d$ > " 0" THEN
        w$ = Digit(3, VAL(LEFT$(d$, 1))) + Digit(1, VAL(RIGHT$(d$, 1))) + "dollars & " + w$
     END IF
     d$ = MID$(n$, 3, 1)
     IF d$ > "0" THEN w$ = Digit(1, VAL(d$)) + "hundred " + w$
     d$ = LEFT$(n$, 2)
     IF d$ > "10" AND d$ < "20" THEN
        w$ = Digit(2, VAL(RIGHT$(d$, 1))) + "thousand " + w$
     ELSEIF d$ > " 0" THEN
        w$ = Digit(3, VAL(LEFT$(d$, 1))) + Digit(1, VAL(RIGHT$(d$, 1))) + "thousand " + w$
     END IF
END SUB

DEFINT A-Z
SUB Printing (c, s)
    CALL MhDisplayByName(0, 0, "Printing", e)
    qPrint STRING$(1 + 50 * (c / s), "Û"), 42, 15, 0, 1
END SUB

DEFSNG A-Q, S-Z
FUNCTION Response (f$)
    SOUND 800, .2
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, f$, e)
    KeyPres f$
    IF f$ <> "*" THEN Response = 1
    PCOPY 1, 0
END FUNCTION

DEFSNG R
SUB StatLin (f$)
    COLOR 28, 0
    LOCATE 50, 50
    PRINT SPACE$(30 - LEN(f$)); f$;
END SUB

DEFINT A-Z
STATIC SUB SubMenu (s, n$)
   's = 1 if first time read menu line
   'n$= menu item to be higlighted
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
    END IF
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
END SUB

DEFSNG A-Z
SUB Terminate ()
    CLOSE
    SHELL "Del Payroll.REG"
    SHELL "Del Payable.REG"
    CLS
    END
END SUB

DEFINT A-Z
FUNCTION WinAcct ()
    PCOPY 0, 1: : CALL MhDisplayByName(0, 0, "Win-Acc", e)
    Bot = 1: r = 5: n = 1: GOSUB T2
T1: GET #1, n, Acc
    COLOR 15, 1: LOCATE r, 41: PRINT USING "&ณ&"; Acc.Num; Acc.Nam;
    KeyPres n$
    COLOR 15, 6: LOCATE r, 41: PRINT USING "&ณ&"; Acc.Num; Acc.Nam;
    SELECT CASE n$
    CASE CHR$(13)
        IF Acc.Nam > "!" THEN
           PCOPY 1, 0
           EXIT FUNCTION
        END IF
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > 99 GOTO T1 ELSE n = n + 1
        IF r + 1 > 19 THEN CALL Scroll(41, 5, 70, 19, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 GOTO T1 ELSE n = n - 1
        IF r - 1 < 5 THEN CALL BkScroll(41, 5, 70, 19, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 10: IF Bot < 1 THEN Bot = 1
        GOSUB T2: n = Bot + r - 5
    CASE CHR$(0) + "Q" 'PgDn
        Bot = 20 - r + n
        IF Bot > 85 THEN Bot = 85: n = 80 + r ELSE n = n + 15
        GOSUB T2
    CASE CHR$(0) + "G" 'Home
        Bot = 1: GOSUB T2
        n = r - 4
    CASE CHR$(0) + "O" 'End
        Bot = 85: GOSUB T2
        n = 80 + r
    CASE ELSE
        WinAcct = 1
        PCOPY 1, 0
        EXIT FUNCTION
    END SELECT
    GOTO T1
T2: COLOR 15, 6: LOCATE 4, 40: PRINT "ษออัอออออออออออออออออออออออออออป"
    FOR x = Bot TO Bot + 14: GET #1, x, Acc
    LOCATE , 40: PRINT USING "บ&ณ&บ"; Acc.Num; Acc.Nam
    NEXT
    LOCATE , 40: PRINT "ศออฯอออออออออออออออออออออออออออผ"
    RETURN
END FUNCTION

SUB WinMssg (f$)
    r = 16
    PCOPY 0, 3
    COLOR 15, 5
    LOCATE r + 1, 31: PRINT STRING$(4 + LEN(f$), " ");
    LOCATE r + 2, 31: PRINT "  "; f$; "  ";
    LOCATE r + 3, 31: PRINT STRING$(4 + LEN(f$), " ");
    COLOR 13
    LOCATE r + 2, 30: PRINT "Û";
    LOCATE r + 3, 30: PRINT "Û";
    LOCATE r + 4, 30: PRINT "ÛÛÛÛ"; STRING$(LEN(f$), "Û");
    SOUND 50, 3: KeyPres f$
    PCOPY 3, 0
END SUB

FUNCTION WinStor ()
    PCOPY 0, 1: : CALL MhDisplayByName(0, 0, "Win-STO", e)
    Bot = 1: Top = 99: r = 5: n = 1: GOSUB s2
s1: GET #12, n, Sto
    COLOR 15, 3: LOCATE r, 41: PRINT USING "&ณ&"; Sto.Num; Sto.Nam;
    KeyPres n$
    COLOR 0, 7:  LOCATE r, 41: PRINT USING "&ณ&"; Sto.Num; Sto.Nam;
    SELECT CASE n$
    CASE CHR$(13)
        IF Sto.Nam > "!" AND ComCode = Sto.Cod THEN
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
        GOSUB s2: n = Bot + r - 5
    CASE CHR$(0) + "Q" 'PgDn
        Bot = 20 - r + n
        IF Bot > 85 THEN Bot = 85: n = 80 + r ELSE n = n + 15
        GOSUB s2
    CASE CHR$(0) + "G" 'Home
        Bot = 1: GOSUB s2
        n = r - 4
    CASE CHR$(0) + "O" 'End
        Bot = 85: GOSUB s2
        n = 80 + r
    CASE ELSE
        WinStor = 1
        PCOPY 1, 0
        EXIT FUNCTION
    END SELECT
    GOTO s1

s2: COLOR 0, 7: LOCATE 5
    FOR x = Bot TO Bot + 14
    GET #12, x, Sto
    LOCATE , 41: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
    NEXT
    RETURN
END FUNCTION

