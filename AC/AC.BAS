DEFINT A-Z
DECLARE SUB PR_Init ()
DECLARE SUB PR_Files ()
DECLARE SUB optDate ()
DECLARE SUB Terminate ()
DECLARE SUB optModify ()

DECLARE SUB mprCards ()
DECLARE SUB mprSheet ()
DECLARE SUB mprRegister ()
DECLARE SUB mprChecks ()
DECLARE SUB mprChkMan ()
DECLARE SUB mprEmpMas ()
DECLARE SUB mprChkMas ()
DECLARE SUB mprAccMas ()
DECLARE SUB mprEmpLst ()
DECLARE SUB mprChkLst ()
DECLARE SUB mprW2s ()
DECLARE SUB mprMonth ()
DECLARE SUB mprQuarter ()
DECLARE SUB mprYear ()

'$INCLUDE: '..\BI\Library.REC'

    ON ERROR GOTO Errors
    CLS : WIDTH 80, 50

    CALL MhOpenScreenLib("..\AC\AC.SLB" + CHR$(0), e)
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

    CHDIR "..\MPC" + ComCode
    CLOSE : OPEN "Config.MAS" FOR INPUT AS #1
    INPUT #1, ComCode
    INPUT #1, Company
    INPUT #1, Street
    INPUT #1, Address
    INPUT #1, City
    INPUT #1, Payroll
    INPUT #1, TaxID
    INPUT #1, EmpID
    CLOSE

    PR_Init
    PR_Files

    m = 2: c = 1
10  IF c > Loct(m, 2) THEN c = Loct(m, 2)
    CALL MhDisplayByName(0, 0, "AC-Logo", e)
    CALL MhDisplayByName(0, 0, Menu(m), e)
    qPrint Company, 50, 2, 0, 1
20  COLOR 27, 0: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
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
                   SELECT CASE c
                   END SELECT
              CASE 2
                   SELECT CASE c
                          CASE 1:  mprCards
                          CASE 2:  mprSheet
                          CASE 3:  mprRegister
                          CASE 4:  mprChecks
                          CASE 5:  mprChkMan
                          CASE 6:  mprEmpMas
                          CASE 7:  mprChkMas
                          CASE 8:  mprAccMas
                          CASE 9:  mprEmpLst
                          CASE 10: mprChkLst
                          CASE 11: mprW2s
                          CASE 12: mprMonth
                          CASE 13: mprQuarter
                          CASE 14: mprYear
                   END SELECT
              CASE 3
                   SELECT CASE c
                   END SELECT
              CASE 4
                   SELECT CASE c
                   END SELECT
              CASE 5
                   SELECT CASE c
                   END SELECT
              CASE 6
                   SELECT CASE c
                   END SELECT
              CASE 7
                   SELECT CASE c
                          CASE 1: optModify
                          CASE 2: optDate
                          CASE 7: Terminate
                   END SELECT
         END SELECT
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
           CASE ELSE: e$ = ERROR$(ERR)
    END SELECT
    qPrint STR$(ERR) + STR$(ERDEV AND &HFF) + STR$((ERDEV AND &HFF00) \ 256), 17, 42, 0, 1
    qPrint e$, 18, 42, 0, 1
    qPrint ERDEV$, 19, 42, 0, 1
    SOUND 50, 3: KeyPres f$
    PCOPY 1, 0
    RESUME 10
MenuData: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    DATA "GL-Menu",2,14
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    DATA "            "," ",2
    
    DATA "PR-Menu",10,14
    DATA " Update Timecards ","U",11
    DATA " Print Timesheets ","T",17
    DATA " Payroll Register ","R",19
    DATA " Issue Checks     ","I",11
    DATA " Manual Check     ","M",11
    DATA " Employee Master  ","E",11
    DATA " Checks Master    ","M",18
    DATA " Payroll Accounts ","A",19
    DATA " Employee Listing ","L",20
    DATA " Check Listing    ","C",11
    DATA " Print W-2 Forms  ","W",17
    DATA " Month Closing    ","M",11
    DATA " Quarter Closing  ","Q",11
    DATA " Year Closing     ","Y",11

    DATA "AP-Menu",19,18
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20
    DATA "                   "," ",20

    DATA "AR-Menu",29,18
    DATA "                   "," ",36
    DATA "                   "," ",36
    DATA "                   "," ",36
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    DATA "                   "," ",30
    
    DATA "IN-Menu",42,14
    DATA "                  "," ",43
    DATA "                  "," ",43
    DATA "                  "," ",44
    DATA "                  "," ",44
    DATA "                  "," ",43
    DATA "                  "," ",43
    DATA "                  "," ",43
    DATA "                  "," ",43
    DATA "                  "," ",56
    DATA "                  "," ",53
    DATA "                  "," ",43
    DATA "                  "," ",43
    DATA "                  "," ",43
    DATA "                  "," ",43

    DATA "FS-Menu",53,7
    DATA "                  "," ",54
    DATA "                  "," ",54
    DATA "                  "," ",54
    DATA "                  "," ",54
    DATA "                  "," ",54
    DATA "                  "," ",54
    DATA "                  "," ",54

    DATA "OP-Menu",65,7
    DATA " Comapny Info ","C",66
    DATA " System Date  ","D",73
    DATA " Backup Data  ","B",66
    DATA " Restore Data ","R",66
    DATA " Close Month  ","M",72
    DATA " Close Year   ","Y",72
    DATA " Terminate    ","T",66

FUNCTION Confirm (f$)
    r = 40
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
SUB Heading ()
    LPRINT CHR$(12); CHR$(27); "@";
    WIDTH "LPT1:", 255
    LPRINT CHR$(27); "M";
    PageNum = PageNum + 1: LineNum = 13
    LPRINT "Date: "; DATE$; TAB(77 - LEN(RTRIM$(Company)) \ 2); Company; TAB(153); "Page: "; PageNum
    LPRINT "Time: "; TIME$; TAB(77 - LEN(RTRIM$(Street)) \ 2); Street
    LPRINT TAB(77 - LEN(RTRIM$(Address)) \ 2); Address
    LPRINT TAB(77 - LEN(RTRIM$(City)) \ 2); City
    LPRINT
    LPRINT
END SUB

SUB KeyPres (f$)
    DO: LOOP WHILE INKEY$ > ""
    DO: f$ = INKEY$: LOOP WHILE f$ = ""
    f$ = UCASE$(f$)
END SUB

DEFINT A-Z
SUB optDate ()
    StatLin "Reset System Date"
    CALL MhDisplayByName(0, 0, "AC-Date", e)
    COLOR 0, 3: LOCATE 12, 51: PRINT DATE$
    f$ = DATE$
    EntryD 14, 51, e, b, f$: IF e OR b THEN EXIT SUB
    IF f$ > "0" THEN DATE$ = f$
END SUB

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
    SHELL "DEL Payroll.REG"
    CHDIR "..\AC"
    CLS
    END
END SUB

DEFINT A-Z
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

