DEFINT A-Z
DECLARE SUB GL_Files ()
DECLARE SUB JrnEnt (e)
DECLARE SUB mJrnNew ()
DECLARE SUB mJrnEdt ()
DECLARE SUB mActMas ()
DECLARE SUB mJrnRpt ()
DECLARE SUB mJrnAct ()
DECLARE SUB JrnDisp ()
DECLARE SUB JrnPrt (f$, t$)
DECLARE SUB JrnAct (f$, t$)
DECLARE SUB Display ()
DECLARE SUB HSummary ()
DECLARE SUB HJournal ()
DECLARE SUB ShowSto (r, c)

DECLARE FUNCTION FindJrn (n&)

REM $INCLUDE: '..\Include\Library.REC'
REM $INCLUDE: '..\Include\Journal.REC'

    DIM SHARED Jrn AS Journal
    DIM SHARED Jnx AS JrnInx
    DIM SHARED JrnNum AS LONG
    DIM SHARED Jn AS INTEGER
    DIM SHARED Jx AS INTEGER

    DIM SHARED oSt    AS STRING * 2
    DIM SHARED oAc(9) AS STRING * 2
    DIM SHARED oAm(9) AS STRING * 8
    DIM SHARED oTy(9) AS STRING * 1

    DIM SHARED Days(12) AS INTEGER
    
   'clear all accounts--------------------------------
   'GL_Files
   'FOR x = 1 TO 99
   'GET #1, x, Acc
   'FOR y = 0 TO 99
   '    Acc.Mbb(y) = 0: Acc.Mdb(y) = 0: Acc.Mcr(y) = 0
   '    Acc.Ybb(y) = 0: Acc.Ydb(y) = 0: Acc.Ycr(y) = 0
   'NEXT
   'PUT #1, x, Acc
   'NEXT
   'CLOSE
   '--------------------------------------------------

DEFSNG A-Z
SUB Display ()
    COLOR 10, 0
    qPrint Acc.Num, 8, 49, 0, 1
    qPrint Acc.Nam, 9, 49, 0, 1
    qPrint Acc.Crd, 10, 49, 0, 1
    qPrint Acc.Typ, 12, 49, 0, 1
    LOCATE 20, 53: PRINT USING "#########.##"; Acc.Mbb / 100;
    LOCATE 20, 67: PRINT USING "#########.##"; Acc.Ybb / 100;
    LOCATE 21, 53: PRINT USING "#########.##"; Acc.Mdb / 100;
    LOCATE 21, 67: PRINT USING "#########.##"; Acc.Ydb / 100;
    LOCATE 22, 53: PRINT USING "#########.##"; Acc.Mcr / 100;
    LOCATE 22, 67: PRINT USING "#########.##"; Acc.Ycr / 100;
    LOCATE 23, 53: PRINT USING "#########.##"; (Acc.Mbb + Acc.Mdb - Acc.Mcr) / 100;
    LOCATE 23, 67: PRINT USING "#########.##"; (Acc.Ybb + Acc.Ydb - Acc.Ycr) / 100;
END SUB

DEFINT A-Z
FUNCTION FindJrn (n&)
   b = 1: t = LOF(2) / LEN(Jrn)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #2, m, Jrn
      IF n& = Jrn.Num THEN FindJrn = m: EXIT DO
      IF n& > Jrn.Num THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

DEFSNG A-Z
SUB GL_Files ()
   CLOSE #1:  OPEN "..\Data\Accounts.MAS" FOR RANDOM LOCK READ WRITE AS #1 LEN = LEN(Acc)
   CLOSE #2:  OPEN "..\Data\Journal .MAS" FOR RANDOM LOCK READ WRITE AS #2 LEN = LEN(Jrn)
   CLOSE #3:  OPEN "..\Data\Journal .INX" FOR RANDOM LOCK READ WRITE AS #3 LEN = LEN(Jnx)
   CLOSE #11: OPEN "LPT1:" FOR OUTPUT AS #11
END SUB

SUB HJournal ()
    Heading
    PRINT #11, TAB(72); "JOURNAL TRANSACTIONS"
    PRINT #11,
    PRINT #11, " ENTRY   TRANSACTION                                           ACC"
    PRINT #11, "  NUM       DATE               TRANSACTION NOTES               NUM          ACCOUNT NAME               DEBIT         CREDIT   "
    PRINT #11, "=======  ===========    =================================     ===== ==============================  ============  ============"
END SUB

SUB HSummary ()
    Heading
    PRINT #11, TAB(72); "ACCOUNT TRANSACTIONS"
    PRINT #11,
    PRINT #11, "ACT                                JOURANL"
    PRINT #11, "NUM          ACCOUNT NAME            NUM       DATE        DEBIT         CREDIT   "
    PRINT #11, "===  ============================  =======  ==========  ============  ============"
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
SUB JrnAct (f$, t$)
    StatLin "Account Activity"
    f& = VAL(RIGHT$(f$, 4) + LEFT$(f$, 2) + MID$(f$, 4, 2)) 'from date
    t& = VAL(RIGHT$(t$, 4) + LEFT$(t$, 2) + MID$(t$, 4, 2)) 'to date
    LOCATE 1: CLOSE : SHELL "AC.COM ..\Data\Journal.INX ..\Data\Journal.DAT /L8 /+8,1 /+7,1 /+6,1 /+5,1 /Q"
    GL_Files
    CLOSE #3: OPEN "..\Data\Journal.DAT" FOR RANDOM AS #3 LEN = LEN(Jnx)
    CLOSE #9: OPEN "..\Data\Summary" FOR OUTPUT AS #9
    FOR x = 1 TO LOF(3) / LEN(Jnx)
        IF INKEY$ = CHR$(27) THEN EXIT SUB
        GET #3, x, Jnx
        IF Jnx.Dat >= f& AND Jnx.Dat <= t& THEN
           Jn = FindJrn(Jnx.Num)
           IF Jn = 0 THEN WinMssg "No such transaction": ERROR 200
           GET #2, Jn, Jrn
           FOR r = 0 TO 9
               IF Jrn.Act(r) > "00" THEN
                  PRINT #9, USING "&&&&########"; Jrn.Act(r); Jrn.Dat; Jrn.Amt(r); Jrn.Typ(r); Jrn.Num
               ELSE
                  EXIT FOR
               END IF
           NEXT
        END IF
    NEXT
    CLOSE #9: LOCATE 1
    SHELL "AC.COM ..\Data\Summary ..\Data\Summary.SRT /L31 /+1,12 /Q"
    KILL "..\Data\Summary"
    OPEN "..\Data\Summary.Srt" FOR RANDOM AS #9 LEN = 31
    FIELD #9, 2 AS a$, 10 AS d$, 8 AS m$, 1 AS k$, 8 AS n$, 2 AS c$
    GET #9, 1: pa$ = a$
    PageNum = 0: HSummary
    FOR x = 1 TO LOF(9) / 31
    GET #9, x
    GET #1, VAL(a$), Acc
    IF pa$ <> a$ THEN
       PRINT #11, : PRINT #11, TAB(55); : PRINT #11, USING "  ########,.##"; T1; T2: PRINT #11,
       T3 = T3 + T1: T1 = 0
       T4 = T4 + T2: T2 = 0
       pa$ = a$
       LineNum = LineNum + 3: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HSummary
    END IF
    PRINT #11, USING " & "; Acc.Num; Acc.Nam; n$; d$; "   ";
    IF k$ = "D" THEN
       PRINT #11, m$: T1 = T1 + VAL(m$)
    ELSE
       PRINT #11, SPACE$(14); m$: T2 = T2 + VAL(m$)
    END IF
    LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HSummary
    NEXT
    PRINT #11, : PRINT #11, TAB(55); : PRINT #11, USING "  ########,.##"; T1; T2
    PRINT #11, : PRINT #11, TAB(55); : PRINT #11, USING "  ########,.##"; T3 + T1; T4 + T2
    PRINT #11, CHR$(12);
END SUB

DEFSNG T
SUB JrnDisp ()
    COLOR 15, 2: LOCATE 6, 24: PRINT USING "   #######"; Jrn.Num;
    qPrint Jrn.Dat, 8, 24, 0, 1
    FOR r = 0 TO 9
    oAc(r) = Jrn.Act(r)
    IF oAc(r) > "00" THEN
       oAm(r) = Jrn.Amt(r)
       oTy(r) = Jrn.Typ(r)
       GET #1, VAL(oAc(r)), Acc
       qPrint Acc.Num, r + 14, 14, 0, 1
       qPrint Acc.Nam, r + 14, 17, 0, 1
       IF Jrn.Typ(r) = "D" THEN
          c = 48: T1 = T1 + VAL(oAm(r))
       ELSE
          c = 59: T2 = T2 + VAL(oAm(r))
       END IF
       qPrint oAm(r), r + 14, c, 0, 1
    ELSE
       EXIT FOR
    END IF
    NEXT
    LOCATE 25, 47: PRINT USING "######.##"; T1;
    LOCATE 25, 58: PRINT USING "######.##"; T2;
    qPrint Jrn.Dsc, 25, 14, 0, 1
END SUB

DEFDBL T
SUB JrnEnt (e)
100 COLOR 15, 2: LOCATE 6, 24: PRINT USING "  ####### "; Jrn.Num;
110 EntryD 8, 24, e, b, Jrn.Dat:        IF e THEN EXIT SUB ELSE IF b THEN 110
    IF Jrn.Dat < "!" THEN Jrn.Dat = DATE$: GOTO 110
    r = 0
112 EntryN r + 14, 14, 2, 0, e, b, Jrn.Act(r): IF e THEN EXIT SUB
    IF b THEN r = r - 1: IF r < 0 THEN 110 ELSE 112
    IF Jrn.Act(r) = " 0" THEN IF WinAcct THEN 112 ELSE Jrn.Act(r) = Acc.Num
    IF Jrn.Act(r) = "  " THEN
       IF Jrn.Typ(r) = "D" THEN T1 = T1 - VAL(Jrn.Amt(r)) ELSE T2 = T2 - VAL(Jrn.Amt(r))
       FOR x = r TO 9
           Jrn.Act(x) = Jrn.Act(x + 1)
           Jrn.Amt(x) = Jrn.Amt(x + 1)
           Jrn.Typ(x) = Jrn.Typ(x + 1)
       NEXT
       IF r < 10 THEN CALL scroll(14, r + 14, 67, 23, 1)
       COLOR 8, 7: LOCATE 23, 14: PRINT "  ³"; SPC(29); "³"; SPC(10); "³"; SPC(9);
       COLOR 14, 7
       LOCATE 25, 47: PRINT USING "######.##"; T1
       LOCATE 25, 58: PRINT USING "######.##"; T2
       IF SCREEN(r + 14, 14) = 32 THEN 120 ELSE 112
    END IF
    GET #1, VAL(Jrn.Act(r)), Acc
    IF Acc.Nam < "!" THEN IF WinAcct THEN 112 ELSE Jrn.Act(r) = Acc.Num
    Jrn.Act(r) = Acc.Num
    qPrint Acc.Num, r + 14, 14, 0, 1
    qPrint Acc.Nam, r + 14, 17, 0, 1
    IF Jrn.Typ(r) = "C" THEN 116
114 EntryN r + 14, 48, 8, 2, e, b, Jrn.Amt(r): IF e THEN EXIT SUB ELSE IF b THEN 112
    IF VAL(Jrn.Amt(r)) = 0 THEN 116 ELSE Jrn.Typ(r) = "D": GOTO 118
116 EntryN r + 14, 59, 8, 2, e, b, Jrn.Amt(r): IF e THEN EXIT SUB ELSE IF b THEN 112
    IF VAL(Jrn.Amt(r)) = 0 THEN Jrn.Typ(r) = "D": GOTO 114 ELSE Jrn.Typ(r) = "C"
118 T1 = 0: T2 = 0
    FOR x = 0 TO 9
        IF Jrn.Typ(x) = "D" THEN T1 = T1 + VAL(Jrn.Amt(x)) ELSE T2 = T2 + VAL(Jrn.Amt(x))
    NEXT
    COLOR 14, 7
    LOCATE 25, 47: PRINT USING "######.##"; T1
    LOCATE 25, 58: PRINT USING "######.##"; T2
    r = r + 1: IF r < 10 THEN 112
120 EntryS 25, 14, 32, e, b, Jrn.Dsc: IF e THEN EXIT SUB ELSE IF b THEN 110
    Picture 25, 47, 9, T1$
    Picture 25, 58, 9, T2$
    IF T1$ <> T2$ THEN WinMssg "Unbalanced entry!": GOTO 120
    IF Confirm("SAVE Journal Entry") = 0 THEN 120
    FOR r = 0 TO 9
    IF Jrn.Act(r) > "00" THEN
       LOCK #1, VAL(Jrn.Act(r)) : GET #1, VAL(Jrn.Act(r)), Acc
       IF Jrn.Typ(r) = "D" THEN
          Acc.Mdb = Acc.Mdb + VAL(Jrn.Amt(r)) * 100
          Acc.Ydb = Acc.Ydb + VAL(Jrn.Amt(r)) * 100
       ELSE
          Acc.Mcr = Acc.Mcr + VAL(Jrn.Amt(r)) * 100
          Acc.Ycr = Acc.Ycr + VAL(Jrn.Amt(r)) * 100
       END IF
       PUT #1, VAL(Jrn.Act(r)), Acc: UNLOCK #1, VAL(Jrn.Act(r))
    ELSE
       EXIT FOR
    END IF
    NEXT
    Jrn.Tot = T1 * 100
    Jnx.Num = Jrn.Num
    Jnx.Dat = VAL(RIGHT$(Jrn.Dat, 4) + LEFT$(Jrn.Dat, 2) + MID$(Jrn.Dat, 4, 2))
    PUT #2, Jn, Jrn
    PUT #3, Jx, Jnx

    IF oAc(0) > "00" THEN
       FOR r = 0 TO 9
       IF oAc(r) > "00" THEN
          LOCK #1, VAL(oAc(r)) : GET #1, VAL(oAc(r)), Acc
          IF oTy(r) = "D" THEN
             Acc.Mdb = Acc.Mdb - VAL(oAm(r)) * 100
             Acc.Ydb = Acc.Ydb - VAL(oAm(r)) * 100
          ELSE
             Acc.Mcr = Acc.Mcr - VAL(oAm(r)) * 100
             Acc.Ycr = Acc.Ycr - VAL(oAm(r)) * 100
          END IF
          PUT #1, VAL(oAc(r)), Acc: : UNLOCK #1, VAL(oAc(r))
       ELSE
          EXIT FOR
       END IF
       NEXT
    END IF
END SUB

DEFSNG A-Z
SUB JrnPrt (f$, t$)
    StatLin "Journal Entries for " + s$
    f& = VAL(RIGHT$(f$, 4) + LEFT$(f$, 2) + MID$(f$, 4, 2)) 'from date
    t& = VAL(RIGHT$(t$, 4) + LEFT$(t$, 2) + MID$(t$, 4, 2)) 'to date
    LOCATE 1: CLOSE : SHELL "AC.COM ..\Data\Journal.INX ..\Data\Journal.DAT /L8 /+8,1 /+7,1 /+6,1 /+5,1 /Q"
    GL_Files
    CLOSE #3: OPEN "..\Data\Journal.DAT" FOR RANDOM AS #3 LEN = LEN(Jnx)
    PageNum = 0: HJournal
    FOR x = 1 TO LOF(3) / LEN(Jnx)
        IF INKEY$ = CHR$(27) THEN PRINT #11, CHR$(12); : EXIT SUB
        GET #3, x, Jnx
        IF Jnx.Dat >= f& AND Jnx.Dat <= t& THEN
           Jn = FindJrn(Jnx.Num)
           IF Jn = 0 THEN WinMssg "No such transaction": ERROR 200
           GET #2, Jn, Jrn
           PRINT #11, USING "#######   &    &"; Jrn.Num; Jrn.Dat; Jrn.Dsc;
           FOR r = 0 TO 9
               IF Jrn.Act(r) > "00" THEN
                  GET #1, VAL(Jrn.Act(r)), Acc
                  PRINT #11, TAB(61);
                  PRINT #11, USING "   &"; Acc.Num; Acc.Nam;
                  IF Jrn.Typ(r) = "D" THEN
                     PRINT #11, SPACE$(9); Jrn.Amt(r); : T1 = T1 + VAL(Jrn.Amt(r))
                  ELSE
                     PRINT #11, SPACE$(23); Jrn.Amt(r); : T2 = T2 + VAL(Jrn.Amt(r))
                  END IF
                  LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HJournal
               ELSE
                  EXIT FOR
               END IF
           NEXT
           PRINT #11,
        END IF
    NEXT
    PRINT #11, : PRINT #11, TAB(99);
    PRINT #11, USING "  #########.##"; T1; T2
    PRINT #11, CHR$(12);
END SUB

DEFINT A-Z
SUB mglActMas ()
    CALL MhDisplayByName(0, 0, "GL-ActMas", e)
    StatLin "Chart of Accounts Master"
    SubMenu 1, "D"
    b = 1: n = 1: r = 3: GOSUB 750
700 GET #1, n, Acc
    COLOR 15, 1: LOCATE r, 2, 0: PRINT Acc.Num; "³"; Acc.Nam;
    KeyPres n$
    COLOR 1, 7: LOCATE r, 2, 0:  PRINT Acc.Num; "³"; Acc.Nam;
    IF n$ = CHR$(13) THEN SubMenu 0, n$
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        EXIT SUB
    CASE CHR$(0) + "M"  '===============right==
        SubMenu 0, n$
    CASE CHR$(0) + "K"  '===============left===
        SubMenu 0, n$
    CASE CHR$(0) + "P"  '===============down===
        IF n + 1 > 99 THEN 700 ELSE n = n + 1
        IF r + 1 > 47 THEN CALL scroll(2, 3, 31, 47, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"  '===============up=====
        IF n - 1 < 1 THEN 700 ELSE n = n - 1
        IF r - 1 < 3 THEN CALL BkScroll(2, 3, 31, 47, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" '================PgUp===
        b = n - r - 42: IF b < 1 THEN b = 1
        GOSUB 750: n = x - 48 + r
    CASE CHR$(0) + "Q" '================PgDn===
        b = 48 - r + n
        IF b > 55 THEN b = 55: n = r + 52 ELSE n = n + 45
        GOSUB 750
    CASE CHR$(0) + "G" '================Home===
        b = 1: GOSUB 750: n = r - 2
    CASE CHR$(0) + "O" '================End====
        b = 55: GOSUB 750: n = r + 52
    CASE "D"
        SubMenu 0, n$
        Display
    CASE "E"
        SubMenu 0, n$
        Display
        IF (Acc.Mbb + Acc.Mdb + Acc.Mcr + Acc.Ybb + Acc.Ydb + Acc.Ycr) <> 0 THEN
           WinMssg "No ERASE, Active account"
        ELSE
           IF Confirm("ERASE Account") THEN
              Acc.Nam = "                           "
              Acc.Typ = " ": Acc.Crd = "  "
              Acc.Mbb = 0: Acc.Mdb = 0: Acc.Mcr = 0
              Acc.Ybb = 0: Acc.Ydb = 0: Acc.Ycr = 0
              PUT #1, n, Acc
           END IF
        END IF
    CASE "M"
        SubMenu 0, n$
        Display
710     EntryS 9, 49, 27, e, b, Acc.Nam: IF e THEN 700 ELSE IF b THEN 710
        IF Acc.Nam < "!" THEN WinMssg "Account name is invalid": GOTO 710
712     EntryS 10, 49, 2, e, b, Acc.Crd:  IF e THEN 700 ELSE IF b THEN 710
        IF Acc.Crd < "00" OR Acc.Crd > "99" THEN WinMssg "Liability account N§": GOTO 712
714     EntryN 12, 49, 1, 0, e, b, Acc.Typ: IF e THEN 700 ELSE IF b THEN 712
        IF INSTR("12345", Acc.Typ) = 0 THEN WinMssg "Choose 1,2,3,4 or 5": GOTO 714
        Picture 20, 53, 12, Mbb$: Picture 20, 67, 12, Ybb$
        Picture 21, 53, 12, Mdb$: Picture 21, 67, 12, Ydb$
        Picture 22, 53, 12, Mcr$: Picture 22, 67, 12, Ycr$
720     EntryN 20, 53, 12, 2, e, b, Mbb$: IF e THEN 700 ELSE IF b THEN 720
722     EntryN 21, 53, 12, 2, e, b, Mdb$: IF e THEN 700 ELSE IF b THEN 720
724     EntryN 22, 53, 12, 2, e, b, Mcr$: IF e THEN 700 ELSE IF b THEN 722
        LOCATE 23, 53: PRINT USING "#########.##"; VAL(Mbb$) + VAL(Mdb$) - VAL(Mcr$);
726     EntryN 20, 67, 12, 2, e, b, Ybb$: IF e THEN 700 ELSE IF b THEN 724
728     EntryN 21, 67, 12, 2, e, b, Ydb$: IF e THEN 700 ELSE IF b THEN 726
730     EntryN 22, 67, 12, 2, e, b, Ycr$: IF e THEN 700 ELSE IF b THEN 728
        LOCATE 23, 67: PRINT USING "#########.##"; VAL(Ybb$) + VAL(Ydb$) - VAL(Ycr$);

        Picture 23, 53, 12, Meb$: Picture 23, 67, 12, Yeb$
        IF Meb$ <> Yeb$ THEN WinMssg "Unbalanced": GOTO 730

        IF Confirm("UPDATE Account") THEN
           Acc.Mbb = VAL(Mbb$) * 100: Acc.Ybb = VAL(Ybb$) * 100
           Acc.Mdb = VAL(Mdb$) * 100: Acc.Ydb = VAL(Ydb$) * 100
           Acc.Mcr = VAL(Mcr$) * 100: Acc.Ycr = VAL(Ycr$) * 100
           PUT #1, VAL(Acc.Num), Acc
        ELSE
           GOTO 730
        END IF
    END SELECT
    GOTO 700
750 COLOR 1, 7: LOCATE 2
    FOR x = b TO b + 44
    GET #1, x, Acc
    LOCATE CSRLIN + 1, 2: PRINT Acc.Num; "³"; Acc.Nam;
    NEXT
    RETURN
END SUB

SUB mglJrnAct ()
    StatLin "Print Account Activities"
    IF RprtDate(f$, t$) THEN EXIT SUB
    JrnAct f$, t$
END SUB

SUB mglJrnEdt ()
    StatLin "Edit Journal Entry"
    f$ = "          "
    ERASE oAc, oAm, oTy
    CALL MhDisplayByName(0, 0, "GL-Journal", e)
200 EntryN 6, 24, 10, 0, e, b, f$: IF e THEN EXIT SUB ELSE IF b THEN 200
    Jn = FindJrn(VAL(f$))
    IF Jn = 0 THEN WinMssg "No such transaction": GOTO 200
    Jx = Jn
    GET #2, Jn, Jrn
    JrnDisp
    JrnEnt e
    IF e = 0 THEN mglJrnEdt
END SUB

SUB mglJrnMas ()
    StatLin "General Journal Master"
    CALL MhDisplayByName(0, 0, "GL-JrnMas", e)
    LOCATE 1: CLOSE : SHELL "AC.COM ..\Data\Journal.INX ..\Data\Journal.DAT /L8 /+8,1 /+7,1 /+6,1 /+5,1 /Q"
    
    GL_Files
    CLOSE #3: OPEN "..\Data\Journal.DAT" FOR RANDOM AS #3 LEN = LEN(Jnx)
    SubMenu 1, "D"
    r = 3: b = 1: t = LOF(3) \ LEN(Jnx): GOSUB 704
    IF t = 0 THEN WinMssg "Sorry no Journal Entries": EXIT SUB
702 GET #3, n, Jnx
    Jn = FindJrn(Jnx.Num)
    IF Jn = 0 THEN WinMssg "No such transaction": ERROR 200
    GET #2, Jn, Jrn
    COLOR 17, 7: LOCATE r, 2, 0: PRINT USING " ####### ³ & ³ ######.## ³ & "; Jrn.Num; Jrn.Dat; Jrn.Tot / 100; Jrn.Dsc;
    KeyPres n$
    COLOR 27, 1: LOCATE r, 2, 0: PRINT USING " ####### ³ & ³ ######.## ³ & "; Jrn.Num; Jrn.Dat; Jrn.Tot / 100; Jrn.Dsc;
    IF n$ = CHR$(13) THEN SubMenu 0, n$
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        EXIT SUB
    CASE CHR$(0) + "M"  '===============right==
        SubMenu 0, n$
    CASE CHR$(0) + "K"  '===============left===
        SubMenu 0, n$
    CASE CHR$(0) + "P"  '===============down===
        IF n + 1 > t THEN 702 ELSE n = n + 1
        IF r + 1 > 47 THEN CALL scroll(3, 3, 69, 47, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"  '===============up=====
        IF n - 1 < 1 THEN 702 ELSE n = n - 1
        IF r - 1 < 3 THEN CALL BkScroll(2, 3, 69, 47, 1) ELSE r = r - 1
    CASE CHR$(0) + "I"  '===============PgUp===
        b = n - r - 42: IF b < 1 THEN b = 1
        GOSUB 704
    CASE CHR$(0) + "Q"  '===============PgDn===
        b = n - r + 48: IF b > t - 44 THEN b = t - 44
        IF b < 1 THEN b = 1
        GOSUB 704
    CASE CHR$(0) + "G"  '===============Home===
        b = 1: GOSUB 704
    CASE CHR$(0) + "O"  '===============End====
        b = t - 44: IF b < 1 THEN b = 1
        GOSUB 704
    CASE "D"
        SubMenu 0, n$
        PCOPY 0, 2: CALL MhDisplayByName(0, 0, "GL-Journal", e)
        JrnDisp
        KeyPres f$
        PCOPY 2, 0
    CASE "M"
        SubMenu 0, n$
        PCOPY 0, 2: CALL MhDisplayByName(0, 0, "GL-Journal", e)
        GL_Files
        Jx = Jn
        JrnDisp
        JrnEnt e
        LOCATE 1: CLOSE : SHELL "AC.COM ..\Data\Journal.INX ..\Data\Journal.DAT /L8 /+8,1 /+7,1 /+6,1 /+5,1 /Q"
        GL_Files
        CLOSE #3: OPEN "..\Data\Journal.DAT" FOR RANDOM AS #3 LEN = LEN(Jnx)
        PCOPY 2, 0
    END SELECT
    GOTO 702
704 COLOR 27, 1: LOCATE 3
    FOR x = b TO t
        GET #3, x, Jnx
        Jn = FindJrn(Jnx.Num)
        IF Jn = 0 THEN WinMssg "No such transaction": ERROR 200
        GET #2, Jn, Jrn
        LOCATE , 2: PRINT USING " ####### ³ & ³ ######.## ³ & "; Jrn.Num; Jrn.Dat; Jrn.Tot / 100; Jrn.Dsc
        IF CSRLIN > 47 THEN EXIT FOR
    NEXT
    n = b + r - 3
    RETURN
END SUB

SUB mglJrnNew ()
    StatLin "Make New Journal Entry"
    oSt = ""
    ERASE oAc, oAm, oTy
    IF LOF(2) / LEN(Jrn) > 0 THEN GET #2, LOF(2) / LEN(Jrn), Jrn
    JrnNum = Jrn.Num + 1
    GET #2, JrnNum, Jrn
    Jn = 1 + LOF(2) / LEN(Jrn)
    Jx = Jn
    Jrn.Num = JrnNum
    CALL MhDisplayByName(0, 0, "GL-Journal", e)
    JrnEnt e
    IF e = 0 THEN mglJrnNew
END SUB

DEFSNG A-Z
SUB mglJrnRpt ()
    StatLin "Print Journal Entries"
    IF RprtDate(f$, t$) THEN EXIT SUB
    JrnPrt f$, t$
END SUB

DEFINT A-Z
SUB mglPurge ()
    CALL MhDisplayByName(0, 0, "OP-Purge", e)
    StatLin "Purge Journal Entries"

p1: EntryD 12, 43, e, b, d$: IF e THEN EXIT SUB ELSE IF b GOTO p1
    IF Confirm("PURGE Journal Entries") = 0 THEN EXIT SUB
    d$ = RIGHT$(d$, 4) + LEFT$(d$, 2) + MID$(d$, 4, 2)
    CLOSE #4: OPEN "..\Data\Journal.M" FOR RANDOM SHARED AS #4 LEN = LEN(Jrn)
    CLOSE #5: OPEN "..\Data\Journal.X" FOR RANDOM SHARED AS #5 LEN = LEN(Jnx)
    Top = LOF(2) / LEN(Jrn)
    FOR x = 1 TO Top
       GET #2, x, Jrn
       IF INKEY$ = CHR$(27) THEN EXIT SUB
       LOCATE 50, 40: PRINT USING "##### ###.##%"; n; (x / Top) * 100;
       i$ = RIGHT$(Jrn.Dat, 4) + LEFT$(Jrn.Dat, 2) + MID$(Jrn.Dat, 4, 2)
       IF i$ > d$ THEN
          n = n + 1
          Jnx.Num = Jrn.Num
          Jnx.Dat = VAL(RIGHT$(Jrn.Dat, 4) + LEFT$(Jrn.Dat, 2) + MID$(Jrn.Dat, 4, 2))
          PUT #4, n, Jrn
          PUT #5, n, Jnx
       END IF
    NEXT
    CLOSE
    KILL "..\Data\Journal.MAS": NAME "..\Data\Journal.M" AS "..\Data\Journal.MAS"
    KILL "..\Data\Journal.INX": NAME "..\Data\Journal.X" AS "..\Data\Journal.INX"
END SUB

