DEFINT A-Z

DECLARE SUB StatLin (f$)
DECLARE SUB AccLine ()
DECLARE SUB Display ()
DECLARE SUB HSummary ()
DECLARE SUB HJournal ()

DECLARE SUB WinAcc (f$)

'$INCLUDE: '..\BI\Library.REC'
'$INCLUDE: '..\BI\Accounts.REC'
'$INCLUDE: '..\BI\Journal.REC'

    DIM SHARED Acc AS Accounts
    DIM SHARED Jrn AS Journal

    DIM SHARED oAc(10) AS STRING * 4
    DIM SHARED oAm(10) AS STRING * 10
    DIM SHARED oTy(10) AS STRING * 1

   
Journal: '==================================================================
    T1 = 0: T2 = 0: PageNum = 0: HJournal
    FOR x = FromEnt TO LOF(2) / LEN(Jrn)
    GET #2, x, Jrn
    LPRINT " "; RIGHT$(STR$(100000 + x), 5); "    "; Jrn.Dat;
    z = 1
    FOR r = 1 TO 10
    
    IF Act$ > "00" THEN
       GET #1, VAL(Act$), Acc
       LPRINT TAB(25);
       LPRINT USING "&    "; Acc.Num; Acc.Nam; "";
       IF Typ$ = "D" THEN LPRINT Amt$: T1 = T1 + VAL(Amt$) ELSE LPRINT SPACE$(14); Amt$: T2 = T2 + VAL(Amt$)
       LineNum = LineNum + 1: IF LineNum > 60 THEN HJournal
    ELSE
       EXIT FOR
    END IF
    z = z + 11
    NEXT
    LPRINT TAB(31); : LPRINT : LPRINT : LineNum = LineNum + 2
    NEXT
    LPRINT : LPRINT TAB(60);
    LPRINT USING "    #######.##"; T1; T2
    RETURN
NewSum: '==================================================================
    IF LastEnt = LOF(2) / LEN(Jrn) THEN WinMssg "No new entries to list"
    LOCATE 15, 30: PRINT "List New Entries Summary."
    FromEnt = LastEnt + 1: GOSUB Summary:
MonSum: '==================================================================
    IF EndEnt = LOF(2) / LEN(Jrn) THEN WinMssg "No new entries to list":
    LOCATE 17, 30: PRINT "List All Entries Summary."
    FromEnt = EndEnt + 1: GOSUB Summary:
Summary: '==================================================================
    CLOSE #3: OPEN "Summary" FOR OUTPUT AS #3
    FOR x = FromEnt TO LOF(2) / LEN(Jrn)
    GET #2, x, Jrn
    z = 1
    FOR r = 1 TO 10
    IF Act$ > "00" THEN
       PRINT #3, USING "&&&&&"; Act$; Jrn.Dat; Amt$; Typ$; RIGHT$(STR$(100000 + x), 5)
    ELSE
       EXIT FOR
    END IF
    z = z + 11
    NEXT
    NEXT
    CLOSE #3: SHELL "SortF Summary Summary.Srt /L28 /Q": KILL "Summary"
    PageNum = 0: HSummary
    OPEN "Summary.Srt" FOR RANDOM AS #3 LEN = 28
    FIELD #3, 2 AS A1$, 10 AS B1$, 8 AS C1$, 1 AS D1$, 5 AS E1$, 2 AS F1$
    T1 = 0: T2 = 0: T3 = 0: T4 = 0
    GET #3, 1: p$ = A1$
    FOR x = 1 TO LOF(3) / 28
    GET #3, x: GET #1, VAL(A1$), Acc
    IF p$ <> A1$ THEN
       LPRINT : LPRINT TAB(62); : LPRINT USING "    #######.##"; T1; T2: LPRINT
       T3 = T3 + T1: T4 = T4 + T2: T1 = 0: T2 = 0: p$ = A1$
       LineNum = LineNum + 3: IF LineNum > 60 THEN HSummary
    END IF
    LPRINT USING " &   "; Acc.Num; Acc.Nam; E1$; B1$; "   ";
    IF D1$ = "D" THEN LPRINT C1$: T1 = T1 + VAL(C1$) ELSE LPRINT SPACE$(14); C1$: T2 = T2 + VAL(C1$)
    LineNum = LineNum + 1: IF LineNum > 60 THEN HSummary
    NEXT
    LPRINT : LPRINT TAB(62); : LPRINT USING "    #######.##"; T1; T2
    LPRINT : LPRINT TAB(62); : LPRINT USING "    #######.##"; T1 + T3; T2 + T4
    RETURN
Account: '==================================================================
    
    b = 1: n = 1: r = 2: GOSUB 750
700 GET #1, n, Acc
    COLOR 15, 1: LOCATE r, 2, 0: PRINT Acc.Num; "ณ"; Acc.Nam;
    KeyPres n$
    COLOR 1, 7: LOCATE r, 2, 0:  PRINT Acc.Num; "ณ"; Acc.Nam;
   
    SELECT CASE n$
    CASE CHR$(27) 'Esc
        
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > 99 THEN 700 ELSE n = n + 1
        IF r + 1 > 24 THEN CALL scroll(2, 2, 31, 24, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 THEN 700 ELSE n = n - 1
        IF r - 1 < 2 THEN CALL BkScroll(2, 2, 31, 24, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        b = n - r - 21: IF b < 1 THEN b = 1
        GOSUB 750: n = x - 25 + r
    CASE CHR$(0) + "Q" 'PgDn
        b = 25 - r + n
        IF b > 77 THEN b = 77: n = r + 75 ELSE n = n + 23
        GOSUB 750
    CASE CHR$(0) + "G" 'Home
        b = 1: GOSUB 750: n = r - 1
    CASE CHR$(0) + "O" 'End
        b = 77: GOSUB 750: n = r + 75
    CASE "D"
        AccLine
    CASE "E"
        AccLine
        IF Confirm("ERASE Account") THEN
           Acc.Nam = "                           "
           Acc.Typ = " ": Acc.Crd = "  "
           Acc.Mbb = 0: Acc.Mdb = 0: Acc.Mcr = 0
           Acc.Ybb = 0: Acc.Ydb = 0: Acc.Ycr = 0
           PUT #1, n, Acc
        END IF
    CASE "M"
        AccLine
710     EntryS 3, 49, 27, e, b, Acc.Nam: IF e THEN 700 ELSE IF b THEN 710
        IF Acc.Nam < "!" THEN WinMssg "Account name is invalid": GOTO 710
712     EntryS 5, 49, 2, e, b, Acc.Crd:  IF e THEN 700 ELSE IF b THEN 710
        IF Acc.Crd < "00" OR Acc.Crd > "99" THEN WinMssg "Liability account Nง": GOTO 712
714     EntryN 7, 49, 1, 0, e, b, Acc.Typ: IF e THEN 700 ELSE IF b THEN 712
        IF INSTR("12345", Acc.Typ) = 0 THEN WinMssg "Choose 1,2,3,4 or 5": GOTO 714
        
        IF f$ = "*" THEN
           PUT #1, VAL(Acc.Num), Acc
        ELSEIF f$ = "U" THEN
           Picture 16, 55, 10, Mbb$: Picture 16, 69, 10, Ybb$
           Picture 17, 55, 10, Mdb$: Picture 17, 69, 10, Ydb$
           Picture 18, 55, 10, Mcr$: Picture 18, 69, 10, Ycr$
720        EntryN 16, 55, 10, 2, e, b, Mbb$: IF e THEN 700 ELSE IF b THEN 720
722        EntryN 17, 55, 10, 2, e, b, Mdb$: IF e THEN 700 ELSE IF b THEN 720
724        EntryN 18, 55, 10, 2, e, b, Mcr$: IF e THEN 700 ELSE IF b THEN 722
726        EntryN 16, 69, 10, 2, e, b, Ybb$: IF e THEN 700 ELSE IF b THEN 724
728        EntryN 17, 69, 10, 2, e, b, Ydb$: IF e THEN 700 ELSE IF b THEN 726
730        EntryN 18, 69, 10, 2, e, b, Ycr$: IF e THEN 700 ELSE IF b THEN 728
           WinMssg "Press [*] to Save Account"
           IF Confirm("SAVE entry") THEN
              Acc.Mbb = VAL(Mbb$) * 100: Acc.Ybb = VAL(Ybb$) * 100
              Acc.Mdb = VAL(Mdb$) * 100: Acc.Ydb = VAL(Ydb$) * 100
              Acc.Mcr = VAL(Mcr$) * 100: Acc.Ycr = VAL(Ycr$) * 100
              PUT #1, VAL(Acc.Num), Acc
           END IF
        ELSE
           GOTO 710
        END IF
    CASE ELSE
        WinMssg "Select from highlited keys"
    END SELECT
    GOTO 700
750 COLOR 1, 7: LOCATE 1
    FOR x = b TO b + 22
    GET #1, x, Acc
    LOCATE CSRLIN + 1, 2: PRINT Acc.Num; "ณ"; Acc.Nam;
    NEXT
    RETURN
Period: '==================================================================
    f$ = ""
    LOCATE 23, 20: PRINT "    Type yes to close end of period.     "
900 EntryS 23, 29, 3, e, b, f$: IF e THEN  ELSE IF b THEN 900
    IF UCASE$(f$) = "YES" THEN
       LOCATE 16, 30: PRINT "List All Journal Entries."
       FromEnt = EndEnt + 1: GOSUB Journal
       LOCATE 17, 30: PRINT "List All Entries Summary."
       FromEnt = EndEnt + 1: GOSUB Summary
       EndEnt = LOF(2) / LEN(Jrn)
    END IF

DEFSNG A-Z
SUB AccLine ()
    COLOR 10, 0
    LOCATE 2, 49: PRINT Acc.Num
    LOCATE 3, 49: PRINT Acc.Nam
    LOCATE 5, 49: PRINT Acc.Crd
    LOCATE 7, 49: PRINT Acc.Typ
    LOCATE 16, 53: PRINT USING "#########.##-"; Acc.Mbb / 100
    LOCATE 16, 67: PRINT USING "#########.##-"; Acc.Ybb / 100
    LOCATE 17, 53: PRINT USING "#########.##-"; Acc.Mdb / 100
    LOCATE 17, 67: PRINT USING "#########.##-"; Acc.Ydb / 100
    LOCATE 18, 53: PRINT USING "#########.##-"; Acc.Mcr / 100
    LOCATE 18, 67: PRINT USING "#########.##-"; Acc.Ycr / 100
    LOCATE 19, 53: PRINT USING "#########.##-"; (Acc.Mbb + Acc.Mdb - Acc.Mcr) / 100
    LOCATE 19, 67: PRINT USING "#########.##-"; (Acc.Ybb + Acc.Ydb - Acc.Ycr) / 100
END SUB

SUB GL_Files ()
    OPEN "Accounts.Mas" FOR RANDOM AS #1 LEN = LEN(Acc)
    OPEN "Journal .Mas" FOR RANDOM AS #2 LEN = LEN(Jrn)
    OPEN "Journal .Inx" FOR RANDOM AS #3 LEN = LEN(Jrx)
END SUB

SUB HJournal ()
    Heading
    LPRINT TAB(69); "JOURNAL  TRANSACTIONS"
    LPRINT : LPRINT
    LPRINT " ENTRY   TRANSACTION   ACC"
    LPRINT "  NUM       DATE       NUM           ACCOUNT NAME               DEBIT         CREDIT   "
    LPRINT "=======  ===========  =====  ==============================  ============  ============"
END SUB

SUB HSummary ()
    Heading
    LPRINT TAB(69); "TRANSACTIONS  SUMMARY"
    LPRINT : LPRINT
    LPRINT " ACC                                  ENTRY   TRANSACTION"
    LPRINT " NUM           ACCOUNT NAME            NUM       DATE             DEBIT         CREDIT   "
    LPRINT "=====  ============================  =======  ===========      ============  ============"
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
SUB mglJourn ()
    CALL MhDisplayByName(0, 0, "gl-Journal", e)
    StatLin "General Journal"
    
100 RecNum$ = RIGHT$(STR$(100000 + VAL(f$)), 5)
    qPrint RecNum$, 10, 5, 0, 1
110 EntryD 10, 12, e, b, Jrn.Dat: IF e THEN 100 ELSE IF b THEN 110
    IF Jrn.Dat < "!" THEN Jrn.Dat = DATE$: GOTO 110
    r = 1
112 EntryN r + 9, 24, 2, 0, e, b, Jrn.Act(r): IF e THEN 100
    IF b THEN r = r - 1: IF r < 1 THEN 110 ELSE 112
    IF Jrn.Act(r) = " 0" THEN WinAcc Jrn.Act(r): IF Jrn.Act(r) = "  " THEN 112
    IF Jrn.Act(r) = "  " THEN
       IF Jrn.Typ(r) = "D" THEN
          T1 = T1 - VAL(Jrn.Amt(r))
       ELSE
          T2 = T2 - VAL(Jrn.Amt(r))
       END IF
       FOR x = r TO 10
           Jrn.Act(x) = Jrn.Act(x + 1)
           Jrn.Amt(x) = Jrn.Amt(x + 1)
           Jrn.Typ(x) = Jrn.Typ(x + 1)
       NEXT
       IF r < 10 THEN CALL scroll(24, r + 9, 77, 19, 1)
       COLOR 3, 0: LOCATE 19, 24: PRINT "   ณ"; SPC(29); "ณ"; SPC(10); "ณ"; SPC(9);
       COLOR 11, 0
       LOCATE 21, 58: PRINT USING "######.##"; T1
       LOCATE 21, 69: PRINT USING "######.##"; T2
       IF SCREEN(r + 9, 24) = 32 THEN 120 ELSE 112
    END IF
    GET #1, VAL(Jrn.Act(r)), Acc
    IF Acc.Nam < "!" THEN WinAcc Jrn.Act(r): GOTO 112
    LOCATE r + 9, 24: PRINT Acc.Num: Jrn.Act(r) = Acc.Num
    LOCATE r + 9, 29: PRINT Acc.Nam
    IF Jrn.Typ(r) = "C" THEN 116
114 EntryN r + 9, 59, 8, 2, e, b, Jrn.Amt(r): IF e THEN 100 ELSE IF b THEN 112
    IF VAL(Jrn.Amt(r)) = 0 THEN 116 ELSE Jrn.Typ(r) = "D": GOTO 118
116 EntryN r + 9, 70, 8, 2, e, b, Jrn.Amt(r): IF e THEN 100 ELSE IF b THEN 112
    IF VAL(Jrn.Amt(r)) = 0 THEN Jrn.Typ(r) = "D": GOTO 114 ELSE Jrn.Typ(r) = "C"
118 T1 = 0
    T2 = 0
    FOR x = 1 TO 10
        IF Jrn.Typ(x) = "D" THEN
           T1 = T1 + VAL(Jrn.Amt(x))
        ELSE
           T2 = T2 + VAL(Jrn.Amt(x))
        END IF
    NEXT
    COLOR 11, 0
    LOCATE 21, 58: PRINT USING "######.##"; T1
    LOCATE 21, 69: PRINT USING "######.##"; T2
    r = r + 1: IF r < 11 THEN 112
120 EntryS 21, 24, 32, e, b, Jrn.Des: IF e THEN 100 ELSE IF b THEN 110
    Picture 21, 58, 9, T1$
    Picture 21, 69, 9, T2$
    IF T1$ <> T2$ THEN 120
       
    IF Confirm("journal entry") = 0 THEN 120
    FOR r = 1 TO 10
    IF Jrn.Act(r) > "00" THEN
       GET #1, VAL(Jrn.Act(r)), Acc
       IF Jrn.Typ(r) = "D" THEN
          Acc.Mdb = Acc.Mdb + VAL(Jrn.Amt(r)) * 100
          Acc.Ydb = Acc.Ydb + VAL(Jrn.Amt(r)) * 100
       ELSE
          Acc.Mcr = Acc.Mcr + VAL(Jrn.Amt(r)) * 100
          Acc.Ycr = Acc.Ycr + VAL(Jrn.Amt(r)) * 100
       END IF
       PUT #1, VAL(Jrn.Act(r)), Acc
    ELSE
       EXIT FOR
    END IF
    NEXT

    PUT #2, VAL(RecNum$), Jrn

    IF oAc(1) > "00" THEN
       FOR r = 1 TO 10
       IF oAc(r) > "00" THEN
          GET #1, VAL(oAc(r)), Acc
          IF oTy(r) = "D" THEN
             Acc.Mdb = Acc.Mdb - VAL(oAm(r)) * 100
             Acc.Ydb = Acc.Ydb - VAL(oAm(r)) * 100
          ELSE
             Acc.Mcr = Acc.Mcr - VAL(oAm(r)) * 100
             Acc.Ycr = Acc.Ycr - VAL(oAm(r)) * 100
          END IF
          PUT #1, VAL(oAc(r)), Acc
       ELSE
          EXIT FOR
       END IF
       NEXT
       GOTO Updat
    END IF
    GOTO entry
END SUB

DEFINT L, T
SUB WinAcc (f$)
    SCREEN , , 1, 1
    PCOPY 0, 1
    Bot = 1: r = 5: n = 1: GOSUB T2
T1: GET #1, n, Acc
    COLOR 15, 1: LOCATE r, 41: PRINT USING "&ณ&"; Acc.Num; Acc.Nam
    KeyHit n$
    COLOR 15, 6: LOCATE r, 41: PRINT USING "&ณ&"; Acc.Num; Acc.Nam
    SELECT CASE n$
    CASE CHR$(13)
        f$ = Acc.Num
        GOTO T3
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > 99 GOTO T1 ELSE n = n + 1
        IF r + 1 > 19 THEN CALL scroll(41, 5, 70, 19, 1) ELSE r = r + 1
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
        f$ = "  ": GOTO T3
    END SELECT
    GOTO T1

T2: COLOR 15, 6: LOCATE 4, 40: PRINT "ษออัอออออออออออออออออออออออออออป"
    FOR x = Bot TO Bot + 14: GET #1, x, Acc
    LOCATE , 40: PRINT USING "บ&ณ&บ"; Acc.Num; Acc.Nam
    NEXT
    LOCATE , 40: PRINT "ศออฯอออออออออออออออออออออออออออผ"
    RETURN
T3: SCREEN , , 0, 0
    COLOR 14, 0
END SUB

