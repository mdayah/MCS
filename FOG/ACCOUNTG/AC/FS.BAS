DEFINT A-Z
DEFLNG L
DECLARE SUB Trial (s$)
DECLARE SUB Sheet (s$)
DECLARE SUB Income (s$)
DECLARE SUB HTrial (s$)
DECLARE SUB HSheet (s$)
DECLARE SUB HIncome (s$)
DECLARE SUB ShowSto (r, c)
DECLARE SUB BalLine (L1&, L2&)
DECLARE SUB IncLine (L1&, L2&, L3&, L4&)
DECLARE SUB ActsTot (s%, M1@, M2@, M3@, M4@, M5@, Y1@, Y2@, Y3@, Y4@, Y5@)
DECLARE FUNCTION PickSto (s$)

REM $INCLUDE: '..\BI\Library.REC'

DEFSNG B-L, N-R, T-X, Z
DEFCUR M, Y
SUB ActsTot (s, M1, M2, M3, M4, M5, Y1, Y2, Y3, Y4, Y5)
    FOR a = 1 TO 99
        GET #1, a, Acc
        SELECT CASE Acc.Typ
               CASE "1" '--------------------------------total assets
                    M1 = M1 - Acc.Mcr(s) + Acc.Mbb(s) + Acc.Mdb(s)
                    Y1 = Y1 - Acc.Ycr(s) + Acc.Ybb(s) + Acc.Ydb(s)
               CASE "2" '--------------------------------total liability
                    M2 = M2 - Acc.Mdb(s) - Acc.Mbb(s) + Acc.Mcr(s)
                    Y2 = Y2 - Acc.Ydb(s) - Acc.Ybb(s) + Acc.Ycr(s)
               CASE "3" '--------------------------------total equity
                    M3 = M3 - Acc.Mdb(s) - Acc.Mbb(s) + Acc.Mcr(s)
                    Y3 = Y3 - Acc.Ydb(s) - Acc.Ybb(s) + Acc.Ycr(s)
               CASE "4" '--------------------------------total revenues
                    M4 = M4 - Acc.Mbb(s) - Acc.Mdb(s) + Acc.Mcr(s)
                    Y4 = Y4 - Acc.Ybb(s) - Acc.Ydb(s) + Acc.Ycr(s)
               CASE "5" '--------------------------------total expenses
                    M5 = M5 - Acc.Mcr(s) + Acc.Mbb(s) + Acc.Mdb(s)
                    Y5 = Y5 - Acc.Ycr(s) + Acc.Ybb(s) + Acc.Ydb(s)
               CASE ELSE
        END SELECT
    NEXT
END SUB

DEFLNG M, Y
DEFSNG A, S
SUB BalLine (M, Y)
    PRINT #11, USING "   ##########,.##"; Y / 100;
    PRINT #11, TAB(73);
    IF M = 0 THEN PRINT #11, "        0" ELSE PRINT #11, USING "   ###.##"; Y / M * 100
END SUB

DEFSNG M, Y
SUB FS_Files ()
   CLOSE #1: OPEN "Accounts.MAS" FOR RANDOM SHARED AS #1 LEN = LEN(Acc)
   CLOSE #11: OPEN "LPT1:" FOR OUTPUT AS #11
   CLOSE #12: OPEN Path + "Stores.MAS" FOR RANDOM ACCESS READ SHARED AS #12 LEN = LEN(Sto)
END SUB

SUB HIncome (s$)
    Heading s$
    PRINT #11, CHR$(27); "(s12H"   'Pitch 12 cpi
    PRINT #11, TAB(42); "INCOME STATEMENT"
    PRINT #11,
    PRINT #11, "                              MONTH-TO-DATE   PERCENT     YEAR-TO-DATE   PERCENT"
    PRINT #11, "                             ===============  =======    ==============  ======="
END SUB

SUB HSheet (s$)
    Heading s$
    PRINT #11, CHR$(27); "(s12H"   'Pitch 12 cpi
    PRINT #11, TAB(43); "BALANCE  SHEET"
    PRINT #11,
END SUB

SUB HTrial (s$)
    Heading s$
    PRINT #11, TAB(74); "TRIAL  BALANCE"
    PRINT #11,
    PRINT #11, "                                            ===================== M O N T H ======================      ====================== Y E A R ======================="
    PRINT #11, "NUM         DESCRIPTION           TYPE      BEG. BALANCE     DEBITS        CREDITS    END. BALANCE      BEG. BALANCE     DEBITS        CREDITS    END. BALANCE"
    PRINT #11, "===  ==========================  ======     ======================================================      ======================================================"
END SUB

DEFLNG M, S, Y
SUB IncLine (SM, SY, M, Y)
    PRINT #11, USING "   ##########,.##"; M / 100;
    IF SM = 0 OR M = 0 THEN PRINT #11, "        0  ";  ELSE PRINT #11, USING "   ###.##  "; M / SM * 100;
    PRINT #11, USING " ###########,.##"; Y / 100;
    IF SY = 0 OR Y = 0 THEN PRINT #11, "        0" ELSE PRINT #11, USING "   ###.##"; Y / SY * 100
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB Income (s$)
    '  LM is sales revenue for the month
    '  LY is sales revenue for the year
    '  L3 is net profit for the month
    '  L4 is net profit for the year
    '
    StatLin "Income Statement for " + s$
    s = VAL(s$)
    GET #1, 50, Acc '----------sales revenue
    LM = Acc.Mcr(s) - Acc.Mdb(s)
    LY = Acc.Ycr(s) - Acc.Ydb(s)
    GET #1, 51, Acc '----------rental revenue
    LM = LM + Acc.Mcr(s) - Acc.Mdb(s)
    LY = LY + Acc.Ycr(s) - Acc.Ydb(s)
    PageNum = 0: HIncome s$
    CLOSE #2: OPEN "Income.PIC" FOR INPUT AS #2
    WHILE NOT EOF(2)
       INPUT #2, f$, k$
       SELECT CASE f$
       CASE "^" '--------------------------------------------page break
          HIncome s$
       CASE "$" '--------------------------------------------title line
          PRINT #11, k$
       CASE "@" '--------------------------------------------underlined title
          PRINT #11, CHR$(27); "&d0D"; 'enable underline
          PRINT #11, k$;
          PRINT #11, CHR$(27); "&d@"   'disable underline
       CASE "+" '--------------increment
          GET #1, VAL(k$), Acc
          PRINT #11, Acc.Nam;
          IF Acc.Typ = "4" THEN
             L1 = L1 + (Acc.Mcr(s) - Acc.Mdb(s))
             L2 = L2 + (Acc.Ycr(s) - Acc.Ydb(s))
             IncLine LM, LY, Acc.Mcr(s) - Acc.Mdb(s), Acc.Ycr(s) - Acc.Ydb(s)
          ELSEIF Acc.Typ = "5" THEN
             L1 = L1 + (Acc.Mdb(s) - Acc.Mcr(s))
             L2 = L2 + (Acc.Ydb(s) - Acc.Ycr(s))
             IncLine LM, LY, Acc.Mdb(s) - Acc.Mcr(s), Acc.Ydb(s) - Acc.Ycr(s)
          ELSE
             WinMssg Acc.Nam + "-Account Type Error"
          END IF
       CASE "-" '--------------decrement
          GET #1, VAL(k$), Acc
          PRINT #11, Acc.Nam;
          IF Acc.Typ = "4" THEN
             L1 = L1 - (Acc.Mcr(s) - Acc.Mdb(s))
             L2 = L2 - (Acc.Ycr(s) - Acc.Ydb(s))
             IncLine LM, LY, Acc.Mcr(s) - Acc.Mdb(s), Acc.Ycr(s) - Acc.Ydb(s)
          ELSEIF Acc.Typ = "5" THEN
             L1 = L1 - (Acc.Mdb(s) - Acc.Mcr(s))
             L2 = L2 - (Acc.Ydb(s) - Acc.Ycr(s))
             IncLine LM, LY, Acc.Mdb(s) - Acc.Mcr(s), Acc.Ydb(s) - Acc.Ycr(s)
          ELSE
             WinMssg Acc.Nam + "-Account Type Error"
          END IF
       CASE "/" '--------------sub total+
          PRINT #11, k$;
          IncLine LM, LY, L1, L2
          L3 = L3 + L1: L4 = L4 + L2: L1 = 0: L2 = 0
       CASE "\" '--------------sub total-
          PRINT #11, k$;
          IncLine LM, LY, L1, L2
          L3 = L3 - L1: L4 = L4 - L2: L1 = 0: L2 = 0
       CASE "*" '--------------grand total
          PRINT #11, k$;
          IncLine LM, LY, L3, L4
       CASE ELSE '-------------code error
          WinMssg "Error in Income Statement Picture"
       END SELECT
    WEND
    PRINT #11, CHR$(12);
END SUB

DEFSNG A-Z
SUB mfsEincome ()
    CLOSE : SHELL "EDIT Income.PIC"
END SUB

SUB mfsEsheet ()
    CLOSE : SHELL "EDIT Sheet.PIC"
END SUB

SUB mfsIncome ()
    IF PickSto(s$) THEN EXIT SUB
    Income s$
END SUB

SUB mfsSheet ()
    IF PickSto(s$) THEN EXIT SUB
    Sheet s$
END SUB

SUB mfsTrial ()
    IF PickSto(s$) THEN EXIT SUB
    Trial s$
END SUB

DEFINT A-Z
FUNCTION PickSto (s$)
    CALL MhDisplayByName(0, 0, "FS-PickSto", e)
    s$ = "00"
t1: EntryU 19, 43, 2, e, b, s$: IF e THEN PickSto = 1: EXIT FUNCTION ELSE IF b GOTO t1
    IF s$ = "00" THEN
       Sto.Nam = "All Stores"
    ELSE
       IF VAL(s$) < 1 THEN IF WinStor GOTO t1 ELSE s$ = Sto.Num
       GET #12, VAL(s$), Sto
       IF Sto.Nam < "!" OR ComCode <> Sto.Cod THEN WinMssg "Not " + ComCode + " Store": GOTO t1
       s$ = Sto.Num
    END IF
    qPrint s$, 19, 43, 0, 1
    ShowSto 19, 46
END FUNCTION

DEFLNG L-M, Y
SUB Sheet (s$)
    StatLin "Balance Sheet for " + s$
    s = VAL(s$)
    ActsTot s, M1@, M2@, M3@, M4@, M5@, Y1@, Y2@, Y3@, Y4@, Y5@
    PageNum = 0: HSheet s$
    CLOSE #2: OPEN "Sheet.PIC" FOR INPUT AS #2
    WHILE NOT EOF(2)
       INPUT #2, f$, k$
       SELECT CASE f$
       CASE "^" '--------------------------------------------page break
          HSheet s$
       CASE "$" '--------------------------------------------title line
          PRINT #11, k$
       CASE "@" '--------------------------------------------underlined title
          PRINT #11, CHR$(27); "&d0D"; 'enable underline
          PRINT #11, k$
          PRINT #11, CHR$(27); "&d@";  'disable underline
       CASE "+" '--------------------------------------------increment
          GET #1, VAL(k$), Acc
          PRINT #11, Acc.Nam;
          SELECT CASE Acc.Typ
          CASE "1"
             L1 = L1 + (Acc.Ybb(s) + Acc.Ydb(s) - Acc.Ycr(s))
             BalLine Y1, Acc.Ybb(s) + Acc.Ydb(s) - Acc.Ycr(s)
          CASE "2"
             L1 = L1 + (Acc.Ycr(s) - Acc.Ydb(s) - Acc.Ybb(s))
             BalLine Y2 + Y3, Acc.Ycr(s) - Acc.Ydb(s) - Acc.Ybb(s)
          CASE "3"
             SELECT CASE Acc.Num
             CASE "47" '----------AAA account
                L1 = L1 + (Acc.Mcr(s) - Acc.Mdb(s) - Acc.Mbb(s))
                BalLine Y2 + Y3, (Acc.Mcr(s) - Acc.Mdb(s) - Acc.Mbb(s))
             CASE "48" '----------P/L account
                L1 = L1 + M4 - M5
                BalLine Y2 + Y3, M4 - M5
             CASE ELSE
                L1 = L1 + (Acc.Ycr(s) - Acc.Ydb(s) - Acc.Ybb(s))
                BalLine Y2 + Y3, Acc.Ycr(s) - Acc.Ydb(s) - Acc.Ybb(s)
             END SELECT
          CASE ELSE
             WinMssg Acc.Nam + "-Account Type Error"
          END SELECT
       CASE "-" '--------------------------------------------decrement
          GET #1, VAL(k$), Acc
          PRINT #11, Acc.Nam;
          SELECT CASE Acc.Typ
          CASE "1"
             L1 = L1 - (Acc.Ybb(s) + Acc.Ydb(s) - Acc.Ycr(s))
             BalLine Y1, Acc.Ybb(s) + Acc.Ydb(s) - Acc.Ycr(s)
          CASE "2"
             L1 = L1 - (Acc.Ycr(s) - Acc.Ydb(s) - Acc.Ybb(s))
             BalLine Y2 + Y3, Acc.Ycr(s) - Acc.Ydb(s) - Acc.Ybb(s)
          CASE "3"
             L1 = L1 - (Acc.Ycr(s) - Acc.Ydb(s) - Acc.Ybb(s))
             BalLine Y2 + Y3, Acc.Ycr(s) - Acc.Ydb(s) - Acc.Ybb(s)
          CASE ELSE
             WinMssg Acc.Nam + "-Account Type Error"
          END SELECT
       CASE "/" '---------------------------------------------
          PRINT #11, k$;
          SELECT CASE Acc.Typ
          CASE "1"
             BalLine Y1, L1
             L2 = L2 + L1: L1 = 0
          CASE "2", "3"
             BalLine Y2 + Y3, L1
             L2 = L2 + L1: L1 = 0
          CASE ELSE
          END SELECT
       CASE "*"  '---------------------------------------------
          PRINT #11, k$;
          SELECT CASE Acc.Typ
          CASE "1"
             BalLine Y1, L2
             L1 = 0: L2 = 0
          CASE "2", "3"
             BalLine Y2 + Y3 + Y4 - Y5, L2
             L1 = 0: L2 = 0
          CASE ELSE
          END SELECT
       CASE ELSE
          WinMssg "Error in Balance Sheet Picture"
       END SELECT
    WEND
    PRINT #11, CHR$(12);
END SUB

DEFINT M, Y
SUB Trial (s$)
    StatLin "Trail Balance for " + s$
    PageNum = 0: HTrial s$
    s = VAL(s$)
    FOR Y = 1 TO 5
    FOR x = 1 TO 99
    GET #1, x, Acc
    IF VAL(Acc.Typ) = Y THEN
       PRINT #11, USING "&  "; Acc.Typ + Acc.Num; Acc.Nam; ""; Acc.Crd; "";
       PRINT #11, USING "#########,.##-"; Acc.Mbb(s) / 100; Acc.Mdb(s) / 100; Acc.Mcr(s) / 100; (Acc.Mbb(s) + Acc.Mdb(s) - Acc.Mcr(s)) / 100;
       PRINT #11, "    ";
       PRINT #11, USING "#########,.##-"; Acc.Ybb(s) / 100; Acc.Ydb(s) / 100; Acc.Ycr(s) / 100; (Acc.Ybb(s) + Acc.Ydb(s) - Acc.Ycr(s)) / 100
       L1 = L1 + Acc.Mbb(s): L2 = L2 + Acc.Mdb(s): L3 = L3 + Acc.Mcr(s)
       L4 = L4 + Acc.Ybb(s): L5 = L5 + Acc.Ydb(s): L6 = L6 + Acc.Ycr(s)
       LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HTrial s$
    END IF
    NEXT
    PRINT #11, : LineNum = LineNum + 1
    NEXT
    PRINT #11, TAB(43);
    PRINT #11, USING "#########,.##-"; L1 / 100; L2 / 100; L3 / 100; (L1 + L2 - L3) / 100;
    PRINT #11, "    ";
    PRINT #11, USING "#########,.##-"; L4 / 100; L5 / 100; L6 / 100; (L4 - L6 + L5) / 100;
    PRINT #11, CHR$(12);
END SUB

