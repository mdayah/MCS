DEFINT A-Z
DEFLNG L-M, Y
DECLARE SUB Trial ()
DECLARE SUB Sheet ()
DECLARE SUB Income ()
DECLARE SUB HTrial ()
DECLARE SUB HSheet ()
DECLARE SUB HIncome ()
DECLARE SUB ShowSto (r, c)
DECLARE SUB BalLine (L1, L2)
DECLARE SUB IncLine (L1, L2, L3, L4)
DECLARE SUB ActsTot (M1, M2, M3, M4, M5, Y1, Y2, Y3, Y4, Y5)
DECLARE FUNCTION PickSto (s$)

REM $INCLUDE: '..\Include\Library.REC'

DEFSNG B-L, N-R, T-X, Z
SUB ActsTot (M1, M2, M3, M4, M5, Y1, Y2, Y3, Y4, Y5)
    FOR a = 1 TO 99
        GET #1, a, Acc
        SELECT CASE Acc.Typ
               CASE "1" '--------------------------------total assets
                    M1 = M1 - Acc.Mcr + Acc.Mbb + Acc.Mdb
                    Y1 = Y1 - Acc.Ycr + Acc.Ybb + Acc.Ydb
               CASE "2" '--------------------------------total liability
                    M2 = M2 - Acc.Mdb - Acc.Mbb + Acc.Mcr
                    Y2 = Y2 - Acc.Ydb - Acc.Ybb + Acc.Ycr
               CASE "3" '--------------------------------total equity
                    M3 = M3 - Acc.Mdb - Acc.Mbb + Acc.Mcr
                    Y3 = Y3 - Acc.Ydb - Acc.Ybb + Acc.Ycr
               CASE "4" '--------------------------------total revenues
                    M4 = M4 - Acc.Mbb - Acc.Mdb + Acc.Mcr
                    Y4 = Y4 - Acc.Ybb - Acc.Ydb + Acc.Ycr
               CASE "5" '--------------------------------total expenses
                    M5 = M5 - Acc.Mcr + Acc.Mbb + Acc.Mdb
                    Y5 = Y5 - Acc.Ycr + Acc.Ybb + Acc.Ydb
               CASE ELSE
        END SELECT
    NEXT
END SUB

DEFSNG A, S
SUB BalLine (M, Y)
    PRINT #11, USING "   ##########,.##"; Y / 100;
    PRINT #11, TAB(73);
    IF M = 0 THEN PRINT #11, "        0" ELSE PRINT #11, USING "   ###.##"; Y / M * 100
END SUB

DEFSNG M, Y
SUB FS_Files ()
   CLOSE #1:  OPEN "..\Data\Accounts.MAS" FOR RANDOM LOCK READ WRITE AS #1 LEN = LEN(Acc)
   CLOSE #11: OPEN "LPT1:" FOR OUTPUT AS #11
END SUB

SUB HIncome ()
    Heading
   'Laser
   'PRINT #11, CHR$(27); "(s12H"   'Pitch 12 cpi
   'Epson
    PRINT #11, CHR$(27); "M";       'Elite
    PRINT #11, CHR$(27); CHR$(15);  'Condensed
    PRINT #11, TAB(42); "INCOME STATEMENT"
    PRINT #11,
    PRINT #11, "                              MONTH-TO-DATE   PERCENT     YEAR-TO-DATE   PERCENT"
    PRINT #11, "                             ===============  =======    ==============  ======="
END SUB

SUB HSheet ()
    Heading
   'Laser
   'PRINT #11, CHR$(27); "(s12H"   'Pitch 12 cpi
   'Epson
    PRINT #11, CHR$(27); "M";       'Elite
    PRINT #11, CHR$(27); CHR$(15);  'Condensed
    PRINT #11, TAB(43); "BALANCE  SHEET"
    PRINT #11,
END SUB

SUB HTrial ()
    Heading
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
SUB Income ()
    '  LM is sales revenue for the month
    '  LY is sales revenue for the year
    '  L3 is net profit for the month
    '  L4 is net profit for the year
    '
    StatLin "Income Statement"
    
    GET #1, 50, Acc '----------sales revenue
    LM = Acc.Mcr - Acc.Mdb
    LY = Acc.Ycr - Acc.Ydb
    GET #1, 51, Acc '----------rental revenue
    LM = LM + Acc.Mcr - Acc.Mdb
    LY = LY + Acc.Ycr - Acc.Ydb
    PageNum = 0: HIncome
    CLOSE #2: OPEN "..\Data\Income.PIC" FOR INPUT AS #2
    WHILE NOT EOF(2)
       INPUT #2, f$, k$
       SELECT CASE f$
       CASE "^" '--------------------------------------------page break
          PRINT #11, CHR$(12);
          HIncome
       CASE "$" '--------------------------------------------title line
          PRINT #11, k$
       CASE "@" '--------------------------------------------underlined title
         'PRINT #11, CHR$(27); "&d0D"; 'enable underline Laser
          PRINT #11, CHR$(27); "-1";   'enable underline Epson
          PRINT #11, k$
          PRINT #11, CHR$(27); "-0";   'disable underline Epson
         'PRINT #11, CHR$(27); "&d@"   'disable underline Laser
       CASE "+" '--------------increment
          GET #1, VAL(k$), Acc
          PRINT #11, Acc.Nam;
          IF Acc.Typ = "4" THEN
             L1 = L1 + (Acc.Mcr - Acc.Mdb)
             L2 = L2 + (Acc.Ycr - Acc.Ydb)
             IncLine LM, LY, Acc.Mcr - Acc.Mdb, Acc.Ycr - Acc.Ydb
          ELSEIF Acc.Typ = "5" THEN
             L1 = L1 + (Acc.Mdb - Acc.Mcr)
             L2 = L2 + (Acc.Ydb - Acc.Ycr)
             IncLine LM, LY, Acc.Mdb - Acc.Mcr, Acc.Ydb - Acc.Ycr
          ELSE
             WinMssg Acc.Nam + "-Account Type Error"
          END IF
       CASE "-" '--------------decrement
          GET #1, VAL(k$), Acc
          PRINT #11, Acc.Nam;
          IF Acc.Typ = "4" THEN
             L1 = L1 - (Acc.Mcr - Acc.Mdb)
             L2 = L2 - (Acc.Ycr - Acc.Ydb)
             IncLine LM, LY, Acc.Mcr - Acc.Mdb, Acc.Ycr - Acc.Ydb
          ELSEIF Acc.Typ = "5" THEN
             L1 = L1 - (Acc.Mdb - Acc.Mcr)
             L2 = L2 - (Acc.Ydb - Acc.Ycr)
             IncLine LM, LY, Acc.Mdb - Acc.Mcr, Acc.Ydb - Acc.Ycr
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
    CLOSE : SHELL "EDIT ..\Data\Income.PIC"
END SUB

SUB mfsEsheet ()
    CLOSE : SHELL "EDIT ..\Data\Sheet.PIC"
END SUB

SUB mfsIncome ()
    Income
END SUB

SUB mfsSheet ()
    Sheet
END SUB

SUB mfsTrial ()
    Trial
END SUB

DEFINT A-K, N-X, Z
DEFLNG L-M, Y
SUB Sheet ()
    StatLin "Balance Sheet"
    ActsTot M1, M2, M3, M4, M5, Y1, Y2, Y3, Y4, Y5
    PageNum = 0: HSheet
    CLOSE #2: OPEN "..\Data\Sheet.PIC" FOR INPUT AS #2
    WHILE NOT EOF(2)
       INPUT #2, f$, k$
       SELECT CASE f$
       CASE "^" '--------------------------------------------page break
          PRINT #11, CHR$(12);
          HSheet
       CASE "$" '--------------------------------------------title line
          PRINT #11, k$
       CASE "@" '--------------------------------------------underlined title
         'PRINT #11, CHR$(27); "&d0D"; 'enable underline Laser
          PRINT #11, CHR$(27); "-1";   'enable underline Epson
          PRINT #11, k$
          PRINT #11, CHR$(27); "-0";   'disable underline Epson
         'PRINT #11, CHR$(27); "&d@";  'disable underline Laser
       CASE "+" '--------------------------------------------increment
          GET #1, VAL(k$), Acc
          PRINT #11, Acc.Nam;
          SELECT CASE Acc.Typ
          CASE "1"
             L1 = L1 + (Acc.Ybb + Acc.Ydb - Acc.Ycr)
             BalLine Y1, Acc.Ybb + Acc.Ydb - Acc.Ycr
          CASE "2"
             L1 = L1 + (Acc.Ycr - Acc.Ydb - Acc.Ybb)
             BalLine Y2 + Y3, Acc.Ycr - Acc.Ydb - Acc.Ybb
          CASE "3"
             SELECT CASE Acc.Num
             CASE "48" '----------AAA account
                L1 = L1 + (Acc.Mcr - Acc.Mdb - Acc.Mbb)
                BalLine Y2 + Y3, (Acc.Mcr - Acc.Mdb - Acc.Mbb)
             CASE "49" '----------P/L account
                L1 = L1 + M4 - M5
                BalLine Y2 + Y3, M4 - M5
             CASE ELSE
                L1 = L1 + (Acc.Ycr - Acc.Ydb - Acc.Ybb)
                BalLine Y2 + Y3, Acc.Ycr - Acc.Ydb - Acc.Ybb
             END SELECT
          CASE ELSE
             WinMssg Acc.Nam + "-Account Type Error"
          END SELECT
       CASE "-" '--------------------------------------------decrement
          GET #1, VAL(k$), Acc
          PRINT #11, Acc.Nam;
          SELECT CASE Acc.Typ
          CASE "1"
             L1 = L1 - (Acc.Ybb + Acc.Ydb - Acc.Ycr)
             BalLine Y1, Acc.Ybb + Acc.Ydb - Acc.Ycr
          CASE "2"
             L1 = L1 - (Acc.Ycr - Acc.Ydb - Acc.Ybb)
             BalLine Y2 + Y3, Acc.Ycr - Acc.Ydb - Acc.Ybb
          CASE "3"
             L1 = L1 - (Acc.Ycr - Acc.Ydb - Acc.Ybb)
             BalLine Y2 + Y3, Acc.Ycr - Acc.Ydb - Acc.Ybb
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
SUB Trial ()
    StatLin "Trail Balance"
    PageNum = 0: HTrial
    FOR Y = 1 TO 5
        FOR x = 1 TO 99
        GET #1, x, Acc
        IF VAL(Acc.Typ) = Y THEN
           PRINT #11, USING "&  "; Acc.Typ + Acc.Num; Acc.Nam; ""; Acc.Crd; "";
           PRINT #11, USING "#########,.##-"; Acc.Mbb / 100; Acc.Mdb / 100; Acc.Mcr / 100; (Acc.Mbb + Acc.Mdb - Acc.Mcr) / 100;
           PRINT #11, "    ";
           PRINT #11, USING "#########,.##-"; Acc.Ybb / 100; Acc.Ydb / 100; Acc.Ycr / 100; (Acc.Ybb + Acc.Ydb - Acc.Ycr) / 100
           L1 = L1 + Acc.Mbb: L2 = L2 + Acc.Mdb: L3 = L3 + Acc.Mcr
           L4 = L4 + Acc.Ybb: L5 = L5 + Acc.Ydb: L6 = L6 + Acc.Ycr
           LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HTrial
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

