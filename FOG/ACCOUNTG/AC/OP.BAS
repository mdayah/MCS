DECLARE SUB VenClose (p%)
DEFINT A-Z

DECLARE SUB CompShow ()

DECLARE SUB FS_Files ()
DECLARE SUB Income (s$)
DECLARE SUB Sheet (s$)
DECLARE SUB Trial (s$)

DECLARE SUB GL_Files ()
DECLARE SUB JrnPrt (s$, f$, t$)
DECLARE SUB JrnAct (s$, f$, t$)

DECLARE SUB AP_Files ()
DECLARE SUB Aging ()
DECLARE SUB Require ()
DECLARE SUB VenAcct ()
DECLARE SUB InvLong (f$, t$)
DECLARE SUB PrtChk (f$, t$)

DECLARE SUB ActsTot (s%, M1@, M2@, M3@, M4@, M5@, Y1@, Y2@, Y3@, Y4@, Y5@)
DECLARE FUNCTION RptDate (f$, t$)

REM $INCLUDE: '..\BI\Library.REC'

DEFSNG A-Z
SUB CompShow ()
   '$INCLUDE: '..\BI\StartUp.DEC'
    CLOSE
    qPrint ComCode, 13, 48, 0, 1
    qPrint Company, 15, 48, 0, 1
    qPrint Street, 16, 48, 0, 1
    qPrint Address, 17, 48, 0, 1
    qPrint City, 18, 48, 0, 1
    qPrint Payroll, 20, 56, 0, 1
    qPrint TaxID, 21, 56, 0, 1
    qPrint EmpID, 22, 56, 0, 1
    SELECT CASE Payroll
        CASE "W": qPrint "Weekly  ", 20, 56, 0, 1
        CASE "B": qPrint "BiWeekly", 20, 56, 0, 1
    END SELECT
END SUB

DEFINT A-Z
SUB optBackup (s)
    SELECT CASE s
    CASE 1
       StatLin "Backup Data Files"
       IF Confirm("start BACKUP") THEN
          CLS : SHELL "pkzip -es -&v -sJuMaNa a:\backup." + ComCode + " *.*"
       END IF
    CASE 2
       StatLin "Verify Backup Set"
       IF Confirm("verify BACKUP set") THEN
          CLS : SHELL "pkunzip -t -sJuMaNa a:\backup." + ComCode
       END IF
    CASE 3
       StatLin "Restore Data Files"
       IF Confirm("start RESTORE") THEN
          CLS : SHELL "pkunzip -o -sJuMaNa a:\backup." + ComCode
       END IF
    END SELECT
END SUB

SUB optDate ()
    StatLin "Reset System Date"
    CALL MhDisplayByName(0, 0, "AC-Date", e)
    COLOR 0, 3: LOCATE 12, 51: PRINT DATE$
    f$ = DATE$
    EntryD 14, 51, e, b, f$: IF e OR b THEN EXIT SUB
    IF f$ > "0" THEN DATE$ = f$
END SUB

SUB optModify ()
    StatLin "Modify Client"
    CALL MhDisplayByName(0, 0, "AC-Company", e)
    CompShow
809 EntryS 15, 48, 30, e, b, Company: IF e THEN EXIT SUB ELSE IF b THEN 809
810 EntryS 16, 48, 30, e, b, Street:  IF e THEN EXIT SUB ELSE IF b THEN 809
811 EntryS 17, 48, 30, e, b, Address: IF e THEN EXIT SUB ELSE IF b THEN 810
812 EntryS 18, 48, 30, e, b, City:    IF e THEN EXIT SUB ELSE IF b THEN 811
813 EntryU 20, 56, 1, e, b, Payroll:  IF e THEN EXIT SUB ELSE IF b THEN 812
    SELECT CASE Payroll
    CASE "W": qPrint "Weekly  ", 20, 56, 0, 1
    CASE "B": qPrint "BiWeekly", 20, 56, 0, 1
    CASE ELSE: WinMssg "Weekly or Biweekly": GOTO 813
    END SELECT
814 EntryS 21, 56, 10, e, b, TaxID:   IF e THEN EXIT SUB ELSE IF b THEN 813
815 EntryU 22, 56, 7, e, b, EmpID:    IF e THEN EXIT SUB ELSE IF b THEN 814
    IF Confirm("SAVE Company Profile") = 0 THEN 815
   '$INCLUDE: '..\BI\Terminat.DEC'
END SUB

DEFCUR M, Y
SUB optMonth ()
    StatLin "End of Month Closing"
    IF RptDate(f$, t$) THEN EXIT SUB

    IF Confirm("Print MONTHLY Reports") THEN
       'GL_Files
       'JrnPrt "00", f$, t$ '===============Print Journal Entries=====
       'JrnAct "00", f$, t$ '===============Print Account Activity====
       'CLOSE

       'AP_Files
       'Aging               '===============Print Aging Report========
       'Require             '===============Cash Requirements=========
       'VenAcct             '===============Vendor Accounts===========
       'InvLong f$, t$      '===============Invoice Details===========
       'PrtChk f$, t$       '===============Check Details=============

       FS_Files
       'Trial "00"  '=======================Print Trail Balance=======
       'FOR s = 1 TO 99
       '    GET #12, s, Sto
       '    IF Sto.Nam > "!" AND ComCode = Sto.Cod THEN Trial Sto.Num
       'NEXT
       Income "00" '=======================Print Income Statement====
       FOR s = 1 TO 99
           GET #12, s, Sto
           IF Sto.Nam > "!" AND ComCode = Sto.Cod THEN Income Sto.Num
       NEXT
       Sheet "00"  '=======================Print Balance Sheet=======
       FOR s = 1 TO 99
           GET #12, s, Sto
           IF Sto.Nam > "!" AND ComCode = Sto.Cod THEN Sheet Sto.Num
       NEXT
       CLOSE

       IF Confirm("Close the MONTH") THEN
          FS_Files
          FOR s = 0 TO 99
              ActsTot s, M1, M2, M3, M4, M5, Y1, Y2, Y3, Y4, Y5
              GET #1, 47, Acc '----------------post AAA ACCOUNT
              Acc.Mcr(s) = Acc.Mcr(s) + M4 - M5
              PUT #1, 47, Acc
          NEXT

          FOR a = 1 TO 99
              GET #1, a, Acc
              FOR s = 0 TO 99
                  SELECT CASE Acc.Typ
                  CASE "1", "2", "3"
                       Acc.Mbb(s) = Acc.Mbb(s) + Acc.Mdb(s) - Acc.Mcr(s)
                       Acc.Mdb(s) = 0
                       Acc.Mcr(s) = 0
                  CASE "4", "5"
                       Acc.Mdb(s) = 0
                       Acc.Mcr(s) = 0
                  CASE ELSE
                  END SELECT
              NEXT
              PUT #1, a, Acc
          NEXT
          VenClose 1
       END IF
    END IF
END SUB

SUB optYear ()
    StatLin "End of Year Closing"
    IF RptDate(f$, t$) THEN EXIT SUB

    IF Confirm("Print YEAR-END Reports") THEN
       'GL_Files
       'JrnPrt "00", f$, t$ '===============Print Journal Entries=====
       'JrnAct "00", f$, t$ '===============Print Account Activity====
       'CLOSE
       
       'AP_Files
       'Aging               '===============Print Aging Report========
       'Require             '===============Cash Requirements=========
       'VenAcct             '===============Vendor Accounts===========
       'InvLong f$, t$      '===============Invoice Details===========
       'PrtChk f$, t$       '===============Check Details=============

       FS_Files
       Trial "00"  '=======================Print Trail Balance=======
       FOR s = 1 TO 99
           GET #12, s, Sto
           IF Sto.Nam > "!" AND ComCode = Sto.Cod THEN Trial Sto.Num
       NEXT
       Income "00" '=======================Print Income Statement====
       FOR s = 1 TO 99
           GET #12, s, Sto
           IF Sto.Nam > "!" AND ComCode = Sto.Cod THEN Income Sto.Num
       NEXT
       Sheet "00"  '=======================Print Balance Sheet=======
       FOR s = 1 TO 99
           GET #12, s, Sto
           IF Sto.Nam > "!" AND ComCode = Sto.Cod THEN Sheet Sto.Num
       NEXT

       IF Confirm("Close the YEAR") THEN
          FS_Files
          FOR s = 0 TO 99
              ActsTot s, M1, M2, M3, M4, M5, Y1, Y2, Y3, Y4, Y5
              GET #1, 47, Acc '----------------post AAA ACCOUNT
              Acc.Ycr(s) = Acc.Ycr(s) + Y4 - Y5
              PUT #1, 47, Acc
          NEXT
          
          FOR a = 1 TO 99
              GET #1, a, Acc
              FOR s = 0 TO 99
                  SELECT CASE Acc.Typ
                  CASE "1", "2", "3"
                       Acc.Ybb(s) = Acc.Ybb(s) + Acc.Ydb(s) - Acc.Ycr(s)
                       Acc.Ydb(s) = 0
                       Acc.Ycr(s) = 0
                  CASE "4", "5"
                       Acc.Ydb(s) = 0
                       Acc.Ycr(s) = 0
                  CASE ELSE
                  END SELECT
              NEXT
              PUT #1, a, Acc
          NEXT
          VenClose 2
       END IF
    END IF
END SUB

DEFINT M, Y
FUNCTION RptDate (f$, t$)
    CALL MhDisplayByName(0, 0, "OP-Limit", e)
    f$ = "          "
    t$ = "          "
o1: EntryD 21, 43, e, b, f$: IF e THEN RptDate = 1: EXIT FUNCTION ELSE IF b GOTO o1
    IF f$ < "!" GOTO o1
o2: EntryD 22, 43, e, b, t$: IF e THEN RptDate = 1: EXIT FUNCTION ELSE IF b GOTO o1
    IF t$ < "!" GOTO o2
END FUNCTION

