DEFINT A-Z
DEFLNG M, Y
DECLARE SUB VenClose (p)
DECLARE SUB CompShow ()

DECLARE SUB FS_Files ()
DECLARE SUB Income ()
DECLARE SUB Sheet ()
DECLARE SUB Trial ()

DECLARE SUB GL_Files ()
DECLARE SUB JrnPrt (f$, t$)
DECLARE SUB JrnAct (f$, t$)

DECLARE SUB AP_Files ()
DECLARE SUB Aging ()
DECLARE SUB Require ()
DECLARE SUB VenAcct ()
DECLARE SUB InvLong (f$, t$)
DECLARE SUB PrtChk (f$, t$)

DECLARE SUB ActsTot (M1, M2, M3, M4, M5, Y1, Y2, Y3, Y4, Y5)

REM $INCLUDE: '..\Include\Library.REC'

DEFSNG A-Z
SUB CompShow ()
   '$INCLUDE: '..\Include\StartUp.DEC'
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
          COLOR 7, 0: CLS : SHELL "pkzip -exx -&v -sJuMaNa a:\backup." + ComCode + " ..\Data\*.*"
       END IF
    CASE 2
       StatLin "Verify Backup Set"
       IF Confirm("verify BACKUP set") THEN
          COLOR 2, 0: CLS : SHELL "pkunzip -t -sJuMaNa a:\backup." + ComCode
       END IF
    CASE 3
       StatLin "View Backup Set"
       IF Confirm("view BACKUP set") THEN
          COLOR 3, 0: CLS : SHELL "pkunzip -vnb -sJuMaNa a:\backup." + ComCode
       END IF
    CASE 4
       StatLin "Restore Data Files"
       IF Confirm("start RESTORE") THEN
          COLOR 4, 0: CLS : SHELL "pkunzip -o -sJuMaNa a:\backup." + ComCode + " ..\Data"
       END IF
    END SELECT
    WinMssg "Press a key to continue"
END SUB

SUB optDate ()
    StatLin "Set System Date & Time"
    CALL MhDisplayByName(0, 0, "OP-Date", e)
    qPrint DATE$, 12, 51, 0, 1: f$ = DATE$
    qPrint TIME$, 14, 51, 0, 1: t$ = TIME$
50  EntryD 13, 51, e, b, f$:    IF b THEN 50 ELSE IF e THEN EXIT SUB
    IF f$ > "0" THEN DATE$ = f$
51  EntryU 15, 51, 8, e, b, t$: IF b THEN 50 ELSE IF e THEN EXIT SUB
    IF t$ > "0" THEN TIME$ = t$
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
   '$INCLUDE: '..\Include\Terminat.DEC'
END SUB

DEFLNG M, Y
SUB optMonth ()
    StatLin "End of Month Closing"
    IF RprtDate(f$, t$) THEN EXIT SUB

    IF Confirm("Print MONTHLY Reports") THEN
       GL_Files
       JrnPrt f$, t$       '===============Print Journal Entries=====
       JrnAct f$, t$       '===============Print Account Activity====
       CLOSE

       AP_Files
       Aging               '===============Print Aging Report========
       Require             '===============Cash Requirements=========
       VenAcct             '===============Vendor Accounts===========
       InvLong f$, t$      '===============Invoice Details===========
       PrtChk f$, t$       '===============Check Details=============

       FS_Files
       Trial               '===============Print Trail Balance=======
       Income              '===============Print Income Statement====
       Sheet               '===============Print Balance Sheet=======
       CLOSE

       IF Closing("MONTH") THEN
          FS_Files
          ActsTot M1, M2, M3, M4, M5, Y1, Y2, Y3, Y4, Y5
          GET #1, 48, Acc '----------------post AAA ACCOUNT
          Acc.Mcr = Acc.Mcr + M4 - M5
          PUT #1, 48, Acc
          
          FOR a = 1 TO 99
              GET #1, a, Acc
              SELECT CASE Acc.Typ
              CASE "1", "2", "3"
                  Acc.Mbb = Acc.Mbb + Acc.Mdb - Acc.Mcr
                  Acc.Mdb = 0
                  Acc.Mcr = 0
              CASE "4", "5"
                  Acc.Mdb = 0
                  Acc.Mcr = 0
              CASE ELSE
              END SELECT
              PUT #1, a, Acc
          NEXT
          VenClose 1
       END IF
    END IF
END SUB

DEFCUR M, Y
SUB optYear ()
    StatLin "End of Year Closing"
    IF RprtDate(f$, t$) THEN EXIT SUB

    IF Confirm("Print YEAR-END Reports") THEN
       GL_Files
       JrnPrt f$, t$       '===============Print Journal Entries=====
       JrnAct f$, t$       '===============Print Account Activity====
       CLOSE
       
       AP_Files
       Aging               '===============Print Aging Report========
       Require             '===============Cash Requirements=========
       VenAcct             '===============Vendor Accounts===========
       InvLong f$, t$      '===============Invoice Details===========
       PrtChk f$, t$       '===============Check Details=============

       FS_Files
       Trial               '===============Print Trail Balance=======
       Income              '===============Print Income Statement====
       Sheet               '===============Print Balance Sheet=======

       IF Closing("YEAR") THEN
          FOR a = 1 TO 99
              GET #1, a, Acc
              SELECT CASE Acc.Typ
              CASE "1", "2", "3"
                   Acc.Ybb = Acc.Ybb + Acc.Ydb - Acc.Ycr
                   Acc.Ydb = 0
                   Acc.Ycr = 0
              CASE "4", "5"
                   Acc.Ydb = 0
                   Acc.Ycr = 0
              CASE ELSE
              END SELECT
              PUT #1, a, Acc
          NEXT
          VenClose 2
       END IF
    END IF
END SUB

