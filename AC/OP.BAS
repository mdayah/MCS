DEFINT A-Z
DECLARE SUB PR_Files ()
DECLARE SUB CompShow ()

'$INCLUDE: '..\BI\Library.REC'

DEFSNG A-Z
SUB CompShow ()
    CHDIR "..\" + ComCode
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
    CLOSE : OPEN "Config.MAS" FOR OUTPUT AS #1
    WRITE #1, ComCode
    WRITE #1, Company
    WRITE #1, Street
    WRITE #1, Address
    WRITE #1, City
    WRITE #1, Payroll
    WRITE #1, TaxID
    WRITE #1, EmpID
    CLOSE
    PR_Files
END SUB

