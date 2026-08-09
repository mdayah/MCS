DEFINT A-Z
DECLARE SUB PR_Files ()
DECLARE SUB CompShow ()

'$INCLUDE: '..\BI\Library.REC'

DEFSNG A-Z
SUB CompShow ()
    CHDIR "..\ÿ" + ComCode
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
SUB optCreate ()
    StatLin "Create new Client"
    CALL MhDisplayByName(0, 0, "AC-Company", e)
800 EntryU 13, 48, 7, e, b, a$:  IF e THEN EXIT SUB ELSE IF b THEN 800
    IF a$ < "!" THEN WinMssg "First charcter must not be blank": GOTO 800
801 EntryS 15, 48, 30, e, b, b$: IF e THEN EXIT SUB ELSE IF b THEN 800
802 EntryS 16, 48, 30, e, b, c$: IF e THEN EXIT SUB ELSE IF b THEN 801
803 EntryS 17, 48, 30, e, b, d$: IF e THEN EXIT SUB ELSE IF b THEN 802
804 EntryS 18, 48, 30, e, b, e$:    IF e THEN EXIT SUB ELSE IF b THEN 803
805 EntryU 20, 56, 1, e, b, f$:  IF e THEN EXIT SUB ELSE IF b THEN 804
    SELECT CASE f$
    CASE "W": qPrint "Weekly  ", 20, 56, 0, 1
    CASE "B": qPrint "BiWeekly", 20, 56, 0, 1
    CASE ELSE: WinMssg "Weekly or Biweekly": GOTO 805
    END SELECT
806 EntryS 21, 56, 10, e, b, g$: IF e THEN EXIT SUB ELSE IF b THEN 805
807 EntryU 22, 56, 7, e, b, h$:  IF e THEN EXIT SUB ELSE IF b THEN 806
    
    IF Confirm("SAVE Client") = 0 THEN 807
    ComCode = a$
    MKDIR "..\ÿ" + ComCode
    CHDIR "..\ÿ" + ComCode
    Company = b$
    Street = c$
    Address = d$
    City = e$
    Payroll = f$
    TaxID = g$
    EmpID = h$
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
    SHELL "Copy ..\AC\AC.MAS Accounts.MAS"
    ErrS(1) = 0
    ErrS(2) = 0
    ErrS(3) = 0
    ErrS(4) = 0
    ErrS(5) = 1
    PR_Files
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

    IF Confirm("SAVE Client") = 0 THEN 815
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

SUB optSelect ()
    FileCount "..\ÿ*.", 16, t, e
    IF e OR t = 0 THEN WinMssg "No clients found": EXIT SUB
    REDIM c(1 TO t) AS STRING * 12
    s = VARSEG(c(1))
    o = VARPTR(c(1))
    LoadDir "..\ÿ*.", 16, s, o, e
    CALL MhDisplayByName(0, 0, "ac-ComList", e)
    StatLin "Select new Client"
    r = 10: b = 1: GOSUB c2
c1: COLOR 16, 7: LOCATE r, 23: PRINT USING "& "; MID$(c(n), 1, 8);
    KeyPres f$
    SELECT CASE f$
    CASE CHR$(0) + "P"
         COLOR 16, 2: LOCATE r, 23: PRINT USING "& "; MID$(c(n), 1, 8);
         IF n + 1 > t GOTO c1 ELSE n = n + 1
         IF r + 1 > 24 THEN CALL Scroll(23, 10, 31, 24, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"
         COLOR 16, 2: LOCATE r, 23: PRINT USING "& "; MID$(c(n), 1, 8);
         IF n - 1 < 1 GOTO c1 ELSE n = n - 1
         IF r - 1 < 10 THEN CALL BkScroll(23, 10, 31, 24, 1) ELSE r = r - 1
    CASE CHR$(13)
         CALL MhDisplayByName(0, 0, "AC-Company", e)
         ComCode = MID$(c(n), 2, 7)
         CompShow
         ErrS(1) = 0
         ErrS(2) = 0
         ErrS(3) = 0
         ErrS(4) = 0
         ErrS(5) = 1
         PR_Files
         KeyPres f$
         EXIT SUB
    CASE ELSE
         PCOPY 1, 0
         EXIT SUB
    END SELECT
    GOTO c1
c2: COLOR 16, 2: LOCATE 10
    FOR x = b TO t
    LOCATE , 23: PRINT USING "& "; MID$(c(x), 1, 8)
    IF CSRLIN > 24 THEN EXIT FOR
    NEXT
    n = b + r - 10
    RETURN
END SUB

