DEFINT A-Z
DEFLNG L
DECLARE FUNCTION Confirm ()
DECLARE SUB OpenFiles ()
DECLARE SUB WinMssg (f$)
DECLARE SUB Heading ()
DECLARE SUB HChecks ()
DECLARE SUB HMaster ()
DECLARE SUB HQuarter ()
DECLARE SUB HYearEnd ()
DECLARE SUB HTimeCard ()
DECLARE SUB HRegister ()
DECLARE SUB TotalHours ()
DECLARE SUB TimeDate ()
DECLARE SUB KeyHit (f$)
DECLARE SUB HitKey (f$)
DECLARE SUB WinEmp (f$)
DECLARE SUB NumWords (n$, f$)

DECLARE SUB EntryC (e, b)
DECLARE SUB Picture (r, c, S, f$)
DECLARE SUB EntryX (r, c, e, b, f$)
DECLARE SUB EntryT (r, c, e, b, f$)
DECLARE SUB EntryD (r, c, e, b, f$)
DECLARE SUB EntryU (r, c, S, e, b, f$)
DECLARE SUB EntryS (r, c, S, e, b, f$)
DECLARE SUB EntryN (r, c, S, d, e, b, f$)
DECLARE SUB Display (r, L1, L2, L3, L4)
DECLARE SUB IncomeTax (S$, d$, L1, L2)
DECLARE SUB SocSecTax (L1, L2, L3, L4)

REM $INCLUDE: '..\BI\Accounts.REC'
REM $INCLUDE: '..\BI\Employee.REC'
REM $INCLUDE: '..\BI\EmpIndex.REC'
REM $INCLUDE: '..\BI\TimeCard.REC'
	 
	DIM SHARED Emp AS Employee
	DIM SHARED Inx AS Index
	DIM SHARED Tim AS TimeCard
	DIM SHARED Acc AS Accounts

	DIM SHARED Dig(1 TO 3, 0 TO 9) AS STRING
	DIM SHARED Clk(0 TO 1, 0 TO 5, 0 TO 3) AS STRING * 4
	DIM SHARED Dec(0 TO 1, 0 TO 5, 0 TO 3) AS INTEGER
	DIM SHARED Hrs(0 TO 1, 0 TO 5) AS STRING * 6
	DIM SHARED Thr(0 TO 1, 0 TO 2) AS SINGLE
	DIM SHARED w       AS INTEGER
	DIM SHARED d       AS INTEGER
	DIM SHARED T       AS INTEGER
	DIM SHARED FDat    AS STRING * 10
	DIM SHARED TDat    AS STRING * 10
	DIM SHARED Sort    AS INTEGER
	DIM SHARED Top     AS INTEGER
	DIM SHARED Bot     AS INTEGER
	DIM SHARED LineNum AS INTEGER
	DIM SHARED PageNum AS INTEGER

	DIM WeekDay$(0 TO 6)
	ON ERROR GOTO Errors
	FOR y = 1 TO 3: FOR x = 0 TO 9: READ Dig(y, x): NEXT x, y
	FOR y = 0 TO 5: READ WeekDay$(y): NEXT

	KEY OFF
	KEY 1, "1"
	KEY 2, "2"
	KEY 3, "3"
	KEY 4, "4"
	KEY 5, "5"
	KEY 6, "6"
	KEY 7, "7"
	KEY 8, "8"
	KEY 9, "9"
	KEY 10, "0"

	SCREEN 0, 0, 0, 0
	CALL MhOpenScreenLib("PRS.SLB" + CHR$(0), e)
	CALL MhShadowOn(0, 8)
	CALL MhBlinkOff
Menu: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
10  CLOSE : CALL MhDisplayByName(0, 0, "Menu", e)
	HitKey f$: f = VAL(f$)
	IF f$ = "0" THEN f = 10
	IF f$ = CHR$(27) GOTO Terminate
	ON f GOTO TimeCard, TimeSheet, Register, Checks, Employee, PrintEmp, ListChk, Quarter, YearEnd
	GOTO 10
TimeCard: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	OpenFiles
	CALL MhDisplayByName(0, 0, "TimeCard", e)
110 EntryN 5, 14, 2, 0, e, b, Emp.Num: IF e THEN 10 ELSE IF b THEN 110
	IF Emp.Num < " 1" THEN WinEmp f$: IF f$ = "" THEN 110
	GET #1, VAL(Emp.Num), Emp: IF Emp.Nam < "!" THEN BEEP: GOTO 110
	LOCATE 5, 14: PRINT Emp.Num
	LOCATE 5, 18: PRINT Emp.Nam;
	IF Emp.Typ = "S" THEN WinMssg "Salaried Employee, cant have timecard.": GOTO 110
	ERASE Clk, Dec, Hrs, Thr
	GET #2, VAL(Emp.Num), Tim
	IF Tim.Hrs > "  0.00" THEN
	   x = 0: COLOR 14, 0
	   FOR w = 0 TO 1: FOR d = 0 TO 5: FOR T = 0 TO 3
	   Clk(w, d, T) = MID$(Tim.Clk, 1 + x, 4): x = x + 4
	   Dec(w, d, T) = 100 * VAL(LEFT$(Clk(w, d, T), 2)) + VAL(RIGHT$(Clk(w, d, T), 2)) / .6
	   LOCATE 7 + 2 * d, 16 + 5 * T + 29 * w: PRINT Clk(w, d, T)
	   NEXT T, d, w
	   x = 0: COLOR 15, 6
	   FOR w = 0 TO 1: FOR d = 0 TO 5
	   Hrs(w, d) = MID$(Tim.Thr, 1 + x, 6): x = x + 6
	   Thr(w, 0) = Thr(w, 0) + VAL(Hrs(w, d))
	   LOCATE 7 + 2 * d, 36 + 29 * w: PRINT Hrs(w, d)
	   NEXT d
	   TotalHours
	   COLOR 15, 6
	   NEXT w
	END IF
112 FOR w = 0 TO 1: FOR d = 0 TO 5: FOR T = 0 TO 3
	EntryC e, b: IF e THEN 10
	IF b THEN T = T - 2: IF T < -1 THEN T = 2: d = d - 1: IF d < 0 THEN d = 5: IF w = 1 THEN w = 0 ELSE w = 1
	NEXT T, d, w
	IF Confirm THEN
	   LOCATE 25, 18: PRINT "One moment please,  S A V I N G  Timecard.";
	   Picture 19, 74, 6, Tim.Hrs
	   Picture 21, 74, 6, Tim.Reg
	   Picture 23, 74, 6, Tim.Ovt
	   f$ = "": FOR w = 0 TO 1: FOR d = 0 TO 5: FOR T = 0 TO 3: f$ = f$ + Clk(w, d, T): NEXT T, d, w: Tim.Clk = f$
	   f$ = "": FOR w = 0 TO 1: FOR d = 0 TO 5: f$ = f$ + Hrs(w, d): NEXT d, w: Tim.Thr = f$
	   PUT #2, VAL(Emp.Num), Tim
	   b = 0: ErrSw1 = 1: ErrSw4 = 1
	   GOTO TimeCard
	END IF
	GOTO 112
TimeSheet: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
200 LOCATE 13, 30: PRINT "Print Employees Timecards."
	OpenFiles
	ERASE Thr: ErrSw1 = 0
	PageNum = 0: HTimeCard
	FOR x = 1 TO 99
	GET #2, x, Tim
	ERASE Clk, Dec, Hrs, Thr
	IF Tim.Hrs > "  0.00" THEN
		GET #1, x, Emp
		LOCATE 23, 21: PRINT Emp.Num; " "; Emp.Nam; " "; Emp.SSn
		IF LineNum > 55 THEN HTimeCard
		LPRINT USING " & "; Emp.Num; Emp.Nam; Emp.SSn;

		y = 0
		FOR w = 0 TO 1: FOR d = 0 TO 5: FOR T = 0 TO 3
		Clk(w, d, T) = MID$(Tim.Clk, 1 + y, 4): y = y + 4
		IF ASC(Clk(w, d, T)) = 0 THEN Clk(w, d, T) = "    "
		NEXT T, d, w

		y = 0
		FOR w = 0 TO 1: FOR d = 0 TO 5
		Hrs(w, d) = MID$(Tim.Thr, 1 + y, 6): y = y + 6
		Thr(w, 0) = Thr(w, 0) + VAL(Hrs(w, d))
		NEXT d, w

		FOR d = 0 TO 5
			LPRINT TAB(47); WeekDay$(d); SPACE$(6);
			FOR w = 0 TO 1
				FOR T = 0 TO 3
					LPRINT USING "&     "; Clk(w, d, T);
				NEXT T
				LPRINT " "; Hrs(w, d); SPACE$(5);
				IF Thr(w, 0) > 40 THEN
				   Thr(w, 1) = 40: Thr(w, 2) = Thr(w, 0) - 40
				ELSE
				   Thr(w, 1) = Thr(w, 0): Thr(w, 2) = 0
				END IF
			NEXT w
			LPRINT
		NEXT d
		LPRINT
		LPRINT TAB(83); "   Total hours: "; : LPRINT USING "###.##&"; Thr(0, 0); SPACE$(42); Thr(1, 0); "    " + Tim.Hrs
		LPRINT TAB(83); " Regular hours: "; : LPRINT USING "###.##&"; Thr(0, 1); SPACE$(42); Thr(1, 1); "    " + Tim.Reg
		LPRINT TAB(83); "Overtime hours: "; : LPRINT USING "###.##&"; Thr(0, 2); SPACE$(42); Thr(1, 2); "    " + Tim.Ovt
		LPRINT
		LineNum = LineNum + 11
	END IF
	NEXT x
	GOTO 10
Register: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	OpenFiles
	IF ErrSw1 THEN WinMssg "Sorry, you must print TimeSheets first": GOTO 10
	LOCATE 23, 20: PRINT " Enter starting check number [        ]. "
300 EntryN 23, 50, 8, 0, e, b, ChkNum$: IF e THEN 10 ELSE IF b THEN 300
	LChk = VAL(ChkNum$): IF LChk < 1 THEN BEEP: GOTO 300
	CDat$ = LEFT$(DATE$, 5)
	ERASE Thr: ErrSw2 = 0: ErrSw3 = 0: ErrSw4 = 0
	COLOR 1, 7: CLS : LOCATE 25, 1
	PRINT SPACE$(25); "Checking Employee Master Files"; SPACE$(25); : COLOR 14, 0
	CLOSE #5: OPEN "Register.DTA" FOR OUTPUT AS #5
	Heading
	FOR x = 1 TO 99
	GET #6, x, Inx
	GET #1, VAL(Inx.Num), Emp
	GET #2, VAL(Inx.Num), Tim
	IF Emp.Nam > "!" THEN
	   LPRINT Emp.Grp; Emp.Num; " "; Emp.Nam; " ";
	   PRINT Emp.Grp; Emp.Num; " "; Emp.Nam; " ";
	   SELECT CASE Emp.Typ
	   CASE "S" '------------------------Salary-----
			IF Tim.Hrs > "  0.00" THEN
			   ErrNum = 1: GOSUB ErrTyp
			ELSE
			   LReg = VAL(Emp.Pay) * 100
			   LOvt = 0
			   Lbon = VAL(Emp.Bon) * 100
			   Lins = VAL(Emp.Ins) * 100
			   Lded = VAL(Emp.Ded) * 100
			END IF
	   CASE "W" '------------------------Wages------
			IF Tim.Hrs > "  0.00" THEN
			   LReg = VAL(Emp.Pay) * VAL(Tim.Reg) * 100
			   LOvt = VAL(Emp.Pay) * VAL(Tim.Ovt) * 150
			   Lbon = VAL(Emp.Bon) * 100
			   Lins = VAL(Emp.Ins) * 100
			   Lded = VAL(Emp.Ded) * 100
			ELSE
			   LOCATE , 31: PRINT "Does not have a timecard."
			   LPRINT TAB(31); "Does not have a timecard."
			   LReg = 0
			   LOvt = 0
			   Lbon = VAL(Emp.Bon) * 100
			   Lins = VAL(Emp.Ins) * 100
			   Lded = VAL(Emp.Ded) * 100
			END IF
	   CASE "H" '------------------------Hold-------
			LOCATE , 31: PRINT "On Hold, will not get a check."
			LPRINT TAB(31); "On Hold, will not get a check."
			LReg = 0
			LOvt = 0
			Lbon = 0
			Lins = 0
			Lded = 0
	   CASE ELSE
			ErrNum = 2: GOSUB ErrTyp
	   END SELECT
	   LGrs = LReg + LOvt + Lbon
	   SELECT CASE Emp.Tax
	   CASE "P"
			LOCATE , 31: PRINT "Percentage tax rate: ..%": f$ = "00"
304         EntryN 23, 52, 2, 0, e, b, f$: IF e OR b THEN 304
			COLOR 14, 0: PRINT : LFIT = LGrs * VAL(f$) / 100
		CASE "T"
			IncomeTax Emp.Mar, Emp.Dep, LGrs, LFIT
		CASE ELSE
			ErrNum = 3: GOSUB ErrTyp
		END SELECT
		SocSecTax Emp.Ygr, Emp.Yfi, LGrs, LFICA
		LNet = LGrs - LFIT - LFICA - Lins - Lded
		IF LNet <> 0 THEN
		   ChkNum$ = RIGHT$(STR$(100000000 + LChk), 8)
		   WRITE #5, CDat$, Emp.Num, VAL(Tim.Reg) * 100, VAL(Tim.Ovt) * 100, Lbon, Lins, Lded, LReg, LOvt, LGrs, LFIT, LFICA, LNet, ChkNum$
		   LChk = LChk + 1
		   LOCATE , 31: PRINT "Payroll Check Number: "; ChkNum$
		   LPRINT TAB(31); "Payroll Check Number: "; ChkNum$
		END IF
		IF LNet < 0 THEN ErrNum = 4: GOSUB ErrTyp
	END IF
	NEXT x
	COLOR 0, 7: LOCATE 25, 26: PRINT "   PRINTING PAYROLL REGISTER    ";
	FtSw = 1: PageNum = 0: HRegister
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0: L6 = 0: L7 = 0: L8 = 0: L9 = 0: L0 = 0: LN = 0
	La = 0: Lb = 0: Lc = 0: Ld = 0: Le = 0: Lf = 0: Lg = 0: Lh = 0: Li = 0: Lj = 0: Lk = 0
	CLOSE #5: OPEN "Register.DTA" FOR INPUT AS #5
	WHILE NOT EOF(5)
		INPUT #5, CDat$, Emp.Num, Lrh, Loh, Lbo, Lin, Ldd, Lrp, Lop, Lgr, Lft, Lfi, Lnt, ChkNum$
		GET #1, VAL(Emp.Num), Emp
		IF FtSw THEN PGroup$ = Emp.Grp: FtSw = 0
		IF Emp.Grp <> PGroup$ THEN GOSUB 305
		LineNum = LineNum + 1: IF LineNum > 60 THEN HRegister
		LPRINT USING "& "; Emp.Grp + Emp.Num; Emp.Nam; Emp.SSn;
		LPRINT USING "  #####.##"; Lrh / 100; Loh / 100; Lrp / 100; Lop / 100; Lbo / 100; Lgr / 100; Lft / 100; Lfi / 100; Lin / 100; Ldd / 100; Lnt / 100;
		LPRINT "   "; ChkNum$
		L1 = L1 + Lrh: L2 = L2 + Loh: L3 = L3 + Lrp: L4 = L4 + Lop
		L5 = L5 + Lbo: L6 = L6 + Lgr: L7 = L7 + Lft: L8 = L8 + Lfi
		L9 = L9 + Lin: L0 = L0 + Ldd: LN = LN + Lnt
	WEND
	GOSUB 305
	LPRINT TAB(42);
	LPRINT USING "#######.##"; La / 100; Lb / 100; Lc / 100; Ld / 100; Le / 100; Lf / 100; Lg / 100; Lh / 100; Li / 100; Lj / 100; Lk / 100
	CLOSE #5: IF ErrSw2 THEN KILL "Register.DTA"
	GOTO 10
305 PGroup$ = Emp.Grp
	LPRINT : LPRINT TAB(42);
	LPRINT USING "#######.##"; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; L8 / 100; L9 / 100; L0 / 100; LN / 100
	LPRINT : LineNum = LineNum + 3
	La = La + L1: Lb = Lb + L2: Lc = Lc + L3: Ld = Ld + L4
	Le = Le + L5: Lf = Lf + L6: Lg = Lg + L7: Lh = Lh + L8
	Li = Li + L9: Lj = Lj + L0: Lk = Lk + LN
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0
	L6 = 0: L7 = 0: L8 = 0: L9 = 0: L0 = 0: LN = 0
	RETURN
ErrTyp: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	LOCATE , 31: LPRINT TAB(31);
	SELECT CASE ErrNum
		CASE 1
			 LPRINT "Salaried employee with a timecard.          "
			  PRINT "Salaried employee with a timecard.          "
		CASE 2
			 LPRINT "Type of pay must be (S)alary or (W)ages.    "
			  PRINT "Type of pay must be (S)alary or (W)ages.    "
		CASE 3
			 LPRINT "Tax method must be (T)ables or (P)ercentage."
			  PRINT "Tax method must be (T)ables or (P)ercentage."
		CASE 4
			 LPRINT "Net pay is less than one penny.             "
			  PRINT "Net pay is less than one penny.             "
		CASE ELSE
			 LPRINT "Error without an error code.                "
			  PRINT "Error without an error code.                "
	END SELECT
	BEEP: ErrSw2 = 1: RETURN
Checks: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	OpenFiles
	IF ErrSw1 THEN WinMssg "Sorry you must print TimeSheets first": GOTO 10
	IF ErrSw2 THEN WinMssg "Errors were found in Payroll Register": GOTO 10
	IF ErrSw3 THEN WinMssg "Changes in Master, must redo register": GOTO 10
	IF ErrSw4 THEN WinMssg "Changes in TimeCard must redo register": GOTO 10
400 CLOSE #5: OPEN "PR-" + LEFT$(DATE$, 5) + ".DTA" FOR INPUT AS #5
	WinMssg "You can't do payroll twice in one day": GOTO 10
410 CLOSE #5: OPEN "Register.DTA" FOR INPUT AS #5
	IF EOF(5) THEN WinMssg "There are no payroll checks to print": GOTO 10
	LOCATE 15, 30: PRINT "Print Payroll Checks."
	LOCATE 23, 20: PRINT " Press [*] key to print or [Esc] to exit ": KeyHit f$
	IF f$ = "*" THEN
		L1 = 0
		LPRINT CHR$(27); "@"; : WIDTH "LPT1:", 255
		WHILE NOT EOF(5)
			INPUT #5, CDat$, Emp.Num, Lrh, Loh, Lbo, Lin, Ldd, Lrp, Lop, Lgr, Lft, Lfi, Lnt, ChkNum$
			WRITE #7, CDat$, Emp.Num, Lrh, Loh, Lbo, Lin, Ldd, Lrp, Lop, Lgr, Lft, Lfi, Lnt, ChkNum$
			GET #1, VAL(Emp.Num), Emp
			LOCATE 23, 21: PRINT Emp.Num; " "; Emp.Nam; " "; Emp.SSn
			L1 = L1 + Lnt: Number$ = "       "
			RSET Number$ = RIGHT$(STR$(Lnt), 8): NumWords Number$, Word$
			Emp.Qrh = Emp.Qrh + Lrh: Emp.Yrh = Emp.Yrh + Lrh
			Emp.Qoh = Emp.Qoh + Loh: Emp.Yoh = Emp.Yoh + Loh
			Emp.Qrp = Emp.Qrp + Lrp: Emp.Yrp = Emp.Yrp + Lrp
			Emp.Qop = Emp.Qop + Lop: Emp.Yop = Emp.Yop + Lop
			Emp.Qbo = Emp.Qbo + Lbo: Emp.Ybo = Emp.Ybo + Lbo
			Emp.Qgr = Emp.Qgr + Lgr: Emp.Ygr = Emp.Ygr + Lgr
			Emp.Qft = Emp.Qft + Lft: Emp.Yft = Emp.Yft + Lft
			Emp.Qfi = Emp.Qfi + Lfi: Emp.Yfi = Emp.Yfi + Lfi
			Emp.Qin = Emp.Qin + Lin: Emp.Yin = Emp.Yin + Lin
			Emp.Qdd = Emp.Qdd + Ldd: Emp.Ydd = Emp.Ydd + Ldd
			Emp.Qnt = Emp.Qnt + Lnt: Emp.Ynt = Emp.Ynt + Lnt
			PUT #1, VAL(Emp.Num), Emp '----------------Update Employee master-
			'.................................................................
			LPRINT CHR$(27); "@";
			LPRINT CHR$(27); "E";
			LPRINT
			LPRINT
			LPRINT
			LPRINT USING "####.##"; Lrh / 100; Loh / 100; : LPRINT Emp.Pay; TAB(23); : LPRINT USING "#####.## "; Lrp / 100; Lop / 100; : LPRINT TAB(53); : LPRINT USING "#####.##  "; Lbo / 100; Lgr / 100; : LPRINT TAB(73); DATE$
			LPRINT
			LPRINT
			LPRINT USING "####.##"; Emp.Yrh / 100; Emp.Yoh / 100; : LPRINT TAB(22); : LPRINT USING "######.##"; Emp.Yrp / 100; Emp.Yop / 100; : LPRINT TAB(52); : LPRINT USING "######.## "; Emp.Ybo / 100; Emp.Ygr / 100; : LPRINT TAB(74); : LPRINT  _
USING "#####.##"; Lgr / 100
			LPRINT
			LPRINT
			LPRINT
			LPRINT USING " ######.## "; Lfi / 100; Lft / 100; : LPRINT TAB(34); : LPRINT USING "#####.##  "; Lin / 100; Ldd / 100; : LPRINT TAB(62); : LPRINT USING "######.##"; Lnt / 100; : LPRINT TAB(76); ChkNum$
			LPRINT
			LPRINT
			LPRINT USING " ######.## "; Emp.Yfi / 100; Emp.Yft / 100; : LPRINT TAB(34); : LPRINT USING "#####.##  "; Emp.Yin / 100; Emp.Ydd / 100; : LPRINT TAB(62); : LPRINT USING "######.##  "; Emp.Ynt / 100; (Lft + Lfi + Lin + Ldd) / 100
			LPRINT
			LPRINT
			LPRINT "   "; Emp.Nam
			LPRINT "   "; Emp.SSn; TAB(74); : LPRINT USING "#####.##"; Lnt / 100
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT CHR$(27); "G";
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT TAB(12); Emp.Nam; TAB(41); DATE$; TAB(55); ChkNum$; TAB(65); : LPRINT USING "$$#####,.##"; Lnt / 100
			LPRINT
			LPRINT TAB(12); Word$
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			LPRINT
			'....................................................................................
		WEND
		LPRINT CHR$(18)
		IF L1 > 0 THEN
		   GET #3, 81, Acc
		   Acc.Mdb = Acc.Mdb + L1: Acc.Ydb = Acc.Ydb + L1
		   PUT #3, 81, Acc '----------------Debit Payroll Expense (81)-------------
		   GET #3, 1, Acc
		   Acc.Mcr = Acc.Mcr + L1: Acc.Ycr = Acc.Ycr + L1
		   PUT #3, 1, Acc  '----------------Credit Cash in Bank (01)---------------
		   CLOSE : NAME "Register.DTA" AS "PR-" + LEFT$(DATE$, 5) + ".DTA"
				   NAME "Timecard.MAS" AS "TC-" + LEFT$(DATE$, 5) + ".DTA"
		   CLS : SHELL "Copy Timecard.TMP Timecard.MAS"
		   OpenFiles
		ELSE
		   WinMssg "There are no Payroll Checks to print"
		END IF
	END IF
	GOTO 10
Employee: '==================================================================
	OpenFiles
	CALL MhDisplayByName(0, 0, "Employee", e)
500 EntryN 5, 21, 2, 0, e, b, Emp.Num: IF e THEN 10 ELSE IF b THEN 500
	IF Emp.Num < " 1" THEN WinEmp f$: IF f$ = "" THEN 500
	EmpQrh$ = "": EmpYrh$ = ""
	EmpQoh$ = "": EmpYoh$ = ""
	EmpQrp$ = "": EmpYrp$ = ""
	EmpQop$ = "": EmpYop$ = ""
	EmpQbo$ = "": EmpYbo$ = ""
	EmpQgr$ = "": EmpYgr$ = ""
	EmpQft$ = "": EmpYft$ = ""
	EmpQfi$ = "": EmpYfi$ = ""
	EmpQin$ = "": EmpYin$ = ""
	EmpQdd$ = "": EmpYdd$ = ""
	EmpQnt$ = "": EmpYnt$ = ""
	GET #1, VAL(Emp.Num), Emp
	LOCATE 5, 21: PRINT Emp.Num; Emp.Grp
	LOCATE 6, 21: PRINT Emp.Nam
	LOCATE 7, 21: PRINT Emp.Add:  LOCATE 8, 21:  PRINT Emp.Cty
	LOCATE 8, 37: PRINT Emp.Sta:  LOCATE 8, 40:  PRINT Emp.Zip
	LOCATE 5, 68: PRINT Emp.SSn:  LOCATE 6, 68:  PRINT Emp.Tel
	LOCATE 7, 68: PRINT Emp.Hir:  LOCATE 8, 68:  PRINT Emp.Rel
	LOCATE 10, 21: PRINT Emp.Typ: LOCATE 11, 21: PRINT Emp.Mar
	LOCATE 12, 21: PRINT Emp.Dep: LOCATE 13, 21: PRINT Emp.Tax
	LOCATE 10, 71: PRINT Emp.Pay: LOCATE 11, 72: PRINT Emp.Ins
	LOCATE 12, 72: PRINT Emp.Ded: LOCATE 13, 72: PRINT Emp.Bon
			
	Display 17, Emp.Qrh, Emp.Yrh, Emp.Qgr, Emp.Ygr
	Display 18, Emp.Qoh, Emp.Yoh, Emp.Qft, Emp.Yft
	Display 19, Emp.Qrp, Emp.Yrp, Emp.Qfi, Emp.Yfi
	Display 20, Emp.Qop, Emp.Yop, Emp.Qin, Emp.Yin
	Display 21, Emp.Qbo, Emp.Ybo, Emp.Qdd, Emp.Ydd
	Display 23, Emp.Qgr, Emp.Ygr, Emp.Qnt, Emp.Ynt
	
509 EntryU 5, 23, 1, e, b, Emp.Grp:  IF e THEN 10 ELSE IF b THEN 500
510 EntryS 6, 21, 24, e, b, Emp.Nam: IF e THEN 10 ELSE IF b THEN 509
	IF Emp.Nam < "!" THEN WinMssg "Name please!": GOTO 510
511 EntryS 7, 21, 24, e, b, Emp.Add: IF e THEN 10 ELSE IF b THEN 510
512 EntryS 8, 21, 15, e, b, Emp.Cty: IF e THEN 10 ELSE IF b THEN 511
513 EntryU 8, 37, 2, e, b, Emp.Sta:  IF e THEN 10 ELSE IF b THEN 512
514 EntryU 8, 40, 5, e, b, Emp.Zip:  IF e THEN 10 ELSE IF b THEN 513
515 EntryX 5, 68, e, b, Emp.SSn:     IF e THEN 10 ELSE IF b THEN 514
516 EntryS 6, 68, 11, e, b, Emp.Tel: IF e THEN 10 ELSE IF b THEN 515
517 EntryS 7, 68, 11, e, b, Emp.Hir: IF e THEN 10 ELSE IF b THEN 516
518 EntryS 8, 68, 11, e, b, Emp.Rel: IF e THEN 10 ELSE IF b THEN 517
519 EntryU 10, 21, 1, e, b, Emp.Typ: IF e THEN 10 ELSE IF b THEN 518
	IF INSTR("SWH", Emp.Typ) = 0 THEN BEEP: GOTO 519
520 EntryU 11, 21, 1, e, b, Emp.Mar: IF e THEN 10 ELSE IF b THEN 519
	IF INSTR("SM", Emp.Mar) = 0 THEN BEEP: GOTO 520
521 EntryS 12, 21, 1, e, b, Emp.Dep: IF e THEN 10 ELSE IF b THEN 520
	IF INSTR("0123456789", Emp.Dep) = 0 THEN BEEP: GOTO 521
522 EntryU 13, 21, 1, e, b, Emp.Tax: IF e THEN 10 ELSE IF b THEN 521
	IF INSTR("TP", Emp.Tax) = 0 THEN BEEP: GOTO 522
523 EntryN 10, 71, 8, 2, e, b, Emp.Pay: IF e THEN 10 ELSE IF b THEN 522
'***IF Emp.Pay < "    0.01" THEN BEEP: GOTO 523
524 EntryN 11, 72, 7, 2, e, b, Emp.Ins: IF e THEN 10 ELSE IF b THEN 523
525 EntryN 12, 72, 7, 2, e, b, Emp.Ded: IF e THEN 10 ELSE IF b THEN 524
526 EntryN 13, 72, 7, 2, e, b, Emp.Bon: IF e THEN 10 ELSE IF b THEN 525

	COLOR 15, 4: LOCATE 25, 1: SOUND 1200, 2
	PRINT " [*] to save     [U] to modify totals     [E] to erase     Any key to continue  ";
	KeyHit f$
	COLOR 14, 1: LOCATE 25, 1
	PRINT "           Press [Esc] to return to menu without saving this record.            ";
	SELECT CASE f$
	CASE "U" '---------------------------------------------------------------
	   Picture 17, 19, 9, EmpQrh$: Picture 17, 29, 9, EmpYrh$
	   Picture 18, 19, 9, EmpQoh$: Picture 18, 29, 9, EmpYoh$
	   Picture 19, 19, 9, EmpQrp$: Picture 19, 29, 9, EmpYrp$
	   Picture 20, 19, 9, EmpQop$: Picture 20, 29, 9, EmpYop$
	   Picture 21, 19, 9, EmpQbo$: Picture 21, 29, 9, EmpYbo$
	   Picture 23, 19, 9, EmpQgr$: Picture 23, 29, 9, EmpYgr$

	   Picture 18, 61, 9, EmpQft$: Picture 18, 71, 9, EmpYft$
	   Picture 19, 61, 9, EmpQfi$: Picture 19, 71, 9, EmpYfi$
	   Picture 20, 61, 9, EmpQin$: Picture 20, 71, 9, EmpYin$
	   Picture 21, 61, 9, EmpQdd$: Picture 21, 71, 9, EmpYdd$
	   Picture 23, 61, 9, EmpQnt$: Picture 23, 71, 9, EmpYnt$

530    EntryN 17, 19, 9, 2, e, b, EmpQrh$: IF e THEN 500 ELSE IF b THEN 526
531    EntryN 17, 29, 9, 2, e, b, EmpYrh$: IF e THEN 500 ELSE IF b THEN 530
532    EntryN 18, 19, 9, 2, e, b, EmpQoh$: IF e THEN 500 ELSE IF b THEN 531
533    EntryN 18, 29, 9, 2, e, b, EmpYoh$: IF e THEN 500 ELSE IF b THEN 532
534    EntryN 19, 19, 9, 2, e, b, EmpQrp$: IF e THEN 500 ELSE IF b THEN 533
535    EntryN 19, 29, 9, 2, e, b, EmpYrp$: IF e THEN 500 ELSE IF b THEN 534
536    EntryN 20, 19, 9, 2, e, b, EmpQop$: IF e THEN 500 ELSE IF b THEN 535
537    EntryN 20, 29, 9, 2, e, b, EmpYop$: IF e THEN 500 ELSE IF b THEN 536
538    EntryN 21, 19, 9, 2, e, b, EmpQbo$: IF e THEN 500 ELSE IF b THEN 537
539    EntryN 21, 29, 9, 2, e, b, EmpYbo$: IF e THEN 500 ELSE IF b THEN 538

	   LOCATE 23, 19: PRINT USING "######.##"; VAL(EmpQrp$) + VAL(EmpQop$) + VAL(EmpQbo$)
	   Picture 23, 19, 9, EmpQgr$: LOCATE 17, 61: PRINT EmpQgr$
	   LOCATE 23, 29: PRINT USING "######.##"; VAL(EmpYrp$) + VAL(EmpYop$) + VAL(EmpYbo$)
	   Picture 23, 29, 9, EmpYgr$: LOCATE 17, 71: PRINT EmpYgr$

542    EntryN 18, 61, 9, 2, e, b, EmpQft$: IF e THEN 500 ELSE IF b THEN 539
543    EntryN 18, 71, 9, 2, e, b, EmpYft$: IF e THEN 500 ELSE IF b THEN 542
544    EntryN 19, 61, 9, 2, e, b, EmpQfi$: IF e THEN 500 ELSE IF b THEN 543
545    EntryN 19, 71, 9, 2, e, b, EmpYfi$: IF e THEN 500 ELSE IF b THEN 544
546    EntryN 20, 61, 9, 2, e, b, EmpQin$: IF e THEN 500 ELSE IF b THEN 545
547    EntryN 20, 71, 9, 2, e, b, EmpYin$: IF e THEN 500 ELSE IF b THEN 546
548    EntryN 21, 61, 9, 2, e, b, EmpQdd$: IF e THEN 500 ELSE IF b THEN 547
549    EntryN 21, 71, 9, 2, e, b, EmpYdd$: IF e THEN 500 ELSE IF b THEN 548

	   LOCATE 23, 61: PRINT USING "######.##"; VAL(EmpQgr$) - VAL(EmpQft$) - VAL(EmpQfi$) - VAL(EmpQin$) - VAL(EmpQdd$)
	   Picture 23, 61, 9, EmpQnt$
	   LOCATE 23, 71: PRINT USING "######.##"; VAL(EmpYgr$) - VAL(EmpYft$) - VAL(EmpYfi$) - VAL(EmpYin$) - VAL(EmpYdd$)
	   Picture 23, 71, 9, EmpYnt$

	   Emp.Qrh = VAL(EmpQrh$) * 100: Emp.Yrh = VAL(EmpYrh$) * 100
	   Emp.Qoh = VAL(EmpQoh$) * 100: Emp.Yoh = VAL(EmpYoh$) * 100
	   Emp.Qrp = VAL(EmpQrp$) * 100: Emp.Yrp = VAL(EmpYrp$) * 100
	   Emp.Qop = VAL(EmpQop$) * 100: Emp.Yop = VAL(EmpYop$) * 100
	   Emp.Qbo = VAL(EmpQbo$) * 100: Emp.Ybo = VAL(EmpYbo$) * 100
	   Emp.Qgr = VAL(EmpQgr$) * 100: Emp.Ygr = VAL(EmpYgr$) * 100
	   Emp.Qft = VAL(EmpQft$) * 100: Emp.Yft = VAL(EmpYft$) * 100
	   Emp.Qfi = VAL(EmpQfi$) * 100: Emp.Yfi = VAL(EmpYfi$) * 100
	   Emp.Qin = VAL(EmpQin$) * 100: Emp.Yin = VAL(EmpYin$) * 100
	   Emp.Qdd = VAL(EmpQdd$) * 100: Emp.Ydd = VAL(EmpYdd$) * 100
	   Emp.Qnt = VAL(EmpQnt$) * 100: Emp.Ynt = VAL(EmpYnt$) * 100
	   IF Confirm THEN PUT #1, VAL(Emp.Num), Emp ELSE 530
	CASE "*"
	   Inx.Grp = Emp.Grp
	   Inx.Nam = Emp.Nam
	   Inx.Num = Emp.Num
	   PUT #1, VAL(Emp.Num), Emp
	   PUT #4, VAL(Emp.Num), Inx
	   Sort = 1
	   ErrSw3 = 1
	   OpenFiles
	CASE "E"
	   IF Emp.Ygr = 0 THEN
		  f$ = Emp.Num
		  PUT #1, VAL(f$)
		  GET #1, VAL(f$), Emp
		  Emp.Num = f$
		  Inx.Grp = "þ"
		  Inx.Nam = "                        "
		  Inx.Num = Emp.Num
		  PUT #1, VAL(f$), Emp
		  PUT #4, VAL(Emp.Num), Inx
		  Sort = 1
		  ErrSw3 = 1
		  GOTO Employee
	   ELSE
		  WinMssg "Active, no erase"
	   END IF
	CASE ELSE
	END SELECT
	GOTO 500
PrintEmp: '==================================================================
	OpenFiles
	LOCATE 17, 30: PRINT "Print Employee Master.    "
	LOCATE 23, 20: PRINT " Printing Employee Master File, No. [  ] ";
	PageNum = 0: HMaster
	FOR x = 1 TO 99
	GET #6, x, Inx
	GET #1, VAL(Inx.Num), Emp
	IF Emp.Nam > "!" THEN
	   LineNum = LineNum + 5: IF LineNum > 63 THEN HMaster
	   LOCATE 23, 57: PRINT Emp.Num;
	   LPRINT "Employee: "; Emp.Num; " "; Emp.Nam; TAB(80); "           Type of Pay: ";
	   IF Emp.Typ = "S" THEN LPRINT "Salary ";
	   IF Emp.Typ = "W" THEN LPRINT "Hourly ";
	   IF Emp.Typ = "H" THEN LPRINT "On Hold";
	   LPRINT TAB(129); "             Pay Rate: $"; Emp.Pay
	   LPRINT "   Phone: "; Emp.Tel; TAB(40); "Social Security No: "; Emp.SSn; TAB(80); "         Filing Status: ";
	   IF Emp.Mar = "M" THEN LPRINT "Married";
	   IF Emp.Mar = "S" THEN LPRINT "Single ";
	   LPRINT TAB(129); "  Insurance Deduction: $ "; Emp.Ins
	   LPRINT " Address: "; Emp.Add; TAB(40); "        Date Hired: "; Emp.Hir; TAB(80); "Withholding Allowances: ";
	   LPRINT Emp.Dep; " Allowances";
	   LPRINT TAB(129); "     Other Deductions: $ "; Emp.Ded
	   LPRINT "          "; RTRIM$(Emp.Cty) + ", " + Emp.Sta + " " + Emp.Zip; TAB(40); "     Date Released:"; Emp.Rel; TAB(80); "            Tax Method: ";
	   IF Emp.Tax = "T" THEN LPRINT "Tax Rates ";
	   IF Emp.Tax = "P" THEN LPRINT "Percentage";
	   LPRINT TAB(129); "Bonus or Vacation Pay: $ "; Emp.Bon
	   LPRINT STRING$(160, "=")
	END IF
	NEXT
	GOTO 10
ListChk: '===================================================================
	OpenFiles
	CALL MhDisplayByName(0, 0, "CheckList", e)
	f$ = "A ": FDat = "": TDat = ""
700 EntryN 14, 45, 2, 0, e, b, f$: IF e THEN 10 ELSE IF b THEN 700
	IF f$ < " 1" THEN WinEmp f$:   IF f$ = "" THEN 700
	IF f$ < "01" THEN f$ = "0" + RIGHT$(f$, 1)
	LOCATE 14, 45: PRINT f$: IF f$ = "A " THEN A = 1 ELSE A = 0
702 EntryD 15, 45, e, b, FDat:     IF e THEN 10 ELSE IF b THEN 700
	IF FDat < "!" THEN FDat = "01-01": LOCATE 15, 45: PRINT "Begining  "
704 EntryD 16, 45, e, b, TDat:     IF e THEN 10 ELSE IF b THEN 702
	IF TDat < "!" THEN TDat = "12-31": LOCATE 16, 45: PRINT "End       "
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0: L6 = 0: L7 = 0: L8 = 0: L9 = 0: L0 = 0: LN = 0
	FDat = LEFT$(FDat, 5)
	TDat = LEFT$(TDat, 5)
	PageNum = 0: HChecks
	CLOSE #7: OPEN "Employee.CHk" FOR INPUT AS #8
	WHILE NOT EOF(8)
		INPUT #8, CDat$, Emp.Num, Lrh, Loh, Lbo, Lin, Ldd, Lrp, Lop, Lgr, Lft, Lfi, Lnt, ChkNum$
		IF CDat$ >= FDat AND CDat$ <= TDat THEN
		   IF A THEN f$ = Emp.Num
		   IF f$ = Emp.Num THEN
			  GET #1, VAL(Emp.Num), Emp
			  LineNum = LineNum + 1: IF LineNum > 60 THEN HChecks
			  LPRINT USING "& "; Emp.Grp + Emp.Num; Emp.Nam; Emp.SSn;
			  LPRINT USING " #####.##"; Lrh / 100; Loh / 100; Lrp / 100; Lop / 100; Lbo / 100; Lgr / 100; Lft / 100; Lfi / 100;
			  LPRINT USING "  #####.##"; Lin / 100; Ldd / 100; Lnt / 100;
			  LPRINT USING "   &"; ChkNum$; CDat$
			  L1 = L1 + Lrh: L2 = L2 + Loh: L3 = L3 + Lrp: L4 = L4 + Lop
			  L5 = L5 + Lbo: L6 = L6 + Lgr: L7 = L7 + Lft: L8 = L8 + Lfi
			  L9 = L9 + Lin: L0 = L0 + Ldd: LN = LN + Lnt
		   END IF
		END IF
	WEND
	LPRINT : LPRINT TAB(42);
	LPRINT USING "######.##"; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; L8 / 100;
	LPRINT USING " ######.##"; L9 / 100; L0 / 100; LN / 100
	GOTO 10
Quarter: '===================================================================
	OpenFiles
	LOCATE 19, 30: PRINT "Payroll Closing {Quarter}."
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0: L6 = 0: L7 = 0: L8 = 0: L9 = 0: L0 = 0: LN = 0
	La = 0: Lb = 0: Lc = 0: Ld = 0: Le = 0: Lf = 0: Lg = 0: Lh = 0: Li = 0: Lj = 0: Lk = 0
	FtSw = 1: PageNum = 0: HQuarter
	FOR x = 1 TO 99
	TimeDate
	GET #6, x, Inx
	GET #1, VAL(Inx.Num), Emp
	IF Emp.Nam > "!" THEN
	   IF FtSw THEN PGroup$ = Emp.Grp: FtSw = 0
	   IF Emp.Grp <> PGroup$ THEN GOSUB 805
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HQuarter
	   LPRINT USING "& "; Emp.Grp + Emp.Num; Emp.Nam; Emp.SSn;
	   LPRINT USING " #######.##"; Emp.Qrh / 100; Emp.Qoh / 100; Emp.Qrp / 100; Emp.Qop / 100; Emp.Qbo / 100; Emp.Qgr / 100; Emp.Qft / 100; Emp.Qfi / 100; Emp.Qin / 100; Emp.Qdd / 100; Emp.Qnt / 100
	   L1 = L1 + Emp.Qrh: L2 = L2 + Emp.Qoh: L3 = L3 + Emp.Qrp: L4 = L4 + Emp.Qop
	   L5 = L5 + Emp.Qbo: L6 = L6 + Emp.Qgr: L7 = L7 + Emp.Qft: L8 = L8 + Emp.Qfi
	   L9 = L9 + Emp.Qin: L0 = L0 + Emp.Qdd: LN = LN + Emp.Qnt
	END IF
	NEXT
	GOSUB 805
	LPRINT TAB(42);
	LPRINT USING " #######.##"; La / 100; Lb / 100; Lc / 100; Ld / 100; Le / 100; Lf / 100; Lg / 100; Lh / 100; Li / 100; Lj / 100; Lk / 100
	LOCATE 23, 20: PRINT "  Type yes to close the Quarter period.  ": f$ = ""
800 EntryU 23, 27, 3, e, b, f$: IF e THEN 10 ELSE IF b THEN 800
	IF f$ = "YES" THEN
	   LOCATE 23, 20: PRINT "   One moment please, closing QUARTER.   "
	   FOR x = 1 TO 99
	   TimeDate
	   GET #1, x, Emp
	   Emp.Qrh = 0: Emp.Qoh = 0: Emp.Qrp = 0: Emp.Qop = 0
	   Emp.Qbo = 0: Emp.Qgr = 0: Emp.Qft = 0: Emp.Qfi = 0
	   Emp.Qin = 0: Emp.Qdd = 0: Emp.Qnt = 0
	   PUT #1, x, Emp
	   NEXT
	END IF
	GOTO 10
805 PGroup$ = Emp.Grp
	LPRINT : LPRINT TAB(42);
	LPRINT USING " #######.##"; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; L8 / 100; L9 / 100; L0 / 100; LN / 100
	LPRINT : LineNum = LineNum + 3
	La = La + L1: Lb = Lb + L2: Lc = Lc + L3: Ld = Ld + L4
	Le = Le + L5: Lf = Lf + L6: Lg = Lg + L7: Lh = Lh + L8
	Li = Li + L9: Lj = Lj + L0: Lk = Lk + LN
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0
	L6 = 0: L7 = 0: L8 = 0: L9 = 0: L0 = 0: LN = 0
	RETURN
YearEnd: '===================================================================
	OpenFiles
	LOCATE 20, 30: PRINT "Payroll Closing {YearEnd}."
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0: L6 = 0: L7 = 0: L8 = 0: L9 = 0: L0 = 0: LN = 0
	La = 0: Lb = 0: Lc = 0: Ld = 0: Le = 0: Lf = 0: Lg = 0: Lh = 0: Li = 0: Lj = 0: Lk = 0
	FtSw = 1: PageNum = 0: HYearEnd
	FOR x = 1 TO 99
	TimeDate
	GET #6, x, Inx
	GET #1, VAL(Inx.Num), Emp
	IF Emp.Nam > "!" THEN
	   IF FtSw THEN PGroup$ = Emp.Grp: FtSw = 0
	   IF Emp.Grp <> PGroup$ THEN GOSUB 905
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HYearEnd
	   LPRINT USING "& "; Emp.Grp + Emp.Num; Emp.Nam; Emp.SSn;
	   LPRINT USING " #######.##"; Emp.Yrh / 100; Emp.Yoh / 100; Emp.Yrp / 100; Emp.Yop / 100; Emp.Ybo / 100; Emp.Ygr / 100; Emp.Yft / 100; Emp.Yfi / 100; Emp.Yin / 100; Emp.Ydd / 100; Emp.Ynt / 100
	   L1 = L1 + Emp.Yrh: L2 = L2 + Emp.Yoh: L3 = L3 + Emp.Yrp: L4 = L4 + Emp.Yop
	   L5 = L5 + Emp.Ybo: L6 = L6 + Emp.Ygr: L7 = L7 + Emp.Yft: L8 = L8 + Emp.Yfi
	   L9 = L9 + Emp.Yin: L0 = L0 + Emp.Ydd: LN = LN + Emp.Ynt
	END IF
	NEXT
	GOSUB 905
	LPRINT TAB(42);
	LPRINT USING " #######.##"; La / 100; Lb / 100; Lc / 100; Ld / 100; Le / 100; Lf / 100; Lg / 100; Lh / 100; Li / 100; Lj / 100; Lk / 100
	LOCATE 23, 20: PRINT " Type YES to start printing the {W-2}'s. ": f$ = ""
900 EntryU 23, 26, 3, e, b, f$: IF e THEN 10 ELSE IF b THEN 900
	IF f$ = "YES" THEN GOSUB W2Forms
	LOCATE 23, 20: PRINT " If W2's are printed, type YES to close. ": f$ = ""
902 EntryU 23, 47, 3, e, b, f$: IF e THEN 10 ELSE IF b THEN 902
	IF f$ = "YES" THEN
	   LOCATE 23, 20: PRINT "   One moment please, closing YEAREND.   "
	   FOR x = 1 TO 99
	   TimeDate
	   GET #1, x, Emp
	   Emp.Yrh = 0: Emp.Yoh = 0: Emp.Yrp = 0: Emp.Yop = 0
	   Emp.Ybo = 0: Emp.Ygr = 0: Emp.Yft = 0: Emp.Yfi = 0
	   Emp.Yin = 0: Emp.Ydd = 0: Emp.Ynt = 0
	   PUT #1, x, Emp
	   NEXT
	   CLOSE : KILL "Employee.CHK"
	END IF
	GOTO 10
905 PGroup$ = Emp.Grp
	LPRINT : LPRINT TAB(42);
	LPRINT USING " #######.##"; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; L8 / 100; L9 / 100; L0 / 100; LN / 100
	LPRINT : LineNum = LineNum + 3
	La = La + L1: Lb = Lb + L2: Lc = Lc + L3: Ld = Ld + L4
	Le = Le + L5: Lf = Lf + L6: Lg = Lg + L7: Lh = Lh + L8
	Li = Li + L9: Lj = Lj + L0: Lk = Lk + LN
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0
	L6 = 0: L7 = 0: L8 = 0: L9 = 0: L0 = 0: LN = 0
	RETURN
W2Forms: '===================================================================
	LOCATE 23, 20: PRINT " Printing W-2 for [                    ] ";
	LPRINT CHR$(27); "@";
	WIDTH "LPT1:", 255
	LPRINT CHR$(27); "E"; CHR$(27); "G";
	Z = 0
	FOR x = 1 TO 99
	GET #6, x, Inx
	GET #1, VAL(Inx.Num), Emp
	IF Emp.Nam > "!" THEN
	   LOCATE 23, 39: PRINT LEFT$(Emp.Nam, 20);
	   '........................................................................
		Z = Z + 1
		LPRINT "    "; RIGHT$(STR$(100000 + Z), 5)
		LPRINT
		LPRINT
		LPRINT "    Processed Foods Corporation"
		LPRINT
		LPRINT "    707 Willow Avenue          "
		LPRINT "    Knoxville         TN  37915";
		  LPRINT USING "    ########,.##"; Emp.Yft / 100; Emp.Ygr / 100
		LPRINT
		LPRINT "    62-0000000          233-200";
		  IF Emp.Yfi >= 408510 THEN
			 LPRINT USING "    ########,.##"; Emp.Yfi / 100; 5340000 / 100
		  ELSE
			 LPRINT USING "    ########,.##"; Emp.Yfi / 100; Emp.Ygr / 100
		  END IF
		LPRINT
		LPRINT "    "; Emp.SSn
		LPRINT
		LPRINT
		LPRINT "    "; Emp.Nam
		LPRINT
		LPRINT "    "; Emp.Add
		LPRINT "    "; Emp.Cty; "   "; Emp.Sta; "  "; Emp.Zip
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
		LPRINT
	   ':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	END IF
	NEXT
	RETURN
Errors: '====================================================================
	IF ERR = 53 AND ERL = 400 THEN RESUME 410
	IF ERR = 53 AND ERL = 410 THEN WinMssg "Print the register or make corrections": RESUME 10
	WinMssg "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
Terminate: '=================================================================
	CLS : CLOSE : SHELL "Del Register.DTA"
	CLS : SYSTEM
ArrayData: '=================================================================
	DATA "","one "   ,"two "   ,"three "   ,"four "   ,"five "   ,"six "    ,"seven "    ,"eight "   ,"nine "
	DATA "","eleven ","twelve ","thirteen ","fourteen ","fifteen ","sixteen ","seventeen ","eighteen ","nineteen "
	DATA "","ten "   ,"twenty ","thirty "  ,"forty "  ,"fifty "  ,"sixty "  ,"seventy "  ,"eighty "  ,"ninety "
	DATA "Friday   ", "Saturday ", "Monday   ", "Tuesday  ","Wednesday", "Thursday "

DEFSNG A-B, D-Z
FUNCTION Confirm ()
	PCOPY 0, 1
	COLOR 4, 3: LOCATE 13
	LOCATE , 36: PRINT "                     "
	LOCATE , 36: PRINT "  Press [*] to save  "
	LOCATE , 36: PRINT "                     "
	COLOR 11, 0: LOCATE 14
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"
	SOUND 1000, .1: SOUND 800, .3: KeyHit f$
	IF f$ = "*" THEN Confirm = 1 ELSE Confirm = 0
	PCOPY 1, 0
END FUNCTION

DEFINT R
DEFLNG L
DEFSNG C
SUB Display (r, L1, L2, L3, L4)
	LOCATE r, 19: PRINT USING "######.##"; L1 / 100
	LOCATE r, 29: PRINT USING "######.##"; L2 / 100
	LOCATE r, 61: PRINT USING "######.##"; L3 / 100
	LOCATE r, 71: PRINT USING "######.##"; L4 / 100
END SUB

DEFINT A-Q, S-Z
SUB EntryC (e, b)
	 e = 0: b = 0: r = 7 + 2 * d: c = 16 + 5 * T + 29 * w
t10: EntryN r, c, 4, 0, e, b, Clk(w, d, T): IF e THEN EXIT SUB ELSE IF b GOTO t30
	 IF LEFT$(Clk(w, d, T), 2) > "12" THEN BEEP:   GOTO t10
	 IF RIGHT$(Clk(w, d, T), 2) > "59" THEN BEEP:  GOTO t10
	 SELECT CASE T
	 CASE 1
		IF Clk(w, d, 1) > "    " AND Clk(w, d, 0) < "   1" THEN BEEP: GOTO t10
	 CASE 2
		IF Clk(w, d, 2) > "    " AND Clk(w, d, 1) < "   1" THEN BEEP: GOTO t10
	 CASE 3
		IF Clk(w, d, 3) > "    " AND Clk(w, d, 0) < "   1" THEN BEEP: GOTO t10
	 END SELECT
	 Dec(w, d, T) = 100 * VAL(LEFT$(Clk(w, d, T), 2)) + VAL(RIGHT$(Clk(w, d, T), 2)) / .6
t30: IF T = 3 THEN
		Thr(w, 0) = Thr(w, 0) - VAL(Hrs(w, d))
		IF Dec(w, d, 1) < Dec(w, d, 0) THEN Befor = 1200 + Dec(w, d, 1) - Dec(w, d, 0) ELSE Befor = Dec(w, d, 1) - Dec(w, d, 0)
		IF Dec(w, d, 3) < Dec(w, d, 2) THEN After = 1200 + Dec(w, d, 3) - Dec(w, d, 2) ELSE After = Dec(w, d, 3) - Dec(w, d, 2)
		COLOR 15, 6: LOCATE r, 36 + w * 29
		PRINT USING "###.##"; (Befor + After) / 100
		Picture r, 36 + w * 29, 6, S$
		Hrs(w, d) = S$: Thr(w, 0) = Thr(w, 0) + VAL(S$)
		TotalHours
	 END IF
END SUB

DEFSNG A-Z
SUB HChecks ()
	Heading
	LPRINT TAB(70); "  PAYROLL  CHECKS  "
	LPRINT TAB(70); "From "; FDat; " To "; TDat
	LPRINT : LPRINT
	LPRINT "Emp                            Social     Regular  Overtime  Regular Overtime  Bonus    Gross                     Insurance  Other                Check     Check "
	LPRINT "Num      Employee Name        Security     Hours     Hours     Pay     Pay      Pay      Pay      FIT      FICA   Deduction Deduction  Net Pay    Number     Date "
	LPRINT "=== ======================== =========== ========= ======== ======== ======== ======== ======== ======== ======== ========= ========= ========= ========== ======="
END SUB

SUB Heading ()
	 LPRINT CHR$(12); CHR$(27); "@"; : WIDTH "LPT1:", 255: LPRINT CHR$(27); "M"
	 PageNum = PageNum + 1: LineNum = 13
	 LPRINT "Date: "; DATE$; TAB(61); "     Processed Foods Corporation     "; TAB(153); "Page: "; PageNum
	 LPRINT "Time: "; TIME$; TAB(61); "707 Willow Avenue, Knoxville TN 37915"
	 LPRINT :         LPRINT TAB(71); "BIWEEKLY  PAYROLL": LPRINT
END SUB

SUB HitKey (f$)
	DO: f$ = INKEY$: LOOP WHILE f$ <> ""
	DO: f$ = INKEY$: TimeDate: LOOP WHILE f$ = ""
END SUB

SUB HMaster ()
	Heading
	LPRINT TAB(67); " EMPLOYEE PERTINENT DATA "
	LPRINT
	LPRINT "================================================================================================================================================================"
END SUB

SUB HQuarter ()
	Heading
	LPRINT TAB(67); "QUARTERLY PAYROLL REGISTER"
	LPRINT TAB(67); "Quarter Ending: "; DATE$: LPRINT : LPRINT
	LPRINT "                             SOC. SEC.     REGULAR   OVERTIME   REGULAR    OVERTIME               GROSS                          INSURANCE    OTHER       NET    "
	LPRINT "Num      EMPLOYEE NAME         NUMBER        HOURS      HOURS      PAY        PAY       BONUS       PAY        FIT        FICA    DEDUCTION  DEDUCTIONS    PAY    "
	LPRINT "=== ======================== ===========   ========= ========== ========== ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

SUB HRegister ()
	Heading
	LPRINT TAB(70); "BIWEEKLY  REGISTER": LPRINT : LPRINT
	LPRINT "Emp                            Social      Regular  Overtime   Regular  Overtime                                          Insurance   Other               Check   "
	LPRINT "Num      Employee Name        Security      Hours     Hours      Pay      Pay     Bonus Pay Gross Pay    FIT      FICA    Deduction Deduction  Net Pay    Number  "
	LPRINT "=== ======================== ===========  ========= ========= ========= ========= ========= ========= ========= ========= ========= ========= ========= =========="
END SUB

SUB HTimeCard ()
	Heading
	LPRINT TAB(70); "BIWEEKLY  TIMECARDS": LPRINT : LPRINT
	LPRINT "EMP                              SOCIAL                    =============================================   =============================================     TOTAL"
	LPRINT "NUM       EMPLOYEE NAME         SECURITY         DAY       CLOCK-IN    LUNCH-BREAK    CLOCK-OUT    HOURS   CLOCK-IN    LUNCH-BREAK    CLOCK-OUT    HOURS     HOURS"
	LPRINT "===  ========================  ===========   ===========   =============================================   =============================================     ====="
END SUB

SUB HYearEnd ()
	Heading
	LPRINT TAB(67); "YEAR-END  PAYROLL REGISTER"
	LPRINT TAB(67); "  Year Ending: "; DATE$: LPRINT : LPRINT
	LPRINT "                             SOC. SEC.     REGULAR   OVERTIME   REGULAR    OVERTIME               GROSS                          INSURANCE    OTHER       NET    "
	LPRINT "Num      EMPLOYEE NAME         NUMBER        HOURS      HOURS      PAY        PAY       BONUS       PAY        FIT        FICA    DEDUCTION  DEDUCTIONS    PAY    "
	LPRINT "=== ======================== ===========   ========= ========== ========== ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB IncomeTax (Status$, Deductions$, LGrs, LFIT)
	LTI = LGrs - (VAL(Deductions$) * 9615)
	SELECT CASE Status$
	CASE "S"    '=========SINGLE TAX RATES==========
		  SELECT CASE LTI
				 CASE IS > 991900: LFIT = .396 * (LTI - 991900) + 314264
				 CASE IS > 459000: LFIT = .36 * (LTI - 459000) + 122420
				 CASE IS > 199800: LFIT = .31 * (LTI - 199800) + 42068
				 CASE IS > 95200:  LFIT = .28 * (LTI - 95200) + 12780
				 CASE IS > 10000:  LFIT = .15 * (LTI - 10000)
				 CASE ELSE:        LFIT = 0
		  END SELECT
	CASE "M"
		  SELECT CASE LTI
				 CASE IS > 1002100: LFIT = .396 * (LTI - 1002100) + 297156
				 CASE IS > 567900: LFIT = .36 * (LTI - 567900) + 140844
				 CASE IS > 332900: LFIT = .31 * (LTI - 332900) + 67994
				 CASE IS > 165600: LFIT = .28 * (LTI - 165600) + 21150
				 CASE IS > 24600:  LFIT = .15 * (LTI - 24600)
				 CASE ELSE:        LFIT = 0
		  END SELECT
	CASE ELSE
		  LOCATE , 31: PRINT "Filling status must be (S)ingle or (M)arried."
		  LPRINT TAB(31); "Filling status must be (S)ingle or (M)arried."
	END SELECT
END SUB

DEFSNG A-Z
SUB KeyHit (f$)
	DO: f$ = INKEY$: LOOP WHILE f$ > ""
	DO: f$ = INKEY$: LOOP WHILE f$ = ""
	f$ = UCASE$(f$)
END SUB

SUB NumWords (Number$, Word$)
	 Word$ = ""
	 Word$ = RIGHT$(Number$, 2) + " cents."
	 digit$ = MID$(Number$, 4, 2)
	 IF digit$ > "10" AND digit$ < "20" THEN
		Word$ = Dig(2, VAL(RIGHT$(digit$, 1))) + "dollars & " + Word$
	 ELSEIF digit$ > " 0" THEN
		Word$ = Dig(3, VAL(LEFT$(digit$, 1))) + Dig(1, VAL(RIGHT$(digit$, 1))) + "dollars & " + Word$
	 END IF
	 IF VAL(digit$) > 0 AND LEFT$(Number$, 3) > "  0" THEN Word$ = "and " + Word$
	 digit$ = MID$(Number$, 3, 1)
	 IF digit$ > "0" THEN Word$ = Dig(1, VAL(digit$)) + "hundred " + Word$
	 digit$ = LEFT$(Number$, 2)
	 IF digit$ > "10" AND digit$ < "20" THEN
		Word$ = Dig(2, VAL(RIGHT$(digit$, 1))) + "thousand " + Word$
	 ELSEIF digit$ > " 0" THEN
		Word$ = Dig(3, VAL(LEFT$(digit$, 1))) + Dig(1, VAL(RIGHT$(digit$, 1))) + "thousand " + Word$
	 END IF
END SUB

SUB OpenFiles ()
	CLOSE
	IF Sort THEN
	   LOCATE 1
	   SHELL "Sortf Employee.INX Employee.NAM /L27 /+1,25 /Q"
	   Sort = 0
	END IF
	OPEN "Employee.MAS" FOR RANDOM AS #1 LEN = LEN(Emp)
	OPEN "TimeCard.MAS" FOR RANDOM AS #2 LEN = LEN(Tim)
	OPEN "..\AP\Accounts.DTA" FOR RANDOM AS #3 LEN = LEN(Acc)
	OPEN "Employee.INX" FOR RANDOM AS #4 LEN = LEN(Inx)
		'"Register.MAS" reserved   AS #5
	OPEN "Employee.NAM" FOR RANDOM AS #6 LEN = LEN(Inx)
	OPEN "Employee.CHk" FOR APPEND AS #7
END SUB

DEFLNG A-Z
SUB SocSecTax (YGrs, YFICA, Grs, FICA)
	IF Grs = 0 THEN FICA = 0: EXIT SUB
	FX = 379440
	YF = .8104575 * YFICA
	YM = YFICA - YF

	MEDI = .0145 * Grs
	FICA = .062 * Grs

   'adjust FICA if close to maximum
	SELECT CASE YF
		   CASE IS = FX: FICA = 0
		   CASE IS > FX: FICA = 0
		   CASE IS < FX: IF YF + FICA > FX THEN FICA = FX - YF
	END SELECT
   'adjust FICA rounding to year to date FICA total
	SELECT CASE ((YGrs + Grs) * .062) - (YF + FICA)
		   CASE IS < -.5: FICA = FICA - 1
		   CASE IS > .5:  FICA = FICA + 1
	END SELECT
   'adjust MEDICARE rounding to year to date MEDICARE total
	SELECT CASE ((YGrs + Grs) * .0145) - (YM + MEDI)
		   CASE IS < -.5: MEDI = MEDI - 1
		   CASE IS > .5:  MEDI = MEDI + 1
	END SELECT

	FICA = FICA + MEDI  'add medicare
END SUB

DEFSNG A-Z
SUB TimeDate ()
	COLOR 0, 7
	LOCATE 10, 23, 0: PRINT DATE$
	LOCATE 10, 48, 0: PRINT " "; TIME$; " "
END SUB

DEFINT A-Z
SUB TotalHours ()
	IF Thr(w, 0) > 40 THEN Thr(w, 1) = 40: Thr(w, 2) = Thr(w, 0) - 40 ELSE Thr(w, 1) = Thr(w, 0): Thr(w, 2) = 0
	COLOR 11, 6
	LOCATE 19, 36 + w * 29: PRINT USING "###.##"; Thr(w, 0)
	LOCATE 21, 36 + w * 29: PRINT USING "###.##"; Thr(w, 1)
	LOCATE 23, 36 + w * 29: PRINT USING "###.##"; Thr(w, 2)
	LOCATE 19, 74: PRINT USING "###.##"; Thr(0, 0) + Thr(1, 0)
	LOCATE 21, 74: PRINT USING "###.##"; Thr(0, 1) + Thr(1, 1)
	LOCATE 23, 74: PRINT USING "###.##"; Thr(0, 2) + Thr(1, 2)
END SUB

SUB WinEmp (f$)
	PCOPY 0, 1
	CALL MhDisplayByName(0, 0, "EmpList", e)
	Bot = 1: Top = 99: r = 5: n = 1: GOSUB T2
T1: GET #6, n, Inx
	COLOR 15, 1: LOCATE r, 41: PRINT USING "&ณ&"; Inx.Grp + Inx.Num; Inx.Nam
	KeyHit n$
	COLOR 15, 6: LOCATE r, 41: PRINT USING "&ณ&"; Inx.Grp + Inx.Num; Inx.Nam
	SELECT CASE n$
	CASE CHR$(13)
		Picture r, 42, 2, f$
		GET #1, VAL(f$), Emp
		GOTO T3
	CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
		IF n + 1 > 99 GOTO T1 ELSE n = n + 1
		IF r + 1 > 19 THEN CALL scroll(41, 5, 68, 19, 1) ELSE r = r + 1
	CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
		IF n - 1 < 1 GOTO T1 ELSE n = n - 1
		IF r - 1 < 5 THEN CALL BkScroll(41, 5, 68, 19, 1) ELSE r = r - 1
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
		f$ = "": GOTO T3
	END SELECT
	GOTO T1

T2: COLOR 15, 6: LOCATE 5
	FOR x = Bot TO Bot + 14: GET #6, x, Inx
	LOCATE , 41: PRINT USING "&ณ&"; Inx.Grp + Inx.Num; Inx.Nam
	NEXT
	RETURN
T3: PCOPY 1, 0
	COLOR 14, 0
END SUB

DEFSNG A-Z
SUB WinMssg (f$)
	PCOPY 0, 1
	SCREEN , , 1, 1
	COLOR 14, 4: LOCATE 12
	LOCATE , 36: PRINT " ษอ"; STRING$(LEN(f$), "อ"); "อป "
	LOCATE , 36: PRINT " บ "; f$; " บ "
	LOCATE , 36: PRINT " ศอ"; STRING$(LEN(f$), "อ"); "อผ "
	COLOR 12, 0: LOCATE 13
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "฿฿฿"; STRING$(LEN(f$) + 2, "฿")
	SOUND 660, 1: SOUND 770, 3: KeyHit f$
	SCREEN , , 0, 0
END SUB

