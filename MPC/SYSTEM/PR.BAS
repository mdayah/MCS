DEFINT A-Z: DEFLNG L

DECLARE SUB EmpSort ()
DECLARE SUB EmpShow ()
DECLARE SUB EmpEdit (n)
DECLARE SUB EmpWind (f$)
DECLARE SUB ChkShow ()
DECLARE SUB ChkWind ()
DECLARE SUB ChkPrnt ()
DECLARE SUB ChkPost ()
DECLARE SUB ChkVoid (n)
DECLARE SUB ChkFind (f, t)

DECLARE SUB HprReg ()
DECLARE SUB HprChk ()
DECLARE SUB HMaster ()
DECLARE SUB HMonth ()
DECLARE SUB HQuarter ()
DECLARE SUB HYear ()
DECLARE SUB HTimeCard ()

DECLARE SUB PR_Files ()
DECLARE SUB NumWords (n$, f$)
DECLARE SUB EmpLine (r, L0, L1, L2)
DECLARE SUB IncomeTax (L1, L2)
DECLARE SUB SocSecTax (L1, L2)
DECLARE SUB Medicare (L1, L2)

'$INCLUDE: '..\Include\Library.REC'
'$INCLUDE: '..\Include\Employee.REC'

	DIM SHARED Emp AS Employee
	DIM SHARED Emx AS EmpIndex
	DIM SHARED Emc AS EmpCheck
	DIM SHARED Ecx AS ChkIndex
	
	DIM SHARED fDat         AS STRING
	DIM SHARED tDat         AS STRING
	DIM SHARED Tot(2)       AS INTEGER
	DIM SHARED Hrs(1, 2)    AS INTEGER
	DIM SHARED Dec(1, 6, 3) AS INTEGER

DEFINT L
SUB ChkFind (f, e)
	f = 0: e = 0: b = 1: t = LOF(5) / LEN(Ecx)
	DO UNTIL (t < b)
	   m = (t + b) \ 2: GET #5, m, Ecx
	   IF VAL(Emx.Num) > Ecx.Emp THEN b = m + 1 ELSE t = m - 1
	LOOP
	DO WHILE VAL(Emx.Num) > Ecx.Emp AND NOT EOF(5)
	   GET #5, , Ecx
	LOOP
	IF VAL(Emx.Num) = Ecx.Emp THEN
	   f = LOC(5): e = f
	   DO WHILE NOT EOF(5)
		  GET #5, , Ecx
		  IF VAL(Emx.Num) = Ecx.Emp THEN e = LOC(5) ELSE EXIT DO
	   LOOP
	END IF
END SUB

DEFSNG A-Z
SUB ChkPost ()
'   ON LOCAL ERROR GOTO prer1
	Emp.Mrh = Emp.Mrh + Emc.Lrh: Emp.Qrh = Emp.Qrh + Emc.Lrh: Emp.Yrh = Emp.Yrh + Emc.Lrh'regular hours
	Emp.Moh = Emp.Moh + Emc.Loh: Emp.Qoh = Emp.Qoh + Emc.Loh: Emp.Yoh = Emp.Yoh + Emc.Loh'overtime hours
	Emp.Mrp = Emp.Mrp + Emc.Lrp: Emp.Qrp = Emp.Qrp + Emc.Lrp: Emp.Yrp = Emp.Yrp + Emc.Lrp'regular pay
	Emp.Mop = Emp.Mop + Emc.Lop: Emp.Qop = Emp.Qop + Emc.Lop: Emp.Yop = Emp.Yop + Emc.Lop'overtime pay
	Emp.Mbo = Emp.Mbo + Emc.Lbo: Emp.Qbo = Emp.Qbo + Emc.Lbo: Emp.Ybo = Emp.Ybo + Emc.Lbo'bonus pay
	Emp.Mgr = Emp.Mgr + Emc.Lgr: Emp.Qgr = Emp.Qgr + Emc.Lgr: Emp.Ygr = Emp.Ygr + Emc.Lgr'gross pay
	Emp.Mft = Emp.Mft + Emc.Lft: Emp.Qft = Emp.Qft + Emc.Lft: Emp.Yft = Emp.Yft + Emc.Lft'FIT
	Emp.Mfi = Emp.Mfi + Emc.Lfi: Emp.Qfi = Emp.Qfi + Emc.Lfi: Emp.Yfi = Emp.Yfi + Emc.Lfi'FICA
	Emp.Mmd = Emp.Mmd + Emc.Lmd: Emp.Qmd = Emp.Qmd + Emc.Lmd: Emp.Ymd = Emp.Ymd + Emc.Lmd'MEDICARE
	Emp.Min = Emp.Min + Emc.Lin: Emp.Qin = Emp.Qin + Emc.Lin: Emp.Yin = Emp.Yin + Emc.Lin'Insurance
	Emp.Mod = Emp.Mod + Emc.Lod: Emp.Qod = Emp.Qod + Emc.Lod: Emp.Yod = Emp.Yod + Emc.Lod'other deduction
	Emp.Mnt = Emp.Mnt + Emc.Lnt: Emp.Qnt = Emp.Qnt + Emc.Lnt: Emp.Ynt = Emp.Ynt + Emc.Lnt'net pay
	PUT #2, VAL(Emc.Emp), Emp '------(Update Employee master)-------------
	
	LOCK #1, 90 : GET #1, 90, Acc  '-----------------------------------------
	Acc.Mdb = Acc.Mdb + Emc.Lgr                     'Gross + FICA + Medicare
	Acc.Ydb = Acc.Ydb + Emc.Lgr                     'Gross + FICA + Medicare
	PUT #1, 90, Acc: UNLOCK #1, 90 '------------(90-Payroll Expense.......DB)

	LOCK #1, 73 : GET #1, 73, Acc  '-----------------------------------------
	Acc.Mdb = Acc.Mdb + Emc.Lfi + Emc.Lmd           'FICA + Med
	Acc.Ydb = Acc.Ydb + Emc.Lfi + Emc.Lmd           'FICA + Med
	PUT #1, 73, Acc: UNLOCK #1, 73 '------------(73-Tax and Licenses......DB)

	LOCK #1, 35 : GET #1, 35, Acc  '-----------------------------------------
	Acc.Mcr = Acc.Mcr + Emc.Lft + Emc.Lfi + Emc.Lfi + Emc.Lmd + Emc.Lmd 'FIT + 2(FICA+Med)
	Acc.Ycr = Acc.Ycr + Emc.Lft + Emc.Lfi + Emc.Lfi + Emc.Lmd + Emc.Lmd 'FIT + 2(FICA+Med)
	PUT #1, 35, Acc: UNLOCK #1, 35 '------------(35-Payroll Tax Payable...CR)

	LOCK #1, 36 : GET #1, 36, Acc  '-----------------------------------------
	Acc.Mcr = Acc.Mcr + Emc.Lin                     'Insurance
	Acc.Ycr = Acc.Ycr + Emc.Lin                     'Insurance
	PUT #1, 36, Acc: UNLOCK #1, 36 '------------(36-Employee Insurance....CR)

	LOCK #1, 99 : GET #1, 99, Acc  '-----------------------------------------
	Acc.Mcr = Acc.Mcr + Emc.Lod                     'Other deduction
	Acc.Ycr = Acc.Ycr + Emc.Lod                     'Other deduction
	PUT #1, 99, Acc: UNLOCK #1, 99 '------------(99-Employee Deductions...CR)

	LOCK #1, 1 : GET #1, 1, Acc    '-----------------------------------------
	Acc.Mcr = Acc.Mcr + Emc.Lnt                     'Net
	Acc.Ycr = Acc.Ycr + Emc.Lnt                     'Net
	PUT #1, 1, Acc: UNLOCK #1, 1   '------------(01-Cash in Bank..........CR)
'EXIT SUB
'prer1:
'   IF ERR = 70 THEN
'      DO:
'           WHILE Tries < 10
'               Tries = Tries + 1
'               RESUME
'           WEND
'           WinMssg "Record in use, try again?"
'           Tries = 0
'      LOOP
'   ELSE
'      ON LOCAL ERROR GOTO 0
'      ERROR ERR
'   END IF
END SUB

SUB ChkPrnt ()
	Number$ = "       "
	RSET Number$ = RIGHT$(STR$(Emc.Lnt), 7)
	NumWords Number$, word$
	PRINT #11, CHR$(27); "@";
	WIDTH "LPT1:", 255
	PRINT #11, CHR$(27); "E";
	PRINT #11,
	PRINT #11, "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ»"
	PRINT #11, "º      Employee Name       ³Social Security Number³  Check Number  ³   Date   º"
	PRINT #11, "ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ¶"
	PRINT #11, USING "º & ³     &      ³    &    ³&º"; Emp.Nam; Emp.SSn; Emc.Chk; Emc.Dat
	PRINT #11, "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ¼"
	PRINT #11, "ÉÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍ»"
	PRINT #11, "º  Regular   ³  OverTime  ³   Regular  ³  OverTime  ³ Additional ³            º"
	PRINT #11, "º   Hours    ³   Hours    ³     Pay    ³     Pay    ³    Pay     ³  Gross Pay º"
	PRINT #11, "ÇÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ¶"
	PRINT #11, "º"; : PRINT #11, USING " ######,.## ³"; Emc.Lrh / 100; Emc.Loh / 100; Emc.Lrp / 100; Emc.Lop / 100; Emc.Lbo / 100; : PRINT #11, USING " ######,.## º"; Emc.Lgr / 100
	PRINT #11, "º"; : PRINT #11, USING " ######,.## ³"; Emp.Yrh / 100; Emp.Yoh / 100; Emp.Yrp / 100; Emp.Yop / 100; Emp.Ybo / 100; : PRINT #11, USING " ######,.## º"; Emp.Ygr / 100
	PRINT #11, "ÌÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍ¹"
	PRINT #11, "º            ³            ³  Medicare  ³ Insurance  ³   Other    ³            º"
	PRINT #11, "º    FICA    ³    FIT     ³  Deduction ³ Deductions ³ Deductions ³   Net Pay  º"
	PRINT #11, "ÇÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ¶"
	PRINT #11, "º"; : PRINT #11, USING " ######,.## ³"; Emc.Lfi / 100; Emc.Lft / 100; Emc.Lmd / 100; Emc.Lin / 100; Emc.Lod / 100; : PRINT #11, USING " ######,.## º"; Emc.Lnt / 100
	PRINT #11, "º"; : PRINT #11, USING " ######,.## ³"; Emp.Yfi / 100; Emp.Yft / 100; Emp.Ymd / 100; Emp.Yin / 100; Emp.Yod / 100; : PRINT #11, USING " ######,.## º"; Emp.Ynt / 100
	PRINT #11, "ÈÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍ¼"
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11, CHR$(27); "G";
	PRINT #11,
	PRINT #11, TAB(70); Emc.Chk
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11, TAB(51); Emc.Dat; TAB(68); : PRINT #11, USING "$$#####,.##"; Emc.Lnt / 100
	PRINT #11, word$
	PRINT #11,
	PRINT #11, TAB(8); Emp.Nam
	PRINT #11, TAB(8); Emp.Add
	PRINT #11, TAB(8); RTRIM$(Emp.Cty); ", "; Emp.Sta; " "; Emp.Zip
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,
END SUB

SUB ChkShow ()
	PCOPY 0, 2: CALL MhDisplayByName(0, 0, "PR-Check", e)
	GET #2, VAL(Emc.Emp), Emp

	Number$ = "       "
	RSET Number$ = RIGHT$(STR$(Emc.Lnt), 7)
	NumWords Number$, word$

	COLOR 16, 7
	LOCATE 17, 6:  PRINT USING "#####.##"; Emc.Lrh / 100;
	LOCATE 17, 15: PRINT USING "#####.##"; Emc.Loh / 100;
	LOCATE 17, 25: PRINT USING "#####.##"; Emc.Lrp / 100;
	LOCATE 17, 35: PRINT USING "#####.##"; Emc.Lop / 100;
	LOCATE 17, 45: PRINT USING "#####.##"; Emc.Lbo / 100;
	LOCATE 17, 54: PRINT USING "$$#####,.##"; Emc.Lgr / 100;

	LOCATE 22, 6:  PRINT USING "#####.##"; Emc.Lfi / 100;
	LOCATE 22, 15: PRINT USING "#####.##"; Emc.Lft / 100;
	LOCATE 22, 25: PRINT USING "#####.##"; Emc.Lmd / 100;
	LOCATE 22, 35: PRINT USING "#####.##"; Emc.Lin / 100;
	LOCATE 22, 45: PRINT USING "#####.##"; Emc.Lod / 100;
	LOCATE 22, 54: PRINT USING "$$#####,.##"; Emc.Lnt / 100;

	COLOR 0, 7
	LOCATE 32, 54: PRINT USING "$$#####,.##"; Emc.Lnt / 100;

	qPrint Emp.Nam, 11, 6, 0, 1
	qPrint Emp.SSn, 11, 34, 0, 1
	qPrint Emc.Chk, 11, 46, 0, 1
	qPrint Emc.Dat, 11, 55, 0, 1

	qPrint Company, 26, 6, 0, 1
	qPrint Street, 27, 6, 0, 1
	qPrint Address, 28, 6, 0, 1
	qPrint City, 29, 6, 0, 1
	
	qPrint Emc.Chk, 26, 57, 0, 1
	qPrint Emc.Dat, 32, 40, 0, 1
	qPrint word$, 34, 6, 0, 1

	qPrint Emp.Nam, 36, 17, 0, 1
	qPrint Emp.Add, 37, 17, 0, 1
	qPrint RTRIM$(Emp.Cty) + ", " + Emp.Sta + " " + Emp.Zip, 38, 17, 0, 1
END SUB

DEFINT A-Z
SUB ChkVoid (n)
'    ON LOCAL ERROR GOTO prer2
	Emp.Mrh = Emp.Mrh - Emc.Lrh: Emp.Qrh = Emp.Qrh - Emc.Lrh: Emp.Yrh = Emp.Yrh - Emc.Lrh'regular hours
	Emp.Moh = Emp.Moh - Emc.Loh: Emp.Qoh = Emp.Qoh - Emc.Loh: Emp.Yoh = Emp.Yoh - Emc.Loh'overtime hours
	Emp.Mrp = Emp.Mrp - Emc.Lrp: Emp.Qrp = Emp.Qrp - Emc.Lrp: Emp.Yrp = Emp.Yrp - Emc.Lrp'regular pay
	Emp.Mop = Emp.Mop - Emc.Lop: Emp.Qop = Emp.Qop - Emc.Lop: Emp.Yop = Emp.Yop - Emc.Lop'overtime pay
	Emp.Mbo = Emp.Mbo - Emc.Lbo: Emp.Qbo = Emp.Qbo - Emc.Lbo: Emp.Ybo = Emp.Ybo - Emc.Lbo'bonus pay
	Emp.Mgr = Emp.Mgr - Emc.Lgr: Emp.Qgr = Emp.Qgr - Emc.Lgr: Emp.Ygr = Emp.Ygr - Emc.Lgr'gross pay
	Emp.Mft = Emp.Mft - Emc.Lft: Emp.Qft = Emp.Qft - Emc.Lft: Emp.Yft = Emp.Yft - Emc.Lft'FIT
	Emp.Mfi = Emp.Mfi - Emc.Lfi: Emp.Qfi = Emp.Qfi - Emc.Lfi: Emp.Yfi = Emp.Yfi - Emc.Lfi'FICA
	Emp.Mmd = Emp.Mmd - Emc.Lmd: Emp.Qmd = Emp.Qmd - Emc.Lmd: Emp.Ymd = Emp.Ymd - Emc.Lmd'MEDICARE
	Emp.Min = Emp.Min - Emc.Lin: Emp.Qin = Emp.Qin - Emc.Lin: Emp.Yin = Emp.Yin - Emc.Lin'Insurance
	Emp.Mod = Emp.Mod - Emc.Lod: Emp.Qod = Emp.Qod - Emc.Lod: Emp.Yod = Emp.Yod - Emc.Lod'other deduction
	Emp.Mnt = Emp.Mnt - Emc.Lnt: Emp.Qnt = Emp.Qnt - Emc.Lnt: Emp.Ynt = Emp.Ynt - Emc.Lnt'net pay
	PUT #2, VAL(Emp.Num), Emp '------(Update Employee master)-------------

	LOCK #1, 90 : GET #1, 90, Acc   '-----------------------------------------
	Acc.Mdb = Acc.Mdb - Emc.Lgr                  'Gross + FICA + Medicare
	Acc.Ydb = Acc.Ydb - Emc.Lgr                  'Gross + FICA + Medicare
	PUT #1, 90, Acc: UNLOCK #1, 90  '------------(90-Payroll Expense.......DB)

	LOCK #1, 73 : GET #1, 73, Acc   '-----------------------------------------
	Acc.Mdb = Acc.Mdb - Emc.Lfi - Emc.Lmd        'FICA + Med
	Acc.Ydb = Acc.Ydb - Emc.Lfi - Emc.Lmd        'FICA + Med
	PUT #1, 73, Acc: UNLOCK #1, 73  '------------(73-Tax and Licenses......DB)

	LOCK #1, 35 : GET #1, 35, Acc   '-----------------------------------------
	Acc.Mcr = Acc.Mcr - Emc.Lft - Emc.Lfi - Emc.Lfi - Emc.Lmd - Emc.Lmd 'FIT + 2(FICA+Med)
	Acc.Ycr = Acc.Ycr - Emc.Lft - Emc.Lfi - Emc.Lfi - Emc.Lmd - Emc.Lmd 'FIT + 2(FICA+Med)
	PUT #1, 35, Acc: UNLOCK #1, 35  '------------(35-Payroll Tax Payable...CR)

	LOCK #1, 36 : GET #1, 36, Acc   '-----------------------------------------
	Acc.Mcr = Acc.Mcr - Emc.Lin                  'Insurance
	Acc.Ycr = Acc.Ycr - Emc.Lin                  'Insurance
	PUT #1, 36, Acc: UNLOCK #1, 36  '------------(36-Employee Insurance....CR)

	LOCK #1, 99 : GET #1, 99, Acc   '-----------------------------------------
	Acc.Mcr = Acc.Mcr - Emc.Lod                  'Other deduction
	Acc.Ycr = Acc.Ycr - Emc.Lod                  'Other deduction
	PUT #1, 99, Acc: UNLOCK #1, 99  '------------(99-Employee Deductions...CR)

	LOCK #1, 1 : GET #1, 1, Acc     '-----------------------------------------
	Acc.Mcr = Acc.Mcr - Emc.Lnt                  'Net
	Acc.Ycr = Acc.Ycr - Emc.Lnt                  'Net
	PUT #1, 1, Acc: UNLOCK #1, 1    '------------(01-Cash in Bank..........CR)

	Emc.Chk = "Void    "
	Emc.Lrh = 0
	Emc.Loh = 0
	Emc.Lrp = 0
	Emc.Lop = 0
	Emc.Lbo = 0
	Emc.Lgr = 0
	Emc.Lft = 0
	Emc.Lfi = 0
	Emc.Lmd = 0
	Emc.Lin = 0
	Emc.Lod = 0
	Emc.Lnt = 0
	PUT #4, n, Emc  '----------------(Update Check master)----------------
'EXIT SUB
'prer2:
'   IF ERR = 70 THEN
'      DO:
'           WHILE Tries < 10
'               Tries = Tries + 1
'               RESUME
'           WEND
'           WinMssg "Record in use, try again?"
'           Tries = 0
'      LOOP
'   ELSE
'      ON LOCAL ERROR GOTO 0
'      ERROR ERR
'   END IF
END SUB

SUB ChkWind ()
	ChkFind f, t
	IF f = 0 THEN WinMssg "No checks for this employee": EXIT SUB
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "pr-ChkList", e)
	r = 20: b = f: n = f: GOSUB m2
m1: GET #5, n, Ecx: GET #4, Ecx.Num, Emc
	COLOR 11, 7: LOCATE r, 23: PRINT USING "&³ & ³ #####.## "; Emc.Dat; Emc.Chk; Emc.Lnt / 100;
	KeyPres f$
	COLOR 27, 1: LOCATE r, 23: PRINT USING "&³ & ³ #####.## "; Emc.Dat; Emc.Chk; Emc.Lnt / 100;
	SELECT CASE f$
	CASE CHR$(0) + "P"
		 IF n + 1 > t GOTO m1 ELSE n = n + 1
		 IF r + 1 > 34 THEN CALL scroll(23, 20, 54, 34, 1) ELSE r = r + 1
	CASE CHR$(0) + "H"
		 IF n - 1 < f GOTO m1 ELSE n = n - 1
		 IF r - 1 < 20 THEN CALL BkScroll(23, 20, 54, 34, 1) ELSE r = r - 1
	CASE CHR$(13)
		 ChkShow
		 KeyPres f$
		 PCOPY 2, 0
	CASE ELSE
		 PCOPY 1, 0
		 EXIT SUB
	END SELECT
	GOTO m1

m2: COLOR 27, 1: LOCATE 20
	FOR x = b TO t
	GET #5, x, Ecx: GET #4, Ecx.Num, Emc
	LOCATE , 23: PRINT USING "&³ & ³ #####.## "; Emc.Dat; Emc.Chk; Emc.Lnt / 100
	IF CSRLIN > 34 THEN EXIT FOR
	NEXT
	RETURN
END SUB

SUB EmpEdit (n)
	EmpShow
	qPrint Emx.Num, 3, 53, 0, 1
212 EntryU 4, 53, 1, e, b, Emp.Dpt:     IF e THEN 260 ELSE IF b THEN 212
213 EntryS 6, 53, 24, e, b, Emp.Nam:    IF e THEN 260 ELSE IF b THEN 212
	IF Emp.Nam < "!" THEN WinMssg "Must have name": GOTO 213
214 EntryS 7, 53, 24, e, b, Emp.Add:    IF e THEN 260 ELSE IF b THEN 213
215 EntryS 8, 53, 15, e, b, Emp.Cty:    IF e THEN 260 ELSE IF b THEN 214
216 EntryU 8, 69, 2, e, b, Emp.Sta:     IF e THEN 260 ELSE IF b THEN 215
217 EntryN 8, 72, 5, 0, e, b, Emp.Zip:  IF e THEN 260 ELSE IF b THEN 216

218 EntryU 11, 53, 1, e, b, Emp.Typ:    IF e THEN 260 ELSE IF b THEN 217
	IF INSTR("SWH", Emp.Typ) = 0 THEN WinMssg "Salary, Wages, or Hold": GOTO 218
219 EntryU 12, 53, 1, e, b, Emp.Mar:    IF e THEN 260 ELSE IF b THEN 218
	IF INSTR("SM", Emp.Mar) = 0 THEN WinMssg "Married or Single": GOTO 219
220 EntryN 13, 53, 1, 0, e, b, Emp.Dep: IF e THEN 260 ELSE IF b THEN 219
	IF Emp.Dep = " " THEN Emp.Dep = "0": qPrint Emp.Dep, 12, 54, 0, 1
221 EntryU 14, 53, 1, e, b, Emp.Tax:    IF e THEN 260 ELSE IF b THEN 220
	IF INSTR("TP", Emp.Tax) = 0 THEN WinMssg "Tax tables or Percentage method": GOTO 221

222 EntryX 17, 62, e, b, Emp.SSn:       IF e THEN 260 ELSE IF b THEN 221
223 EntryT 18, 62, e, b, Emp.Tel:       IF e THEN 260 ELSE IF b THEN 222
224 EntryD 19, 62, e, b, Emp.Hir:       IF e THEN 260 ELSE IF b THEN 223
225 EntryD 20, 62, e, b, Emp.Rel:       IF e THEN 260 ELSE IF b THEN 224

226 EntryN 23, 63, 8, 2, e, b, Emp.Pay: IF e THEN 260 ELSE IF b THEN 225
227 EntryN 24, 63, 8, 2, e, b, Emp.Ins: IF e THEN 260 ELSE IF b THEN 226
228 EntryN 25, 63, 8, 2, e, b, Emp.Ded: IF e THEN 260 ELSE IF b THEN 227
229 EntryN 26, 63, 8, 2, e, b, Emp.Bon: IF e THEN 260 ELSE IF b THEN 228

	Picture 31, 48, 10, EmpMrh$: Picture 31, 59, 10, EmpQrh$: Picture 31, 70, 10, EmpYrh$
	Picture 32, 48, 10, EmpMoh$: Picture 32, 59, 10, EmpQoh$: Picture 32, 70, 10, EmpYoh$
	Picture 33, 48, 10, EmpMrp$: Picture 33, 59, 10, EmpQrp$: Picture 33, 70, 10, EmpYrp$
	Picture 34, 48, 10, EmpMop$: Picture 34, 59, 10, EmpQop$: Picture 34, 70, 10, EmpYop$
	Picture 35, 48, 10, EmpMbo$: Picture 35, 59, 10, EmpQbo$: Picture 35, 70, 10, EmpYbo$

	Picture 37, 48, 10, EmpMgr$: Picture 37, 59, 10, EmpQgr$: Picture 37, 70, 10, EmpYgr$

	Picture 39, 48, 10, EmpMft$: Picture 39, 59, 10, EmpQft$: Picture 39, 70, 10, EmpYft$
	Picture 40, 48, 10, EmpMfi$: Picture 40, 59, 10, EmpQfi$: Picture 40, 70, 10, EmpYfi$
	Picture 41, 48, 10, EmpMmd$: Picture 41, 59, 10, EmpQmd$: Picture 41, 70, 10, EmpYmd$
	Picture 42, 48, 10, EmpMin$: Picture 42, 59, 10, EmpQin$: Picture 42, 70, 10, EmpYin$
	Picture 43, 48, 10, EmpMod$: Picture 43, 59, 10, EmpQod$: Picture 43, 70, 10, EmpYod$

	Picture 45, 48, 10, EmpMgr$: Picture 45, 59, 10, EmpQgr$: Picture 45, 70, 10, EmpYgr$

	Picture 47, 48, 10, EmpMnt$: Picture 47, 59, 10, EmpQnt$: Picture 47, 70, 10, EmpYnt$

230 EntryN 31, 48, 10, 2, e, b, EmpMrh$: IF e THEN 260 ELSE IF b THEN 229
231 EntryN 31, 59, 10, 2, e, b, EmpQrh$: IF e THEN 260 ELSE IF b THEN 230
232 EntryN 31, 70, 10, 2, e, b, EmpYrh$: IF e THEN 260 ELSE IF b THEN 231
233 EntryN 32, 48, 10, 2, e, b, EmpMoh$: IF e THEN 260 ELSE IF b THEN 232
234 EntryN 32, 59, 10, 2, e, b, EmpQoh$: IF e THEN 260 ELSE IF b THEN 233
235 EntryN 32, 70, 10, 2, e, b, EmpYoh$: IF e THEN 260 ELSE IF b THEN 234
236 EntryN 33, 48, 10, 2, e, b, EmpMrp$: IF e THEN 260 ELSE IF b THEN 235
237 EntryN 33, 59, 10, 2, e, b, EmpQrp$: IF e THEN 260 ELSE IF b THEN 236
238 EntryN 33, 70, 10, 2, e, b, EmpYrp$: IF e THEN 260 ELSE IF b THEN 237
239 EntryN 34, 48, 10, 2, e, b, EmpMop$: IF e THEN 260 ELSE IF b THEN 238
240 EntryN 34, 59, 10, 2, e, b, EmpQop$: IF e THEN 260 ELSE IF b THEN 239
241 EntryN 34, 70, 10, 2, e, b, EmpYop$: IF e THEN 260 ELSE IF b THEN 240
242 EntryN 35, 48, 10, 2, e, b, EmpMbo$: IF e THEN 260 ELSE IF b THEN 241
243 EntryN 35, 59, 10, 2, e, b, EmpQbo$: IF e THEN 260 ELSE IF b THEN 242
244 EntryN 35, 70, 10, 2, e, b, EmpYbo$: IF e THEN 260 ELSE IF b THEN 243

	LOCATE 37, 48: PRINT USING "#######.##"; VAL(EmpMrp$) + VAL(EmpMop$) + VAL(EmpMbo$);
	LOCATE 37, 59: PRINT USING "#######.##"; VAL(EmpQrp$) + VAL(EmpQop$) + VAL(EmpQbo$);
	LOCATE 37, 70: PRINT USING "#######.##"; VAL(EmpYrp$) + VAL(EmpYop$) + VAL(EmpYbo$);
	Picture 37, 48, 10, EmpMgr$
	Picture 37, 59, 10, EmpQgr$
	Picture 37, 70, 10, EmpYgr$

245 EntryN 39, 48, 10, 2, e, b, EmpMft$: IF e THEN 260 ELSE IF b THEN 244
246 EntryN 39, 59, 10, 2, e, b, EmpQft$: IF e THEN 260 ELSE IF b THEN 245
247 EntryN 39, 70, 10, 2, e, b, EmpYft$: IF e THEN 260 ELSE IF b THEN 246
248 EntryN 40, 48, 10, 2, e, b, EmpMfi$: IF e THEN 260 ELSE IF b THEN 247
249 EntryN 40, 59, 10, 2, e, b, EmpQfi$: IF e THEN 260 ELSE IF b THEN 248
250 EntryN 40, 70, 10, 2, e, b, EmpYfi$: IF e THEN 260 ELSE IF b THEN 249
251 EntryN 41, 48, 10, 2, e, b, EmpMmd$: IF e THEN 260 ELSE IF b THEN 250
252 EntryN 41, 59, 10, 2, e, b, EmpQmd$: IF e THEN 260 ELSE IF b THEN 251
253 EntryN 41, 70, 10, 2, e, b, EmpYmd$: IF e THEN 260 ELSE IF b THEN 252
254 EntryN 42, 48, 10, 2, e, b, EmpMin$: IF e THEN 260 ELSE IF b THEN 253
255 EntryN 42, 59, 10, 2, e, b, EmpQin$: IF e THEN 260 ELSE IF b THEN 254
256 EntryN 42, 70, 10, 2, e, b, EmpYin$: IF e THEN 260 ELSE IF b THEN 255
257 EntryN 43, 48, 10, 2, e, b, EmpMod$: IF e THEN 260 ELSE IF b THEN 256
258 EntryN 43, 59, 10, 2, e, b, EmpQod$: IF e THEN 260 ELSE IF b THEN 257
259 EntryN 43, 70, 10, 2, e, b, EmpYod$: IF e THEN 260 ELSE IF b THEN 258

	LOCATE 45, 48: PRINT USING "#######.##"; VAL(EmpMft$) + VAL(EmpMfi$) + VAL(EmpMmd$) + VAL(EmpMin$) + VAL(EmpMod$);
	LOCATE 45, 59: PRINT USING "#######.##"; VAL(EmpQft$) + VAL(EmpQfi$) + VAL(EmpQmd$) + VAL(EmpQin$) + VAL(EmpQod$);
	LOCATE 45, 70: PRINT USING "#######.##"; VAL(EmpYft$) + VAL(EmpYfi$) + VAL(EmpYmd$) + VAL(EmpYin$) + VAL(EmpYod$);

	LOCATE 47, 48: PRINT USING "#######.##"; VAL(EmpMgr$) - VAL(EmpMft$) - VAL(EmpMmd$) - VAL(EmpMfi$) - VAL(EmpMin$) - VAL(EmpMod$);
	LOCATE 47, 59: PRINT USING "#######.##"; VAL(EmpQgr$) - VAL(EmpQft$) - VAL(EmpQmd$) - VAL(EmpQfi$) - VAL(EmpQin$) - VAL(EmpQod$);
	LOCATE 47, 70: PRINT USING "#######.##"; VAL(EmpYgr$) - VAL(EmpYft$) - VAL(EmpYmd$) - VAL(EmpYfi$) - VAL(EmpYin$) - VAL(EmpYod$);

	Picture 47, 48, 10, EmpMnt$
	Picture 47, 59, 10, EmpQnt$
	Picture 47, 70, 10, EmpYnt$

	Emp.Mrh = VAL(EmpMrh$) * 100: Emp.Qrh = VAL(EmpQrh$) * 100: Emp.Yrh = VAL(EmpYrh$) * 100
	Emp.Moh = VAL(EmpMoh$) * 100: Emp.Qoh = VAL(EmpQoh$) * 100: Emp.Yoh = VAL(EmpYoh$) * 100
	Emp.Mrp = VAL(EmpMrp$) * 100: Emp.Qrp = VAL(EmpQrp$) * 100: Emp.Yrp = VAL(EmpYrp$) * 100
	Emp.Mop = VAL(EmpMop$) * 100: Emp.Qop = VAL(EmpQop$) * 100: Emp.Yop = VAL(EmpYop$) * 100
	Emp.Mbo = VAL(EmpMbo$) * 100: Emp.Qbo = VAL(EmpQbo$) * 100: Emp.Ybo = VAL(EmpYbo$) * 100
	Emp.Mgr = VAL(EmpMgr$) * 100: Emp.Qgr = VAL(EmpQgr$) * 100: Emp.Ygr = VAL(EmpYgr$) * 100
	Emp.Mft = VAL(EmpMft$) * 100: Emp.Qft = VAL(EmpQft$) * 100: Emp.Yft = VAL(EmpYft$) * 100
	Emp.Mfi = VAL(EmpMfi$) * 100: Emp.Qfi = VAL(EmpQfi$) * 100: Emp.Yfi = VAL(EmpYfi$) * 100
	Emp.Mmd = VAL(EmpMmd$) * 100: Emp.Qmd = VAL(EmpQmd$) * 100: Emp.Ymd = VAL(EmpYmd$) * 100
	Emp.Min = VAL(EmpMin$) * 100: Emp.Qin = VAL(EmpQin$) * 100: Emp.Yin = VAL(EmpYin$) * 100
	Emp.Mod = VAL(EmpMod$) * 100: Emp.Qod = VAL(EmpQod$) * 100: Emp.Yod = VAL(EmpYod$) * 100
	Emp.Mnt = VAL(EmpMnt$) * 100: Emp.Qnt = VAL(EmpQnt$) * 100: Emp.Ynt = VAL(EmpYnt$) * 100

	IF Confirm("SAVE Employee") = 0 THEN 259

	Emx.Dpt = Emp.Dpt
	Emx.Nam = Emp.Nam
	Emp.Num = Emx.Num
	PUT #2, VAL(Emx.Num), Emp
	PUT #3, n, Emx
	EmpSort
260
END SUB

DEFLNG L
SUB EmpLine (r, L0, L1, L2)
	LOCATE r, 48: PRINT USING "#######.##"; L0 / 100;
	LOCATE r, 59: PRINT USING "#######.##"; L1 / 100;
	LOCATE r, 70: PRINT USING "#######.##"; L2 / 100;
END SUB

DEFINT L
SUB EmpShow ()
	GET #2, VAL(Emx.Num), Emp
	COLOR 27, 0
	qPrint Emp.Num, 3, 53, 0, 1
	qPrint Emp.Dpt, 4, 53, 0, 1
	qPrint Emp.Nam, 6, 53, 0, 1
	qPrint Emp.Add, 7, 53, 0, 1
	qPrint Emp.Cty, 8, 53, 0, 1
	qPrint Emp.Sta, 8, 69, 0, 1
	qPrint Emp.Zip, 8, 72, 0, 1

	qPrint Emp.Typ, 11, 53, 0, 1
	qPrint Emp.Mar, 12, 53, 0, 1
	qPrint Emp.Dep, 13, 53, 0, 1
	qPrint Emp.Tax, 14, 53, 0, 1

	qPrint Emp.SSn, 17, 62, 0, 1
	qPrint Emp.Tel, 18, 62, 0, 1
	qPrint Emp.Hir, 19, 62, 0, 1
	qPrint Emp.Rel, 20, 62, 0, 1

	qPrint Emp.Pay, 23, 63, 0, 1
	qPrint Emp.Ins, 24, 63, 0, 1
	qPrint Emp.Ded, 25, 63, 0, 1
	qPrint Emp.Bon, 26, 63, 0, 1

	EmpLine 31, Emp.Mrh, Emp.Qrh, Emp.Yrh
	EmpLine 32, Emp.Moh, Emp.Qoh, Emp.Yoh
	EmpLine 33, Emp.Mrp, Emp.Qrp, Emp.Yrp
	EmpLine 34, Emp.Mop, Emp.Qop, Emp.Yop
	EmpLine 35, Emp.Mbo, Emp.Qbo, Emp.Ybo

	EmpLine 37, Emp.Mgr, Emp.Qgr, Emp.Ygr

	EmpLine 39, Emp.Mft, Emp.Qft, Emp.Yft
	EmpLine 40, Emp.Mfi, Emp.Qfi, Emp.Yfi
	EmpLine 41, Emp.Mmd, Emp.Qmd, Emp.Ymd
	EmpLine 42, Emp.Min, Emp.Qin, Emp.Yin
	EmpLine 43, Emp.Mod, Emp.Qod, Emp.Yod

	EmpLine 45, Emp.Mft + Emp.Mfi + Emp.Mmd + Emp.Min + Emp.Mod, Emp.Qft + Emp.Qfi + Emp.Qmd + Emp.Qin + Emp.Qod, Emp.Yft + Emp.Yfi + Emp.Ymd + Emp.Yin + Emp.Yod

	EmpLine 47, Emp.Mnt, Emp.Qnt, Emp.Ynt
END SUB

DEFSNG A-Z
SUB EmpSort ()
	CLOSE
	SHELL "AC.COM ..\Data\Employee.NAM ..\Data\Names /L28 /+1,25 /Q"
	KILL "..\Data\Employee.NAM"
	NAME "..\Data\Names" AS "..\Data\Employee.NAM"
	PR_Files
	ErrSw(3) = 1
END SUB

DEFINT A-Z
SUB EmpWind (f$)
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "PR-EMPList", e)
	t = LOF(3) \ LEN(Emx)
	r = 9: b = 1: GOSUB 281
280 GET #3, n, Emx
	COLOR 15, 4: LOCATE r, 40: PRINT USING "&³&"; Emx.Dpt + Emx.Num; Emx.Nam;
	KeyPres n$
	COLOR 16, 7: LOCATE r, 40: PRINT USING "&³&"; Emx.Dpt + Emx.Num; Emx.Nam;
	SELECT CASE n$
	CASE CHR$(13)
		IF Emx.Nam > "!" THEN
		   GET #2, VAL(Emx.Num), Emp
		   f$ = Emx.Num
		   PCOPY 1, 0
		   EXIT SUB
		END IF
	CASE CHR$(27)
		f$ = ""
		PCOPY 1, 0
		EXIT SUB
	CASE CHR$(0) + "M", CHR$(0) + "K" '===========left=right==
		PCOPY 0, 2: CALL MhDisplayByName(0, 0, "pr-EmpData", e)
		EmpShow
		KeyPres f$
		PCOPY 2, 0
		CASE CHR$(0) + "P"  '===============Down===
		IF n + 1 > t GOTO 280 ELSE n = n + 1
		IF r + 1 > 23 THEN CALL scroll(40, 9, 68, 23, 1) ELSE r = r + 1
	CASE CHR$(0) + "H"  '===============Up=====
		IF n - 1 < 1 GOTO 280 ELSE n = n - 1
		IF r - 1 < 9 THEN CALL BkScroll(40, 9, 68, 23, 1) ELSE r = r - 1
	CASE CHR$(0) + "I"  '===============PgUp===
		b = n - r - 6: IF b < 1 THEN b = 1
		GOSUB 281
	CASE CHR$(0) + "Q"  '===============PgDn===
		b = n - r + 24: IF b > t - 14 THEN b = t - 14
		IF b < 1 THEN b = 1
		GOSUB 281
	CASE CHR$(0) + "G"  '===============Home===
		b = 1: GOSUB 281
	CASE CHR$(0) + "O"  '===============End====
		b = t - 14: IF b < 1 THEN b = 1
		GOSUB 281
	CASE "M"
		PCOPY 0, 2: CALL MhDisplayByName(0, 0, "pr-EmpData", e)
		EmpShow
		EmpEdit n
		PCOPY 2, 0
		GOSUB 281
	CASE "N"
		PCOPY 0, 2: CALL MhDisplayByName(0, 0, "pr-EmpData", e)
		GET #3, t, Emx
		IF Emx.Dpt = "þ" THEN n = t ELSE n = t + 1: Emx.Num = RIGHT$(STR$(1000 + n), 3)
		EmpEdit n
		t = LOF(3) / LEN(Emx)
		PCOPY 2, 0
		GOSUB 281
	END SELECT
	GOTO 280

281 COLOR 16, 7
	LOCATE 9
	FOR x = b TO t
		GET #3, x, Emx
		LOCATE , 40: PRINT USING "&³&"; Emx.Dpt + Emx.Num; Emx.Nam
		IF CSRLIN > 23 THEN EXIT FOR
	NEXT
	n = b + r - 9
	RETURN
END SUB

DEFSNG A-Z
SUB HMaster ()
	Heading
	PRINT #11, TAB(65); "EMPLOYEE PERTINENT DATA"
	PRINT #11,
	PRINT #11, "================================================================================================================================================================"
END SUB

SUB HMonth ()
	Heading
	PRINT #11, TAB(65); "MONTHLY PAYROLL REGISTER"
	PRINT #11, TAB(65); "Month Ending: "; DATE$
	PRINT #11,
	PRINT #11,
	PRINT #11, "                                 REGULAR   OVERTIME   REGULAR    OVERTIME               GROSS                           MEDICARE  INSURANCE    OTHER       NET    "
	PRINT #11, "Code      EMPLOYEE NAME           HOURS      HOURS      PAY        PAY       BONUS       PAY        FIT        FICA    DEDUCTIONS DEDUCTIONS DEDUCTIONS    PAY    "
	PRINT #11, "==== ========================   ========= ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

SUB HprChk ()
	Heading
	PRINT #11, TAB(70); "PAYROLL  CHECKS"
	PRINT #11, TAB(67); "From "; fDat; " To "; tDat
	PRINT #11,
	PRINT #11,
	PRINT #11, "                         Regular  Overtime  Regular  Overtime    Bonus     Gross                       Medicare  Insurance   Other              Check     Check   "
	PRINT #11, "     Employee Name        Hours     Hours     Pay       Pay       Pay       Pay       FIT      FICA    Dedection Deduction Deduction  Net Pay   Number     Date   "
	PRINT #11, "======================== ======== ======== ========= ========= ========= ========= ========= ========= ========= ========= ========= ========= ======== =========="
END SUB

SUB HprReg ()
	Heading
	PRINT #11, TAB(69); "PAYROLL REGISTER"
	PRINT #11,
	PRINT #11,
	PRINT #11, "Emp                             Regular  Overtime   Regular  Overtime                                          Medicare  Insurance   Other               Check   "
	PRINT #11, "Code      Employee Name          Hours     Hours      Pay      Pay     Bonus Pay Gross Pay    FIT      FICA    Deduction Deduction Deduction  Net Pay    Number  "
	PRINT #11, "==== ========================  ========= ========= ========= ========= ========= ========= ========= ========= ========= ========= ========= ========= =========="
END SUB

SUB HQuarter ()
	Heading
	PRINT #11, TAB(64); "QUARTERLY PAYROLL REGISTER"
	PRINT #11, TAB(64); "Quarter Ending: "; DATE$
	PRINT #11,
	PRINT #11,
	PRINT #11, "                                 REGULAR   OVERTIME   REGULAR    OVERTIME               GROSS                           MEDICARE  INSURANCE    OTHER       NET    "
	PRINT #11, "Code      EMPLOYEE NAME           HOURS      HOURS      PAY        PAY       BONUS       PAY        FIT        FICA    DEDUCTIONS DEDUCTIONS DEDUCTIONS    PAY    "
	PRINT #11, "==== ========================   ========= ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

SUB HTimeCard ()
	Heading
	PRINT #11, TAB(72); "TIMECARDS"
	PRINT #11,
	PRINT #11,
	PRINT #11, " EMP                               SOCIAL                  ------------------------------------    ------------------------------------    ----------"
	PRINT #11, " NUM        EMPLOYEE NAME         SECURITY       DAY       CLOCK-IN   LUNCH   CLOCK-OUT   HOURS    CLOCK-IN   LUNCH   CLOCK-OUT   HOURS      TOTALS  "
	PRINT #11, "-----  ------------------------  -----------  ---------    ------------------------------------    ------------------------------------    ----------"
END SUB

SUB HYear ()
	Heading
	PRINT #11, TAB(65); "YEAR-END PAYROLL REGISTER"
	PRINT #11, TAB(65); " Year Ending: "; DATE$
	PRINT #11,
	PRINT #11,
	PRINT #11, "                                 REGULAR   OVERTIME   REGULAR    OVERTIME               GROSS                           MEDICARE  INSURANCE    OTHER       NET    "
	PRINT #11, "Code      EMPLOYEE NAME           HOURS      HOURS      PAY        PAY       BONUS       PAY        FIT        FICA    DEDUCTIONS DEDUCTIONS DEDUCTIONS    PAY    "
	PRINT #11, "==== ========================   ========= ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

DEFLNG A-Z
SUB IncomeTax (Grs, FIT)
	IF Payroll = "B" THEN t = 10 ELSE t = 5
	Income = Grs - (VAL(Emp.Dep) * 1530 * t)
	SELECT CASE Payroll
	CASE "W"       '=============================WEEKLY==============
		 SELECT CASE Emp.Mar
		 CASE "S"  '---------SINGLE TAX RATES------------------------
			  SELECT CASE Income
			  CASE IS > 617700:  FIT = 178225 + (.35 * (Income - 617700))
			  CASE IS > 286000:  FIT = 68764 + (.33 * (Income - 286000))
			  CASE IS > 131700:  FIT = 25560 + (.28 * (Income - 131700))
			  CASE IS > 59200:   FIT = 7435 + (.25 * (Income - 59200))
			  CASE IS > 18700:   FIT = 1360 + (.15 * (Income - 18700))
			  CASE IS > 5100:    FIT = .1 * (Income - 5100)
			  CASE ELSE:         FIT = 0
			  END SELECT
		 CASE "M"  '---------MARRIED TAX RATES-----------------------
			  SELECT CASE Income
			  CASE IS > 627100:  FIT = 166158 + (.35 * (Income - 627100))
			  CASE IS > 356800:  FIT = 76959 + (.33 * (Income - 356800))
			  CASE IS > 227000:  FIT = 40615 + (.28 * (Income - 227000))
			  CASE IS > 124500:  FIT = 14900 + (.25 * (Income - 124500))
			  CASE IS > 42900:   FIT = 2750 + (.15 * (Income - 42900))
			  CASE IS > 15400:   FIT = .1 * (Income - 15400)
			  CASE ELSE:         FIT = 0
			  END SELECT
		 END SELECT
	CASE "B"       '=============================BIWEEKLY============
		 SELECT CASE Emp.Mar
		 CASE "S"  '---------SINGLE TAX RATES------------------------
			  SELECT CASE Income
			  CASE IS > 1235400: FIT = 356447 + (.35 * (Income - 1235400))
			  CASE IS > 571900:  FIT = 137492 + (.33 * (Income - 571900))
			  CASE IS > 263500:  FIT = 51140 + (.28 * (Income - 263500))
			  CASE IS > 118500:  FIT = 14890 + (.25 * (Income - 118500))
			  CASE IS > 37300:   FIT = 2710 + (.15 * (Income - 37300))
			  CASE IS > 10200:   FIT = .1 * (Income - 10200)
			  CASE ELSE:         FIT = 0
			  END SELECT
		 CASE "M"  '---------MARRIED TAX RATES-----------------------
			  SELECT CASE Income
			  CASE IS > 1254200: FIT = 332311 + (.35 * (Income - 1254200))
			  CASE IS > 713700:  FIT = 153946 + (.33 * (Income - 713700))
			  CASE IS > 454000:  FIT = 81230 + (.28 * (Income - 454000))
			  CASE IS > 249000:  FIT = 29980 + (.25 * (Income - 249000))
			  CASE IS > 85800:   FIT = 5500 + (.15 * (Income - 85800))
			  CASE IS > 30800:   FIT = .1 * (Income - 30800)
			  CASE ELSE:         FIT = 0
			  END SELECT
		 END SELECT
	END SELECT
END SUB

SUB Medicare (Grs, CARE)
	CAREtax! = .0145
	CARE = CAREtax! * Grs
END SUB

DEFINT A-Z
SUB mprCards ()
200 IF Payroll = "W" THEN
	   t = 0: CALL MhDisplayByName(0, 0, "PR-Card1W", e)
	ELSE
	   t = 1: CALL MhDisplayByName(0, 0, "PR-Card2W", e)
	END IF
	StatLin "Employee Timecard"
201 EntryN 8, 19, 3, 0, e, b, Emx.Num: IF e THEN EXIT SUB ELSE IF b THEN 200
	IF Emx.Num < "  1" THEN EmpWind f$: IF f$ = "" THEN 201
	GET #2, VAL(Emx.Num), Emp: IF Emp.Nam < "!" THEN EmpWind f$: IF f$ = "" THEN 201
	qPrint Emp.Num, 8, 19, 0, 1
	qPrint Emp.Nam, 9, 10, 0, 1
	qPrint Emp.Add, 10, 10, 0, 1
	qPrint Emp.Cty, 11, 10, 0, 1
	qPrint Emp.Sta, 11, 25, 0, 1
	qPrint Emp.Zip, 11, 28, 0, 1
	IF Emp.Typ = "S" THEN WinMssg "Salaried Employee!": GOTO 201
	IF Emp.Typ = "H" THEN WinMssg "Employee is On-Hold!": GOTO 201
	FOR w = 0 TO t
	FOR d = 0 TO 6
	FOR h = 0 TO 3
		r = 21 + 2 * d
		c = 11 + 5 * h + 34 * w
		qPrint Emp.Clk(w, d, h), r, c, 0, 1
	NEXT
		c = 31 + 34 * w
		qPrint Emp.Hrs(w, d), r, c, 0, 1
	NEXT
		qPrint Emp.Hrs(w, 7), r + 2, c, 0, 1
		qPrint Emp.Hrs(w, 8), r + 4, c, 0, 1
		qPrint Emp.Hrs(w, 9), r + 6, c, 0, 1
		Hrs(w, 0) = 100 * VAL(Emp.Hrs(w, 7))
		Hrs(w, 1) = 100 * VAL(Emp.Hrs(w, 8))
		Hrs(w, 2) = 100 * VAL(Emp.Hrs(w, 9))
	NEXT
	IF t THEN
		qPrint Emp.Tot(0), 9, 65, 0, 1
		qPrint Emp.Tot(1), 10, 65, 0, 1
		qPrint Emp.Tot(2), 11, 65, 0, 1
		Tot(0) = 100 * VAL(Emp.Tot(0))
		Tot(1) = 100 * VAL(Emp.Tot(1))
		Tot(2) = 100 * VAL(Emp.Tot(2))
	END IF

	w = 0
	d = 0
	h = 0
202 r = 21 + 2 * d
	c = 11 + 5 * h + 34 * w
	EntryN r, c, 4, 0, e, b, Emp.Clk(w, d, h): IF e THEN 201
	IF b THEN
	   h = h - 1: IF h < 0 THEN h = 3: d = d - 1: IF d < 0 THEN d = 6: w = w - 1: IF w < 0 THEN w = t
	   GOTO 202
	END IF

	IF MID$(Emp.Clk(w, d, h), 1, 2) > "12" THEN BEEP: GOTO 202
	IF MID$(Emp.Clk(w, d, h), 3, 2) > "59" THEN BEEP: GOTO 202
	Dec(w, d, h) = 100 * VAL(MID$(Emp.Clk(w, d, h), 1, 2)) + VAL(MID$(Emp.Clk(w, d, h), 3, 2)) / .6

	SELECT CASE h
	CASE 1
		 IF Emp.Clk(w, d, 1) > "   1" AND Emp.Clk(w, d, 0) < "   1" THEN BEEP: GOTO 202
	CASE 2
		 IF Emp.Clk(w, d, 2) > "   1" AND Emp.Clk(w, d, 1) < "   1" THEN BEEP: GOTO 202
	CASE 3
		 IF Emp.Clk(w, d, 3) > "   1" AND Emp.Clk(w, d, 0) < "   1" THEN BEEP: GOTO 202
		 IF Emp.Clk(w, d, 3) < "   1" AND Emp.Clk(w, d, 2) > "   1" THEN BEEP: GOTO 202
		 Hrs(w, 0) = Hrs(w, 0) - 100 * VAL(Emp.Hrs(w, d))
		 IF Dec(w, d, 1) < Dec(w, d, 0) THEN Bef = 1200 + Dec(w, d, 1) - Dec(w, d, 0) ELSE Bef = Dec(w, d, 1) - Dec(w, d, 0)
		 IF Dec(w, d, 3) < Dec(w, d, 2) THEN Aft = 1200 + Dec(w, d, 3) - Dec(w, d, 2) ELSE Aft = Dec(w, d, 3) - Dec(w, d, 2)
		 COLOR 16, 3: LOCATE r, c + 5, 0: PRINT USING "###.##"; (Bef + Aft) / 100;
		 Picture r, c + 5, 6, Emp.Hrs(w, d)
		 Hrs(w, 0) = Hrs(w, 0) + 100 * VAL(Emp.Hrs(w, d))
		 IF Hrs(w, 0) > 4000 THEN
			Hrs(w, 1) = 4000
			Hrs(w, 2) = Hrs(w, 0) - 4000
		 ELSE
			Hrs(w, 1) = Hrs(w, 0)
			Hrs(w, 2) = 0
		 END IF
		 Tot(0) = Hrs(0, 0) + Hrs(1, 0)
		 Tot(1) = Hrs(0, 1) + Hrs(1, 1)
		 Tot(2) = Hrs(0, 2) + Hrs(1, 2)
		 COLOR 4, 7
		 LOCATE 35, c + 5: PRINT USING "###.##"; Hrs(w, 0) / 100;
		 LOCATE 37, c + 5: PRINT USING "###.##"; Hrs(w, 1) / 100;
		 LOCATE 39, c + 5: PRINT USING "###.##"; Hrs(w, 2) / 100;
		 Picture 35, c + 5, 6, Emp.Hrs(w, 7)
		 Picture 37, c + 5, 6, Emp.Hrs(w, 8)
		 Picture 39, c + 5, 6, Emp.Hrs(w, 9)
		 IF t THEN
			LOCATE 9, 65:  PRINT USING "###.##"; Tot(0) / 100;
			LOCATE 10, 65: PRINT USING "###.##"; Tot(1) / 100;
			LOCATE 11, 65: PRINT USING "###.##"; Tot(2) / 100;
		 END IF
	END SELECT
	h = h + 1
	IF h > 3 THEN
	   h = 0
	   d = d + 1
	   IF d > 6 THEN
		  d = 0
		  w = w + 1
		  IF w > t THEN
			 IF Confirm("SAVE TimeCard") THEN
				Picture 9, 65, 6, Emp.Tot(0)
				Picture 10, 65, 6, Emp.Tot(1)
				Picture 11, 65, 6, Emp.Tot(2)
				PUT #2, VAL(Emx.Num), Emp
				ErrSw(1) = 1
				ErrSw(4) = 1
				GOTO 200
			 ELSE
				w = t
				d = 6
				h = 3
			 END IF
		  END IF
	   END IF
	END IF
	GOTO 202
END SUB

DEFLNG L
SUB mprChecks ()
	StatLin "Print Payroll Checks"
	IF ErrSw(1) THEN WinMssg "Must print TimeSheets first": EXIT SUB
	IF ErrSw(2) THEN WinMssg "Errors found in Payroll": EXIT SUB
	IF ErrSw(3) THEN WinMssg "Master changed, reprint register": EXIT SUB
	IF ErrSw(4) THEN WinMssg "Timecard changed, reprint register": EXIT SUB
	IF ErrSw(5) THEN WinMssg "Print payroll register": EXIT SUB
	CLOSE #6: OPEN "..\Data\Payroll.REG" FOR INPUT AS #6
	IF EOF(6) THEN WinMssg "No payroll checks to print": EXIT SUB
	IF Response("PR-print") THEN EXIT SUB
	PRINT #11, CHR$(27); "@";
	WIDTH "LPT1:", 255
	WHILE NOT EOF(6)
		INPUT #6, Emc.Chk, Emc.Dat, Emc.Emp, Emc.Lrh, Emc.Loh, Emc.Lrp, Emc.Lop, Emc.Lbo, Emc.Lgr, Emc.Lft, Emc.Lfi, Emc.Lmd, Emc.Lin, Emc.Lod, Emc.Lnt
		GET #2, VAL(Emc.Emp), Emp

		n = 1 + LOF(4) / LEN(Emc)
		Ecx.Emp = VAL(Emc.Emp)
		Ecx.Dat = VAL(MID$(Emc.Dat, 9, 2) + MID$(Emc.Dat, 1, 2) + MID$(Emc.Dat, 4, 2))
		Ecx.Num = n
		PUT #4, n, Emc
		PUT #5, n, Ecx

		ERASE Emp.Clk, Emp.Hrs, Emp.Tot
		ChkPost
		ChkPrnt
	WEND
	PRINT #11, CHR$(18)

	CLOSE
	LOCATE 1
	KILL "..\Data\Payroll.REG"
	SHELL "AC.COM ..\Data\Employee.CKX ..\Data\Emp /L10 /+2,1 /+1,1 /+6,1 /+5,1 /+4,1 /+3,1 /Q"
	KILL "..\Data\Employee.CKX"
	NAME "..\Data\Emp" AS "..\Data\Employee.CKX"
	ErrSw(5) = 1
END SUB

SUB mprChkLst ()
	StatLin "Payroll between 2 Dates"
	CALL MhDisplayByName(0, 0, "PR-List", e)
	n$ = "ALL"
	fDat = "01-01-" + MID$(DATE$, 7, 4)
	tDat = "12-31-" + MID$(DATE$, 7, 4)
204 EntryU 14, 38, 3, e, b, n$: IF e THEN EXIT SUB ELSE IF b THEN 204
	IF n$ = "ALL" THEN
	   a = 1
	ELSE
	   a = 0
	   IF n$ < "001" THEN EmpWind n$: IF n$ = "" THEN 204
	   GET #2, VAL(n$), Emp: IF Emp.Nam < "!" THEN EmpWind n$: IF n$ = "" THEN 204
	   qPrint n$, 14, 38, 0, 1
	END IF
205 EntryD 16, 38, e, b, fDat:  IF e THEN EXIT SUB ELSE IF b THEN 204
	IF tDat < "0" THEN WinMssg "From date please": GOTO 205
206 EntryD 17, 38, e, b, tDat:  IF e THEN EXIT SUB ELSE IF b THEN 205
	IF tDat < "0" THEN WinMssg "To date please": GOTO 206
	n = VAL(n$)
	lfD = VAL(MID$(fDat, 9, 2) + MID$(fDat, 1, 2) + MID$(fDat, 4, 2))
	ltD = VAL(MID$(tDat, 9, 2) + MID$(tDat, 1, 2) + MID$(tDat, 4, 2))
	PageNum = 0: HprChk
	t = LOF(5) \ LEN(Ecx)
	FOR x = 1 TO t
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		Printing x, t
		GET #5, x, Ecx
		IF a THEN n = Ecx.Emp
		IF n = Ecx.Emp THEN
		   IF Ecx.Dat >= lfD AND Ecx.Dat <= ltD THEN
			  GET #4, Ecx.Num, Emc
			  GET #2, VAL(Emc.Emp), Emp
			  LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HprChk
			  PRINT #11, USING "&"; Emp.Nam;
			  PRINT #11, USING " #####.##"; Emc.Lrh / 100; Emc.Loh / 100;
			  PRINT #11, USING " ######.##"; Emc.Lrp / 100; Emc.Lop / 100;
			  PRINT #11, USING " ######.##"; Emc.Lbo / 100; Emc.Lgr / 100;
			  PRINT #11, USING " ######.##"; Emc.Lft / 100; Emc.Lfi / 100;
			  PRINT #11, USING " ######.##"; Emc.Lmd / 100; Emc.Lin / 100;
			  PRINT #11, USING " ######.##"; Emc.Lod / 100; Emc.Lnt / 100;
			  PRINT #11, USING " &"; Emc.Chk; Emc.Dat
			  L0 = L0 + Emc.Lrh: L1 = L1 + Emc.Loh
			  L2 = L2 + Emc.Lrp: L3 = L3 + Emc.Lop
			  L4 = L4 + Emc.Lbo: L5 = L5 + Emc.Lgr
			  L6 = L6 + Emc.Lft: L7 = L7 + Emc.Lfi
			  L8 = L8 + Emc.Lmd: L9 = L9 + Emc.Lin
			  Lx = Lx + Emc.Lod: Ly = Ly + Emc.Lnt
		   END IF
		END IF
	NEXT
	PRINT #11,
	PRINT #11, TAB(25);
	PRINT #11, USING "######.##"; L0 / 100; L1 / 100;
	PRINT #11, USING "#######.##"; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100;
	PRINT #11, USING "#######.##"; L7 / 100; L8 / 100; L9 / 100; Lx / 100; Ly / 100
	PRINT #11, CHR$(12)
END SUB

SUB mprChkMan ()
	StatLin "Manual Payroll Check"
261 CALL MhDisplayByName(0, 0, "PR-Check", e)
	COLOR 16, 7
	qPrint Company, 26, 6, 0, 1
	qPrint Street, 27, 6, 0, 1
	qPrint Address, 28, 6, 0, 1
	qPrint City, 29, 6, 0, 1
	dt$ = DATE$
262 EntryN 9, 16, 3, 0, e, b, Emx.Num: IF e THEN EXIT SUB ELSE IF b THEN 262
	IF Emx.Num < "  1" THEN EmpWind f$: IF f$ = "" THEN 262
	GET #2, VAL(Emx.Num), Emp

	qPrint Emp.Num, 9, 16, 0, 1
	qPrint Emp.Nam, 11, 6, 0, 1
	qPrint Emp.SSn, 11, 34, 0, 1

	qPrint Emp.Pay, 17, 25, 0, 1: rp$ = Emp.Pay
	qPrint Emp.Bon, 17, 45, 0, 1: bo$ = Emp.Bon
	qPrint Emp.Ins, 22, 35, 0, 1: in$ = Emp.Ins
	qPrint Emp.Ded, 22, 45, 0, 1: od$ = Emp.Ded

	qPrint Emp.Nam, 36, 17, 0, 1
	qPrint Emp.Add, 37, 17, 0, 1
	qPrint RTRIM$(Emp.Cty) + ", " + Emp.Sta + " " + Emp.Zip, 38, 17, 0, 1

263 EntryU 11, 46, 8, e, b, ck$:       IF e THEN 261 ELSE IF b THEN 263
	IF ck$ < "0" THEN WinMssg "Check number please": GOTO 263
	ck$ = RIGHT$(STR$(100000000 + VAL(ck$)), 8)
	qPrint ck$, 11, 46, 0, 1
	qPrint ck$, 26, 57, 0, 1
264 EntryD 11, 55, e, b, dt$:          IF e THEN 261 ELSE IF b THEN 263
	IF dt$ < "0" THEN WinMssg "Date please": GOTO 264
	qPrint dt$, 32, 40, 0, 1
265 IF Emp.Typ = "W" THEN
	   EntryN 17, 8, 6, 2, e, b, rh$:  IF e THEN 261 ELSE IF b THEN 264
	   IF VAL(rh$) * VAL(Emp.Pay) > 99999.99 THEN WinMssg "Regular pay exceeds $99,999.99": GOTO 265
	   LOCATE 17, 25: PRINT USING "#####.##"; VAL(rh$) * VAL(Emp.Pay);
	   Picture 17, 25, 8, rp$
	END IF
266 IF Emp.Typ = "W" THEN
	   EntryN 17, 17, 6, 2, e, b, oh$: IF e THEN 261 ELSE IF b THEN 265
	   IF VAL(oh$) * VAL(Emp.Pay) * 1.5 > 99999.99 THEN WinMssg "Regular pay exceeds $99,999.99": GOTO 266
	   LOCATE 17, 35: PRINT USING "#####.##"; VAL(oh$) * VAL(Emp.Pay) * 1.5;
	   Picture 17, 35, 8, op$
	END IF
267 EntryN 17, 25, 8, 2, e, b, rp$:    IF e THEN 261 ELSE IF b THEN IF Emp.Typ = "W" THEN 266 ELSE 264
268 EntryN 17, 35, 8, 2, e, b, op$:    IF e THEN 261 ELSE IF b THEN 267
269 EntryN 17, 45, 8, 2, e, b, bo$:    IF e THEN 261 ELSE IF b THEN 268
	IF VAL(rp$) + VAL(op$) + VAL(bo$) > 99999.99 THEN WinMssg "Gross exceed $99,999.99": GOTO 269
	IF VAL(rp$) + VAL(op$) + VAL(bo$) < 0 THEN WinMssg "Gross is negative": GOTO 269
	LOCATE 17, 56: PRINT USING "#####.##"; VAL(rp$) + VAL(op$) + VAL(bo$);
	Picture 17, 56, 8, gr$
	IncomeTax 100 * VAL(gr$), Lft
	SocSecTax 100 * VAL(gr$), Lfi
	Medicare 100 * VAL(gr$), Lmd
	LOCATE 22, 6:  PRINT USING "#####.##"; Lfi / 100; : Picture 22, 6, 8, fi$
	LOCATE 22, 15: PRINT USING "#####.##"; Lft / 100; : Picture 22, 15, 8, ft$
	LOCATE 22, 25: PRINT USING "#####.##"; Lmd / 100; : Picture 22, 25, 8, md$
270 EntryN 22, 6, 8, 2, e, b, fi$:     IF e THEN 261 ELSE IF b THEN 269
271 EntryN 22, 15, 8, 2, e, b, ft$:    IF e THEN 261 ELSE IF b THEN 270
272 EntryN 22, 25, 8, 2, e, b, md$:    IF e THEN 261 ELSE IF b THEN 271
273 EntryN 22, 35, 8, 2, e, b, in$:    IF e THEN 261 ELSE IF b THEN 272
274 EntryN 22, 45, 8, 2, e, b, od$:    IF e THEN 261 ELSE IF b THEN 273
	IF VAL(gr$) - VAL(fi$) - VAL(ft$) - VAL(md$) - VAL(in$) - VAL(od$) < .01 THEN WinMssg "Net is negative": GOTO 274
	LOCATE 22, 56: PRINT USING "#####.##"; VAL(gr$) - VAL(fi$) - VAL(ft$) - VAL(md$) - VAL(in$) - VAL(od$);
	Picture 22, 56, 8, nt$
	NumWords MID$(nt$, 1, 5) + MID$(nt$, 7, 2), w$
	qPrint "$" + nt$, 32, 55, 0, 1
	qPrint w$ + STRING$(60 - LEN(w$), " "), 34, 6, 0, 1

	IF Confirm("SAVE Check") = 0 THEN 274
	n = 1 + LOF(4) / LEN(Emc)
	Emc.Emp = Emp.Num
	Emc.Dat = dt$
	Emc.Chk = ck$
	Emc.Lrh = 100 * VAL(rh$)
	Emc.Loh = 100 * VAL(oh$)
	Emc.Lrp = 100 * VAL(rp$)
	Emc.Lop = 100 * VAL(op$)
	Emc.Lbo = 100 * VAL(bo$)
	Emc.Lgr = 100 * VAL(gr$)
	Emc.Lft = 100 * VAL(ft$)
	Emc.Lfi = 100 * VAL(fi$)
	Emc.Lmd = 100 * VAL(md$)
	Emc.Lin = 100 * VAL(in$)
	Emc.Lod = 100 * VAL(od$)
	Emc.Lnt = 100 * VAL(nt$)
	Ecx.Emp = VAL(Emc.Emp)
	Ecx.Dat = VAL(MID$(dt$, 9, 2) + MID$(dt$, 1, 2) + MID$(dt$, 4, 2))
	Ecx.Num = n
	PUT #4, n, Emc
	PUT #5, n, Ecx

	ChkPost

	CLOSE
	LOCATE 1
	SHELL "AC.COM ..\Data\Employee.CKX ..\Data\Emp /L10 /+2,1 /+1,1 /+6,1 /+5,1 /+4,1 /+3,1 /Q"
	KILL "..\Data\Employee.CKX"
	NAME "..\Data\Emp" AS "..\Data\Employee.CKX"
	PR_Files
	IF Confirm("PRINT payroll Check") THEN ChkPrnt
	ck$ = RIGHT$(STR$(100000001 + VAL(ck$)), 8)
	Emx.Num = ""
	GOTO 261
END SUB

DEFINT L
SUB mprChkMas ()
	CALL MhDisplayByName(0, 0, "pr-ChkMast", e)
	StatLin "Payroll Checks Master"
	SubMenu 1, "D"
	r = 3: b = 1: t = LOF(4) \ LEN(Emc): GOSUB d2
	IF t = 0 THEN WinMssg "Sorry no payroll check": EXIT SUB
d1: GET #4, n, Emc: GET #2, VAL(Emc.Emp), Emp
	COLOR 11, 7: LOCATE r, 2: PRINT USING " & ³ & ³ & ³ & ³ #####.## "; Emc.Dat; Emp.Dpt + Emp.Num; Emp.Nam; Emc.Chk; Emc.Lnt / 100;
	KeyPres n$
	IF n$ = CHR$(13) THEN SubMenu 0, n$
	SELECT CASE n$
	CASE CHR$(27) 'Esc
		EXIT SUB
	CASE CHR$(0) + "M"  '===============right==
		SubMenu 0, n$
	CASE CHR$(0) + "K"  '===============left===
		SubMenu 0, n$
	CASE CHR$(0) + "P"  '===============Down===
		IF n + 1 > t GOTO d1 ELSE n = n + 1
		COLOR 27, 1: LOCATE r, 2: PRINT USING " & ³ & ³ & ³ & ³ #####.## "; Emc.Dat; Emp.Dpt + Emp.Num; Emp.Nam; Emc.Chk; Emc.Lnt / 100;
		IF r + 1 > 47 THEN CALL scroll(3, 3, 68, 47, 1) ELSE r = r + 1
	CASE CHR$(0) + "H"  '===============Up=====
		IF n - 1 < 1 GOTO d1 ELSE n = n - 1
		COLOR 27, 1: LOCATE r, 2: PRINT USING " & ³ & ³ & ³ & ³ #####.## "; Emc.Dat; Emp.Dpt + Emp.Num; Emp.Nam; Emc.Chk; Emc.Lnt / 100;
		IF r - 1 < 3 THEN CALL BkScroll(3, 3, 68, 47, 1) ELSE r = r - 1
	CASE CHR$(0) + "I"  '===============PgUp===
		b = n - r - 42: IF b < 1 THEN b = 1
		GOSUB d2
	CASE CHR$(0) + "Q"  '===============PgDn===
		b = n - r + 48: IF b > t - 44 THEN b = t - 44
		IF b < 1 THEN b = 1
		GOSUB d2
	CASE CHR$(0) + "G"  '===============Home===
		b = 1: GOSUB d2
	CASE CHR$(0) + "O"  '===============End====
		b = t - 44: IF b < 1 THEN b = 1
		GOSUB d2
	CASE "D"            '===============Display
		SubMenu 0, n$
		ChkShow
		KeyPres f$
		PCOPY 2, 0
	CASE "V"            '===============Void===
		SubMenu 0, n$
		ChkShow
		IF Confirm("VOID Check") THEN ChkVoid n
		PCOPY 2, 0
	CASE "R"            '===============Reprint
		SubMenu 0, n$
		ChkShow
d3:     EntryU 11, 46, 8, e, b, Emc.Chk: IF b GOTO d3
		IF e = 0 THEN
		   IF Emc.Chk < "0" THEN WinMssg "Check number please": GOTO d3
		   Emc.Chk = RIGHT$(STR$(100000000 + VAL(Emc.Chk)), 8)
		   qPrint Emc.Chk, 11, 46, 0, 1
		   qPrint Emc.Chk, 26, 57, 0, 1
		   IF Confirm("REPRINT Check") THEN ChkPrnt: PUT #4, n, Emc
		END IF
		PCOPY 2, 0
	CASE "E"            '===============Employee
		SubMenu 0, n$
		PCOPY 0, 1: CALL MhDisplayByName(0, 0, "pr-EmpData", e)
		Emx.Num = Emc.Emp
		EmpShow
		KeyPres f$
		PCOPY 1, 0
	END SELECT
	GOTO d1

d2: COLOR 27, 1: LOCATE 3
	FOR x = b TO t
	GET #4, x, Emc: GET #2, VAL(Emc.Emp), Emp
	LOCATE , 2: PRINT USING " & ³ & ³ & ³ & ³ #####.## "; Emc.Dat; Emp.Dpt + Emp.Num; Emp.Nam; Emc.Chk; Emc.Lnt / 100
	IF CSRLIN > 47 THEN EXIT FOR
	NEXT
	n = b + r - 3
	RETURN
END SUB

DEFLNG L
SUB mprChkReg ()
	StatLin "Print Payroll Status"
	IF ErrSw(1) THEN WinMssg "Must print TimeSheets first": EXIT SUB
	CALL MhDisplayByName(0, 0, "PR-CheckNo", e)
207 EntryN 15, 48, 8, 0, e, b, cNum$: IF e THEN EXIT SUB ELSE IF b THEN 207
	LChk = VAL(cNum$)
	IF LChk < 1 THEN WinMssg "Check number invalid": GOTO 207
	cDat$ = DATE$
	CLOSE #6: OPEN "..\Data\Payroll.REG" FOR OUTPUT AS #6
	t = LOF(3) / LEN(Emx)
	PageNum = 0: Heading
	ErrSw(2) = 0
	FOR x = 1 TO t
		Printing x, t
		IF INKEY$ = CHR$(27) THEN ErrSw(2) = 1: EXIT SUB
		GET #3, x, Emx
		GET #2, VAL(Emx.Num), Emp
		IF Emp.Nam > "!" THEN
		   PRINT #11, USING "& "; Emp.Dpt + Emp.Num; Emp.Nam; Emp.SSn;
		   IF Payroll = "B" THEN
			  Lrh = VAL(Emp.Tot(1)) * 100
			  Loh = VAL(Emp.Tot(2)) * 100
		   ELSE
			  Lrh = VAL(Emp.Hrs(0, 8)) * 100
			  Loh = VAL(Emp.Hrs(0, 9)) * 100
		   END IF
		   SELECT CASE Emp.Typ
		   CASE "S" '------------------------Salary-----
				IF Lrh > 0 THEN
				   ErrNum = 1: GOSUB 210
				ELSE
				   Lrp = VAL(Emp.Pay) * 100
				   Lop = 0
				   Lbo = VAL(Emp.Bon) * 100
				   Lin = VAL(Emp.Ins) * 100
				   Lod = VAL(Emp.Ded) * 100
				END IF
		   CASE "W" '------------------------Wages------
				IF Lrh > 0 THEN
				   Lrp = VAL(Emp.Pay) * Lrh
				   Lop = VAL(Emp.Pay) * Loh * 1.5
				   Lbo = VAL(Emp.Bon) * 100
				   Lin = VAL(Emp.Ins) * 100
				   Lod = VAL(Emp.Ded) * 100
				ELSE
				   PRINT #11, TAB(45); "Employee does not have a timecard."
				   LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HprReg
				   Lrp = 0
				   Lop = 0
				   Lbo = VAL(Emp.Bon) * 100
				   Lin = VAL(Emp.Ins) * 100
				   Lod = VAL(Emp.Ded) * 100
				END IF
		   CASE "H" '------------------------Hold-------
				PRINT #11, TAB(45); "Employee is on hold."
				LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HprReg
				Lrp = 0
				Lop = 0
				Lbo = 0
				Lin = 0
				Lod = 0
		   END SELECT
		   Lgr = Lrp + Lop + Lbo

		   IF Emp.Tax = "P" THEN
			  LOCATE 23, 20: PRINT " Tax rate xx.xx% for "; Emp.Nam; : f$ = "00.00"
208           EntryN 23, 30, 5, 2, e, b, f$: IF e OR b THEN 208
			  Lft = Lgr * VAL(f$) / 100
		   ELSE
			  IncomeTax Lgr, Lft
		   END IF
		   SocSecTax Lgr, Lfi
		   Medicare Lgr, Lmd

		   Lnt = Lgr - Lft - Lfi - Lmd - Lin - Lod
		   IF Lgr = 0 OR Lnt = 0 THEN
			  PRINT #11, TAB(45); "No payroll check, gross pay is zero"
			  LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HprReg
		   ELSE
			  SELECT CASE Lnt
					 CASE IS < 0
						  ErrNum = 2: GOSUB 210
						  cNum$ = "Error   "
					 CASE IS = 0, IS > 0
						  cNum$ = RIGHT$(STR$(100000000 + LChk), 8)
						  PRINT #11, TAB(45); "Payroll check number: "; cNum$
						  LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : Heading
						  LChk = LChk + 1
			  END SELECT
			  WRITE #6, cNum$, cDat$, Emp.Num, Lrh, Loh, Lrp, Lop, Lbo, Lgr, Lft, Lfi, Lmd, Lin, Lod, Lnt
		   END IF
		END IF
	NEXT x

	StatLin "Print Payroll Register"

	s = 1: PRINT #11, CHR$(12); : HprReg
	CLOSE #6: OPEN "..\Data\Payroll.REG" FOR INPUT AS #6
	WHILE NOT EOF(6)
		INPUT #6, cNum$, cDat$, Emp.Num, Lrh, Loh, Lrp, Lop, Lbo, Lgr, Lft, Lfi, Lmd, Lin, Lod, Lnt
		GET #2, VAL(Emp.Num), Emp
		IF s THEN p$ = Emp.Dpt: s = 0
		IF Emp.Dpt <> p$ THEN GOSUB 209
		LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HprReg
		PRINT #11, USING "& "; Emp.Dpt + Emp.Num; Emp.Nam;
		PRINT #11, USING "  #####.##"; Lrh / 100; Loh / 100; Lrp / 100; Lop / 100; Lbo / 100; Lgr / 100; Lft / 100; Lfi / 100; Lmd / 100; Lin / 100; Lod / 100; Lnt / 100;
		PRINT #11, "   "; cNum$
		L0 = L0 + Lrh: L1 = L1 + Loh
		L2 = L2 + Lrp: L3 = L3 + Lop
		L4 = L4 + Lbo: L5 = L5 + Lgr
		L6 = L6 + Lft: L7 = L7 + Lfi
		L8 = L8 + Lmd: L9 = L9 + Lin
		Lx = Lx + Lod: Ly = Ly + Lnt
	WEND
	GOSUB 209
	PRINT #11, TAB(31);
	PRINT #11, USING "#######.##"; La / 100; Lb / 100; Lc / 100; Ld / 100; Le / 100; Lf / 100; Lg / 100; Lh / 100; Li / 100; Lj / 100; Lk / 100; Ll / 100
	CLOSE #6: IF ErrSw(2) THEN KILL "..\Data\Payroll.REG"
	ErrSw(3) = 0
	ErrSw(4) = 0
	ErrSw(5) = 0
	EXIT SUB

209 p$ = Emp.Dpt
	PRINT #11,
	PRINT #11, TAB(31);
	PRINT #11, USING "#######.##"; L0 / 100; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; L8 / 100; L9 / 100; Lx / 100; Ly / 100
	PRINT #11,
	LineNum = LineNum + 3
	La = La + L0: Lb = Lb + L1: L0 = 0: L1 = 0
	Lc = Lc + L2: Ld = Ld + L3: L2 = 0: L3 = 0
	Le = Le + L4: Lf = Lf + L5: L4 = 0: L5 = 0
	Lg = Lg + L6: Lh = Lh + L7: L6 = 0: L7 = 0
	Li = Li + L8: Lj = Lj + L9: L8 = 0: L9 = 0
	Lk = Lk + Lx: Ll = Ll + Ly: Lx = 0: Ly = 0
	RETURN

210 SELECT CASE ErrNum
		CASE 1
			 PRINT #11, TAB(45); "Salaried employee with a timecard.     "
		CASE 2
			 PRINT #11, TAB(45); "Net pay is less than one penny.        "
		CASE ELSE
			 PRINT #11, TAB(45); "Error without an error code.           "
		END SELECT
		ErrSw(2) = 1
		LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HprReg
	RETURN
END SUB

DEFINT L
SUB mprEmpLst ()
	StatLin "Print Employee Master"
	IF Response("PR-print") THEN EXIT SUB
	PageNum = 0: HMaster
	t = LOF(3) / LEN(Emx)
	FOR x = 1 TO t
		IF INKEY$ = CHR$(27) THEN EXIT SUB
		Printing x, t
		GET #3, x, Emx
		GET #2, VAL(Emx.Num), Emp
		IF Emp.Nam > "!" THEN
		   LineNum = LineNum + 5: IF LineNum > 63 THEN PRINT #11, CHR$(12); : HMaster
		   PRINT #11, "Employee: "; Emp.Num; " "; Emp.Nam; TAB(80); "           Type of Pay: ";
		   IF Emp.Typ = "S" THEN PRINT #11, "Salary ";
		   IF Emp.Typ = "W" THEN PRINT #11, "Hourly ";
		   IF Emp.Typ = "H" THEN PRINT #11, "On Hold";
		   PRINT #11, TAB(129); "             Pay Rate: $ "; Emp.Pay
		   PRINT #11, "   Phone: "; Emp.Tel; TAB(40); "Social Security No: "; Emp.SSn; TAB(80); "         Filing Status: ";
		   IF Emp.Mar = "M" THEN PRINT #11, "Married";
		   IF Emp.Mar = "S" THEN PRINT #11, "Single ";
		   PRINT #11, TAB(129); "  Insurance Deduction: $ "; Emp.Ins
		   PRINT #11, " Address: "; Emp.Add; TAB(40); "        Date Hired: "; Emp.Hir; TAB(80); "Withholding Allowances: ";
		   PRINT #11, Emp.Dep; " Allowances";
		   PRINT #11, TAB(129); "     Other Deductions: $ "; Emp.Ded
		   PRINT #11, "          "; RTRIM$(Emp.Cty) + ", " + Emp.Sta + " " + Emp.Zip; TAB(40); "     Date Released: "; Emp.Rel; TAB(80); "            Tax Method: ";
		   IF Emp.Tax = "T" THEN PRINT #11, "Tax Rates ";
		   IF Emp.Tax = "P" THEN PRINT #11, "Percentage";
		   PRINT #11, TAB(129); "Bonus or Vacation Pay: $ "; Emp.Bon
		   PRINT #11, STRING$(161, "=")
		END IF
	NEXT
END SUB

SUB mprEmpMas ()
	StatLin "Employee Master File"
	CALL MhDisplayByName(0, 0, "PR-EmpMast", e)
	SubMenu 1, "D"
	r = 3: b = 1: t = LOF(3) / LEN(Emx): GOSUB EmpList

203 GET #3, n, Emx
	COLOR 1, 15: LOCATE r, 2: PRINT USING "&&³&"; Emx.Dpt; Emx.Num; Emx.Nam;
	KeyPres n$
	IF n$ = CHR$(13) THEN SubMenu 0, n$
	SELECT CASE n$
	CASE CHR$(27) 'Esc
		EXIT SUB
	CASE CHR$(0) + "M"  '===============right==
		SubMenu 0, n$
	CASE CHR$(0) + "K"  '===============left===
		SubMenu 0, n$
	CASE CHR$(0) + "P"  '===============Down===
		IF n + 1 > t THEN 203 ELSE n = n + 1
		COLOR 16, 3: LOCATE , 2: PRINT USING "&&³&"; Emx.Dpt; Emx.Num; Emx.Nam;
		IF r + 1 > 47 THEN CALL scroll(2, 3, 30, 47, 1) ELSE r = r + 1
	CASE CHR$(0) + "H"  '===============Up=====
		IF n - 1 < 1 THEN 203 ELSE n = n - 1
		COLOR 16, 3: LOCATE , 2: PRINT USING "&&³&"; Emx.Dpt; Emx.Num; Emx.Nam;
		IF r - 1 < 3 THEN CALL BkScroll(2, 3, 30, 47, 1) ELSE r = r - 1
	CASE CHR$(0) + "I"  '===============PgUp===
		b = n - r - 42: IF b < 1 THEN b = 1
		GOSUB EmpList
	CASE CHR$(0) + "Q"  '===============PgDn===
		b = n - r + 48: IF b > t - 44 THEN b = t - 44
		IF b < 1 THEN b = 1
		GOSUB EmpList
	CASE CHR$(0) + "G"  '===============Home===
		b = 1: GOSUB EmpList
	CASE CHR$(0) + "O"  '===============End====
		b = t - 44: IF b < 1 THEN b = 1
		GOSUB EmpList
	CASE "D"            '===============Display
		SubMenu 0, n$
		EmpShow
	CASE "N"            '===============New====
		SubMenu 0, n$
		IF t > 0 THEN GET #3, t, Emx
		IF Emx.Dpt = "þ" THEN n = t ELSE n = t + 1: Emx.Num = RIGHT$(STR$(1000 + n), 3)
		EmpEdit n
		t = LOF(3) / LEN(Emx)
		GOSUB EmpList
	CASE "M"            '===============Modify=
		SubMenu 0, n$
		EmpEdit n
		GOSUB EmpList
	CASE "E"            '===============Erase==
		SubMenu 0, n$
		EmpShow
		IF Emp.Ygr > 0 THEN WinMssg "Warning: active employee"
		IF Confirm("ERASE Employee") THEN
		   n$ = Emx.Num
		   GET #2, t + 1, Emp
		   Emx.Dpt = "þ"
		   Emx.Nam = ""
		   Emx.Num = n$
		   PUT #3, n, Emx
		   PUT #2, VAL(Emx.Num), Emp
		   EmpSort
		   GOSUB EmpList
		END IF
	CASE "C"            '===============Checks=
		SubMenu 0, n$
		EmpShow
		ChkWind
	END SELECT
	GOTO 203

EmpList:
	LOCATE 3: COLOR 16, 3
	FOR x = b TO t
	GET #3, x, Emx
	LOCATE , 2: PRINT USING "&&³&"; Emx.Dpt; Emx.Num; Emx.Nam
	IF CSRLIN > 47 THEN EXIT FOR
	NEXT
	n = b + r - 3
	RETURN
END SUB

DEFLNG L
SUB mprMonth ()
	StatLin "Print Month Register"
	IF Response("PR-print") THEN EXIT SUB
	s = 1: t = LOF(3) \ LEN(Emx): f$ = " #######.##"
	PageNum = 0: HMonth
	FOR x = 1 TO t
		Printing x, t
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		GET #3, x, Emx
		GET #2, VAL(Emx.Num), Emp
		IF Emp.Nam > "!" THEN
		   IF s THEN p$ = Emp.Dpt: s = 0
		   IF Emp.Dpt <> p$ THEN GOSUB 806
		   LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HMonth
		   PRINT #11, USING "& "; Emp.Dpt + Emp.Num; Emp.Nam;
		   PRINT #11, USING f$; Emp.Mrh / 100; Emp.Moh / 100;
		   PRINT #11, USING f$; Emp.Mrp / 100; Emp.Mop / 100;
		   PRINT #11, USING f$; Emp.Mbo / 100; Emp.Mgr / 100;
		   PRINT #11, USING f$; Emp.Mft / 100; Emp.Mfi / 100;
		   PRINT #11, USING f$; Emp.Mmd / 100; Emp.Min / 100;
		   PRINT #11, USING f$; Emp.Mod / 100; Emp.Mnt / 100
		   L0 = L0 + Emp.Mrh
		   L1 = L1 + Emp.Moh
		   L2 = L2 + Emp.Mrp
		   L3 = L3 + Emp.Mop
		   L4 = L4 + Emp.Mbo
		   L5 = L5 + Emp.Mgr
		   L6 = L6 + Emp.Mft
		   L7 = L7 + Emp.Mfi
		   L8 = L8 + Emp.Mmd
		   L9 = L9 + Emp.Min
		   Lx = Lx + Emp.Mod
		   Ly = Ly + Emp.Mnt
		END IF
	NEXT
	GOSUB 806
	PRINT #11, TAB(31);
	PRINT #11, USING f$; La / 100; Lb / 100;
	PRINT #11, USING f$; Lc / 100; Ld / 100;
	PRINT #11, USING f$; Le / 100; Lf / 100;
	PRINT #11, USING f$; Lg / 100; Lh / 100;
	PRINT #11, USING f$; Li / 100; Lj / 100;
	PRINT #11, USING f$; Lk / 100; Ll / 100
	PRINT #11, CHR$(12)
	PCOPY 1, 0
	StatLin "Close END OF MONTH"
	IF Closing("MONTH") = 0 THEN EXIT SUB
	FOR x = 1 TO t
		Printing x, t
		GET #2, x, Emp
		Emp.Mrh = 0
		Emp.Moh = 0
		Emp.Mrp = 0
		Emp.Mop = 0
		Emp.Mbo = 0
		Emp.Mgr = 0
		Emp.Mft = 0
		Emp.Mfi = 0
		Emp.Mmd = 0
		Emp.Min = 0
		Emp.Mod = 0
		Emp.Mnt = 0
		PUT #2, x, Emp
	NEXT
	EXIT SUB

806 p$ = Emp.Dpt
	PRINT #11,
	PRINT #11, TAB(31);
	PRINT #11, USING f$; L0 / 100; L1 / 100; L2 / 100;
	PRINT #11, USING f$; L3 / 100; L4 / 100; L5 / 100;
	PRINT #11, USING f$; L6 / 100; L7 / 100; L8 / 100;
	PRINT #11, USING f$; L9 / 100; Lx / 100; Ly / 100
	PRINT #11,
	LineNum = LineNum + 3
	La = La + L0: L0 = 0
	Lb = Lb + L1: L1 = 0
	Lc = Lc + L2: L2 = 0
	Ld = Ld + L3: L3 = 0
	Le = Le + L4: L4 = 0
	Lf = Lf + L5: L5 = 0
	Lg = Lg + L6: L6 = 0
	Lh = Lh + L7: L7 = 0
	Li = Li + L8: L8 = 0
	Lj = Lj + L9: L9 = 0
	Lk = Lk + Lx: Lx = 0
	Ll = Ll + Ly: Ly = 0
	RETURN
END SUB

SUB mprQuarter ()
	StatLin "Print Quarter Register"
	IF Response("PR-print") THEN EXIT SUB
	s = 1: t = LOF(3) \ LEN(Emx): f$ = " #######.##"
	PageNum = 0: HQuarter
	FOR x = 1 TO t
		Printing x, t
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		GET #3, x, Emx
		GET #2, VAL(Emx.Num), Emp
		IF Emp.Nam > "!" THEN
		   IF s THEN p$ = Emp.Dpt: s = 0
		   IF Emp.Dpt <> p$ THEN GOSUB 805
		   LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HQuarter
		   PRINT #11, USING "& "; Emp.Dpt + Emp.Num; Emp.Nam;
		   PRINT #11, USING f$; Emp.Qrh / 100; Emp.Qoh / 100;
		   PRINT #11, USING f$; Emp.Qrp / 100; Emp.Qop / 100;
		   PRINT #11, USING f$; Emp.Qbo / 100; Emp.Qgr / 100;
		   PRINT #11, USING f$; Emp.Qft / 100; Emp.Qfi / 100;
		   PRINT #11, USING f$; Emp.Qmd / 100; Emp.Qin / 100;
		   PRINT #11, USING f$; Emp.Qod / 100; Emp.Qnt / 100
		   L0 = L0 + Emp.Qrh
		   L1 = L1 + Emp.Qoh
		   L2 = L2 + Emp.Qrp
		   L3 = L3 + Emp.Qop
		   L4 = L4 + Emp.Qbo
		   L5 = L5 + Emp.Qgr
		   L6 = L6 + Emp.Qft
		   L7 = L7 + Emp.Qfi
		   L8 = L8 + Emp.Qmd
		   L9 = L9 + Emp.Qin
		   Lx = Lx + Emp.Qod
		   Ly = Ly + Emp.Qnt
		END IF
	NEXT
	GOSUB 805
	PRINT #11, TAB(31);
	PRINT #11, USING f$; La / 100; Lb / 100;
	PRINT #11, USING f$; Lc / 100; Ld / 100;
	PRINT #11, USING f$; Le / 100; Lf / 100;
	PRINT #11, USING f$; Lg / 100; Lh / 100;
	PRINT #11, USING f$; Li / 100; Lj / 100;
	PRINT #11, USING f$; Lk / 100; Ll / 100
	PRINT #11, CHR$(12)
	PCOPY 1, 0
	StatLin "CLOSE END OF QUARTER"
	IF Closing("QUARTER") = 0 THEN EXIT SUB
	FOR x = 1 TO t
		Printing x, t
		GET #2, x, Emp
		Emp.Qrh = 0
		Emp.Qoh = 0
		Emp.Qrp = 0
		Emp.Qop = 0
		Emp.Qbo = 0
		Emp.Qgr = 0
		Emp.Qft = 0
		Emp.Qfi = 0
		Emp.Qmd = 0
		Emp.Qin = 0
		Emp.Qod = 0
		Emp.Qnt = 0
		PUT #2, x, Emp
	NEXT
	EXIT SUB

805 p$ = Emp.Dpt
	PRINT #11,
	PRINT #11, TAB(31);
	PRINT #11, USING f$; L0 / 100; L1 / 100; L2 / 100;
	PRINT #11, USING f$; L3 / 100; L4 / 100; L5 / 100;
	PRINT #11, USING f$; L6 / 100; L7 / 100; L8 / 100;
	PRINT #11, USING f$; L9 / 100; Lx / 100; Ly / 100
	PRINT #11,
	LineNum = LineNum + 3
	La = La + L0: L0 = 0
	Lb = Lb + L1: L1 = 0
	Lc = Lc + L2: L2 = 0
	Ld = Ld + L3: L3 = 0
	Le = Le + L4: L4 = 0
	Lf = Lf + L5: L5 = 0
	Lg = Lg + L6: L6 = 0
	Lh = Lh + L7: L7 = 0
	Li = Li + L8: L8 = 0
	Lj = Lj + L9: L9 = 0
	Lk = Lk + Lx: Lx = 0
	Ll = Ll + Ly: Ly = 0
	RETURN
END SUB

DEFINT L
SUB mprSheet ()
	StatLin "Print Timesheets"
	IF Response("PR-print") THEN EXIT SUB
	IF Payroll = "B" THEN t = 1 ELSE t = 0
	r = LOF(3) / LEN(Emx)
	PageNum = 0: HTimeCard
	FOR x = 1 TO r
		Printing x, r
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		GET #3, x, Emx
		GET #2, VAL(Emx.Num), Emp
		IF Payroll = "B" THEN
		   h$ = Emp.Tot(0)
		ELSE
		   h$ = Emp.Hrs(0, 7)
		END IF
		IF h$ > "  0.00" THEN
			IF LineNum > 55 THEN PRINT #11, CHR$(12); : HTimeCard
			PRINT #11, USING " & "; Emp.Dpt + Emp.Num; Emp.Nam; Emp.SSn;
			FOR d = 0 TO 6
				PRINT #11, TAB(47); dWeek(d); "   ";
			FOR w = 0 TO t
			FOR h = 0 TO 3
				PRINT #11, USING "   &"; Emp.Clk(w, d, h);
			NEXT
				PRINT #11, "   "; Emp.Hrs(w, d); "   ";
			NEXT
			NEXT
			PRINT #11,
			PRINT #11,
			PRINT #11, USING " &"; SPC(71); "   Total hours: "; Emp.Hrs(0, 7); SPC(33); Emp.Hrs(1, 7); SPC(7); Emp.Tot(0)
			PRINT #11, USING " &"; SPC(71); " Regular hours: "; Emp.Hrs(0, 8); SPC(33); Emp.Hrs(1, 8); SPC(7); Emp.Tot(1)
			PRINT #11, USING " &"; SPC(71); "Overtime {1.5}: "; Emp.Hrs(0, 9); SPC(33); Emp.Hrs(1, 9); SPC(7); Emp.Tot(2)
			PRINT #11,
			PRINT #11,
			LineNum = LineNum + 13
		END IF
	NEXT
	PRINT #11, CHR$(12)
	ErrSw(1) = 0
END SUB

DEFLNG A-S
SUB mprW2s ()
	StatLin "Print Employees W-2s"
	IF Response("PR-print") THEN EXIT SUB
	PRINT #11, CHR$(27); "@";
	WIDTH "LPT1:", 255
	PRINT #11, CHR$(27); "E"; CHR$(27); "G";
	t = LOF(3) / LEN(Emx)
	FOR x = 1 TO t
		IF INKEY$ = CHR$(27) THEN EXIT SUB
		Printing x, t
		GET #3, x, Emx
		GET #2, VAL(Emx.Num), Emp
		IF Emp.Nam > "!" AND Emp.Ygr > 0 THEN
			IF Emp.Yfi >= 379440 THEN Lwf = 6120000 ELSE Lwf = Emp.Ygr
		   '........................................................................
			z = z + 1
			PRINT #11,
			PRINT #11,
			PRINT #11, TAB(5); RIGHT$(STR$(100000 + z), 5)
			PRINT #11,
			PRINT #11, TAB(5); TaxID; TAB(40); :   PRINT #11, USING "   ########,.##"; Emp.Ygr / 100; Emp.Yft / 100
			PRINT #11,
			PRINT #11, TAB(5); Company; TAB(40); : PRINT #11, USING "   ########,.##"; Lwf / 100; Emp.Yfi / 100
			PRINT #11,
			PRINT #11, TAB(5); Street; TAB(40); :  PRINT #11, USING "   ########,.##"; Emp.Ygr / 100; Emp.Ymd / 100
			PRINT #11, TAB(5); Address
			PRINT #11, TAB(5); City
			PRINT #11,
			PRINT #11, TAB(5); Emp.SSn
			PRINT #11,
			PRINT #11, TAB(5); Emp.Nam
			PRINT #11,
			PRINT #11,
			PRINT #11, TAB(5); Emp.Add
			PRINT #11,
			PRINT #11, TAB(5); Emp.Cty; "  "; Emp.Sta; "  "; Emp.Zip
			PRINT #11,
			PRINT #11,
			PRINT #11,
			PRINT #11, TAB(4); "TN"; TAB(10); EmpID
			PRINT #11,
			PRINT #11,
			PRINT #11,
			PRINT #11,
			PRINT #11,
			PRINT #11,
			PRINT #11,
			PRINT #11,
			PRINT #11,
			':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
			Lgr = Lgr + Emp.Ygr
			Lft = Lft + Emp.Yft
			Lfi = Lfi + Emp.Yfi
			Lmd = Lmd + Emp.Ymd
			Lfw = Lfw + Lwf
			
			Fgr = Fgr + Emp.Ygr
			Fft = Fft + Emp.Yft
			Ffi = Ffi + Emp.Yfi
			Fmd = Fmd + Emp.Ymd
			Ffw = Ffw + Lwf
			
			IF z MOD 41 = 0 THEN GOSUB w2s
		END IF
	NEXT
	IF z > 41 AND z MOD 41 <> 0 THEN GOSUB w2s
	WinMssg "Wait until W-2s are printed then load W-3"
	IF Confirm("Print W-3") THEN
	   PRINT #11,
	   PRINT #11,
	   PRINT #11, TAB(5); "XXXXX"
	   PRINT #11,
	   PRINT #11, TAB(20); "X"; TAB(41); : PRINT #11, USING "   ########,.##"; Fgr / 100; Fft / 100
	   PRINT #11,
	   PRINT #11, TAB(41); :               PRINT #11, USING "   ########,.##"; Ffw / 100; Ffi / 100
	   PRINT #11,
	   PRINT #11, TAB(41); :               PRINT #11, USING "   ########,.##"; Fgr / 100; Fmd / 100
	   PRINT #11,
	   PRINT #11, TAB(5); : PRINT #11, USING "####"; z; : PRINT #11, TAB(15); TaxID
	   PRINT #11,
	   PRINT #11, TAB(5); Company
	   PRINT #11,
	   PRINT #11,
	   PRINT #11, TAB(5); Street
	   PRINT #11, TAB(5); Address
	   PRINT #11, TAB(5); City
	   PRINT #11,
	   PRINT #11,
	   PRINT #11,
	   PRINT #11,
	   PRINT #11, TAB(5); EmpID
	   PRINT #11, CHR$(12);
	END IF
	EXIT SUB

w2s: PRINT #11, 'W-2 subtotal every 41 emplyees
	 PRINT #11,
	 PRINT #11, TAB(5); "TOTAL"
	 PRINT #11,
	 PRINT #11, TAB(5); TaxID; TAB(40); :   PRINT #11, USING "   ########,.##"; Lgr / 100; Lft / 100
	 PRINT #11,
	 PRINT #11, TAB(5); Company; TAB(40); : PRINT #11, USING "   ########,.##"; Lfw / 100; Lfi / 100
	 PRINT #11,
	 PRINT #11, TAB(5); Street; TAB(40); :  PRINT #11, USING "   ########,.##"; Lgr / 100; Lmd / 100
	 PRINT #11, TAB(5); Address
	 PRINT #11, TAB(5); City
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11, TAB(58); "X"
	 PRINT #11,
	 PRINT #11, TAB(4); "TN"; TAB(10); EmpID
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 PRINT #11,
	 Lgr = 0
	 Lft = 0
	 Lfi = 0
	 Lmd = 0
	 Lfw = 0
	 Lmw = 0
	 RETURN
END SUB

DEFINT A-K, M-S
SUB mprYear ()
	StatLin "Print Year Register"
	IF Response("PR-print") THEN EXIT SUB
	s = 1: t = LOF(3) \ LEN(Emx): f$ = " #######.##"
	PageNum = 0: HYear
	FOR x = 1 TO t
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		Printing x, t
		GET #3, x, Emx
		GET #2, VAL(Emx.Num), Emp
		IF Emp.Nam > "!" THEN
		   IF s THEN p$ = Emp.Dpt: s = 0
		   IF Emp.Dpt <> p$ THEN GOSUB 211
		   LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HYear
		   PRINT #11, USING "& "; Emp.Dpt + Emp.Num; Emp.Nam;
		   PRINT #11, USING f$; Emp.Yrh / 100; Emp.Yoh / 100;
		   PRINT #11, USING f$; Emp.Yrp / 100; Emp.Yop / 100;
		   PRINT #11, USING f$; Emp.Ybo / 100; Emp.Ygr / 100;
		   PRINT #11, USING f$; Emp.Yft / 100; Emp.Yfi / 100;
		   PRINT #11, USING f$; Emp.Ymd / 100; Emp.Yin / 100;
		   PRINT #11, USING f$; Emp.Yod / 100; Emp.Ynt / 100
		   L0 = L0 + Emp.Yrh
		   L1 = L1 + Emp.Yoh
		   L2 = L2 + Emp.Yrp
		   L3 = L3 + Emp.Yop
		   L4 = L4 + Emp.Ybo
		   L5 = L5 + Emp.Ygr
		   L6 = L6 + Emp.Yft
		   L7 = L7 + Emp.Yfi
		   L8 = L8 + Emp.Ymd
		   L9 = L9 + Emp.Yin
		   Lx = Lx + Emp.Yod
		   Ly = Ly + Emp.Ynt
		END IF
	NEXT
	GOSUB 211
	PRINT #11, TAB(31);
	PRINT #11, USING f$; La / 100; Lb / 100; Lc / 100;
	PRINT #11, USING f$; Ld / 100; Le / 100; Lf / 100;
	PRINT #11, USING f$; Lg / 100; Lh / 100; Li / 100;
	PRINT #11, USING f$; Lj / 100; Lk / 100; Ll / 100
	PRINT #11, CHR$(12)
	PCOPY 1, 0
	IF Closing("YEAR") = 0 THEN EXIT SUB
	StatLin "Close END OF YEAR"
	FOR x = 1 TO t
		Printing x, t
		GET #2, x, Emp
		Emp.Yrh = 0
		Emp.Yoh = 0
		Emp.Yrp = 0
		Emp.Yop = 0
		Emp.Ybo = 0
		Emp.Ygr = 0
		Emp.Yft = 0
		Emp.Yfi = 0
		Emp.Ymd = 0
		Emp.Yin = 0
		Emp.Yod = 0
		Emp.Ynt = 0
		PUT #2, x, Emp
	NEXT
	CLOSE
	NAME "..\Data\Employee.CHK" AS "..\Data\Employee.C" + MID$(DATE$, 9, 2)
	NAME "..\Data\Employee.CKX" AS "..\Data\Employee.X" + MID$(DATE$, 9, 2)
	EXIT SUB
211 p$ = Emp.Dpt
	PRINT #11,
	PRINT #11, TAB(31);
	PRINT #11, USING f$; L0 / 100; L1 / 100; L2 / 100;
	PRINT #11, USING f$; L3 / 100; L4 / 100; L5 / 100;
	PRINT #11, USING f$; L6 / 100; L7 / 100; L8 / 100;
	PRINT #11, USING f$; L9 / 100; Lx / 100; Ly / 100
	PRINT #11,
	LineNum = LineNum + 3
	La = La + L0: L0 = 0
	Lb = Lb + L1: L1 = 0
	Lc = Lc + L2: L2 = 0
	Ld = Ld + L3: L3 = 0
	Le = Le + L4: L4 = 0
	Lf = Lf + L5: L5 = 0
	Lg = Lg + L6: L6 = 0
	Lh = Lh + L7: L7 = 0
	Li = Li + L8: L8 = 0
	Lj = Lj + L9: L9 = 0
	Lk = Lk + Lx: Lx = 0
	Ll = Ll + Ly: Ly = 0
	RETURN
END SUB

DEFSNG A-Z
SUB PR_Files ()
	CLOSE 1: OPEN "..\Data\Accounts.MAS" FOR RANDOM SHARED AS #1 LEN = LEN(Acc)
	CLOSE 2: OPEN "..\Data\Employee.MAS" FOR RANDOM LOCK READ WRITE AS #2 LEN = LEN(Emp)
	CLOSE 3: OPEN "..\Data\Employee.NAM" FOR RANDOM LOCK READ WRITE AS #3 LEN = LEN(Emx)
	CLOSE 4: OPEN "..\Data\Employee.CHK" FOR RANDOM LOCK READ WRITE AS #4 LEN = LEN(Emc)
	CLOSE 5: OPEN "..\Data\Employee.CKX" FOR RANDOM LOCK READ WRITE AS #5 LEN = LEN(Ecx)
	'             "Payroll .REG" reserved   AS #6
	CLOSE #11: OPEN "LPT1:" FOR OUTPUT AS #11
END SUB

DEFLNG A-Z
SUB SocSecTax (Grs, FICA)
	IF Grs = 0 THEN
	   FICA = 0
	   EXIT SUB
	END IF

	WAGEmax = 8790000
	FICAmax = 544980
	SSECtax! = .062

	IF Emp.Yfi >= FICAmax THEN
	   FICA = 0
	ELSEIF Emp.Ygr + Grs >= WAGEmax THEN
	   FICA = FICAmax - Emp.Yfi
	ELSE FICA = SSECtax! * Grs
	   OverPaid! = ((Emp.Ygr + Grs) * SSECtax!) - (Emp.Yfi + FICA)
	   IF OverPaid! < -.5 THEN FICA = FICA - 1 ELSE IF OverPaid! > .5 THEN FICA = FICA + 1
	END IF
END SUB

