DECLARE SUB apRepost ()
DEFINT A-Z: DEFLNG L: DEFDBL T

DECLARE FUNCTION InvNet ()
DECLARE FUNCTION InvDis ()
DECLARE FUNCTION ChkTot ()
DECLARE FUNCTION VenFind (f$)
DECLARE FUNCTION FindInv (n&)
DECLARE FUNCTION InvOpen$ ()
DECLARE FUNCTION RptDate (f$, t$)
DECLARE FUNCTION PickSto (s$)

DECLARE SUB AP_Files ()
DECLARE SUB AccDist ()
DECLARE SUB NumWords (n$, f$)
DECLARE SUB PrtChk (f$, t$)
DECLARE SUB InvFind (n$, f, e)
DECLARE SUB InvLong (f$, t$)
DECLARE SUB InvShor (f$, t$)
DECLARE SUB InvDisp (n)
DECLARE SUB InvList (n)
DECLARE SUB InvSort ()
DECLARE SUB InvPost ()
DECLARE SUB InvEnt (e)

DECLARE SUB apCkVoid ()
DECLARE SUB apCkShow ()
DECLARE SUB apCkPost ()
DECLARE SUB apCkPrnt ()
DECLARE SUB apCkSort ()
DECLARE SUB apCkWind ()
DECLARE SUB apCkFind (f, e)

DECLARE SUB WinVen (f$)
DECLARE SUB WinAcc (f$)
DECLARE SUB WinInv (f, e)

DECLARE SUB VenLine (r, Lm, Ly)
DECLARE SUB VenDisp (x)
DECLARE SUB VenEdit ()
DECLARE SUB VenAcct ()
DECLARE SUB VenShow ()
DECLARE SUB VenSort ()

DECLARE SUB HAging (s$)
DECLARE SUB HAccount (s$)
DECLARE SUB HInvoice (s$)
DECLARE SUB HInvList (s$)
DECLARE SUB HRegister (f$)
DECLARE SUB HChecks (s$)
DECLARE SUB HRequir (s$)
DECLARE SUB HStoreInv (s$)
DECLARE SUB HVendor (s$)
DECLARE SUB HVenList (s$)

DECLARE SUB Aging ()
DECLARE SUB Require ()

REM $INCLUDE: '..\BI\Library.REC'
REM $INCLUDE: '..\BI\Payables.Rec'

	DIM SHARED Ven AS Vendors
	DIM SHARED Vnx AS VenIndex
	DIM SHARED Inv AS Invoices
	DIM SHARED Inx AS InvIndex
	DIM SHARED Ins AS InvStore
	DIM SHARED Chk AS Checks
	DIM SHARED Chx AS ChkIndex

	DIM SHARED oVe          AS INTEGER
	DIM SHARED oGr          AS STRING * 8
	DIM SHARED oDi          AS STRING * 8
	DIM SHARED oCr          AS STRING * 8
	DIM SHARED oNe          AS STRING * 8
	DIM SHARED oSt(0 TO 24) AS STRING * 2
	DIM SHARED oAc(0 TO 24) AS STRING * 2
	DIM SHARED oAm(0 TO 24) AS STRING * 8
	DIM SHARED InN(0 TO 12) AS STRING * 8
	DIM SHARED InA(0 TO 12) AS STRING * 8
	DIM SHARED In           AS INTEGER
	DIM SHARED Ix           AS INTEGER
	DIM SHARED Cn           AS INTEGER
	DIM SHARED Cx           AS INTEGER
	DIM SHARED Sort         AS INTEGER

DEFINT L, T
SUB AccDist ()
	ERASE oSt, oAc, oAm
	FOR x = 0 TO 24
		IF Inv.Act(x) < " 0" THEN EXIT FOR
		oSt(x) = Inv.Sto(x)
		oAc(x) = Inv.Act(x)
		oAm(x) = Inv.Amt(x)
	NEXT x
END SUB

DEFLNG L
DEFDBL T
SUB Activity (f$, t$)
	StatLin "Accounts Activity"
	f& = VAL(RIGHT$(f$, 4) + LEFT$(f$, 2) + MID$(f$, 4, 2)) 'from date
	t& = VAL(RIGHT$(t$, 4) + LEFT$(t$, 2) + MID$(t$, 4, 2)) 'to date

	PageNum = 0: HAccount "00"
	FOR x = 1 TO LOF(7) / LEN(Inv)
		IF INKEY$ = CHR$(27) THEN PRINT #11, CHR$(12); : EXIT SUB
		GET #7, x, Inv
		d& = VAL(RIGHT$(Inv.Bdt, 4) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2))
		IF d& >= f& AND d& <= t& THEN
		   GET #2, VAL(Inv.Ven), Ven
		   FOR a = 0 TO 24
			   IF Inv.Act(a) > " 0" THEN
				  GET #1, VAL(Inv.Act(a)), Acc
				  PRINT #11, USING "#####  "; Inx.Inv;
				  PRINT #11, USING " & "; Ven.Num; Ven.Nam; Inv.Ref; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk; Inv.Sto(x); Inv.Act(x); Acc.Nam; Inv.Amt(x)
				  T1 = T1 + VAL(Inv.Amt(a))
				  LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HAccount "00"
			   END IF
		   NEXT
		END IF
	NEXT
	PRINT #11, : PRINT #11, TAB(134); : PRINT #11, USING "########.##"; T1
	PRINT #11, CHR$(12);
END SUB

SUB Aging ()
	StatLin "Aging of Payables"
	f = 1: PageNum = 0: HAging "00"
	FOR x = 1 TO LOF(9) \ LEN(Inx)
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		GET #9, x, Inx
		GET #7, Inx.Num, Inv
		IF Inv.Chk = "*Not Paid*" THEN
		   IF p <> Inx.Ven THEN
			  p = Inx.Ven
			  IF f THEN f = 0 ELSE GOSUB 532
			  GET #2, Inx.Ven, Ven
			  PRINT #11, USING "& "; Ven.Num; LEFT$(Ven.Nam, 26);
		   END IF
		   PRINT #11, TAB(33);
		   PRINT #11, Inv.Ddt;
		   PRINT #11, USING " ########"; Inx.Inv;
		   d = DateDif(DATE$, Inv.Ddt)
		   SELECT CASE d
				  CASE IS < -60: y = 53:  T1 = T1 + VAL(Inv.Net)
				  CASE IS < -45: y = 63:  T2 = T2 + VAL(Inv.Net)
				  CASE IS < -30: y = 73:  T3 = T3 + VAL(Inv.Net)
				  CASE IS < -15: y = 83:  T4 = T4 + VAL(Inv.Net)
				  CASE IS < 0:   y = 93:  T5 = T5 + VAL(Inv.Net)
				  CASE IS = 0:   y = 103: T6 = T6 + VAL(Inv.Net)
				  CASE IS > 60:  y = 153: T10 = T10 + VAL(Inv.Net)
				  CASE IS > 45:  y = 143: T0 = T0 + VAL(Inv.Net)
				  CASE IS > 30:  y = 133: T9 = T9 + VAL(Inv.Net)
				  CASE IS > 15:  y = 123: T8 = T8 + VAL(Inv.Net)
				  CASE IS > 0:   y = 113: T7 = T7 + VAL(Inv.Net)
		   END SELECT
		   PRINT #11, TAB(y); : PRINT #11, Inv.Net
		   LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HAging "00"
		END IF
	NEXT
	GOSUB 532: PRINT #11, : PRINT #11, TAB(52);
	PRINT #11, USING "######.## "; TA; TB; TC; TD; TE; TF; TG; TH; TI; TJ; TK
	PRINT #11, CHR$(12)
	EXIT SUB
532 PRINT #11, CHR$(27); "G": PRINT #11, TAB(52); : PRINT #11, USING "######.## "; T1; T2; T3; T4; T5; T6; T7; T8; T9; T0; T10: PRINT #11, CHR$(27); "H"
	TA = TA + T1: T1 = 0
	TB = TB + T2: T2 = 0
	TC = TC + T3: T3 = 0
	TD = TD + T4: T4 = 0
	TE = TE + T5: T5 = 0
	TF = TF + T6: T6 = 0
	TG = TG + T7: T7 = 0
	TH = TH + T8: T8 = 0
	TI = TI + T9: T9 = 0
	TJ = TJ + T0: T0 = 0
	TK = TK + T10: T10 = 0
	LineNum = LineNum + 3: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HAging "00"
	RETURN
END SUB

DEFINT L, T
SUB apCkFind (f, e)
	f = 0: e = 0: b = 1: t = LOF(6) / LEN(Chx)
	DO UNTIL (t < b)
	   m = (t + b) \ 2: GET #6, m, Chx
	   IF Vnx.Num > Chx.Ven THEN b = m + 1 ELSE t = m - 1
	LOOP
	DO WHILE Vnx.Num > Chx.Ven AND NOT EOF(6)
	   GET #6, , Chx
	LOOP
	IF Vnx.Num = Chx.Ven THEN
	   f = LOC(6): e = f
	   DO WHILE NOT EOF(6)
		  GET #6, , Chx
		  IF Vnx.Num = Chx.Ven THEN e = LOC(6) ELSE EXIT DO
	   LOOP
	END IF
END SUB

DEFLNG I
SUB apCkPost ()
	Chk.Sta = "    "
	PUT #5, Cn, Chk '-------------------------------[save check master]------
	Chx.Ven = VAL(Chk.Ven)
	Chx.Dat = VAL(RIGHT$(Chk.Dat, 4) + LEFT$(Chk.Dat, 2) + MID$(Chk.Dat, 4, 2))
	Chx.Num = Cx
	PUT #6, Cx, Chx '-------------------------------[save check index]-------
	
	GET #2, VAL(Chk.Ven), Ven
	Ven.Mpd = Ven.Mpd + VAL(Chk.Amt) * 100
	Ven.Ypd = Ven.Ypd + VAL(Chk.Amt) * 100
	PUT #2, VAL(Chk.Ven), Ven '---------------------[post vendor]------------
	FOR x = 0 TO 11
		IF Chk.Inv(x) > 0 THEN
		   In = FindInv(Chk.Inv(x))
		   IF In = 0 THEN WinMssg "Invoice not found": ERROR 200
		   Inv.Chk = "C " + Chk.Num
		   Inv.Pdt = Chk.Dat
		   PUT #7, In, Inv '------------------------[post invoice]-----------
		   FOR y = 0 TO 24
			   IF Inv.Act(y) > " 0" THEN
				  GET #1, 1, Acc
				  Acc.Mcr(0) = Acc.Mcr(0) + VAL(Inv.Amt(y)) * 100
				  Acc.Ycr(0) = Acc.Ycr(0) + VAL(Inv.Amt(y)) * 100
				  Acc.Mcr(VAL(Inv.Sto(y))) = Acc.Mcr(VAL(Inv.Sto(y))) + VAL(Inv.Amt(y)) * 100
				  Acc.Ycr(VAL(Inv.Sto(y))) = Acc.Ycr(VAL(Inv.Sto(y))) + VAL(Inv.Amt(y)) * 100
				  PUT #1, 1, Acc '------------------[credit store Cash-01]---
				  
				  GET #1, VAL(Inv.Act(y)), Acc
				  GET #1, VAL(Acc.Crd), Acc
				  Acc.Mdb(0) = Acc.Mdb(0) + VAL(Inv.Amt(y)) * 100
				  Acc.Ydb(0) = Acc.Ydb(0) + VAL(Inv.Amt(y)) * 100
				  Acc.Mdb(VAL(Inv.Sto(y))) = Acc.Mdb(VAL(Inv.Sto(y))) + VAL(Inv.Amt(y)) * 100
				  Acc.Ydb(VAL(Inv.Sto(y))) = Acc.Ydb(VAL(Inv.Sto(y))) + VAL(Inv.Amt(y)) * 100
				  PUT #1, LOC(1), Acc '-------------[debit store payable]----
			   ELSE
				  EXIT FOR
			   END IF
		   NEXT
		END IF
	NEXT
END SUB

DEFINT I
DEFLNG L
DEFDBL T
SUB apCkPrnt ()
	WIDTH "LPT1:", 255
	PRINT #11, CHR$(27); "E";       'Reset Printer
	PRINT #11, CHR$(27); "(10U";    'PC-8 character set
	PRINT #11, CHR$(27); "(s0P";    'Fixed font
	PRINT #11, CHR$(27); "(s4099T"; 'Letter Courier
	PRINT #11, CHR$(27); "(s2B";    'Medium bold
	PRINT #11, CHR$(27); "(s10H";   'Pitch 10 cpi
	PRINT #11, CHR$(27); "&l2A";    '8.5" x 11"
	PRINT #11, CHR$(27); "&l0E";    'Top margin 0 lines
	PRINT #11, CHR$(27); "&a0L";    'Left margin 0
	PRINT #11, CHR$(27); "&a164M";  'Right margin
	PRINT #11, CHR$(27); "&l66F";   'Text length 66 lines
	PRINT #11, CHR$(27); "&s0C"     'Line wrap
	
	GET #2, VAL(Chk.Ven), Ven
	NumWords Chk.Amt, word$
	
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11, TAB(45); Chk.Dat; "  "; Chk.Num; "  "; : PRINT #11, USING "$$#####,.##"; VAL(Chk.Amt)
	PRINT #11,
	PRINT #11,
	PRINT #11, TAB(6); word$
	PRINT #11,
	PRINT #11,
	PRINT #11, TAB(13); Ven.Nam;
	PRINT #11, TAB(13); Ven.Add
	PRINT #11, TAB(13); Ven.Str
	PRINT #11, TAB(13); RTRIM$(Ven.Cty); ", "; Ven.Sta; " "; Ven.Zip
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11,

	PRINT #11, CHR$(27); "(s4102T"; 'Letter Gothic
	PRINT #11, CHR$(27); "(s0B";    'Demi bold
	PRINT #11,
	PRINT #11, " …ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕª "
	PRINT #11, " ∫Invoice No≥Reference ≥   Date   ≥   Gross  ≥ Discount ≥  Credit  ≥Net Amount∫ "
	PRINT #11, " «ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ∂ "

	FOR x = 0 TO 11
	IF Chk.Inv(x) > 0 THEN
	   IF FindInv(Chk.Inv(x)) = 0 THEN WinMssg "Invoice not found"
	   PRINT #11, " ∫";
	   PRINT #11, USING " ######## ≥&≥&≥&≥"; Inv.Num; Inv.Ref; Inv.Bdt;
	   PRINT #11, USING " & ≥"; Inv.Grs; Inv.Dis; Inv.Crd;
	   PRINT #11, USING " & ∫"; Inv.Net
	   T1 = T1 + VAL(Inv.Grs)
	   T2 = T2 + VAL(Inv.Dis)
	   T3 = T3 + VAL(Inv.Crd)
	   T4 = T4 + VAL(Inv.Net)
	ELSE
	   PRINT #11, " ∫          ≥          ≥          ≥          ≥          ≥          ≥          ∫ "
	END IF
	NEXT
	PRINT #11, " «ƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ∂ "
	PRINT #11, " ∫                          Total ≥";
	PRINT #11, USING "######.## ≥"; T1; T2; T3;
	PRINT #11, USING "######.## ∫"; T4
	PRINT #11, " »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕº "
	PRINT #11,
	PRINT #11,
	PRINT #11,
	PRINT #11, " …ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕª "
	PRINT #11, " ∫Invoice No≥Reference ≥   Date   ≥   Gross  ≥ Discount ≥  Credit  ≥Net Amount∫ "
	PRINT #11, " «ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ∂ "

	FOR x = 0 TO 11
	IF Chk.Inv(x) > 0 THEN
	   IF FindInv(Chk.Inv(x)) = 0 THEN WinMssg "Invoice not found"
	   PRINT #11, " ∫";
	   PRINT #11, USING " ######## ≥&≥&≥&≥"; Inv.Num; Inv.Ref; Inv.Bdt;
	   PRINT #11, USING " & ≥"; Inv.Grs; Inv.Dis; Inv.Crd;
	   PRINT #11, USING " & ∫"; Inv.Net
	ELSE
	   PRINT #11, " ∫          ≥          ≥          ≥          ≥          ≥          ≥          ∫ "
	END IF
	NEXT
	PRINT #11, " «ƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ∂ "
	PRINT #11, " ∫                          Total ≥";
	PRINT #11, USING "######.## ≥"; T1; T2; T3;
	PRINT #11, USING "######.## ∫"; T4
	PRINT #11, " »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕº "
	PRINT #11, CHR$(12);
END SUB

DEFINT L, T
SUB apCkShow ()
	PCOPY 0, 2: CALL MhDisplayByName(0, 0, "AP-Check", e)
	COLOR 29, 15
	NumWords Chk.Amt, w$
	qPrint Company, 8, 6, 0, 1
	qPrint Street, 9, 6, 0, 1
	qPrint Address, 10, 6, 0, 1
	qPrint City, 11, 6, 0, 1
	qPrint Chk.Num, 8, 60, 0, 1
	qPrint w$, 16, 6, 0, 1
	qPrint Chk.Dat, 14, 43, 0, 1
	qPrint Chk.Amt, 14, 60, 0, 1
	qPrint Ven.Num, 18, 13, 0, 1
	qPrint Ven.Nam, 18, 17, 0, 1
	qPrint Ven.Add, 19, 17, 0, 1
	qPrint Ven.Cty + " " + Ven.Sta + " " + Ven.Zip, 20, 17, 0, 1
	qPrint Chk.Sta, 22, 12, 0, 1
	FOR r = 0 TO 11
		IF Chk.Inv(r) > 0 THEN
		   LOCATE r + 28, 5: PRINT Chk.Inv(r);
		   IF FindInv(Chk.Inv(r)) = 0 THEN
			  Inv.Ref = "* PURGED *"
			  Inv.Bdt = ""
			  Inv.Ddt = ""
			  Inv.Chk = ""
			  Inv.Net = ""
		   END IF
		   qPrint Inv.Ref, r + 28, 16, 0, 1
		   qPrint Inv.Bdt, r + 28, 27, 0, 1
		   qPrint Inv.Ddt, r + 28, 38, 0, 1
		   qPrint Inv.Chk, r + 28, 49, 0, 1
		   qPrint Inv.Net, r + 28, 61, 0, 1
		END IF
	NEXT
	qPrint Chk.Amt, 41, 61, 0, 1
END SUB

DEFSNG A-Z
SUB apCkSort ()
	LOCATE 1: CLOSE
	SHELL "AC.COM Vendors.CHX Vendors /L8 /+2,1 /+1,1 /+6,1 /+5,1 /+4,1 /+3,1 /Q"
	KILL "Vendors.CHX"
	NAME "Vendors" AS "Vendors.CHX"
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
SUB apCkVoid ()
	GET #2, VAL(Chk.Ven), Ven
	Ven.Mpd = Ven.Mpd - VAL(Chk.Amt) * 100
	Ven.Ypd = Ven.Ypd - VAL(Chk.Amt) * 100
	PUT #2, VAL(Chk.Ven), Ven '---------------------[post vendor]------------
	FOR x = 0 TO 11
		IF Chk.Inv(x) > 0 THEN
		   In = FindInv(Chk.Inv(x))
		   IF In = 0 THEN WinMssg "Invoice not found": ERROR 200
		   Inv.Chk = "*Not Paid*"
		   Inv.Pdt = "**-**-****"
		   PUT #7, In, Inv '------------------------[post invoice]-----------
		   FOR y = 0 TO 24
			   IF Inv.Act(y) > " 0" THEN
				  GET #1, 1, Acc
				  Acc.Mcr(0) = Acc.Mcr(0) - VAL(Inv.Amt(y)) * 100
				  Acc.Ycr(0) = Acc.Ycr(0) - VAL(Inv.Amt(y)) * 100
				  Acc.Mcr(VAL(Inv.Sto(y))) = Acc.Mcr(VAL(Inv.Sto(y))) - VAL(Inv.Amt(y)) * 100
				  Acc.Ycr(VAL(Inv.Sto(y))) = Acc.Ycr(VAL(Inv.Sto(y))) - VAL(Inv.Amt(y)) * 100
				  PUT #1, 1, Acc '------------------[credit store Cash-01]---

				  GET #1, VAL(Inv.Act(y)), Acc
				  GET #1, VAL(Acc.Crd), Acc
				  Acc.Mdb(0) = Acc.Mdb(0) - VAL(Inv.Amt(y)) * 100
				  Acc.Ydb(0) = Acc.Ydb(0) - VAL(Inv.Amt(y)) * 100
				  Acc.Mdb(VAL(Inv.Sto(y))) = Acc.Mdb(VAL(Inv.Sto(y))) - VAL(Inv.Amt(y)) * 100
				  Acc.Ydb(VAL(Inv.Sto(y))) = Acc.Ydb(VAL(Inv.Sto(y))) - VAL(Inv.Amt(y)) * 100
				  PUT #1, LOC(1), Acc '-------------[debit store payable]----
			   ELSE
				  EXIT FOR
			   END IF
		   NEXT
		END IF
	NEXT
	Chk.Amt = "    0.00"
	Chk.Sta = "VOID"
	PUT #5, Cn, Chk '-------------------------------[save check master]------
END SUB

DEFINT L, T
SUB apCkWind ()
	apCkFind f, t
	IF f = 0 THEN WinMssg "No checks for this vendor": EXIT SUB
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "pr-ChkList", e)
	r = 20: b = f: n = f: GOSUB m2
m1: GET #6, n, Chx: GET #5, Chx.Num, Chk
	COLOR 11, 7: LOCATE r, 23: PRINT USING "&≥ & ≥ & "; Chk.Dat; Chk.Num; Chk.Amt;
	KeyPres f$
	COLOR 27, 1: LOCATE r, 23: PRINT USING "&≥ & ≥ & "; Chk.Dat; Chk.Num; Chk.Amt;
	SELECT CASE f$
	CASE CHR$(0) + "P"
		 IF n + 1 > t GOTO m1 ELSE n = n + 1
		 IF r + 1 > 34 THEN CALL Scroll(23, 20, 54, 34, 1) ELSE r = r + 1
	CASE CHR$(0) + "H"
		 IF n - 1 < f GOTO m1 ELSE n = n - 1
		 IF r - 1 < 20 THEN CALL BkScroll(23, 20, 54, 34, 1) ELSE r = r - 1
	CASE CHR$(13)
		 apCkShow
		 KeyPres f$
		 PCOPY 2, 0
	CASE ELSE
		 PCOPY 1, 0
		 EXIT SUB
	END SELECT
	GOTO m1

m2: COLOR 27, 1: LOCATE 20
	FOR x = b TO t
	GET #6, x, Chx: GET #5, Chx.Num, Chk
	LOCATE , 23: PRINT USING "&≥ & ≥ & "; Chk.Dat; Chk.Num; Chk.Amt
	IF CSRLIN > 34 THEN EXIT FOR
	NEXT
	RETURN
END SUB

DEFLNG L
DEFDBL T
SUB apRepost ()
	AP_Files
	FOR x = 1 TO LOF(2) \ LEN(Ven)
		GET #2, x, Ven
		Ven.Ybb = 0
		Ven.Ygr = 0
		Ven.Yds = 0
		Ven.Ycr = 0
		Ven.Ypd = 0
		PUT #2, x, Ven
	NEXT
	
	FOR x = 1 TO LOF(7) \ LEN(Inv)
	GET #7, x, Inv
	GET #2, VAL(Inv.Ven), Ven
	b& = VAL(RIGHT$(Inv.Bdt, 4) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2))
	
	IF b& > 19990630 THEN
	   Ven.Ygr = Ven.Ygr + VAL(Inv.Grs) * 100
	   Ven.Yds = Ven.Yds + VAL(Inv.Dis) * 100
	   Ven.Ycr = Ven.Ycr + VAL(Inv.Crd) * 100
	   PUT #2, VAL(Inv.Ven), Ven
	END IF

	IF Inv.Chk = "*Not Paid*" THEN
	   'do nothing
	ELSE
	   p& = VAL(RIGHT$(Inv.Pdt, 4) + LEFT$(Inv.Pdt, 2) + MID$(Inv.Pdt, 4, 2))
	   IF p& > 19990630 THEN
		  Ven.Ypd = Ven.Ypd + 100 * (VAL(Inv.Grs) - VAL(Inv.Dis) - VAL(Inv.Crd))
		  PUT #2, VAL(Inv.Ven), Ven
	   END IF
	END IF
	NEXT
END SUB

DEFSNG A-Z
SUB AP_Files ()
	CLOSE #1:  OPEN "Accounts.MAS" FOR RANDOM SHARED AS #1 LEN = LEN(Acc)

	CLOSE #2:  OPEN "Vendors .MAS" FOR RANDOM SHARED AS #2 LEN = LEN(Ven)
	CLOSE #3:  OPEN "Vendors .INX" FOR RANDOM SHARED AS #3 LEN = LEN(Vnx)
	CLOSE #4:  OPEN "Vendors .NAM" FOR RANDOM SHARED AS #4 LEN = LEN(Vnx)
	CLOSE #5:  OPEN "Vendors .CHK" FOR RANDOM SHARED AS #5 LEN = LEN(Chk)
	CLOSE #6:  OPEN "Vendors .CHX" FOR RANDOM SHARED AS #6 LEN = LEN(Chx)

	CLOSE #7:  OPEN "Invoices.MAS" FOR RANDOM SHARED AS #7 LEN = LEN(Inv)
	CLOSE #8:  OPEN "Invoices.INX" FOR RANDOM SHARED AS #8 LEN = LEN(Inx)
	CLOSE #9:  OPEN "Invoices.VEN" FOR RANDOM SHARED AS #9 LEN = LEN(Inx)
	CLOSE #10: OPEN "Invoices.DUE" FOR RANDOM SHARED AS #10 LEN = LEN(Inx)
   
	CLOSE #11: OPEN "LPT1:" FOR OUTPUT AS #11
	CLOSE #12: OPEN Path + "Stores.MAS" FOR RANDOM ACCESS READ SHARED AS #12 LEN = LEN(Sto)
END SUB

DEFINT A-S, U-Z
DEFDBL T
FUNCTION ChkTot ()
	T0 = 0: FOR x = 0 TO 12: T0 = T0 + VAL(InA(x)): NEXT
	SELECT CASE T0
		   CASE IS > 99999.99: ChkTot = 1: WinMssg "Check amount exceeds 99999.99"
		   CASE IS = 0:        ChkTot = 1: WinMssg "Check amount is zero"
		   CASE IS < -9999.99: ChkTot = 1: WinMssg "Check amount less than -9999.99"
	END SELECT
	COLOR 15, 7: LOCATE 41, 61: PRINT USING "#####.##"; T0;
	COLOR 0, 7:  LOCATE 14, 60: PRINT USING "#####.##"; T0;
	Picture 14, 60, 8, Chk.Amt
	NumWords Chk.Amt, w$
	qPrint SPACE$(63), 16, 6, 0, 1
	qPrint w$, 16, 6, 0, 1
END FUNCTION

DEFINT T
FUNCTION FindInv (n&)
   b = 1: t = LOF(7) / LEN(Inv)
   DO UNTIL (t < b)
	  m = t / 2 + b / 2: GET #7, m, Inv
	  IF n& = Inv.Num THEN FindInv = m: EXIT DO
	  IF n& > Inv.Num THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

DEFSNG A-Z
SUB HAccount (s$)
	Heading s$
	PRINT #11, TAB(71); "ACCOUNT  ACTIVITY"
	PRINT #11, : PRINT #11, : PRINT #11,
	PRINT #11, "  Inv   Ven                                             Invoice       Due         Date       Check                                              "
	PRINT #11, "  Num   Num       Vendor                   Reference      Date        Date        Paid       Number    No         Account Name           Amount "
	PRINT #11, " =====  ===  ============================  ==========  ==========  ==========  ==========  ==========  ==  ===========================  ========"
END SUB

SUB HAging (s$)
	 Heading s$
	 PRINT #11, TAB(73); "AGING OF PAYABLES"
	 PRINT #11,
	 PRINT #11, "                                   Due     Invoice  _________________P A S T   D U E________________    Due     _______________D U E   L A T E R________________"
	 PRINT #11, "      Vendor Name                  Date     Number  >60 Days  >45 Days  >30 Days  >15 Days   >1 Day    Today     >1 Day   >15 Days  >30 Days  >45 Days  >60 Days"
	 PRINT #11, "=============================== ========== ======== ========  ========  ========  ========  ========  ========  ========  ========  ========  ========  ========"
END SUB

SUB HChecks (s$)
	Heading s$
	PRINT #11, TAB(73); "PAYABLES  CHECKS"
	PRINT #11,
	PRINT #11, "                                   Check    Check              Invoice                                                 ______________DISTRIBUTION_______________"
	PRINT #11, "      Vendor                       Number   Amount  Check Date Number   Due Date   Gross   Discount  Credit     Net    St Ac  Account Name               Amount "
	PRINT #11, "================================= ======== ======== ========== ======= ========== ======== ======== ======== ========= ========================================="
END SUB

SUB HInvList (s$)
	Heading s$
	PRINT #11, TAB(71); "PAYABLE INVOICES"
	PRINT #11,
	PRINT #11, "                                    Invoice                Invoice       Due         Date       Check                                                   "
	PRINT #11, "              Vendor                 Number   Reference      Date        Date        Paid       Number       Gross     Discount     Credit    Net Amount"
	PRINT #11, "==================================  ========  ==========  ==========  ==========  ==========  ==========   =========  ==========  ==========  =========="
END SUB

SUB HInvoice (s$)
	Heading s$
	PRINT #11, TAB(73); "INVOICE REGISTER"
	PRINT #11,
	PRINT #11, " Invoice                                    Invoice      Date      Check                                          _______________DISTRIBUTION______________"
	PRINT #11, " Number                Vendor                 Date       Paid      Number     Gross  Discount  Credit  Net Amount St Ac    Account Description      Amount "
	PRINT #11, "======== ================================= ========== ========== ========== ======== ======== ======== ========== ========================================="
END SUB

SUB HRegister (s$)
	 Heading s$
	 PRINT #11, TAB(73); "CHECK  REGISTER"
	 PRINT #11,
	 PRINT #11, "Vend                              Invoice              Invoice      Due       Check                                         Net       Check                 "
	 PRINT #11, " No          Vendor Name           Number  Reference     Date       Date      Number      Gross    Discount    Credit      Amount     Amount   Check Number "
	 PRINT #11, "==== ============================ ======== ========== ========== ========== ==========  =========  =========  =========  =========  =========  ============="
END SUB

SUB HRequir (s$)
	 Heading s$
	 PRINT #11, TAB(69); "CASH REQUIREMENTS REPORT"
	 PRINT #11,
	 PRINT #11, "                                     Invoice               Invoice                                                   Net        Total              "
	 PRINT #11, "        Vendor Name                  Number   Reference      Date      Due Date     Gross    Discount    Credit     Amount     To Date    Due Date "
	 PRINT #11, "==================================  ========  ==========  ==========  ==========  =========  =========  =========  =========  =========  =========="
END SUB

SUB HStoreInv (s$)
	Heading s$
	PRINT #11, TAB(73); "PAYABLE INVOICES"
	PRINT #11,
	PRINT #11, "                                                                                      Invoice     Check"
	PRINT #11, "            Store                           Vendor               Invoice  Reference     Date      Number              Account                 Amount   "
	PRINT #11, "============================= ================================= ========= ========== ========== ========== ============================== ============="
END SUB

SUB HVendor (s$)
	 Heading s$
	 PRINT #11, TAB(69); " VENDORS ACCOUNT SUMMARY"
	 PRINT #11,
	 PRINT #11, "                                  ______________________M  O  N  T  H_______________________     __________________________Y  E  A  R___________________________"
	 PRINT #11, "       Vendor Name                B. Forward    Gross     Deductions   Payments    Balance       B. Forward      Gross     Deductions    Payments      Balance  "
	 PRINT #11, "================================= ==========  ==========  ==========  ==========  ==========     ===========  ===========  ===========  ===========  ==========="
END SUB

SUB HVenList (s$)
	 Heading s$
	 PRINT #11, TAB(72); " VENDORS LIST "
	 PRINT #11,
	 PRINT #11, "   Num        Vendor Name                Terms      Telephone    Address                        Street                         City             ST   Zip       "
	 PRINT #11, "   ===   ============================   =======   ============   ============================   ============================   ==============   ==   =========="
END SUB

DEFINT A-S, U-Z
DEFDBL T
FUNCTION InvDis ()
	FOR y = 0 TO 24
		t = t + VAL(Inv.Amt(y))
	NEXT
	IF t > 99999.99 OR t < -9999.99 THEN
	   InvDis = 1: WinMssg "Distribution exceeds 99999.99"
	ELSE
	   LOCATE 40, 70: PRINT USING "#####.##"; t;
	END IF
END FUNCTION

DEFINT T
SUB InvDisp (n)
	GET #7, n, Inv
	GET #2, VAL(Inv.Ven), Ven
	
	LOCATE 3, 38: PRINT USING "########"; Inv.Num;
	qPrint Ven.Num, 5, 42, 0, 1:              oVe = VAL(Inv.Ven)
	qPrint Ven.Nam, 3, 48, 0, 1
	qPrint Ven.Add, 4, 48, 0, 1
	qPrint Ven.Cty + " " + Ven.Sta + " " + Ven.Zip, 5, 48, 0, 1

	qPrint Inv.Ref, 7, 48, 0, 1
	qPrint Inv.Bdt, 8, 48, 0, 1
	qPrint Inv.Ddt, 9, 48, 0, 1
	qPrint Inv.Pdt, 10, 48, 0, 1
	qPrint Inv.Chk, 11, 48, 0, 1

	qPrint Inv.Grs, 7, 70, 0, 1:               oGr = Inv.Grs
	qPrint Inv.Dis, 8, 70, 0, 1:               oDi = Inv.Dis
	qPrint Inv.Crd, 9, 70, 0, 1:               oCr = Inv.Crd
	qPrint Inv.Net, 11, 70, 0, 1:              oNe = Inv.Net

	FOR r = 0 TO 24
	   IF Inv.Act(r) > " 0" THEN
		  GET #1, VAL(Inv.Act(r)), Acc
		  qPrint Inv.Sto(r), r + 14, 29, 0, 1: oSt(r) = Inv.Sto(r)
		  qPrint Inv.Act(r), r + 14, 34, 0, 1: oAc(r) = Inv.Act(r)
		  qPrint Acc.Nam, r + 14, 39, 0, 1
		  qPrint Inv.Amt(r), r + 14, 70, 0, 1: oAm(r) = Inv.Amt(r)
	   END IF
	NEXT
	qPrint Inv.Net, 40, 70, 0, 1
END SUB

DEFLNG L
DEFDBL T
SUB InvEnt (e)
	LOCATE 3, 38: PRINT USING "########"; Inv.Num;
100 EntryN 5, 42, 4, 0, e, b, Inv.Ven: IF e THEN EXIT SUB ELSE IF b THEN 100
	IF Inv.Ven < "   1" THEN WinVen Inv.Ven: IF Inv.Ven < "   1" THEN GOTO 100
	GET #2, VAL(Inv.Ven), Ven: IF Ven.Nam < "!" THEN WinVen Inv.Ven: GOTO 100
	Inv.Ven = Ven.Num
	qPrint Ven.Num, 5, 42, 0, 1
	qPrint Ven.Nam, 3, 48, 0, 1
	qPrint Ven.Add, 4, 48, 0, 1
	qPrint Ven.Cty + " " + Ven.Sta + " " + Ven.Zip, 5, 48, 0, 1
101 EntryS 7, 48, 10, e, b, Inv.Ref:   IF e THEN EXIT SUB ELSE IF b THEN 100
102 EntryD 8, 48, e, b, Inv.Bdt:       IF e THEN EXIT SUB ELSE IF b THEN 101
	IF Inv.Bdt < "0" THEN Inv.Bdt = DATE$: GOTO 102
	IF Inv.Ddt < "0" THEN Inv.Ddt = DayMath$(Inv.Bdt, VAL(Ven.Trm))
103 EntryD 9, 48, e, b, Inv.Ddt:       IF e THEN EXIT SUB ELSE IF b THEN 102
	IF Inv.Ddt < "0" THEN Inv.Ddt = DayMath$(Inv.Bdt, VAL(Ven.Trm)): GOTO 103
	IF DateDif(Inv.Bdt, Inv.Ddt) < 0 THEN WinMssg "Due before invoice date!": GOTO 103
	qPrint Inv.Pdt, 10, 48, 0, 1
	qPrint Inv.Chk, 11, 48, 0, 1
104 EntryN 7, 70, 8, 2, e, b, Inv.Grs: IF e THEN EXIT SUB
	IF InvNet THEN 104 ELSE IF b THEN 103
105 EntryN 8, 70, 8, 2, e, b, Inv.Dis: IF e THEN EXIT SUB
	IF InvNet THEN 105 ELSE IF b THEN 104
106 EntryN 9, 70, 8, 2, e, b, Inv.Crd: IF e THEN EXIT SUB
	IF InvNet THEN 106 ELSE IF b THEN 105

	r = 0
107 EntryN r + 14, 29, 2, 0, e, b, Inv.Sto(r): IF e THEN EXIT SUB
	IF b THEN r = r - 1: IF r < 0 THEN 106 ELSE 109
	IF Inv.Sto(r) = "  " THEN
	   FOR x = r TO 24
		   Inv.Sto(x) = Inv.Sto(x + 1)
		   Inv.Act(x) = Inv.Act(x + 1)
		   Inv.Amt(x) = Inv.Amt(x + 1)
	   NEXT
	   IF r < 25 THEN CALL Scroll(29, r + 14, 77, 38, 1)
	   COLOR 11, 1: LOCATE 38, 29: PRINT "   ≥    ≥                              ≥         ";
	   y = InvDis
	   IF SCREEN(r + 14, 29) = 32 THEN 110 ELSE 107
	END IF
	IF Inv.Sto(r) < " 1" THEN IF WinStor THEN 107 ELSE Inv.Sto(r) = Sto.Num
	GET #12, VAL(Inv.Sto(r)), Sto
	IF Sto.Nam < "!" OR ComCode <> Sto.Cod THEN WinMssg "Not " + ComCode + " Store": GOTO 107
	Inv.Sto(r) = Sto.Num
	qPrint Inv.Sto(r), r + 14, 29, 0, 1
108 EntryN r + 14, 34, 2, 0, e, b, Inv.Act(r): IF e THEN EXIT SUB ELSE IF b THEN 107
	IF Inv.Act(r) < " 1" THEN IF WinAcct THEN 108 ELSE Inv.Act(r) = Acc.Num
	GET #1, VAL(Inv.Act(r)), Acc
	IF Acc.Crd = "00" THEN WinMssg "Sorry, reserved account.": GOTO 108
	Inv.Act(r) = Acc.Num
	qPrint Acc.Num, r + 14, 34, 0, 1
	qPrint Acc.Nam, r + 14, 39, 0, 1
109 EntryN r + 14, 70, 8, 2, e, b, Inv.Amt(r): IF e THEN EXIT SUB ELSE IF b THEN 108
	IF InvDis THEN 109 ELSE IF b THEN 108
	r = r + 1: IF r < 25 THEN 107 ELSE r = 24
110 Picture 40, 70, 8, f$
	IF f$ <> Inv.Net THEN 107
	IF Confirm("SAVE Invoice") THEN InvPost ELSE 107
END SUB

DEFINT L, T
SUB InvFind (n$, f, e)
   'find range of invoices for a vendor
   'n$ is vendor num, f is starting invoice, e is last invoice
	f = 0: e = 0: b = 1: t = LOF(9) / LEN(Inx)
	DO UNTIL (t < b)
	   m = t / 2 + b / 2: GET #9, m, Inx
	   IF VAL(n$) > Inx.Ven THEN b = m + 1 ELSE t = m - 1
	LOOP
	DO WHILE VAL(n$) > Inx.Ven AND NOT EOF(9)
	   GET #9, , Inx
	LOOP
	IF VAL(n$) = Inx.Ven THEN
	   f = LOC(9): e = f
	   DO WHILE NOT EOF(9)
		  GET #9, , Inx
		  IF VAL(n$) = Inx.Ven THEN e = LOC(9) ELSE EXIT DO
	   LOOP
	END IF
END SUB

DEFDBL T
SUB InvLong (f$, t$)
	StatLin "Detail Invoice Listing"
	f& = VAL(RIGHT$(f$, 4) + LEFT$(f$, 2) + MID$(f$, 4, 2)) 'from date
	t& = VAL(RIGHT$(t$, 4) + LEFT$(t$, 2) + MID$(t$, 4, 2)) 'to date
	PageNum = 0: HInvoice "00"

	FOR x = 1 TO LOF(4) \ LEN(Vnx)
		IF INKEY$ = CHR$(27) THEN PRINT #11, CHR$(12); : EXIT SUB
		GET #4, x, Vnx
		GET #2, Vnx.Num, Ven
		InvFind Ven.Num, b, e
		IF b THEN
		   FOR y = b TO e
			   GET #9, y, Inx
			   GET #7, Inx.Num, Inv
			   d& = VAL(RIGHT$(Inv.Bdt, 4) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2))
			   IF d& >= f& AND d& <= t& THEN
				  PRINT #11, USING "########"; Inx.Inv;
				  PRINT #11, USING " &"; Ven.Num; Ven.Nam; Inv.Bdt; Inv.Pdt; Inv.Chk;
				  PRINT #11, USING " &"; Inv.Grs; Inv.Dis; Inv.Crd;
				  PRINT #11, "   "; Inv.Net;
				  T1 = T1 + VAL(Inv.Grs)
				  T2 = T2 + VAL(Inv.Dis)
				  T3 = T3 + VAL(Inv.Crd)
				  T4 = T4 + VAL(Inv.Net)
				  FOR a = 0 TO 24
					  IF Inv.Act(a) > " 0" THEN
						 GET #1, VAL(Inv.Act(a)), Acc
						 PRINT #11, TAB(115);
						 PRINT #11, USING "& "; Inv.Sto(a); Inv.Act(a);
						 PRINT #11, Acc.Nam; Inv.Amt(a)
						 T5 = T5 + VAL(Inv.Amt(a))
						 LineNum = LineNum + 1: IF LineNum > 58 THEN PRINT #11, CHR$(12); : HInvoice "00"
					  ELSE
						 EXIT FOR
					  END IF
				  NEXT
			   END IF
		   NEXT
		END IF
	NEXT
	PRINT #11,
	PRINT #11, TAB(76); : PRINT #11, USING "######.##"; T1; T2; T3;
	PRINT #11, USING "########.##"; T4;
	PRINT #11, TAB(146); : PRINT #11, USING "#######.##"; T5
END SUB

FUNCTION InvNet ()
	LOCATE 11, 70
	t = VAL(Inv.Grs) - VAL(Inv.Dis) - VAL(Inv.Crd)
	IF t > 99999.99 OR t < -9999.99 THEN
	   InvNet = 1: WinMssg "Net Amount exceeds -9999.99"
	ELSE
	   PRINT USING "#####.##"; t;
	   Picture 11, 70, 8, Inv.Net
	END IF
END FUNCTION

DEFINT T
DEFLNG L
FUNCTION InvOpen$ ()
	REDIM OpInv(100)
	InvFind Ven.Num, s, f
	IF s THEN
	   FOR x = s TO f
		   GET #9, x, Inx
		   GET #7, Inx.Num, Inv
		   IF Inv.Chk = "*Not Paid*" THEN n = n + 1: OpInv(n) = Inx.Num
	   NEXT
	   IF n > 0 THEN
		  PCOPY 0, 1
		  b = 1: t = n: r = 6: n = 1: GOSUB o2
o1:       GET #7, OpInv(n), Inv
		  COLOR 0, 7: LOCATE r, 3: PRINT USING "########≥&≥&≥&"; Inv.Num; Inv.Ref; Inv.Bdt; Inv.Net;
		  KeyPres n$
		  COLOR 15, 4: LOCATE r, 3: PRINT USING "########≥&≥&≥&"; Inv.Num; Inv.Ref; Inv.Bdt; Inv.Net;
		  SELECT CASE n$
				 CASE CHR$(13)
					  Picture r, 3, 8, f$
					  InvOpen$ = f$
					  PCOPY 1, 0
					  EXIT FUNCTION
				 CASE CHR$(0) + "P" 'Dn
					  IF n + 1 > t GOTO o1 ELSE n = n + 1
					  IF r + 1 > 20 THEN CALL Scroll(3, 6, 43, 20, 1) ELSE r = r + 1
				 CASE CHR$(0) + "H" 'Up
					  IF n - 1 < 1 GOTO o1 ELSE n = n - 1
					  IF r - 1 < 6 THEN CALL BkScroll(3, 6, 43, 20, 1) ELSE r = r - 1
				 CASE ELSE
					  PCOPY 1, 0
					  EXIT FUNCTION
		  END SELECT
		  GOTO o1
	   ELSE
		  WinMssg "No Open Invoices"
	   END IF
	   EXIT FUNCTION
	ELSE
	   WinMssg "No Open Invoices"
	END IF
o2: COLOR 15, 4: LOCATE 5, 1: PRINT " …ÕNUMBERÕ—REFERENCEÕ—ÕDUE DATEÕ—ÕAMOUNTÕª "
	FOR x = b TO t: GET #7, OpInv(x), Inv
	PRINT USING " ∫########≥&≥&≥&∫ "; Inv.Num; Inv.Ref; Inv.Bdt; Inv.Net
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	PRINT " »ÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕº "
	RETURN
END FUNCTION

DEFINT L
SUB InvPost ()
	Sort = 1
	FOR x = 0 TO 24
		IF Inv.Act(x) > "00" THEN
		   GET #1, VAL(Inv.Act(x)), Acc
		   Acc.Mdb(VAL(Inv.Sto(x))) = Acc.Mdb(VAL(Inv.Sto(x))) + VAL(Inv.Amt(x)) * 100
		   Acc.Ydb(VAL(Inv.Sto(x))) = Acc.Ydb(VAL(Inv.Sto(x))) + VAL(Inv.Amt(x)) * 100
		   PUT #1, VAL(Inv.Act(x)), Acc '--------------Debit Account-------
		   GET #1, VAL(Acc.Crd), Acc
		   Acc.Mcr(VAL(Inv.Sto(x))) = Acc.Mcr(VAL(Inv.Sto(x))) + VAL(Inv.Amt(x)) * 100
		   Acc.Ycr(VAL(Inv.Sto(x))) = Acc.Ycr(VAL(Inv.Sto(x))) + VAL(Inv.Amt(x)) * 100
		   PUT #1, LOC(1), Acc          '--------------Credit Payable------
		ELSE EXIT FOR
		END IF
	NEXT
	PUT #7, In, Inv                     '--------------Save Invoice--------
	Inx.Inv = Inv.Num
	Inx.Ven = VAL(Ven.Num)
	Inx.Ddt = VAL(RIGHT$(Inv.Ddt, 4) + LEFT$(Inv.Ddt, 2) + MID$(Inv.Ddt, 4, 2))
	Inx.Num = In
	PUT #8, Ix, Inx                     '--------------Save Index----------

	Ven.Mgr = Ven.Mgr + VAL(Inv.Grs) * 100
	Ven.Mds = Ven.Mds + VAL(Inv.Dis) * 100
	Ven.Mcr = Ven.Mcr + VAL(Inv.Crd) * 100
	Ven.Ygr = Ven.Ygr + VAL(Inv.Grs) * 100
	Ven.Yds = Ven.Yds + VAL(Inv.Dis) * 100
	Ven.Ycr = Ven.Ycr + VAL(Inv.Crd) * 100
	PUT #2, VAL(Ven.Num), Ven           '--------------Update Vendor-------

	IF oVe > 0 THEN     '::::::::::::::::::::::::::::::::::::::::::::::::::
	   FOR x = 0 TO 24
		   IF oAc(x) > "00" THEN
			  GET #1, VAL(oAc(x)), Acc
			  Acc.Mdb(VAL(oSt(x))) = Acc.Mdb(VAL(oSt(x))) - VAL(oAm(x)) * 100
			  Acc.Ydb(VAL(oSt(x))) = Acc.Ydb(VAL(oSt(x))) - VAL(oAm(x)) * 100
			  PUT #1, VAL(oAc(x)), Acc  '--------------Reverse Acc.Debit---
			  GET #1, VAL(Acc.Crd), Acc
			  Acc.Mcr(VAL(oSt(x))) = Acc.Mcr(VAL(oSt(x))) - VAL(oAm(x)) * 100
			  Acc.Ycr(VAL(oSt(x))) = Acc.Ycr(VAL(oSt(x))) - VAL(oAm(x)) * 100
			  PUT #1, LOC(1), Acc       '--------------Reverse Acc.Credit--
		   ELSE EXIT FOR
		   END IF
	   NEXT
	   GET #2, oVe, Ven
	   Ven.Mgr = Ven.Mgr - VAL(oGr) * 100
	   Ven.Mds = Ven.Mds - VAL(oDi) * 100
	   Ven.Mcr = Ven.Mcr - VAL(oCr) * 100
	   Ven.Ygr = Ven.Ygr - VAL(oGr) * 100
	   Ven.Yds = Ven.Yds - VAL(oDi) * 100
	   Ven.Ycr = Ven.Ycr - VAL(oCr) * 100
	   PUT #2, oVe, Ven                 '--------------Reverse Vendor------
	END IF
END SUB

DEFDBL T
SUB InvShor (f$, t$)
	StatLin "Summary Invoice Listing"
	f& = VAL(RIGHT$(f$, 4) + LEFT$(f$, 2) + MID$(f$, 4, 2)) 'from date
	t& = VAL(RIGHT$(t$, 4) + LEFT$(t$, 2) + MID$(t$, 4, 2)) 'to date
	PageNum = 0: HInvList "00"
	
	FOR x = 1 TO LOF(4) / LEN(Vnx)
		IF INKEY$ = CHR$(27) THEN PRINT #11, CHR$(12); : EXIT SUB
		GET #4, x, Vnx
		GET #2, Vnx.Num, Ven
		InvFind Ven.Num, b, e
		IF b THEN
		   FOR y = b TO e
			   GET #9, y, Inx
			   GET #7, Inx.Num, Inv
			   d& = VAL(RIGHT$(Inv.Bdt, 4) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2))
			   IF d& >= f& AND d& <= t& THEN
				  LineNum = LineNum + 1: IF LineNum > 58 THEN PRINT #11, CHR$(12); : HInvList "00"
				  PRINT #11, USING "&  "; Ven.Num; Ven.Nam;
				  PRINT #11, USING "########  "; Inx.Inv;
				  PRINT #11, USING "&  "; Inv.Ref; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk;
				  PRINT #11, USING "  &  "; Inv.Grs; Inv.Dis; Inv.Crd; Inv.Net
				  T1 = T1 + VAL(Inv.Grs)
				  T2 = T2 + VAL(Inv.Dis)
				  T3 = T3 + VAL(Inv.Crd)
				  T4 = T4 + VAL(Inv.Net)
			   END IF
		   NEXT
		END IF
	NEXT
	PRINT #11, : PRINT #11, TAB(105); : PRINT #11, USING " ########.##"; T1; T2; T3; T4
END SUB

DEFSNG A-Z
SUB InvSort ()
	CLOSE : LOCATE 1
	SHELL "AC.COM Invoices.INX Invoices.VEN /L12 /+6,1 /+5,1 /+10,1 /+9,1 /+8,1 /+7,1 /Q"
	AP_Files
	Sort = 0
	ErrSw(1) = 2
END SUB

SUB mapAging ()
	IF Confirm("Print Aging Report") THEN Aging
END SUB

SUB mapChkDet ()
	IF RptDate(f$, t$) THEN EXIT SUB
	IF Confirm("Print Check Listing") THEN PrtChk f$, t$
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
SUB mapChkMan ()
	StatLin "Manual Payable Check"
261 CALL MhDisplayByName(0, 0, "AP-Check", e)
	ERASE InN, InA
	GET #5, 1 + LOF(5) \ LEN(Chk), Chk
	qPrint Company, 8, 6, 0, 1
	qPrint Street, 9, 6, 0, 1
	qPrint Address, 10, 6, 0, 1
	qPrint City, 11, 6, 0, 1
	Chk.Dat = DATE$
262 EntryU 8, 60, 8, e, b, Chk.Num:     IF e THEN EXIT SUB ELSE IF b THEN 262
	IF Chk.Num < "0" THEN WinMssg "Check number please": GOTO 262
	Chk.Num = RIGHT$(STR$(100000000 + VAL(Chk.Num)), 8)
	qPrint Chk.Num, 8, 60, 0, 1
263 EntryD 14, 43, e, b, Chk.Dat:       IF e THEN 261 ELSE IF b THEN 262
	IF Chk.Dat < "0" THEN WinMssg "Date please": GOTO 263
264 EntryN 18, 13, 4, 0, e, b, Chk.Ven: IF e THEN 261 ELSE IF b THEN 263
	IF Chk.Ven < "   1" THEN WinVen Chk.Ven: IF Chk.Ven < "   1" THEN GOTO 264
	GET #2, VAL(Chk.Ven), Ven: IF Ven.Nam < "!" THEN WinVen Chk.Ven: GOTO 264
	Chk.Ven = Ven.Num
	qPrint Ven.Num, 18, 13, 0, 1
	qPrint Ven.Nam, 18, 17, 0, 1
	qPrint Ven.Add, 19, 17, 0, 1
	qPrint Ven.Cty + " " + Ven.Sta + " " + Ven.Zip, 20, 17, 0, 1
	
	r = 0
265 EntryN r + 28, 5, 8, 0, e, b, InN(r): IF e THEN 261
	IF b THEN r = r - 1: IF r < 0 THEN 264 ELSE 265
	IF InN(r) = "        " THEN
	   FOR x = r TO 11
		   InN(x) = InN(x + 1)
		   InA(x) = InA(x + 1)
	   NEXT
	   IF r < 11 THEN CALL Scroll(5, r + 28, 68, 39, 1)
	   COLOR 21, 15: LOCATE 39, 5: PRINT "         ≥           ≥          ≥          ≥          ≥         ";
	   IF ChkTot THEN 265
	   IF SCREEN(r + 28, 5) = 32 THEN 266 ELSE 265
	ELSE
	   IF FindInv(VAL(InN(r))) = 0 THEN InN(r) = InvOpen$()
	   IF VAL(InN(r)) = 0 THEN 265
	END IF
	FOR x = 0 TO 11
		IF x <> r THEN IF InN(r) = InN(x) THEN InN(r) = "": WinMssg "Duplicate Invoice": GOTO 265
	NEXT
	qPrint InN(r), r + 28, 5, 0, 1
	qPrint Inv.Ref, r + 28, 16, 0, 1
	qPrint Inv.Bdt, r + 28, 27, 0, 1
	qPrint Inv.Ddt, r + 28, 38, 0, 1
	qPrint Inv.Chk, r + 28, 49, 0, 1
	qPrint Inv.Net, r + 28, 61, 0, 1
	IF VAL(Inv.Ven) <> VAL(Chk.Ven) THEN WinMssg "Belongs to another vendor": GOTO 265
	IF Inv.Chk <> "*Not Paid*" THEN WinMssg "Paid Invoice": GOTO 265
	InA(r) = Inv.Net
	IF ChkTot THEN 265
	r = r + 1: IF r > 11 THEN r = 11 ELSE 265

266 IF Confirm("SAVE Payroll Check") THEN
	   FOR x = 0 TO 11: Chk.Inv(x) = VAL(InN(x)): NEXT
	   Cn = 1 + LOF(5) \ LEN(Chk)
	   Cx = Cn
	   apCkPost
	   apCkSort
	   AP_Files
	   IF Confirm("PRINT Payable Check") THEN apCkPrnt
	   GOTO 261
	END IF
	GOTO 265
END SUB

SUB mapChkMas ()
	CALL MhDisplayByName(0, 0, "ap-ChkMas", e)
	StatLin "Payables Check Master"

	SubMenu 1, "D"
	r = 3: b = 1: t = LOF(5) \ LEN(Chk): GOSUB d2
	IF t = 0 THEN WinMssg "Sorry no payable checks": EXIT SUB
d1: GET #5, n, Chk: GET #2, VAL(Chk.Ven), Ven
	COLOR 11, 7: LOCATE r, 2, 0: PRINT USING "& ≥ & ≥ & ≥ & ≥ & ≥ & "; Chk.Sta; Chk.Dat; Ven.Num; Ven.Nam; Chk.Num; Chk.Amt;
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
		COLOR 27, 1: LOCATE r, 2: PRINT USING "& ≥ & ≥ & ≥ & ≥ & ≥ & "; Chk.Sta; Chk.Dat; Ven.Num; Ven.Nam; Chk.Num; Chk.Amt;
		IF r + 1 > 47 THEN CALL Scroll(2, 3, 79, 47, 1) ELSE r = r + 1
	CASE CHR$(0) + "H"  '===============Up=====
		IF n - 1 < 1 GOTO d1 ELSE n = n - 1
		COLOR 27, 1: LOCATE r, 2: PRINT USING "& ≥ & ≥ & ≥ & ≥ & ≥ & "; Chk.Sta; Chk.Dat; Ven.Num; Ven.Nam; Chk.Num; Chk.Amt;
		IF r - 1 < 3 THEN CALL BkScroll(2, 3, 79, 47, 1) ELSE r = r - 1
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
		apCkShow
		KeyPres f$
		PCOPY 2, 0
	CASE "V"            '===============Void===
		SubMenu 0, n$
		apCkShow
		IF Chk.Sta = "VOID" THEN
		   WinMssg "Check is already Void"
		ELSE
		   IF Confirm("VOID Check") THEN Cn = n: apCkVoid
		END IF
		PCOPY 2, 0
	CASE "R"            '===============Reprint
		SubMenu 0, n$
		apCkShow
		IF Chk.Sta = "VOID" THEN
		   WinMssg "Void Check, no reprint"
		ELSE
d3:        EntryN 8, 60, 8, 0, e, b, Chk.Num: IF b GOTO d3
		   IF e = 0 THEN
			  IF Chk.Num < "       1" THEN WinMssg "Check number please": GOTO d3
			  Chk.Num = RIGHT$(STR$(100000000 + VAL(Chk.Num)), 8)
			  qPrint Chk.Num, 8, 60, 0, 1
			  IF Confirm("REPRINT Check") THEN apCkPrnt: PUT #5, n, Chk
		   END IF
		END IF
		PCOPY 2, 0
	CASE "S"            '===============Vendor
		SubMenu 0, n$
		PCOPY 0, 1: CALL MhDisplayByName(0, 0, "ap-VenData", e)
		Vnx.Num = VAL(Chk.Ven)
		VenShow
		KeyPres f$
		PCOPY 1, 0
	END SELECT
	GOTO d1

d2: COLOR 27, 1: LOCATE 3
	FOR x = b TO t
	GET #5, x, Chk: GET #2, VAL(Chk.Ven), Ven
	LOCATE , 2: PRINT USING "& ≥ & ≥ & ≥ & ≥ & ≥ & "; Chk.Sta; Chk.Dat; Ven.Num; Ven.Nam; Chk.Num; Chk.Amt
	IF CSRLIN > 47 THEN EXIT FOR
	NEXT
	n = b + r - 3
	RETURN
END SUB

DEFINT L, T
SUB mapChkPrt ()
	StatLin "Print Payable Checks"
	IF ErrSw(1) = 1 THEN WinMssg "Print check register first": EXIT SUB
	IF ErrSw(1) = 2 THEN WinMssg "Invoices changed, reprint register": EXIT SUB
	IF ErrSw(1) = 3 THEN WinMssg "Negative check, reprint register": EXIT SUB
	IF ErrSw(1) = 4 THEN WinMssg "Vendor changes, reprint register": EXIT SUB
	CLOSE #10: OPEN "Payable.REG" FOR INPUT AS #10
	WinMssg "Load Laser Checks in the printer"
	IF Confirm("Print Checks") = 0 THEN EXIT SUB
	CLOSE #10: OPEN "Payable.REG" FOR INPUT AS #10
	DO WHILE NOT EOF(10)
	   INPUT #10, Chk.Num, Chk.Ven, Chk.Dat, Chk.Inv(0), Chk.Inv(1), Chk.Inv(2), Chk.Inv(3), Chk.Inv(4), Chk.Inv(5), Chk.Inv(6), Chk.Inv(7), Chk.Inv(8), Chk.Inv(9), Chk.Inv(10), Chk.Inv(11), Chk.Amt
	   Cn = 1 + LOF(5) \ LEN(Chk)
	   Cx = Cn
	   apCkPrnt
	   apCkPost
	LOOP
	apCkSort
79  EXIT SUB
END SUB

DEFLNG L
DEFDBL T
SUB mapChkReg ()
	StatLin "Generate Check Register"
	CALL MhDisplayByName(0, 0, "AP-ChkNum", e)
	i = 0: f = 1
	ErrSw(1) = 0
410 EntryD 14, 49, e, b, d$:  IF e THEN EXIT SUB ELSE IF b THEN 410
411 EntryN 16, 49, 8, 0, e, b, c$: IF e THEN EXIT SUB ELSE IF b THEN 410
	LDue = VAL(RIGHT$(d$, 4) + LEFT$(d$, 2) + MID$(d$, 4, 2))
	LChk = VAL(c$)
	Chk.Dat = DATE$
	PageNum = 0: HRegister "00"
	CLOSE #10: OPEN "Payable.REG" FOR OUTPUT AS 10
	FOR x = 1 TO LOF(9) \ LEN(Inx)
		GET #9, x, Inx
		GET #7, Inx.Num, Inv
		IF Inv.Chk = "*Not Paid*" AND Inx.Ddt <= LDue THEN
		   GET #2, Inx.Ven, Ven
		   IF f THEN f = 0: GOSUB 414
		   IF p <> Inx.Ven OR i > 11 THEN GOSUB 412: GOSUB 414
		   IF T4 + VAL(Inv.Net) > 99999.99 THEN GOSUB 412: GOSUB 414
		   PRINT #11, TAB(35); : PRINT #11, USING "######## "; Inx.Inv;
		   PRINT #11, USING "& "; Inv.Ref; Inv.Bdt; Inv.Ddt; Inv.Chk;
		   PRINT #11, USING "  & "; Inv.Grs; Inv.Dis; Inv.Crd; Inv.Net
		   T1 = T1 + VAL(Inv.Grs)
		   T2 = T2 + VAL(Inv.Dis)
		   T3 = T3 + VAL(Inv.Crd)
		   T4 = T4 + VAL(Inv.Net)
		   PRINT #10, USING ",########"; Inx.Inv;
		   i = i + 1
		   LineNum = LineNum + 1: IF LineNum > 58 THEN PRINT #11, CHR$(12); : HRegister "00"
		END IF
	NEXT
	GOSUB 412: PRINT #11, TAB(87);
	PRINT #11, USING "#######,.##"; T5; T6; T7; T8
	PRINT #11, CHR$(12);
	IF ErrSw(1) = 3 THEN CLOSE #9: KILL "INVOICES.CCK"
	EXIT SUB
412 T5 = T5 + T1
	T6 = T6 + T2
	T7 = T7 + T3
	T8 = T8 + T4
	PRINT #11, CHR$(27); "G": PRINT #11, TAB(87);
	PRINT #11, USING "#######,.##"; T1; T2; T3; T4; T4;
	PRINT #11, "   "; c$
	PRINT #11, CHR$(27); "H"
	LineNum = LineNum + 3: IF LineNum > 58 THEN PRINT #11, CHR$(12); : HRegister "00"
	IF i > 0 THEN
	   FOR y = i TO 11
		   PRINT #10, USING ",########"; 0;
	   NEXT
	   PRINT #10, USING ",&#####.##&"; CHR$(34); T4; CHR$(34)
	END IF
	IF T4 < 0 THEN WinMssg "Check Nß " + c$ + " is negative": ErrSw(1) = 3
	LChk = LChk + 1
	c$ = "00000000" + LTRIM$(STR$(LChk))
	c$ = RIGHT$(c$, 8)
	RETURN
414 c$ = "00000000" + LTRIM$(STR$(LChk))
	c$ = RIGHT$(c$, 8)
	PRINT #10, USING "&&&,&&&,&&&&"; CHR$(34); c$; CHR$(34); CHR$(34); Inv.Ven; CHR$(34); CHR$(34); Chk.Dat; CHR$(34);
	T1 = 0
	T2 = 0
	T3 = 0
	T4 = 0
	i = 0
	p = Inx.Ven
	IF LineNum > 58 THEN PRINT #11, CHR$(12); : HRegister "00"
	PRINT #11, Ven.Num; " "; Ven.Nam;
	RETURN
END SUB

DEFSNG A-Z
SUB mapInvDet ()
	IF RptDate(f$, t$) THEN EXIT SUB
	IF Confirm("Print Detailes Invoices") THEN InvLong f$, t$
END SUB

DEFINT A-Z
SUB mapInvEdt ()
	StatLin "Edit Payable Invoice"
	f$ = "        "
	ERASE oSt, oAc, oAm
	CALL MhDisplayByName(0, 0, "AP-Invoice", e)
200 EntryN 3, 38, 8, 0, e, b, f$: IF e THEN EXIT SUB ELSE IF b THEN 200
	In = FindInv(VAL(f$))
	IF In = 0 THEN WinMssg "Invoice not found": GOTO 200
	Ix = In
	InvDisp In
	IF LEFT$(Inv.Chk, 1) = "C" OR LEFT$(Inv.Chk, 1) = "W" THEN WinMssg "Paid Invoice": GOTO 200
	InvEnt e
	IF e = 0 THEN mapInvEdt
	IF Sort THEN InvSort
END SUB

DEFLNG I
SUB mapInvNew ()
	StatLin "New Payable Invoice"
	CALL MhDisplayByName(0, 0, "AP-Invoice", e)
	ERASE oSt, oAc, oAm
	oVe = 0
	oGr = ""
	oDi = ""
	oCr = ""
	oNe = ""
	t = LOF(7) / LEN(Inv)
	IF t > 0 THEN GET #7, t, Inv
	InvNum = Inv.Num + 1
	GET #7, InvNum, Inv
	In = 1 + LOF(7) / LEN(Inv)
	Ix = In
	Inv.Num = InvNum
	Inv.Pdt = "**-**-****"
	Inv.Chk = "*Not Paid*"
	InvEnt e
	IF e = 0 THEN mapInvNew
	IF Sort THEN InvSort
END SUB

DEFSNG A-Z
SUB mapInvSum ()
	IF RptDate(f$, t$) THEN EXIT SUB
	IF Confirm("Print Summary Invoice") THEN InvShor f$, t$
END SUB

DEFINT A-Z
SUB mapPurge ()
	StatLin "Purge Payable Invoices"
	CALL MhDisplayByName(0, 0, "OP-Purge", e)
p1: EntryD 12, 43, e, b, d$: IF e THEN EXIT SUB ELSE IF b GOTO p1
	IF Confirm("PURGE Payables Invoices") = 0 THEN EXIT SUB
	d$ = RIGHT$(d$, 4) + LEFT$(d$, 2) + MID$(d$, 4, 2)
	CLOSE #2: OPEN "Invoices.M" FOR RANDOM SHARED AS #2 LEN = LEN(Inv)
	CLOSE #3: OPEN "Invoices.X" FOR RANDOM SHARED AS #3 LEN = LEN(Inx)
	Top = LOF(7) / LEN(Inv)
	FOR x = 1 TO Top
	   GET #7, x, Inv
	   IF INKEY$ = CHR$(27) THEN EXIT SUB
	   LOCATE 50, 40: PRINT USING "##### ###.##%"; n; (x / Top) * 100;
	   i$ = RIGHT$(Inv.Bdt, 4) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2)
	   IF i$ > d$ OR Inv.Chk = "*Not Paid*" THEN
		  n = n + 1
		  Inx.Inv = Inv.Num
		  Inx.Ven = VAL(Inv.Ven)
		  Inx.Ddt = VAL(RIGHT$(Inv.Ddt, 4) + LEFT$(Inv.Ddt, 2) + MID$(Inv.Ddt, 4, 2))
		  Inx.Num = n
		  PUT #2, n, Inv
		  PUT #3, n, Inx
	   END IF
	NEXT
	CLOSE
	KILL "Invoices.MAS": NAME "Invoices.M" AS "Invoices.MAS"
	KILL "Invoices.INX": NAME "Invoices.X" AS "Invoices.INX"
	InvSort

	AP_Files
	CLOSE #2: OPEN "Vendors.C" FOR RANDOM SHARED AS #2 LEN = LEN(Chk)
	CLOSE #3: OPEN "Vendors.X" FOR RANDOM SHARED AS #3 LEN = LEN(Chx)
	n = 0: Top = LOF(5) / LEN(Chk)
	FOR x = 1 TO Top
	   GET #5, x, Chk
	   IF INKEY$ = CHR$(27) THEN EXIT SUB
	   LOCATE 50, 40: PRINT USING "##### ###.##%"; n; (x / Top) * 100;
	   i$ = RIGHT$(Chk.Dat, 4) + LEFT$(Chk.Dat, 2) + MID$(Chk.Dat, 4, 2)
	   IF i$ > d$ THEN
		  n = n + 1
		  Chx.Ven = VAL(Chk.Ven)
		  Chx.Dat = VAL(i$)
		  Chx.Num = n
		  PUT #2, n, Chk
		  PUT #3, n, Chx
	   END IF
	NEXT
	CLOSE
	KILL "Vendors.CHK": NAME "Vendors.C" AS "Vendors.CHK"
	KILL "Vendors.CHX": NAME "Vendors.X" AS "Vendors.CHX"
	apCkSort
END SUB

DEFSNG A-Z
SUB mapRequir ()
	IF Confirm("Print Cash Requirements") THEN Require
END SUB

DEFINT A-S, U-Z
DEFDBL T
SUB mapSelInv ()
	StatLin "Print Select Vendor Invoices"
	CALL MhDisplayByName(0, 0, "AP-Limit", e)
	f$ = "          "
	t$ = "          "
	s$ = "00"
	v$ = "ALL "
11  EntryD 21, 43, e, b, f$: IF e THEN EXIT SUB ELSE IF b GOTO 11
	IF f$ < "!" GOTO 11
12  EntryD 22, 43, e, b, t$: IF e THEN EXIT SUB ELSE IF b GOTO 11
	IF t$ < "!" GOTO 12

13  EntryU 24, 43, 2, e, b, s$: IF e THEN EXIT SUB ELSE IF b GOTO 12
	IF s$ = "00" THEN
	   Sto.Nam = "ALL STORES"
	ELSE
	   IF VAL(s$) < 1 THEN IF WinStor GOTO 13 ELSE s$ = Sto.Num
	   GET #12, VAL(s$), Sto
	   IF Sto.Nam < "!" THEN GOTO 13
	   s$ = Sto.Num
	END IF
	qPrint s$, 24, 43, 0, 1
	qPrint Sto.Nam, 24, 46, 0, 1

14  EntryU 26, 43, 4, e, b, v$: IF e THEN EXIT SUB ELSE IF b GOTO 13
	IF v$ = "ALL " THEN
	   Ven.Nam = "ALL VENDORS"
	ELSE
	   IF VAL(v$) < 1 THEN WinVen v$: IF Inv.Ven < "   1" THEN GOTO 14
	   GET #2, VAL(v$), Ven: IF Ven.Nam < "!" THEN WinVen v$: GOTO 14
	   v$ = Ven.Num
	END IF

	qPrint v$, 26, 43, 0, 1
	qPrint Ven.Nam, 26, 48, 0, 1

	IF Confirm("Print Invoice Report") THEN
	   StatLin "Processing....Please wait"
	   f& = VAL(RIGHT$(f$, 4) + LEFT$(f$, 2) + MID$(f$, 4, 2)) 'from date
	   t& = VAL(RIGHT$(t$, 4) + LEFT$(t$, 2) + MID$(t$, 4, 2)) 'to date

	   CLOSE #13: OPEN "Invoices.INS" FOR OUTPUT AS #13
	   CLOSE #13: OPEN "Invoices.INS" FOR RANDOM SHARED AS #13 LEN = LEN(Ins)
	   FOR x = 1 TO LOF(7) / LEN(Inv)
		   GET #7, x, Inv
		   IF v$ = "ALL " OR v$ = Inv.Ven THEN
			  d& = VAL(RIGHT$(Inv.Bdt, 4) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2))
			  IF d& >= f& AND d& <= t& THEN
				 FOR a = 0 TO 24
					 IF Inv.Act(a) > " 0" THEN
						IF s$ = "00" OR s$ = Inv.Sto(a) THEN
						   GET #1, VAL(Inv.Act(a)), Acc
						   Ins.Sto = Inv.Sto(a)
						   Ins.Ven = VAL(Inv.Ven)
						   Ins.Bdt = VAL(RIGHT$(Inv.Bdt, 4) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2))
						   Ins.Act = Inv.Act(a)
						   Ins.Amt = Inv.Amt(a)
						   Ins.Num = x
						   PUT #13, , Ins
						END IF
					 ELSE
						EXIT FOR
					 END IF
				 NEXT
			  END IF
		   END IF
	   NEXT

	   CLOSE : LOCATE 1
	   PCOPY 0, 1
	   SHELL "RPSORT /Q Invoices.INS Invoices.STO /F20 /+1:2 /+3:2I /+5:4F"
	   PCOPY 1, 0
	   StatLin "Printing Report"
	   AP_Files
	   CLOSE #13: OPEN "Invoices.STO" FOR RANDOM SHARED AS #13 LEN = LEN(Ins)

	   PageNum = 0: HStoreInv s$
	   GET #13, 1, Ins: s$ = Ins.Sto: v = Ins.Ven
	   FOR x = 1 TO LOF(13) / LEN(Ins)
		   IF INKEY$ = CHR$(27) THEN PRINT #11, CHR$(12); : EXIT SUB
		   GET #13, x, Ins
		   IF s$ <> Ins.Sto THEN
			  PRINT #11, : PRINT #11, TAB(138); : PRINT #11, USING " #########,.##"; T1
			  PRINT #11,
			  PRINT #11, : PRINT #11, TAB(138); : PRINT #11, USING " #########,.##"; T2
			  PRINT #11,
			  LineNum = LineNum + 6: IF LineNum > 58 THEN PRINT #11, CHR$(12); : HStoreInv s$
			  T1 = 0
			  T2 = 0
			  s$ = Ins.Sto
			  v = Ins.Ven
		   END IF
		   IF v <> Ins.Ven THEN
			  PRINT #11, : PRINT #11, TAB(138); : PRINT #11, USING " #########,.##"; T1
			  PRINT #11,
			  LineNum = LineNum + 3: IF LineNum > 58 THEN PRINT #11, CHR$(12); : HStoreInv s$
			  T1 = 0
			  v = Ins.Ven
		   END IF
		   GET #12, VAL(Ins.Sto), Sto
		   GET #1, VAL(Ins.Act), Acc
		   GET #2, Ins.Ven, Ven
		   GET #7, Ins.Num, Inv
		   LineNum = LineNum + 1: IF LineNum > 58 THEN PRINT #11, CHR$(12); : HStoreInv s$
		   PRINT #11, USING "& "; Sto.Num; Sto.Nam; Ven.Num; Ven.Nam;
		   PRINT #11, USING "######### "; Inv.Num;
		   PRINT #11, USING "& "; Inv.Ref; Inv.Bdt; Inv.Chk; Ins.Act; Acc.Nam;
		   PRINT #11, USING "   ######,.##"; VAL(Ins.Amt)
		   T1 = T1 + VAL(Ins.Amt)
		   T2 = T2 + VAL(Ins.Amt)
		   T3 = T3 + VAL(Ins.Amt)
	   NEXT
	   PRINT #11, : PRINT #11, TAB(138); : PRINT #11, USING " #########,.##"; T1
	   PRINT #11,
	   PRINT #11, : PRINT #11, TAB(138); : PRINT #11, USING " #########,.##"; T2
	   PRINT #11,
	   PRINT #11, : PRINT #11, TAB(138); : PRINT #11, USING " #########,.##"; T3
	END IF
END SUB

DEFINT T
DEFLNG L
SUB mapVenMas ()
	CALL MhDisplayByName(0, 0, "AP-VenMas", e)
	StatLin "Vendor Master"
	SubMenu 1, "D"
	r = 3: n = 1: b = 1: t = LOF(4) / LEN(Vnx): GOSUB 601
	
600 IF n > 0 THEN GET #4, n, Vnx
	COLOR 11, 1: LOCATE r, 2: PRINT USING "####≥&"; Vnx.Num; Vnx.Ven
	KeyPres n$
	COLOR 0, 3:  LOCATE r, 2: PRINT USING "####≥&"; Vnx.Num; Vnx.Ven
	IF n$ = CHR$(13) THEN SubMenu 0, n$
	SELECT CASE n$
	CASE CHR$(27)       '===============esc====
		 EXIT SUB
	CASE CHR$(0) + "M"  '===============right==
		SubMenu 0, n$
	CASE CHR$(0) + "K"  '===============left===
		SubMenu 0, n$
	CASE CHR$(0) + "P"  '===============down===
		IF n + 1 > t THEN 600 ELSE n = n + 1
		IF r + 1 > 47 THEN CALL Scroll(3, 3, 34, 47, 1) ELSE r = r + 1
	CASE CHR$(0) + "H"  '===============up=====
		IF n - 1 < 1 THEN 600 ELSE n = n - 1
		IF r - 1 < 3 THEN CALL BkScroll(2, 3, 34, 47, 1) ELSE r = r - 1
	CASE CHR$(0) + "I"  '===============PgUp===
		b = n - r - 42: IF b < 1 THEN b = 1
		GOSUB 601
	CASE CHR$(0) + "Q"  '===============PgDn===
		b = n - r + 48: IF b > t - 44 THEN b = t - 44
		IF b < 1 THEN b = 1
		GOSUB 601
	CASE CHR$(0) + "G"  '===============Home===
		b = 1: GOSUB 601
	CASE CHR$(0) + "O"  '===============End====
		b = t - 44: IF b < 1 THEN b = 1
		GOSUB 601
	CASE "D"            '===============Display
		 SubMenu 0, n$
		 VenShow
	CASE "M"            '===============Modify=
		 SubMenu 0, n$
		 VenEdit
	CASE "N"            '===============New====
		 SubMenu 0, n$
		 y = 0
		 FOR z = 1 TO t
			 GET #3, z, Vnx
			 IF LEFT$(Vnx.Ven, 1) = "˛" THEN y = z: EXIT FOR
		 NEXT
		 IF y = 0 THEN Vnx.Num = 1 + LOF(2) \ LEN(Ven): t = Vnx.Num
		 IF Vnx.Num > 9999 THEN WinMssg "Vendors Master is full" ELSE VenEdit
	CASE "E"            '===============Erase==
		 SubMenu 0, n$
		 VenShow
		 L1 = Ven.Ybb + Ven.Ygr + Ven.Yds + Ven.Ycr + Ven.Ypd
		 IF L1 <> 0 THEN
			WinMssg "Active vendor, no erase"
		 ELSE
			IF Confirm("Erase Vendor") THEN
			   Vnx.Ven = "˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛"
			   PUT #3, Vnx.Num, Vnx
			   PUT #2, Vnx.Num
			   VenShow
			   VenSort
			   GOSUB 601
			END IF
		 END IF
	CASE "L"            '===============Print=====
		 SubMenu 0, n$
		 PageNum = 0: HVenList "00"
		 FOR y = 1 TO LOF(4) / LEN(Vnx)
		 IF INKEY$ = CHR$(27) THEN EXIT FOR
		 GET #4, y, Vnx
		 GET #2, Vnx.Num, Ven
		 IF Ven.Nam > "!" THEN
		   LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HVenList "00"
		   PRINT #11, USING "   &"; Ven.Num; Ven.Nam; Ven.Dis + " n " + Ven.Trm; Ven.Tel; Ven.Add; Ven.Str; Ven.Cty; Ven.Sta; Ven.Zip
		 END IF
		 NEXT
	CASE "I"            '===============Invoices==
		 SubMenu 0, n$
		 VenShow
		 InvFind Ven.Num, s, f
		 IF s THEN WinInv s, f ELSE WinMssg "No invoices for this vendor"
	CASE "C"            '===============Checks====
		SubMenu 0, n$
		VenShow
		apCkWind
	END SELECT
	GOTO 600

601 COLOR 0, 3: LOCATE 3
	FOR x = b TO t
		GET #4, x, Vnx
		LOCATE , 2: PRINT USING "####≥&"; Vnx.Num; Vnx.Ven
		IF CSRLIN > 47 THEN EXIT FOR
	NEXT
	n = b + r - 3
RETURN
END SUB

DEFSNG A-Z
SUB mapVenRpt ()
	IF Confirm("Print Vendor Accounts") THEN VenAcct
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB mapVerify ()
	CALL MhDisplayByName(0, 0, "AP-Verify", e)
	StatLin "Verify Payables"
	IF Confirm("Verfiy Payables") = 0 THEN EXIT SUB
	f = 1
	FOR x = 1 TO LOF(9) \ LEN(Inx)
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		GET #9, x, Inx
		IF f THEN f = 0: p = Inx.Ven
		IF p <> Inx.Ven THEN GOSUB 700
		GET #7, Inx.Num, Inv
		IF Inv.Chk = "*Not Paid*" THEN L1 = L1 + VAL(Inv.Net) * 100
	NEXT
	GOSUB 700
	CALL Scroll(7, 8, 68, 26, 1)
	LOCATE 26, 11: PRINT USING "≥& Balance≥#######.##≥#######.##≥"; SPACE$(20); L2 / 100; L3 / 100;
	WinMssg "Press any key to return to menu"
	EXIT SUB
700 GET #2, p, Ven
	L0 = Ven.Ybb + Ven.Ygr - Ven.Yds - Ven.Ycr - Ven.Ypd
	CALL Scroll(7, 7, 68, 26, 1)
	COLOR 26, 9: LOCATE 26, 7: PRINT USING "&≥&≥#######.##≥#######.##≥"; Ven.Num; Ven.Nam; L0 / 100; L1 / 100;
	IF L0 <> L1 THEN
	   PRINT "ERROR"; : BEEP
	   IF INPUT$(1) = CHR$(27) THEN EXIT SUB
	END IF
	L2 = L2 + L0
	L3 = L3 + L1: L1 = 0: p = Inx.Ven
	RETURN
END SUB

DEFINT L
DEFDBL T
SUB PrtChk (f$, t$)
	StatLin "Payable Checks Listing"
	f& = VAL(RIGHT$(f$, 4) + LEFT$(f$, 2) + MID$(f$, 4, 2)) 'from date
	t& = VAL(RIGHT$(t$, 4) + LEFT$(t$, 2) + MID$(t$, 4, 2)) 'to date
	PageNum = 0: HChecks "00"
	FOR x = 1 TO LOF(5) \ LEN(Chk)
		IF INKEY$ = CHR$(27) THEN PRINT #11, CHR$(12); : EXIT SUB
		GET #5, x, Chk
		c& = VAL(RIGHT$(Chk.Dat, 4) + LEFT$(Chk.Dat, 2) + MID$(Chk.Dat, 4, 2)) 'check date
		IF c& >= f& AND c& <= t& THEN
		   GET #2, VAL(Chk.Ven), Ven
		   T0 = T0 + VAL(Chk.Amt)
		   PRINT #11, USING "& "; Ven.Num; Ven.Nam; Chk.Num; Chk.Amt; Chk.Dat;
		   IF Chk.Sta = "VOID" THEN
			  PRINT #11, "VOID"
		   ELSE
			  FOR a = 0 TO 11
				  IF Chk.Inv(a) > 0 THEN
					 In = FindInv(Chk.Inv(a))
					 IF In = 0 THEN WinMssg "Invoice not found": ERROR 200
					 PRINT #11, TAB(64); : PRINT #11, USING "#######"; Chk.Inv(a);

					 PRINT #11, USING " &"; Inv.Ddt; Inv.Grs; Inv.Dis; Inv.Crd;
					 PRINT #11, "  "; Inv.Net;
					 T1 = T1 + VAL(Inv.Grs)
					 T2 = T2 + VAL(Inv.Dis)
					 T3 = T3 + VAL(Inv.Crd)
					 T4 = T4 + VAL(Inv.Net)
					 FOR z = 0 TO 24
					 IF Inv.Act(z) > " 0" THEN
						 GET #1, VAL(Inv.Act(z)), Acc
						 PRINT #11, TAB(120);
						 PRINT #11, USING "& "; Inv.Sto(z); Inv.Act(z);
						 PRINT #11, Acc.Nam; Inv.Amt(z)
						 T5 = T5 + VAL(Inv.Amt(z))
						 LineNum = LineNum + 1: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HChecks "00"
					 ELSE
						 EXIT FOR
					 END IF
					 NEXT
				  END IF
			  NEXT
		   END IF
		END IF
	NEXT
	PRINT #11,
	PRINT #11, USING "& ######.## &"; SPACE$(41); T0; SPACE$(29);
	PRINT #11, USING "######.##"; T1; T2; T3;
	PRINT #11, USING "#######.## & ######.##"; T4; SPACE$(31); T5
	PRINT #11, CHR$(12);
END SUB

DEFLNG P
DEFSNG L
SUB Require ()
	StatLin "Cash Requirements"
	CLOSE : LOCATE 1
	SHELL "AC.COM Invoices.INX Invoices.DUE /L12 /+10,1 /+9,1 /+8,1 /+7,1 /+6,1 /+5,1 /Q"
	AP_Files
	f = 1: PageNum = 0: HRequir "00"
	FOR x = 1 TO LOF(10) \ LEN(Inx)
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		GET #10, x, Inx
		GET #7, Inx.Num, Inv
		IF Inv.Chk = "*Not Paid*" THEN
		   IF f THEN f = 0: p = Inx.Ddt
		   IF p <> Inx.Ddt THEN GOSUB 540
		   GET #2, Inx.Ven, Ven
		   LineNum = LineNum + 1: IF LineNum > 58 THEN PRINT #11, CHR$(12); : HRequir "00"
		   PRINT #11, USING "&  &  ########"; Ven.Num; Ven.Nam; Inx.Inv;
		   PRINT #11, USING "  &"; Inv.Ref; Inv.Bdt; Inv.Ddt;
		   PRINT #11, USING "   &"; Inv.Grs; Inv.Dis; Inv.Crd; Inv.Net
		   T1 = T1 + VAL(Inv.Grs): T2 = T2 + VAL(Inv.Dis): T3 = T3 + VAL(Inv.Crd): T4 = T4 + VAL(Inv.Net)
		END IF
	NEXT
	GOSUB 540
	PRINT #11, TAB(82); : PRINT #11, USING "#######,.##"; T5; T6; T7; T8; T8
	PRINT #11, CHR$(12);
	EXIT SUB
540 T5 = T5 + T1: T6 = T6 + T2: T7 = T7 + T3: T8 = T8 + T4
	PRINT #11, : PRINT #11, CHR$(27); "G"; TAB(82);
	PRINT #11, USING "#######,.##"; T1; T2; T3; T4; T8;
	PRINT #11, "  "; Inv.Ddt; CHR$(27); "H": PRINT #11,
	LineNum = LineNum + 3
	p = Inx.Ddt: T1 = 0: T2 = 0: T3 = 0: T4 = 0
	RETURN
END SUB

DEFINT P, T
DEFLNG L
SUB VenAcct ()
	StatLin "Vendor Listing"
	PageNum = 0: HVendor "00"
	FOR y = 1 TO LOF(4) / LEN(Vnx)
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		GET #4, y, Vnx
		GET #2, Vnx.Num, Ven
		IF Ven.Ygr > 0 THEN
			IF Ven.Num > "0000" THEN
			   PRINT #11, USING "&  "; Ven.Num; Ven.Nam;
			   PRINT #11, USING "#######,.##-"; Ven.Mbb / 100; Ven.Mgr / 100; (Ven.Mds + Ven.Mcr) / 100; Ven.Mpd / 100; (Ven.Mbb + Ven.Mgr - Ven.Mds - Ven.Mcr - Ven.Mpd) / 100; : PRINT #11, "   ";
			   PRINT #11, USING "########,.##-"; Ven.Ybb / 100; Ven.Ygr / 100; (Ven.Yds + Ven.Ycr) / 100; Ven.Ypd / 100; (Ven.Ybb + Ven.Ygr - Ven.Yds - Ven.Ycr - Ven.Ypd) / 100
			   L1 = L1 + Ven.Mbb: L2 = L2 + Ven.Mgr: L3 = L3 + Ven.Mds + Ven.Mcr: L4 = L4 + Ven.Mpd
			   L5 = L5 + Ven.Ybb: L6 = L6 + Ven.Ygr: L7 = L7 + Ven.Yds + Ven.Ycr: L8 = L8 + Ven.Ypd
			   LineNum = LineNum + 6: IF LineNum > 60 THEN PRINT #11, CHR$(12); : HVendor "00"
			END IF
			PRINT #11, Ven.Con, , Ven.Co1
			PRINT #11, Ven.Add, , Ven.Ad1
			PRINT #11, Ven.Str, , Ven.Sr1
			PRINT #11, Ven.Cty; " "; Ven.Sta; " "; Ven.Zip, , Ven.Ct1; " "; Ven.St1; " "; Ven.Zi1
			PRINT #11, "TEL: "; Ven.Tel; "  FAX: "; Ven.Fax, , "TEL: "; Ven.Te1; "  FAX: "; Ven.Fa1
		END IF
	NEXT
	PRINT #11,
	PRINT #11, TAB(37); :  PRINT #11, USING "#######,.##-"; L1 / 100; L2 / 100; L3 / 100; L4 / 100; (L1 + L2 - L3 - L4) / 100;
	PRINT #11, TAB(100); : PRINT #11, USING "########,.##-"; L5 / 100; L6 / 100; L7 / 100; L8 / 100; (L5 + L6 - L7 - L8) / 100
	PRINT #11, CHR$(12);
END SUB

DEFINT L
SUB VenClose (p)
	AP_Files
	SELECT CASE p
	CASE 1  'close month
		FOR x = 1 TO LOF(2) \ LEN(Ven)
			GET #2, x, Ven
			Ven.Mbb = Ven.Mbb + Ven.Mgr - Ven.Mds - Ven.Mcr - Ven.Mpd
			Ven.Mgr = 0
			Ven.Mds = 0
			Ven.Mcr = 0
			Ven.Mpd = 0
			PUT #2, x, Ven   '--------------close vendors------------
		NEXT
	 CASE 2  'close year
		 FOR x = 1 TO LOF(2) \ LEN(Ven)
			 GET #2, x, Ven
			 Ven.Ybb = Ven.Ybb + Ven.Ygr - Ven.Yds - Ven.Ycr - Ven.Ypd
			 Ven.Ygr = 0
			 Ven.Yds = 0
			 Ven.Ycr = 0
			 Ven.Ypd = 0
			 PUT #2, x, Ven  '===============close vendors=============
		  NEXT
	 END SELECT
END SUB

SUB VenEdit ()
	RSET Ven.Num = STR$(Vnx.Num)
	VenShow
	p$ = Ven.Nam
610 EntryS 3, 52, 28, e, b, Ven.Nam:    IF e THEN EXIT SUB ELSE IF b THEN 610
	IF Ven.Nam < "!" THEN WinMssg "Name must not be blank": GOTO 610
611 EntryS 8, 52, 28, e, b, Ven.Add:    IF e THEN EXIT SUB ELSE IF b THEN 610
612 EntryS 9, 52, 28, e, b, Ven.Str:    IF e THEN EXIT SUB ELSE IF b THEN 611
613 EntryS 10, 52, 14, e, b, Ven.Cty:   IF e THEN EXIT SUB ELSE IF b THEN 612
614 EntryU 10, 67, 2, e, b, Ven.Sta:    IF e THEN EXIT SUB ELSE IF b THEN 613
615 EntryS 10, 70, 10, e, b, Ven.Zip:   IF e THEN EXIT SUB ELSE IF b THEN 614
616 EntryS 11, 52, 28, e, b, Ven.Con:   IF e THEN EXIT SUB ELSE IF b THEN 615
617 EntryT 12, 52, e, b, Ven.Tel:       IF e THEN EXIT SUB ELSE IF b THEN 616
618 EntryT 13, 52, e, b, Ven.Fax:       IF e THEN EXIT SUB ELSE IF b THEN 617
619 EntryS 17, 52, 28, e, b, Ven.Ad1:   IF e THEN EXIT SUB ELSE IF b THEN 618
620 EntryS 18, 52, 28, e, b, Ven.Sr1:   IF e THEN EXIT SUB ELSE IF b THEN 619
621 EntryS 19, 52, 14, e, b, Ven.Ct1:   IF e THEN EXIT SUB ELSE IF b THEN 620
622 EntryU 19, 67, 2, e, b, Ven.St1:    IF e THEN EXIT SUB ELSE IF b THEN 621
623 EntryS 19, 70, 10, e, b, Ven.Zi1:   IF e THEN EXIT SUB ELSE IF b THEN 622
624 EntryS 20, 52, 28, e, b, Ven.Co1:   IF e THEN EXIT SUB ELSE IF b THEN 623
625 EntryT 21, 52, e, b, Ven.Te1:       IF e THEN EXIT SUB ELSE IF b THEN 624
626 EntryT 22, 52, e, b, Ven.Fa1:       IF e THEN EXIT SUB ELSE IF b THEN 625
627 EntryU 26, 52, 1, e, b, Ven.Pay:    IF e THEN EXIT SUB ELSE IF b THEN 626
628 EntryN 27, 52, 2, 0, e, b, Ven.Trm: IF e THEN EXIT SUB ELSE IF b THEN 627
629 EntryN 28, 52, 2, 0, e, b, Ven.Dis: IF e THEN EXIT SUB ELSE IF b THEN 628
630 EntryS 32, 52, 12, e, b, Ven.Act:   IF e THEN EXIT SUB ELSE IF b THEN 629
	IF Confirm("Save Vendor") = 0 THEN 630
	Ven.Num = RIGHT$(STR$(10000 + Vnx.Num), 4)
	Vnx.Ven = Ven.Nam
	PUT #3, Vnx.Num, Vnx
	PUT #2, Vnx.Num, Ven
	IF p$ <> Ven.Nam THEN VenSort
END SUB

FUNCTION VenFind (f$)
   b = 1: t = LOF(4) \ LEN(Vnx)
   DO UNTIL (t < b)
	  m = (t + b) \ 2: GET #4, m, Vnx
	  t$ = UCASE$(Vnx.Ven)
	  IF f$ = t$ THEN VenFind = m: EXIT DO
	  IF f$ > t$ THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

DEFLNG L
DEFSNG A-K, M-Q, S-Z
SUB VenLine (r, Lm, Ly)
	LOCATE r, 52: PRINT USING "########,.##-"; Lm / 100;
	LOCATE r, 67: PRINT USING "########,.##-"; Ly / 100;
END SUB

DEFSNG L, R
SUB VenShow ()
	COLOR 10, 0
	GET #2, Vnx.Num, Ven
	qPrint Ven.Num, 3, 47, 0, 1
	qPrint Ven.Nam, 3, 52, 0, 1
	qPrint Ven.Add, 8, 52, 0, 1
	qPrint Ven.Str, 9, 52, 0, 1
	qPrint Ven.Cty + " " + Ven.Sta + " " + Ven.Zip, 10, 52, 0, 1
	qPrint Ven.Con, 11, 52, 0, 1
	qPrint Ven.Tel, 12, 52, 0, 1
	qPrint Ven.Fax, 13, 52, 0, 1
	
	qPrint Ven.Ad1, 17, 52, 0, 1
	qPrint Ven.Sr1, 18, 52, 0, 1
	qPrint Ven.Ct1 + " " + Ven.St1 + " " + Ven.Zi1, 19, 52, 0, 1
	qPrint Ven.Co1, 20, 52, 0, 1
	qPrint Ven.Te1, 21, 52, 0, 1
	qPrint Ven.Fa1, 22, 52, 0, 1

	qPrint Ven.Trm, 27, 52, 0, 1
	qPrint Ven.Dis, 28, 52, 0, 1
	
	VenLine 37, Ven.Mgr, Ven.Ygr
	VenLine 38, Ven.Mds, Ven.Yds
	VenLine 39, Ven.Mcr, Ven.Ycr
	VenLine 41, Ven.Mgr - Ven.Mds - Ven.Mcr, Ven.Ygr - Ven.Yds - Ven.Ycr
	VenLine 42, Ven.Mbb, Ven.Ybb
	VenLine 44, Ven.Mbb + Ven.Mgr - Ven.Mds - Ven.Mcr, Ven.Ybb + Ven.Ygr - Ven.Yds - Ven.Ycr
	VenLine 45, Ven.Mpd, Ven.Ypd
	VenLine 47, Ven.Mbb + Ven.Mgr - Ven.Mds - Ven.Mcr - Ven.Mpd, Ven.Ybb + Ven.Ygr - Ven.Yds - Ven.Ycr - Ven.Ypd
END SUB

SUB VenSort ()
	LOCATE 1: CLOSE
	SHELL "AC.COM Vendors.Inx Vendors.Nam /L30 /+1,28 /C /Q"
	ErrSw(1) = 4
	AP_Files
END SUB

DEFINT A-Z
SUB WinInv (e, t)
	PCOPY 0, 1
	b = e: r = 6: n = b: GOSUB W2
W1: GET #9, n, Inx: GET #7, Inx.Num, Inv
	COLOR 0, 7: LOCATE r, 3: PRINT USING "&≥&≥&≥&"; Inv.Ref; Inv.Ddt; Inv.Chk; Inv.Net
	KeyPres n$
	COLOR 15, 4: LOCATE r, 3: PRINT USING "&≥&≥&≥&"; Inv.Ref; Inv.Ddt; Inv.Chk; Inv.Net
	SELECT CASE n$
	CASE CHR$(13)
		PCOPY 0, 2
		CALL MhDisplayByName(0, 0, "AP-Invoice", v)
		InvDisp Inx.Num
		KeyPres f$
		PCOPY 2, 0
	CASE CHR$(0) + "P" 'Dn
		IF n + 1 > t GOTO W1 ELSE n = n + 1
		IF r + 1 > 20 THEN CALL Scroll(3, 6, 43, 20, 1) ELSE r = r + 1
	CASE CHR$(0) + "H" 'Up
		IF n - 1 < e GOTO W1 ELSE n = n - 1
		IF r - 1 < 6 THEN CALL BkScroll(3, 6, 43, 20, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
		b = n - r - 9: IF b < e THEN b = e
		GOSUB W2: n = b + r - 6
	CASE CHR$(0) + "Q" 'PgDn
		b = 21 - r + n
		IF t - 14 > e THEN
		   IF b > t - 14 THEN b = t - 14: n = t - 20 + r ELSE n = n + 15
		   GOSUB W2
		END IF
	CASE CHR$(0) + "G" 'Home
		b = e: GOSUB W2: n = e + r - 6
	CASE CHR$(0) + "O" 'End
		b = t - 14: IF b < e THEN b = e
		GOSUB W2: n = b + r - 6
	CASE ELSE
		PCOPY 1, 0
		EXIT SUB
	END SELECT
	GOTO W1

W2: COLOR 15, 4: LOCATE 5, 1: PRINT " …REFERENCEÕ—ÕDUE DATEÕ—ÕÕCHECKÕÕÕ—ÕAMOUNTÕª "
	FOR x = b TO t: GET #9, x, Inx: GET #7, Inx.Num, Inv
	PRINT USING " ∫&≥&≥&≥&∫ "; Inv.Ref; Inv.Ddt; Inv.Chk; Inv.Net
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	PRINT " »ÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕº "
	RETURN
END SUB

SUB WinVen (f$)
	PCOPY 0, 1
	Bot = 1: Top = LOF(4) \ LEN(Vnx)
	n = 1: r = 7
	GOSUB C2
C1: GET #4, n, Vnx
	COLOR 15, 0: LOCATE r, 42: PRINT USING "####≥&"; Vnx.Num; Vnx.Ven;
	KeyPres n$
	COLOR 16, 13: LOCATE r, 42: PRINT USING "####≥&"; Vnx.Num; Vnx.Ven;
	SELECT CASE n$
	CASE CHR$(27)
		 f$ = ""
		 PCOPY 1, 0
		 EXIT SUB
	CASE CHR$(13)
		 GET #2, Vnx.Num, Ven
		 IF Ven.Nam < "!" GOTO C1
		 f$ = Ven.Num
		 PCOPY 1, 0
		 EXIT SUB
	CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
		 IF n + 1 > Top GOTO C1 ELSE n = n + 1
		 IF r + 1 > 20 THEN CALL Scroll(42, 7, 74, 20, 1) ELSE r = r + 1
	CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
		 IF n - 1 < 1 GOTO C1 ELSE n = n - 1
		 IF r - 1 < 7 THEN CALL BkScroll(42, 7, 74, 20, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
		 Bot = n - r - 7: IF Bot < 1 THEN Bot = 1
		 GOSUB C2: n = Bot + r - 7
	CASE CHR$(0) + "Q" 'PgDn
		 Bot = 21 - r + n
		 IF Bot > Top - 13 THEN Bot = Top - 13
		 n = Bot + r - 7: GOSUB C2
	CASE CHR$(0) + "G" 'Home
		 Bot = 1: GOSUB C2
		 n = r - 6
	CASE CHR$(0) + "O" 'End
		 Bot = Top - 13
		 n = Top - 20 + r: GOSUB C2
	CASE ELSE
		 f = VenFind(n$): Bot = LOC(4)
		 IF Bot > Top - 13 THEN Bot = Top - 13
		 n = Bot + r - 7: GOSUB C2
	END SELECT
	GOTO C1
 
C2: COLOR 16, 13: LOCATE 6, 40: PRINT " …ÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª "
	FOR x = Bot TO Top: GET #4, x, Vnx
	LOCATE , 40: PRINT USING " ∫####≥&∫ "; Vnx.Num; Vnx.Ven
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	LOCATE , 40: PRINT " »ÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº ": COLOR 30
	LOCATE , 40: PRINT " "; CHR$(17); "Ÿ "; CHR$(24); " "; CHR$(25); "  PgUp  PgDn  Home  End  Esc  "
	RETURN
END SUB

