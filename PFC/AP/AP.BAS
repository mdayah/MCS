DEFINT A-Z
DEFLNG L
DEFDBL T
DECLARE FUNCTION Confirm ()
DECLARE FUNCTION InvNet ()
DECLARE FUNCTION InvDis ()
DECLARE SUB AccDist ()
DECLARE SUB OpenFiles ()
DECLARE SUB NumWord (n$, w$)
DECLARE SUB TimeDate ()
DECLARE SUB HiLite (m, p)
DECLARE SUB KeyHit (f$)
DECLARE SUB HitKey (f$)

DECLARE SUB WinMssg (f$)
DECLARE SUB WinInv (b, e)
DECLARE SUB WinAcc (f$)
DECLARE SUB WinVen (f$)

DECLARE SUB HAging ()
DECLARE SUB Heading ()
DECLARE SUB HInvoice ()
DECLARE SUB HRegister (d$)
DECLARE SUB HChecks ()
DECLARE SUB HWritten ()
DECLARE SUB HRequir ()
DECLARE SUB HVendor ()
DECLARE SUB HInvList ()
DECLARE SUB HVenList ()
DECLARE SUB HAccount ()

DECLARE SUB VenLine (r, Lm, Ly)
DECLARE SUB VenDisp (x)
DECLARE SUB VenEdit ()
DECLARE SUB VenShow ()
DECLARE SUB VenSort ()

DECLARE SUB InvFind (n$, f, e)
DECLARE SUB InvDisp (n)
DECLARE SUB InvLong (b)
DECLARE SUB InvShor (i)
DECLARE SUB InvSort ()

DECLARE SUB PrtChk (f$)
DECLARE SUB PrtHwc (f$)

REM $INCLUDE: '..\BI\Library.Dec'
REM $INCLUDE: '..\BI\Vendors.Rec'
REM $INCLUDE: '..\BI\Accounts.Rec'
REM $INCLUDE: '..\BI\PInvoice.Rec'
REM $INCLUDE: '..\BI\PInvIndx.Rec'
REM $INCLUDE: '..\BI\VenIndex.Rec'
REM $INCLUDE: '..\BI\Purchase.REC'

TYPE HandWritten
	Chk AS STRING * 10
	Inv AS INTEGER
	END TYPE
  
	DIM SHARED Ven AS Vendors
	DIM SHARED Acc AS Accounts
	DIM SHARED Inv AS Invoices
	DIM SHARED Pur AS Purchase
	DIM SHARED Inx AS Index
	DIM SHARED Vnx AS VenIndex
	DIM SHARED Hwc AS HandWritten

	DIM InN(13) AS INTEGER
	DIM SHARED Act(0 TO 10) AS STRING * 2
	DIM SHARED Amt(0 TO 10) AS STRING * 8
	DIM SHARED oAc(0 TO 9)  AS STRING * 2
	DIM SHARED oAm(0 TO 9)  AS STRING * 8
	DIM SHARED Digits(1 TO 3, 0 TO 9) AS STRING
	DIM SHARED Menu(6, 6) AS INTEGER
	DIM SHARED Sort    AS INTEGER
	DIM SHARED Top     AS INTEGER
	DIM SHARED Bot     AS INTEGER
   
	ON ERROR GOTO Errors

FOR y = 1 TO 3: FOR x = 0 TO 9: READ Digits(y, x): NEXT x, y
FOR y = 1 TO 6: FOR x = 1 TO 2: READ Menu(y, x): NEXT x, y

KEY OFF: KEY 1, "1": KEY 2, "2": KEY 3, "3": KEY 4, "4": KEY 5, "5"
	 KEY 6, "6": KEY 7, "7": KEY 8, "8": KEY 9, "9": KEY 10, "0"
	OPEN "Index.HWC" FOR OUTPUT AS #1: CLOSE
	OPEN "Invoices.Num" FOR INPUT AS #1
	INPUT #1, InvLast, InvMnth, InvYear: CLOSE

	SCREEN 0, 0, 0, 0
	CALL MhOpenScreenLib("AP.SLB" + CHR$(0), e)
	CALL MhShadowOn(0, 8)
	CALL MhBlinkOff
Menu: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
10  CLOSE : CALL MhDisplayByName(0, 0, "Menu", e)
	HitKey f$: f = VAL(f$)
	IF f$ = "0" THEN f = 10
	IF f$ = CHR$(27) GOTO Terminate
	ON f GOTO EntInv, EdtInv, NewInv, ChkReg, Checks, 20, Vendor, 30, Monthend, Yearend
	GOTO 10
20  CLOSE : CALL MhDisplayByName(0, 0, "Reports", e)
	HitKey f$: f = VAL(f$)
	IF f$ = "0" THEN f = 10
	IF f$ = CHR$(27) THEN 10
	ON f GOSUB PrtVen, Aging, Requir, DetInv, MonInv, MonCom, MonHwc, YerInv, YerCom, YerHwc
	GOTO 20
30  CLOSE : CALL MhDisplayByName(0, 0, "Utility", e)
	HitKey f$: f = VAL(f$)
	IF f$ = CHR$(27) THEN 10
	ON f GOTO RevCom, RevHwc, Verify, AccAct, Purge
	GOTO 30
EntInv: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	OpenFiles
	ERASE Act, Amt, oAc, oAm
	Inv.Ven = "   ": Vid = 0
	Inv.Ref = "          "
	Inv.Bdt = "          "
	Inv.Ddt = "          "
	Inv.Pdt = "          "
	Inv.Chk = "          "
	Inv.Grs = "    0.00": Grs$ = ""
	Inv.Dis = "    0.00": Dis$ = ""
	Inv.Crd = "    0.00": Crd$ = ""
	Inv.Net = "    0.00": Net$ = ""
	Inv.Pur = "     "
	Rec = 1 + LOF(4) \ LEN(Inv)
	CALL MhDisplayByName(0, 0, "Invoice", e)
	LOCATE 2, 20: PRINT USING "#####"; Rec: Picture 2, 20, 5, Rec$
100 EntryN 2, 42, 3, 0, e, b, Inv.Ven: IF e THEN 10 ELSE IF b THEN 100
	IF Inv.Ven < "  1" THEN WinVen Inv.Ven: IF Inv.Ven < "  1" THEN GOTO 100
	GET #1, VAL(Inv.Ven), Ven: IF Ven.Nam < "!" THEN WinVen Inv.Ven: GOTO 100
	LOCATE 2, 42: PRINT Ven.Num: Inv.Ven = Ven.Num
	LOCATE 2, 48: PRINT Ven.Nam; "  "
	LOCATE 3, 48: PRINT Ven.Str; "  "
	LOCATE 4, 48: PRINT Ven.Cty; " "; Ven.Sta; " "; Ven.Zip; "  "
105 EntryS 6, 48, 10, e, b, Inv.Ref: IF e THEN 10 ELSE IF b THEN 100
110 EntryD 7, 48, e, b, Inv.Bdt:     IF e THEN 10 ELSE IF b THEN 105
	IF Inv.Bdt < "0" THEN WinMssg "Must have the billing date": GOTO 110
	IF Inv.Ddt < "0" THEN Inv.Ddt = DayMath$(Inv.Bdt, VAL(Ven.Trm))
115 EntryD 8, 48, e, b, Inv.Ddt:     IF e THEN 10 ELSE IF b THEN 110
	IF Inv.Ddt < "0" THEN Inv.Ddt = DayMath$(Inv.Bdt, VAL(Ven.Trm)): GOTO 115
	IF DateDif(Inv.Bdt, Inv.Ddt) < 0 THEN WinMssg "Due before invoice date!": GOTO 115
120 EntryD 9, 48, e, b, Inv.Pdt:     IF e THEN 10 ELSE IF b THEN 115
	IF Inv.Pdt < "0" THEN
	   Inv.Pdt = "**-**-****": LOCATE 9, 48: PRINT Inv.Pdt
	   Inv.Chk = "*Not Paid*"
	ELSE
	   IF DateDif(Inv.Bdt, Inv.Pdt) < 0 THEN WinMssg "Paid before invoice date!": GOTO 120
	END IF
125 EntryU 10, 48, 10, e, b, Inv.Chk: IF e THEN 10 ELSE IF b THEN 120
	IF Inv.Pdt = "**-**-****" AND LEFT$(Inv.Chk, 1) = "W" THEN WinMssg "Date paid please!": GOTO 120
	IF Inv.Pdt = "**-**-****" AND LEFT$(Inv.Chk, 1) = "H" THEN Inv.Chk = "Hold      "
	LOCATE 10, 48: PRINT Inv.Chk

130 EntryN 6, 70, 8, 2, e, b, Inv.Grs: IF e THEN 10
	IF InvNet THEN 130 ELSE IF b THEN 125
135 EntryN 7, 70, 8, 2, e, b, Inv.Dis: IF e THEN 10
	IF InvNet THEN 135 ELSE IF b THEN 130
140 EntryN 8, 70, 8, 2, e, b, Inv.Crd: IF e THEN 10
	IF InvNet THEN 140 ELSE IF b THEN 135
	
	x = 0
155 EntryN x + 13, 35, 2, 0, e, b, Act(x): IF e THEN 10
	IF b THEN x = x - 1: IF x < 0 THEN 140 ELSE 160
	IF Act(x) = "  " THEN
	   FOR y = x TO 9: Act(y) = Act(y + 1): Amt(y) = Amt(y + 1): NEXT y
	   IF CSRLIN < 22 THEN CALL Scroll(35, x + 13, 77, 22, 1)
	   COLOR 11, 1: LOCATE 22, 35: PRINT "ฒฒ ณ ฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒ ณ ฒฒฒฒฒฒฒฒ"
	   y = InvDis
	   GOTO 170
	END IF
	IF Act(x) = " 0" THEN WinAcc Act(x): IF Act(x) = "  " THEN 155
	GET #8, VAL(Act(x)), Acc
	IF Acc.Nam < "!" THEN WinMssg "Sorry, blank account.": GOTO 155
	IF Acc.Crd = "00" THEN WinMssg "Sorry, reserved account.": GOTO 155
	LOCATE x + 13, 35: PRINT Acc.Num;
	LOCATE , 40: PRINT Acc.Nam; : Act(x) = Acc.Num
160 EntryN x + 13, 70, 8, 2, e, b, Amt(x): IF e THEN 10
	IF InvDis THEN 160 ELSE IF b THEN 155
	x = x + 1: IF x < 10 THEN 155 ELSE x = 9

170 Picture 24, 70, 8, f$
	IF f$ <> Inv.Net THEN 155
	IF Confirm THEN
	   s$ = "": Sort = 1
	   FOR x = 0 TO 9
	   IF Act(x) > "00" THEN
	  GET #8, VAL(Act(x)), Acc
	  Acc.Mdb = Acc.Mdb + VAL(Amt(x)) * 100
	  Acc.Ydb = Acc.Ydb + VAL(Amt(x)) * 100
	  PUT #8, VAL(Act(x)), Acc '--------------------Debit Account----
	  IF LEFT$(Inv.Chk, 1) = "W" THEN
		 GET #8, 1, Acc
		 Acc.Mcr = Acc.Mcr + VAL(Amt(x)) * 100
		 Acc.Ycr = Acc.Ycr + VAL(Amt(x)) * 100
		 PUT #8, 1, Acc '----------------------------Credit Cash(1)---
	  ELSE
		 GET #8, VAL(Acc.Crd), Acc
		 Acc.Mcr = Acc.Mcr + VAL(Amt(x)) * 100
		 Acc.Ycr = Acc.Ycr + VAL(Amt(x)) * 100
		 PUT #8, LOC(8), Acc '-----------------------Credit Payable---
	  END IF
	   ELSE
	  Act(x) = "  "
	   END IF
	   s$ = s$ + Act(x)
	   NEXT
	   FOR x = 0 TO 9: s$ = s$ + Amt(x): NEXT
	   Inv.Dst = s$
	   PUT #4, Rec, Inv '-----------------------------Save Invoice--------
	   IF LEFT$(Inv.Chk, 1) = "W" THEN
	  Ven.Mpd = Ven.Mpd + VAL(Inv.Net) * 100
	  Ven.Ypd = Ven.Ypd + VAL(Inv.Net) * 100
	  Inx.Mrk = "W"
	   ELSE
	  Inx.Mrk = " "
	   END IF
	   Inx.Ven = Ven.Num
	   Inx.Ddt = RIGHT$(Inv.Ddt, 2) + LEFT$(Inv.Ddt, 2) + MID$(Inv.Ddt, 4, 2)
	   Inx.Inv = Rec
	   PUT #5, Rec, Inx '-----------------------------Save Index----------
	   Ven.Mgr = Ven.Mgr + VAL(Inv.Grs) * 100
	   Ven.Mds = Ven.Mds + VAL(Inv.Dis) * 100
	   Ven.Mcr = Ven.Mcr + VAL(Inv.Crd) * 100
	   Ven.Ygr = Ven.Ygr + VAL(Inv.Grs) * 100
	   Ven.Yds = Ven.Yds + VAL(Inv.Dis) * 100
	   Ven.Ycr = Ven.Ycr + VAL(Inv.Crd) * 100
	   PUT #1, VAL(Ven.Num), Ven '--------------------Update Vendor-------
	  
	   IF Vid > 0 THEN '::::::::::::::::::::::::::::::::::::::::::::::::::
	  GET #1, Vid, Ven
	  Ven.Mgr = Ven.Mgr - VAL(Grs$) * 100
	  Ven.Mds = Ven.Mds - VAL(Dis$) * 100
	  Ven.Mcr = Ven.Mcr - VAL(Crd$) * 100
	  Ven.Ygr = Ven.Ygr - VAL(Grs$) * 100
	  Ven.Yds = Ven.Yds - VAL(Dis$) * 100
	  Ven.Ycr = Ven.Ycr - VAL(Crd$) * 100
	  PUT #1, Vid, Ven '--------------------------Reverse Vendor------
	  FOR x = 0 TO 9
	  IF oAc(x) > "00" THEN
		 GET #8, VAL(oAc(x)), Acc
		 Acc.Mdb = Acc.Mdb - VAL(oAm(x)) * 100
		 Acc.Ydb = Acc.Ydb - VAL(oAm(x)) * 100
		 PUT #8, VAL(oAc(x)), Acc '---------------Reverse Acc.Debit---
		 GET #8, VAL(Acc.Crd), Acc
		 Acc.Mcr = Acc.Mcr - VAL(oAm(x)) * 100
		 Acc.Ycr = Acc.Ycr - VAL(oAm(x)) * 100
		 PUT #8, LOC(8), Acc '--------------------Reverse Acc.Credit--
	  ELSE EXIT FOR
	  END IF
	  NEXT
	  GOTO EdtInv
	   ELSE
	  GOTO EntInv
	   END IF
	END IF
	GOTO 155
EdtInv: '====================================================================
	CALL MhDisplayByName(0, 0, "Invoice", e)
	Rec$ = "     ": OpenFiles
200 EntryN 2, 20, 5, 0, e, b, Rec$: IF e THEN 10 ELSE IF b THEN 200
	Rec = VAL(Rec$)
	IF Rec > LOF(4) \ LEN(Inv) OR Rec < 1 THEN WinMssg "No such invoice": GOTO 200
	InvDisp Rec
	Vid = VAL(Ven.Num)
	Grs$ = Inv.Grs
	Dis$ = Inv.Dis
	Crd$ = Inv.Crd
	Net$ = Inv.Net
	IF LEFT$(Inv.Chk, 1) = "C" OR LEFT$(Inv.Chk, 1) = "W" THEN 200
	GOTO 100
NewInv: '====================================================================
	LOCATE 14, 30: PRINT "Print New Invoice Entries."
	InvLong InvLast
	InvLast = LOF(4) \ LEN(Inv)
	GOTO 10
DetInv: '====================================================================
	LOCATE 15, 30: PRINT "Detailed Invoices....[Month]"
	InvLong InvMnth
	RETURN
MonInv: '====================================================================
	LOCATE 16, 30: PRINT "Invoices by Vendor...[Month]"
	InvShor InvMnth
	RETURN
MonCom: '====================================================================
	LOCATE 17, 30: PRINT "Computer Checks......[Month]"
	PrtChk "MCK"
	RETURN
MonHwc: '====================================================================
	LOCATE 18, 30: PRINT "Handwritten Checks...[Month]"
	PrtHwc "W"
	RETURN
YerInv: '====================================================================
	LOCATE 19, 30: PRINT "Payables Invoices....[Year ]"
	InvShor InvYear
	RETURN
YerCom: '====================================================================
	LOCATE 20, 30: PRINT "Computer Checks......[Year ]"
	PrtChk "YCK"
	RETURN
YerHwc: '====================================================================
	LOCATE 21, 30: PRINT "Handwritten Checks...[Year ]"
	PrtHwc "Y"
	RETURN
AccAct: '====================================================================
	LOCATE 23, 20: PRINT "   Activity for account number:          "
	Rec$ = "": EntryS 23, 55, 2, e, b, Rec$: IF e OR b THEN 10
	OpenFiles
	PageNum = 0: HAccount: T1 = 0
	FOR y = 1 TO LOF(4) \ LEN(Inv)
	GET #6, y, Inx
	IF Inx.Inv > InvYear THEN
	   GET #4, Inx.Inv, Inv
	   GET #1, VAL(Inv.Ven), Ven
	   AccDist
	   FOR x = 0 TO 9
	   IF Act(x) > " 0" THEN
	  IF VAL(Act(x)) = VAL(Rec$) THEN
		 GET #8, VAL(Act(x)), Acc
		 LPRINT USING "#####  "; Inx.Inv;
		 LPRINT USING " & "; Ven.Num; Ven.Nam; Inv.Ref; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk; Acc.Num; Acc.Nam; Amt(x)
		 T1 = T1 + VAL(Amt(x))
		 LineNum = LineNum + 1: IF LineNum > 60 THEN HAccount
	  END IF
	   ELSE
	  EXIT FOR
	   END IF
	   NEXT
	END IF
	NEXT
	LPRINT : LPRINT TAB(134); : LPRINT USING "########.##"; T1
	GOTO 30
PrtVen: '====================================================================
	LOCATE 12, 30: PRINT "Print Vendors' Summary."
	OpenFiles
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0: L6 = 0: L7 = 0: L8 = 0
	PageNum = 0: HVendor
	LOCATE 23, 20: PRINT "       Printing Vendor Number  [   ]     "
	FOR y = 1 TO LOF(3) / LEN(Vnx)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #3, y, Vnx
	GET #1, Vnx.Num, Ven
	IF Ven.Num > "000" THEN
	   LOCATE 23, 52: PRINT Ven.Num;
	   IF LineNum > 60 THEN HVendor
	   LPRINT USING "&  "; Ven.Num; Ven.Nam;
	   LPRINT USING "#######,.##-"; Ven.Mbb / 100; Ven.Mgr / 100; (Ven.Mds + Ven.Mcr) / 100; Ven.Mpd / 100; (Ven.Mbb + Ven.Mgr - Ven.Mds - Ven.Mcr - Ven.Mpd) / 100; : LPRINT "   ";
	   LPRINT USING "########,.##-"; Ven.Ybb / 100; Ven.Ygr / 100; (Ven.Yds + Ven.Ycr) / 100; Ven.Ypd / 100; (Ven.Ybb + Ven.Ygr - Ven.Yds - Ven.Ycr - Ven.Ypd) / 100
	   L1 = L1 + Ven.Mbb: L2 = L2 + Ven.Mgr: L3 = L3 + Ven.Mds + Ven.Mcr: L4 = L4 + Ven.Mpd
	   L5 = L5 + Ven.Ybb: L6 = L6 + Ven.Ygr: L7 = L7 + Ven.Yds + Ven.Ycr: L8 = L8 + Ven.Ypd
	   LineNum = LineNum + 1
	END IF
	NEXT y
	LPRINT
	LPRINT TAB(36); : LPRINT USING "#######,.##-"; L1 / 100; L2 / 100; L3 / 100; L4 / 100; (L1 + L2 - L3 - L4) / 100;
	LPRINT TAB(99); : LPRINT USING "########,.##-"; L5 / 100; L6 / 100; L7 / 100; L8 / 100; (L5 + L6 - L7 - L8) / 100
	RETURN
Aging: '=====================================================================
	LOCATE 13, 30: PRINT "Print Aging of Payables."
	IF Sort THEN InvSort
	OpenFiles
	f = 1: PageNum = 0: HAging
	T1 = 0: T2 = 0: T3 = 0: T4 = 0: T5 = 0: T6 = 0: T7 = 0: T8 = 0: T9 = 0: T0 = 0: T10 = 0
	TA = 0: TB = 0: TC = 0: TD = 0: TE = 0: TF = 0: TG = 0: TH = 0: TI = 0: TJ = 0: TK = 0
	LOCATE 23, 20: PRINT " Aging of Payables report for "; DATE$; " "
	FOR x = 1 TO LOF(6) \ LEN(Inx)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #6, x, Inx
	IF Inx.Mrk = " " THEN
	   IF p$ <> Inx.Ven THEN
		  IF f THEN f = 0 ELSE GOSUB 532
		  GET #1, VAL(Inx.Ven), Ven
		  LPRINT LEFT$(Ven.Nam, 26); : p$ = Inx.Ven
	   END IF
	   GET #4, Inx.Inv, Inv
	   LPRINT TAB(28); Inv.Chk; " "; Inv.Ddt;
	   LPRINT USING " #####"; Inx.Inv;
	   d = DateDif(DATE$, Inv.Ddt)
	   IF d < -60 THEN y = 56:  T1 = T1 + VAL(Inv.Net): GOTO 530
	   IF d < -45 THEN y = 66:  T2 = T2 + VAL(Inv.Net): GOTO 530
	   IF d < -30 THEN y = 76:  T3 = T3 + VAL(Inv.Net): GOTO 530
	   IF d < -15 THEN y = 86:  T4 = T4 + VAL(Inv.Net): GOTO 530
	   IF d < 0 THEN y = 96:    T5 = T5 + VAL(Inv.Net): GOTO 530
	   IF d = 0 THEN y = 106:   T6 = T6 + VAL(Inv.Net): GOTO 530
	   IF d > 60 THEN y = 156: T10 = T10 + VAL(Inv.Net): GOTO 530
	   IF d > 45 THEN y = 146:  T0 = T0 + VAL(Inv.Net): GOTO 530
	   IF d > 30 THEN y = 136:  T9 = T9 + VAL(Inv.Net): GOTO 530
	   IF d > 15 THEN y = 126:  T8 = T8 + VAL(Inv.Net): GOTO 530
	   IF d > 0 THEN y = 116:   T7 = T7 + VAL(Inv.Net)
530    LPRINT TAB(y); : LPRINT Inv.Net
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HAging
	END IF
	NEXT
	GOSUB 532: LPRINT : LPRINT TAB(55);
	LPRINT USING "######.## "; TA; TB; TC; TD; TE; TF; TG; TH; TI; TJ; TK
	RETURN
532 LPRINT CHR$(27); "G": LPRINT TAB(55); : LPRINT USING "######.## "; T1; T2; T3; T4; T5; T6; T7; T8; T9; T0; T10
	LPRINT CHR$(27); "H";
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
	LineNum = LineNum + 3: IF LineNum > 60 THEN HAging
	RETURN
Requir: '====================================================================
	LOCATE 14, 30: PRINT "Print Cash Requirements."
	SHELL "SORTF Invoices.INX Invoices.DUE /L12 /+4,6 /Q"
	OpenFiles
	f = 1: PageNum = 0: HRequir
	T1 = 0: T2 = 0: T3 = 0: T4 = 0: T5 = 0: T6 = 0: T7 = 0: T8 = 0
	LOCATE 23, 20: PRINT "    Printing Cash Requirements Report    "
	FOR x = 1 TO LOF(7) \ LEN(Inx)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #7, x, Inx
	IF Inx.Mrk = " " THEN
	   IF f THEN f = 0: p$ = Inx.Ddt
	   IF p$ <> Inx.Ddt THEN GOSUB 540
	   GET #4, Inx.Inv, Inv
	   GET #1, VAL(Inv.Ven), Ven
	   LineNum = LineNum + 1: IF LineNum > 58 THEN HRequir
	   LPRINT USING "&  #####"; Ven.Nam; Inx.Inv;
	   LPRINT USING "  &"; Inv.Ref; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk;
	   LPRINT USING "   &"; Inv.Grs; Inv.Dis; Inv.Crd; Inv.Net
	   T1 = T1 + VAL(Inv.Grs): T2 = T2 + VAL(Inv.Dis): T3 = T3 + VAL(Inv.Crd): T4 = T4 + VAL(Inv.Net)
	END IF
	NEXT x
	GOSUB 540
	LPRINT TAB(96); : LPRINT USING "#######,.##"; T5; T6; T7; T8; T8
	RETURN
540 T5 = T5 + T1: T6 = T6 + T2: T7 = T7 + T3: T8 = T8 + T4
	LPRINT : LPRINT CHR$(27); "G"; TAB(97);
	LPRINT USING "#######,.##"; T1; T2; T3; T4; T8;
	LPRINT "  "; Inv.Ddt; CHR$(27); "H": LPRINT
	LineNum = LineNum + 3: p$ = Inx.Ddt: T1 = 0: T2 = 0: T3 = 0: T4 = 0
	RETURN
ChkReg: '====================================================================
	IF Sort THEN InvSort
	OpenFiles
	T1 = 0: T2 = 0: T3 = 0: T4 = 0: T5 = 0: T6 = 0: T7 = 0: T8 = 0
	i = 0: f = 1: ErrSw = 0: Dat$ = "": Chk$ = "": CLS : COLOR 3, 0
	LOCATE 11, 28: PRINT "     P R I N T I N G     "
	LOCATE 12, 28: PRINT " PAYABLES CHECK REGISTER "
	LOCATE 14, 28: PRINT "   Due Date: mm-dd-yyyy  "
	LOCATE 15, 28: PRINT "   Check Nง:             "
410 EntryD 14, 41, e, b, Dat$:       IF e THEN 10 ELSE IF b THEN 410
411 EntryN 15, 43, 8, 0, e, b, Chk$: IF e THEN 10 ELSE IF b THEN 410
	Due$ = RIGHT$(Dat$, 2) + LEFT$(Dat$, 2) + MID$(Dat$, 4, 2)
	LChk = VAL(Chk$)
	PageNum = 0: HRegister Dat$
	CLOSE #9: OPEN "INVOICES.CCK" FOR OUTPUT AS #9
	FOR x = 1 TO LOF(6) \ LEN(Inx)
	GET #6, x, Inx
	IF Inx.Mrk = " " AND Inx.Ddt <= Due$ THEN
	   GET #4, Inx.Inv, Inv
	   IF LEFT$(Inv.Chk, 1) <> "H" THEN
		  GET #1, VAL(Inv.Ven), Ven
		  LOCATE 18, 26: PRINT USING "Vendor [&]  Invoice [#####]"; Ven.Num; Inx.Inv
		  IF f THEN f = 0: GOSUB 414
		  IF p$ <> Inx.Ven OR i > 12 THEN GOSUB 412: GOSUB 414
		  IF T4 + VAL(Inv.Net) > 99999.99 THEN GOSUB 412: GOSUB 414
		  LPRINT TAB(34); : LPRINT USING "##### "; Inx.Inv;
		  LPRINT USING "& "; Inv.Ref; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk;
		  LPRINT USING "  & "; Inv.Grs; Inv.Dis; Inv.Crd; Inv.Net
		  T1 = T1 + VAL(Inv.Grs): T2 = T2 + VAL(Inv.Dis): T3 = T3 + VAL(Inv.Crd): T4 = T4 + VAL(Inv.Net)
		  PRINT #9, USING ";######"; Inx.Inv;
		  i = i + 1
		  LineNum = LineNum + 1: IF LineNum > 58 THEN HRegister Dat$
	   END IF
	END IF
	NEXT x
	GOSUB 412: LPRINT TAB(94);
	LPRINT USING "#######,.##"; T5; T6; T7; T8
	IF ErrSw THEN CLOSE #9: KILL "INVOICES.CCK"
	GOTO 10
412 T5 = T5 + T1: T6 = T6 + T2: T7 = T7 + T3: T8 = T8 + T4
	Chk$ = "00000000" + LTRIM$(STR$(LChk))
	Chk$ = "C " + RIGHT$(Chk$, 8)
	LPRINT CHR$(27); "G": LPRINT TAB(94);
	LPRINT USING "#######,.##"; T1; T2; T3; T4; T4;
	LPRINT "   "; Chk$
	LPRINT CHR$(27); "H"
	LineNum = LineNum + 3: IF LineNum > 58 THEN HRegister Dat$
	IF i > 0 THEN
	   FOR y = i TO 12: PRINT #9, USING ";######"; 0; : NEXT
	   PRINT #9, USING ";######.##"; T4
	END IF
	IF T4 < 0 THEN WinMssg "Check Nง " + Chk$ + " is negative": ErrSw = 1
	LChk = LChk + 1
	RETURN
414 PRINT #9, USING "#########;####"; LChk; VAL(Inv.Ven);
	T1 = 0: T2 = 0: T3 = 0: T4 = 0: i = 0: p$ = Inv.Ven
	IF LineNum > 58 THEN HRegister Dat$
	LPRINT Ven.Num; " "; Ven.Nam;
	RETURN
Checks: '=====================================================================
	LOCATE 16, 30: PRINT "Print Payables Checks."
	IF ErrSw = 1 THEN WinMssg "Found negative check": GOTO 10
	IF Sort THEN WinMssg "Invoices were modified": GOTO 10
	LOCATE 23, 20: PRINT "Press [*] to print checks or ESC to exit.": HitKey f$
	IF f$ <> "*" THEN 10
	OpenFiles
	CLOSE #9: OPEN "INVOICES.CCK" FOR INPUT AS #9
	T1 = 0: T2 = 0: T3 = 0: T4 = 0
420 IF EOF(9) THEN 424
	INPUT #9, LChk, Vin, InN(1), InN(2), InN(3), InN(4), InN(5), InN(6), InN(7), InN(8), InN(9), InN(10), InN(11), InN(12), InN(13), TAmt
	Chk$ = "00000000" + LTRIM$(STR$(LChk))
	Chk$ = "C " + RIGHT$(Chk$, 8)
	GET #1, Vin, Ven
	LPRINT CHR$(27); "@"; CHR$(27); "E";
	LPRINT
	LPRINT
	LPRINT
	FOR x = 1 TO 13
	IF InN(x) > 0 THEN
	   GET #4, InN(x), Inv '
	   GET #5, InN(x), Inx '
	   Inv.Chk = Chk$      '
	   Inv.Pdt = DATE$     '------------------------[post invoice,index]----
	   Inx.Mrk = "C"       '
	   PUT #4, InN(x), Inv '
	   PUT #5, InN(x), Inx '

	   LPRINT Inv.Bdt; TAB(29); Inv.Ref; "           ";
	   LPRINT USING "######,.##"; VAL(Inv.Grs); VAL(Inv.Dis) + VAL(Inv.Crd); VAL(Inv.Net)

	   T1 = T1 + VAL(Inv.Grs)
	   T2 = T2 + VAL(Inv.Dis) + VAL(Inv.Crd)
	   T3 = T3 + VAL(Inv.Net)
	   
	   AccDist
	   FOR y = 0 TO 9
	   IF Act(y) > " 0" THEN
		  GET #8, VAL(Act(y)), Acc              '
		  GET #8, VAL(Acc.Crd), Acc             '
		  Acc.Mdb = Acc.Mdb + VAL(Amt(y)) * 100 '---[debit payable]---------
		  Acc.Ydb = Acc.Ydb + VAL(Amt(y)) * 100 '
		  PUT #8, LOC(8), Acc                   '
	   ELSE
		  EXIT FOR
	   END IF
	   NEXT y
	ELSE
	   LPRINT
	END IF
	NEXT x
	LPRINT TAB(31); "Account No"
	LPRINT USING "& "; DATE$; Inv.Chk; "       "; Ven.Act;
	LPRINT TAB(50); : LPRINT USING "######,.##"; T1; T2; T3

	T1 = 0: T2 = 0: T3 = 0
	LOCATE 23, 20: PRINT USING " Printing check Nง: & $#####.## "; Inv.Chk; TAmt
	Picture 23, 52, 8, f$: T4 = T4 + TAmt
	NumWord f$, Word$
   
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT TAB(10); Word$
	LPRINT
	LPRINT
	LPRINT TAB(10); Ven.Nam; TAB(41); DATE$; " "; RIGHT$(Inv.Chk, 8); "     "; : LPRINT USING "$$#####,.##"; TAmt
	LPRINT
	LPRINT
	LPRINT TAB(6); Ven.Nam
	LPRINT TAB(6); Ven.Add
	LPRINT TAB(6); Ven.Str
	LPRINT TAB(6); RTRIM$(Ven.Cty); ", "; Ven.Sta; " "; Ven.Zip
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT

	Ven.Mpd = Ven.Mpd + VAL(f$) * 100
	Ven.Ypd = Ven.Ypd + VAL(f$) * 100
	PUT #1, Vin, Ven '------------------------------[post vendor]-----------
	GOTO 420

424 LOCATE 23, 20: PRINT USING "   Printed Checks Totaling: $######.##   "; T4
	Picture 23, 49, 9, f$
	GET #8, 1, Acc
	Acc.Mcr = Acc.Mcr + VAL(f$) * 100
	Acc.Ycr = Acc.Ycr + VAL(f$) * 100
	PUT #8, 1, Acc '--------------------------------[credit Cash-01]--------
	CLS : CLOSE : SHELL "COPY INVOICES.MCK+INVOICES.CCK": KILL "INVOICES.CCK"
	InvSort
	GOTO 10
Vendor: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	IF Sort THEN InvSort
	OpenFiles
	CALL MhDisplayByName(0, 0, "Vendors", e)
	Bot = 1: Top = LOF(3) / LEN(Vnx): VenDisp x
	r = 2: n = 1: m = 4: HiLite m, 1
600 IF n > 0 THEN GET #3, n, Vnx
	COLOR 11, 1: LOCATE r, 2: PRINT USING "###ณ&"; Vnx.Num; Vnx.Ven
	KeyHit n$
	COLOR 0, 3:  LOCATE r, 2: PRINT USING "###ณ&"; Vnx.Num; Vnx.Ven
	IF n$ = CHR$(13) THEN n$ = MID$(" Add  Modify  Erase  Display  List  Invoices ", 1 + Menu(m, 1), 1)
	SELECT CASE n$
	CASE CHR$(27) 'Esc
	GOTO 10
	CASE CHR$(0) + "M" 'right
	HiLite m, 0
	m = m + 1: IF m > 6 THEN m = 1
	HiLite m, 1
	CASE CHR$(0) + "K" 'left
	HiLite m, 0
	m = m - 1: IF m < 1 THEN m = 6
	HiLite m, 1
	CASE CHR$(0) + "P" 'Dn
	IF n + 1 > Top THEN 600 ELSE n = n + 1
	IF r + 1 > 23 THEN CALL Scroll(2, 2, 33, 23, 1) ELSE r = r + 1
	CASE CHR$(0) + "H" 'Up
	IF n - 1 < 1 THEN 600 ELSE n = n - 1
	IF r - 1 < 2 THEN CALL BkScroll(2, 2, 33, 23, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
	Bot = n - r - 20
	IF Bot < 1 THEN Bot = 1 ELSE VenDisp x: n = x - 23 + r
	CASE CHR$(0) + "Q" 'PgDn
	Bot = n - r + 24
	IF Top - 21 < Bot THEN Bot = Top - 21
	VenDisp x: n = x + r - 23
	CASE CHR$(0) + "G" 'Home
	Bot = 1: VenDisp x: n = r - 1
	CASE CHR$(0) + "O" 'End
	Bot = Top - 21
	IF Bot < 1 THEN Bot = 1 ELSE VenDisp x: n = Top + r - 23
	CASE "L"
	HiLite m, 0
	m = 5: HiLite m, 1
	PageNum = 0: HVenList
	FOR y = 1 TO LOF(3) / LEN(Vnx)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #3, y, Vnx
	GET #1, Vnx.Num, Ven
	IF Ven.Nam > "!" THEN
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HVenList
	   LPRINT USING "&   "; Ven.Num; Ven.Nam; Ven.Add; Ven.Str; Ven.Cty; Ven.Sta; Ven.Zip; Ven.Cnt; Ven.Ord; Ven.Tel; Ven.Fax; Ven.Act; Ven.Dis + " n " + Ven.Trm
	END IF
	NEXT
	CASE "E"
	HiLite m, 0
	m = 3: HiLite m, 1
	VenShow
	L1 = Ven.Ybb + Ven.Ygr + Ven.Yds + Ven.Ycr + Ven.Ypd
	IF L1 <> 0 THEN
	   WinMssg "Active vendor, no erase"
	ELSE
	   IF Confirm THEN
		  Vnx.Ven = "þþþþþþþþþþþþþþþþþþþþþþþþþþþþ"
		  PUT #2, Vnx.Num, Vnx
		  PUT #1, Vnx.Num
		  VenShow
		  VenSort
		  VenDisp x
	   END IF
	END IF
	CASE "D"
	HiLite m, 0
	m = 4: HiLite m, 1
	VenShow
	CASE "A"
	HiLite m, 0
	m = 1: HiLite m, 1
	y = 0
	FOR z = 1 TO Top: GET #2, z, Vnx
	IF LEFT$(Vnx.Ven, 1) = "þ" THEN y = z: EXIT FOR
	NEXT
	IF y = 0 THEN Vnx.Num = 1 + LOF(1) \ LEN(Ven)
	IF Vnx.Num > 999 THEN WinMssg "Vendors Master is full" ELSE VenEdit
	CASE "M"
	HiLite m, 0
	m = 2: HiLite m, 1
	VenEdit
	CASE "I"
	HiLite m, 0
	m = 6: HiLite m, 1
	VenShow
	InvFind Ven.Num, b, e
	IF b THEN WinInv b, e ELSE WinMssg "No invoices for this vendor"
	CASE ELSE
	WinMssg "Select from highlited keys"
	END SELECT
	GOTO 600
Verify: '=======================================================================
	IF Sort THEN InvSort
	OpenFiles
	CLS : COLOR 15, 4: f = 1: L1 = 0: L2 = 0
	WIDTH 80, 25
	LOCATE 25, 1: PRINT "NumÛ      Vendor Name           Û   Master   Û  Invoices  Û    Checking Files   ": COLOR 3, 0
	GET #6, 1, Inx: p$ = Inx.Ven
	FOR x = 1 TO LOF(6) \ LEN(Inx)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #6, x, Inx
	IF p$ <> Inx.Ven THEN GOSUB 700
	IF Inx.Mrk < "!" THEN
	   GET #4, Inx.Inv, Inv
	   L1 = L1 + VAL(Inv.Net) * 100
	END IF
	NEXT
	GOSUB 700
	WinMssg "Hit any key to return to menu"
	GOTO 30
700 GET #1, VAL(p$), Ven
	PRINT USING "&ณ&ณ"; Ven.Num; Ven.Nam;
	L2 = Ven.Ybb + Ven.Ygr - Ven.Yds - Ven.Ycr - Ven.Ypd
	PRINT USING "########,.##ณ"; L2 / 100; L1 / 100;
	IF L1 <> L2 THEN
	   PRINT "    *** ERROR ***    "; : BEEP: KeyHit f$
	ELSE
	   PRINT "                     ";
	END IF
	L1 = 0
	L2 = 0
	p$ = Inx.Ven
	RETURN
RevCom: '====================================================================
	OpenFiles
	CALL MhDisplayByName(0, 0, "Cancel", e)
	File$ = "MCK": Found = 0: Chk$ = "        "
	ERASE InN: T1 = 0: T2 = 0: T3 = 0: T4 = 0
440 EntryN 24, 33, 8, 0, e, b, Chk$: IF e OR b THEN 30
442 CLOSE #9: OPEN "INVOICES." + File$ FOR RANDOM AS #9 LEN = 117
	FIELD #9, 10 AS A4$, 5 AS B4$, 91 AS C4$, 11 AS D4$
	COLOR 14, 4
	FOR x = 1 TO LOF(9) / 117
	GET #9, x
	LOCATE 24, 54: PRINT USING "Searching: [&][#######]"; File$; VAL(A4$);
	IF VAL(Chk$) = VAL(A4$) THEN
	   COLOR 7, 1
	   FOR y = 0 TO 12
	   InN(y) = VAL(MID$(C4$, 1 + 7 * y, 7))
	   IF InN(y) > 0 THEN
		  GET #4, InN(y), Inv
		  LOCATE y + 2, 3: PRINT USING "#####ณ&ณ&ณ&ณ&ณ&ณ&ณ&"; InN(y); Inv.Ref; Inv.Bdt; Inv.Ddt; Inv.Grs; Inv.Dis; Inv.Crd; Inv.Net
		  T1 = T1 + VAL(Inv.Grs): T2 = T2 + VAL(Inv.Dis): T3 = T3 + VAL(Inv.Crd): T4 = T4 + VAL(Inv.Net)
	   ELSE
		  EXIT FOR
	   END IF
	   NEXT y
	   LOCATE 16, 41: PRINT USING "ณ#####.##"; T1; T2; T3; T4
	   IF VAL(B4$) = 0 THEN
		  WinMssg "This is a cancled check": GOTO 30
	   ELSE
		  GET #1, VAL(B4$), Ven
		  LOCATE 22, 3: COLOR 15, 2
		  PRINT USING " &    &    &    $$#####,.##"; Ven.Nam; Inv.Pdt; Inv.Chk; VAL(D4$);
		  Found = 1: EXIT FOR
	   END IF
	END IF
	NEXT x
	COLOR 14, 4
	IF Found THEN '-------------------------------------Post----------------
	   LOCATE 24, 2: PRINT "Press [*] to reverse the check or [Any] key to exit.           ";
	   KeyHit f$: IF f$ <> "*" THEN 30
	   LOCATE 24, 60: PRINT "ฎR E V E R S I N Gฏ";
	   Sort = 1
	   Ven.Mpd = Ven.Mpd - VAL(D4$) * 100
	   Ven.Ypd = Ven.Ypd - VAL(D4$) * 100
	   PUT #1, VAL(B4$), Ven '----------------------[Vendor]----------------
	   GET #8, 1, Acc
	   Acc.Mcr = Acc.Mcr - VAL(D4$) * 100
	   Acc.Ycr = Acc.Ycr - VAL(D4$) * 100
	   PUT #8, 1, Acc '-----------------------------[Reverse Cash]----------
	   RSET B4$ = SPACE$(5)
	   RSET C4$ = SPACE$(84)
	   RSET D4$ = SPACE$(9) + CHR$(13) + CHR$(10)
	   PUT #9, x '----------------------------------[Invoices.?Ck]----------
	   FOR y = 0 TO 12
	   IF InN(y) > 0 THEN
	  GET #4, InN(y), Inv
	  AccDist
	  FOR z = 0 TO 9
	  IF Act(z) > " 0" THEN
		 GET #8, VAL(Act(z)), Acc
		 GET #8, VAL(Acc.Crd), Acc
		 Acc.Mdb = Acc.Mdb - VAL(Amt(z)) * 100
		 Acc.Ydb = Acc.Ydb - VAL(Amt(z)) * 100
		 PUT #8, LOC(8), Acc '------------------[Reverse payable]-------
	  ELSE EXIT FOR
	  END IF
	  NEXT z
	  Inv.Pdt = "**-**-****"
	  Inv.Chk = "*Not Paid*"
	  PUT #4, InN(y), Inv '---------------------[Invoice]---------------
	  GET #5, InN(y), Inx
	  Inx.Mrk = " "
	  PUT #5, InN(y), Inx '---------------------[Index]-----------------
	   ELSE EXIT FOR
	   END IF
	   NEXT y
	ELSE
	   IF File$ = "MCK" THEN File$ = "YCK": GOTO 442
	   WinMssg "Check not found": KeyHit f$
	END IF
	GOTO 30
RevHwc: '====================================================================
	CALL MhDisplayByName(0, 0, "Invoice", e)
	Rec$ = "     ": OpenFiles
450 EntryN 2, 20, 5, 0, e, b, Rec$: IF e THEN 10 ELSE IF b THEN 450
	Rec = VAL(Rec$)
	IF Rec > LOF(4) \ LEN(Inv) OR Rec < 1 THEN WinMssg "No such invoice": GOTO 450
	InvDisp Rec
	IF LEFT$(Inv.Chk, 1) <> "W" THEN WinMssg "Not a handwritten check": GOTO 450
	COLOR 15, 4
	LOCATE 22, 1: PRINT " Press [*] to cancel this check "
	LOCATE 23, 1: PRINT " or [Any] key to return to menu "
	KeyHit f$: IF f$ <> "*" THEN 30
	LOCATE 24, 1: PRINT "     ฎ R E V E R S I N G ฏ      ";
	Sort = 1
	Ven.Mpd = Ven.Mpd - VAL(Inv.Net) * 100
	Ven.Ypd = Ven.Ypd - VAL(Inv.Net) * 100
	PUT #1, VAL(Ven.Num), Ven '---------------------[Vendor]----------------
	GET #8, 1, Acc
	Acc.Mcr = Acc.Mcr - VAL(Inv.Net) * 100
	Acc.Ycr = Acc.Ycr - VAL(Inv.Net) * 100
	PUT #8, 1, Acc '--------------------------------[Reverse Cash]----------
	GET #4, Rec, Inv
	AccDist
	FOR z = 0 TO 9
	IF Act(z) > " 0" THEN
	   GET #8, VAL(Act(z)), Acc
	   GET #8, VAL(Acc.Crd), Acc
	   Acc.Mdb = Acc.Mdb - VAL(Amt(z)) * 100
	   Acc.Ydb = Acc.Ydb - VAL(Amt(z)) * 100
	   PUT #8, LOC(8), Acc '------------------------[Reverse payable]-------
	ELSE EXIT FOR
	END IF
	NEXT z
	Inv.Pdt = "**-**-****"
	Inv.Chk = "*Not Paid*"
	PUT #4, Rec, Inv '------------------------------[Invoice]---------------
	GET #5, Rec, Inx
	Inx.Mrk = " "
	PUT #5, Rec, Inx '------------------------------[Index]-----------------
	GOTO 30
Purge: '===================================================================
	OpenFiles
	IF Confirm = 0 GOTO 30
	CLOSE #7: OPEN "..\IN\Inventor.Pur" FOR RANDOM AS #7 LEN = LEN(Pur)
	CLOSE #8: OPEN "..\IN\Inventor.New" FOR RANDOM AS #8 LEN = LEN(Pur)
	CLOSE #9: OPEN "..\AP\Invoices.New" FOR RANDOM AS #9 LEN = LEN(Inv)
	CLOSE #6: OPEN "..\AP\InvIndex.New" FOR RANDOM AS #6 LEN = LEN(Inx)

	i = 0
	p = 0
	FOR x = 1 TO LOF(5) / LEN(Inx)
	GET #5, x, Inx
	IF Inx.Mrk < "!" THEN
	   i = i + 1
	   GET #4, x, Inv
	   IF Inv.Pur > "    0" THEN
		  p = p + 1
		  GET #7, VAL(Inv.Pur), Pur
		  Pur.Inv = RIGHT$(STR$(100000 + i), 5)
		  PUT #8, p, Pur
		  Inv.Pur = RIGHT$(STR$(100000 + p), 5)
	   ELSE
		  Inv.Pur = "     "
	   END IF
	   Inx.Inv = i
	   PUT #9, i, Inv
	   PUT #6, i, Inx
	   PRINT USING "Keeping invoice ##### and purchase &"; i; Inv.Pur
	END IF
	NEXT

	CLOSE
	KILL "..\IN\Inventor.Pur": NAME "..\IN\Inventor.New" AS "..\IN\Inventor.Pur"
	KILL "..\AP\Invoices.Mas": NAME "..\AP\Invoices.New" AS "..\AP\Invoices.Mas"
	KILL "..\AP\Invoices.Inx": NAME "..\AP\InvIndex.New" AS "..\AP\Invoices.Inx"
	InvSort
	LastInv = LOF(3) / LEN(Inv)
	EndInv = LastInv
	InvMnth = LastInv
	InvYear = LastInv
	GOTO 30
Monthend: '=====================================================================
	IF Sort THEN InvSort
	OpenFiles
	IF InvLast < LOF(4) \ LEN(Inv) THEN WinMssg "There are invoices to be printed": GOTO 10
	LOCATE 23, 20: PRINT "Press [*] to begin closing or ESC to exit": HitKey f$
	IF f$ <> "*" THEN 10
	CLS : GOSUB PrtVen
		  GOSUB Aging
		  GOSUB Requir
		  GOSUB MonInv
		  GOSUB MonCom
		  GOSUB MonHwc
	LOCATE 23, 20: PRINT "Type YES if you wish to close the Month  ": f$ = ""
	EntryU 23, 25, 3, e, b, f$: IF e OR b THEN 10
	IF f$ = "YES" THEN
	   FOR x = 1 TO LOF(1) \ LEN(Ven)
	   GET #1, x, Ven
	   Ven.Mbb = Ven.Mbb + Ven.Mgr - Ven.Mds - Ven.Mcr - Ven.Mpd
	   Ven.Mgr = 0: Ven.Mds = 0: Ven.Mcr = 0: Ven.Mpd = 0
	   PUT #1, x, Ven
	   NEXT
	   FOR x = 1 TO LOF(5) / LEN(Inx)
	   GET #5, x, Inx
	   IF Inx.Mrk = "W" THEN Inx.Mrk = "Y": PUT #5, x, Inx
	   NEXT
	   InvMnth = LOF(4) \ LEN(Inv)
	   CLOSE : SHELL "COPY INVOICES.YCK+INVOICES.MCK"
	   OPEN "INVOICES.MCK" FOR OUTPUT AS #1: CLOSE
	END IF
	GOTO 10
Yearend: '======================================================================
	IF Sort THEN InvSort
	OpenFiles
	WinMssg "Remember to close the MONTH first"
	LOCATE 23, 20: PRINT "Press [*] to begin closing or ESC to exit": HitKey f$
	IF f$ <> "*" THEN 10
	CLS : GOSUB PrtVen
		  GOSUB YerInv
		  GOSUB YerCom
		  GOSUB YerHwc
	LOCATE 23, 20: PRINT "Type YES if you wish to close the YEAR   ": f$ = ""
	EntryU 23, 25, 3, e, b, f$: IF e OR b THEN 10
	IF f$ = "YES" THEN
		FOR x = 1 TO LOF(1) \ LEN(Ven)
		GET #1, x, Ven
		Ven.Ybb = Ven.Ybb + Ven.Ygr - Ven.Yds - Ven.Ycr - Ven.Ypd
		Ven.Ygr = 0: Ven.Yds = 0: Ven.Ycr = 0: Ven.Ypd = 0
		PUT #1, x, Ven
		NEXT
		FOR x = 1 TO LOF(5) / LEN(Inx)
		GET #5, x, Inx
		IF Inx.Mrk = "Y" THEN Inx.Mrk = "L": PUT #5, x, Inx
		NEXT
		InvYear = LOF(4) \ LEN(Inv)
		CLOSE : NAME "INVOICES.YCK" AS "Checks." + RIGHT$(DATE$, 2)
		OPEN "INVOICES.YCK" FOR OUTPUT AS #1: CLOSE
	END IF
	GOTO 10
Errors: '====================================================================
	IF ERR = 53 AND ERL = 414 THEN WinMssg "Sorry, no checks to print": RESUME 10
	WinMssg "Error:" + STR$(ERR) + " Line:" + STR$(ERL)
Terminate: '=================================================================
	IF Sort THEN InvSort
	CLOSE : OPEN "Invoices.Num" FOR OUTPUT AS #1
			WRITE #1, InvLast, InvMnth, InvYear
	CLOSE : CLS : SHELL "Del Invoices.CCK": END
'=============================================================================
DATA "","one "   ,"two "   ,"three "   ,"four "   ,"five "   ,"six "    ,"seven "    ,"eight "   ,"nine "
DATA "","eleven ","twelve ","thirteen ","fourteen ","fifteen ","sixteen ","seventeen ","eighteen ","nineteen "
DATA "","ten "   ,"twenty ","thirty "  ,"forty "  ,"fifty "  ,"sixty "  ,"seventy "  ,"eighty "  ,"ninety "
DATA 1,5, 6,8, 14,7, 21,9, 30,6, 36,10

DEFINT L, T
SUB AccDist ()
	ERASE Act, Amt, oAc, oAm
	FOR x = 0 TO 9
	Act(x) = MID$(Inv.Dst, 1 + 2 * x, 2):  oAc(x) = Act(x)
	Amt(x) = MID$(Inv.Dst, 21 + 8 * x, 8): oAm(x) = Amt(x)
	IF Act(x) < " 0" THEN EXIT FOR
	NEXT x
END SUB

DEFSNG A-B, D-Z
FUNCTION Confirm ()
	PCOPY 0, 1
	SCREEN , , 1, 1
	COLOR 14, 4: LOCATE 18
	LOCATE , 36: PRINT " ษออออออออออออออออออออออป "
	LOCATE , 36: PRINT " บ Press [*] to Confirm บ "
	LOCATE , 36: PRINT " ศออออออออออออออออออออออผ "
	COLOR 12, 0: LOCATE 19
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"
	SOUND 1000, .1: SOUND 800, .3: KeyHit f$
	IF f$ = "*" THEN Confirm = 1 ELSE Confirm = 0
	SCREEN , , 0, 0
END FUNCTION

DEFSNG C
SUB HAccount ()
	Heading
	LPRINT TAB(71); "ACCOUNT  ACTIVITY"
	LPRINT : LPRINT : LPRINT
	LPRINT "  Inv   Ven                                             Invoice       Due         Date       Check                                              "
	LPRINT "  Num   Num       Vendor                   Reference      Date        Date        Paid       Number    No         Account Name           Amount "
	LPRINT " =====  ===  ============================  ==========  ==========  ==========  ==========  ==========  ==  ===========================  ========"
END SUB

SUB HAging ()
	Heading
	LPRINT TAB(71); "AGING OF PAYABLES"
	LPRINT : LPRINT
	LPRINT "                             Check       Due      Inv  _________________P A S T   D U E________________    Due     _______________D U E   L A T E R________________"
	LPRINT "      Vendor Name            Number      Date     Num  >60 Days  >45 Days  >30 Days  >15 Days   >1 Day    Today     >1 Day   >15 Days  >30 Days  >45 Days  >60 Days"
	LPRINT "========================== ========== ========== ===== ========  ========  ========  ========  ========  ========  ========  ========  ========  ========  ========"
END SUB

SUB HChecks ()
	Heading
	LPRINT TAB(63); "CHECK REGISTER for COMPUTER CHECKS"
	LPRINT : LPRINT : LPRINT
	LPRINT "                               Check      Check               Inv                                                              ____________DISTRIBUTION____________"
	LPRINT "      Vendor                   Number     Amount  Check Date  Num  Reference   Due Date    Gross  Discount  Credit  Net Amount  Account Name                Amount "
	LPRINT "============================ ========== ========= ========== ===== ========== ========== ======== ======== ======== ========== ===================================="
END SUB

SUB Heading ()
	 LPRINT CHR$(12); CHR$(27); "@";
	 WIDTH "LPT1:", 255
	 LPRINT CHR$(27); "M"
	 PageNum = PageNum + 1: LineNum = 13
	 LPRINT "Date: "; DATE$; TAB(61); "     Processed Foods Corporation     "; TAB(153); "Page: "; PageNum
	 LPRINT "Time: "; TIME$; TAB(61); "707 Willow Avenue, Knoxville TN 37915"
	 LPRINT :         LPRINT TAB(71); "ACCOUNTS  PAYABLE": LPRINT
END SUB

DEFINT A-Z
SUB HiLite (m, p)
	IF p THEN f = 4: b = 7 ELSE f = 14: b = 1
	LOCATE 25, Menu(m, 1) + 35
	f$ = MID$(" Add  Modify  Erase  Display  List  Invoices ", Menu(m, 1), Menu(m, 2))
	COLOR f, b: PRINT MID$(f$, 1, 2);
	COLOR f - 3, b: PRINT MID$(f$, 3);
END SUB

DEFSNG A-Z
SUB HInvList ()
	Heading
	LPRINT TAB(71); "INVOICE  REGISTER"
	LPRINT : LPRINT : LPRINT
	LPRINT "Ven                                 Inv                Invoice       Due         Date       Check                                                   "
	LPRINT "Num            Vendor               Num   Reference      Date        Date        Paid       Number       Gross     Discount     Credit    Net Amount"
	LPRINT "===  ============================  =====  ==========  ==========  ==========  ==========  ==========   =========  ==========  ==========  =========="
END SUB

SUB HInvoice ()
	Heading
	LPRINT TAB(71); "INVOICE  REGISTER"
	LPRINT : LPRINT : LPRINT
	LPRINT " Inv                                           Invoice      Due        Date      Check                                          ___________DISTRIBUTION____________"
	LPRINT " Num        Vendor                 Reference     Date       Date       Paid      Number     Gross  Discount  Credit  Net Amount  Account Name               Amount "
	LPRINT "===== ============================ ========== ========== ========== ========== ========== ======== ======== ======== ========== ==================================="
END SUB

SUB HitKey (f$)
	DO: f$ = INKEY$: TimeDate: LOOP WHILE f$ = ""
END SUB

SUB HRegister (d$)
	 Heading
	 LPRINT TAB(57); "CHECK REGISTER for Invoices Due by: "; d$
	 LPRINT : LPRINT : LPRINT
	 LPRINT "Ven                               Inv              Invoice      Due        Date      Check                                         Net       Check                 "
	 LPRINT "Num         Vendor Name           Num  Reference     Date       Date       Paid      Number      Gross    Discount    Credit      Amount     Amount   Check Number "
	 LPRINT "=== ============================ ===== ========== ========== ========== ========== ==========  =========  =========  =========  =========  =========  ============="
END SUB

SUB HRequir ()
	 Heading
	 LPRINT TAB(67); "CASH REQUIREMENTS REPORT"
	 LPRINT : LPRINT : LPRINT
	 LPRINT "                               Inv                Invoice       Date        Date       Check                                        Net        Total              "
	 LPRINT "        Vendor Name            Num   Reference      Date        Due         Paid       Number      Gross    Discount    Credit     Amount     To Date    Due Date "
	 LPRINT "============================  =====  ==========  ==========  ==========  ==========  ==========  =========  =========  =========  =========  =========  =========="
END SUB

SUB HVendor ()
	 Heading
	 LPRINT TAB(67); " VENDORS ACCOUNT SUMMARY"
	 LPRINT : LPRINT : LPRINT
	 LPRINT "Ven                                  ______________________M  O  N  T  H_______________________     __________________________Y  E  A  R___________________________"
	 LPRINT "Num       Vendor Name                B. Forward    Gross     Deductions   Payments    Balance       B. Forward      Gross     Deductions    Payments      Balance  "
	 LPRINT "===  ============================    ==========  ==========  ==========  ==========  ==========     ===========  ===========  ===========  ===========  ==========="
END SUB

SUB HVenList ()
	Heading
	LPRINT TAB(70); "  VENDORS LISTING  "
	LPRINT CHR$(15);
	LPRINT
	LPRINT "Num             Vendor                           Name                         Address               City            St      Zip       ";
	LPRINT "Accounts Receivable Contact           Order Contact             Telephone      Facsimile     Account Number     Terms "
	LPRINT "===   ============================   ============================   ============================   ==============   ==   ==========   ";
	LPRINT "============================   ============================   ============   ============   ================   ======="
END SUB

SUB HWritten ()
	Heading
	LPRINT TAB(61); "CHECK REGISTER for HAND WRITTEN CHECKS"
	LPRINT : LPRINT : LPRINT
	LPRINT "                               Check      Check               Inv                                                              ____________DISTRIBUTION____________"
	LPRINT "      Vendor                   Number     Amount  Check Date  Num  Reference   Due Date    Gross  Discount  Credit  Net Amount  Account Name                Amount "
	LPRINT "============================ ========== ========= ========== ===== ========== ========== ======== ======== ======== ========== ===================================="
END SUB

DEFDBL T
FUNCTION InvDis% ()
	T = 0: FOR y = 0 TO 9: T = T + VAL(Amt(y)): NEXT
	IF T > 99999.99 OR T < -9999.99 THEN
	   InvDis% = 1: WinMssg "Distribution exceeds 99999.99"
	ELSE
	   LOCATE 24, 70: PRINT USING "#####.##"; T;
	END IF
END FUNCTION

DEFINT A-Z
SUB InvDisp (n)
	COLOR 10, 1
	GET #4, n, Inv
	GET #1, VAL(Inv.Ven), Ven
	AccDist
	LOCATE 2, 20: PRINT USING "#####"; n;
	LOCATE , 42:  PRINT Ven.Num;
	LOCATE , 48:  PRINT Ven.Nam; "  "
	LOCATE , 48:  PRINT Ven.Add; "  "
	LOCATE , 48:  PRINT Ven.Cty; " "; Ven.Sta; " "; Ven.Zip; "  "
	LOCATE 6, 48: PRINT Inv.Ref; : LOCATE , 70: PRINT Inv.Grs
	LOCATE , 48:  PRINT Inv.Bdt; : LOCATE , 70: PRINT Inv.Dis
	LOCATE , 48:  PRINT Inv.Ddt; : LOCATE , 70: PRINT Inv.Crd
	LOCATE , 48:  PRINT Inv.Pdt
	LOCATE , 48:  PRINT Inv.Chk; : LOCATE , 70: PRINT Inv.Net
	LOCATE 13
	FOR x = 0 TO 9
	IF Act(x) > " 0" THEN
	   GET #8, VAL(Act(x)), Acc
	   LOCATE , 35: PRINT Acc.Num;
	   LOCATE , 40: PRINT Acc.Nam;
	   LOCATE , 70: PRINT Amt(x)
	ELSE
	   COLOR 11, 1: LOCATE , 35: PRINT "ฒฒ ณ ฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒ ณ ฒฒฒฒฒฒฒฒ"
	END IF
	NEXT
	LOCATE 24, 70: PRINT Inv.Net;
END SUB

SUB InvFind (n$, f, e)
   f = 0: e = 0
   b = 1: T = LOF(6) \ LEN(Inx)
   DO UNTIL (T < b)
	  m = (T + b) \ 2: GET #6, m, Inx
	  IF n$ > Inx.Ven THEN b = m + 1 ELSE T = m - 1
   LOOP
   DO WHILE n$ > Inx.Ven AND NOT EOF(6)
	  GET #6, , Inx
   LOOP
   IF n$ = Inx.Ven THEN
	  f = LOC(6): e = f
	  DO WHILE NOT EOF(6)
	 GET #6, , Inx
	 IF n$ = Inx.Ven THEN e = LOC(6) ELSE EXIT DO
	  LOOP
   END IF
END SUB

DEFDBL T
SUB InvLong (b)
	OpenFiles
	IF b = LOF(4) \ LEN(Inv) THEN WinMssg "No new invoices to print": EXIT SUB
	LOCATE 23, 20: PRINT "     Printing invoice number [     ]     "
	PageNum = 0: HInvoice
	FOR y = b + 1 TO LOF(4) \ LEN(Inv)
	   IF INKEY$ = CHR$(27) THEN EXIT FOR ELSE TimeDate
	   GET #4, y, Inv
	   GET #5, y, Inx
	   GET #1, VAL(Inv.Ven), Ven
	   LOCATE 23, 50: PRINT USING "#####"; Inx.Inv
	   LPRINT USING "#####"; Inx.Inv;
	   LPRINT USING " &"; Ven.Nam; Inv.Ref; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk;
	   LPRINT USING " &"; Inv.Grs; Inv.Dis; Inv.Crd;
	   LPRINT "   "; Inv.Net;
	   T1 = T1 + VAL(Inv.Grs): T2 = T2 + VAL(Inv.Dis): T3 = T3 + VAL(Inv.Crd): T4 = T4 + VAL(Inv.Net)
	   AccDist
	   FOR x = 0 TO 9
	   IF Act(x) > " 0" THEN
	  GET #8, VAL(Act(x)), Acc
	  LPRINT TAB(129); Acc.Nam; Amt(x)
	  T5 = T5 + VAL(Amt(x))
	  LineNum = LineNum + 1: IF LineNum > 58 THEN HInvoice
	   END IF
	NEXT x, y
	LPRINT : LPRINT TAB(90); : LPRINT USING "######.##"; T1; T2; T3;
	LPRINT USING "########.##"; T4;
	LPRINT TAB(154); : LPRINT USING "#######.##"; T5
END SUB

DEFSNG A-S, U-Z
FUNCTION InvNet% ()
	LOCATE 10, 70
	T = VAL(Inv.Grs) - VAL(Inv.Dis) - VAL(Inv.Crd)
	IF T > 99999.99 OR T < -9999.99 THEN
	   InvNet% = 1: WinMssg "Net Amount exceeds -9999.99"
	ELSE
	   PRINT USING "#####.##"; T
	   Picture 10, 70, 8, Inv.Net
	END IF
END FUNCTION

DEFINT A-S, U-Z
SUB InvShor (i)
	IF Sort THEN InvSort
	OpenFiles
	LOCATE 23, 20: PRINT "     Printing invoice number [     ]     "
	PageNum = 0: HInvList
	FOR x = 1 TO LOF(1) / LEN(Ven)
	GET #3, x, Vnx: GET #1, Vnx.Num, Ven
	InvFind Ven.Num, b, e
	IF b THEN
	   T1 = 0: T2 = 0: T3 = 0: T4 = 0
	   FOR y = b TO e
	   GET #6, y, Inx
	   IF Inx.Inv > i THEN
	  GET #4, Inx.Inv, Inv
	  GET #1, VAL(Inv.Ven), Ven
	  LOCATE 23, 50: PRINT USING "#####"; Inx.Inv
	  LineNum = LineNum + 1: IF LineNum > 58 THEN HInvList
	  LPRINT USING "&  "; Ven.Num; Ven.Nam;
	  LPRINT USING "#####  "; Inx.Inv;
	  LPRINT USING "&  "; Inv.Ref; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk;
	  LPRINT USING "  &  "; Inv.Grs; Inv.Dis; Inv.Crd; Inv.Net
	  T1 = T1 + VAL(Inv.Grs): T2 = T2 + VAL(Inv.Dis): T3 = T3 + VAL(Inv.Crd): T4 = T4 + VAL(Inv.Net)
	   END IF
	   NEXT y
	   IF T1 > 0 THEN
	  LPRINT : LPRINT TAB(101); : LPRINT USING " ########.##"; T1; T2; T3; T4: LPRINT
	  T5 = T5 + T1: T6 = T6 + T2: T7 = T7 + T3: T8 = T8 + T4
	  LineNum = LineNum + 3
	   END IF
	END IF
	NEXT
	LPRINT : LPRINT TAB(101); : LPRINT USING " ########.##"; T5; T6; T7; T8
END SUB

DEFSNG A-Z
SUB InvSort ()
	CLOSE : Sort = 0
	SHELL "SORTF Invoices.INX Invoices.VEN /L12 /+1,10 /Q"
	OpenFiles
END SUB

SUB KeyHit (f$)
	DO: f$ = INKEY$: LOOP WHILE f$ = ""
	f$ = UCASE$(f$)
END SUB

SUB NumWord (n$, w$)
	 w$ = ""
	 w$ = RIGHT$(n$, 2) + " cents."
	 d$ = MID$(n$, 4, 2)
	 IF d$ > "10" AND d$ < "20" THEN
	w$ = Digits(2, VAL(RIGHT$(d$, 1))) + "dollars & " + w$
	 ELSEIF d$ > " 0" THEN
	w$ = Digits(3, VAL(LEFT$(d$, 1))) + Digits(1, VAL(RIGHT$(d$, 1))) + "dollars & " + w$
	 END IF
	 IF VAL(d$) > 0 AND LEFT$(n$, 3) > "  0" THEN w$ = "and " + w$
	 d$ = MID$(n$, 3, 1)
	 IF d$ > "0" THEN w$ = Digits(1, VAL(d$)) + "hundred " + w$
	 d$ = LEFT$(n$, 2)
	 IF d$ > "10" AND d$ < "20" THEN
	w$ = Digits(2, VAL(RIGHT$(d$, 1))) + "thousand " + w$
	 ELSEIF d$ > " 0" THEN
	w$ = Digits(3, VAL(LEFT$(d$, 1))) + Digits(1, VAL(RIGHT$(d$, 1))) + "thousand " + w$
	 END IF
END SUB

SUB OpenFiles ()
	CLOSE
	OPEN "Vendors .MAS" FOR RANDOM AS #1 LEN = LEN(Ven)
	OPEN "Vendors .INX" FOR RANDOM AS #2 LEN = LEN(Vnx)
	OPEN "Vendors .NAM" FOR RANDOM AS #3 LEN = LEN(Vnx)
   
	OPEN "Invoices.MAS" FOR RANDOM AS #4 LEN = LEN(Inv)
	OPEN "Invoices.INX" FOR RANDOM AS #5 LEN = LEN(Inx)
	OPEN "Invoices.VEN" FOR RANDOM AS #6 LEN = LEN(Inx)
	OPEN "Invoices.DUE" FOR RANDOM AS #7 LEN = LEN(Inx)
	
	OPEN "Accounts.DTA" FOR RANDOM AS #8 LEN = LEN(Acc)

	OPEN "Index   .HWC" FOR RANDOM AS #12 LEN = LEN(Hwc)
END SUB

DEFINT A-S, U-Z
DEFDBL T
SUB PrtChk (f$)
	OpenFiles
	LOCATE 23, 20: PRINT " Printing Computer Check Nง:             "
	PageNum = 0: HChecks
	CLOSE #9: OPEN "INVOICES." + f$ FOR RANDOM AS #9 LEN = 117
	FIELD #9, 10 AS A4$, 5 AS B4$, 91 AS C4$, 11 AS D4$
	FOR x = 1 TO LOF(9) \ 110
	GET #9, x
	Chk$ = "C " + RIGHT$("00000000" + LTRIM$(LEFT$(A4$, 9)), 8)
	LOCATE 23, 50: PRINT Chk$;
	IF VAL(B4$) > 0 THEN
	   GET #1, VAL(B4$), Ven
	   GET #4, VAL(LEFT$(C4$, 6)), Inv
	   LPRINT USING "& &  #####.## &"; Ven.Nam; Inv.Chk; VAL(D4$); Inv.Pdt;
	   T0 = T0 + VAL(D4$)
	   FOR y = 0 TO 12
	   InN = VAL(MID$(C4$, 1 + 7 * y, 7))
	   IF InN > 0 THEN
	  GET #4, InN, Inv
	  LPRINT TAB(62); : LPRINT USING "#####"; InN;
	  LPRINT USING " &"; Inv.Ref; Inv.Ddt; Inv.Grs; Inv.Dis; Inv.Crd;
	  LPRINT "  "; Inv.Net;
	  T1 = T1 + VAL(Inv.Grs): T2 = T2 + VAL(Inv.Dis): T3 = T3 + VAL(Inv.Crd): T4 = T4 + VAL(Inv.Net)
	  AccDist
	  FOR z = 0 TO 9
	  IF Act(z) > " 0" THEN
		  GET #8, VAL(Act(z)), Acc
		  LPRINT TAB(128); Acc.Nam; " "; Amt(z)
		  T5 = T5 + VAL(Amt(z))
		  LineNum = LineNum + 1: IF LineNum > 60 THEN HChecks
	  ELSE
		  EXIT FOR
	  END IF
	  NEXT z
	   END IF
	   NEXT y
	ELSE
	   LPRINT "****** Canceled Check ****** "; Chk$
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HChecks
	END IF
	NEXT x
	LPRINT
	LPRINT USING "& ######.## &"; SPACE$(39); T0; SPACE$(38);
	LPRINT USING "######.##"; T1; T2; T3;
	LPRINT USING "#######.## & ######.##"; T4; SPACE$(27); T5
END SUB

SUB PrtHwc (f$)
	OpenFiles
	FOR x = 1 TO LOF(5) \ LEN(Inx)
	GET #5, x, Inx
	IF Inx.Mrk = f$ THEN
	   GET #4, Inx.Inv, Inv
	   Hwc.Chk = Inv.Chk
	   Hwc.Inv = Inx.Inv
	   PUT #12, , Hwc
	END IF
	NEXT
	CLOSE
	SHELL "SortF Index.HWC Index /L12 /Q"
	KILL "Index.HWC"
	NAME "Index" AS "Index.HWC"
	OpenFiles
	PageNum = 0: HWritten
	LOCATE 23, 20: PRINT "Printing HandWritten Check Nง            "
	FOR x = 1 TO LOF(12) \ LEN(Hwc)
	GET #12, x, Hwc
	GET #4, Hwc.Inv, Inv
	GET #1, VAL(Inv.Ven), Ven
	LOCATE 23, 50: PRINT Inv.Chk
	IF LineNum > 60 THEN HWritten
	LPRINT USING "& "; Ven.Nam; Inv.Chk; ""; Inv.Net; Inv.Pdt;
	LPRINT USING "#####"; Hwc.Inv;
	LPRINT USING " &"; Inv.Ref; Inv.Ddt; Inv.Grs; Inv.Dis; Inv.Crd;
	LPRINT "  "; Inv.Net;
	T1 = T1 + VAL(Inv.Grs): T2 = T2 + VAL(Inv.Dis): T3 = T3 + VAL(Inv.Crd): T4 = T4 + VAL(Inv.Net)
	AccDist
	FOR z = 0 TO 9
	IF Act(z) > " 0" THEN
	   GET #8, VAL(Act(z)), Acc
	   LPRINT TAB(128); Acc.Nam; " "; Amt(z)
	   T5 = T5 + VAL(Amt(z))
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HChecks
	ELSE
	   EXIT FOR
	END IF
	NEXT z
	NEXT
	LPRINT : LPRINT TAB(89);
	LPRINT USING "######.##"; T1; T2; T3;
	LPRINT USING "#######.## & ######.##"; T4; SPACE$(27); T5
END SUB

DEFSNG A-Z
SUB TimeDate ()
 COLOR 0, 7
 LOCATE 10, 22, 0: PRINT " "; DATE$; " "
 T$ = " " + LEFT$(TIME$, 2)
 IF TIME$ > "11:59:59" THEN f$ = "pm " ELSE f$ = "am "
 IF TIME$ > "12:59:59" THEN T$ = " " + STR$(VAL(T$) - 12)
 LOCATE 10, 47: PRINT T$; MID$(TIME$, 3, 6) + f$
END SUB

DEFINT A-Z
SUB VenDisp (x)
	LOCATE 2: COLOR 0, 3
	FOR x = Bot TO Top
	GET #3, x, Vnx
	LOCATE , 2: PRINT USING "###ณ&"; Vnx.Num; Vnx.Ven
	IF CSRLIN = 24 THEN EXIT FOR
	NEXT
END SUB

SUB VenEdit ()
	VenShow
	p$ = Ven.Nam

610 EntryS 2, 52, 28, e, b, Ven.Nam:    IF e THEN 630 ELSE IF b THEN 610
	IF Ven.Nam < "!" THEN WinMssg "Name must not be blank": GOTO 610
611 EntryS 3, 52, 28, e, b, Ven.Add:    IF e THEN 630 ELSE IF b THEN 610
612 EntryS 4, 52, 28, e, b, Ven.Str:    IF e THEN 630 ELSE IF b THEN 611
613 EntryS 5, 52, 14, e, b, Ven.Cty:    IF e THEN 630 ELSE IF b THEN 612
614 EntryU 5, 67, 2, e, b, Ven.Sta:     IF e THEN 630 ELSE IF b THEN 613
615 EntryU 5, 70, 10, e, b, Ven.Zip:    IF e THEN 630 ELSE IF b THEN 614
616 EntryS 6, 52, 28, e, b, Ven.Ord:    IF e THEN 630 ELSE IF b THEN 615
617 EntryS 7, 52, 28, e, b, Ven.Cnt:    IF e THEN 630 ELSE IF b THEN 616
618 EntryT 8, 52, e, b, Ven.Tel:        IF e THEN 630 ELSE IF b THEN 617
619 EntryT 8, 68, e, b, Ven.Fax:        IF e THEN 630 ELSE IF b THEN 618
620 EntryS 9, 52, 16, e, b, Ven.Act:    IF e THEN 630 ELSE IF b THEN 619
621 EntryN 10, 52, 2, 0, e, b, Ven.Dis: IF e THEN 630 ELSE IF b THEN 620
622 EntryN 10, 72, 2, 0, e, b, Ven.Trm: IF e THEN 630 ELSE IF b THEN 621
	IF Confirm THEN
	   Ven.Num = RIGHT$(STR$(1000 + Vnx.Num), 3)
	   Vnx.Ven = Ven.Nam
	   PUT #2, Vnx.Num, Vnx
	   PUT #1, Vnx.Num, Ven
	   IF p$ <> Ven.Nam THEN
	  VenSort
	  Top = LOF(1) \ LEN(Ven)
	  VenDisp x
	   END IF
	ELSE
	   GOTO 610
	END IF
630
END SUB

DEFLNG L
DEFSNG A-K, M-Q, S-Z
SUB VenLine (r, Lm, Ly)
	IF Lm < 0 THEN COLOR 12 ELSE COLOR 10
	LOCATE r, 52: PRINT USING "########,.##-"; Lm / 100
	IF Lm < 0 THEN COLOR 12 ELSE COLOR 10
	LOCATE r, 67: PRINT USING "########,.##-"; Ly / 100
END SUB

DEFSNG L, R
SUB VenShow ()
	COLOR 9, 0
	GET #1, Vnx.Num, Ven
	LOCATE 2, 38: PRINT Ven.Num
	LOCATE 2, 52: PRINT Ven.Nam
	LOCATE 3, 52: PRINT Ven.Add
	LOCATE 4, 52: PRINT Ven.Str
	LOCATE 5, 52: PRINT Ven.Cty; " "; Ven.Sta; " "; Ven.Zip
	COLOR 12, 0
	LOCATE 6, 52: PRINT Ven.Ord
	LOCATE 7, 52: PRINT Ven.Cnt
	COLOR 13, 0
	LOCATE 8, 52: PRINT Ven.Tel
	LOCATE 8, 68: PRINT Ven.Fax
	LOCATE 9, 52: PRINT Ven.Act
	LOCATE 10, 52: PRINT Ven.Dis
	LOCATE 10, 72: PRINT Ven.Trm
	
	VenLine 13, Ven.Mgr, Ven.Ygr
	VenLine 14, Ven.Mds, Ven.Yds
	VenLine 15, Ven.Mcr, Ven.Ycr
	VenLine 17, Ven.Mgr - Ven.Mds - Ven.Mcr, Ven.Ygr - Ven.Yds - Ven.Ycr
	VenLine 18, Ven.Mbb, Ven.Ybb
	VenLine 20, Ven.Mbb + Ven.Mgr - Ven.Mds - Ven.Mcr, Ven.Ybb + Ven.Ygr - Ven.Yds - Ven.Ycr
	VenLine 21, Ven.Mpd, Ven.Ypd
	VenLine 23, Ven.Mbb + Ven.Mgr - Ven.Mds - Ven.Mcr - Ven.Mpd, Ven.Ybb + Ven.Ygr - Ven.Yds - Ven.Ycr - Ven.Ypd
END SUB

SUB VenSort ()
	CLOSE
	SHELL "SortF Vendors.Inx Vendors.Nam /L30 /+1,28 /Q"
	OpenFiles
END SUB

DEFINT A-Z
SUB WinAcc (f$)
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "Win-Acc", e)
	Bot = 1: r = 5: n = 1: GOSUB T2
T1: GET #8, n, Acc
	COLOR 15, 1: LOCATE r, 41: PRINT USING "&ณ&"; Acc.Num; Acc.Nam
	KeyHit n$
	COLOR 15, 6: LOCATE r, 41: PRINT USING "&ณ&"; Acc.Num; Acc.Nam
	SELECT CASE n$
	CASE CHR$(13)
	f$ = Acc.Num
	GOTO T3
	CASE CHR$(0) + "P" 'Dn
	IF n + 1 > 99 GOTO T1 ELSE n = n + 1
	IF r + 1 > 19 THEN CALL Scroll(41, 5, 70, 19, 1) ELSE r = r + 1
	CASE CHR$(0) + "H" 'Up
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

T2: COLOR 15, 6: LOCATE 5
	FOR x = Bot TO Bot + 14: GET #8, x, Acc
	LOCATE , 41: PRINT USING "&ณ&"; Acc.Num; Acc.Nam
	NEXT
	RETURN
T3: PCOPY 1, 0
	COLOR 14, 0
END SUB

SUB WinInv (e, T)
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "Win-Inv", s)
	b = e: r = 6: n = b: GOSUB W2
W1: GET #6, n, Inx: GET #4, Inx.Inv, Inv
	COLOR 0, 7: LOCATE r, 3: PRINT USING "&ณ&ณ&ณ&"; Inv.Ref; Inv.Bdt; Inv.Chk; Inv.Net
	KeyHit n$
	COLOR 15, 4: LOCATE r, 3: PRINT USING "&ณ&ณ&ณ&"; Inv.Ref; Inv.Bdt; Inv.Chk; Inv.Net
	SELECT CASE n$
	CASE CHR$(13)
	PCOPY 0, 2
	CALL MhDisplayByName(0, 0, "Invoice", s)
	InvDisp Inx.Inv
	KeyHit f$
	PCOPY 2, 0
	CASE CHR$(0) + "P" 'Dn
	IF n + 1 > T GOTO W1 ELSE n = n + 1
	IF r + 1 > 20 THEN CALL Scroll(3, 6, 43, 20, 1) ELSE r = r + 1
	CASE CHR$(0) + "H" 'Up
	IF n - 1 < e GOTO W1 ELSE n = n - 1
	IF r - 1 < 6 THEN CALL BkScroll(3, 6, 43, 20, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
	b = n - r - 9: IF b < e THEN b = e
	GOSUB W2: n = b + r - 6
	CASE CHR$(0) + "Q" 'PgDn
	b = 21 - r + n
	IF T - 14 > e THEN
	   IF b > T - 14 THEN b = T - 14: n = T - 20 + r ELSE n = n + 15
	   GOSUB W2
	END IF
	CASE CHR$(0) + "G" 'Home
	b = e: GOSUB W2: n = e + r - 6
	CASE CHR$(0) + "O" 'End
	b = T - 14: IF b < e THEN b = e
	GOSUB W2: n = b + r - 6
	CASE ELSE
	PCOPY 1, 0
	EXIT SUB
	END SELECT
	GOTO W1

W2: COLOR 15, 4: LOCATE 6
	FOR x = b TO T: GET #6, x, Inx: GET #4, Inx.Inv, Inv
	LOCATE , 3: PRINT USING "&ณ&ณ&ณ&"; Inv.Ref; Inv.Bdt; Inv.Chk; Inv.Net
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	RETURN
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

DEFINT A-Z
SUB WinVen (f$)
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "Win-Ven", e)
	Bot = 1: Top = LOF(3) \ LEN(Vnx)
	r = 7: n = 1
	GOSUB C2
C1: GET #3, n, Vnx
	COLOR 7, 0: LOCATE r, 42: PRINT USING "###ณ&"; Vnx.Num; Vnx.Ven
	KeyHit n$
	COLOR 0, 5: LOCATE r, 42: PRINT USING "###ณ&"; Vnx.Num; Vnx.Ven
	SELECT CASE n$
	CASE CHR$(13)
	 GET #1, Vnx.Num, Ven
	 IF Ven.Nam < "!" GOTO C1
	 f$ = Ven.Num
	 COLOR 14, 0
	 PCOPY 1, 0
	 EXIT SUB
	CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
	 IF n + 1 > Top GOTO C1 ELSE n = n + 1
	 IF r + 1 > 20 THEN CALL Scroll(42, 7, 73, 20, 1) ELSE r = r + 1
	CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
	 IF n - 1 < 1 GOTO C1 ELSE n = n - 1
	 IF r - 1 < 7 THEN CALL BkScroll(42, 7, 73, 20, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
	 Bot = n - r - 7: IF Bot < 1 THEN Bot = 1
	 GOSUB C2: n = Bot + r - 7
	CASE CHR$(0) + "Q" 'PgDn
	 Bot = 21 - r + n
	 IF Bot > Top - 13 THEN Bot = Top - 13: n = Top - 20 + r ELSE n = n + 14
	 GOSUB C2
	CASE CHR$(0) + "G" 'Home
	 Bot = 1: GOSUB C2
	 n = r - 6
	CASE CHR$(0) + "O" 'End
	 Bot = Top - 13: GOSUB C2
	 n = Top - 20 + r
	CASE ELSE
	 f$ = ""
	 PCOPY 1, 0
	 EXIT SUB
	END SELECT
	GOTO C1
 
C2: COLOR 0, 5: LOCATE 7
	FOR x = Bot TO Top: GET #3, x, Vnx
	LOCATE , 42: PRINT USING "###ณ&"; Vnx.Num; Vnx.Ven
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	RETURN
END SUB

