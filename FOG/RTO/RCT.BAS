DECLARE SUB AgrDetl (u$)
DEFINT A-Z: DEFLNG L
DECLARE SUB AgrSort ()
DECLARE SUB AgrPrnt ()
DECLARE SUB InvCalc (r)
DECLARE SUB InvRest ()
DECLARE SUB PyoDate (f$)
DECLARE SUB RatDisp (z)
DECLARE SUB RctCash ()
DECLARE SUB CashAgr ()

DECLARE FUNCTION RctType ()
DECLARE FUNCTION ChgLoan ()

DECLARE SUB PyoAmnt (l&)
DECLARE SUB InsActv (s, n)
DECLARE SUB InvActv (s)
DECLARE SUB AgrStat (r, c)
DECLARE SUB AgrFreq (r, c)
DECLARE SUB AgrCust ()
DECLARE SUB TranLog ()

DECLARE SUB RctTotal ()
DECLARE SUB RctSort ()
DECLARE SUB RctStup ()
DECLARE SUB RctDisp ()
DECLARE SUB RctPrnt ()
DECLARE SUB RctPror ()
DECLARE SUB RctPost ()
DECLARE SUB RctStat ()
DECLARE SUB RctTotl ()
DECLARE SUB RctDist ()
DECLARE SUB RctLate ()
DECLARE SUB RctInve ()
DECLARE SUB RctInvD (n)
DECLARE SUB RctInvN (n)
DECLARE SUB RctLonD (n)
DECLARE SUB RctVoid (p$)
DECLARE SUB RctAgrt (f$, f, e)

REM $INCLUDE: '..\Include\Config.Dec'

	DIM SHARED Pdate  AS STRING * 10

DEFINT L
SUB mBadRct ()
	p$ = Pas.Ini
	h$ = Pas.Lvl
	CALL MhDisplayByName(0, 0, "Receipt", e)
	Statlin "Bad Check"
500 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 500
	Yes = 2: IF WinCust(h$) THEN 500
	RctDisp
	IF Rct.Sta <> "A" THEN WinMssg "Receipt not active": GOTO 500
	IF RIGHT$(Rct.Cod, 1) <> "K" THEN
	   WinMssg "Not a check"
	   IF h$ < "7" THEN WinMssg "Low level password": GOTO 500
	END IF
	
	IF Confirm(1, "confirm BAD-CHECK") THEN
	   Rct.Sta = "B"
	   PUT #19, Rcx.Num, Rct
	   GET #19, Rcx.Num, Rcl
	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Typ = "RB"
	   Tra.Mgr = p$
	   Tra.Rec = Rcl.Rct
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	   PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Rct.Amt; Rct.Num; Rct.Agr; Cus.Nam

	   Agr.Act = "RB"
	   Agr.Bad = Agr.Bad + 1
	   RctVoid p$
	   n = 1 + LOF(19) \ LEN(Rct)
	   RctNum = RctNum + 1
	   Rct.Dat = DATE$
	   Rct.Mgr = p$
	   Rct.Sta = "B"
	   Rct.Num = RIGHT$(STR$(10000000 + RctNum), 6)
	   Rcx.Agr = VAL(Rct.Agr)
	   Rcx.Rct = VAL(Rct.Num)
	   Rcx.Num = n
	   PUT #19, Rcx.Num, Rct
	   PUT #20, Rcx.Num, Rcx
	   GET #19, Rcx.Num, Rcl
	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Typ = "RB"
	   Tra.Mgr = p$
	   Tra.Rec = Rcl.Rct
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	   Tra.Typ = "RN"
	   PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Rct.Amt; Rct.Num; Rct.Agr; Cus.Nam

	   GET #22, 1, Act
	   FOR z = 1 TO 5
	   Act.Rcb(z) = Act.Rcb(z) + 1
	   NEXT
	   PUT #22, 1, Act
	   SortR = 1
	   RctDisp
	   RctPrnt
	   RctSort
	   OpenFiles
	END IF
	GOTO 500
END SUB

SUB mDspRct ()
	CALL MhDisplayByName(0, 0, "Receipt", e)
	Statlin "Display a Receipt"
300 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 300
	Yes = 2: IF WinCust(Pas.Lvl) THEN 300
	RctDisp
	GOTO 300
END SUB

SUB mMrkRct ()
	Statlin "Bad Check Marker"
	CALL MhDisplayByName(0, 0, "Receipt", e)
450 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 450
	Yes = 2: IF WinCust(Pas.Lvl) THEN 450
	RctDisp
	IF Rct.Mrk = "û" THEN WinMssg "Already marked": GOTO 450
	IF Rct.Sta <> "B" THEN WinMssg "Not a bad check": GOTO 450
	IF VAL(Rct.Amt) > 0 THEN WinMssg "Pick the bad check issue": GOTO 450
	IF Confirm(1, "MARK bad check") THEN
	   Rct.Mrk = "û"
	   PUT #19, Rcx.Num, Rct
	   Agr.Bad = Agr.Bad - 1
	   PUT #16, Agx.Num, Agr
	   qPrint Rct.Mrk, 20, 76, 0, 1
	END IF
	GOTO 450
END SUB

DEFLNG L
SUB mNewRct ()
	CALL MhDisplayByName(0, 0, "Receipt", e)
	Statlin "Issue New Receipt"
100 n = 1 + LOF(19) \ LEN(Rct)
	GET #19, n, Rct
	Rct.Dat = DATE$
	Rct.Mgr = Pas.Ini
	Rct.Num = RIGHT$(STR$(10000001 + RctNum), 6)
	
	qPrint Rct.Dat, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	qPrint Rct.Num, 5, 33, 0, 1

	IF Setup THEN
101 EntryD 5, 2, e, b, Rct.Dat:          IF e THEN 120 ELSE IF b THEN 101
	IF Rct.Dat < "0" THEN WinMssg "Must have!!": GOTO 101
	END IF
102 EntryS 3, 54, 26, e, b, Cus.Nam:     IF e THEN 120 ELSE IF b THEN IF Setup THEN 101 ELSE 102
	Yes = 1: IF WinCust(Pas.Lvl) THEN 102
	IF Agr.Sta <> "A" AND Agr.Sta <> "T" THEN RctDisp: WinMssg "Inactive agreement": GOTO 102
	Rct.Cus = Cus.Num
	Rct.Agr = Agr.Num
	RctStup
	RctDisp
	IF Agr.Frq = "C" THEN WinMssg "Must use Cash Sale": GOTO 102
103 EntryD 18, 64, e, b, Rct.Nxt:        IF e THEN 120 ELSE IF b THEN 102
	IF Rct.Nxt <> Pdate THEN
	   IF DateDif(Rct.Ddt, Rct.Nxt) < 4 THEN WinMssg "Must prorate more than 4 days": GOTO 103
	   RctPror
	   RctTotl
	END IF
104 EntryU 19, 64, 2, e, b, Rct.Cod:     IF e THEN 120 ELSE IF b THEN 103
	IF RctType THEN 104
	IF RIGHT$(Rct.Cod, 1) = "K" THEN
106    EntryU 20, 64, 10, e, b, Rct.Chk: IF e THEN 120 ELSE IF b THEN 104
	   IF Rct.Chk < "!" THEN WinMssg "Check number please!": GOTO 106
	END IF
	qPrint Rct.Cod, 19, 64, 0, 1
	qPrint Rct.Chk, 20, 64, 0, 1
	qPrint Rct.Trp, 32, 63, 0, 1
	RctTotl
108 IF Pas.Lvl < "8" AND Agr.Rct > 0 THEN 110
	EntryN 29, 63, 7, 2, e, b, Rct.Dis:  IF e THEN 120 ELSE IF b THEN 104
	IF VAL(Rct.Dis) > VAL(Rct.Rnt) THEN WinMssg "Discount exceeds rent": GOTO 108
	IF VAL(Rct.Dis) < 0 THEN 108
	RctTotl

'use the next lines for store setup when sales changed
'109 EntryN 30, 63, 7, 2, e, b, Rct.Tax:  IF e THEN 120 ELSE IF b THEN 104
'    IF VAL(Rct.Tax) < 0 THEN 109
'    RctTotal

110 EntryN 31, 63, 7, 2, e, b, Rct.Lat:  IF e THEN 120 ELSE IF b THEN 104
	IF VAL(Rct.Lat) < 0 THEN 110
	RctTotal
112 EntryN 32, 63, 7, 2, e, b, Rct.Trp:  IF e THEN 120 ELSE IF b THEN 110
	IF VAL(Rct.Trp) < 0 THEN 112
	RctTotal
114 EntryN 33, 63, 7, 2, e, b, Rct.Oth:  IF e THEN 120 ELSE IF b THEN 112
	IF VAL(Rct.Oth) < 0 THEN 114
	RctTotal
118 EntryN 35, 63, 7, 2, e, b, Rct.Ref:  IF e THEN 120 ELSE IF b THEN 114
	IF VAL(Rct.Ref) < 0 THEN 118
	RctTotal
119 EntryN 39, 63, 7, 2, e, b, Rct.Amt:  IF e THEN 120 ELSE IF b THEN 118
	IF VAL(Rct.Amt) < .01 THEN 119
	RctDist
	RctTotal
	IF Agr.Una < 0 THEN 119
	IF VAL(Rct.Tax) < 0 THEN WinMssg "Discount is killing me": GOTO 119
	IF Rct.Nxt = Agr.Pod THEN
	   IF ChgLoan = 0 THEN WinMssg "Recover loaners first": GOTO 119
	   IF Agr.Una > 0 THEN WinMssg "Unapplied on payouts!!": GOTO 119
	   IF Agr.Sta = "T" THEN WinMssg "Payout on terminated!": GOTO 119
	END IF
	IF Confirm(1, "save NEW receipt") = 0 THEN 119
	SortR = 1
	RctPost
	RctPrnt
	IF Agr.Sta = "P" THEN RctSort: OpenFiles: AgrDetl Pas.Nam
	GOTO 100
120 IF SortR THEN RctSort
END SUB

DEFINT L
SUB mPrtRct ()
	CALL MhDisplayByName(0, 0, "Receipt", e)
	Statlin "Reprint a Receipt"
	p$ = Pas.Ini
200 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 200
	Yes = 2: IF WinCust(Pas.Lvl) THEN 200
	RctDisp
	RctPrnt
	IF Tra.Typ = "RR" THEN
	   PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Rct.Amt; Rct.Num; Rct.Agr; Cus.Nam
	END IF
	GOTO 200
END SUB

DEFLNG L
SUB mSalRct ()
	Statlin "Cash Sale Receipt"
	CALL MhDisplayByName(0, 0, "Receipt", e)
201 n = 1 + LOF(19) \ LEN(Rct)
	GET #19, n, Rct
	Rct.Dat = DATE$
	Rct.Mgr = Pas.Ini
	Rct.Num = RIGHT$(STR$(10000001 + RctNum), 6)

	qPrint Rct.Dat, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	qPrint Rct.Num, 5, 33, 0, 1
	
202 EntryS 3, 54, 26, e, b, Cus.Nam:     IF e THEN 220 ELSE IF b THEN 202
	Yes = 1: IF WinCust(Pas.Lvl) THEN 202
	IF Agr.Sta <> "A" THEN WinMssg "Inactive agreement": GOTO 202
	IF Agr.Frq <> "C" THEN WinMssg "Not Cash Sale Agreement": GOTO 202
	
	Rct.Cus = Cus.Num
	Rct.Agr = Agr.Num
	Rct.Sta = "A"
	Rct.Dat = DATE$
	Rct.Nxt = Agr.Dat
	Rct.Ddt = Agr.Nxt
	Rct.Dis = "   0.00"
	Rct.Lat = "   0.00"
	Rct.Trp = "   0.00"
	Rct.Wav = "   0.00"
	Rct.Ref = "   0.00"
	Rct.Crd = "   0.00"
	Rct.Amt = "   0.00"
	Rct.Due = "   0.00"
	RctCash
	RctDisp
204 EntryU 19, 64, 2, e, b, Rct.Cod:     IF e THEN 220 ELSE IF b THEN 202
	IF RctType THEN 204
	IF RIGHT$(Rct.Cod, 1) = "K" THEN
206    EntryU 20, 64, 10, e, b, Rct.Chk: IF e THEN 220 ELSE IF b THEN 204
	   IF Rct.Chk < "!" THEN WinMssg "Check number please!": GOTO 206
	END IF
	COLOR 14, 0
	qPrint Rct.Cod, 19, 64, 0, 1
	qPrint Rct.Chk, 20, 64, 0, 1
207 EntryN 28, 63, 7, 2, e, b, Rct.Rnt:  IF e THEN 220 ELSE IF b THEN 204
	Lr = 100 * VAL(Agr.Pay) - 100 * VAL(Rct.Rnt) - Agr.Tri
	IF Lr < 0 THEN WinMssg "Exceeds sale price": RctCash: GOTO 207
	RctCash
209 EntryN 35, 63, 7, 2, e, b, Rct.Ref:  IF e THEN 220 ELSE IF b THEN 207
	RctCash
219 EntryN 39, 63, 7, 2, e, b, Rct.Amt:  IF e THEN 220 ELSE IF b THEN 209
	IF VAL(Rct.Amt) < .01 THEN 219
	IF Rct.Amt <> Rct.Due THEN
	   Lr = (100 * VAL(Rct.Amt) - 100 * VAL(Rct.Oth) - 100 * VAL(Rct.Ref)) / (1 + VAL(Sto.Stx) / 100)'sales amount
	   COLOR 14, 2
	   LOCATE 28, 63: PRINT USING "####.##"; Lr / 100; 'sale amount
	   Picture 28, 63, 7, Rct.Rnt
	END IF
	RctCash
	IF Agr.Una < 0 THEN 219
	IF ChgLoan = 0 THEN WinMssg "No loaners on cash sale": GOTO 219
	IF Agr.Una > 0 THEN WinMssg "No unapplied on cash sales": GOTO 219
	IF Confirm(1, "save CASH receipt") = 0 THEN 219
	SortR = 1
	RctPost
	RctPrnt
	GOTO 201
220 IF SortR THEN RctSort
END SUB

SUB mTelRct ()
	Statlin "Telephone Receipts"
	CALL MhDisplayByName(0, 0, "Phone", e)

	GET #22, 1, Act                                              'get activity
	LOCATE 20, 45, 0: PRINT USING "#####.##"; Act.Tel(1) / 100;  'display amount
	Picture 20, 45, 8, Tel$                                      'get picture
	FOR x = 1 TO 5                                               'reverse old
		Act.Tel(x) = Act.Tel(x) - 100 * VAL(Tel$)                'day value
	NEXT                                                         'and get ready
	PUT #22, 1, Act                                              'for new value
	
230 EntryN 20, 45, 8, 2, e, b, Tel$:  IF e THEN 230
	IF Confirm(1, "Phone Receipts") THEN
	   FOR x = 1 TO 5
		   Act.Tel(x) = Act.Tel(x) + 100 * VAL(Tel$)
	   NEXT
	   PUT #22, 1, Act
	ELSE
	   GOTO 230
	END IF

	Tra.Typ = "RT"
	PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Tel$
END SUB

DEFINT L
SUB mVoiRct ()
	p$ = Pas.Ini
	h$ = Pas.Lvl
	CALL MhDisplayByName(0, 0, "Receipt", e)
	Statlin "Void a Receipt"
400 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 400
	Yes = 2: IF WinCust(h$) THEN 400
	RctDisp
	IF Rct.Sta <> "A" THEN WinMssg "Inactive receipt": GOTO 400
	IF Rct.Dat <> DATE$ AND h$ < "7" THEN WinMssg "Low level password": GOTO 400
	IF Confirm(1, "VOID receipt") THEN
	   Rct.Sta = "V"
	   PUT #19, Rcx.Num, Rct
	   GET #19, Rcx.Num, Rcl
	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Typ = "RV"
	   Tra.Mgr = p$
	   Tra.Rec = Rcl.Rct
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	   PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Rct.Amt; Rct.Num; Rct.Agr; Cus.Nam

	   Agr.Act = "RV"
	   RctVoid p$
	   n = 1 + LOF(19) \ LEN(Rct)
	   RctNum = RctNum + 1
	   Rct.Dat = DATE$
	   Rct.Mgr = p$
	   Rct.Sta = "V"
	   Rct.Num = RIGHT$(STR$(10000000 + RctNum), 6)
	   Rcx.Agr = VAL(Rct.Agr)
	   Rcx.Rct = VAL(Rct.Num)
	   Rcx.Num = n
	   PUT #19, Rcx.Num, Rct
	   PUT #20, Rcx.Num, Rcx
	   GET #19, Rcx.Num, Rcl
	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Typ = "RV"
	   Tra.Mgr = p$
	   Tra.Rec = Rcl.Rct
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	   Tra.Typ = "RN"
	   PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Rct.Amt; Rct.Num; Rct.Agr; Cus.Nam
	   
	   GET #22, 1, Act
	   FOR z = 1 TO 5
	   Act.Rcv(z) = Act.Rcv(z) + 1
	   NEXT
	   PUT #22, 1, Act
	   SortR = 1
	   RctDisp
	   RctPrnt
	   RctSort
	   OpenFiles
	END IF
	GOTO 400
END SUB

DEFLNG L
DEFSNG P
SUB PyoAmnt (Lr)
	p = 1
	IF DateDif(Agr.Nxt, Agr.Pod) > 60 THEN             'none in last 60 days
	   IF Agr.Mtr > " 11" THEN p = .8                  '
	   IF Agr.Mtr > " 14" THEN p = .7                  '
	   IF Agr.Mtr > " 17" THEN p = .6                  '
	END IF                                             '
	IF Agr.Ref > "1" THEN p = 1                        'no discount on notes
	Lm = 100 * VAL(Agr.Mrt)                            'monthly rate
	Lw = 100 * VAL(Agr.Wrt)                            'weekly rate
	Ld = Lw / 7                                        'daily rate
	d = DateDif(Agr.Nxt, Agr.Pod)                      'elapsed days
	m = VAL(LEFT$(Agr.Nxt, 2))                         'month number
	y = VAL(RIGHT$(Agr.Nxt, 4))                        '
	IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29     'leap year?
	WHILE d >= Ndm(m)                                  'repeat until
		IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29 'elapsed number
		d = d - Ndm(m)                                 'of whole months
		nm = nm + 1                                    'are determined
		m = m + 1: IF m > 12 THEN m = 1: y = y + 1     '
	WEND                                               '
	nw = d \ 7                                         'number of weeks
	nd = d MOD 7                                       'number of days
	IF Agr.Rct = 0 AND Agr.Frq = "M" THEN              'prorate monthly
	   Lr = nm * Lm + (7 * nw + nd) * Lm / Ndm(m)      'on first receipt
	ELSE                                               '
	   Lr = nm * Lm + nw * Lw + nd * Ld                'compute rent
	END IF                                             '
	Lr = Lr * p                                        'apply discount
END SUB

DEFINT L, P
SUB RctAgrt (f$, f, e)
	f = 0
	e = 0
	b = 1
	n& = VAL(f$)
	T = LOF(21) / LEN(Rcx)
	DO UNTIL (T < b)
	   m = (T + b) \ 2: GET #21, m, Rcx
	   IF n& > Rcx.Agr THEN b = m + 1 ELSE T = m - 1
	LOOP
	DO WHILE n& > Rcx.Agr AND NOT EOF(21)
	   GET #21, , Rcx
	LOOP
	IF n& = Rcx.Agr THEN
	   f = LOC(21): e = f
	   DO WHILE NOT EOF(21)
		  GET #21, , Rcx
		  IF n& = Rcx.Agr THEN e = LOC(21) ELSE EXIT DO
	   LOOP
	END IF
END SUB

DEFLNG L
SUB RctCash ()
	Lr = 100 * VAL(Rct.Rnt)                         'rent amount
	Lo = 100 * VAL(Rct.Oth)                         'other charges
	Lf = 100 * VAL(Rct.Ref)                         'refferal
	Lp = 100 * VAL(Rct.Amt)                         'receipt amount
	Lc = 100 * VAL(Agr.Pay)                         'sales total
	La = Lr + Ls + Lo + Lf
	IF Lr = 0 THEN
	   Lr = Lc - Agr.Tri
	   Lo = 100 * VAL(Agr.Pro) - Agr.Tot
	END IF
	IF Lc - Lr - Agr.Tri < 0 THEN
	   Lr = 100 * VAL(Agr.Pay) - Agr.Tri            'amount due
	   Lo = 100 * VAL(Agr.Pro) - Agr.Tot            'processing fee
	END IF
	Ls = Lr * VAL(Sto.Stx) / 100                    'sales tax
	La = Lr + Ls + Lo + Lf
	IF La > 99999 THEN
	   Lr = (99999 - Lo - Lf) / (1 + VAL(Sto.Stx) / 100)'sales amount
	   Ls = Lr * VAL(Sto.Stx) / 100                 'sales tax
	   La = Lr + Ls + Lo + Lf
	END IF
	Lu = Lp - La
	COLOR 14, 2
	LOCATE 28, 63: PRINT USING "####.##"; Lr / 100; 'sale amount
	LOCATE 30, 63: PRINT USING "####.##"; Ls / 100; 'sales tax
	LOCATE 33, 63: PRINT USING "####.##"; Lo / 100; 'delivery
	LOCATE 35, 63: PRINT USING "####.##"; Lf / 100; 'referral
	LOCATE 38, 63: PRINT USING "####.##"; La / 100; 'amount due
	LOCATE 41, 63: PRINT USING "####.##"; Lu / 100; 'unapplied
	Picture 28, 63, 7, Rct.Rnt
	Picture 30, 63, 7, Rct.Tax
	Picture 33, 63, 7, Rct.Oth
	Picture 35, 63, 7, Rct.Ref
	Picture 38, 63, 7, Rct.Due
	Agr.Una = Lu
	IF Lc - Lr - Agr.Tri = 0 THEN
	   Agr.Sta = "P": qPrint " Payout ", 13, 60, 0, 1
	ELSE
	   Agr.Sta = "A": qPrint "        ", 13, 60, 0, 1
	END IF
	
END SUB

SUB RctDisp ()
	IF FindIni(Rct.Mgr) = 0 THEN ERROR 201
	COLOR 7, 1
	GET #13, VAL(Agr.Cus), Cus
	AgrCust
	RctStat
	qPrint Rct.Dat, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	qPrint Rct.Num, 5, 33, 0, 1
	qPrint Agr.Rte, 5, 42, 0, 1
	qPrint Rct.Dat, 16, 64, 0, 1
	qPrint Rct.Ddt, 17, 64, 0, 1
	qPrint Rct.Nxt, 18, 64, 0, 1
	qPrint Rct.Cod, 19, 64, 0, 1
	qPrint Rct.Chk, 20, 64, 0, 1
	qPrint Rct.Mrk, 20, 76, 0, 1

	qPrint Rct.Rnt, 28, 63, 0, 1
	qPrint Rct.Dis, 29, 63, 0, 1
	qPrint Rct.Tax, 30, 63, 0, 1
	qPrint Rct.Lat, 31, 63, 0, 1
	qPrint Rct.Trp, 32, 63, 0, 1
	qPrint Rct.Oth, 33, 63, 0, 1
	qPrint Rct.Wav, 34, 63, 0, 1
	qPrint Rct.Ref, 35, 63, 0, 1
	qPrint Rct.Crd, 36, 63, 0, 1
	qPrint Rct.Due, 38, 63, 0, 1
	qPrint Rct.Amt, 39, 63, 0, 1

	COLOR 14, 2
	f$ = "######.##"
	LOCATE 41, 63: PRINT USING "####.##"; VAL(Rct.Amt) - VAL(Rct.Due);
	LOCATE 28, 71: PRINT USING f$; Agr.Tri / 100;
	LOCATE 29, 71: PRINT USING f$; Agr.Tds / 100;
	LOCATE 30, 71: PRINT USING f$; Agr.Ttx / 100;
	LOCATE 31, 71: PRINT USING f$; Agr.Tlc / 100;
	LOCATE 32, 71: PRINT USING f$; Agr.Ttc / 100;
	LOCATE 33, 71: PRINT USING f$; Agr.Tot / 100;
	LOCATE 34, 71: PRINT USING f$; Agr.Tin / 100;
	LOCATE 35, 71: PRINT USING f$; Agr.Trf / 100;
	LOCATE 36, 71: PRINT USING f$; 0;
	LOCATE 38, 71: PRINT USING f$; (Agr.Tri - Agr.Tds + Agr.Ttx + Agr.Tlc + Agr.Ttc + Agr.Tot + Agr.Tin - Agr.Trf) / 100;
	LOCATE 39, 71: PRINT USING f$; VAL(Rct.Crd) + (Agr.Tri - Agr.Tds + Agr.Ttx + Agr.Tlc + Agr.Ttc + Agr.Tot + Agr.Tin - Agr.Trf) / 100;
	LOCATE 41, 71: PRINT USING f$; VAL(Rct.Crd);

	IF FindCmb(Agr.Cmb) = 0 THEN Cmb.Des = ""
	COLOR 13, 4: LOCATE 11, 14: PRINT Agr.Cmb; "  "; Cmb.Des;
	COLOR 11, 1
	Ser(1) = Agr.It1: Lon(1) = Agr.Lo1: Dis(1) = Agr.Ds1
	Ser(2) = Agr.It2: Lon(2) = Agr.Lo2: Dis(2) = Agr.Ds2
	Ser(3) = Agr.It3: Lon(3) = Agr.Lo3: Dis(3) = Agr.Ds3
	Ser(4) = Agr.It4: Lon(4) = Agr.Lo4: Dis(4) = Agr.Ds4
	IF FindSer(Ser(1)) THEN RctInvD 1 ELSE RctInvN 1
	IF FindSer(Ser(2)) THEN RctInvD 2 ELSE RctInvN 3
	IF FindSer(Ser(3)) THEN RctInvD 3 ELSE RctInvN 5
	IF FindSer(Ser(4)) THEN RctInvD 4 ELSE RctInvN 7

	IF FindSer(Lon(1)) THEN RctLonD 1 ELSE RctInvN 2
	IF FindSer(Lon(2)) THEN RctLonD 2 ELSE RctInvN 4
	IF FindSer(Lon(3)) THEN RctLonD 3 ELSE RctInvN 6
	IF FindSer(Lon(4)) THEN RctLonD 4 ELSE RctInvN 8
	
	COLOR 1, 7
	LOCATE 28, 21: PRINT Agr.Mtr; : LOCATE , 27: PRINT Agr.Mrt;
	LOCATE 28, 36: PRINT USING "####.##"; VAL(Agr.Mtr) * VAL(Agr.Mrt);
	LOCATE 29, 21: PRINT Agr.Wtr; : LOCATE , 27: PRINT Agr.Wrt;
	LOCATE 29, 36: PRINT USING "####.##"; VAL(Agr.Wtr) * VAL(Agr.Wrt);

	COLOR 1, 3
	PyoAmnt Lp
	AgrStat 35, 16
	AgrFreq 36, 16
	qPrint Agr.Num, 34, 16, 0, 1
	qPrint Agr.Dat, 37, 16, 0, 1
	qPrint Agr.Due, 38, 16, 0, 1
	qPrint Rct.Nxt, 39, 16, 0, 1
	qPrint Agr.Pod, 40, 16, 0, 1
	IF Agr.Frq = "C" THEN
	   qPrint Agr.Pay, 41, 16, 0, 1
	ELSE
	   LOCATE 41, 16: PRINT USING "####.##"; Lp / 100;
	END IF
	qPrint Agr.Lat, 34, 39, 0, 1
	qPrint Agr.Trp, 35, 39, 0, 1
	d = DateDif(Rct.Ddt, Rct.Dat): IF d < 1 THEN d = 0
	LOCATE 36, 39: PRINT USING "#####"; Agr.Eza;
	LOCATE 37, 39: PRINT USING "#####"; Agr.Fre;
	LOCATE 38, 39: PRINT USING "#####"; d;
	LOCATE 39, 39: PRINT USING "#####"; Agr.Rct;
	qPrint Rct.Typ, 40, 39, 0, 1
	qPrint Rct.Day, 41, 39, 0, 1
END SUB

SUB RctDist ()
	IF Rct.Due <> Rct.Amt THEN
	   PyoAmnt Lp                   'payout amount
	   Ld = 100 * VAL(Rct.Dis)      'discount
	   Ll = 100 * VAL(Rct.Lat)      'late charge
	   Lt = 100 * VAL(Rct.Trp)      'trip charge
	   Lo = 100 * VAL(Rct.Oth)      'other charges
	   Lf = 100 * VAL(Rct.Ref)      'referral
	   Lc = 100 * VAL(Rct.Crd)      'credit
	   La = 100 * VAL(Rct.Amt)      'amount paid
	   Lw = 100 * VAL(Agr.Wrt)      'weekly rate
	   Lm = 100 * VAL(Agr.Mrt)      'monthly rate
	   Lv = 100 * VAL(Rct.Wav)      'waiver

333    Lr = La - Ll - Lt - Lo + Lf + Lc
	   Lr = Lr + Ld * (1 + VAL(Sto.Stx) / 100)
	   Lr = Lr / (1 + VAL(Sto.Stx) / 100)
									'======================
	   IF Lr > 0 THEN               'if rent is not zero
		  nm = Lr \ Lm              'months rent covers
		  Lr = Lr MOD Lm            'and left over rent
		  nw = Lr \ Lw              'weeks rent convers
		  Lr = nm * Lm + nw * Lw    'compute rent amount
	   ELSE                         '
		  Lr = 0                    '
	   END IF                       '
	   IF Lr >= Lp THEN             'if rent amount
		  Rct.Nxt = Agr.Pod         'exceeds payout
		  RctPror                   'prorate and
		  EXIT SUB                  'exit subroutine
	   END IF                       '======================

	   IF Agr.Wav = "Y" THEN
		  IF Lm > 8000 THEN Lv = Lm * nm * .05 ELSE Lv = nm * 400
		  IF Lw > 2000 THEN Lv = Lv + (Lw * nw * .05) ELSE Lv = Lv + (nw * 100)
	   END IF
	   COLOR 14, 2: LOCATE 28, 63: PRINT USING "####.##"; Lr / 100;
					LOCATE 34, 63: PRINT USING "####.##"; Lv / 100;
	   Picture 28, 63, 7, Rct.Rnt
	   Picture 34, 63, 7, Rct.Wav

	   RctTotl
	   SELECT CASE Agr.Una
		   CASE IS < -1: IF Lr > 0 THEN La = La - Lv: GOTO 333
		   CASE IS = -1: La = La - 1: GOTO 333
	   END SELECT

	   IF Lr > 99999 THEN ERROR 203

	   Rct.Nxt = MonMath$(Rct.Ddt, nm)         'advance next due date ? months
	   Rct.Nxt = DayMath$(Rct.Nxt, nw * 7)     'advance next due date ? weeks
	   IF DateDif(Agr.Pod, Rct.Nxt) >= 0 THEN RctPror: EXIT SUB
	   
	   Pdate = Rct.Nxt
	   qPrint "        ", 13, 60, 0, 1
	   qPrint Rct.Nxt, 18, 64, 0, 1
	   qPrint Rct.Nxt, 39, 16, 0, 1
	   COLOR 1, 3: LOCATE 40, 39: PRINT USING "##m#w"; nm; nw;
				   LOCATE 41, 39: PRINT USING "####d"; d;
				   Picture 40, 39, 5, Rct.Typ
				   Picture 41, 39, 5, Rct.Day
	END IF
END SUB

DEFINT L
SUB RctInvD (n)
	GET #10, Inx.Num, Inv
	IF FindCod(Inx.Cod) = 0 THEN ERROR 200
	qPrint Inv.Ser, 2 * n + 13, 2, 0, 1
	qPrint Inv.Cod, 2 * n + 13, 13, 0, 1
	qPrint Cod.Des, 2 * n + 13, 18, 0, 1
END SUB

SUB RctInve ()
	FOR r = 1 TO 4
	IF FindSer(Ser(r)) THEN
	   GET #10, Inx.Num, Inv
	   IF FindCod(Inv.Cod) = 0 THEN ERROR 200
	   PRINT #24, TAB(5); Inv.Bra; TAB(16); Inv.Mdl; TAB(33); Inv.Ser; TAB(56); Cod.Des
	ELSE
	   PRINT #24,
	END IF
	NEXT
END SUB

SUB RctInvN (n)
	qPrint "          ", n + 14, 2, 0, 1
	qPrint "    ", n + 14, 13, 0, 1
	qPrint "                        ", n + 14, 18, 0, 1
END SUB

SUB RctLate ()
	IF Agr.Sta = "T" THEN                                     'if terminated
	   d = DateDif(Rct.Ddt, Rct.Nxt)                          'days late up
	ELSE                                                      'to next date
	   d = DateDif(Rct.Ddt, Rct.Dat)                          'otherwise up
	END IF                                                    'todays date
	IF d > 0 THEN                                             'check number
	   e = d                                                  'of late days
	   m = VAL(LEFT$(Rct.Ddt, 2))                             'month number
	   y = VAL(RIGHT$(Rct.Ddt, 4))                            'year number
	   IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29         'leap year?
	   WHILE d >= Ndm(m)                                      'if past due
			 IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29   '   compute number
			 d = d - Ndm(m)                                   '   of monthly
			 n = n + 1                                        '   months late
			 m = m + 1: IF m > 12 THEN m = 1: y = y + 1       '   compute number
	   WEND                                                   '   of weekly
	   n = n + (6 + d) \ 7                                    '   weeks late
	END IF                                                    '
	IF n * VAL(Agr.Lat) > 99.99 THEN ERROR 204                'error late
	COLOR 14, 2: LOCATE 31, 63: PRINT USING "####.##"; n * VAL(Agr.Lat);
	COLOR 1, 3:  LOCATE 38, 39: PRINT USING "#####"; e;
	Picture 31, 63, 7, Rct.Lat
END SUB

SUB RctLonD (n)
	GET #10, Inx.Num, Inv
	IF FindCod(Inx.Cod) = 0 THEN ERROR 200
	qPrint Inv.Ser, 2 * n + 14, 2, 0, 1
	qPrint Inv.Cod, 2 * n + 14, 13, 0, 1
	qPrint Cod.Des, 2 * n + 14, 18, 0, 1
END SUB

DEFLNG L
SUB RctPost ()
	GET #22, 1, Act
	Picture 38, 39, 5, f$: d = VAL(f$)
	Agr.Ndl = Agr.Ndl + d
	Agr.Rct = Agr.Rct + 1
	Agr.Nxt = Rct.Nxt
	Agr.Act = "RN"
	IF Agr.Sta <> "T" THEN Agr.Dta = DATE$
	IF Agr.Frq <> "C" AND Rct.Nxt = Agr.Pod THEN Agr.Sta = "P"
	Agr.Npd = Agr.Npd + VAL(MID$(Rct.Day, 1, 4))
	Agr.Npw = Agr.Npw + VAL(MID$(Rct.Typ, 4, 1))
	Agr.Npm = Agr.Npm + VAL(MID$(Rct.Typ, 1, 2))
   'Agreement post
   '--------------
	
	IF Agr.Frq = "C" THEN
	l = 100 * VAL(Rct.Rnt): FOR z = 1 TO 5: Act.Sal(z) = Act.Sal(z) + l: NEXT
							FOR z = 1 TO 5: Act.Rcc(z) = Act.Rcc(z) + 1: NEXT
	ELSE
	IF Agr.Sta = "P" THEN
	l = 100 * VAL(Rct.Rnt): FOR z = 1 TO 5: Act.Pay(z) = Act.Pay(z) + l: NEXT
							FOR z = 1 TO 5: Act.Rcn(z) = Act.Rcn(z) + 1: NEXT
	ELSE
	l = 100 * VAL(Rct.Rnt): FOR z = 1 TO 5: Act.Rnt(z) = Act.Rnt(z) + l: NEXT
							FOR z = 1 TO 5: Act.Rcn(z) = Act.Rcn(z) + 1: NEXT
	END IF
	END IF
							Agr.Mri = Agr.Mri + l
							Agr.Yri = Agr.Yri + l
							Agr.Tri = Agr.Tri + l
	l = 100 * VAL(Rct.Dis): FOR z = 1 TO 5: Act.Dis(z) = Act.Dis(z) + l: NEXT
							Agr.Mds = Agr.Mds + l
							Agr.Yds = Agr.Yds + l
							Agr.Tds = Agr.Tds + l
	l = 100 * VAL(Rct.Tax): FOR z = 1 TO 5: Act.Tax(z) = Act.Tax(z) + l: NEXT
							Agr.Mtx = Agr.Mtx + l
							Agr.Ytx = Agr.Ytx + l
							Agr.Ttx = Agr.Ttx + l
	l = 100 * VAL(Rct.Lat): FOR z = 1 TO 5: Act.Lat(z) = Act.Lat(z) + l: NEXT
							Agr.Mlc = Agr.Mlc + l
							Agr.Ylc = Agr.Ylc + l
							Agr.Tlc = Agr.Tlc + l
	l = 100 * VAL(Rct.Trp): FOR z = 1 TO 5: Act.Trp(z) = Act.Trp(z) + l: NEXT
							Agr.Mtc = Agr.Mtc + l
							Agr.Ytc = Agr.Ytc + l
							Agr.Ttc = Agr.Ttc + l
	l = 100 * VAL(Rct.Oth): FOR z = 1 TO 5: Act.Oth(z) = Act.Oth(z) + l: NEXT
							Agr.Mot = Agr.Mot + l
							Agr.Yot = Agr.Yot + l
							Agr.Tot = Agr.Tot + l
	l = 100 * VAL(Rct.Wav): FOR z = 1 TO 5: Act.Wav(z) = Act.Wav(z) + l: NEXT
							Agr.Min = Agr.Min + l
							Agr.Yin = Agr.Yin + l
							Agr.Tin = Agr.Tin + l
	l = 100 * VAL(Rct.Ref): FOR z = 1 TO 5: Act.Ref(z) = Act.Ref(z) + l: NEXT
							Agr.Mrf = Agr.Mrf + l
							Agr.Yrf = Agr.Yrf + l
							Agr.Trf = Agr.Trf + l
	l = 100 * VAL(Rct.Crd): FOR z = 1 TO 5: Act.Crd(z) = Act.Crd(z) + l: NEXT
	l = 100 * VAL(Rct.Amt): FOR z = 1 TO 5: Act.Amt(z) = Act.Amt(z) + l: NEXT

						   
							FOR z = 1 TO 5: Act.Ltd(z) = Act.Ltd(z) + d: NEXT
							FOR z = 1 TO 5: Act.Una(z) = Act.Una(z) + Agr.Una: NEXT
	PUT #16, Agx.Num, Agr
	PUT #22, 1, Act
   'receipt post
   '------------
	n = 1 + LOF(19) \ LEN(Rct)
	RctNum = RctNum + 1
	Rcx.Agr = VAL(Rct.Agr)
	Rcx.Rct = VAL(Rct.Num)
	Rcx.Num = n
	PUT #19, Rcx.Num, Rct
	PUT #20, Rcx.Num, Rcx
   'inventory distribution
   '----------------------
	FOR x = 1 TO 4
		IF FindSer(Ser(x)) THEN
		   GET #10, Inx.Num, Inv
		   IF Agr.Sta = "P" THEN
			  s$ = s$ + " " + Inv.Ser
			  InvActv 10
			  InsActv 2, 1
			  Inv.Sta = 10
			  Inv.Act = "RP"
			  Inv.Dta = DATE$
		   END IF
		   l = Dis(x) * (VAL(Rct.Rnt) - VAL(Rct.Dis))
		   Inv.Min = Inv.Min + l
		   Inv.Yin = Inv.Yin + l
		   Inv.Tin = Inv.Tin + l
		   PUT #10, Inx.Num, Inv
		END IF
	NEXT
   'record transaction
   '------------------
	GET #19, Rcx.Num, Rcl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	IF Agr.Frq = "C" THEN Tra.Typ = "RC" ELSE Tra.Typ = "RN"
	Tra.Mgr = Pas.Ini
	Tra.Rec = Rcl.Rct
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Rct.Amt; Rct.Num; Rct.Agr; Cus.Nam
	IF Agr.Sta = "P" THEN
	   Tra.Typ = "AA"
	   PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Agr.Num; Cus.Nam; Agr.Rte; s$
	END IF
END SUB

DEFSNG A-Z
SUB RctPrnt ()
	Tra.Typ = "  "
	IF Response("Resp-06") THEN EXIT SUB
	IF PrinterReady%(2) = 0 THEN ERROR 209
	PRINT #24, CHR$(27); "@";
	PRINT #24, CHR$(27); "C"; "!";
	PRINT #24,
	PRINT #24,
	PRINT #24,
	PRINT #24,
	PRINT #24,
	PRINT #24,
	PRINT #24, TAB(4); Sto.Nam; TAB(48); Cus.Nam
	PRINT #24, TAB(4); Sto.Add; TAB(48); Cus.Nm1
	PRINT #24, TAB(4); Sto.Str; TAB(48); Cus.Add
	PRINT #24, TAB(4); RTRIM$(Sto.Cty); ", "; Sto.Sta; " "; Sto.Zip; TAB(48); Cus.Str
	PRINT #24, TAB(4); "Telephone: "; RIGHT$(Sto.Tel, 8); TAB(48); RTRIM$(Cus.Cty); ", "; Cus.Sta; " "; Cus.Zip
	PRINT #24,
	PRINT #24, : IF Rct.Nxt = Agr.Pod THEN Rct.Nxt = "[Paid-Out]"
	PRINT #24, USING "  &   &  &  &  &   &   &   &"; Agr.Num; Sto.Num; Rct.Dat; Rct.Nxt; Rct.Ddt; Rct.Mgr; Rct.Chk; Rct.Num
	PRINT #24,
	RctInve
	PRINT #24,
	PRINT #24, USING "              Rental Amount:  &               Waiver:  &"; Rct.Rnt; Rct.Wav
	PRINT #24, USING "                   Discount:  &            Referrals:  &"; Rct.Dis; Rct.Ref
	PRINT #24, USING "                  Sales Tax:  &               Credit:  &"; Rct.Tax; Rct.Crd
	PRINT #24, USING "                  Late Fees:  &           Amount Due:  &"; Rct.Lat; Rct.Due
	PRINT #24, USING "               Trip Charges:  &          Amount Paid:  &"; Rct.Trp; Rct.Amt
	PRINT #24, USING "              Other Charges:  &            Unapplied:  ####.##"; Rct.Oth; VAL(Rct.Due) - VAL(Rct.Amt)
	PRINT #24, CHR$(12);
	Tra.Typ = "RR"
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB RctPror ()
	IF DateDif(Agr.Pod, Rct.Nxt) >= 0 THEN             '=====================
	   Rct.Nxt = Agr.Pod                               'check for past payout
	   qPrint Rct.Nxt, 18, 64, 0, 1                    'date and adjust
	   qPrint " Payout ", 13, 60, 0, 1                 'show payout status
	ELSE                                               'or show
	   qPrint "        ", 13, 60, 0, 1                 'no payout status
	END IF                                             '=====================

	Lm = 100 * VAL(Agr.Mrt)                            'monthly rate
	Lw = 100 * VAL(Agr.Wrt)                            'weekly rate
	Ld = Lw / 7                                        'daily rate
	d = DateDif(Rct.Ddt, Rct.Nxt)                      'elapsed days
	m = VAL(LEFT$(Rct.Ddt, 2))                         'month number
	y = VAL(RIGHT$(Rct.Ddt, 4))                        '

	IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29     'leap year?
	WHILE d >= Ndm(m)                                  'repeat until
		IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29 'elapsed number
		d = d - Ndm(m)                                 'of whole months
		nm = nm + 1                                    'are determined
		m = m + 1: IF m > 12 THEN m = 1: y = y + 1     '
	WEND                                               '
	nw = d \ 7                                         'number of weeks
	nd = d MOD 7                                       'number of days

	IF Agr.Rct = 0 AND Agr.Frq = "M" THEN              '=====================
	   Lr = nm * Lm + (7 * nw + nd) * Lm / Ndm(m)      'prorate monthly on
	ELSE                                               'first receipt
	   Lr = nm * Lm + nw * Lw + nd * Ld                '
	END IF                                             '

	PyoAmnt Lp                                         'compute payout
	IF Lr > Lp THEN Lr = Lp                            'check if payout
	IF Lr > 99999 THEN ERROR 203                       'maximum rent exceeded
	Pdate = Rct.Nxt                                    '
	RctLate                                            'compute late
	IF Rct.Nxt <> Agr.Pod AND Agr.Wav = "Y" THEN       'no waiver on payout
	   IF Lm > 8000 THEN Lv = Lm * nm * .05 ELSE Lv = nm * 400
	   IF Lw > 2000 THEN Lv = Lv + (Lw * nw * .05) ELSE Lv = Lv + (nw * 100)
	   IF Ld > 2000 THEN Lv = Lv + (Ld * nd * .05) ELSE Lv = Lv + (nd / .07)
	ELSE
	   Lv = 0
	END IF
	
	COLOR 14, 2: LOCATE 28, 63: PRINT USING "####.##"; Lr / 100;
				 LOCATE 34, 63: PRINT USING "####.##"; Lv / 100;
	COLOR 1, 3:  LOCATE 39, 16: PRINT Rct.Nxt;
				 LOCATE 40, 39: PRINT USING "##m#w"; nm; nw;
				 LOCATE 41, 39: PRINT USING "####d"; nd;
	Picture 28, 63, 7, Rct.Rnt
	Picture 34, 63, 7, Rct.Wav
	Picture 40, 39, 5, Rct.Typ
	Picture 41, 39, 5, Rct.Day
	RctTotl
END SUB

DEFSNG A-Z
SUB RctStat ()
	SELECT CASE Rct.Sta
	CASE "A"
		 f$ = "                 "
	CASE "V"
		 f$ = "      Void       "
	CASE "B"
		 f$ = "Insuficient Funds"
	CASE ELSE
		 f$ = "  Unkown Status  "
	END SELECT
	qPrint f$, 14, 55, 0, 1
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB RctStup ()
	COLOR 14, 2                                            '
	LOCATE 36, 63: PRINT USING "####.##"; Agr.Una / 100;   'set the unapplied
	Picture 36, 63, 7, Rct.Crd                             'the credit field
	Rct.Sta = "A"                                          'active receipt
	Rct.Ddt = Agr.Nxt                                      'recipt due date
	Rct.Dis = "   0.00"                                    '
	Rct.Trp = "   0.00"                                    '
	Rct.Oth = "   0.00"                                    '
	Rct.Wav = "   0.00"                                    '
	Rct.Ref = "   0.00"                                    '
	Rct.Amt = "   0.00"                                    '==================

	IF Agr.Sta = "T" THEN                                  'if terminated
	   Rct.Ddt = Agr.Nxt                                   'prorate up to
	   Rct.Nxt = Agr.Dta                                   'last activity
	   Rct.Oth = "  10.00"                                 'date and add
	   RctPror                                             'other charges
	   EXIT SUB                                            '
	END IF                                                 '==================

	IF Agr.Rct = 0 THEN                                    'first receipt
	   RSET Rct.Oth = Agr.Pro                              'add processing fee
	   Rct.Nxt = Agr.Due                                   'set next due
	   RctPror                                             'and prorate
	   EXIT SUB                                            '
	END IF                                                 '==================

	Lm = 100 * VAL(Agr.Mrt)                                'monthly rate
	Lw = 100 * VAL(Agr.Wrt)                                'weekly rate
	Ld = Lw / 7                                            'daily rate
	Le = DateDif(Rct.Ddt, Rct.Dat)                         'elapsed days
	m = VAL(LEFT$(Rct.Ddt, 2))                             'month number
	y = VAL(RIGHT$(Rct.Ddt, 4))                            'year number

	IF Le > 0 THEN                                         '==================
	   IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29      'leap year?
	   WHILE Le >= Ndm(m)                                  'if past due
		  IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29   '   compute number
		  Le = Le - Ndm(m)                                 '   of monthly
		  nm = nm + 1                                      '   payments and
		  m = m + 1: IF m > 12 THEN m = 1: y = y + 1       '   compute number
	   WEND                                                '   of weekly
	   nw = (7 + Le) \ 7                                   '   payments
	   IF nw > 4 THEN nm = nm + 1: Le = Le - Ndm(m): nw = (7 + Le) \ 7
	ELSE                                                   '------------------
	   IF Agr.Frq = "W" THEN nw = 1 ELSE nm = 1            '   one week
	END IF                                                 '==================

	Lr = nm * Lm + nw * Lw: IF Lr > 99999 THEN ERROR 203   'compute rent
	Rct.Nxt = MonMath$(Rct.Ddt, nm)                        'compute next due
	Rct.Nxt = DayMath$(Rct.Nxt, nw * 7)                    'compute next due

	RctLate                                                'compute late
	PyoAmnt Lp                                             'compute payout
														   
	IF DateDif(Agr.Pod, Rct.Nxt) >= 0 OR Lr >= Lp THEN     'if exceeds payout
	   RctPror                                             'and prorate
	   EXIT SUB                                            '------------------
	END IF

	Pdate = Rct.Nxt
	IF Agr.Wav = "Y" THEN
	   IF Lm > 8000 THEN Lv = Lm * nm * .05 ELSE Lv = nm * 400
	   IF Lw > 2000 THEN Lv = Lv + (Lw * nw * .05) ELSE Lv = Lv + (nw * 100)
	   IF Ld > 2000 THEN Lv = Lv + (Ld * nd * .05) ELSE Lv = Lv + (nd / .07)
	   COLOR 14, 2: LOCATE 34, 63: PRINT USING "####.##"; Lv / 100;
	   Picture 34, 63, 7, Rct.Wav
	END IF

	qPrint "        ", 13, 60, 0, 1                        'no payout status
	COLOR 14, 2: LOCATE 28, 63: PRINT USING "####.##"; Lr / 100;
	COLOR 1, 3:  LOCATE 40, 39: PRINT USING "##m#w"; nm; nw;
				 LOCATE 41, 39: PRINT USING "####d"; nd;
	Picture 28, 63, 7, Rct.Rnt
	Picture 40, 39, 5, Rct.Typ
	Picture 41, 39, 5, Rct.Day
	RctTotl
END SUB

DEFSNG A-K, M-Z
SUB RctTotal ()
	COLOR 14, 2
	Lr = 100 * VAL(Rct.Rnt)                         'rent amount
	Ld = 100 * VAL(Rct.Dis)                         'discount
	Ls = 100 * VAL(Rct.Tax)                         'sales tax
	Ll = 100 * VAL(Rct.Lat)                         'late fee
	Lt = 100 * VAL(Rct.Trp)                         'trip charges
	Lo = 100 * VAL(Rct.Oth)                         'other charges
	Lw = 100 * VAL(Rct.Wav)                         'waiver charges
	Lf = 100 * VAL(Rct.Ref)                         'refferal
	Lc = 100 * VAL(Rct.Crd)                         'credit
	Lp = 100 * VAL(Rct.Amt)                         'amount paid

	La = Lr - Ld + Ls + Ll + Lt + Lo + Lw - Lf - Lc 'amount due
	Lu = Lp - La                                    'unapplied

	IF La > 99999 THEN ERROR 203                    'amount exceeded

	LOCATE 30, 63: PRINT USING "####.##"; Ls / 100; 'print tax
	LOCATE 38, 63: PRINT USING "####.##"; La / 100; 'amount due
	LOCATE 41, 63: PRINT USING "####.##"; Lu / 100; 'unapplied
	Picture 30, 63, 7, Rct.Tax                      '
	Picture 38, 63, 7, Rct.Due
	Agr.Una = Lu
END SUB

SUB RctTotl ()
	COLOR 14, 2
	Lr = 100 * VAL(Rct.Rnt)                         'rent amount
	Ld = 100 * VAL(Rct.Dis)                         'discount
	Ls = ((Lr - Ld) * VAL(Sto.Stx)) / 100           'sales tax
	Ll = 100 * VAL(Rct.Lat)                         'late fee
	Lt = 100 * VAL(Rct.Trp)                         'trip charges
	Lo = 100 * VAL(Rct.Oth)                         'other charges
	Lw = 100 * VAL(Rct.Wav)                         'waiver charges
	Lf = 100 * VAL(Rct.Ref)                         'refferal
	Lc = 100 * VAL(Rct.Crd)                         'credit
	Lp = 100 * VAL(Rct.Amt)                         'amount paid

	La = Lr - Ld + Ls + Ll + Lt + Lo + Lw - Lf - Lc 'amount due
	Lu = Lp - La                                    'unapplied

	IF La > 99999 THEN ERROR 203                    'amount exceeded

	LOCATE 30, 63: PRINT USING "####.##"; Ls / 100; 'print tax
	LOCATE 38, 63: PRINT USING "####.##"; La / 100; 'amount due
	LOCATE 41, 63: PRINT USING "####.##"; Lu / 100; 'unapplied
	Picture 30, 63, 7, Rct.Tax                      '
	Picture 38, 63, 7, Rct.Due
	Agr.Una = Lu
END SUB

DEFINT A-Z
FUNCTION RctType ()
	DIM rt(9) AS STRING * 2
	STATIC s$
y1: SELECT CASE Rct.Cod
	   CASE "1C"
			IF s$ <> Rct.Cod THEN Rct.Trp = "   0.00"
			Rct.Chk = "Cash      "
	   CASE "2C"
			IF s$ <> Rct.Cod THEN RSET Rct.Trp = Agr.Trp
			Rct.Chk = "Cash      "
	   CASE "3C"
			IF s$ <> Rct.Cod THEN Rct.Trp = "   0.00"
			Rct.Chk = "Cash      "
	   CASE "1M"
			IF s$ <> Rct.Cod THEN Rct.Trp = "   0.00"
			Rct.Chk = "MoneyOrder"
	   CASE "2M"
			IF s$ <> Rct.Cod THEN RSET Rct.Trp = Agr.Trp
			Rct.Chk = "MoneyOrder"
	   CASE "3M"
			IF s$ <> Rct.Cod THEN Rct.Trp = "   0.00"
			Rct.Chk = "MoneyOrder"
	   CASE "1K"
			IF s$ <> Rct.Cod THEN Rct.Trp = "   0.00": Rct.Chk = "          "
	   CASE "2K"
			IF s$ <> Rct.Cod THEN RSET Rct.Trp = Agr.Trp: Rct.Chk = "          "
	   CASE "3K"
			IF s$ <> Rct.Cod THEN Rct.Trp = "   0.00": Rct.Chk = "          "
	   CASE ELSE
			r = 16
			PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-Typ", e)
			rt(1) = "1C"
			rt(2) = "2C"
			rt(3) = "3C"
			rt(4) = "1K"
			rt(5) = "2K"
			rt(6) = "3K"
			rt(7) = "1M"
			rt(8) = "2M"
			rt(9) = "3M"
y2:         COLOR 16, 7: LOCATE r, 54: PRINT rt(r - 15);
			KeyPres f$
			COLOR 11, 1: LOCATE r, 54: PRINT rt(r - 15);
			SELECT CASE f$
			CASE CHR$(0) + "P"
				 IF r + 1 > 24 GOTO y2 ELSE r = r + 1
			CASE CHR$(0) + "H"
				 IF r - 1 < 16 GOTO y2 ELSE r = r - 1
			CASE CHR$(13)
				 PCOPY 1, 0: Rct.Cod = rt(r - 15): GOTO y1
			CASE CHR$(27)
				 RctType = 1: PCOPY 1, 0: EXIT FUNCTION
			CASE ELSE
			END SELECT
			GOTO y2
	   END SELECT
	   s$ = Rct.Cod
END FUNCTION

DEFLNG L
SUB RctVoid (p$)
	Picture 38, 39, 5, f$: f = VAL(f$)
	d = DateDif(Rct.Nxt, Rct.Ddt)
	Agr.Dta = DATE$
	Agr.Rct = Agr.Rct + 1
	Agr.Ndl = Agr.Ndl - f
	Agr.Una = Agr.Una + 100 * (VAL(Rct.Crd) - VAL(Rct.Amt) + VAL(Rct.Due))
	Agr.Nxt = DayMath$(Agr.Nxt, d)
	Agr.Npd = Agr.Npd - VAL(MID$(Rct.Day, 1, 4))
	Agr.Npw = Agr.Npw - VAL(MID$(Rct.Typ, 4, 1))
	Agr.Npm = Agr.Npm - VAL(MID$(Rct.Typ, 1, 2))

	GET #22, 1, Act
	IF Agr.Frq = "C" THEN
	l = 100 * VAL(Rct.Rnt): FOR z = 1 TO 5: Act.Sal(z) = Act.Sal(z) - l: NEXT
	ELSE
	IF Agr.Sta = "P" THEN
	l = 100 * VAL(Rct.Rnt): FOR z = 1 TO 5: Act.Pay(z) = Act.Pay(z) - l: NEXT
	ELSE
	l = 100 * VAL(Rct.Rnt): FOR z = 1 TO 5: Act.Rnt(z) = Act.Rnt(z) - l: NEXT
	END IF
	END IF
							Agr.Mri = Agr.Mri - l
							Agr.Yri = Agr.Yri - l
							Agr.Tri = Agr.Tri - l
	l = 100 * VAL(Rct.Dis): FOR z = 1 TO 5: Act.Dis(z) = Act.Dis(z) - l: NEXT
							Agr.Mds = Agr.Mds - l
							Agr.Yds = Agr.Yds - l
							Agr.Tds = Agr.Tds - l
	l = 100 * VAL(Rct.Tax): FOR z = 1 TO 5: Act.Tax(z) = Act.Tax(z) - l: NEXT
							Agr.Mtx = Agr.Mtx - l
							Agr.Ytx = Agr.Ytx - l
							Agr.Ttx = Agr.Ttx - l
	l = 100 * VAL(Rct.Lat): FOR z = 1 TO 5: Act.Lat(z) = Act.Lat(z) - l: NEXT
							Agr.Mlc = Agr.Mlc - l
							Agr.Ylc = Agr.Ylc - l
							Agr.Tlc = Agr.Tlc - l
	l = 100 * VAL(Rct.Trp): FOR z = 1 TO 5: Act.Trp(z) = Act.Trp(z) - l: NEXT
							Agr.Mtc = Agr.Mtc - l
							Agr.Ytc = Agr.Ytc - l
							Agr.Ttc = Agr.Ttc - l
	l = 100 * VAL(Rct.Oth): FOR z = 1 TO 5: Act.Oth(z) = Act.Oth(z) - l: NEXT
							Agr.Mot = Agr.Mot - l
							Agr.Yot = Agr.Yot - l
							Agr.Tot = Agr.Tot - l
	l = 100 * VAL(Rct.Wav): FOR z = 1 TO 5: Act.Wav(z) = Act.Wav(z) - l: NEXT
							Agr.Min = Agr.Min - l
							Agr.Yin = Agr.Yin - l
							Agr.Tin = Agr.Tin - l
	l = 100 * VAL(Rct.Ref): FOR z = 1 TO 5: Act.Ref(z) = Act.Ref(z) - l: NEXT
							Agr.Mrf = Agr.Mrf - l
							Agr.Yrf = Agr.Yrf - l
							Agr.Trf = Agr.Trf - l
	l = 100 * VAL(Rct.Crd): FOR z = 1 TO 5: Act.Crd(z) = Act.Crd(z) - l: NEXT
	l = 100 * VAL(Rct.Amt): FOR z = 1 TO 5: Act.Amt(z) = Act.Amt(z) - l: NEXT
							FOR z = 1 TO 5: Act.Ltd(z) = Act.Ltd(z) - f: NEXT
							FOR z = 1 TO 5: Act.Una(z) = Act.Una(z) - 100 * (VAL(Rct.Amt) - VAL(Rct.Due)): NEXT
	PUT #22, 1, Act
	FOR x = 1 TO 4                                     'reverse inventory
		IF FindSer(Ser(x)) THEN                        '-----------------
		   GET #10, Inx.Num, Inv
		   IF Inv.Sta = 10 AND Rct.Agr = Inv.Agr THEN
			  InsActv 2, -1
			  IF Agr.Frq = "C" THEN
				 Inv.Sta = 9
			  ELSE
				 Inv.Sta = 2
				 InvActv 2
			  END IF
			  s$ = s$ + " " + Inv.Ser
		   END IF
		   l = Dis(x) * (VAL(Rct.Rnt) - VAL(Rct.Dis))
		   Inv.Min = Inv.Min - l
		   Inv.Yin = Inv.Yin - l
		   Inv.Tin = Inv.Tin - l
		   PUT #10, Inx.Num, Inv
		END IF
	NEXT

	IF Agr.Sta = "P" THEN
	   Agr.Sta = "A"
	   Tra.Typ = "AU"
	   PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; s$
	END IF
	PUT #16, Agx.Num, Agr
	COLOR 14, 2
	f$ = "####.##"
	LOCATE 28, 63: PRINT USING f$; -VAL(Rct.Rnt); : Picture 28, 63, 7, Rct.Rnt
	LOCATE 29, 63: PRINT USING f$; -VAL(Rct.Dis); : Picture 29, 63, 7, Rct.Dis
	LOCATE 30, 63: PRINT USING f$; -VAL(Rct.Tax); : Picture 30, 63, 7, Rct.Tax
	LOCATE 31, 63: PRINT USING f$; -VAL(Rct.Lat); : Picture 31, 63, 7, Rct.Lat
	LOCATE 32, 63: PRINT USING f$; -VAL(Rct.Trp); : Picture 32, 63, 7, Rct.Trp
	LOCATE 33, 63: PRINT USING f$; -VAL(Rct.Oth); : Picture 33, 63, 7, Rct.Oth
	LOCATE 34, 63: PRINT USING f$; -VAL(Rct.Wav); : Picture 34, 63, 7, Rct.Wav
	LOCATE 35, 63: PRINT USING f$; -VAL(Rct.Ref); : Picture 35, 63, 7, Rct.Ref
	LOCATE 36, 63: PRINT USING f$; -VAL(Rct.Crd); : Picture 36, 63, 7, Rct.Crd
	LOCATE 38, 63: PRINT USING f$; -VAL(Rct.Due); : Picture 38, 63, 7, Rct.Due
	LOCATE 39, 63: PRINT USING f$; -VAL(Rct.Amt); : Picture 39, 63, 7, Rct.Amt
	Rct.Typ = "     "
	Rct.Day = "     "
END SUB

