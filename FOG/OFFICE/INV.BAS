DEFINT A-Z: DEFLNG L
DECLARE SUB mAC ()
DECLARE SUB mAA ()
DECLARE SUB mAT ()
DECLARE SUB mAV ()
DECLARE SUB mAN ()
DECLARE SUB mMU ()

DECLARE SUB mMO ()
DECLARE SUB mMI ()
DECLARE SUB mMM ()
DECLARE SUB mMT ()
DECLARE SUB mMR ()
DECLARE SUB mMP ()
DECLARE SUB mMG ()
DECLARE SUB mMC ()
DECLARE SUB mMN ()

DECLARE SUB SubMenu (s%, n$)
DECLARE SUB KeyPres (f$)
DECLARE SUB WinMssg (f$)
DECLARE SUB WinRate ()
DECLARE SUB CusShow ()
DECLARE SUB InvDisp ()

DECLARE FUNCTION InvItems (r)
DECLARE FUNCTION TraStat$ (f$)

DECLARE SUB InvShow ()
DECLARE SUB InvInfo ()
DECLARE SUB InvEdit ()
DECLARE SUB CusEdit ()
DECLARE SUB Statlin (f$)

REM $INCLUDE: '..\Include\Config.off'

DEFINT L
SUB CusEdit ()
	
620 EntryS 11, 54, 10, e, b, Cus.Sal:   IF e THEN 660 ELSE IF b THEN 620
621 EntryS 12, 54, 26, e, b, Cus.Nam:   IF e THEN 660 ELSE IF b THEN 620
	IF Cus.Nam < "!" THEN WinMssg "Customer name please.": GOTO 621
622 EntryS 13, 54, 26, e, b, Cus.Nm1:   IF e THEN 660 ELSE IF b THEN 621
623 EntryS 14, 54, 26, e, b, Cus.Add:   IF e THEN 660 ELSE IF b THEN 622
624 EntryS 15, 54, 26, e, b, Cus.Str:   IF e THEN 660 ELSE IF b THEN 623
625 EntryS 16, 54, 17, e, b, Cus.Cty:   IF e THEN 660 ELSE IF b THEN 624
626 EntryU 16, 72, 2, e, b, Cus.Sta:    IF e THEN 660 ELSE IF b THEN 625
627 EntryN 16, 75, 5, 0, e, b, Cus.Zip: IF e THEN 660 ELSE IF b THEN 626
628 EntryT 17, 54, e, b, Cus.Tel:       IF e THEN 660 ELSE IF b THEN 627
629 EntryS 22, 44, 36, e, b, Cus.Rmk:   IF e THEN 660 ELSE IF b THEN 628

630 EntryX 27, 49, e, b, Cus.SSn:       IF e THEN 660 ELSE IF b THEN 629
631 EntryU 28, 49, 11, e, b, Cus.Dln:   IF e THEN 660 ELSE IF b THEN 630
632 EntryD 29, 49, e, b, Cus.Dob:       IF e THEN 660 ELSE IF b THEN 631
633 EntryU 30, 49, 1, e, b, Cus.Mar:    IF e THEN 660 ELSE IF b THEN 632
634 EntryU 31, 49, 1, e, b, Cus.Rac:    IF e THEN 660 ELSE IF b THEN 633
635 EntryU 32, 49, 1, e, b, Cus.Sex:    IF e THEN 660 ELSE IF b THEN 634

636 EntryU 36, 49, 15, e, b, Cus.Emp:   IF e THEN 660 ELSE IF b THEN 635
637 EntryU 37, 49, 15, e, b, Cus.Lon:   IF e THEN 660 ELSE IF b THEN 636
638 EntryU 38, 49, 15, e, b, Cus.Inc:   IF e THEN 660 ELSE IF b THEN 637
639 EntryU 39, 49, 15, e, b, Cus.Oth:   IF e THEN 660 ELSE IF b THEN 638
640 EntryT 40, 49, e, b, Cus.Wor:       IF e THEN 660 ELSE IF b THEN 639

641 EntryN 44, 49, 3, 0, e, b, Cus.Dis: IF e THEN 660 ELSE IF b THEN 640
642 EntryU 45, 49, 1, e, b, Cus.Ref:    IF e THEN 660 ELSE IF b THEN 641
643 EntryU 46, 49, 1, e, b, Cus.Typ:    IF e THEN 660 ELSE IF b THEN 642

644 EntryX 27, 65, e, b, Cus.SS1:       IF e THEN 660 ELSE IF b THEN 643
645 EntryU 28, 65, 11, e, b, Cus.Dl1:   IF e THEN 660 ELSE IF b THEN 644
646 EntryD 29, 65, e, b, Cus.Bd1:       IF e THEN 660 ELSE IF b THEN 645
647 EntryU 30, 65, 1, e, b, Cus.St1:    IF e THEN 660 ELSE IF b THEN 646
648 EntryU 31, 65, 1, e, b, Cus.Rc1:    IF e THEN 660 ELSE IF b THEN 647
649 EntryU 32, 65, 1, e, b, Cus.Sx1:    IF e THEN 660 ELSE IF b THEN 648

650 EntryU 36, 65, 15, e, b, Cus.Em1:   IF e THEN 660 ELSE IF b THEN 649
651 EntryU 37, 65, 15, e, b, Cus.Ln1:   IF e THEN 660 ELSE IF b THEN 650
652 EntryU 38, 65, 15, e, b, Cus.In1:   IF e THEN 660 ELSE IF b THEN 651
653 EntryU 39, 65, 15, e, b, Cus.Ot1:   IF e THEN 660 ELSE IF b THEN 652
654 EntryT 40, 65, e, b, Cus.Wt1:       IF e THEN 660 ELSE IF b THEN 653
	IF Confirm(0, "SAVE Customer") = 0 THEN 654
	UPDATE #11, Cus
660
END SUB

DEFSNG A-Z
SUB CusShow ()
	qPrint Cus.Sto + Cus.Num, 5, 54, 0, 1
	qPrint Sto.Nam, 6, 54, 0, 1

	qPrint Cus.Sal, 11, 54, 0, 1
	qPrint Cus.Nam, 12, 54, 0, 1
	qPrint Cus.Nm1, 13, 54, 0, 1
	qPrint Cus.Add, 14, 54, 0, 1
	qPrint Cus.Str, 15, 54, 0, 1
	qPrint Cus.Cty, 16, 54, 0, 1
	qPrint Cus.Sta, 16, 72, 0, 1
	qPrint Cus.Zip, 16, 75, 0, 1
	qPrint Cus.Tel, 17, 54, 0, 1
	qPrint Cus.Rmk, 22, 44, 0, 1

	qPrint Cus.SSn, 27, 49, 0, 1
	qPrint Cus.Dln, 28, 49, 0, 1
	qPrint Cus.Dob, 29, 49, 0, 1
	qPrint Cus.Mar, 30, 49, 0, 1
	qPrint Cus.Rac, 31, 49, 0, 1
	qPrint Cus.Sex, 32, 49, 0, 1

	qPrint Cus.Emp, 36, 49, 0, 1
	qPrint Cus.Lon, 37, 49, 0, 1
	qPrint Cus.Inc, 38, 49, 0, 1
	qPrint Cus.Oth, 39, 49, 0, 1
	qPrint Cus.Wor, 40, 49, 0, 1

	qPrint Cus.Dis, 44, 49, 0, 1
	qPrint Cus.Ref, 45, 49, 0, 1
	qPrint Cus.Typ, 46, 49, 0, 1

	qPrint Cus.SS1, 27, 65, 0, 1
	qPrint Cus.Dl1, 28, 65, 0, 1
	qPrint Cus.Bd1, 29, 65, 0, 1
	qPrint Cus.St1, 30, 65, 0, 1
	qPrint Cus.Rc1, 31, 65, 0, 1
	qPrint Cus.Sx1, 32, 65, 0, 1

	qPrint Cus.Em1, 36, 65, 0, 1
	qPrint Cus.Ln1, 37, 65, 0, 1
	qPrint Cus.In1, 38, 65, 0, 1
	qPrint Cus.Ot1, 39, 65, 0, 1
	qPrint Cus.Wt1, 40, 65, 0, 1
END SUB

DEFINT A-Z
SUB InvDisp ()
	IF FindCod(Inv.Cod) = 0 THEN WinMssg "Code not found"
	qPrint Inv.Sto, 9, 39, 0, 1
	qPrint Inv.Typ, 9, 45, 0, 1
	qPrint Inv.Grp, 9, 46, 0, 1
	qPrint Inv.Cod, 9, 50, 0, 1
	qPrint Cod.Cmb, 9, 57, 0, 1
	qPrint Inv.Bra, 9, 62, 0, 1
	qPrint Status(Inv.Sta), 9, 67, 0, 1
	qPrint Cod.Des, 11, 54, 0, 1
	qPrint Inv.Mdl, 13, 54, 0, 1
	qPrint Inv.Ser, 14, 54, 0, 1
	qPrint Inv.Dis, 15, 54, 0, 1
	qPrint Inv.Dtp, 17, 54, 0, 1
	qPrint Inv.Dtr, 18, 54, 0, 1
	qPrint Inv.Dta, 19, 54, 0, 1
	qPrint TraStat(Inv.Act), 19, 65, 0, 1
	IF Inv.Tfr > " 0" THEN GET #6, VAL(Inv.Tfr), Sto: qPrint Sto.Nam, 21, 54, 0, 1
	IF Inv.Tto > " 0" THEN GET #6, VAL(Inv.Tto), Sto: qPrint Sto.Nam, 22, 54, 0, 1
	IF Inv.Ctr > " 0" THEN GET #7, VAL(Inv.Ctr), Ctr: qPrint Ctr.Nam, 23, 54, 0, 1
	qPrint Inv.Agr, 25, 54, 0, 1
	qPrint SPACE$(26), 26, 54, 0, 1

	qPrint Cod.Fmv, 28, 59, 0, 1
	qPrint Cod.Dtr, 29, 59, 0, 1
	qPrint Cod.Dfm, 30, 59, 0, 1
	qPrint Inv.Prc, 31, 56, 0, 1
	qPrint Inv.Pon, 33, 54, 0, 1
	qPrint Inv.Rat, 35, 54, 0, 1
	COLOR 11, 3: LOCATE 36, 54: PRINT USING "###"; Inv.Tim;
'   qPrint STR$(Inv.Tim), 36, 54, 0, 1
	qPrint Inv.Del, 37, 55, 0, 1
	qPrint Inv.Ovr, 38, 55, 0, 1
	qPrint Inv.Par, 40, 54, 0, 1
	qPrint Inv.Lab, 41, 54, 0, 1
END SUB

SUB InvEdit ()
	s$ = LTRIM$(STR$(Inv.Sta))
	Picture 34, 54, 3, t$
	
230 EntryU 4, 39, 2, e, b, Inv.Sto:     IF e THEN EXIT SUB ELSE IF b THEN 230
	IF Inv.Sto > " 0" THEN GET #6, VAL(Inv.Sto), Sto: Inv.Sto = Sto.Num
	qPrint Inv.Sto, 4, 39, 0, 1
231 EntryU 4, 50, 4, e, b, Inv.Cod:     IF e THEN EXIT SUB ELSE IF b THEN 230
	IF FindCod(Inv.Cod) = 0 THEN WinMssg "No such code": GOTO 231

	qPrint Cod.Typ, 4, 45, 0, 1
	qPrint Cod.Grp, 4, 46, 0, 1
	qPrint Cod.Cod, 4, 50, 0, 1
	qPrint Cod.Cmb, 4, 57, 0, 1
	qPrint Cod.Des, 6, 54, 0, 1
	qPrint Cod.Fmv, 26, 59, 0, 1
	qPrint Cod.Dtr, 27, 59, 0, 1
	qPrint Cod.Dfm, 28, 59, 0, 1
	qPrint Cod.Rat, 33, 54, 0, 1

232 EntryU 4, 62, 3, e, b, Inv.Bra:     IF e THEN EXIT SUB ELSE IF b THEN 231
233 EntryN 4, 67, 2, 0, e, b, s$:       IF e THEN EXIT SUB ELSE IF b THEN 232
	IF s$ < " 0" OR s$ > "15" THEN WinMssg "Status: 0-15": GOTO 232
	Inv.Sta = VAL(s$)
	qPrint Status(Inv.Sta), 4, 67, 0, 1
234 EntryU 8, 54, 10, e, b, Inv.Mdl:    IF e THEN EXIT SUB ELSE IF b THEN 233
235 EntryU 9, 54, 10, e, b, Inv.Ser:    IF e THEN EXIT SUB ELSE IF b THEN 234
236 EntryU 10, 54, 10, e, b, Inv.Dis:   IF e THEN EXIT SUB ELSE IF b THEN 235
237 EntryD 12, 54, e, b, Inv.Dtp:       IF e THEN EXIT SUB ELSE IF b THEN 236
238 EntryD 13, 54, e, b, Inv.Dtr:       IF e THEN EXIT SUB ELSE IF b THEN 237
239 EntryD 14, 54, e, b, Inv.Dta:       IF e THEN EXIT SUB ELSE IF b THEN 238
240 EntryU 16, 54, 2, e, b, Inv.Act:    IF e THEN EXIT SUB ELSE IF b THEN 239
	qPrint TraStat(Inv.Act), 16, 54, 0, 1
241 EntryU 17, 54, 3, e, b, Inv.Ini:    IF e THEN EXIT SUB ELSE IF b THEN 240
	IF FindIni(Inv.Ini) THEN qPrint Pas.Nam, 17, 54, 0, 1
242 EntryN 19, 54, 2, 0, e, b, Inv.Tfr: IF e THEN EXIT SUB ELSE IF b THEN 241
	IF Inv.Tfr > " 0" THEN
	   GET #6, VAL(Inv.Tfr), Sto
	   qPrint Sto.Nam, 19, 54, 0, 1
	ELSE
	   qPrint SPACE$(26), 19, 54, 0, 1
	END IF
243 EntryN 20, 54, 2, 0, e, b, Inv.Tto: IF e THEN EXIT SUB ELSE IF b THEN 242
	IF Inv.Tto > " 0" THEN
	   GET #6, VAL(Inv.Tto), Sto
	   qPrint Sto.Nam, 20, 54, 0, 1
	ELSE
	   qPrint SPACE$(26), 20, 54, 0, 1
	END IF
244 EntryN 21, 54, 2, 0, e, b, Inv.Ctr: IF e THEN EXIT SUB ELSE IF b THEN 243
	IF Inv.Ctr > " 0" THEN
	   GET #7, VAL(Inv.Ctr), Ctr
	   qPrint Ctr.Nam, 21, 54, 0, 1
	ELSE
	   qPrint SPACE$(26), 21, 54, 0, 1
	END IF
245 EntryS 23, 54, 6, e, b, Inv.Agr:    IF e THEN EXIT SUB ELSE IF b THEN 244
246 EntryN 29, 56, 7, 2, e, b, Inv.Prc: IF e THEN EXIT SUB ELSE IF b THEN 245
247 EntryU 31, 54, 10, e, b, Inv.Pon:   IF e THEN EXIT SUB ELSE IF b THEN 246
248 EntryU 33, 54, 3, e, b, Inv.Rat:    IF e THEN EXIT SUB ELSE IF b THEN 247
249 EntryN 34, 54, 3, 0, e, b, t$:      IF e THEN EXIT SUB ELSE IF b THEN 248
	Inv.Tim = VAL(t$)
250 EntryN 38, 54, 3, 0, e, b, Inv.Par: IF e THEN EXIT SUB ELSE IF b THEN 249
251 EntryN 39, 54, 3, 0, e, b, Inv.Lab: IF e THEN EXIT SUB ELSE IF b THEN 250
	IF Confirm(1, "Update Item") = 0 THEN 251

	Inv.Typ = Cod.Typ
	Inv.Grp = Cod.Grp
	Inv.Cod = Cod.Cod
	Inv.Rat = Cod.Rat
	UPDATE #10, Inv
END SUB

DEFLNG L
DEFSNG A-K, M-Z
SUB InvInfo ()
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "INV-Info", e)
	COLOR 10, 0
	r$ = "  ###### ³  ###### ³  ###### "
	s$ = "#####.## ³#####.## ³#####.## "
	IF Inv.Sta > 8 THEN d$ = Inv.Dta ELSE d$ = DATE$
	t = DateDif&(Inv.Dtp, d$)
	y = DateDif&("01-01" + RIGHT$(DATE$, 5), d$)
	m = DateDif&(LEFT$(DATE$, 3) + "01" + RIGHT$(DATE$, 5), d$)
	IF y < 0 THEN y = 0 ELSE IF y > t THEN y = t
	IF m < 0 THEN m = 0 ELSE IF m > y THEN m = y
	LOCATE 21, 21: PRINT USING r$; m; y; t;
	LOCATE 22, 21: PRINT USING r$; Inv.Mdi; Inv.Ydi; Inv.Tdi;
	LOCATE 23, 21: PRINT USING r$; Inv.Mdr; Inv.Ydr; Inv.Tdr;
	LOCATE 24, 21: PRINT USING r$; Inv.Mas; Inv.Yas; Inv.Tas;
	LOCATE 25, 21: PRINT USING r$; Inv.Mds; Inv.Yds; Inv.Tds;
	LOCATE 26, 21: PRINT USING r$; Inv.Mar; Inv.Yar; Inv.Tar;
	LOCATE 28, 21: PRINT USING s$; Inv.Min / 100; Inv.Yin / 100; Inv.Tin / 100;
	LOCATE 29, 21: PRINT USING s$; Inv.Mde / 100; Inv.Yde / 100; Inv.Tde / 100;
	LOCATE 30, 21: PRINT USING s$; Inv.Mre / 100; Inv.Yre / 100; Inv.Tre / 100;
	KeyPres f$
	PCOPY 1, 0
END SUB

DEFINT A-Z
FUNCTION InvItems (r)
99  EntryU r, 3, 10, e, b, Inv.Ser
	IF e THEN InvItems = 1: EXIT FUNCTION
	IF b THEN InvItems = 2: EXIT FUNCTION
'   IF FindSer(Inv.Ser) = 0 THEN IF WinItem(0, 0) THEN 99
END FUNCTION

DEFSNG A-Z
SUB InvShow ()
	f$ = SPACE$(26)
	t$ = SPACE$(26)
	s$ = SPACE$(26)
	p$ = SPACE$(26)

	RETRIEVE #10, Inv
	IF Inv.Tfr > " 0" THEN GET #6, VAL(Inv.Tfr), Sto: f$ = Sto.Nam
	IF Inv.Tto > " 0" THEN GET #6, VAL(Inv.Tto), Sto: t$ = Sto.Nam
	IF Inv.Ctr > " 0" THEN GET #7, VAL(Inv.Ctr), Ctr: s$ = Ctr.Nam

	IF FindCod(Inv.Cod) = 0 THEN WinMssg "Code not found"
	IF FindIni(Inv.Ini) THEN p$ = Pas.Nam

	qPrint Inv.Sto, 4, 39, 0, 1
	qPrint Inv.Typ, 4, 45, 0, 1
	qPrint Inv.Grp, 4, 46, 0, 1
	qPrint Inv.Cod, 4, 50, 0, 1
	qPrint Cod.Cmb, 4, 57, 0, 1
	qPrint Inv.Bra, 4, 62, 0, 1
	qPrint Status(Inv.Sta), 4, 67, 0, 1

	qPrint Cod.Des, 6, 54, 0, 1

	qPrint Inv.Mdl, 8, 54, 0, 1
	qPrint Inv.Ser, 9, 54, 0, 1
	qPrint Inv.Dis, 10, 54, 0, 1

	qPrint Inv.Dtp, 12, 54, 0, 1
	qPrint Inv.Dtr, 13, 54, 0, 1
	qPrint Inv.Dta, 14, 54, 0, 1

	qPrint TraStat(Inv.Act), 16, 54, 0, 1
	qPrint p$, 17, 54, 0, 1

	qPrint f$, 19, 54, 0, 1
	qPrint t$, 20, 54, 0, 1
	qPrint s$, 21, 54, 0, 1

	qPrint Inv.Agr, 23, 54, 0, 1
	qPrint SPACE$(26), 24, 54, 0, 1

	qPrint Cod.Fmv, 26, 59, 0, 1
	qPrint Cod.Dtr, 27, 59, 0, 1
	qPrint Cod.Dfm, 28, 59, 0, 1
	qPrint Inv.Prc, 29, 56, 0, 1

	qPrint Inv.Pon, 31, 54, 0, 1

	qPrint Inv.Rat, 33, 54, 0, 1
	COLOR 11, 3: LOCATE 34, 54: PRINT USING "###"; Inv.Tim;
'   qPrint STR$(Inv.Tim), 34, 54, 0, 1
	qPrint Inv.Del, 35, 55, 0, 1
	qPrint Inv.Ovr, 36, 55, 0, 1

	qPrint Inv.Par, 38, 54, 0, 1
	qPrint Inv.Lab, 39, 54, 0, 1

	qPrint MID$(Inv.Rmk, 1, 24), 41, 54, 0, 1
	qPrint MID$(Inv.Rmk, 25, 24), 42, 54, 0, 1
	qPrint MID$(Inv.Rmk, 49, 21), 43, 54, 0, 1
END SUB

DEFINT A-Z
SUB mAA ()
	 CALL MhDisplayByName(0, 0, "IU", e)
	 Statlin "Pay-Out Agreement"
AA0: EntryU 11, 10, 3, e, b, aIni: IF e THEN EXIT SUB ELSE IF b GOTO AA0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO AA0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 SETINDEX 10, "Cus"
AA1: EntryU 13, 22, 6, e, b, aAgr: IF e THEN EXIT SUB ELSE IF b GOTO AA0
	 SEEKEQ #10, aSto, aAgr
	 IF EOF(10) THEN WinMssg "Agreement not found": GOTO AA1
	 RETRIEVE #10, Inv
	 x = 0
	 ERASE aSer
	 CALL MhDisplayByName(0, 0, "IA", e)
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
	 DO WHILE aSto + aAgr = Inv.Sto + Inv.Agr
		x = x + 1
		aSer(x) = Inv.Ser
		qPrint Inv.Ser, 17 + x, 2, 0, 1
		qPrint Status(Inv.Sta), 17 + x, 13, 0, 1
		qPrint Status(10), 17 + x, 23, 0, 1
		InvDisp
		IF Inv.Sta <> 2 THEN WinMssg "Item is not ON-RENT"
		MOVENEXT #10
		RETRIEVE #10, Inv
	 LOOP
	 IF Confirm(0, "PAY-OUT Agreement") = 0 GOTO AA1
	 SETINDEX 10, "Ser"
	 FOR x = 1 TO 8
	 IF aSer(x) > "!" THEN
		SEEKEQ #10, aSer(x)
		RETRIEVE #10, Inv
		Inv.Sto = aSto
		Inv.Dta = aDat
		Inv.Act = aAct
		Inv.Ini = aIni
		Inv.Sta = 10
		UPDATE #10, Inv: InvDisp
	 END IF
	 NEXT
	 GOTO AA0
END SUB

SUB mAC ()
	 CALL MhDisplayByName(0, 0, "IU", e)
	 Statlin "Charge-Off Agreement"
AC0: EntryU 11, 10, 3, e, b, aIni: IF e THEN EXIT SUB ELSE IF b GOTO AC0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO AC0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 SETINDEX 10, "Cus"
AC1: EntryU 13, 22, 6, e, b, aAgr: IF e THEN EXIT SUB ELSE IF b GOTO AC0
	 SEEKEQ #10, aSto, aAgr
	 IF EOF(10) THEN WinMssg "Agreement not found": GOTO AC1
	 RETRIEVE #10, Inv
	 x = 0
	 ERASE aSer
	 CALL MhDisplayByName(0, 0, "IA", e)
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
	 DO
		x = x + 1
		aSer(x) = Inv.Ser
		qPrint Inv.Ser, 17 + x, 2, 0, 1
		qPrint Status(Inv.Sta), 17 + x, 13, 0, 1
		InvDisp
		SELECT CASE Inv.Sta
			   CASE 2: qPrint Status(11), 17 + x, 23, 0, 1
			   CASE 3: qPrint Status(0), 17 + x, 23, 0, 1
			   CASE 4: qPrint Status(1), 17 + x, 23, 0, 1
			   CASE 5: qPrint Status(0), 17 + x, 23, 0, 1
			   CASE 8: qPrint Status(11), 17 + x, 23, 0, 1
			   CASE ELSE: WinMssg "Check status"
					   qPrint Status(11), 17 + x, 23, 0, 1
		END SELECT
		MOVENEXT #10
		RETRIEVE #10, Inv
	 LOOP WHILE aSto + aAgr = Inv.Sto + Inv.Agr

	 IF Confirm(0, "CHARGE-OFF Agreement") = 0 GOTO AC1
	 SETINDEX 10, "Ser"
	 FOR x = 1 TO 8
	 IF aSer(x) > "!" THEN
		SEEKEQ #10, aSer(x)
		RETRIEVE #10, Inv
		Inv.Sto = aSto
		Inv.Dta = aDat
		Inv.Act = aAct
		Inv.Ini = aIni
		Inv.Agr = "      "
		SELECT CASE Inv.Sta
			   CASE 1: Inv.Sta = 11 'OR      charge-off
			   CASE 3: Inv.Sta = 0  'OR-AS   idle
			   CASE 4: Inv.Sta = 1  'OR-IN   in-service
			   CASE 5: Inv.Sta = 0  'OR-AR   idle
			   CASE 8: Inv.Sta = 11 'Loaner  charge-off
			   CASE ELSE
					   Inv.Sta = 11 'else    charge-off
		END SELECT
		UPDATE #10, Inv: InvDisp
	 END IF
	 NEXT
	 GOTO AC0
END SUB

SUB mAgrMas ()
	CALL MhDisplayByName(0, 0, "Mast-Agr", e)
	SubMenu 1, ""
	SETINDEX 10, "Agr"
	i = 1: o = 0
	i$ = "Agr": o$ = "Ser"
	qPrint "Index: " + GETINDEX$(10) + STR$(LOF(10)), 50, 64, 0, 1
	MOVEFIRST #10
	GOSUB AgrDn
	MOVEFIRST #10
	r = 3

325 RETRIEVE #10, Inv
	COLOR 15, 1: LOCATE r, 2: PRINT USING "&³&³&³&³&³&"; Inv.Sto; Inv.Agr; Inv.Typ + Inv.Grp; Inv.Cod; Inv.Ser; Inv.Act;
	KeyPres n$
	IF n$ = CHR$(13) THEN SubMenu 0, n$
	IF n$ = CHR$(0) + "P" OR n$ = CHR$(0) + "H" THEN
	   COLOR 16 + Inv.Sta, 7
	   LOCATE , 2:  PRINT Inv.Sto;
	   LOCATE , 5:  PRINT Inv.Agr;
	   LOCATE , 12: PRINT Inv.Typ + Inv.Grp;
	   LOCATE , 16: PRINT Inv.Cod;
	   LOCATE , 21: PRINT Inv.Ser;
	   LOCATE , 32: PRINT Inv.Act;
	END IF
	SELECT CASE n$
	CASE CHR$(27) 'Esc '==========Escape========
		EXIT SUB
	CASE CHR$(0) + "M" '==========Right=========
		SubMenu 0, n$
	CASE CHR$(0) + "K" '==========Left==========
		SubMenu 0, n$
	CASE CHR$(0) + "P" '==========Down==========
		MOVENEXT #10
		IF EOF(10) THEN
		   MOVELAST #10
		ELSE
		   IF r + 1 > 47 THEN CALL Scroll(2, 3, 33, 47, 1) ELSE r = r + 1
		END IF
	CASE CHR$(0) + "H" '==========Up=============
		MOVEPREVIOUS #10
		IF BOF(10) THEN
		   MOVEFIRST #10
		ELSE
		   IF r - 1 < 3 THEN CALL BkScroll(2, 3, 33, 47, 1) ELSE r = r - 1
		END IF
	CASE CHR$(0) + "I" '==========PgUp===========
		FOR x = r TO -43 STEP -1
			MOVEPREVIOUS #10
			IF BOF(10) THEN
			   MOVEFIRST #10
			   GOSUB AgrDn
			   IF i THEN
				  SEEKEQ #10, r$, t$, g$, c$, m$, s$
			   ELSE
				  SEEKEQ #10, r$
			   END IF
			   GOTO 325
			END IF
			IF x = 3 THEN
			   RETRIEVE #10, Inv
			   IF i THEN
				  r$ = Inv.Sto: t$ = Inv.Agr: g$ = Inv.Typ: c$ = Inv.Grp: m$ = Inv.Cod: s$ = Inv.Ser
			   ELSE
				  r$ = Inv.Ser
			   END IF
			END IF
		NEXT
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
		GOSUB AgrUp
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
	CASE CHR$(0) + "Q" '==========PgDn===========
		FOR x = r TO 92
			MOVENEXT #10
			IF EOF(10) THEN
			   MOVELAST #10
			   GOSUB AgrUp
			   IF i THEN
				  SEEKEQ #10, r$, t$, g$, c$, m$, s$
			   ELSE
				  SEEKEQ #10, r$
			   END IF
			   GOTO 325
			END IF
			IF x = 47 THEN
			   RETRIEVE #10, Inv
			   IF i THEN
				  r$ = Inv.Sto: t$ = Inv.Agr: g$ = Inv.Typ: c$ = Inv.Grp: m$ = Inv.Cod: s$ = Inv.Ser
			   ELSE
				  r$ = Inv.Ser
			   END IF
			END IF
		NEXT
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
		GOSUB AgrDn
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
	CASE CHR$(0) + "G" '==========Home===========
		MOVEFIRST #10
		GOSUB AgrDn
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
	CASE CHR$(0) + "O" '==========End============
		MOVELAST #10
		GOSUB AgrUp
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
	CASE "D"           '==========Display========
		SubMenu 0, n$
		InvShow
	CASE "I"           '==========Index==========
		SubMenu 0, n$
		SWAP i$, o$
		SWAP i, o
		SETINDEX 10, i$
		qPrint "Index: " + GETINDEX$(10) + STR$(LOF(10)), 50, 64, 0, 1
		MOVEFIRST #10
		GOSUB AgrDn
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
	CASE "R"           '==========Rates==========
		SubMenu 0, n$
		InvShow
		GET #4, VAL(Inv.Rat), Rat
		WinRate
	CASE "S"           '==========Serial=========
		SubMenu 0, n$
		IF GETINDEX$(10) = "Agr" THEN WinMssg "Must switch index": GOTO 325
		f$ = Inv.Ser
328     EntryU 9, 54, 10, e, b, Inv.Ser: IF e THEN 325 ELSE IF b THEN 328
		SEEKEQ #10, Inv.Ser
		IF EOF(10) THEN
		   WinMssg "Not found"
		   SEEKEQ #10, f$
		   InvShow
		   GOTO 325
		END IF
		f$ = Inv.Ser
		FOR x = 3 TO r
		   MOVEPREVIOUS #10
		   IF BOF(10) THEN
			  MOVEFIRST #10
			  GOSUB AgrDn
			  SEEKEQ #10, f$
			  r = x
			  InvShow
			  GOTO 325
		   END IF
		NEXT
		SEEKEQ #10, f$
		FOR x = r TO 46
		   MOVENEXT #10
		   IF EOF(10) THEN
			  MOVELAST #10
			  GOSUB AgrUp
			  SEEKEQ #10, f$
			  r = x
			  InvShow
			  GOTO 325
		   END IF
		NEXT
		GOSUB AgrUp
		SEEKEQ #10, f$
		r = x
		InvShow
	CASE "L"           '==========Store==========
		SubMenu 0, n$
		IF GETINDEX$(10) = "Ser" THEN WinMssg "Must switch index": GOTO 325
329     EntryU 4, 39, 2, e, b, Inv.Sto: IF e THEN 325 ELSE IF b THEN 329
		SEEKGE #10, Inv.Sto
		IF EOF(10) THEN WinMssg "Not found"
		RETRIEVE #10, Inv
		r$ = Inv.Sto: t$ = Inv.Agr: g$ = Inv.Typ: c$ = Inv.Grp: m$ = Inv.Cod: s$ = Inv.Ser
		FOR x = 3 TO 47
			MOVENEXT #10
			IF EOF(10) THEN
			   MOVELAST #10
			   GOSUB AgrUp
			   SEEKEQ #10, r$, t$, g$, c$, m$, s$
			   GOTO 325
			END IF
		NEXT
		SEEKEQ #10, r$, t$, g$, c$, m$, s$
		GOSUB AgrDn
		SEEKEQ #10, r$, t$, g$, c$, m$, s$
	CASE "E"           '==========Erase==========
		SubMenu 0, n$
		InvShow
		IF Confirm(1, "ERASE Item") = 0 THEN 325
		DELETE #10
		qPrint "Index: " + GETINDEX$(10) + STR$(LOF(10)), 50, 64, 0, 1
	CASE "M"           '==========Modify=========
		SubMenu 0, n$
		InvShow
		InvEdit
	CASE ELSE
	END SELECT
	GOTO 325

AgrDn:
	LOCATE 3
	WHILE CSRLIN < 48
		RETRIEVE #10, Inv
		IF Inv.Ser = f$ THEN x = CSRLIN
		IF CSRLIN = r THEN
		   IF i THEN
			  r$ = Inv.Sto: t$ = Inv.Agr: g$ = Inv.Typ: c$ = Inv.Grp: m$ = Inv.Cod: s$ = Inv.Ser
		   ELSE
			  r$ = Inv.Ser
		   END IF
		END IF
		COLOR 16 + Inv.Sta, 7
		LOCATE , 2:  PRINT Inv.Sto;
		LOCATE , 5:  PRINT Inv.Agr;
		LOCATE , 12: PRINT Inv.Typ + Inv.Grp;
		LOCATE , 16: PRINT Inv.Cod;
		LOCATE , 21: PRINT Inv.Ser;
		LOCATE , 32: PRINT Inv.Act;
		LOCATE CSRLIN + 1
		MOVENEXT #10
	WEND
	RETURN
AgrUp:
	LOCATE 47
	WHILE CSRLIN > 2
		RETRIEVE #10, Inv
		IF Inv.Ser = f$ THEN x = CSRLIN
		IF CSRLIN = r THEN
		   IF i THEN
			  r$ = Inv.Sto: t$ = Inv.Agr: g$ = Inv.Typ: c$ = Inv.Grp: m$ = Inv.Cod: s$ = Inv.Ser
		   ELSE
			  r$ = Inv.Ser
		   END IF
		END IF
		COLOR 16 + Inv.Sta, 7
		LOCATE , 2:  PRINT Inv.Sto;
		LOCATE , 5:  PRINT Inv.Agr;
		LOCATE , 12: PRINT Inv.Typ + Inv.Grp;
		LOCATE , 16: PRINT Inv.Cod;
		LOCATE , 21: PRINT Inv.Ser;
		LOCATE , 32: PRINT Inv.Act;
		LOCATE CSRLIN - 1
		MOVEPREVIOUS #10
	WEND
	RETURN
END SUB

SUB mAN ()
	 CALL MhDisplayByName(0, 0, "IU", e)
	 Statlin "New Agreement"

AN0: EntryU 11, 10, 3, e, b, aIni:        IF e THEN EXIT SUB ELSE IF b GOTO AN0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO AN0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
AN1: SETINDEX 10, "Cus"
	 EntryU 13, 22, 6, e, b, aAgr:        IF e THEN EXIT SUB ELSE IF b GOTO AN0
	 SEEKEQ #10, aSto, aAgr
	 IF NOT EOF(10) THEN WinMssg "Duplicate agreement": GOTO AN1
	 x = 1
	 SETINDEX 10, "Ser"
	 CALL MhDisplayByName(0, 0, "IA", e)
AN2: EntryU 17 + x, 2, 10, e, b, aSer(x): IF e THEN EXIT SUB
	 IF b THEN x = x - 1: IF x < 1 GOTO AN1 ELSE GOTO AN2
	 IF aSer(x) > "!" THEN
		SEEKEQ #10, aSer(x)
		IF EOF(10) THEN WinMssg "Serial not found": GOTO AN2
		RETRIEVE #10, Inv
		InvDisp
		IF Inv.Sto <> aSto THEN WinMssg "Item store conflict"
		IF Inv.Sta <> 0 THEN WinMssg "Item is not idle"
	 END IF
	 x = x + 1: IF x > 4 THEN x = 4 ELSE GOTO AN2
	 IF Confirm(0, "SAVE New Agreement") = 0 GOTO AN2
	 FOR x = 1 TO 4
	 IF aSer(x) > "!" THEN
		SEEKEQ #10, aSer(x)
		RETRIEVE #10, Inv
		Inv.Sto = aSto
		Inv.Dta = aDat
		Inv.Act = aAct
		Inv.Ini = aIni
		Inv.Agr = aAgr
		Inv.Sta = 2
		Inv.Tim = Inv.Tim + 1
		UPDATE #10, Inv: InvDisp
	 END IF
	 NEXT
	 GOTO AN0
END SUB

SUB mAT ()
	 CALL MhDisplayByName(0, 0, "IU", e)
	 Statlin "Terminate Agreement"
AT0: EntryU 11, 10, 3, e, b, aIni: IF e THEN EXIT SUB ELSE IF b GOTO AT0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO AT0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 SETINDEX 10, "Cus"
AT1: EntryU 13, 22, 6, e, b, aAgr: IF e THEN EXIT SUB ELSE IF b GOTO AT0
	 SEEKEQ #10, aSto, aAgr
	 IF EOF(10) THEN WinMssg "Agreement not found": GOTO AT1
	 RETRIEVE #10, Inv
	 x = 0
	 ERASE aSer
	 CALL MhDisplayByName(0, 0, "IA", e)
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
	 DO WHILE aSto + aAgr = Inv.Sto + Inv.Agr
		x = x + 1
		aSer(x) = Inv.Ser
		qPrint Inv.Ser, 17 + x, 2, 0, 1
		qPrint Status(Inv.Sta), 17 + x, 13, 0, 1
		InvDisp
		SELECT CASE Inv.Sta
			   CASE 2: qPrint Status(0), 17 + x, 23, 0, 1
			   CASE 3: qPrint Status(0), 17 + x, 23, 0, 1
			   CASE 4: qPrint Status(1), 17 + x, 23, 0, 1
			   CASE 5: qPrint Status(0), 17 + x, 23, 0, 1
			   CASE 8: qPrint Status(0), 17 + x, 23, 0, 1
			   CASE ELSE: WinMssg "Check status"
					   qPrint Status(0), 17 + x, 23, 0, 1
		END SELECT
		MOVENEXT #10
		RETRIEVE #10, Inv
	 LOOP

	 IF Confirm(0, "TERMINATE Agreement") = 0 GOTO AT1
	 SETINDEX 10, "Ser"
	 FOR x = 1 TO 8
	 IF aSer(x) > "!" THEN
		SEEKEQ #10, aSer(x)
		RETRIEVE #10, Inv
		Inv.Sto = aSto
		Inv.Dta = aDat
		Inv.Agr = "      "
		Inv.Act = aAct
		Inv.Ini = aIni
		IF Inv.Sta = 4 THEN Inv.Sta = 1 ELSE Inv.Sta = 0
		UPDATE #10, Inv: InvDisp
	 END IF
	 NEXT
	 GOTO AT0
END SUB

SUB mAV ()
	 CALL MhDisplayByName(0, 0, "IU", e)
	 Statlin "Void Agreement"
AV0: EntryU 11, 10, 3, e, b, aIni: IF e THEN EXIT SUB ELSE IF b GOTO AV0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO AV0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 SETINDEX 10, "Cus"
AV1: EntryU 13, 22, 6, e, b, aAgr: IF e THEN EXIT SUB ELSE IF b GOTO AV0
	 SEEKEQ #10, aSto, aAgr
	 IF EOF(10) THEN WinMssg "Agreement not found": GOTO AV1
	 RETRIEVE #10, Inv
	 x = 0
	 ERASE aSer
	 CALL MhDisplayByName(0, 0, "IA", e)
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
	 DO WHILE aSto + aAgr = Inv.Sto + Inv.Agr
		x = x + 1
		aSer(x) = Inv.Ser
		qPrint Inv.Ser, 17 + x, 2, 0, 1
		qPrint Status(Inv.Sta), 17 + x, 13, 0, 1
		InvDisp
		qPrint Status(0), 17 + x, 23, 0, 1
		IF Inv.Sta <> 2 THEN WinMssg "Item is not ON-RENT"
		MOVENEXT #10
		RETRIEVE #10, Inv
	 LOOP
	 
	 IF Confirm(0, "VOID Agreement") = 0 GOTO AV1
	 SETINDEX 10, "Ser"
	 FOR x = 1 TO 8
	 IF aSer(x) > "!" THEN
		SEEKEQ #10, aSer(x)
		RETRIEVE #10, Inv
		Inv.Sto = aSto
		Inv.Dta = aDat
		Inv.Act = aAct
		Inv.Ini = aIni
		Inv.Agr = "      "
		Inv.Sta = 0
		Inv.Tim = Inv.Tim - 1
		UPDATE #10, Inv: InvDisp
	 END IF
	 NEXT
	 GOTO AV0
END SUB

SUB mMasCus ()
	CALL MhDisplayByName(0, 0, "Mast-Cus", e)
	SubMenu 1, ""

'   CREATEINDEX #11, "Num", 1, "Sto", "Num"
'   CREATEINDEX #11, "Nam", 0, "Nam"

	SETINDEX 11, "Nam"
	LOCATE 50, 59: PRINT "Index in use: "; GETINDEX$(11); LOF(11);
	MOVEFIRST #11
	GOSUB CusDn
	MOVEFIRST #11
	r = 3: m = 1

300 RETRIEVE #11, Cus
	COLOR 17, 15: LOCATE r, 1: PRINT USING "&³&"; Cus.Sto + Cus.Num; Cus.Nam;
	KeyPres n$
	COLOR 1, 7: LOCATE r, 1: PRINT USING "&³&"; Cus.Sto + Cus.Num; Cus.Nam;
	
	IF n$ = CHR$(13) THEN n$ = LTRIM$(STR$(m))
	SELECT CASE n$
	CASE CHR$(27) 'Esc
		EXIT SUB
	CASE CHR$(0) + "M" '==========right==========
	CASE CHR$(0) + "K" '==========left===========
	CASE CHR$(0) + "P" '==========down===========
		MOVENEXT #11
		IF EOF(11) THEN
		   MOVELAST #11
		ELSE
		   IF r + 1 > 47 THEN CALL Scroll(1, 3, 33, 47, 1) ELSE r = r + 1
		END IF
	CASE CHR$(0) + "H" '==========Up=============
		MOVEPREVIOUS #11
		IF BOF(11) THEN
		   MOVEFIRST #11
		ELSE
		   IF r - 1 < 3 THEN CALL BkScroll(1, 3, 33, 47, 1) ELSE r = r - 1
		END IF
	CASE CHR$(0) + "I" '==========PgUp===========
		FOR x = r TO -41 STEP -1
			MOVEPREVIOUS #11
			IF BOF(11) THEN EXIT FOR
		NEXT
		GOSUB CusDn
		SEEKEQ #11, m$
	CASE CHR$(0) + "Q" '==========PgDn===========
		FOR x = r TO 91
			MOVENEXT #11
			IF EOF(11) THEN EXIT FOR
		NEXT
		GOSUB CusUp
		SEEKEQ #11, m$
	CASE CHR$(0) + "G" '==========Home===========
		MOVEFIRST #11
		GOSUB CusDn
		SEEKEQ #11, m$
	CASE CHR$(0) + "O" '==========End============
		MOVELAST #11
		GOSUB CusUp
		SEEKEQ #11, m$
	CASE "D", "1"      '==========Display========
		CusShow
	CASE "S", "2"      '==========Search=========
		EntryU 12, 54, 26, e, s, Cus.Nam: IF e THEN 300
		SEEKGE #11, Cus.Nam
		RETRIEVE #11, Cus
		s$ = Cus.Nam
		FOR x = r TO 46
			MOVENEXT #11
			IF EOF(11) THEN EXIT FOR
		NEXT
		GOSUB CusUp
		SEEKEQ #11, s$
	CASE "C", "6"
		InvShow
	CASE "M", "7"
		CusShow
		CusEdit
	CASE ELSE
	END SELECT
	GOTO 300
CusDn:
	LOCATE 3: COLOR 0, 7
	IF BOF(11) THEN MOVEFIRST #11
	WHILE CSRLIN < 48
		RETRIEVE #11, Cus
		IF CSRLIN = r THEN m$ = Cus.Nam
		LOCATE , 1: PRINT USING "&³&"; Cus.Sto + Cus.Num; Cus.Nam;
		LOCATE CSRLIN + 1
		MOVENEXT #11
	WEND
	RETURN
CusUp:
	LOCATE 47: COLOR 0, 7
	IF EOF(11) THEN MOVELAST #11
	WHILE CSRLIN > 2
		RETRIEVE #11, Cus
		IF CSRLIN = r THEN m$ = Cus.Nam
		IF s$ = Cus.Nam THEN r = CSRLIN
		LOCATE , 1: PRINT USING "&³&"; Cus.Sto + Cus.Num; Cus.Nam;
		LOCATE CSRLIN - 1
		MOVEPREVIOUS #11
	WEND
	RETURN
END SUB

SUB mMasInv ()
	CALL MhDisplayByName(0, 0, "Mast-Inv", e)
	SubMenu 1, ""
'   CREATEINDEX #10, "Sto", 1, "Sto", "Typ", "Grp", "Cod", "Mdl", "Ser"
'   CREATEINDEX #10, "Ser", 1, "Ser"
'   CREATEINDEX #10, "Cod", 0, "Cod"
'   CREATEINDEX #10, "Agr", 1, "Sto", "Agr", "Typ", "Grp", "Cod", "Ser"
'   CREATEINDEX #10, "Cus", 0, "Sto", "Agr"
'   CREATEINDEX #10, "Cst", 0, "Grp", "Cod", "Dis", "Mdl", "Prc"
	SETINDEX 10, "Sto"
	i = 1: o = 0
	i$ = "Sto": o$ = "Ser"
	qPrint "Index: " + GETINDEX$(10) + STR$(LOF(10)), 50, 64, 0, 1
	MOVEFIRST #10
	GOSUB DispDn
	MOVEFIRST #10
	r = 3

200 RETRIEVE #10, Inv
	COLOR 15, 1: LOCATE r, 2: PRINT USING "&³&³&³&³&"; Inv.Sto; Inv.Typ + Inv.Grp; Inv.Cod; Inv.Mdl; Inv.Ser;
	KeyPres n$
	IF n$ = CHR$(13) THEN SubMenu 0, n$
	IF n$ = CHR$(0) + "P" OR n$ = CHR$(0) + "H" THEN
	   COLOR 16 + Inv.Sta, 7
	   LOCATE , 2:  PRINT Inv.Sto;
	   LOCATE , 5:  PRINT Inv.Typ + Inv.Grp;
	   LOCATE , 9:  PRINT Inv.Cod;
	   LOCATE , 14: PRINT Inv.Mdl;
	   LOCATE , 25: PRINT Inv.Ser;
	END IF
	SELECT CASE n$
	CASE CHR$(27) 'Esc '==========Escape========
		EXIT SUB
	CASE CHR$(0) + "M" '==========Right=========
		SubMenu 0, n$
	CASE CHR$(0) + "K" '==========Left==========
		SubMenu 0, n$
	CASE CHR$(0) + "P" '==========Down==========
		MOVENEXT #10
		IF EOF(10) THEN
		   MOVELAST #10
		ELSE
		   IF r + 1 > 47 THEN CALL Scroll(2, 3, 34, 47, 1) ELSE r = r + 1
		END IF
	CASE CHR$(0) + "H" '==========Up=============
		MOVEPREVIOUS #10
		IF BOF(10) THEN
		   MOVEFIRST #10
		ELSE
		   IF r - 1 < 3 THEN CALL BkScroll(2, 3, 34, 47, 1) ELSE r = r - 1
		END IF
	CASE CHR$(0) + "I" '==========PgUp===========
		FOR x = r TO -43 STEP -1
			MOVEPREVIOUS #10
			IF BOF(10) THEN
			   MOVEFIRST #10
			   GOSUB DispDn
			   IF i THEN
				  SEEKEQ #10, r$, t$, g$, c$, m$, s$
			   ELSE
				  SEEKEQ #10, r$
			   END IF
			   GOTO 200
			END IF
			IF x = 3 THEN
			   RETRIEVE #10, Inv
			   IF i THEN
				  r$ = Inv.Sto: t$ = Inv.Typ: g$ = Inv.Grp: c$ = Inv.Cod: m$ = Inv.Mdl: s$ = Inv.Ser
			   ELSE
				  r$ = Inv.Ser
			   END IF
			END IF
		NEXT
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
		GOSUB DispUp
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
	CASE CHR$(0) + "Q" '==========PgDn===========
		FOR x = r TO 92
			MOVENEXT #10
			IF EOF(10) THEN
			   MOVELAST #10
			   GOSUB DispUp
			   IF i THEN
				  SEEKEQ #10, r$, t$, g$, c$, m$, s$
			   ELSE
				  SEEKEQ #10, r$
			   END IF
			   GOTO 200
			END IF
			IF x = 47 THEN
			   RETRIEVE #10, Inv
			   IF i THEN
				  r$ = Inv.Sto: t$ = Inv.Typ: g$ = Inv.Grp: c$ = Inv.Cod: m$ = Inv.Mdl: s$ = Inv.Ser
			   ELSE
				  r$ = Inv.Ser
			   END IF
			END IF
		NEXT
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
		GOSUB DispDn
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
	CASE CHR$(0) + "G" '==========Home===========
		MOVEFIRST #10
		GOSUB DispDn
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
	CASE CHR$(0) + "O" '==========End============
		MOVELAST #10
		GOSUB DispUp
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
	CASE "D"           '==========Display========
		SubMenu 0, n$
		InvShow
	CASE "I"           '==========Index==========
		SubMenu 0, n$
		SWAP i$, o$
		SWAP i, o
		SETINDEX 10, i$
		qPrint "Index: " + GETINDEX$(10) + STR$(LOF(10)), 50, 64, 0, 1
		MOVEFIRST #10
		GOSUB DispDn
		IF i THEN
		   SEEKEQ #10, r$, t$, g$, c$, m$, s$
		ELSE
		   SEEKEQ #10, r$
		END IF
	CASE "R"           '==========Rates==========
		SubMenu 0, n$
		InvShow
		GET #4, VAL(Inv.Rat), Rat
		WinRate
	CASE "F"           '==========Find===========
		SubMenu 0, n$
		IF GETINDEX$(10) = "Sto" THEN WinMssg "Must switch index": GOTO 200
		f$ = Inv.Ser
226     EntryU 9, 54, 10, e, b, Inv.Ser: IF e THEN 200 ELSE IF b THEN 226
		s$ = RTRIM$(Inv.Ser)
		qPrint "Seeking", 50, 50, 0, 1
		MOVEFIRST #10
		WHILE NOT EOF(10) AND INKEY$ <> CHR$(27)
		   RETRIEVE #10, Inv
		   f = INSTR(Inv.Ser, s$)
		   IF f THEN
			  f$ = Inv.Ser: e = 0
			  FOR x = 4 TO r
				 MOVEPREVIOUS #10
				 IF BOF(10) THEN MOVEFIRST #10: GOSUB DispDn: e = 1: EXIT FOR
			  NEXT
			  IF e = 0 THEN
				 SEEKEQ #10, f$
				 FOR x = r TO 46
					MOVENEXT #10
					IF EOF(10) THEN MOVELAST #10: GOSUB DispUp: e = 1: EXIT FOR
				 NEXT
			  END IF
			  IF e = 0 THEN
				 SEEKEQ #10, f$
				 FOR x = 4 TO r: MOVEPREVIOUS #10: NEXT
				 GOSUB DispDn
			  END IF
			  r = x
			  SEEKEQ #10, f$
			  InvShow
			  COLOR 15, 1: LOCATE r, 2: PRINT USING "&³&³&³&³&"; Inv.Sto; Inv.Typ + Inv.Grp; Inv.Cod; Inv.Mdl; Inv.Ser;
			  COLOR 14, 1: LOCATE r, 24 + f: PRINT s$;
			  qPrint "       ", 50, 50, 0, 1
			  m$ = "Find next?": WinMssg m$: IF m$ <> "Y" THEN 200
			  qPrint "Seeking", 50, 50, 0, 1
		   END IF
		   MOVENEXT #10
		WEND
		qPrint "       ", 50, 50, 0, 1
		WinMssg "Not found"
		SEEKEQ #10, f$
	CASE "O"           '==========Override=======
		SubMenu 0, n$
		InvShow
		IF Inv.Sta <> 0 THEN WinMssg "Item is not idle"
227     EntryN 33, 54, 3, 0, e, b, Inv.Rat: IF e THEN 200 ELSE IF b THEN 227
		IF Inv.Rat < "  1" THEN WinMssg "Rate must not be zero": GOTO 227
		IF Confirm(1, "Override Rate Code") = 0 THEN 227
		IF Cod.Rat = Inv.Rat THEN Inv.Ovr = " " ELSE Inv.Ovr = "O"
		UPDATE #10, Inv
	CASE "S"           '==========Serial=========
		SubMenu 0, n$
		IF GETINDEX$(10) = "Sto" THEN WinMssg "Must switch index": GOTO 200
		f$ = Inv.Ser
228     EntryU 9, 54, 10, e, b, Inv.Ser: IF e THEN 200 ELSE IF b THEN 228
		SEEKEQ #10, Inv.Ser
		IF EOF(10) THEN
		   WinMssg "Not found"
		   SEEKEQ #10, f$
		   InvShow
		   GOTO 200
		END IF
		f$ = Inv.Ser
		FOR x = 3 TO r
		   MOVEPREVIOUS #10
		   IF BOF(10) THEN
			  MOVEFIRST #10
			  GOSUB DispDn
			  SEEKEQ #10, f$
			  r = x
			  InvShow
			  GOTO 200
		   END IF
		NEXT
		SEEKEQ #10, f$
		FOR x = r TO 46
		   MOVENEXT #10
		   IF EOF(10) THEN
			  MOVELAST #10
			  GOSUB DispUp
			  SEEKEQ #10, f$
			  r = x
			  InvShow
			  GOTO 200
		   END IF
		NEXT
		GOSUB DispUp
		SEEKEQ #10, f$
		r = x
		InvShow
	CASE "L"           '==========Store==========
		SubMenu 0, n$
		IF GETINDEX$(10) = "Ser" THEN WinMssg "Must switch index": GOTO 200
229     EntryU 4, 39, 2, e, b, Inv.Sto: IF e THEN 200 ELSE IF b THEN 229
		SEEKGE #10, Inv.Sto
		IF EOF(10) THEN WinMssg "Not found"
		RETRIEVE #10, Inv
		r$ = Inv.Sto: t$ = Inv.Typ: g$ = Inv.Grp: c$ = Inv.Cod: m$ = Inv.Mdl: s$ = Inv.Ser
		FOR x = 3 TO 47
			MOVENEXT #10
			IF EOF(10) THEN
			   MOVELAST #10
			   GOSUB DispUp
			   SEEKEQ #10, r$, t$, g$, c$, m$, s$
			   GOTO 200
			END IF
		NEXT
		SEEKEQ #10, r$, t$, g$, c$, m$, s$
		GOSUB DispDn
		SEEKEQ #10, r$, t$, g$, c$, m$, s$
	CASE "E"           '==========Erase==========
		SubMenu 0, n$
		InvShow
		IF Confirm(1, "ERASE Item") = 0 THEN 200
		DELETE #10
		qPrint "Index: " + GETINDEX$(10) + STR$(LOF(10)), 50, 64, 0, 1
	CASE "M"           '==========Modify=========
		SubMenu 0, n$
		InvShow
		InvEdit
	CASE ELSE
	END SELECT
	GOTO 200

DispDn:
	LOCATE 3
	WHILE CSRLIN < 48
		RETRIEVE #10, Inv
		IF Inv.Ser = f$ THEN x = CSRLIN
		IF CSRLIN = r THEN
		   IF i THEN
			  r$ = Inv.Sto: t$ = Inv.Typ: g$ = Inv.Grp: c$ = Inv.Cod: m$ = Inv.Mdl: s$ = Inv.Ser
		   ELSE
			  r$ = Inv.Ser
		   END IF
		END IF
		COLOR 16 + Inv.Sta, 7
		LOCATE , 2:  PRINT Inv.Sto;
		LOCATE , 5:  PRINT Inv.Typ + Inv.Grp;
		LOCATE , 9:  PRINT Inv.Cod;
		LOCATE , 14: PRINT Inv.Mdl;
		LOCATE , 25: PRINT Inv.Ser;
		LOCATE CSRLIN + 1
		MOVENEXT #10
	WEND
	RETURN
DispUp:
	LOCATE 47
	WHILE CSRLIN > 2
		RETRIEVE #10, Inv
		IF Inv.Ser = f$ THEN x = CSRLIN
		IF CSRLIN = r THEN
		   IF i THEN
			  r$ = Inv.Sto: t$ = Inv.Typ: g$ = Inv.Grp: c$ = Inv.Cod: m$ = Inv.Mdl: s$ = Inv.Ser
		   ELSE
			  r$ = Inv.Ser
		   END IF
		END IF
		COLOR 16 + Inv.Sta, 7
		LOCATE , 2:  PRINT Inv.Sto;
		LOCATE , 5:  PRINT Inv.Typ + Inv.Grp;
		LOCATE , 9:  PRINT Inv.Cod;
		LOCATE , 14: PRINT Inv.Mdl;
		LOCATE , 25: PRINT Inv.Ser;
		LOCATE CSRLIN - 1
		MOVEPREVIOUS #10
	WEND
	RETURN
END SUB

SUB mMC ()
	 CALL MhDisplayByName(0, 0, aAct, e)
	 Statlin "Rate Override"
	 ON LOCAL ERROR GOTO MC9
MC0: EntryU 11, 10, 3, e, b, aIni:     IF e THEN EXIT SUB ELSE IF b GOTO MC0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO MC0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
MC1: EntryU 13, 19, 10, e, b, Inv.Ser: IF e THEN EXIT SUB ELSE IF b GOTO MC0
	 SEEKEQ #10, Inv.Ser
	 RETRIEVE #10, Inv
	 InvDisp
	 IF Inv.Sto <> aSto THEN WinMssg "Store location conflict"
MC2: EntryU 14, 19, 3, e, b, Inv.Rat:  IF e THEN EXIT SUB ELSE IF b GOTO MC1
	 GET #4, VAL(Inv.Rat), Rat
	 WinRate
	 Inv.Sto = aSto
	 Inv.Dta = aDat
	 Inv.Act = aAct
	 Inv.Ini = aIni
	 Inv.Rat = Rat.Num
	 Inv.Ovr = "O"
	 IF Confirm(0, "Update Item's rate") = 0 GOTO MC2
	 UPDATE #10, Inv: InvDisp
	 EXIT SUB
MC9: IF ERR = 85 THEN WinMssg "Serial number not found": RESUME MC1
END SUB

SUB mMG ()
	 CALL MhDisplayByName(0, 0, aAct, e)
	 Statlin "Charge-Off Giveaway"
	 ON LOCAL ERROR GOTO MG9
MG0: EntryU 11, 10, 3, e, b, aIni:     IF e THEN EXIT SUB ELSE IF b GOTO MG0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO MG0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
MG1: EntryU 13, 19, 10, e, b, Inv.Ser: IF e THEN EXIT SUB ELSE IF b GOTO MG0
	 SEEKEQ #10, Inv.Ser
	 RETRIEVE #10, Inv
	 InvDisp
	 IF Inv.Sto <> aSto THEN WinMssg "Store location conflict"
	 IF Inv.Sta <> 0 THEN WinMssg "Item must be idle"
	 Inv.Sta = 14
	 Inv.Sto = aSto
	 Inv.Dta = aDat
	 Inv.Act = aAct
	 Inv.Ini = aIni
	 IF Confirm(0, "Update item's status") = 0 GOTO MG1
	 UPDATE #10, Inv: InvDisp
	 EXIT SUB
MG9: IF ERR = 85 THEN WinMssg "Serial number not found": RESUME MG1
END SUB

SUB mMI ()
	 CALL MhDisplayByName(0, 0, aAct, e)
	 Statlin "Receive from Service"
	 ON LOCAL ERROR GOTO MI9
MI0: EntryU 11, 10, 3, e, b, aIni:     IF e THEN EXIT SUB ELSE IF b GOTO MI0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO MI0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
MI1: EntryU 13, 19, 10, e, b, Inv.Ser: IF e THEN EXIT SUB ELSE IF b GOTO MI0
	 SEEKEQ #10, Inv.Ser
	 RETRIEVE #10, Inv
	 InvDisp
	 IF Inv.Sto <> aSto THEN WinMssg "Store location conflict"
	 SELECT CASE Inv.Sta
			CASE 1: s$ = "00"
			CASE 4: s$ = "05"
			CASE ELSE: WinMssg "Not InService or OnRent/IS"
	 END SELECT
	 qPrint Status(Inv.Sta), 15, 23, 0, 1
MI2: EntryU 15, 19, 2, e, b, s$: IF e THEN EXIT SUB ELSE IF b GOTO MI1
	 qPrint Status(VAL(s$)), 15, 23, 0, 1
	 Inv.Sta = VAL(s$)
	 Inv.Ctr = "  "
	 Inv.Sto = aSto
	 Inv.Dta = aDat
	 Inv.Act = aAct
	 Inv.Ini = aIni
	 IF Confirm(0, "Update item's status") = 0 GOTO MI2
	 UPDATE #10, Inv: InvDisp
	 EXIT SUB
MI9: IF ERR = 85 THEN WinMssg "Serial number not found": RESUME MI1
END SUB

SUB mMM ()
	 CALL MhDisplayByName(0, 0, aAct, e)
	 Statlin "Manual Transfer IN"
	 ON LOCAL ERROR GOTO MM9
MM0: EntryU 11, 10, 3, e, b, aIni:     IF e THEN EXIT SUB ELSE IF b GOTO MM0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO MM0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
MM1: EntryU 13, 19, 10, e, b, Inv.Ser: IF e THEN EXIT SUB ELSE IF b GOTO MM0
	 SEEKEQ #10, Inv.Ser
	 RETRIEVE #10, Inv
	 InvDisp
	 IF Inv.Sto = aSto THEN WinMssg "Item is already in store"
	 IF Inv.Sta <> 15 THEN WinMssg "Item is not a tansfer"
	 IF Inv.Tto <> aSto THEN WinMssg "Not the receiving store"
	 IF Inv.Tfr = aSto THEN WinMssg "Transfer from same store"
	 Inv.Tfr = Inv.Sto
MM2: EntryU 15, 31, 2, e, b, Inv.Tfr: IF e THEN EXIT SUB ELSE IF b GOTO MM1
	 GET #6, VAL(Inv.Tfr), Sto
	 qPrint Sto.Num, 15, 31, 0, 1
	 qPrint Sto.Nam, 17, 3, 0, 1
	 Inv.Sto = aSto
	 Inv.Dta = aDat
	 Inv.Act = aAct
	 Inv.Ini = aIni
	 Inv.Del = " "
	 Inv.Sta = 0
	 Inv.Tfr = Sto.Num
	 Inv.Tto = "  "
	 Inv.Agr = "      "
	 IF Confirm(0, "Update item's Transfer") = 0 GOTO MM2
	 UPDATE #10, Inv: InvDisp
	 EXIT SUB
MM9: IF ERR = 85 THEN WinMssg "Serial number not found": RESUME MM1
END SUB

SUB mMN ()
	 CALL MhDisplayByName(0, 0, aAct, e)
	 Statlin "New Merchandise"
	 ON LOCAL ERROR GOTO MN9
MN0: EntryU 11, 10, 3, e, b, aIni:  IF e THEN EXIT SUB ELSE IF b GOTO MN0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO MN0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
MN1: EntryU 13, 19, 10, e, b, Inv.Ser: IF e THEN EXIT SUB ELSE IF b GOTO MN0
MN2: EntryU 14, 19, 4, e, b, Inv.Cod:  IF e THEN EXIT SUB ELSE IF b GOTO MN1
MN3: EntryU 15, 19, 10, e, b, Inv.Mdl: IF e THEN EXIT SUB ELSE IF b GOTO MN2
MN4: EntryU 16, 19, 10, e, b, Inv.Dis: IF e THEN EXIT SUB ELSE IF b GOTO MN3
	 IF FindPrc(Inv.Dis + Inv.Mdl + Inv.Cod) = 0 THEN
		IF WinPric THEN
		   WinMssg "Price code not found"
		   GOTO MN2
		ELSE
		   qPrint Prc.Cod, 14, 19, 0, 1: Inv.Cod = Prc.Cod
		   qPrint Prc.Mdl, 15, 19, 0, 1: Inv.Mdl = Prc.Mdl
		   qPrint Prc.Dis, 16, 19, 0, 1: Inv.Dis = Prc.Dis
		   IF FindCod(Inv.Cod) = 0 THEN WinMssg "Code not found": GOTO MN2
		END IF
	 END IF
MN5: EntryU 17, 19, 3, e, b, Inv.Bra:  IF e THEN EXIT SUB ELSE IF b GOTO MN4
MN6: EntryD 18, 19, e, b, Inv.Dtp:     IF e THEN EXIT SUB ELSE IF b GOTO MN5
MN7: EntryU 19, 19, 10, e, b, Inv.Pon: IF e THEN EXIT SUB ELSE IF b GOTO MN6
	 Inv.Sto = aSto
	 Inv.Ini = aIni
	 Inv.Act = aAct
	 Inv.Dta = aDat
	 Inv.Typ = Cod.Typ
	 Inv.Grp = Cod.Grp
	 Inv.Sta = 0
	 Inv.Tim = 0
	 Inv.Ctr = "  "
	 Inv.Dtr = Inv.Dta
	 Inv.Prc = Prc.New
	 Inv.Agr = "      "
	 Inv.Del = " "
	 Inv.Tfr = "  "
	 Inv.Tto = "  "
	 Inv.Par = Prc.Par
	 Inv.Lab = Prc.Lab
	 Inv.Rat = Cod.Rat
	 Inv.Ovr = " "

	 InvDisp
	 IF Confirm(0, "Save Item") = 0 GOTO MN7
	 INSERT #10, Inv: InvDisp
	 EXIT SUB
MN9: IF ERR = 86 THEN WinMssg "Duplicate serial number": RESUME MN1
END SUB

SUB mMO ()
	 CALL MhDisplayByName(0, 0, aAct, e)
	 Statlin "Send to Service"
	 ON LOCAL ERROR GOTO MO9
MO0: EntryU 11, 10, 3, e, b, aIni:     IF e THEN EXIT SUB ELSE IF b GOTO MO0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO MO0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
MO1: EntryU 13, 19, 10, e, b, Inv.Ser: IF e THEN EXIT SUB ELSE IF b GOTO MO0
	 SEEKEQ #10, Inv.Ser
	 RETRIEVE #10, Inv
	 InvDisp
	 IF Inv.Sto <> aSto THEN WinMssg "Store location conflict"
	 SELECT CASE Inv.Sta
			CASE 0: s$ = "01"
			CASE 3: s$ = "04"
			CASE ELSE: WinMssg "Not Idel or OnRent/AS"
	 END SELECT
	 qPrint Status(Inv.Sta), 15, 23, 0, 1
MO2: EntryU 15, 19, 2, e, b, s$: IF e THEN EXIT SUB ELSE IF b GOTO MO1
	 qPrint Status(VAL(s$)), 15, 23, 0, 1
MO3: EntryU 17, 19, 2, e, b, Inv.Ctr: IF e THEN EXIT SUB ELSE IF b GOTO MO1
	 IF Inv.Ctr > " 0" THEN GET #7, VAL(Inv.Ctr), Ctr ELSE GOTO MO2
	 qPrint Ctr.Nam, 19, 3, 0, 1
	 Inv.Sta = VAL(s$)
	 Inv.Ctr = Ctr.Num
	 Inv.Sto = aSto
	 Inv.Dta = aDat
	 Inv.Act = aAct
	 Inv.Ini = aIni
	 IF Confirm(0, "Update item's status") = 0 GOTO MO3
	 UPDATE #10, Inv: InvDisp
	 EXIT SUB
MO9: IF ERR = 85 THEN WinMssg "Serial number not found": RESUME MO1
END SUB

SUB mMP ()
	 CALL MhDisplayByName(0, 0, aAct, e)
	 Statlin "Charge-Off Scrap"
	 ON LOCAL ERROR GOTO MP9
MP0: EntryU 11, 10, 3, e, b, aIni:     IF e THEN EXIT SUB ELSE IF b GOTO MP0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO MP0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
MP1: EntryU 13, 19, 10, e, b, Inv.Ser: IF e THEN EXIT SUB ELSE IF b GOTO MP0
	 SEEKEQ #10, Inv.Ser
	 RETRIEVE #10, Inv
	 InvDisp
	 IF Inv.Sto <> aSto THEN WinMssg "Store location conflict"
	 IF Inv.Sta <> 0 THEN WinMssg "Item is not idle"
	 Inv.Sta = 13
	 Inv.Sto = aSto
	 Inv.Dta = aDat
	 Inv.Act = aAct
	 Inv.Ini = aIni
	 IF Confirm(0, "Update item's status") = 0 GOTO MP1
	 UPDATE #10, Inv: InvDisp
	 EXIT SUB
MP9: IF ERR = 85 THEN WinMssg "Serial number not found": RESUME MP1
END SUB

SUB mMR ()
	 CALL MhDisplayByName(0, 0, aAct, e)
	 Statlin "Trnasfer In"
	 ON LOCAL ERROR GOTO MR9
MR0: EntryU 11, 10, 3, e, b, aIni:     IF e THEN EXIT SUB ELSE IF b GOTO MR0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO MR0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
MR1: EntryU 13, 19, 10, e, b, Inv.Ser: IF e THEN EXIT SUB ELSE IF b GOTO MR0
	 SEEKEQ #10, Inv.Ser
	 RETRIEVE #10, Inv
	 InvDisp
	 IF Inv.Sto = aSto THEN WinMssg "Item is already in store"
	 IF Inv.Sta <> 15 THEN WinMssg "Item is not a tansfer"
	 IF Inv.Tto <> aSto THEN WinMssg "Not the receiving store"
	 IF Inv.Tfr = aSto THEN WinMssg "Transfer from same store"
	 Inv.Tfr = Inv.Sto
MR2: EntryU 15, 28, 2, e, b, Inv.Tfr: IF e THEN EXIT SUB ELSE IF b GOTO MR0
	 IF Inv.Tfr > " 0" THEN GET #6, VAL(Inv.Tfr), Sto ELSE GOTO MR2
	 qPrint Sto.Num, 15, 28, 0, 1
	 qPrint Sto.Nam, 17, 3, 0, 1
	 Inv.Sto = aSto
	 Inv.Dta = aDat
	 Inv.Act = aAct
	 Inv.Ini = aIni
	 Inv.Del = " "
	 Inv.Sta = 0
	 Inv.Tfr = Sto.Num
	 Inv.Tto = "  "
	 Inv.Agr = "      "
	 IF Confirm(0, "Update item's Transfer") = 0 GOTO MR2
	 UPDATE #10, Inv: InvDisp
	 EXIT SUB
MR9: IF ERR = 85 THEN WinMssg "Serial number not found": RESUME MR1
END SUB

SUB mMT ()
	 CALL MhDisplayByName(0, 0, aAct, e)
	 Statlin "Trnasfer Out"
	 ON LOCAL ERROR GOTO MT9
MT0: EntryU 11, 10, 3, e, b, aIni:     IF e THEN EXIT SUB ELSE IF b GOTO MT0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO MT0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-View", e)
MT1: EntryU 13, 19, 10, e, b, Inv.Ser: IF e THEN EXIT SUB ELSE IF b GOTO MT0
	 SEEKEQ #10, Inv.Ser
	 RETRIEVE #10, Inv
	 InvDisp
	 IF Inv.Sto <> aSto THEN WinMssg "Item store conflict"
	 IF Inv.Sta <> 0 THEN WinMssg "Item is not idle"
MT2: EntryU 15, 28, 2, e, b, Inv.Tto: IF e THEN EXIT SUB ELSE IF b GOTO MT1
	 IF Inv.Tto > " 0" THEN GET #6, VAL(Inv.Tto), Sto ELSE GOTO MT2
	 Inv.Tto = Sto.Num
	 IF Inv.Tto = aSto THEN WinMssg "Transfer to same store"
	 qPrint Sto.Num, 15, 28, 0, 1
	 qPrint Sto.Nam, 17, 3, 0, 1
	 Inv.Sto = aSto
	 Inv.Dta = aDat
	 Inv.Act = aAct
	 Inv.Ini = aIni
	 Inv.Del = "T"
	 Inv.Sta = 15
	 Inv.Tto = Sto.Num
	 Inv.Agr = "      "
	 IF Confirm(0, "Update item's Transfer") = 0 GOTO MT2
	 UPDATE #10, Inv: InvDisp
	 EXIT SUB
MT9: IF ERR = 85 THEN WinMssg "Serial number not found": RESUME MT1
END SUB

SUB mMU ()
	 CALL MhDisplayByName(0, 0, aAct, e)
	 Statlin "Modify Merchandise"
	 ON LOCAL ERROR GOTO MU9
MU0: EntryU 11, 10, 3, e, b, aIni:     IF e THEN EXIT SUB ELSE IF b GOTO MU0
	 IF FindIni(aIni) = 0 THEN WinMssg "User not found": GOTO MU0
	 qPrint Pas.Nam, 11, 15, 0, 1
	 CALL MhDisplayByName(0, 0, "Inv-Edit", e)
MU1: EntryU 13, 19, 10, e, b, Inv.Ser: IF e THEN EXIT SUB ELSE IF b GOTO MU0
	 SEEKEQ #10, Inv.Ser
	 InvShow
	 IF Inv.Sto <> aSto THEN WinMssg "Item store conflict"
	 
	 Inv.Dta = aDat
	 Inv.Act = aAct
	 Inv.Ini = aIni
	 InvEdit
	 GOTO MU1
MU9: IF ERR = 85 THEN WinMssg "Serial number not found": RESUME MU1
END SUB

SUB mTA ()
	CALL MhDisplayByName(0, 0, "Header", e)
TA1: EntryD 5, 4, e, b, aDat:     IF e THEN EXIT SUB ELSE IF b GOTO TA1
TA2: EntryS 5, 22, 2, e, b, aSto: IF e THEN EXIT SUB ELSE IF b GOTO TA1
	 IF aSto > "99" OR aSto < "01" THEN
		WinMssg "Store not found"
		GOTO TA2
	 ELSE
		GET #6, VAL(aSto), Sto
		qPrint Sto.Num, 5, 22, 0, 1: aSto = Sto.Num
		qPrint Sto.Nam, 5, 30, 0, 1
		qPrint Sto.Cty, 5, 60, 0, 1
		qPrint Sto.Sta, 5, 77, 0, 1
	 END IF
	 CALL MhDisplayByName(0, 0, "Activity", e)
TA3: EntryU 8, 12, 2, e, b, aAct: IF e THEN EXIT SUB ELSE IF b GOTO TA2
	 qPrint TraStat(aAct), 8, 17, 0, 1
	 SELECT CASE aAct
			CASE "AN": CALL mAN 'New Agreement
			CASE "AV": CALL mAV 'Void Agreement
			CASE "AT": CALL mAT 'Terminate Agreement
			CASE "AC": CALL mAC 'Charge-off Agreement
			CASE "AA": CALL mAA 'Paid-Out Agreement
	 END SELECT
	 GOTO TA3
END SUB

SUB mTM ()
	CALL MhDisplayByName(0, 0, "Header", e)
	SETINDEX 10, "Ser"
TM1: EntryD 5, 4, e, b, aDat:     IF e THEN EXIT SUB ELSE IF b GOTO TM1
TM2: EntryS 5, 22, 2, e, b, aSto: IF e THEN EXIT SUB ELSE IF b GOTO TM1
	 IF aSto > "99" OR aSto < "01" THEN
		WinMssg "Store not found"
		GOTO TM2
	 ELSE
		GET #6, VAL(aSto), Sto
		qPrint Sto.Num, 5, 22, 0, 1: aSto = Sto.Num
		qPrint Sto.Nam, 5, 30, 0, 1
		qPrint Sto.Cty, 5, 60, 0, 1
		qPrint Sto.Sta, 5, 77, 0, 1
	 END IF
	 CALL MhDisplayByName(0, 0, "Activity", e)
TM3: EntryU 8, 12, 2, e, b, aAct: IF e THEN EXIT SUB ELSE IF b GOTO TM2
	 qPrint TraStat(aAct), 8, 17, 0, 1
	 SELECT CASE aAct
			CASE "MN": CALL mMN 'New Merchandise
			CASE "MC": CALL mMC 'Rate Override
			CASE "MG": CALL mMG 'Charge-Off GiveAway
			CASE "MP": CALL mMP 'Charge-Off Scrap
			CASE "MR": CALL mMR 'Transfer-In
			CASE "MT": CALL mMT 'Transfer-Out
			CASE "MM": CALL mMM 'Transfer-Manual
			CASE "MO": CALL mMO 'Service-Out
			CASE "MI": CALL mMI 'Service-In
			CASE "MU": CALL mMU 'Modify Item
	 END SELECT
	 GOTO TM3
END SUB

