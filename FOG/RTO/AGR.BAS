DEFINT A-Z: DEFLNG L
DECLARE FUNCTION InvItem (c, n)
DECLARE FUNCTION InvItmL (n)
DECLARE FUNCTION InvChng ()
DECLARE FUNCTION InvRtrv ()
DECLARE FUNCTION InvVrfy ()
DECLARE FUNCTION ChgLoan ()
DECLARE FUNCTION AgrRate ()
DECLARE FUNCTION Percent ()

DECLARE SUB AgrSort ()
DECLARE SUB AgrCust ()
DECLARE SUB AgrDisp ()
DECLARE SUB AgrPrnt ()
DECLARE SUB AgrProm ()
DECLARE SUB AgrInvD (n)
DECLARE SUB AgrLonD (n)
DECLARE SUB AgrInvN (n)
DECLARE SUB AgrDetl (u$)
DECLARE SUB AgrFreq (r, c)
DECLARE SUB AgrStat (r, c)

DECLARE SUB InvRest ()
DECLARE SUB InvCalc (r)
DECLARE SUB InvLine (n)
DECLARE SUB InvActv (s)
DECLARE SUB InsActv (s, n)

DECLARE SUB RctLoan ()

DECLARE SUB RatDisp (z)
DECLARE SUB PyoAmnt (l)
DECLARE SUB PyoDate (f$)
DECLARE SUB CompNam (f$)
DECLARE SUB CustAgr (f$, f, e)

REM $INCLUDE: '..\Include\Config.Dec'

DEFSNG A-Z
SUB AgrCust ()
	qPrint Agr.Cus, 5, 47, 0, 1
	qPrint Cus.Nam, 3, 54, 0, 1
	qPrint Cus.Nm1, 4, 54, 0, 1
	qPrint Cus.Add, 5, 54, 0, 1
	qPrint Cus.Str, 6, 54, 0, 1
	qPrint Cus.Cty + " " + Cus.Sta + " " + Cus.Zip, 7, 54, 0, 1
	qPrint "Telephone(H): " + Cus.Tel, 8, 54, 0, 1
	qPrint Cus.Rmk, 8, 15, 0, 1
END SUB

SUB AgrDisp ()
	ERASE Ser, Lon, Old, Cob, Cst, Mkt, Dis, Trm, Rats
	IF FindIni(Agr.Mgr) = 0 THEN ERROR 201
	COLOR 7, 1
	GET #13, VAL(Agr.Cus), Cus
	AgrCust
	qPrint Agr.Dat, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	qPrint Agr.Num, 5, 33, 0, 1
	qPrint Agr.Rte, 5, 42, 0, 1
	Ser(1) = Agr.It1: Lon(1) = Agr.Lo1: Old(1) = Agr.Lo1: Dis(1) = Agr.Ds1: Mkt(1) = Agr.Fm1
	Ser(2) = Agr.It2: Lon(2) = Agr.Lo2: Old(2) = Agr.Lo2: Dis(2) = Agr.Ds2: Mkt(2) = Agr.Fm2
	Ser(3) = Agr.It3: Lon(3) = Agr.Lo3: Old(3) = Agr.Lo3: Dis(3) = Agr.Ds3: Mkt(3) = Agr.Fm3
	Ser(4) = Agr.It4: Lon(4) = Agr.Lo4: Old(4) = Agr.Lo4: Dis(4) = Agr.Ds4: Mkt(4) = Agr.Fm4
	IF FindSer(Ser(1)) THEN AgrInvD 1 ELSE AgrInvN 0
	IF FindSer(Ser(2)) THEN AgrInvD 2 ELSE AgrInvN 2
	IF FindSer(Ser(3)) THEN AgrInvD 3 ELSE AgrInvN 4
	IF FindSer(Ser(4)) THEN AgrInvD 4 ELSE AgrInvN 6

	IF FindSer(Lon(1)) THEN AgrLonD 1 ELSE AgrInvN 1
	IF FindSer(Lon(2)) THEN AgrLonD 2 ELSE AgrInvN 3
	IF FindSer(Lon(3)) THEN AgrLonD 3 ELSE AgrInvN 5
	IF FindSer(Lon(4)) THEN AgrLonD 4 ELSE AgrInvN 7

	IF FindCmb(Agr.Cmb) = 0 THEN Cmb.Des = ""
	qPrint Agr.Cmb, 11, 14, 0, 1
	qPrint Cmb.Des, 11, 20, 0, 1

	COLOR 11, 3
	qPrint Agr.Mst, 28, 16, 0, 1
	qPrint Agr.Msr, 28, 22, 0, 1
	LOCATE 28, 31: PRINT USING "####.##"; VAL(Agr.Mst) * VAL(Agr.Msr);
	qPrint Agr.Wst, 29, 16, 0, 1
	qPrint Agr.Wsr, 29, 22, 0, 1
	LOCATE 29, 31: PRINT USING "####.##"; VAL(Agr.Wst) * VAL(Agr.Wsr);
	qPrint Agr.Mtr, 28, 57, 0, 1
	qPrint Agr.Mrt, 28, 63, 0, 1
	LOCATE 28, 72: PRINT USING "####.##"; VAL(Agr.Mtr) * VAL(Agr.Mrt);
	qPrint Agr.Wtr, 29, 57, 0, 1
	qPrint Agr.Wrt, 29, 63, 0, 1
	LOCATE 29, 72: PRINT USING "####.##"; VAL(Agr.Wtr) * VAL(Agr.Wrt);
	
	COLOR 14, 2
	AgrStat 34, 24
	AgrFreq 35, 61
	qPrint Agr.Ref, 35, 24, 0, 1
	qPrint Agr.Nxt, 36, 24, 0, 1
	qPrint Agr.Pod, 37, 24, 0, 1
	qPrint Agr.Dta, 38, 24, 0, 1
	qPrint TraStat(Agr.Act), 39, 24, 0, 1
	LOCATE 40, 24: PRINT USING "####d"; Agr.Eza; Agr.Fre; Agr.Ndl;
	LOCATE 41, 24: PRINT USING "####m####w####d"; Agr.Npm; Agr.Npw; Agr.Npd;
	qPrint Agr.Acc, 34, 61, 0, 1
	qPrint Agr.Wav, 35, 77, 0, 1
	qPrint Agr.Due, 36, 61, 0, 1
	qPrint LEFT$(DayWeek$(Agr.Due), 3), 36, 73, 0, 1
	qPrint Agr.Pay, 37, 61, 0, 1
	qPrint Agr.Pro, 40, 63, 0, 1
END SUB

DEFINT A-Z
SUB AgrFreq (r, c)
	SELECT CASE Agr.Frq
		   CASE "C": f$ = "Cash   "
		   CASE "D": f$ = "Daily  "
		   CASE "W": f$ = "Weekly "
		   CASE "M": f$ = "Monthly"
	END SELECT
	qPrint f$, r, c, 0, 1
END SUB

SUB AgrInvD (n)
	GET #10, Inx.Num, Inv
	IF FindCod(Inx.Cod) = 0 THEN ERROR 200
	Ser(n) = Inv.Ser
	Cob(n) = Cod.Grp
	Cst(n) = Inv.Tdr
	InvLine n
END SUB

SUB AgrInvN (n)
	qPrint "          ", n + 15, 2, 0, 1
	qPrint "          ", n + 15, 13, 0, 1
	qPrint "    ", n + 15, 24, 0, 1
	qPrint "                        ", n + 15, 29, 0, 1
	qPrint "          ", n + 15, 54, 0, 1
	qPrint "   ", n + 15, 65, 0, 1
	qPrint "       ", n + 15, 69, 0, 1
	qPrint "   ", n + 15, 77, 0, 1
END SUB

SUB AgrLonD (n)
	GET #10, Inx.Num, Inv
	IF FindCod(Inx.Cod) = 0 THEN ERROR 200
	Lon(n) = Inv.Ser
	COLOR 7, 4
	qPrint Inv.Ser, 2 * n + 14, 13, 0, 1
	qPrint Inv.Cod, 2 * n + 14, 24, 0, 1
	qPrint Cod.Des, 2 * n + 14, 29, 0, 1
	qPrint Inv.Mdl, 2 * n + 14, 54, 0, 1
	LOCATE 2 * n + 14, 65: PRINT USING "###"; Inv.Tim;
END SUB

SUB AgrPrnt ()
	IF Response("Resp-05") THEN EXIT SUB
	IF PrinterReady%(1) = 0 THEN ERROR 208
	IF UCASE$(LEFT$(Agr.Cod, 3)) = "PAG" THEN Pager = 1
	LPRINT CHR$(27); "@";
	LPRINT CHR$(27); "x"; "1";
	GET #13, VAL(Agr.Cus), Cus
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT TAB(5); Sto.Nam; TAB(48); Cus.Nam
	LPRINT TAB(5); Sto.Add; TAB(48); Cus.Nm1
	LPRINT TAB(5); Sto.Str; TAB(48); Cus.Add
	LPRINT TAB(5); RTRIM$(Sto.Cty); ", "; Sto.Sta; " "; Sto.Zip;
	LPRINT TAB(48); Cus.Str
	LPRINT TAB(5); "Telephone: "; RIGHT$(Sto.Tel, 8);
	LPRINT TAB(48); RTRIM$(Cus.Cty); ", "; Cus.Sta; " "; Cus.Zip
	LPRINT
	LPRINT
	LPRINT
	LPRINT USING "    &    &    &   &   &"; Agr.Dat; Sto.Num; Agr.Mgr; Agr.Num; Agr.Frq;
	SELECT CASE Agr.Frq
		   CASE "W": LPRINT "eekly     ";
		   CASE "M": LPRINT "onthly    ";
		   CASE "C": LPRINT "ash Sale  ";
	END SELECT
	IF Agr.Cmb > "    " THEN
	   LPRINT "RENTED AS GROUP ONLY."
	ELSE
	   LPRINT
	END IF
	LPRINT
	LPRINT
	FOR x = 1 TO 4
	IF FindSer(Ser(x)) THEN
	   GET #10, Inx.Num, Inv
	   IF FindCod(Inv.Cod) = 0 THEN ERROR 200
	   LPRINT USING "   &          &       &         &"; Inv.Bra; Inv.Mdl; Inv.Ser; Cod.Des
	ELSE
	   LPRINT
	END IF
	NEXT
	IF Pager THEN
	   LPRINT "   I understand that no ownership options exist under this agreement."
	   LPRINT "                       Customer signature ___________________________"
	ELSE
	   LPRINT USING "   Accessories: &"; Agr.Acc
	   LPRINT
	END IF
	LPRINT TAB(47); Agr.Wrt; " +"
	LPRINT TAB(47); Agr.Mrt; " +"
	LPRINT TAB(22); DayWeek$(Agr.Due); TAB(45); Agr.Due; TAB(69); Agr.Trp
	LPRINT
	LPRINT TAB(33); Sto.Nam
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	FOR x = 1 TO 4
		IF FindSer(Ser(x)) THEN
		   GET #10, Inx.Num, Inv
		   IF FindCod(Inv.Cod) = 0 THEN ERROR 200
		   LPRINT TAB(7); Cod.Des; TAB(38);
		   IF Inv.Tim > 1 THEN LPRINT TAB(47);
		   LPRINT "X"
		ELSE
		   LPRINT
		END IF
	NEXT
	LPRINT TAB(28); Agr.Lat; TAB(70); Agr.Trp
	LPRINT
	IF Pager THEN
	   LPRINT TAB(45); "N/A"; TAB(62); "N/A"
	   LPRINT TAB(7); "N/A"; TAB(25); "N/A"
	   LPRINT
	   LPRINT TAB(10); "N/A"
	   LPRINT TAB(70); "N/A"
	   LPRINT TAB(20); "N/A"
	ELSE
	   LPRINT TAB(45); Agr.Wtr; TAB(62); Agr.Mtr
	   LPRINT TAB(7); :  LPRINT USING "#####.##"; VAL(Agr.Wtr) * VAL(Agr.Wrt);
	   LPRINT TAB(25); : LPRINT USING "#####.##"; VAL(Agr.Mtr) * VAL(Agr.Mrt)
	   LPRINT
	   LPRINT USING "          #####.## "; .62 * VAL(Agr.Wtr) * VAL(Agr.Wrt)
	   LPRINT TAB(70); Agr.Wtr
	   LPRINT TAB(20); Agr.Mtr
	END IF
	LPRINT
	IF Pager THEN
	   LPRINT "  No ownership options exist under this agreement."
	ELSE
	   f$ = "   Balance less 0%"
	   IF Agr.Mtr > " 11" THEN f$ = " Balance less 20%"
	   IF Agr.Mtr > " 14" THEN f$ = " Balance less 30%"
	   IF Agr.Mtr > " 17" THEN f$ = " Balance less 40%"
	   LPRINT f$; ", no early payoff during the last 60 days of the agreement."
	END IF
	LPRINT
	LPRINT
	LPRINT
	LPRINT USING " ####.##+Tax"; VAL(Agr.Pay) + VAL(Agr.Pro)
	LPRINT CHR$(12);
END SUB

DEFSNG A-Z
SUB AgrProm ()
	IF Response("Resp-08") THEN EXIT SUB
	IF PrinterReady%(1) = 0 THEN ERROR 208
	LPRINT CHR$(27); "@";
	LPRINT CHR$(27); "x"; "1";
	LPRINT SPACE$(35); "PROMISSORY NOTE"
	LPRINT
	LPRINT
	LPRINT "I, "; RTRIM$(Cus.Nam); " promise to pay for value received:"
	LPRINT
	LPRINT "      Code        Model Number     Serial Number     Description             "
	LPRINT "      ----        ------------     -------------   --------------------------"
	FOR x = 1 TO 4
	IF FindSer(Ser(x)) THEN
	   GET #10, Inx.Num, Inv
	   IF FindCod(Inv.Cod) = 0 THEN ERROR 200
	   LPRINT USING "      &         &       &      &"; Inv.Bra; Inv.Mdl; Inv.Ser; Cod.Des
	ELSE
	   LPRINT
	END IF
	NEXT
	LPRINT "ths sum of:"
	LPRINT
	LPRINT USING "#####.## in installments of & for & months due on the & of each month"; VAL(Agr.Mtr) * VAL(Agr.Mrt); Agr.Mrt; Agr.Mtr; MID$(Agr.Due, 4, 2)
	LPRINT "or"
	LPRINT USING "#####.## in installments of & for & weeks due on & of each week"; VAL(Agr.Wtr) * VAL(Agr.Wrt); Agr.Wrt; Agr.Wtr; RTRIM$(DayWeek$(Agr.Due))
	LPRINT
	LPRINT "until the entire amount is fully paid."
	LPRINT
	LPRINT "It is understood and agreed that the failure to pay this note, or an     "
	LPRINT "installment as above promised, or any interest hereon, when due, shall   "
	LPRINT "at the election of the holder of said note at once become due and payable"
	LPRINT
	LPRINT "And it is hereby especially agreed that if this note is placed in the    "
	LPRINT "hands of an attorney for collection, or collected by suit, or in probate "
	LPRINT "or bankruptcy proceedings, the undersigned agrees to pay 10% additional  "
	LPRINT "on the principal then due hereon as attorney's fees."
	LPRINT
	LPRINT
	LPRINT "Witnessed by:                                      Date:                 "
	LPRINT
	LPRINT
	LPRINT "Customer Signature:                                Date:                 "
	LPRINT
END SUB

DEFINT A-Z
FUNCTION AgrRate ()
	n = VAL(Agr.Mtr) * DaysAvg / 7
	RSET Agr.Wtr = RIGHT$(STR$(n), 3)
	IF Agr.Mtr < "  1" THEN WinMssg "Monthly terms must not be zero":    e = 1
	IF Agr.Mtr > "230" THEN WinMssg "Weekly terms execeed 999 weeks":    e = 1
	IF Agr.Mrt < "  0.01" THEN WinMssg "Monthly rates must not be zero": e = 1
	IF Agr.Wrt < "  0.01" THEN WinMssg "Weekly rates must not be zero":  e = 1
	IF VAL(Agr.Mtr) * VAL(Agr.Mrt) > 9999.99 THEN WinMssg "Monthly total exceeds $9999.99": e = 1
	IF VAL(Agr.Wtr) * VAL(Agr.Wrt) > 9999.99 THEN WinMssg "Weekly total exceeds $9999.99":  e = 1
	IF e THEN
	   AgrRate = 1
	ELSE
	   COLOR 11, 3
	   LOCATE 28, 57: PRINT Agr.Mtr;
	   LOCATE 28, 63: PRINT Agr.Mrt;
	   LOCATE 28, 72: PRINT USING "####.##"; VAL(Agr.Mtr) * VAL(Agr.Mrt);
	   LOCATE 29, 57: PRINT Agr.Wtr;
	   LOCATE 29, 63: PRINT Agr.Wrt;
	   LOCATE 29, 72: PRINT USING "####.##"; VAL(Agr.Wtr) * VAL(Agr.Wrt);
	END IF
END FUNCTION

SUB AgrStat (r, c)
	SELECT CASE Agr.Sta
	CASE "A"
		 f$ = "Active    "
	CASE "V"
		 f$ = "Void      "
	CASE "T"
		 f$ = "Terminated"
	CASE "N"
		 f$ = "Note      "
	CASE "P"
		 f$ = "Paid Out  "
	CASE "C"
		 f$ = "Charge-off"
	CASE "S"
		 f$ = "Swap Out  "
	CASE ELSE
		 f$ = "Unkown    "
	END SELECT
	qPrint f$, r, c, 0, 1
END SUB

FUNCTION ChgLoan ()
	ChgLoan = 1
	FOR x = 1 TO 4
		IF Lon(x) > "          " THEN ChgLoan = 0
	NEXT
END FUNCTION

SUB CusEdit (h$)
	PCOPY 0, 2: CALL MhDisplayByName(0, 0, "Customer", e)
	GET #13, Cnx.Num, Cus
	
	qPrint STR$(Cnx.Num), 5, 30, 0, 1
	qPrint Cus.Sal, 7, 30, 0, 1
	qPrint Cus.Nam, 8, 30, 0, 1
	qPrint Cus.Nm1, 9, 30, 0, 1
	qPrint Cus.Add, 10, 30, 0, 1
	qPrint Cus.Str, 11, 30, 0, 1
	qPrint Cus.Cty + " " + Cus.Sta + " " + Cus.Zip, 12, 30, 0, 1
	qPrint Cus.Tel, 13, 30, 0, 1
	qPrint Cus.Rmk, 15, 20, 0, 1

	qPrint Cus.SSn, 19, 25, 0, 1
	qPrint Cus.Dln, 20, 25, 0, 1
	qPrint Cus.Dob, 21, 25, 0, 1
	qPrint Cus.Mar, 22, 25, 0, 1
	qPrint Cus.Rac, 23, 25, 0, 1
	qPrint Cus.Sex, 24, 25, 0, 1

	qPrint Cus.Emp, 26, 25, 0, 1
	qPrint Cus.Lon, 27, 25, 0, 1
	qPrint Cus.Inc, 28, 25, 0, 1
	qPrint Cus.Oth, 29, 25, 0, 1
	qPrint Cus.Wor, 30, 25, 0, 1
								   
	qPrint Cus.Dis, 32, 25, 0, 1
	qPrint Cus.Ref, 33, 27, 0, 1
	qPrint Cus.Typ, 34, 27, 0, 1
									  
	qPrint Cus.SS1, 19, 41, 0, 1
	qPrint Cus.Dl1, 20, 41, 0, 1
	qPrint Cus.Bd1, 21, 41, 0, 1
	qPrint Cus.St1, 22, 41, 0, 1
	qPrint Cus.Rc1, 23, 41, 0, 1
	qPrint Cus.Sx1, 24, 41, 0, 1
											
	qPrint Cus.Em1, 26, 41, 0, 1
	qPrint Cus.Ln1, 27, 41, 0, 1
	qPrint Cus.In1, 28, 41, 0, 1
	qPrint Cus.Ot1, 29, 41, 0, 1
	qPrint Cus.Wt1, 30, 41, 0, 1

	Name$ = Cus.Nam
	NameB$ = Cus.Nam
	ZipCod$ = Cus.Zip
	CompNam NameB$
620 EntryS 7, 30, 10, e, b, Cus.Sal:    IF e THEN 660 ELSE IF b THEN 620
621 EntryS 8, 30, 26, e, b, Cus.Nam:    IF e THEN 660 ELSE IF b THEN 620
	IF Cus.Nam < "!" THEN WinMssg "Customer name please.": GOTO 621
	NameA$ = Cus.Nam
	CompNam NameA$
	IF NameB$ <> NameA$ AND h$ < "8" THEN WinMssg "Name change not allowed": GOTO 621
622 EntryS 9, 30, 26, e, b, Cus.Nm1:    IF e THEN 660 ELSE IF b THEN 621
623 EntryS 10, 30, 26, e, b, Cus.Add:   IF e THEN 660 ELSE IF b THEN 622
624 EntryS 11, 30, 26, e, b, Cus.Str:   IF e THEN 660 ELSE IF b THEN 623
625 EntryS 12, 30, 17, e, b, Cus.Cty:   IF e THEN 660 ELSE IF b THEN 624
626 EntryU 12, 48, 2, e, b, Cus.Sta:    IF e THEN 660 ELSE IF b THEN 625
627 EntryN 12, 51, 5, 0, e, b, Cus.Zip: IF e THEN 660 ELSE IF b THEN 626
628 EntryT 13, 30, e, b, Cus.Tel:       IF e THEN 660 ELSE IF b THEN 627
629 EntryS 15, 20, 36, e, b, Cus.Rmk:   IF e THEN 660 ELSE IF b THEN 628

630 EntryX 19, 25, e, b, Cus.SSn:       IF e THEN 660 ELSE IF b THEN 629
631 EntryU 20, 25, 11, e, b, Cus.Dln:   IF e THEN 660 ELSE IF b THEN 630
632 EntryD 21, 25, e, b, Cus.Dob:       IF e THEN 660 ELSE IF b THEN 631
633 EntryU 22, 25, 1, e, b, Cus.Mar:    IF e THEN 660 ELSE IF b THEN 632
634 EntryU 23, 25, 1, e, b, Cus.Rac:    IF e THEN 660 ELSE IF b THEN 633
635 EntryU 24, 25, 1, e, b, Cus.Sex:    IF e THEN 660 ELSE IF b THEN 634

636 EntryU 26, 25, 15, e, b, Cus.Emp:   IF e THEN 660 ELSE IF b THEN 635
637 EntryU 27, 25, 15, e, b, Cus.Lon:   IF e THEN 660 ELSE IF b THEN 636
638 EntryU 28, 25, 15, e, b, Cus.Inc:   IF e THEN 660 ELSE IF b THEN 637
639 EntryU 29, 25, 15, e, b, Cus.Oth:   IF e THEN 660 ELSE IF b THEN 638
640 EntryT 30, 25, e, b, Cus.Wor:       IF e THEN 660 ELSE IF b THEN 639

641 EntryN 32, 25, 3, 0, e, b, Cus.Dis: IF e THEN 660 ELSE IF b THEN 640
642 EntryU 33, 27, 1, e, b, Cus.Ref:    IF e THEN 660 ELSE IF b THEN 641
643 EntryU 34, 27, 1, e, b, Cus.Typ:    IF e THEN 660 ELSE IF b THEN 642

644 EntryX 19, 41, e, b, Cus.SS1:       IF e THEN 660 ELSE IF b THEN 643
645 EntryU 20, 41, 11, e, b, Cus.Dl1:   IF e THEN 660 ELSE IF b THEN 644
646 EntryD 21, 41, e, b, Cus.Bd1:       IF e THEN 660 ELSE IF b THEN 645
647 EntryU 22, 41, 1, e, b, Cus.St1:    IF e THEN 660 ELSE IF b THEN 646
648 EntryU 23, 41, 1, e, b, Cus.Rc1:    IF e THEN 660 ELSE IF b THEN 647
649 EntryU 24, 41, 1, e, b, Cus.Sx1:    IF e THEN 660 ELSE IF b THEN 648

650 EntryU 26, 41, 15, e, b, Cus.Em1:   IF e THEN 660 ELSE IF b THEN 649
651 EntryU 27, 41, 15, e, b, Cus.Ln1:   IF e THEN 660 ELSE IF b THEN 650
652 EntryU 28, 41, 15, e, b, Cus.In1:   IF e THEN 660 ELSE IF b THEN 651
653 EntryU 29, 41, 15, e, b, Cus.Ot1:   IF e THEN 660 ELSE IF b THEN 652
654 EntryT 30, 41, e, b, Cus.Wt1:       IF e THEN 660 ELSE IF b THEN 653
	SOUND 1000, 1
	COLOR 15, 4: LOCATE 34, 39: PRINT "Press [*] to save"; : KeyPres f$
	COLOR 15, 0: LOCATE 34, 39: PRINT "                 ";
	IF f$ = "*" THEN
	   Cnx.Nam = Cus.Nam
	   Cus.Mmc = 0
	   Cus.Num = RIGHT$(STR$(10000 + Cnx.Num), 4)
	   Agr.Cus = Cus.Num
	   PUT #13, Cnx.Num, Cus
	   PUT #14, Cnx.Num, Cnx
	   GET #13, Cnx.Num, Cul
	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Mgr = Pas.Ini
	   Tra.Rec = Cul.Cus
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	   PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cus.Num; Cus.Nam
	   Top = LOF(13) \ LEN(Cus)
	   IF Name$ <> Cus.Nam THEN
		  CLOSE
		  SHELL "SortF ..\DataFile\Customer.INX ..\DataFile\Customer.NAM /L28 /+1,26 /C /Q"
		  OpenFiles
	   END IF

	   n = FindZip(Cus.Zip) 'new zip code record number
	   o = FindZip(ZipCod$) 'old zip code record number
							'update zip code master
	   IF n = 0 THEN n = 1 + LOF(27) \ LEN(Zip)
	   GET #27, n, Zip
	   Zip.Cod = Cus.Zip
	   Zip.Mrk = " "
	   Zip.Num = Zip.Num + 1
	   PUT #27, n, Zip
	   IF Tra.Typ = "CM" THEN
		  GET #27, o, Zip
		  Zip.Num = Zip.Num - 1
		  PUT #27, o, Zip
	   END IF
	   CLOSE
	   SHELL "SortF ..\DataFile\ZipCodes.Mas ..\DataFile\ZipCodes /L8 /+1,5 /Q"
	   KILL "..\DataFile\ZipCodes.Mas"
	   NAME "..\DataFile\ZipCodes" AS "..\DataFile\ZipCodes.Mas"
	   OpenFiles
	   
	   PCOPY 2, 0
	   EXIT SUB
	ELSE
	   GOTO 654
	END IF
660 Cnx.Num = 0
	PCOPY 2, 0
END SUB

SUB CustAgr (f$, f, e)
   f = 0: e = 0: b = 1: t = LOF(18) / LEN(Agx)
   DO UNTIL (t < b)
	  m = (t + b) \ 2: GET #18, m, Agx
	  IF f$ > Agx.Cus THEN b = m + 1 ELSE t = m - 1
   LOOP
   DO WHILE f$ > Agx.Cus AND NOT EOF(18)
	  GET #18, , Agx
   LOOP
   IF f$ = Agx.Cus THEN
	  f = LOC(18): e = f
	  DO WHILE NOT EOF(18)
		 GET #18, , Agx
		 IF f$ = Agx.Cus THEN e = LOC(18) ELSE EXIT DO
	  LOOP
   END IF
END SUB

DEFLNG T
SUB InvCalc (r)
	COLOR 15, 4
   '==========================================================================
   'Cst = Item's cost
   'Mkt = (Cost x FMV factor) - (Rental Revenue ö FMVdrop factor)
   'Mkt = Use cost if FMV drops below cost
   'Dis = Revenue distribution (Cost ö Total Cost)
   'Tot = Total cost of items
   '--------------------------------------------------------------------------
	Cst(r) = 100 * VAL(Inv.Prc)                              'item's price
	Mkt(r) = Cst(r) * VAL(Cod.Fmv) - (Inv.Tin / VAL(Cod.Dfm))'market value
	IF Mkt(r) < Cst(r) THEN Mkt(r) = Cst(r)                  'if FMV drops
	FOR x = 1 TO r: Tot = Tot + Cst(x): NEXT                 'below cost
	FOR x = 1 TO r                                           'use cost
		Dis(x) = 100 * (Cst(x) / Tot)                        'distribution
		s = 13 + (2 * x)                                     'screen location
		LOCATE s, 77: PRINT USING "###"; Dis(x);             'of revenue
	NEXT                                                     'by cost ratio
   '==========================================================================
   'Trm = Drop terms = Days on rent ö drop terms x days in a month
   'Tot = Agreement Total = Monthly rate x (monthly term - Drop terms)
   '      if Agr.Total is less than fair market value
   'Max = Maximum drop terms = Market ö Monthly rate
   '--------------------------------------------------------------------------
	Ttr = VAL(Rat.Mtr(1))                                    'monthly term
	Trt = VAL(Rat.Mrt(1)) * 100                              'monthly rate
	Trm(r) = Inv.Tdr \ (DaysAvg * VAL(Cod.Dtr))              'drop months
	Tot = Trt * (Ttr - Trm(r))                               'agreement total
	IF Tot < Mkt(r) THEN                                     'if < FMV
	   Max = CINT(Mkt(r) / Trt)                              '
	   Trm(r) = Ttr - Max                                    'max drop terms
	END IF                                                   '
	IF Trm(r) > .33 * Ttr THEN Trm(r) = .33 * Ttr            'bellow 33%
   '--------------------------------------------------------------------------
	IF Inv.Ovr = "O" THEN Trm(r) = 0                         'override no drop
	InvLine r
END SUB

DEFINT T
FUNCTION InvChng ()
	 InvChng = 1
	 FOR x = 1 TO 4
	 IF Old(x) <> Lon(x) THEN
		InvChng = 0
		EXIT FUNCTION
	 END IF
	 NEXT
END FUNCTION

FUNCTION InvItem (c, n)
	r = 13 + n * 2
98  EntryU r, 2, 10, e, b, Ser(n)
	IF e THEN InvItem = 1: EXIT FUNCTION
	IF b THEN InvItem = 2: EXIT FUNCTION
	IF FindSer(Ser(n)) = 0 THEN IF WinItem(c, n) THEN 98
	IF InvRtrv THEN WinMssg "Code not found": GOTO 98
	Ser(n) = Inv.Ser
	IF InvVrfy THEN 98
END FUNCTION

FUNCTION InvItmL (n)
	r = 14 + n * 2
96  EntryU r, 13, 10, e, b, Lon(n)
	IF e THEN InvItmL = 1: EXIT FUNCTION
	IF b THEN InvItmL = 2: EXIT FUNCTION
	IF Lon(n) > "          " THEN
	   IF FindSer(Lon(n)) = 0 THEN IF WinItem(1, n) THEN 96
	   IF InvRtrv THEN WinMssg "Code not found": GOTO 96
	   Lon(n) = Inv.Ser
	   AgrLonD n
	   IF Old(n) <> Lon(n) THEN IF InvVrfy THEN 96
	ELSE
	   Lon(n) = "          "
	   IF Old(n) <> Lon(n) THEN
		  IF FindSer(Ser(n)) = 0 THEN WinMssg "Unable to locate original!": GOTO 96
		  GET #10, Inx.Num, Inv
		  IF Inv.Sta <> 3 AND Inv.Sta <> 5 THEN WinMssg "Original item is " + Status(Inv.Sta): GOTO 96
	   END IF
	   AgrInvN 2 * n - 1
	END IF
END FUNCTION

SUB InvLine (n)
	COLOR 15, 4
	r = 2 * n + 13
	LOCATE r, 2:  PRINT Ser(n);
	LOCATE r, 24: PRINT Inv.Cod;
	LOCATE r, 29: PRINT Cod.Des;
	LOCATE r, 54: PRINT Inv.Mdl;
	LOCATE r, 65: PRINT USING "###"; Inv.Tim;
	LOCATE r, 69: PRINT USING "####.##"; Mkt(n) / 100;
	LOCATE r, 77: PRINT USING "###"; Dis(n);
END SUB

SUB InvRest ()
	ERASE Ser, Lon, Old, Cob, Cst, Mkt, Dis, Trm, Rats
	Agr.Cod = "    "
	FOR x = 0 TO 3: AgrInvN x * 2: NEXT
END SUB

DEFSNG A-Z
FUNCTION InvRtrv ()
	GET #10, Inx.Num, Inv
	IF FindCod(Inv.Cod) = 0 THEN InvRtrv = 1
	IF Inv.Ovr <> "O" THEN Inv.Rat = Cod.Rat
	GET #4, VAL(Inv.Rat), Rat
END FUNCTION

DEFINT I
FUNCTION InvVrfy ()
   IF Inv.Sta > 0 THEN InvVrfy = 1: WinMssg "Item is " + Status(Inv.Sta)
END FUNCTION

DEFINT A-H, J-Z
SUB mChgAgr ()
	Statlin "Charge-off Agreement"
	p$ = Pas.Ini
	u$ = Pas.Nam
	h$ = Pas.Lvl
600 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 600
	Yes = 1: IF WinCust(h$) THEN 600
	AgrDisp
	IF Agr.Sta <> "A" THEN WinMssg "Not an active agreement!": GOTO 600
	IF Agr.Frq = "C" THEN WinMssg "Not allowed, Cash Sale Agreement!": GOTO 600
	IF Confirm(1, "CHARGE-OFF agreement") = 0 THEN 600

	Agr.Sta = "C"
	Agr.Dta = DATE$
	Agr.Act = "AC"
	AgrStat 34, 24

	FOR x = 1 TO 4                  '  repeat for four items
	IF FindSer(Lon(x)) THEN         'if there is a LOANER
	   GET #10, Inx.Num, Inv        '  get loaner record
	   a$ = a$ + Inv.Ser + " "      '  for transaction log
	   InvActv 11                   '  increment activity
	   InsActv 3, 1                 '  increment BOR activity
	   Inv.Sta = 11                 '  it becomes CHARGE-OFF
	   Inv.Act = "AC"               '  stamp activity type
	   Inv.Dta = DATE$              '  record the date and
	   PUT #10, Inx.Num, Inv        '  write it to disk
	   IF FindSer(Ser(x)) THEN      'retrieve original items
		  GET #10, Inx.Num, Inv     '  get original item
		  SELECT CASE Inv.Sta       '  check originals status
		  CASE 3                    'if ON-RENT (AS) then
			 b$ = b$ + Inv.Ser + " "'  for transaction log
			 InvActv 0              '  increment activity
			 Inv.Sta = 0            '  it becomes IDLE
		  CASE 4                    'if ON-RENT (IS) then
			 c$ = c$ + Inv.Ser + " "'  for transaction log
			 InvActv 1              '  increment activity
			 Inv.Sta = 1            '  change to IN-SERVICE
		  CASE 5                    'if ON-RENT (AR) then
			 b$ = b$ + Inv.Ser + " "'  for transaction log
			 InvActv 0              '  increment activity
			 Inv.Sta = 0            '  change to IDLE
		  CASE ELSE                 '
			 WinMssg "Status ERROR" 'otherwise error message
		  END SELECT                '
		  Inv.Act = "AC"            '  stamp activity type
		  Inv.Agr = "      "        'clear agreement #
		  Inv.Dta = DATE$           'according to code master
		  PUT #10, Inx.Num, Inv     'and save items record
	   END IF                       '
	ELSE                            '
	   IF FindSer(Ser(x)) THEN      'if no loaner then
		  GET #10, Inx.Num, Inv     '  get original item
		  a$ = a$ + Inv.Ser + " "   '  for transaction log
		  InvActv 11                '  increment activity
		  InsActv 3, 1              '  increment BOR activity
		  Inv.Sta = 11              '  change status to
		  Inv.Act = "AC"            '  stamp activity type
		  Inv.Dta = DATE$           '  CHARGE-OFF
		  PUT #10, Inx.Num, Inv     '
	   END IF
	END IF
	NEXT
	   
	PUT #16, Agx.Num, Agr
	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AC"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	GET #22, 1, Act
	FOR z = 1 TO 5
	Act.Agc(z) = Act.Agc(z) + 1
	NEXT
	PUT #22, 1, Act

	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; a$; b$; c$

	AgrDetl u$
END SUB

SUB mDspAgr ()
	Yes = 1
	Statlin "Display Agreement"
300 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 300
	IF WinCust(Pas.Lvl) THEN 300
	AgrDisp
	GOTO 300
END SUB

SUB mEzaAgr ()
	p$ = Pas.Ini
	h$ = Pas.Lvl
	Statlin "Deffered (EZA) Days"
850 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 850
	Yes = 1: IF WinCust(h$) THEN 850
	AgrDisp
	IF Agr.Sta <> "A" THEN WinMssg "Agreement is not active": GOTO 850
	IF Agr.Frq = "C" THEN WinMssg "Not allowed, Cash Sale Agreement!": GOTO 850
	COLOR 16, 7
	LOCATE 36, 5: PRINT "     Next Due Date"; : LOCATE , 24: PRINT Agr.Nxt;
	LOCATE 37, 5: PRINT "       Payout Date"; : LOCATE , 24: PRINT Agr.Pod;
	LOCATE 38, 5: PRINT "Number of EZA Days"; : LOCATE , 24: PRINT "      days";
852 EntryN 38, 26, 3, 0, e, b, f$
	IF e THEN EXIT SUB ELSE IF b THEN 850
	IF h$ < "8" THEN
	   IF Agr.Frq = "W" THEN WinMssg "No EZA on weekly agreement": GOTO 852
	   IF Agr.Rct > 0 THEN WinMssg "Only before first receipt": GOTO 852
	   IF Agr.Eza > 0 THEN WinMssg "No more EZA days allowed": GOTO 852
	   IF VAL(f$) > 4 THEN WinMssg "Requires high level User-ID": GOTO 852
	END IF
	COLOR 16, 7
	
	LOCATE 36, 24: PRINT DayMath$(Agr.Nxt, VAL(f$));
	LOCATE 37, 24: PRINT DayMath$(Agr.Pod, VAL(f$));
	IF Confirm(1, "EZA agreement") = 0 THEN 852
	Agr.Nxt = DayMath$(Agr.Nxt, VAL(f$))
	Agr.Pod = DayMath$(Agr.Pod, VAL(f$))
	Agr.Eza = Agr.Eza + VAL(f$)
	Agr.Dta = DATE$
	Agr.Act = "AE"
	Agr.EZd = DATE$
	Agr.EZi = p$
	Agr.EZn = VAL(f$)
	PUT #16, Agx.Num, Agr

	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AE"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	Tra.Prc = VAL(f$)
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; f$; Agr.Num; Cus.Nam; Agr.Rte

	GET #22, 1, Act
	FOR z = 1 TO 5
	Act.Eza(z) = Act.Eza(z) + VAL(f$)
	NEXT
	PUT #22, 1, Act
END SUB

SUB mFreAgr ()
	p$ = Pas.Ini
	h$ = Pas.Lvl
	Statlin "Give Free Days"
950 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 950
	Yes = 1: IF WinCust(h$) THEN 950
	AgrDisp
	IF Agr.Sta <> "A" THEN WinMssg "Agreement not active": GOTO 950
	IF Agr.Frq = "C" THEN WinMssg "Not allowed, Cash Sale Agreement!": GOTO 950
	COLOR 16, 7
	LOCATE 36, 4: PRINT "      Next Due Date"; : LOCATE , 24: PRINT Agr.Nxt;
	LOCATE 37, 4: PRINT "        Payout Date"; : LOCATE , 24: PRINT Agr.Pod;
	LOCATE 38, 4: PRINT "Number of Free Days"; : LOCATE , 24: PRINT "      days";
952 EntryN 38, 26, 3, 0, e, b, f$
	IF e THEN EXIT SUB ELSE IF b THEN 950
	d = DateDif&(DayMath$(Agr.Nxt, VAL(f$)), Agr.Pod)
	IF d < 0 THEN WinMssg "Exceeds payout date": GOTO 952
	IF d = 0 AND ChgLoan = 0 THEN WinMssg "Can't payout, return loaners first": GOTO 952
	IF d = 0 THEN WinMssg "Confirm will payout agreement"
	COLOR 16, 7: LOCATE 36, 24: PRINT DayMath$(Agr.Nxt, VAL(f$));
	IF Confirm(1, "FREE day agreement") = 0 THEN 952

	Agr.Dta = DATE$
	Agr.Act = "AF"
	Agr.Nxt = DayMath$(Agr.Nxt, VAL(f$))
	Agr.Fre = Agr.Fre + VAL(f$)
	Agr.FRd = DATE$
	Agr.FRi = p$
	Agr.FRn = VAL(f$)
	IF Agr.Nxt = Agr.Pod THEN         '-------------------------
	   Agr.Sta = "P"                  'if agreement is paidout
	   FOR x = 1 TO 4                 'inventory is paid out too
		   IF FindSer(Ser(x)) THEN    '-------------------------
			  GET #10, Inx.Num, Inv
			  s$ = s$ + " " + Inv.Ser
			  InvActv 10
			  InsActv 2, 1
			  Inv.Sta = 10            '  change status
			  Inv.Act = "AF"          '  stamp activity type
			  Inv.Dta = DATE$         '  stamp activity date
			  PUT #10, Inx.Num, Inv
		   END IF
	   NEXT
	   Tra.Typ = "AO"
	   PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; s$
	END IF
	PUT #16, Agx.Num, Agr
	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AF"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	Tra.Prc = VAL(f$)
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; f$; Agr.Num; Cus.Nam

	GET #22, 1, Act
	FOR z = 1 TO 5
	Act.FRd(z) = Act.FRd(z) + VAL(f$)
	NEXT
	PUT #22, 1, Act

END SUB

SUB mLonAgr ()
	Statlin "Temporary Loaner"
	p$ = Pas.Ini
	h$ = Pas.Lvl
700 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 700
	Yes = 1: IF WinCust(h$) THEN 700
	AgrDisp
	IF Agr.Sta <> "A" THEN WinMssg "Not an active agreement!": GOTO 700
	IF Agr.Frq = "C" THEN WinMssg "Not allowed, Cash Sale Agreement!": GOTO 700
706 IF Agr.Cmb = "    " THEN
708    x = InvItmL(1): IF x = 1 THEN EXIT SUB ELSE IF x = 2 THEN 700
	   IF Lon(1) > "          " AND Cob(1) <> Cod.Grp THEN
		  IF h$ > "7" THEN WinMssg "Warning: group is not " + Cob(1)
		  IF h$ < "8" THEN WinMssg "Group must be " + Cob(1): GOTO 708
	   END IF
	ELSE
	   r = 1
709    x = InvItmL(r): IF x = 1 THEN EXIT SUB
	   IF x = 2 THEN r = r - 1: IF r < 1 THEN 700 ELSE 709
	   IF Lon(r) > "          " AND Cob(r) <> Cod.Grp THEN
		  IF h$ > "7" THEN WinMssg "Warning: group is not " + Cob(r)
		  IF h$ < "8" THEN WinMssg "Group must be " + Cob(r): GOTO 709
	   END IF
	   IF r < 4 AND Ser(r + 1) > "          " THEN r = r + 1: GOTO 709
	END IF
	IF InvChng THEN 706
	IF Confirm(1, "change LOANERS") = 0 THEN 706
	FOR r = 1 TO 4
	IF Lon(r) <> Old(r) THEN                '  loaner changed then
	   IF FindSer(Lon(r)) THEN           '---- replace a loaner
		  GET #10, Inx.Num, Inv             '  get new loaner item
		  a$ = "{Loaned " + Inv.Ser         '  for transaction log
		  InvActv 8                         '
		  Inv.Sta = 8                       '  change its status
		  Inv.Act = "AL"                    '  stamp activity type
		  Inv.Dta = DATE$                   '  stamp the date
		  Inv.Tim = Inv.Tim + 1             '  increment times rented
		  Inv.Agr = Agr.Num                 '  include agreement num
		  PUT #10, Inx.Num, Inv             '  and save items record
		  IF FindSer(Old(r)) THEN           '
			 GET #10, Inx.Num, Inv          '  if old loaner item
			 a$ = a$ + " replacing loaner " + Inv.Ser + "}"
			 InvActv 0                      '
			 Inv.Sta = 0                    '    change status to idle
			 Inv.Act = "AB"                 '    stamp activity type
			 Inv.Dta = DATE$                '    stamp the date
			 Inv.Agr = "      "             '    clear agreement num
			 PUT #10, Inx.Num, Inv          '    and save it
		  ELSEIF FindSer(Ser(r)) THEN       '  else
			 GET #10, Inx.Num, Inv          '    get original item
			 a$ = a$ + " replacing original " + Inv.Ser + "}"
			 InvActv 3                      '
			 Inv.Sta = 3                    '    change status to (AS)
			 Inv.Act = "AL"                 '    stamp activity type
			 Inv.Dta = DATE$                '    date it
			 PUT #10, Inx.Num, Inv          '    and save it
		  END IF                            '
	   ELSE                                 '
		  IF FindSer(Old(r)) THEN       '---- remove a lonaer
			 GET #10, Inx.Num, Inv          '  get loaner item
			 a$ = a$ + "{Replaced loaner " + Inv.Ser
			 InvActv 0                      '
			 Inv.Sta = 0                    '  idle it
			 Inv.Act = "AB"                 '  stamp activity type
			 Inv.Dta = DATE$                '  date it
			 Inv.Agr = "      "             '  clear agreement num
			 PUT #10, Inx.Num, Inv          '  and save it
			 IF FindSer(Ser(r)) THEN        '
				GET #10, Inx.Num, Inv       '  retrieve original item
				a$ = a$ + " with original " + Inv.Ser + "}"
				InvActv 2                   '
				Inv.Sta = 2                 '  change status on rent
				Inv.Act = "AL"              '  stamp activity type
				Inv.Dta = DATE$             '  date it
				PUT #10, Inx.Num, Inv       '  and save it
			 END IF                         '
		  END IF
	   END IF
	END IF
	NEXT
	Agr.Dta = DATE$
	Agr.Act = "AL"
	Agr.Lo1 = Lon(1)
	Agr.Lo2 = Lon(2)
	Agr.Lo3 = Lon(3)
	Agr.Lo4 = Lon(4)
	PUT #16, Agx.Num, Agr
	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AL"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; a$
	RctLoan
END SUB

DEFLNG L
SUB mNewAgr ()
	GET #16, 1 + LOF(16) \ LEN(Agr), Agr
	ERASE Ser, Lon, Old, Cob, Cst, Mkt, Dis, Trm, Rats
	Yes = 0
	Agr.Dat = DATE$
	Agr.Mgr = Pas.Ini
	Agr.Lat = " 5.00"
	Agr.Trp = " 5.00"
	Agr.Pro = " 5.00"
	Agr.Num = RIGHT$(STR$(10000001 + AgrNum), 6)

	qPrint Agr.Dat, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	qPrint Agr.Num, 5, 33, 0, 1
	
	IF Setup THEN
100 EntryD 5, 2, e, b, Agr.Dat:      IF e THEN 150 ELSE IF b THEN 100
	IF Agr.Dat < "0" THEN WinMssg "Must have!!": GOTO 100
	END IF
101 EntryU 5, 42, 1, e, b, Agr.Rte:  IF e THEN 150 ELSE IF b THEN IF Setup THEN 100 ELSE 101
	IF INSTR("123", Agr.Rte) = 0 THEN WinMssg "Use route 1,2,or 3": GOTO 101
102 EntryS 3, 54, 26, e, b, Cus.Nam: IF e THEN 150 ELSE IF b THEN 101
	IF WinCust(Pas.Lvl) THEN 102
	Agr.Cus = Cus.Num
	GET #13, VAL(Agr.Cus), Cus
	AgrCust
106 EntryU 11, 14, 4, e, b, Agr.Cmb: IF e THEN 150 ELSE IF b THEN 102
	IF Agr.Cmb <> Group$ THEN Group$ = Agr.Cmb: InvRest
	IF Agr.Cmb = "    " THEN
	   COLOR 15, 4: LOCATE 11, 14: PRINT "     Single                  ";
108    x = InvItem(0, 1): IF x = 1 THEN 150 ELSE IF x = 2 THEN 106
	   Agr.Cod = Inv.Cod
	   InvCalc 1
	ELSE
	   IF FindCmb(Agr.Cmb) = 0 THEN IF WinComb THEN 106
	   Agr.Cmb = Cmb.Cod
	   Group$ = Agr.Cmb
	   Combo$ = RTRIM$(" " + Cmb.Cde(1) + " " + Cmb.Cde(2) + " " + Cmb.Cde(3) + " " + Cmb.Cde(4))
	   COLOR 15, 4: LOCATE 11, 14: PRINT USING "& &"; Cmb.Cod; Cmb.Des;
	   r = 1
110    x = InvItem(3, r): IF x = 1 THEN 150
	   IF x = 2 THEN r = r - 1: IF r < 1 THEN 106 ELSE Combo$ = Combo$ + " " + Cod.Cmb: GOTO 110
	   InvCalc r
	   IF RIGHT$(Agr.Cmb, 3) = "XXX" THEN
		  IF r < VAL(Agr.Cmb) THEN r = r + 1: GOTO 110
	   ELSE
		  x = INSTR(Combo$, Cod.Cmb)
		  IF x = 0 THEN WinMssg "Item's group is " + Cod.Cmb: GOTO 110
		  Combo$ = LEFT$(Combo$, x - 2) + RIGHT$(Combo$, LEN(Combo$) - x - 1)
		  IF r < 4 AND Combo$ > " " THEN r = r + 1: GOTO 110
	   END IF
	   GET #4, VAL(Cmb.Rat), Rat
	END IF

	z = WinRate: IF z = 0 THEN 106
	RatDisp z
	Mt = VAL(Agr.Mtr)
112 EntryN 28, 57, 3, 0, e, b, Agr.Mtr:    IF e THEN 150 ELSE IF b THEN 106
	IF AgrRate THEN 112
	SELECT CASE Pas.Lvl
	CASE IS < "6"
		 IF VAL(Agr.Mtr) < Mt - 1 OR VAL(Agr.Mtr) > Mt + 6 THEN WinMssg "Low level password": GOTO 112
	CASE "6"
		 IF VAL(Agr.Mtr) < Mt - 3 OR VAL(Agr.Mtr) > Mt + 6 THEN WinMssg "Low level password": GOTO 112
	CASE IS > "6"
114      EntryN 28, 63, 6, 2, e, b, Agr.Mrt: IF e THEN 150 ELSE IF b THEN 112
		 IF AgrRate THEN 114
116      EntryN 29, 63, 6, 2, e, b, Agr.Wrt: IF e THEN 150 ELSE IF b THEN 114
		 IF AgrRate THEN 116
	END SELECT
	Lm = Mkt(1) + Mkt(2) + Mkt(3) + Mkt(4)
	Lt = 100 * VAL(Agr.Mtr) * VAL(Agr.Mrt)
	IF Lt < Lm THEN
	   WinMssg "Warning agreement total less than FMV"
	   IF Agr.Cmb = "    " AND Inv.Ovr = "O" THEN
		  ELSE
			 IF Pas.Lvl < "8" THEN 112
	   END IF
	   FOR x = 1 TO 4
	   IF Mkt(x) > 0 THEN
		  Mkt(x) = Lt * (Mkt(x) / Lm)
		  COLOR 15, 4: LOCATE 13 + 2 * x, 69: PRINT USING "####.##"; Mkt(x) / 100;
	   END IF
	   NEXT
	END IF
	Agr.Sta = "A"
	AgrStat 34, 24
119 EntryU 34, 61, 19, e, b, Agr.Acc:   IF e THEN 150 ELSE IF b THEN 112
120 EntryU 35, 61, 1, e, b, Agr.Frq:    IF e THEN 150 ELSE IF b THEN 119
	IF Pas.Lvl < "8" THEN
	   IF INSTR("DWM", Agr.Frq) = 0 THEN WinMssg "Daily, Weekly or Monthly": GOTO 120
	ELSE
	   IF INSTR("CDWM", Agr.Frq) = 0 THEN WinMssg "Cash, Daily, Weekly or Monthly": GOTO 120
	END IF

	IF Agr.Frq = "C" THEN
	   Agr.Due = Agr.Dat                               'set due date
	   Agr.Nxt = Agr.Due
	   Agr.Wav = "N"                                   'set waiver to no
	   AgrFreq 35, 61                                  'display cash sale
	   qPrint Agr.Wav, 35, 77, 0, 1                    'display no waiver
	   qPrint Agr.Due, 36, 61, 0, 1                    'display due date
	   qPrint "Sales Price", 37, 49, 0, 1              'display sale field
	   Statlin "Cash Sale Agreement"                   'display status
	   PyoDate Agr.Pod                                 'compute payout date
	   PyoAmnt Lr                                      'compute payout amount
	   LOCATE 37, 61: PRINT USING "####.##"; Lr / 100; 'display total
	   Picture 37, 61, 7, Agr.Pay                      '
121 EntryN 37, 61, 7, 2, e, b, Agr.Pay: IF e THEN 150 ELSE IF b THEN 120
	ELSE

	Statlin "Issue New Agreement"
	qPrint "Rental Rate", 37, 49, 0, 1
	AgrFreq 35, 61
122 EntryU 35, 77, 1, e, b, Agr.Wav:    IF e THEN 150 ELSE IF b THEN 120
	IF INSTR("YN", Agr.Wav) = 0 THEN WinMssg "[Y]es or [N]o": GOTO 122
126 EntryD 36, 61, e, b, Agr.Due:       IF e THEN 150 ELSE IF b THEN 122
	IF Agr.Due < "0" THEN WinMssg "Must have first due date": GOTO 126
	Lm = 100 * VAL(Agr.Mrt)                            'monthly rate
	Lw = 100 * VAL(Agr.Wrt)                            'weekly rate
	Ld = Lw / 7                                        'daily rate
	nm = 0                                             'zero number of
	nw = 0                                             'months, weeks
	nd = 0                                             'and days
	d = DateDif(Agr.Dat, Agr.Due)                      'elapsed days
	m = VAL(LEFT$(Agr.Dat, 2))                         'month number
	y = VAL(RIGHT$(Agr.Dat, 4))                        '
	IF d < 4 THEN WinMssg "Least is 4 days": GOTO 126  '
	IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29     'elapsed number
	WHILE d >= Ndm(m)                                  'repeat until
		IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29 'elapsed number
		d = d - Ndm(m)                                 'of whole months
		nm = nm + 1                                    'are determined
		m = m + 1: IF m > 12 THEN m = 1: y = y + 1     '
	WEND                                               '
	nw = d \ 7                                         'number of weeks
	nd = d MOD 7                                       'number of days
	PyoDate Agr.Pod                                    'compute payout date
	Agr.Nxt = Agr.Dat
	END IF                                             '-----------------

	SELECT CASE Agr.Frq
		   CASE "M"                                    'prorate
			 Lr = nm * Lm + (7 * nw + nd) * Lm / Ndm(m)'monthly
		   CASE "W"                                    'or
			 Lr = nm * Lm + nw * Lw + nd * Ld          'weekly
		   CASE "C"                                    'Cash Sale
			 Lr = 100 * VAL(Agr.Pay)                   'equal to manual entry
	END SELECT                                         '

	Lt = Lr * VAL(Sto.Stx) / 100                       'sales tax
	IF Lr > 999999 THEN ERROR 203                      'maximum rent exceeded
	IF Agr.Wav = "Y" THEN
	   IF Lm > 8000 THEN Lv = Lm * nm * .05 ELSE Lv = nm * 400
	   IF Lw > 2000 THEN Lv = Lv + (Lw * nw * .05) ELSE Lv = Lv + (nw * 100)
	   IF Ld > 2000 THEN Lv = Lv + (Ld * nd * .05) ELSE Lv = Lv + (nd / .07)
	END IF
	LOCATE 36, 72: PRINT LEFT$(DayWeek$(Agr.Due), 3);
	LOCATE 37, 61: PRINT USING "####.##"; Lr / 100;
	LOCATE 38, 61: PRINT USING "####.##"; Lt / 100;
	LOCATE 39, 61: PRINT USING "####.##"; Lv / 100;
	qPrint Agr.Ref, 35, 24, 0, 1
	qPrint Agr.Nxt, 36, 24, 0, 1
	qPrint Agr.Pod, 37, 24, 0, 1
	qPrint Agr.Dta, 38, 24, 0, 1
	qPrint TraStat(Agr.Act), 39, 24, 0, 1
	LOCATE 40, 24: PRINT USING "####d"; Agr.Eza; Agr.Fre; Agr.Ndl;
	LOCATE 41, 24: PRINT USING "####m####w####d"; Agr.Npm; Agr.Npw; Agr.Npd;

128 EntryN 40, 63, 5, 2, e, b, Agr.Pro: IF e THEN 150
	IF b THEN IF Agr.Frq = "C" THEN 120 ELSE 126
	Ld = 100 * VAL(Agr.Pro)
	LOCATE 41, 61: PRINT USING "####.##"; (Lr + Lt + Lv + Ld) / 100;
	IF Confirm(1, "SAVE agreement") = 0 THEN 128

	FOR x = 1 TO 4
	IF FindSer(Ser(x)) THEN
	   GET #10, Inx.Num, Inv
	   IF Agr.Frq = "C" THEN   '  Cash sales
		  Inv.Sta = 9          '  status cash Sale
		  Inv.Act = "AK"       '  stamp activity type
		  InvActv 9            '  add to cash sales
	   ELSE                    '-----------------------
		  Inv.Sta = 2          '  status on rent
		  Inv.Act = "AN"       '  stamp activity type
		  InvActv 2            '  add to on-rent
	   END IF                  '-----------------------
	   InsActv 1, 1            '  add 1 to outs
	   Inv.Dta = Agr.Dat       '  stamp activity date
	   Inv.Tim = Inv.Tim + 1   '  time on rent
	   Inv.Agr = Agr.Num       '  agreement number
	   PUT #10, Inx.Num, Inv
	END IF
	NEXT

	Picture 37, 61, 7, Agr.Pay
	Agr.It1 = Ser(1): Agr.Lo1 = "": Agr.Ds1 = Dis(1): Agr.Fm1 = Mkt(1)
	Agr.It2 = Ser(2): Agr.Lo2 = "": Agr.Ds2 = Dis(2): Agr.Fm2 = Mkt(2)
	Agr.It3 = Ser(3): Agr.Lo3 = "": Agr.Ds3 = Dis(3): Agr.Fm3 = Mkt(3)
	Agr.It4 = Ser(4): Agr.Lo4 = "": Agr.Ds4 = Dis(4): Agr.Fm4 = Mkt(4)
	n = 1 + LOF(16) / LEN(Agr)

	IF Agr.Frq = "C" THEN      '  Cash sales
	   Agr.Act = "AK"          '  agreement activity
	   Tra.Typ = "AK"          '  transaction type
	ELSE                       '-----------------------
	   Agr.Act = "AN"          '  New Agreement
	   Tra.Typ = "AN"          '  agreement activity
	END IF                     '  transaction type

	AgrNum = AgrNum + 1
	Agr.Dta = DATE$
	Agr.Ref = "      "
	Agx.Cus = Agr.Cus
	Agx.Agr = Agr.Num
	Agx.Num = n
	PUT #16, n, Agr
	PUT #17, n, Agx

	GET #16, n, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Mgr = Pas.Ini
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	GET #22, 1, Act
	FOR z = 1 TO 5
	Act.Agn(z) = Act.Agn(z) + 1
	NEXT
	PUT #22, 1, Act

	PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Agr.Num; Cus.Nam; Agr.Rte; Agr.It1; Agr.It2; Agr.It3; Agr.It4
	SortA = 1
	AgrPrnt
	AgrSort
150
END SUB

SUB mProAgr ()
	Statlin "Promissory Note"
	p$ = Pas.Ini
	h$ = Pas.Lvl
900 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 900
	Yes = 1: IF WinCust(h$) THEN 900
	AgrDisp
	IF Agr.Sta <> "A" THEN WinMssg "Not an active agreement!": GOTO 900
	IF Agr.Frq = "C" THEN WinMssg "Not allowed, Cash Sale Agreement!": GOTO 900
	IF ChgLoan = 0 THEN WinMssg "Do permenant swap first": GOTO 900
	IF Confirm(1, "issue PROMISSARY note") = 0 THEN 900

	Agr.Dta = DATE$
	Agr.Act = "AP"
	Agr.Sta = "N"
	AgrStat 34, 24
	Agr.Ref = RIGHT$(STR$(10000001 + AgrNum), 6)
	PUT #16, Agx.Num, Agr
	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AP"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; Agr.It1; Agr.It2; Agr.It3; Agr.It4
	
	Agr.Sta = "A"
	Agr.Dat = DATE$
	Agr.Mgr = p$
	Agr.Ref = Agr.Num
	Agr.Pay = "   0.00"
	Agr.Pro = " 5.00"
	Agr.Num = RIGHT$(STR$(10000001 + AgrNum), 6)
	
	AgrDisp

912 EntryN 28, 57, 3, 0, e, b, Agr.Mtr: IF e THEN 912 ELSE IF b THEN 912
	IF AgrRate THEN 912
914 EntryN 28, 63, 6, 2, e, b, Agr.Mrt: IF e THEN 914 ELSE IF b THEN 912
	IF AgrRate THEN 914
916 EntryN 29, 63, 6, 2, e, b, Agr.Wrt: IF e THEN 916 ELSE IF b THEN 914
	IF AgrRate THEN 916
	Lm = Mkt(1) + Mkt(2) + Mkt(3) + Mkt(4)
	Lt = 100 * VAL(Agr.Mtr) * VAL(Agr.Mrt)
	IF Lt < Lm THEN
	   WinMssg "Warning agreement total less than FMV"
	   FOR x = 1 TO 4
	   IF Mkt(x) > 0 THEN
		  Mkt(x) = Lt * (Mkt(x) / Lm)
		  COLOR 15, 4: LOCATE 13 + 2 * x, 69
		  PRINT USING "####.##"; Mkt(x) / 100;
	   END IF
	   NEXT
	END IF
917 EntryU 34, 61, 19, e, b, Agr.Acc: IF e THEN 917 ELSE IF b THEN 916
918 EntryU 35, 61, 1, e, b, Agr.Frq:  IF e THEN 918 ELSE IF b THEN 917
	IF INSTR("WM", Agr.Frq) = 0 THEN WinMssg "Monthly or Weekly": GOTO 918
	AgrFreq 35, 61
920 EntryU 35, 77, 1, e, b, Agr.Wav:  IF e THEN 920 ELSE IF b THEN 918
	IF INSTR("YN", Agr.Wav) = 0 THEN WinMssg "[Y]es or [N]o": GOTO 920
924 EntryD 36, 61, e, b, Agr.Due:     IF e THEN 924 ELSE IF b THEN 920
	IF Agr.Due < "0" THEN WinMssg "Must have first due date": GOTO 924

	Lm = 100 * VAL(Agr.Mrt)                            'monthly rate
	Lw = 100 * VAL(Agr.Wrt)                            'weekly rate
	Ld = Lw / 7                                        'daily rate
	nm = 0                                             'zero number of
	nw = 0                                             'months, weeks
	nd = 0                                             'and days
	d = DateDif(Agr.Dat, Agr.Due)                      'elapsed days
	m = VAL(LEFT$(Agr.Dat, 2))                         'month number
	y = VAL(RIGHT$(Agr.Dat, 4))                        '

	IF d < 4 THEN WinMssg "Least is 4 days": GOTO 924
	IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29     'elapsed number
	WHILE d >= Ndm(m)                                  'repeat until
		IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29 'elapsed number
		d = d - Ndm(m)                                 'of whole months
		nm = nm + 1                                    'are determined
		m = m + 1: IF m > 12 THEN m = 1: y = y + 1     '
	WEND                                               '
	nw = d \ 7                                         'number of weeks
	nd = d MOD 7                                       'number of days
	IF Agr.Frq = "M" THEN                              'prorate
	   Lr = nm * Lm + (7 * nw + nd) * Lm / Ndm(m)      'monthly
	ELSE                                               'or
	   Lr = nm * Lm + nw * Lw + nd * Ld                'weekly
	END IF                                             '
	Lt = Lr * VAL(Sto.Stx) / 100
	IF Lr > 99999 THEN ERROR 203                       'maximum rent exceeded
	IF Agr.Wav = "Y" THEN
	   IF Lm > 8000 THEN Lv = Lm * nm * .05 ELSE Lv = nm * 400
	   IF Lw > 2000 THEN Lv = Lv + (Lw * nw * .05) ELSE Lv = Lv + (nw * 100)
	   IF Ld > 2000 THEN Lv = Lv + (Ld * nd * .05) ELSE Lv = Lv + (nd / .07)
	END IF
	LOCATE 36, 72: PRINT LEFT$(DayWeek$(Agr.Due), 3);
	LOCATE 37, 61: PRINT USING "####.##"; Lr / 100;
	LOCATE 38, 61: PRINT USING "####.##"; Lt / 100;
	LOCATE 39, 61: PRINT USING "####.##"; Lv / 100;

926 EntryN 40, 63, 5, 2, e, b, Agr.Pro: IF e THEN 926 ELSE IF b THEN 924
	Ld = 100 * VAL(Agr.Pro)
	LOCATE 41, 61: PRINT USING "####.##"; (Lr + Lt + Lv + Ld) / 100;

	IF Confirm(1, "save PROMISSARY note") = 0 THEN 926
	FOR x = 1 TO 4
	IF FindSer(Ser(x)) THEN
	   GET #10, Inx.Num, Inv
	   Inv.Act = "AP"
	   Inv.Dta = Agr.Dat
	   Inv.Tim = Inv.Tim + 1
	   Inv.Agr = Agr.Num
	   PUT #10, Inx.Num, Inv
	END IF
	NEXT
	Picture 37, 61, 7, Agr.Pay

	n = 1 + LOF(16) \ LEN(Agr)
	AgrNum = AgrNum + 1
	Agr.Eza = 0
	PyoDate f$
	Agr.Pod = f$
	Agr.Nxt = Agr.Dat
	Agr.Npd = 0
	Agr.Npw = 0
	Agr.Npm = 0
	Agr.Rct = 0
	Agr.Ndl = 0
	Agr.Fre = 0
	Agr.Fm1 = Mkt(1)
	Agr.Fm2 = Mkt(2)
	Agr.Fm3 = Mkt(3)
	Agr.Fm4 = Mkt(4)
	Agr.Mri = 0: Agr.Yri = 0: Agr.Tri = 0'Rental income
	Agr.Mds = 0: Agr.Yds = 0: Agr.Tds = 0'Discount
	Agr.Mtx = 0: Agr.Ytx = 0: Agr.Ttx = 0'Sales tax
	Agr.Mlc = 0: Agr.Ylc = 0: Agr.Tlc = 0'Late fees
	Agr.Mtc = 0: Agr.Ytc = 0: Agr.Ttc = 0'Trip charge
	Agr.Mot = 0: Agr.Yot = 0: Agr.Tot = 0'Other charge
	Agr.Min = 0: Agr.Yin = 0: Agr.Tin = 0'Waiver
	Agr.Mrf = 0: Agr.Yrf = 0: Agr.Trf = 0'Referral
	Agr.Dta = DATE$
	Agr.Act = "AN"

	Agx.Cus = Agr.Cus
	Agx.Agr = Agr.Num
	Agx.Num = n
	PUT #16, n, Agr
	PUT #17, n, Agx

	GET #16, n, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AN"
	Tra.Mgr = Pas.Ini
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	GET #22, 1, Act
	FOR z = 1 TO 5
	Act.Agp(z) = Act.Agp(z) + 1
	NEXT
	PUT #22, 1, Act

	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; Agr.It1; Agr.It2; Agr.It3; Agr.It4
	
	SortA = 1
	AgrProm
	AgrSort
END SUB

DEFINT L
SUB mPrtAgr ()
	Statlin "Reprint an Agreement"
200 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 200
	Yes = 1: IF WinCust(Pas.Lvl) THEN 200
	AgrDisp
	AgrPrnt
	GOTO 200
END SUB

DEFLNG L
SUB mReiAgr ()
	p$ = Pas.Ini
	h$ = Pas.Lvl
	Statlin "Reinstate Agreement"
750 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 750
	Yes = 1: IF WinCust(h$) THEN 750
	Agr.Lo1 = ""
	Agr.Lo2 = ""
	Agr.Lo3 = ""
	Agr.Lo4 = ""
	AgrDisp
	IF INSTR("VTC", Agr.Sta) = 0 THEN WinMssg "Wrong agreement status": GOTO 750
	l = DateDif(Agr.Dta, DATE$)
	SELECT CASE l
	CASE 0 TO 30
	CASE 31 TO 90
	   IF Percent < 60 THEN
		  WinMssg "Paid less than 60%"
		  IF h$ < "7" THEN 750
	   END IF
	CASE 91 TO 180
	   IF Percent < 80 THEN
		  WinMssg "Paid less than 80%"
		  IF h$ < "7" THEN 750
	   END IF
	CASE ELSE
	   WinMssg "Should not reinstate": GOTO 750
	END SELECT
	IF DateDif(Agr.Nxt, Agr.Dta) > 0 THEN
	   WinMssg "Must collect past due amount"
	   IF h$ < "7" THEN 750
	END IF
	PCOPY 0, 2: CALL MhDisplayByName(0, 0, "Reinstate", e)
	SOUND 550, 2: KeyPres f$: PCOPY 2, 0

133 FOR n = 1 TO 4
	IF Ser(n) > "          " THEN
	   IF FindSer(Ser(n)) = 0 THEN ERROR 205
	   GET #10, Inx.Num, Inv
	   IF Inv.Sta > 0 THEN
		  r = 2 * n + 14
		  LOCATE r - 1, 13: COLOR 7, 4: PRINT "Replace";
134       EntryU r, 2, 10, e, b, Lon(n): IF e THEN 750 ELSE IF b THEN 133
		  IF FindSer(Lon(n)) = 0 THEN IF WinItem(2, n) THEN 134
		  IF InvRtrv THEN WinMssg "Code not found": GOTO 134
		  IF InvVrfy THEN 134
		  COLOR 7, 4
		  LOCATE r, 2:  PRINT Inv.Ser;
		  LOCATE r, 24: PRINT Inv.Cod;
		  LOCATE r, 29: PRINT Cod.Des;
		  LOCATE r, 54: PRINT Inv.Mdl;
		  LOCATE r, 65: PRINT USING "###"; Inv.Tim
		  IF Cob(n) <> Cod.Grp THEN WinMssg "Not same group!": GOTO 134
		  IF ABS(Inv.Tdr - Cst(n)) / Cst(n) > .2 THEN
			 WinMssg "Not of same value"
			 IF h$ < "7" THEN 134
		  END IF
		  Lon(n) = Inv.Ser
	   END IF
	END IF
	NEXT
	
	IF Confirm(1, "REINSTATE agreement") = 0 THEN 750

	IF Lon(1) > "          " THEN Agr.It1 = Lon(1) ELSE Agr.It1 = Ser(1)
	IF Lon(2) > "          " THEN Agr.It2 = Lon(2) ELSE Agr.It2 = Ser(2)
	IF Lon(3) > "          " THEN Agr.It3 = Lon(3) ELSE Agr.It3 = Ser(3)
	IF Lon(4) > "          " THEN Agr.It4 = Lon(4) ELSE Agr.It4 = Ser(4)

	Ser(1) = Agr.It1
	Ser(2) = Agr.It2
	Ser(3) = Agr.It3
	Ser(4) = Agr.It4

	d = DateDif(Agr.Nxt, DATE$): IF d < 0 THEN d = 0
	Agr.Sta = "A"
	Agr.Act = "AR"
	Agr.Dta = DATE$
	Agr.Eza = Agr.Eza + d
	Agr.Nxt = DayMath$(Agr.Nxt, d)
	Agr.Pod = DayMath$(Agr.Pod, d)

	FOR x = 1 TO 4
	IF FindSer(Ser(x)) THEN
	   GET #10, Inx.Num, Inv
	   InvActv 2
	   InsActv 1, 1                 '  increment BOR activity
	   Inv.Sta = 2
	   Inv.Act = "AR"
	   Inv.Dta = DATE$
	   Inv.Agr = Agr.Num
	   PUT #10, Inx.Num, Inv
	END IF
	NEXT

	PUT #16, Agx.Num, Agr
	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AR"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	
	GET #22, 1, Act
	FOR z = 1 TO 5
	Act.Agr(z) = Act.Agr(z) + 1
	NEXT
	PUT #22, 1, Act

	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; Agr.It1; Agr.It2; Agr.It3; Agr.It4
END SUB

DEFINT L
SUB mRteAgr ()
	Statlin "Reroute Agreement"
	p$ = Pas.Ini
	h$ = Pas.Lvl
610 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 610
	Yes = 1: IF WinCust(h$) THEN 610
	AgrDisp
	f$ = Agr.Rte
	IF Agr.Sta <> "A" THEN WinMssg "Not an active agreement!": GOTO 610
	IF DateDif(Agr.Nxt, DATE$) > 3 THEN WinMssg "More than 3 days past due": IF h$ < "8" GOTO 610
611 EntryU 5, 42, 1, e, b, Agr.Rte:  IF e THEN EXIT SUB ELSE IF b THEN 610
	IF INSTR("123", Agr.Rte) = 0 THEN WinMssg "Use route 1,2,or 3": GOTO 611
	IF Confirm(1, "REROUTE agreement") = 0 THEN 611
	PUT #16, Agx.Num, Agr
	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AM"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; f$; Agr.Rte
END SUB

SUB mSwpAgr ()
	Statlin "Permenant Swap Out"
	p$ = Pas.Ini
	h$ = Pas.Lvl
800 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 800
	Yes = 1: IF WinCust(h$) THEN 800
	AgrDisp
	IF Agr.Sta <> "A" THEN WinMssg "Not an active agreement!": GOTO 800
	IF Agr.Frq = "C" THEN WinMssg "Not allowed, Cash Sale Agreement!": GOTO 800
807 IF Agr.Cmb = "    " THEN
808    x = InvItmL(1): IF x = 1 THEN EXIT SUB ELSE IF x = 2 THEN 800
	   IF Lon(1) > "          " AND Cob(1) <> Cod.Grp THEN
		  IF h$ > "7" THEN WinMssg "Warning: group is not " + Cob(1)
		  IF h$ < "8" THEN WinMssg "Group must be " + Cob(1): GOTO 808
	   END IF
	ELSE
	   r = 1
809    x = InvItmL(r): IF x = 1 THEN EXIT SUB
	   IF x = 2 THEN r = r - 1: IF r < 1 THEN 800 ELSE 809
	   IF Lon(r) > "          " AND Cob(r) <> Cod.Grp THEN
		  IF h$ > "7" THEN WinMssg "Warning: group is not " + Cob(r)
		  IF h$ < "8" THEN WinMssg "Group must be " + Cob(r): GOTO 809
	   END IF
	   IF r < 4 AND Ser(r + 1) > "          " THEN r = r + 1: GOTO 809
	END IF
	IF ChgLoan THEN 807
	IF Confirm(1, "SWAP merchandise") = 0 THEN 807
	FOR x = 1 TO 4
	   IF FindSer(Lon(x)) THEN              '-------------------loaner
		  GET #10, Inx.Num, Inv             ' get loaner record
		  a$ = a$ + Inv.Ser + " for "       ' transaction log
		  InvActv 2                         ' increment activity
		  Inv.Sta = 2                       ' change loaner to ON-RENT
		  Inv.Act = "AS"                    ' stamp activity type
		  Inv.Dta = DATE$                   ' stamp date
		  Inv.Agr = Agr.Num                 ' stamp agreement number
		  PUT #10, Inx.Num, Inv             ' save items record
		  IF FindSer(Ser(x)) THEN           '-------------------original
			 GET #10, Inx.Num, Inv          ' get original item
			 a$ = a$ + Inv.Ser              ' transaction log
			 SELECT CASE Inv.Sta            '
			 CASE 2                         'if ON-RENT then
				a$ = a$ + " idle "          ' transaction log
				InvActv 0                   '
				Inv.Sta = 0                 ' change to IDLE
			 CASE 3                         'if ON-RENT (AS) then
				a$ = a$ + " idle "          ' transaction log
				InvActv 0                   '
				Inv.Sta = 0                 ' change to IDLE
			 CASE 4                         'if ON-RENT (IS) then
				a$ = a$ + " in-service "    ' transaction log
				InvActv 1                   '
				Inv.Sta = 1                 ' change to IN-SERVICE
			 CASE 5                         'if ON-RENT (AR) then
				a$ = a$ + " idle "          ' transaction log
				InvActv 0                   '
				Inv.Sta = 0                 ' change to IDLE
			 CASE ELSE                      '
				WinMssg "Status ERROR"      'status error
			 END SELECT                     '
			 Inv.Act = "AS"                 ' stamp activity type
			 Inv.Dta = DATE$                ' stamp date
			 Inv.Agr = "      "             ' clear agreement number
			 PUT #10, Inx.Num, Inv          ' and save items record
		  END IF
	   END IF
	NEXT
	IF Lon(1) > "          " THEN Agr.It1 = Lon(1): Agr.Lo1 = ""
	IF Lon(2) > "          " THEN Agr.It2 = Lon(2): Agr.Lo2 = ""
	IF Lon(3) > "          " THEN Agr.It3 = Lon(3): Agr.Lo3 = ""
	IF Lon(4) > "          " THEN Agr.It4 = Lon(4): Agr.Lo4 = ""
	AgrDisp
	Agr.Act = "AS"
	Agr.Dta = DATE$
	PUT #16, Agx.Num, Agr
	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AS"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; a$
	AgrPrnt
END SUB

SUB mTerAgr ()
	p$ = Pas.Ini
	u$ = Pas.Nam
	h$ = Pas.Lvl
	Statlin "Terminate Agreement"
400 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 400
	Yes = 1: IF WinCust(h$) THEN 400
	AgrDisp
	IF Agr.Sta <> "A" THEN WinMssg "Not an active agreement!": GOTO 400
	IF Agr.Rct = 0 THEN WinMssg "No terminate, try void.": GOTO 400
	IF Confirm(1, "TERMINATE agreement") = 0 THEN 400

	Agr.Sta = "T"
	Agr.Act = "AT"
	Agr.Dta = DATE$
	AgrStat 34, 24

	FOR x = 1 TO 4
	IF FindSer(Lon(x)) THEN '-------------loaner
	   GET #10, Inx.Num, Inv
	   a$ = a$ + Inv.Ser + " "
	   InvActv 0
	   InsActv 0, 1
	   Inv.Sta = 0
	   Inv.Act = "AT"
	   Inv.Dta = DATE$
	   Inv.Agr = "      "
	   PUT #10, Inx.Num, Inv
	   IF FindSer(Ser(x)) THEN '----------original
		  GET #10, Inx.Num, Inv
		  SELECT CASE Inv.Sta
		  CASE 3
			 a$ = a$ + Inv.Ser + " "
			 InvActv 0
			 Inv.Sta = 0
			 Inv.Mas = Inv.Mas + m
			 Inv.Yas = Inv.Yas + y
			 Inv.Tas = Inv.Tas + t
		  CASE 4
			 b$ = b$ + Inv.Ser + " "
			 InvActv 1
			 Inv.Sta = 1
		  CASE 5
			 a$ = a$ + Inv.Ser + " "
			 InvActv 0
			 Inv.Sta = 0
			 Inv.Mar = Inv.Mar + m
			 Inv.Yar = Inv.Yar + y
			 Inv.Tar = Inv.Tar + t
		  CASE ELSE
			 WinMssg "Status ERROR"
		  END SELECT
		  Inv.Act = "AT"
		  Inv.Dta = DATE$
		  Inv.Agr = "      "
		  PUT #10, Inx.Num, Inv
	   END IF
	ELSE
	   IF FindSer(Ser(x)) THEN '----------original
		  GET #10, Inx.Num, Inv
		  a$ = a$ + Inv.Ser + " "
		  InvActv 0
		  InsActv 0, 1
		  Inv.Sta = 0
		  Inv.Act = "AT"
		  Inv.Dta = DATE$
		  Inv.Agr = "      "
		  PUT #10, Inx.Num, Inv
	   END IF
	END IF
	NEXT
	
	PUT #16, Agx.Num, Agr
	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AT"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	GET #22, 1, Act
	FOR z = 1 TO 5
	Act.Agt(z) = Act.Agt(z) + 1
	NEXT
	PUT #22, 1, Act

	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; a$; b$

	AgrDetl u$
END SUB

SUB mVoiAgr ()
	Statlin "Void an Agreement"
	p$ = Pas.Ini
	h$ = Pas.Lvl
500 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 500
	Yes = 1: IF WinCust(h$) THEN 500
	AgrDisp
	IF Agr.Sta <> "A" THEN WinMssg "Not an active agreement!": GOTO 500
	IF ChgLoan = 0 THEN WinMssg "Loaner must be replaced first": GOTO 500
	IF Agr.Rct > 0 THEN WinMssg "Agreement with receipts, No void!": GOTO 500
	IF Agr.Dat <> DATE$ AND h$ < "7" THEN WinMssg "Low level password": GOTO 500
	IF Confirm(1, "VOID Agreement") = 0 THEN 500

	Agr.Sta = "V"
	Agr.Act = "AV"
	Agr.Dta = DATE$
	AgrStat 34, 24
	FOR x = 1 TO 4
	IF FindSer(Ser(x)) THEN
	   GET #10, Inx.Num, Inv     'retrive item's record
	   InvActv 0                 'increment idle activity
	   InsActv 1, -1             'subrtact 1 from outs
	   Inv.Sta = 0               'idle inentory item
	   Inv.Act = "AV"            'stamp items activity
	   Inv.Dta = DATE$           'stamp activity date
	   Inv.Tim = Inv.Tim - 1     'decrement times on rent
	   Inv.Agr = "      "        'clear agreement number
	   PUT #10, Inx.Num, Inv     'save item's record
	END IF
	NEXT
	PUT #16, Agx.Num, Agr
	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AV"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	GET #22, 1, Act
	FOR z = 1 TO 5
	Act.Agv(z) = Act.Agv(z) + 1
	NEXT
	PUT #22, 1, Act

	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; Agr.It1; Agr.It2; Agr.It3; Agr.It4
END SUB

SUB mWavAgr ()
	Statlin "Agreement Waiver"
	p$ = Pas.Ini
	h$ = Pas.Lvl
680 EntryS 3, 54, 26, e, b, Cus.Nam
	IF e THEN EXIT SUB ELSE IF b THEN 680
	Yes = 1: IF WinCust(h$) THEN 680
	AgrDisp
	f$ = Agr.Wav
	IF Agr.Sta <> "A" THEN WinMssg "Not an active agreement!": GOTO 680
	IF Agr.Frq = "C" THEN WinMssg "Not allowed, Cash Sale Agreement!": GOTO 680
681 EntryU 35, 77, 1, e, b, Agr.Wav: IF e THEN EXIT SUB ELSE IF b THEN 680
	IF INSTR("YN", Agr.Wav) = 0 THEN WinMssg "Waiver, Yes or No": GOTO 681
	IF Agr.Wav = f$ THEN WinMssg "No change to waiver": GOTO 681
	IF Confirm(1, "accept WAIVER") = 0 THEN 681
	PUT #16, Agx.Num, Agr
	GET #16, Agx.Num, Agl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "AW"
	Tra.Mgr = p$
	Tra.Rec = Agl.Agr
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	IF Agr.Wav = "Y" THEN f$ = "instituted" ELSE f$ = "removed"
	PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Agr.Num; Cus.Nam; Agr.Rte; f$
END SUB

SUB PyoDate (f$)
	 SELECT CASE Agr.Frq
	 CASE "W"
		f$ = DayMath$(Agr.Dat, VAL(Agr.Wtr) * 7)
	 CASE "M"
		f$ = MonMath$(Agr.Dat, VAL(Agr.Mtr))
	 CASE "C"
		f$ = MonMath$(Agr.Dat, VAL(Agr.Mtr))
	 END SELECT
	 f$ = DayMath$(f$, Agr.Eza)
END SUB

DEFSNG A-Y
SUB RatDisp (z)
	COLOR 11, 3
	LOCATE 28, 16: PRINT Rat.Mtr(z);
	LOCATE 28, 22: PRINT Rat.Mrt(z);
	LOCATE 28, 31: PRINT USING "####.##"; VAL(Rat.Mtr(z)) * VAL(Rat.Mrt(z));
	LOCATE 29, 16: PRINT Rat.Wtr(z);
	LOCATE 29, 22: PRINT Rat.Wrt(z);
	LOCATE 29, 31: PRINT USING "####.##"; VAL(Rat.Wtr(z)) * VAL(Rat.Wrt(z));

	LOCATE 28, 57: PRINT USING "###"; Rats(1, z);
	LOCATE 28, 63: PRINT Rat.Mrt(z);
	LOCATE 28, 72: PRINT USING "####.##"; Rats(1, z) * VAL(Rat.Mrt(z));
	LOCATE 29, 57: PRINT USING "###"; Rats(2, z);
	LOCATE 29, 63: PRINT Rat.Wrt(z);
	LOCATE 29, 72: PRINT USING "####.##"; Rats(2, z) * VAL(Rat.Wrt(z));

	Picture 28, 57, 3, Agr.Mtr: Agr.Mrt = Rat.Mrt(z)
	Picture 29, 57, 3, Agr.Wtr: Agr.Wrt = Rat.Wrt(z)
			 Agr.Mst = Agr.Mtr: Agr.Msr = Agr.Mrt
			 Agr.Wst = Agr.Wtr: Agr.Wsr = Agr.Wrt
END SUB

DEFSNG Z
SUB RctLoan ()
	IF Response("Resp-07") THEN EXIT SUB
	IF PrinterReady%(2) = 0 THEN ERROR 209
	WIDTH #24, 100
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
	PRINT #24,
	PRINT #24, USING "  &   &  & & & &"; Agr.Num; Sto.Num; DATE$; SPACE$(25); Pas.Ini; "      LOANER RECEIPT"
	PRINT #24,
	PRINT #24, TAB(4); "This will acknowledge receipt of the item(s) listed below as loaner item(s)"
	PRINT #24, TAB(4); "which I will retain under the same terms as my original agreement. I hereby"
	PRINT #24, TAB(4); "accept this unit as a temporary replacement for my original item(s), also  "
	PRINT #24, TAB(4); "listed below."
	PRINT #24, CHR$(27); "M";
	PRINT #24,
	PRINT #24, TAB(4); "Loaner Item(s)"; TAB(48); "Original Item(s)"
	FOR r = 1 TO 4
	IF FindSer(Lon(r)) THEN
	   GET #10, Inx.Num, Inv
	   IF FindCod(Inv.Cod) = 0 THEN ERROR 200
	   PRINT #24, USING "   & Serial No &"; RTRIM$(Cod.Des); Inv.Ser;
	END IF
	IF FindSer(Ser(r)) THEN
	   PRINT #24, TAB(48);
	   GET #10, Inx.Num, Inv
	   IF FindCod(Inv.Cod) = 0 THEN ERROR 200
	   PRINT #24, USING "& Serial No &"; RTRIM$(Cod.Des); Inv.Ser
	ELSE
	   PRINT #24,
	END IF
	NEXT
	PRINT #24,
	PRINT #24, CHR$(27); "P";
	PRINT #24, TAB(53); "Customer's Signature"; CHR$(12);
END SUB

