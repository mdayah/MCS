DEFINT A-Z: DEFLNG L
DECLARE FUNCTION InvItems (r)
DECLARE FUNCTION InvFind ()
DECLARE FUNCTION InvRtrv ()

DECLARE SUB HiLite (m, p)
DECLARE SUB InsActv (s, n)
DECLARE SUB InvActv (s)
DECLARE SUB InvShow ()
DECLARE SUB InvInfo ()

DECLARE SUB InvEdit ()
DECLARE SUB DispRat ()
DECLARE SUB DispItm (r)
DECLARE SUB HToServ ()
DECLARE SUB HNewItm ()
DECLARE SUB HTransfi (s$)
DECLARE SUB HTransfo (s$)
DECLARE SUB HTransfm (s$)

REM $INCLUDE: '..\Include\Config.Dec'

DIM SHARED Menu(7, 2) AS INTEGER
DIM SHARED Dt(2, 36)  AS STRING * 10
DIM SHARED td(36)     AS LONG

DEFINT L
SUB DispItm (r)
   '**note****************************************************************
   '* the findprice will reset the warranty for parts and labor on the   *
   '* inventory file equal to the price master.  if the warranty changes *
   '* on the same item then this setting should be removed.  Otherwise   *
   '* it will updated every time DISPITEM followed by save.              *
   '**********************************************************************
	IF FindCod(Inv.Cod) = 0 THEN ERROR 200
'   IF FindPrc(Inv.Dis + Inv.Mdl + Inv.Cod) THEN Inv.Lab = Prc.Lab: Inv.Par = Prc.Par
	qPrint Inv.Ser, r, 3, 0, 1
	qPrint Cod.Des, r, 15, 0, 1
	qPrint Cod.Des, 3, 56, 0, 1
	qPrint Status(Inv.Sta), 4, 56, 0, 1
	qPrint Inv.Dis, 5, 56, 0, 1
	qPrint Inv.Mdl, 6, 56, 0, 1
	qPrint Inv.Ser, 7, 56, 0, 1
	qPrint Cod.Typ, 8, 56, 0, 1
	qPrint Cod.Grp, 9, 56, 0, 1
	qPrint Inv.Bra, 10, 56, 0, 1
	qPrint Inv.Cod, 11, 56, 0, 1
	qPrint Inv.Lab, 12, 56, 0, 1
	qPrint Inv.Par, 13, 56, 0, 1
	qPrint Inv.Dtp, 14, 56, 0, 1
	qPrint Inv.Dtr, 15, 56, 0, 1
	qPrint Inv.Dta, 16, 56, 0, 1
END SUB

DEFLNG T
SUB DispRat ()
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "Rates", e)
   '-----------------------------------------------------------------------
	COLOR 23, 0                                         'load and print
	FOR z = 1 TO 3                                      'standared rates
	LOCATE z + 16, 7                                    '
	PRINT USING " &³&³ &³&"; Rat.Mtr(z); Rat.Mrt(z); Rat.Wtr(z); Rat.Wrt(z);
	NEXT                                                '
   '-----------------------------------------------------------------------
	tt = VAL(Rat.Mtr(1))                                'monthly term
	tr = VAL(Rat.Mrt(1)) * 100                          'monthly rate
	tc = 100 * VAL(Inv.Prc)                             'item's cost
	tm = lc * VAL(Cod.Fmv) - (Inv.Tin / VAL(Cod.Dfm))   'market value
	td = Inv.Tdr \ (DaysAvg * VAL(Cod.Dtr))             'drop months
	ta = tt * (tr - td)                                 'agreement total
	IF tm < tc THEN tm = tc                             'if FMV drops
	IF ta < tm THEN                                     'if < FMV
	   dx = ABS(INT(-(tm / tr / 100)))                  'change drop
	   td = tt - dx                                     'max drop terms
	END IF                                              '
	IF td > .33 * tt THEN td = .33 * tt                 'bellow 33%
	IF Inv.Ovr = "O" THEN td = 0                        'override no drop
	m = td                                              'round drop terms
	w = m * DaysAvg \ 7                                 'weeks to drop
   '-----------------------------------------------------------------------
	COLOR 7, 0                                          'load and print
	FOR z = 1 TO 3                                      'last rate
		IF Rat.Mtr(z) < "  1" THEN EXIT FOR             'droping empty
		Rats(1, z) = VAL(Rat.Mtr(z)) - m                'modified terms
		Rats(2, z) = VAL(Rat.Wtr(z)) - w                'modified terms
		LOCATE z + 16, 32                               '
		PRINT USING " ###³&³ ###³&"; Rats(1, z); Rat.Mrt(z); Rats(2, z); Rat.Wrt(z);
	NEXT
   '-----------------------------------------------------------------------
	KeyPres f$
	PCOPY 1, 0
END SUB

DEFINT T
SUB HiLite (m, p)
	IF p THEN f = 18: b = 14 ELSE f = 10: b = 0
	LOCATE 42, Menu(m, 1)
	f$ = MID$(" Display  Search  Rates  Info  Override  Customer  Modify ", Menu(m, 1), Menu(m, 2))
	COLOR f, b:     PRINT MID$(f$, 1, 2);
	COLOR f + 3, b: PRINT MID$(f$, 3);
END SUB

DEFSNG A-Z
SUB HNewItm ()
	  IF PrinterReady%(1) = 0 THEN ERROR 208
	  LPRINT CHR$(27); "@";
	  LPRINT CHR$(27); "!"; CHR$(0);
	  LPRINT CHR$(14); "       NEW MERCHANDISE RECEIVED"
	  LPRINT
	  LPRINT TAB(52); "Date Received: "; DATE$
	  LPRINT TAB(52); "Purchase Date: "; Inv.Dtp
	  LPRINT
	  LPRINT USING "         Store: &           Distributor: &"; Sto.Nam; Inv.Dis
	  LPRINT USING "   Received by: &               Invoice Number: &"; Pas.Nam; Inv.Pon
	  LPRINT
	  LPRINT
	  LPRINT
	  LPRINT "    Code  Brand         Description          Model Number  Serial Number"
	  LPRINT "    ====  =====  ==========================  ============  ============="
	  LineNum = 12
END SUB

SUB HToServ ()
	IF PrinterReady%(1) = 0 THEN ERROR 208
	LPRINT CHR$(27); "@";
	LPRINT CHR$(27); "!"; CHR$(0);
	LPRINT CHR$(14); "      SEND MERCHANDISE TO SERVICE"
	WIDTH LPRINT 255       'Supress Line Feed
	LPRINT CHR$(15);
	LPRINT TAB(56); Sto.Nam
	LPRINT
	LPRINT Ctr.Nam; TAB(105); "Date sent: "; DATE$
	LPRINT Ctr.Str; TAB(105); "  Manager: "; Pas.Nam
	LPRINT Ctr.Cty; " "; Ctr.Sta; " "; Ctr.Zip
	LPRINT
	LPRINT
	LPRINT "                                                                                   Purchase   ====WARRANTY=====  ==UNDER WARRANTY=="
	LPRINT " Code  Brand         Description           Model N§    Serial N§       Status        Date       Parts   Labor      Parts    Labor  "
	LPRINT " ====  =====  =========================  ===========  ===========  ============== ==========  =================  =================="
	LineNum = 12
END SUB

SUB HTransfi (s$)
	IF PrinterReady%(1) = 0 THEN ERROR 208
	LPRINT CHR$(27); "@";
	LPRINT CHR$(27); "!"; CHR$(0);
	LPRINT CHR$(14); TAB(15); "TRANSFER-IN"
	LPRINT
	LPRINT
	LPRINT
		   GET #6, VAL(StoNum), Sto
	LPRINT USING "   RECEIVING STORE: &       DATE: &"; Sto.Nam; DATE$
		   GET #6, VAL(s$), Sto
	LPRINT USING "TRANSFERRING STORE: &    MANAGER: &"; Sto.Nam; Pas.Nam
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT "Code Brand       Description         Model Num  Serial Num       Errors      "
	LPRINT "==== ===== ========================= ========== ========== =================="
	LineNum = 12
END SUB

SUB HTransfm (s$)
	IF PrinterReady%(1) = 0 THEN ERROR 208
	LPRINT CHR$(27); "@";
	LPRINT CHR$(27); "!"; CHR$(0);
	LPRINT CHR$(10); TAB(15); "TRANSFER-MANUAL"
	LPRINT
	LPRINT
	LPRINT
		   GET #6, VAL(StoNum), Sto
	LPRINT USING "   RECEIVING STORE: &       DATE: &"; Sto.Nam; DATE$
		   GET #6, VAL(s$), Sto
	LPRINT USING "TRANSFERRING STORE: &    MANAGER: &"; Sto.Nam; Pas.Nam
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT "    Code  Brand         Description          Model Number  Serial Number"
	LPRINT "    ====  =====  ==========================  ============  ============="
	LineNum = 12
END SUB

SUB HTransfo (s$)
	IF PrinterReady%(1) = 0 THEN ERROR 208
	LPRINT CHR$(27); "@";
	LPRINT CHR$(27); "!"; CHR$(0);
	LPRINT CHR$(14); TAB(15); "TRANSFER - OUT"
	LPRINT
	LPRINT
	LPRINT
		   GET #6, VAL(StoNum), Sto
	LPRINT USING "TRANSFERRING STORE: &       DATE: &"; Sto.Nam; DATE$
		   GET #6, VAL(s$), Sto
	LPRINT USING "   RECEIVING STORE: &    MANAGER: &"; Sto.Nam; Pas.Nam
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT "    Code  Brand         Description          Model Number  Serial Number"
	LPRINT "    ====  =====  ==========================  ============  ============="
	LineNum = 12
END SUB

DEFINT A-Z
SUB InvEdit ()
	s$ = LTRIM$(STR$(Inv.Sta))
	sb$ = Inv.Ser
	Picture 31, 54, 2, t$
230 EntryU 4, 49, 4, e, b, Inv.Cod:     IF e THEN EXIT SUB ELSE IF b THEN 230
	IF FindCod(Inv.Cod) = 0 THEN WinMssg "No such code": GOTO 230

	Inv.Rat = Cod.Rat
	Inv.Ovr = " "
	qPrint Cod.Typ, 4, 39, 0, 1
	qPrint Cod.Grp, 4, 45, 0, 1
	qPrint Cod.Cmb, 4, 56, 0, 1
	qPrint Cod.Des, 6, 53, 0, 1
	qPrint Cod.Fmv, 23, 58, 0, 1
	qPrint Cod.Dtr, 24, 58, 0, 1
	qPrint Cod.Dfm, 25, 58, 0, 1
	qPrint Inv.Rat, 30, 53, 0, 1
	qPrint Inv.Ovr, 33, 55, 0, 1

231 EntryU 4, 61, 3, e, b, Inv.Bra:     IF e THEN EXIT SUB ELSE IF b THEN 230
232 EntryN 4, 66, 2, 0, e, b, s$:       IF e THEN EXIT SUB ELSE IF b THEN 231
	IF s$ < " 0" OR s$ > "15" THEN WinMssg "Status: 0-15": GOTO 232
	Inv.Sta = VAL(s$)
	qPrint Status(Inv.Sta), 4, 66, 0, 1
233 EntryU 8, 53, 10, e, b, Inv.Mdl:    IF e THEN EXIT SUB ELSE IF b THEN 232
234 EntryU 9, 53, 10, e, b, Inv.Ser:    IF e THEN EXIT SUB ELSE IF b THEN 233
235 EntryU 10, 53, 10, e, b, Inv.Dis:   IF e THEN EXIT SUB ELSE IF b THEN 234
236 EntryD 12, 53, e, b, Inv.Dtp:       IF e THEN EXIT SUB ELSE IF b THEN 235
237 EntryD 13, 53, e, b, Inv.Dtr:       IF e THEN EXIT SUB ELSE IF b THEN 236
238 EntryD 14, 53, e, b, Inv.Dta:       IF e THEN EXIT SUB ELSE IF b THEN 237
239 EntryU 14, 64, 2, e, b, Inv.Act:    IF e THEN EXIT SUB ELSE IF b THEN 238
	qPrint TraStat(Inv.Act), 14, 64, 0, 1
240 EntryN 16, 53, 2, 0, e, b, Inv.Tfr: IF e THEN EXIT SUB ELSE IF b THEN 239
	IF Inv.Tfr > " 0" THEN GET #6, VAL(Inv.Tfr), Sto: qPrint Sto.Nam, 16, 53, 0, 1 ELSE qPrint SPACE$(26), 16, 53, 0, 1
241 EntryN 17, 53, 2, 0, e, b, Inv.Tto: IF e THEN EXIT SUB ELSE IF b THEN 240
	IF Inv.Tto > " 0" THEN GET #6, VAL(Inv.Tto), Sto: qPrint Sto.Nam, 17, 53, 0, 1 ELSE qPrint SPACE$(26), 17, 53, 0, 1
242 EntryN 18, 53, 2, 0, e, b, Inv.Ctr: IF e THEN EXIT SUB ELSE IF b THEN 240
	IF Inv.Ctr > " 0" THEN GET #7, VAL(Inv.Ctr), Ctr: qPrint Ctr.Nam, 18, 53, 0, 1 ELSE qPrint SPACE$(26), 18, 53, 0, 1
243 EntryS 20, 53, 6, e, b, Inv.Agr:    IF e THEN EXIT SUB ELSE IF b THEN 242
	IF Inv.Agr > "      " THEN
	   IF FindAgr(Inv.Agr) THEN
		  GET #13, VAL(Agx.Cus), Cus
		  qPrint Cus.Nam, 21, 53, 0, 1
	   ELSE
		  WinMssg "Agreement does not exist": GOTO 243
	   END IF
	ELSE
	   qPrint SPACE$(26), 21, 53, 0, 1
	END IF
244 EntryN 26, 55, 7, 2, e, b, Inv.Prc: IF e THEN EXIT SUB ELSE IF b THEN 243
245 EntryU 28, 53, 10, e, b, Inv.Pon:   IF e THEN EXIT SUB ELSE IF b THEN 244
246 EntryN 31, 54, 2, 0, e, b, t$:      IF e THEN EXIT SUB ELSE IF b THEN 245
	Inv.Tim = VAL(t$)
247 EntryN 35, 53, 3, 0, e, b, Inv.Par: IF e THEN EXIT SUB ELSE IF b THEN 246
248 EntryN 36, 53, 3, 0, e, b, Inv.Lab: IF e THEN EXIT SUB ELSE IF b THEN 247

	qPrint MID$(Inv.Rmk, 1, 23), 38, 53, 0, 1
	qPrint MID$(Inv.Rmk, 24, 23), 39, 53, 0, 1
	qPrint MID$(Inv.Rmk, 47, 23), 40, 53, 0, 1

	IF Confirm(1, "MODIFY item") = 0 THEN 248

	IF FindAgr(Inv.Agr) THEN
	   GET #16, Agx.Num, Agr
	   IF sb$ = Agr.It1 THEN Agr.It1 = Inv.Ser
	   IF sb$ = Agr.It2 THEN Agr.It2 = Inv.Ser
	   IF sb$ = Agr.It3 THEN Agr.It3 = Inv.Ser
	   IF sb$ = Agr.It4 THEN Agr.It4 = Inv.Ser
	   PUT #16, Agx.Num, Agr
	END IF

	Inx.Typ = Cod.Typ
	Inx.Grp = Cod.Grp
	Inx.Cod = Inv.Cod
	Inx.Mdl = Inv.Mdl
	Inx.Ser = Inv.Ser
	Inx.Dtp = VAL(RIGHT$(Inv.Dtp, 4) + MID$(Inv.Dtp, 4, 2) + LEFT$(Inv.Dtp, 2))
	Inx.Num = Inx.Num
	PUT #10, Inx.Num, Inv
	PUT #11, LOC(11), Inx

	GET #10, Inx.Num, Inl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "MU"
	Tra.Mgr = Pas.Ini
	Tra.Rec = Inl.Inv
	Tra.Prc = 0
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	
	PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; Inv.Ser; Cod.Des; Inv.Mdl; Inv.Dis; Inv.Sta

	CLOSE
	SHELL "Sortf ..\DataFile\Inventor.Inx ..\DataFile\Inventor.Tmp /L33 /+1,27 /Q"
	SHELL "Sortf ..\DataFile\Inventor.Inx ..\DataFile\Inventor.Ser /L33 /+18,10 /Q"
	KILL "..\DataFile\Inventor.Inx"
	NAME "..\DataFile\Inventor.Tmp" AS "..\DataFile\Inventor.Inx"
	OpenFiles
END SUB

FUNCTION InvFind ()
	b = 1: t = LOF(11) \ LEN(Inx)
	f$ = Inx.Typ + Inx.Grp + Inx.Cod + Inx.Mdl + Inx.Ser
	DO UNTIL (t < b)
	   m = (t + b) \ 2: GET #11, m, Inx
	   IF f$ = Inx.Typ + Inx.Grp + Inx.Cod + Inx.Mdl + Inx.Ser THEN InvFind = m: EXIT DO
	   IF f$ > Inx.Typ + Inx.Grp + Inx.Cod + Inx.Mdl + Inx.Ser THEN b = m + 1 ELSE t = m - 1
	LOOP
END FUNCTION

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
	IF FindSer(Inv.Ser) = 0 THEN IF WinItem(0, 0) THEN 99
END FUNCTION

DEFSNG A-Z
SUB InvShow ()
	COLOR 11, 3
	c$ = SPACE$(26)
	f$ = SPACE$(26)
	t$ = SPACE$(26)
	s$ = SPACE$(26)
	IF InvRtrv THEN WinMssg "Code not found"
	IF FindAgr(Inv.Agr) THEN GET #13, VAL(Agx.Cus), Cus: c$ = Cus.Nam
	IF Inv.Tfr > " 0" THEN GET #6, VAL(Inv.Tfr), Sto: f$ = Sto.Nam
	IF Inv.Tto > " 0" THEN GET #6, VAL(Inv.Tto), Sto: t$ = Sto.Nam
	IF Inv.Ctr > " 0" THEN GET #7, VAL(Inv.Ctr), Ctr: s$ = Ctr.Nam
	qPrint Inx.Typ, 4, 39, 0, 1
	qPrint Inx.Grp, 4, 45, 0, 1
	qPrint Inv.Cod, 4, 49, 0, 1
	qPrint Cod.Cmb, 4, 56, 0, 1
	qPrint Inv.Bra, 4, 61, 0, 1
	qPrint Status(Inv.Sta), 4, 66, 0, 1
	qPrint Cod.Des, 6, 53, 0, 1
	qPrint Inv.Mdl, 8, 53, 0, 1
	qPrint Inv.Ser, 9, 53, 0, 1
	qPrint Inv.Dis, 10, 53, 0, 1
	qPrint Inv.Dtp, 12, 53, 0, 1
	qPrint Inv.Dtr, 13, 53, 0, 1
	qPrint Inv.Dta + " " + TraStat(Inv.Act), 14, 53, 0, 1

	qPrint f$, 16, 53, 0, 1
	qPrint t$, 17, 53, 0, 1
	qPrint s$, 18, 53, 0, 1
	qPrint Inv.Agr, 20, 53, 0, 1
	qPrint c$, 21, 53, 0, 1

	IF Pas.Lvl > "7" THEN
	   qPrint Cod.Fmv, 23, 58, 0, 1
	   qPrint Cod.Dtr, 24, 58, 0, 1
	   qPrint Cod.Dfm, 25, 58, 0, 1
	   qPrint Inv.Prc, 26, 55, 0, 1
	END IF

	qPrint Inv.Pon, 28, 53, 0, 1

	qPrint Inv.Rat, 30, 53, 0, 1
	LOCATE 31, 53: PRINT USING "###"; Inv.Tim;
	qPrint Inv.Ovr, 33, 55, 0, 1
	
	qPrint Inv.Par, 35, 53, 0, 1
	qPrint Inv.Lab, 36, 53, 0, 1
	qPrint MID$(Inv.Rmk, 1, 23), 38, 53, 0, 1
	qPrint MID$(Inv.Rmk, 24, 23), 39, 53, 0, 1
	qPrint MID$(Inv.Rmk, 47, 23), 40, 53, 0, 1
END SUB

DEFINT A-Z
SUB mChkInv ()
	Statlin "Checking Inventory Codes"
	FOR x = 1 TO LOF(10) \ LEN(Inv)
		GET #10, x, Inv
		IF FindCod(Inv.Cod) = 0 THEN WinMssg "Code " + Inv.Cod + " missing for " + Inv.Ser
	NEXT
END SUB

SUB mGivInv ()
	CALL MhDisplayByName(0, 0, "GiveAway", e)
	Statlin "Giveaway Charge-off"

	qPrint DATE$, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	Inv.Ser = ""
170 x = InvItems(16): IF x = 1 THEN 179 ELSE IF x = 2 THEN 170
	GET #10, Inx.Num, Inv
	DispItm 16
	qPrint Inv.Rmk, 19, 11, 0, 1
	IF Inv.Sta <> 0 THEN WinMssg "Item must be IDLE": GOTO 170
171 EntryU 20, 11, 69, e, b, r$: IF e THEN 179 ELSE IF b THEN 170
	IF Confirm(1, "GIVEAWAY item") = 0 THEN 170
	InvActv 14
	Inv.Sta = 14
	Inv.Act = "MG"
	Inv.Dta = DATE$
	Inv.Rmk = r$
	PUT #10, Inx.Num, Inv
	
	GET #10, Inx.Num, Inl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "MG"
	Tra.Mgr = Pas.Ini
	Tra.Rec = Inl.Inv
	Tra.Prc = 0
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; Inv.Ser; Cod.Des; Inv.Mdl; Inv.Dis
179
END SUB

DEFLNG L
SUB mMasInv ()
	CALL MhDisplayByName(0, 0, "Mast-Inv", e)
	
	Menu(1, 1) = 1:  Menu(1, 2) = 9
	Menu(2, 1) = 10: Menu(2, 2) = 8
	Menu(3, 1) = 18: Menu(3, 2) = 7
	Menu(4, 1) = 25: Menu(4, 2) = 6
	Menu(5, 1) = 31: Menu(5, 2) = 10
	Menu(6, 1) = 41: Menu(6, 2) = 10
	Menu(7, 1) = 51: Menu(7, 2) = 8

	b = 1: t = LOF(11) \ LEN(Inx): GOSUB InvDsp
	r = 3: n = 1: m = 1: HiLite m, 1
200 GET #11, n, Inx: GET #10, Inx.Num, Inv
	COLOR 15, 1: LOCATE r, 3: PRINT USING "&³&³&³&"; Inx.Typ + Inx.Grp; Inx.Cod; Inx.Mdl; Inx.Ser;
	KeyPres n$
	IF n$ = CHR$(0) + "P" OR n$ = CHR$(0) + "H" THEN
	   COLOR 16 + Inv.Sta, 7
	   LOCATE , 3:  PRINT Inx.Typ + Inx.Grp;
	   LOCATE , 7:  PRINT Inx.Cod;
	   LOCATE , 12: PRINT Inx.Mdl;
	   LOCATE , 23: PRINT Inx.Ser;
	END IF
	IF n$ = CHR$(13) THEN n$ = LTRIM$(STR$(m))
	SELECT CASE n$
	CASE CHR$(27) 'Esc
		GET #6, VAL(StoNum), Sto
		EXIT SUB
	CASE CHR$(0) + "M" 'right
		HiLite m, 0
		m = m + 1: IF m > 7 THEN m = 1
		HiLite m, 1
	CASE CHR$(0) + "K" 'left
		HiLite m, 0
		m = m - 1: IF m < 1 THEN m = 7
		HiLite m, 1
	CASE CHR$(0) + "P" 'down
		IF n + 1 > t THEN 200 ELSE n = n + 1
		IF r + 1 > 40 THEN CALL Scroll(3, 3, 32, 40, 1) ELSE r = r + 1
	CASE CHR$(0) + "H" 'Up
		IF n - 1 < 1 THEN 200 ELSE n = n - 1
		IF r - 1 < 3 THEN CALL BkScroll(3, 3, 32, 40, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
		b = n - r - 35: IF b < 1 THEN b = 1
		GOSUB InvDsp
		n = x - 40 + r
	CASE CHR$(0) + "Q" 'PgDn
		b = 41 - r + n
		IF b > t - 37 THEN b = t - 37: n = b + r - 3 ELSE n = n + 38
		GOSUB InvDsp
	CASE CHR$(0) + "G" 'Home
		b = 1: GOSUB InvDsp
		n = r - 2
	CASE CHR$(0) + "O" 'End
		b = t - 37: GOSUB InvDsp
		n = t + r - 40
	CASE "D", "1"
		HiLite m, 0
		m = 1: HiLite m, 1
		InvShow
	CASE "S", "2"
		HiLite m, 0
		m = 2: HiLite m, 1
228     EntryU 9, 53, 10, e, s, Inv.Ser: IF e THEN 200 ELSE IF s THEN 228
		IF FindSer(Inv.Ser) THEN
		   GET #10, Inx.Num, Inv
		   InvShow
		   n = InvFind: b = n - r + 3
		   IF b < 1 THEN b = 1: r = n + 2 ELSE IF b > t - 37 THEN b = t - 37: r = n - b + 3
		   GOSUB InvDsp
		END IF
	CASE "R", "3"
		HiLite m, 0
		m = 3: HiLite m, 1
		InvShow
		DispRat
	CASE "I", "4"
		HiLite m, 0
		m = 4: HiLite m, 1
		InvShow
		IF Pas.Lvl = "9" THEN InvInfo
	CASE "O", "5"
		HiLite m, 0
		m = 5: HiLite m, 1
		InvShow
		IF Inv.Sta <> 0 THEN
		   WinMssg "Item is not idle"
		ELSE
		   IF Pas.Lvl > "6" THEN
			  WinMssg "Original rate code: " + Cod.Rat
229           EntryN 30, 53, 3, 0, e, b, Inv.Rat: IF e THEN 200 ELSE IF b THEN 229
			  IF Inv.Rat < "  1" THEN WinMssg "Rate must not be zero": GOTO 229
			  IF Confirm(1, "change RATE") = 0 THEN 229
			  IF Cod.Rat = Inv.Rat THEN Inv.Ovr = " " ELSE Inv.Ovr = "O"
			  PUT #10, Inx.Num, Inv
			  GET #10, Inx.Num, Inl
			  Tra.Dat = DATE$
			  Tra.Tim = TIME$
			  Tra.Typ = "MC"
			  Tra.Mgr = Pas.Ini
			  Tra.Rec = Inl.Inv
			  Tra.Prc = 0
			  PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
			  PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; Inv.Ser; Cod.Des; Inv.Mdl; Inv.Dis; Inv.Rat
		   END IF
		END IF
	CASE "C", "6"
		HiLite m, 0
		m = 6: HiLite m, 1
		InvShow
		IF Inv.Agr > "000000" THEN
		   Tra.Typ = "CM"
		   Cnx.Num = VAL(Cus.Num)
		   CusEdit Pas.Lvl
		ELSE
		   WinMssg "No agreement - no customer"
		END IF
	CASE "M", "7"
		HiLite m, 0
		m = 7: HiLite m, 1
		IF Pas.Lvl > "7" THEN
		   InvShow
		   InvEdit
		   IF FindSer(Inv.Ser) THEN
			  GET #10, Inx.Num, Inv
			  InvShow
			  n = InvFind: b = n - r + 3
			  IF b < 1 THEN b = 1: r = n + 2 ELSE IF b > t - 37 THEN b = t - 37: r = n - b + 3
			  GOSUB InvDsp
		   END IF
		ELSE
		   WinMssg "Low level password"
		END IF
	CASE ELSE
	END SELECT
	GOTO 200
InvDsp:
	COLOR 25, 7: LOCATE 3
	FOR x = b TO t
	GET #11, x, Inx: : GET #10, Inx.Num, Inv
	COLOR 16 + Inv.Sta, 7:
	LOCATE , 3:  PRINT Inx.Typ + Inx.Grp;
	LOCATE , 7:  PRINT Inx.Cod;
	LOCATE , 12: PRINT Inx.Mdl;
	LOCATE , 23: PRINT Inx.Ser
	IF CSRLIN > 40 THEN EXIT FOR
	NEXT
	RETURN
END SUB

DEFINT L
SUB mNewInv ()
	CALL MhDisplayByName(0, 0, "NewItem", e)
	Statlin "Receive New Merchandise"
	
	qPrint DATE$, 5, 2, 0, 1
	qPrint DATE$, 9, 18, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
110 Yes = 0: IF WinPric THEN 110
	d$ = Prc.Dis
	qPrint d$, 8, 18, 0, 1
112 EntryU 10, 18, 10, e, b, i$: IF e THEN 119 ELSE IF b THEN 110
	IF i$ < "!" THEN WinMssg "Must have Number": GOTO 112
	IF Response("Resp-01") THEN 119
	Inv.Dis = d$
	Inv.Pon = i$
	Inv.Dtp = DATE$
	HNewItm
113 RecNum = 1 + (LOF(10) \ LEN(Inv)): GET #10, RecNum, Inv
	Inv.Cod = Prc.Cod
	Inv.Mdl = Prc.Mdl
	Inv.Dis = d$
	Inv.Pon = i$
	Inv.Dtp = DATE$
114 Yes = 1: IF WinPric THEN 118
	Inv.Cod = Prc.Cod
	Inv.Mdl = Prc.Mdl
	Inv.Prc = Prc.New
	Inv.Par = Prc.Par
	Inv.Lab = Prc.Lab
	Inv.Dtr = DATE$
	Inv.Dta = DATE$
	Inv.Act = "MN"
	DispItm 14
	Inv.Rat = Cod.Rat
115 EntryU 14, 3, 10, e, b, Inv.Ser: IF e THEN 118 ELSE IF b THEN 114
	IF Inv.Ser < "!" THEN WinMssg "No! No! No!": GOTO 115
	IF FindSer(Inv.Ser) THEN WinMssg "Duplicate serial N§": GOTO 115
116 EntryU 16, 15, 3, e, b, Inv.Bra: IF e THEN 118 ELSE IF b THEN 115
	IF Inv.Bra < "!" THEN WinMssg "I need a Brand Code": GOTO 116

	IF Confirm(1, "accept NEW item") THEN
	   Inx.Typ = Cod.Typ
	   Inx.Grp = Cod.Grp
	   Inx.Cod = Inv.Cod
	   Inx.Mdl = Inv.Mdl
	   Inx.Ser = Inv.Ser
	   Inx.Dtp = VAL(RIGHT$(Inv.Dtp, 4) + MID$(Inv.Dtp, 4, 2) + LEFT$(Inv.Dtp, 2))
	   Inx.Num = RecNum
	   PUT #10, RecNum, Inv
	   PUT #11, RecNum, Inx

	   GET #10, RecNum, Inl
	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Typ = "MN"
	   Tra.Mgr = Pas.Ini
	   Tra.Rec = Inl.Inv
	   Tra.Prc = 0
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	   GET #22, 1, Act
	   FOR z = 1 TO 5
	   Act.New(z) = Act.New(z) + 1
	   Act.Inn(z) = Act.Inn(z) + 100 * VAL(Inv.Prc)
	   NEXT
	   PUT #22, 1, Act

	   PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; Inv.Ser; Cod.Des; Inv.Mdl; Inv.Dis

	   LineNum = LineNum + 1: IF LineNum > 60 THEN HNewItm
	   LPRINT USING "    &   &   &    &    &"; Inv.Cod; Inv.Bra; Cod.Des; Inv.Mdl; Inv.Ser
	   CLOSE
	   SHELL "Sortf ..\DataFile\Inventor.Inx ..\DataFile\Inventor.Tmp /L33 /+1,27 /Q"
	   SHELL "Sortf ..\DataFile\Inventor.Inx ..\DataFile\Inventor.Ser /L33 /+18,10 /Q"
	   KILL "..\DataFile\Inventor.Inx"
	   NAME "..\DataFile\Inventor.Tmp" AS "..\DataFile\Inventor.Inx"
	   OpenFiles
	   GOTO 113
	END IF
	GOTO 114
118 IF Response("Resp-02") = 0 THEN 114
	FOR x = LineNum TO 59: LPRINT : NEXT
	LPRINT TAB(50); "Signature: ___________________"; CHR$(12);
119
END SUB

SUB mScrInv ()
	CALL MhDisplayByName(0, 0, "Scrap", e)
	Statlin "Scrap Charge-off"
	
	qPrint DATE$, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	Inv.Ser = ""
160 x = InvItems(16): IF x = 1 THEN 169 ELSE IF x = 2 THEN 160
	GET #10, Inx.Num, Inv
	DispItm 16
	qPrint Inv.Rmk, 19, 11, 0, 1
	IF Inv.Sta <> 0 THEN WinMssg "Item must be IDLE": GOTO 160
161 EntryU 20, 11, 69, e, b, r$: IF e THEN 169 ELSE IF b THEN 160
	IF Confirm(1, "SCRAP item") = 0 THEN 161
	   InvActv 13
	   Inv.Sta = 13
	   Inv.Act = "MP"
	   Inv.Dta = DATE$
	   Inv.Rmk = r$
	   PUT #10, Inx.Num, Inv
	   
	   GET #10, Inx.Num, Inl
	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Typ = "MP"
	   Tra.Mgr = Pas.Ini
	   Tra.Rec = Inl.Inv
	   Tra.Prc = 0
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra
	   PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; Inv.Ser; Cod.Des; Inv.Mdl; Inv.Dis
169
END SUB

SUB mSriInv ()
140 CALL MhDisplayByName(0, 0, "Service-IN", e)
	Statlin "Return from Service"
	
	qPrint DATE$, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1

'    IF Confirm(1, "begin SERVICE-IN") THEN
'       OPEN "A:\Service.In" FOR RANDOM AS #25
'       IF LOF(25) > 0 THEN
'          INPUT #25, Sto.Num, Inv.Bra, Inv.Mdl, Inv.Ser, Inv.Lab, Inv.Par, Inv.Dtp, Inv.Rmk, Inv.Sta, Cus.Num, Cus.Nam, Cus.Add, Cus.Str, Cus.Cty, Cus.Sta, Cus.Zip, Cus.Tel, Cus.Wor


	p$ = "": r$ = "": Inv.Ser = ""
141 x = InvItems(14): IF x = 1 THEN 149 ELSE IF x = 2 THEN 141
	GET #10, Inx.Num, Inv
	DispItm 14
	qPrint Inv.Rmk, 19, 11, 0, 1
	IF Inv.Sta <> 1 AND Inv.Sta <> 4 THEN WinMssg "Must be IN SERVICE!": GOTO 141
	GET #7, VAL(Inv.Ctr), Ctr
	
	qPrint Ctr.Nam, 8, 11, 0, 1
	qPrint Ctr.Str, 9, 11, 0, 1
	qPrint Ctr.Cty + " " + Ctr.Sta + " " + Ctr.Zip, 10, 11, 0, 1
142 EntryN 16, 15, 6, 2, e, b, p$: IF e THEN 149 ELSE IF b THEN 141
	IF p$ < "  0.00" THEN WinMssg "NO Repair charges!": GOTO 142
143 EntryU 20, 11, 69, e, b, r$: IF e THEN 149 ELSE IF b THEN 142
	IF r$ < "!" THEN WinMssg "Joey needs to know kind of repairs.": GOTO 143
	IF Confirm(1, "return item from SERVICE") THEN
	   IF Inv.Sta = 1 THEN
		  InvActv 0
		  Inv.Sta = 0
	   ELSE
		  InvActv 5
		  Inv.Sta = 5
	   END IF
	   Inv.Ctr = "  "
	   Inv.Rmk = r$
	   Inv.Act = "MI"
	   Inv.Dta = DATE$
	   Inv.Mre = Inv.Mre + (VAL(p$) * 100)
	   Inv.Yre = Inv.Yre + (VAL(p$) * 100)
	   Inv.Tre = Inv.Tre + (VAL(p$) * 100)
	   PUT #10, Inx.Num, Inv

	   GET #10, Inx.Num, Inl
	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Typ = "MI"
	   Tra.Mgr = Pas.Ini
	   Tra.Rec = Inl.Inv
	   Tra.Prc = 100 * VAL(p$)
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	   PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; Inv.Ser; Cod.Des; Inv.Mdl; Inv.Dis; Ctr.Nam

	   GET #22, 1, Act
	   FOR z = 1 TO 5
	   Act.Sri(z) = Act.Sri(z) + 1
	   NEXT
	   PUT #22, 1, Act

	   GOTO 140
	END IF
	GOTO 143
149
END SUB

SUB mSroInv ()
130 CALL MhDisplayByName(0, 0, "Service-Out", e)
	Statlin "Send to Service"
	
	qPrint DATE$, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	IF Response("Resp-15") THEN EXIT SUB

	OPEN "A:\Service.Out" FOR RANDOM AS #25
	IF LOF(25) > 0 THEN WinMssg "Disk contains a service file": EXIT SUB
	CLOSE 25: OPEN "A:\Service.Out" FOR OUTPUT AS #25
	
	
131 EntryN 8, 11, 2, 0, e, b, Ctr.Num: IF e THEN EXIT SUB ELSE IF b THEN 131
	IF Ctr.Num < " 1" THEN IF WinCntr THEN 131
	Inv.Ctr = Ctr.Num
	GET #7, VAL(Inv.Ctr), Ctr
	
	qPrint Ctr.Num, 8, 11, 0, 1
	qPrint Ctr.Nam, 9, 11, 0, 1
	qPrint Ctr.Str, 11, 11, 0, 1
	qPrint Ctr.Cty + " " + Ctr.Sta + " " + Ctr.Zip, 12, 11, 0, 1
	HToServ
	
132 x = InvItems(16): IF x = 1 THEN 139 ELSE IF x = 2 THEN 132
	GET #10, Inx.Num, Inv
	DispItm 16
	qPrint Inv.Rmk, 19, 11, 0, 1
	IF Inv.Sta <> 0 AND Inv.Sta <> 3 THEN WinMssg "Not IDLE or AWAITING SERVICE": GOTO 132
133 EntryU 20, 11, 69, e, b, r$: IF e THEN 139 ELSE IF b THEN 132
	IF r$ < "!" THEN WinMssg "Joey needs to know kind of repairs.": GOTO 133
	IF Confirm(1, "send item to SERVICE") THEN

	   IF Inv.Sta = 0 THEN
		  InvActv 1
		  Inv.Sta = 1
	   ELSE
		  InvActv 4
		  Inv.Sta = 4
	   END IF
	   Inv.Rmk = r$
	   Inv.Act = "MO"
	   Inv.Dta = DATE$
	   Inv.Ctr = Ctr.Num
	   PUT #10, Inx.Num, Inv

	   GET #10, Inx.Num, Inl
	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Typ = "MO"
	   Tra.Mgr = Pas.Ini
	   Tra.Rec = Inl.Inv
	   Tra.Prc = 0
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	   PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; Inv.Ser; Cod.Des; Inv.Mdl; Inv.Dis; Ctr.Nam

	   IF FindAgr(Inv.Agr) THEN GET #13, VAL(Agx.Cus), Cus
	   WRITE #25, Sto.Num, Inv.Bra, Inv.Mdl, Inv.Ser, Inv.Lab, Inv.Par, Inv.Dtp, Inv.Rmk, Inv.Sta, Cus.Num, Cus.Nam, Cus.Add, Cus.Str, Cus.Cty, Cus.Sta, Cus.Zip, Cus.Tel, Cus.Wor
	   
	   GET #22, 1, Act
	   FOR z = 1 TO 5
	   Act.Sro(z) = Act.Sro(z) + 1
	   NEXT
	   PUT #22, 1, Act

	   LineNum = LineNum + 2: IF LineNum > 60 THEN HToServ
	   d = DateDif&(Inv.Dtp, DATE$)
	   p = VAL(Inv.Par) - d: IF p < 0 THEN p = 0
	   l = VAL(Inv.Lab) - d: IF l < 0 THEN l = 0
	   LPRINT USING " &  "; Inv.Cod; Inv.Bra; Cod.Des; Inv.Mdl; Inv.Ser; Status(Inv.Sta); Inv.Dtp;
	   LPRINT USING "   &   "; Inv.Par; Inv.Lab;
	   LPRINT USING "   ####   "; p; l
	   LPRINT TAB(59); "Comments: "; Inv.Rmk
	   Inv.Ser = ""
	   GOTO 132
	END IF
	GOTO 133
139 IF Response("Resp-16") = 0 THEN 132
'   CLOSE #25
	FOR x = LineNum TO 59: LPRINT : NEXT
	LPRINT "SERVICE MAN: ____________________________________                            MANAGER: ___________________________________"
	LPRINT CHR$(12);
END SUB

SUB mSrsInv ()
150 CALL MhDisplayByName(0, 0, "Service-In", e)
	Statlin "Site Service"
	
	qPrint DATE$, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	p$ = "": r$ = "": Inv.Ser = ""
151 EntryN 8, 10, 2, 0, e, b, Ctr.Num: IF e THEN 159 ELSE IF b THEN 151
	IF Ctr.Num < " 1" THEN IF WinCntr THEN 151
	Inv.Ctr = Ctr.Num
	GET #7, VAL(Inv.Ctr), Ctr
	
	qPrint Ctr.Num, 8, 10, 0, 1
	qPrint Ctr.Nam, 8, 13, 0, 1
	qPrint Ctr.Str, 9, 13, 0, 1
	qPrint Ctr.Cty + " " + Ctr.Sta + " " + Ctr.Zip, 10, 13, 0, 1
152 x = InvItems(14): IF x = 1 THEN 159 ELSE IF x = 2 THEN 151
	GET #10, Inx.Num, Inv
	DispItm 14
	qPrint Inv.Rmk, 19, 11, 0, 1
	SELECT CASE Inv.Sta
	   CASE 0, 2, 3, 6, 7, 8
	   CASE ELSE: WinMssg "Item status not 0, 2, 3, 6, 7, 8.": GOTO 152
	END SELECT
153 EntryN 16, 15, 6, 2, e, b, p$: IF e THEN 159 ELSE IF b THEN 152
	IF p$ < "  0.00" THEN WinMssg "NO Repair charges!": GOTO 153
154 EntryU 20, 11, 69, e, b, r$: IF e THEN 159 ELSE IF b THEN 153
	IF r$ < "!" THEN WinMssg "Joey needs to know kind of repairs.": GOTO 154
	IF Confirm(1, "record on site SERVICE") THEN
	   IF Inv.Sta = 3 THEN Inv.Sta = 5: InvActv 5
	   Inv.Rmk = r$
	   Inv.Act = "MS"
	   Inv.Dta = DATE$
	   Inv.Mre = Inv.Mre + (VAL(p$) * 100)
	   Inv.Yre = Inv.Yre + (VAL(p$) * 100)
	   Inv.Tre = Inv.Tre + (VAL(p$) * 100)
	   PUT #10, Inx.Num, Inv

	   GET #10, Inx.Num, Inl
	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Typ = "MS"
	   Tra.Mgr = Pas.Ini
	   Tra.Rec = Inl.Inv
	   Tra.Prc = 100 * VAL(p$)
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	   PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; Inv.Ser; Cod.Des; Inv.Mdl; Inv.Dis; Ctr.Nam
	   GOTO 150
	END IF
	GOTO 154
159
END SUB

SUB mTriInv ()
	CALL MhDisplayByName(0, 0, "Transfr-I", e)
	Statlin "Transfer In"
   
	qPrint DATE$, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	IF Confirm(1, "begin TRANSFER-IN") THEN
	   OPEN "A:\Transfer.Mas" FOR RANDOM AS #25 LEN = LEN(Trf)
	   IF LOF(25) \ LEN(Trf) > 0 THEN
		  GET #25, 1, Trf
		  s$ = MID$(Trf.Rec, 94, 2)
		  GET #6, VAL(s$), Sto
		  IF Trf.Sto = StoNum THEN
			 qPrint Sto.Num, 8, 13, 0, 1
			 qPrint Sto.Nam, 9, 13, 0, 1
			 qPrint Sto.Str, 11, 13, 0, 1
			 qPrint Sto.Cty + " " + Sto.Sta + " " + Sto.Zip, 12, 13, 0, 1
			 HTransfi s$
			 FOR x = 1 TO LOF(25) \ LEN(Trf)
				 GET #25, x, Trf
				 m$ = "": d = 0
				 n = FindSer(MID$(Trf.Rec, 30, 10))
				 c = FindCod(MID$(Trf.Rec, 7, 4))
				 IF n > 0 THEN
					GET #10, Inx.Num, Inv
					IF Inv.Sta <> 15 THEN n = 0: d = 1
				 END IF
				 IF n = 0 THEN n = 1 + (LOF(10) \ LEN(Inv)): Inx.Num = n
				 
				 Inl.Inv = Trf.Rec
				 PUT #10, Inx.Num, Inl
				 GET #10, Inx.Num, Inv

				 qPrint Inv.Ser, 16, 3, 0, 1
				 qPrint Cod.Des, 16, 15, 0, 1
				 qPrint Cod.Des, 3, 56, 0, 1
				 qPrint Status(Inv.Sta), 4, 56, 0, 1
				 qPrint Inv.Dis, 5, 56, 0, 1
				 qPrint Inv.Mdl, 6, 56, 0, 1
				 qPrint Inv.Ser, 7, 56, 0, 1
				 qPrint Cod.Typ, 8, 56, 0, 1
				 qPrint Cod.Grp, 9, 56, 0, 1
				 qPrint Inv.Bra, 10, 56, 0, 1
				 qPrint Inv.Cod, 11, 56, 0, 1
				 qPrint Inv.Lab, 12, 56, 0, 1
				 qPrint Inv.Par, 13, 56, 0, 1
				 qPrint Inv.Dtp, 14, 56, 0, 1
				 qPrint Inv.Dtr, 15, 56, 0, 1
				 qPrint Inv.Dta, 16, 56, 0, 1
				 qPrint Inv.Rmk, 19, 11, 0, 1

				 IF c = 0 THEN WinMssg "Code not in master": m$ = " ***CODE ERROR***"
				 IF d = 1 THEN WinMssg "Duplicate serial number": m$ = " **SERIAL ERROR**": Inv.Ser = "??????????"

				 Inv.Sta = 0
				 Inv.Dtr = DATE$
				 Inv.Dta = DATE$
				 Inv.Act = "MR"
				 Inv.Tto = "  "
				 PUT #10, Inx.Num, Inv

				 Inx.Typ = Cod.Typ
				 Inx.Grp = Cod.Grp
				 Inx.Cod = Inv.Cod
				 Inx.Mdl = Inv.Mdl
				 Inx.Ser = Inv.Ser
				 Inx.Dtp = VAL(RIGHT$(Inv.Dtp, 4) + MID$(Inv.Dtp, 4, 2) + LEFT$(Inv.Dtp, 2))
				 PUT #12, n, Inx

				 GET #10, Inx.Num, Inl
				 Tra.Dat = DATE$
				 Tra.Tim = TIME$
				 Tra.Typ = "MR"
				 Tra.Mgr = Pas.Ini
				 Tra.Rec = Inl.Inv
				 PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

				 PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; MID$(Trf.Rec, 30, 10); Cod.Des; Inv.Mdl; Sto.Nam; m$

				 GET #22, 1, Act
				 FOR z = 1 TO 5
				 Act.Tri(z) = Act.Tri(z) + 1
				 IF Inv.Tim > 0 THEN
					Act.Inu(z) = Act.Inu(z) + (VAL(Inv.Prc) * 100) - Inv.Tde
				 ELSE
					Act.Inn(z) = Act.Inn(z) + (VAL(Inv.Prc) * 100) - Inv.Tde
				 END IF
				 NEXT
				 PUT #22, 1, Act

				 LineNum = LineNum + 1: IF LineNum > 60 THEN HTransfi s$
				 LPRINT USING "&  &  &  & & &"; Inv.Cod; Inv.Bra; Cod.Des; Inv.Mdl; MID$(Trf.Rec, 30, 10); m$

				 WinMssg "Press any key for next item"
				 CLOSE 10, 11, 12
				 SHELL "Sortf ..\DataFile\Inventor.Ser ..\DataFile\Inventor.Tmp /L33 /+18,10 /Q"
				 SHELL "Sortf ..\DataFile\Inventor.Ser ..\DataFile\Inventor.Inx /L33 /+1,27 /Q"
				 KILL "..\DataFile\Inventor.Ser"
				 NAME "..\DataFile\Inventor.Tmp" AS "..\DataFile\Inventor.Ser"
				 OpenFiles
			 NEXT
			 CLOSE #25: KILL "A:Transfer.MAS"
			 FOR x = LineNum TO 58: LPRINT : NEXT
			 LPRINT "DELIVERY MAN: ____________________   TRANSFERRING MANAGER: ____________________"
			 LPRINT
			 LPRINT "                                        RECEIVING MANAGER: ____________________"
			 LPRINT CHR$(12);
		  ELSE
			 GET #6, VAL(Trf.Sto), Sto
			 WinMssg "Destination: " + Sto.Nam
		  END IF
	   ELSE
		  WinMssg "No merchandise to transfer in"
	   END IF
	   GET #6, VAL(StoNum), Sto
	END IF
END SUB

DEFLNG D
SUB mTrmInv ()
	CALL MhDisplayByName(0, 0, "Transfr-M", e)
	Statlin "Manual Transfer In"
	
	qPrint DATE$, 5, 4, 0, 1
	qPrint Pas.Nam, 5, 17, 0, 1
	ERASE Dt, td

180 EntryN 8, 17, 2, 0, e, b, Sto.Num: IF e THEN 189 ELSE IF b THEN 180
	IF Sto.Num < " 1" THEN IF WinStor THEN 180
	GET #6, VAL(Sto.Num), Sto
	s$ = Sto.Num
	
	qPrint Sto.Num, 8, 17, 0, 1
	qPrint Sto.Nam, 9, 17, 0, 1
	qPrint Sto.Str, 10, 17, 0, 1
	qPrint Sto.Cty + " " + Sto.Sta + " " + Sto.Zip, 11, 17, 0, 1
	IF Sto.Num = StoNum THEN WinMssg "Pick a store other than yours!!": GOTO 180
	HTransfm s$
181 EntryU 14, 17, 10, e, b, Inv.Ser:  IF e THEN 188 ELSE IF b THEN 181
	IF Inv.Ser < "!" THEN WinMssg "Serial Number!": GOTO 181
	Rx = FindSer(Inv.Ser)
	IF Rx = 0 THEN
	   Rx = 1 + (LOF(10) \ LEN(Inv))
	   Inx.Num = Rx
	ELSE
	   GET #10, Inx.Num, Inv
	   IF Inv.Sta <> 15 THEN WinMssg "Duplicate serial N§": GOTO 181
	END IF
182 Yes = 0: IF WinPric THEN 181
	IF FindCod(Prc.Cod) = 0 THEN WinMssg "Code not found": GOTO 182
183 EntryU 15, 17, 3, e, b, Inv.Bra:   IF e THEN 188 ELSE IF b THEN 182
	IF Inv.Bra < "!" THEN WinMssg "I need a Brand Code": GOTO 183
184 EntryD 16, 17, e, b, Inv.Dtp:      IF e THEN 188 ELSE IF b THEN 183
	IF Inv.Dtp < "0" THEN WinMssg "Purchase date??": GOTO 184
	IF DateDif(Inv.Dtp, DATE$) < 0 THEN WinMssg "No future date": GOTO 184
	GET #4, VAL(Cod.Rat), Rat
	Inv.Cod = Prc.Cod
	Inv.Sta = 0
	Inv.Mdl = Prc.Mdl
	Inv.Dis = Prc.Dis
	Inv.Prc = Prc.New
	Inv.Par = Prc.Par
	Inv.Lab = Prc.Lab
	Inv.Del = " "
	Inv.Ovr = " "
	Inv.Tfr = s$
	Inv.Tto = "  "
	Inv.Ctr = "  "
	Inv.Agr = "      "
	Inv.Pon = "          "
	Inv.Dtr = DATE$
	Inv.Dta = DATE$
	Inv.Act = "MM"
	Inv.Rat = Cod.Rat
	Inv.Rmk = SPACE$(69)
	Inv.Mdi = 0: Inv.Ydi = 0 'D-Idle
	Inv.Mdr = 0: Inv.Ydr = 0 'D-Rented
	Inv.Mas = 0: Inv.Yas = 0 'D-Awaiting Service
	Inv.Mds = 0: Inv.Yds = 0 'D-In Service
	Inv.Mar = 0: Inv.Yar = 0 'D-Awaiting Return
	Inv.Min = 0: Inv.Yin = 0 'Tin AS LONG'$-Revenues
	Inv.Mde = 0: Inv.Yde = 0 '$-Depreciation
	Inv.Mre = 0: Inv.Yre = 0 '$-Repairs
	 
	qPrint Cod.Typ, 19, 17, 0, 1
	qPrint Cod.Grp, 20, 17, 0, 1
	qPrint Inv.Bra, 21, 17, 0, 1
	qPrint Inv.Cod, 22, 17, 0, 1
	qPrint Status(Inv.Sta), 23, 17, 0, 1
	qPrint Cod.Des, 24, 17, 0, 1
	qPrint Inv.Dis, 25, 17, 0, 1
	qPrint Inv.Mdl, 26, 17, 0, 1
	qPrint Inv.Ser, 27, 17, 0, 1
	qPrint Inv.Lab, 28, 17, 0, 1
	qPrint Inv.Par, 29, 17, 0, 1
	qPrint Inv.Dtp, 30, 17, 0, 1
	qPrint Inv.Dtr, 31, 17, 0, 1
	qPrint Inv.Dta, 32, 17, 0, 1
	qPrint Rat.Wtr(1), 34, 18, 0, 1
	qPrint Rat.Wrt(1), 35, 18, 0, 1

	r = 1
185 EntryD r + 4, 46, e, b, Dt(1, r): IF e THEN 188
	IF b THEN r = r - 1: IF r < 1 THEN 184 ELSE 185
	IF Dt(1, r) < "0" THEN
	   dr = dr - td(r)
	   FOR y = r TO 35
		   Dt(1, y) = Dt(1, y + 1)
		   Dt(2, y) = Dt(2, y + 1)
		   td(y) = td(y + 1)
		   IF Dt(1, y) < "0" THEN EXIT FOR
	   NEXT
	   IF CSRLIN < 39 THEN CALL Scroll(46, r + 4, 78, 39, 1)
	   COLOR 9: LOCATE 39, 46: PRINT "           ³            ³      ";
	   COLOR 22, 3: LOCATE 41, 73: PRINT USING "######"; dr;
	   IF Dt(1, r) > "0" THEN 185 ELSE 187
	ELSE
	   IF DateDif(Inv.Dtp, Dt(1, r)) < 0 THEN BEEP: GOTO 185
	END IF
186 EntryD r + 4, 59, e, b, Dt(2, r): IF e THEN 188
	IF b THEN 185
	IF DateDif(Inv.Dtp, Dt(2, r)) < 0 THEN BEEP:  GOTO 186
	IF DateDif(Dt(1, r), Dt(2, r)) < 0 THEN BEEP: GOTO 186
	td(r) = DateDif(Dt(1, r), Dt(2, r))
	dr = 0: FOR y = 1 TO 35: dr = dr + td(y): NEXT
	LOCATE r + 4, 73: PRINT USING "######"; td(r);
	COLOR 22, 3: LOCATE 41, 73: PRINT USING "######"; dr;
	r = r + 1: IF r < 36 THEN 185

187 Inv.Tdr = dr                                               'days on rent
	Inv.Tin = 85 * dr * VAL(Rat.Wrt(1)) / 7                    'rental income
		 da = DateDif&(Inv.Dtp, DATE$)                         'items age
	Inv.Tdi = da - Inv.Tdr                                     'idle days
		 dp = 100 * da * VAL(Inv.Prc) / (VAL(Cod.Dep) * 30.145)'dep. value
	IF dp > 100 * VAL(Inv.Prc) THEN dp = 100 * VAL(Inv.Prc)    'adjust dep                               'value to
	Inv.Tde = dp
	Inv.Tim = r - 1
	COLOR 5, 7
	LOCATE 37, 18: PRINT USING "#####"; da;
	LOCATE 38, 18: PRINT USING "#####"; Inv.Tdr;
	LOCATE 39, 18: PRINT USING "#####"; Inv.Tdi;
	LOCATE 40, 18: PRINT USING "#####.##"; Inv.Tin / 100;
	LOCATE 41, 18: PRINT USING "#####.##"; Inv.Tde / 100;
	
	IF Inv.Tdi < 0 THEN WinMssg "Negative idle days": GOTO 185
	IF Confirm(1, "accept MANUAL TRANSFER") = 0 THEN 185
	Inx.Typ = Cod.Typ
	Inx.Grp = Cod.Grp
	Inx.Cod = Inv.Cod
	Inx.Mdl = Inv.Mdl
	Inx.Ser = Inv.Ser
	Inx.Dtp = VAL(RIGHT$(Inv.Dtp, 4) + MID$(Inv.Dtp, 4, 2) + LEFT$(Inv.Dtp, 2))
	PUT #10, Inx.Num, Inv
	PUT #12, Rx, Inx

	GET #10, Inx.Num, Inl
	Tra.Dat = DATE$
	Tra.Tim = TIME$
	Tra.Typ = "MM"
	Tra.Mgr = Pas.Ini
	Tra.Rec = Inl.Inv
	Tra.Prc = 0
	PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	GET #22, 1, Act
	FOR z = 1 TO 5
	Act.Tri(z) = Act.Tri(z) + 1
	IF Inv.Tdr > 0 THEN
	   Act.Inu(z) = Act.Inu(z) + (VAL(Inv.Prc) * 100) - Inv.Tde
	ELSE
	   Act.Inn(z) = Act.Inn(z) + (VAL(Inv.Prc) * 100) - Inv.Tde
	END IF
	NEXT
	PUT #22, 1, Act
	
	PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; Inv.Ser; Cod.Des; Inv.Mdl; Inv.Dis; Sto.Nam

	LineNum = LineNum + 1: IF LineNum > 60 THEN HTransfm s$
	LPRINT USING "    &   &   &    &    &"; Inv.Cod; Inv.Bra; Cod.Des; Inv.Mdl; Inv.Ser
	CLOSE
	SHELL "Sortf ..\DataFile\Inventor.Ser ..\DataFile\Inventor.Tmp /L33 /+18,10 /Q"
	SHELL "Sortf ..\DataFile\Inventor.Ser ..\DataFile\Inventor.Inx /L33 /+1,27 /Q"
	KILL "..\DataFile\Inventor.Ser"
	NAME "..\DataFile\Inventor.Tmp" AS "..\DataFile\Inventor.Ser"
	OpenFiles
	CALL MhDisplayByName(0, 0, "Transfr-M", e)
	Statlin "Manual Transfer In"
	COLOR 7, 1
	LOCATE 5, 4:  PRINT DATE$
	LOCATE 5, 17: PRINT Pas.Nam
	COLOR 11, 7
	LOCATE 8, 17:  PRINT Sto.Num;
	LOCATE 9, 17:  PRINT Sto.Nam;
	LOCATE 10, 17: PRINT Sto.Str;
	LOCATE 11, 17: PRINT Sto.Cty; " "; Sto.Sta; " "; Sto.Zip;
	dr = 0
	Inv.Ser = ""
	Inv.Dtp = ""
	Inv.Bra = ""
	ERASE Dt, td
	GOTO 181
188 IF Response("Resp-04") = 0 THEN 185
	FOR x = LineNum TO 59: LPRINT : NEXT
	LPRINT TAB(50); "Signature: ___________________"; CHR$(12);
189 GET #6, VAL(StoNum), Sto
END SUB

DEFINT D
SUB mTroInv ()
	CALL MhDisplayByName(0, 0, "Transfr-O", e)
	Statlin "Transfer Out"
	
	qPrint DATE$, 5, 2, 0, 1
	qPrint Pas.Nam, 5, 13, 0, 1
	Inv.Ser = ""
	IF Response("Resp-03") THEN 129
	OPEN "A:\Transfer.Mas" FOR RANDOM AS #25 LEN = LEN(Trf)
	IF LOF(25) \ LEN(Trf) > 0 THEN WinMssg "Disk contains a transfer file": EXIT SUB
120 EntryN 8, 13, 2, 0, e, b, Sto.Num: IF e THEN 129 ELSE IF b THEN 120
	IF Sto.Num < " 1" THEN IF WinStor THEN 120
	GET #6, VAL(Sto.Num), Sto
	s$ = Sto.Num
	
	qPrint Sto.Num, 8, 13, 0, 1
	qPrint Sto.Nam, 9, 13, 0, 1
	qPrint Sto.Str, 11, 13, 0, 1
	qPrint Sto.Cty + " " + Sto.Sta + " " + Sto.Zip, 12, 13, 0, 1
	IF Sto.Num = StoNum THEN WinMssg "Pick a store other than yours!!": GOTO 120
	HTransfo s$
121 IF InvItems(16) THEN 128
	GET #10, Inx.Num, Inv
	DispItm 16
	qPrint Inv.Rmk, 19, 11, 0, 1
	IF Inv.Sta <> 0 THEN WinMssg "Item is " + Status(Inv.Sta): GOTO 121
	IF Confirm(1, "TRANSFER-OUT") THEN
	   Inv.Sta = 15
	   Inv.Act = "MT"
	   Inv.Dta = DATE$
	   Inv.Tfr = StoNum
	   Inv.Tto = s$
	   PUT #10, Inx.Num, Inv
	   GET #10, Inx.Num, Inl

	   Trf.Dat = DATE$
	   Trf.Sto = s$
	   Trf.Rec = Inl.Inv
	   PUT #25, 1 + LOF(25) \ LEN(Trf), Trf

	   Tra.Dat = DATE$
	   Tra.Tim = TIME$
	   Tra.Typ = "MT"
	   Tra.Mgr = Pas.Ini
	   Tra.Rec = Inl.Inv
	   PUT #9, 1 + LOF(9) \ LEN(Tra), Tra

	   PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; Cod.Cod; Inv.Ser; Cod.Des; Inv.Mdl; Inv.Dis; Sto.Nam

	   GET #22, 1, Act
	   FOR z = 1 TO 5
	   Act.Tro(z) = Act.Tro(z) + 1
	   IF Inv.Tim > 0 THEN
		  Act.Inu(z) = Act.Inu(z) - (VAL(Inv.Prc) * 100) + Inv.Tde
	   ELSE
		  Act.Inn(z) = Act.Inn(z) - (VAL(Inv.Prc) * 100) + Inv.Tde
	   END IF
	   NEXT
	   PUT #22, 1, Act

	   LineNum = LineNum + 1: IF LineNum > 60 THEN HTransfo s$
	   LPRINT USING "    &   &   &    &    &"; Inv.Cod; Inv.Bra; Cod.Des; Inv.Mdl; Inv.Ser
	   Inv.Ser = ""
	END IF
	GOTO 121
128 IF Response("Resp-04") = 0 THEN 121
	CLOSE #25
	FOR x = LineNum TO 58: LPRINT : NEXT
	LPRINT "DELIVERY MAN: ____________________   TRANSFERRING MANAGER: ____________________"
	LPRINT
	LPRINT "                                        RECEIVING MANAGER: ____________________"
	LPRINT CHR$(12);
129 GET #6, VAL(StoNum), Sto
	SLEEP 6
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "Waite", e)
	KeyPres f$
END SUB

SUB mVewInv ()
	CALL MhDisplayByName(0, 0, "Transfr-V", e)
	Statlin "View Transfer File"
	OPEN "A:\Transfer.Mas" FOR RANDOM AS #25 LEN = LEN(Trf)
	IF LOF(25) \ LEN(Trf) > 0 THEN
	   LOCATE 8
	   FOR x = 1 TO LOF(25) \ LEN(Trf)
		   GET #25, x, Trf
		   LOCATE , 11: PRINT MID$(Trf.Rec, 94, 2);
		   LOCATE , 15: PRINT MID$(Trf.Rec, 96, 2);
		   LOCATE , 18: PRINT MID$(Trf.Rec, 1, 1);
		   LOCATE , 19: PRINT MID$(Trf.Rec, 2, 2);
		   LOCATE , 23: PRINT MID$(Trf.Rec, 7, 4);
		   LOCATE , 29: PRINT MID$(Trf.Rec, 11, 3);
		   LOCATE , 34: PRINT MID$(Trf.Rec, 20, 10);
		   LOCATE , 45: PRINT MID$(Trf.Rec, 30, 10);
		   LOCATE , 56: PRINT MID$(Trf.Rec, 40, 10)
	   NEXT
	   KeyPres f$
	ELSE
	   WinMssg "No transfer file"
	END IF
END SUB

