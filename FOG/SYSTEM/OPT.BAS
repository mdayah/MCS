DEFINT A-Z: DEFLNG L
DECLARE SUB Heading ()
DECLARE SUB RctSort ()
DECLARE SUB AgrSort ()
DECLARE SUB PastDue ()
DECLARE SUB TranLog ()

DECLARE SUB mTelRct ()
DECLARE SUB ZeroAct (a)
DECLARE SUB mDueOpt (a)
DECLARE SUB mVewOpt (a)
DECLARE SUB mStsRpt (a)
DECLARE SUB mCshRpt (a)
DECLARE SUB mOrdRpt (a)
DECLARE SUB mPriRpt (a)

DECLARE FUNCTION Percent ()

REM $INCLUDE: '..\Include\Config.Dec'

DEFINT L
SUB InsActv (s, n)
    '+-----------------------+
    '| s = 0 is ins          |
    '| s = 1 is outs         |
    '| s = 2 is paid-outs    |
    '| s = 3 is charge-offs  |
    '| n = ñ1                |
    '+-----------------------+
    GET #22, 1, Act
    FOR z = 1 TO 5
    Act.Bor(s, z) = Act.Bor(s, z) + n
    NEXT
    PUT #22, 1, Act
END SUB

SUB InvActv (s)
    GET #22, 1, Act
    FOR z = 1 TO 5
    Act.Sta(s, z) = Act.Sta(s, z) + 1
    NEXT
    PUT #22, 1, Act
END SUB

DEFSNG A-R, T-Z
SUB mBakOpt (s)
    '$INCLUDE: '..\Include\Terminat.DEC'
    SELECT CASE s
    CASE 1
       Statlin "Backup Data Files"
       IF Confirm(0, "start BACKUP") THEN
          COLOR 2: CLS
          SHELL "pkzip -es -&v -sJuMaNa a:\backup.rto ..\datafile\*.*"
          EndBak = DATE$
          OpenFiles
          Tra.Typ = "OB"
          PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini
          CLOSE
       END IF
    CASE 2
       Statlin "Test Backup Set"
       IF Confirm(0, "test BACKUP set") THEN
          COLOR 2: CLS
          SHELL "pkunzip -t -sJuMaNa a:\backup.rto"
          WinMssg "Press a key to continue"
       END IF
    CASE 3
       Statlin "View Backup Set"
       IF Confirm(0, "view BACKUP set") THEN
          COLOR 2: CLS
          SHELL "pkunzip -vnb -sJuMaNa a:\backup.rto"
          WinMssg "Press a key to continue"
       END IF
    CASE 4
       Statlin "Restore Data Files"
       IF Confirm(0, "start RESTORE") THEN
          COLOR 2: CLS
          SHELL "pkunzip -o -sJuMaNa a:\backup.rto ..\datafile"
         '$INCLUDE: '..\Include\LoadCfg.DEC'
          OpenFiles
          GET #6, VAL(StoNum), Sto
       END IF
    END SELECT
'   CALL Clock(1)
END SUB

DEFSNG S
SUB mComOpt ()
    Statlin "Remote Communications"
    IF Pas.Lvl > "7" THEN
       Tra.Typ = "OC": PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini
      '$INCLUDE: '..\Include\Terminat.DEC'
       CALL PrtSc(1)
       CHDIR "\anyWhere"
       RUN "awREMOTE"
    END IF
END SUB

DEFINT A-Z
SUB mDatOpt ()
    Statlin "Reset System Date"
    CALL MhDisplayByName(0, 0, "Set-Date", e)
    qPrint DATE$, 12, 51, 0, 1: f$ = DATE$
    qPrint TIME$, 14, 51, 0, 1: t$ = TIME$
50  EntryD 13, 51, e, b, f$:    IF b THEN 50 ELSE IF e THEN EXIT SUB
    IF f$ > "0" THEN DATE$ = f$
51  EntryU 15, 51, 8, e, b, t$: IF b THEN 50 ELSE IF e THEN EXIT SUB
    IF t$ > "0" THEN TIME$ = t$
END SUB

DEFLNG L
SUB mDayOpt ()
    Statlin "End of Day Closing"
    d = DateDif&(EndDay, DATE$)
    IF d > 0 THEN
       IF Confirm(1, "close the DAY") THEN
          FOR x = 1 TO LOF(10) \ LEN(Inv)          'test for code errors
          GET #10, x, Inv                          'before closing
          IF FindCod(Inv.Cod) = 0 THEN WinMssg "Code " + Inv.Cod + " missing for " + Inv.Ser: c = 1
          NEXT
          IF c THEN EXIT SUB

         'Petty Cash transactions
          Tra.Typ = "PC"
          FOR x = 1 TO LOF(28) \ LEN(Pet)
              GET #28, x, Pet
              PRINT #26, USING TraType; Tra.Typ; TIME$; p$; Pet.Det; Pet.Amt
          NEXT
          CLOSE 28: KILL "..\DataFile\Expenses.Mas"

          Statlin "Updating Merchandise"
          FOR x = 1 TO LOF(10) \ LEN(Inv)          'DEPRECIATION
          GET #10, x, Inv                          'retrieve inventory
          IF FindCod(Inv.Cod) = 0 THEN ERROR 200   'retrieve code
          SELECT CASE Inv.Sta
          CASE 0 TO 8
               p = VAL(Cod.Dep) * DaysAvg          'life in days
               f = DateDif&(Inv.Dtp, EndDay) + 1   'from date
               t = DateDif&(Inv.Dtp, DATE$)        'to date
               L = 100 * VAL(Inv.Prc) * (2 * p - f - t + 2) * (t - f + 1) / (p ^ 2 + p)
               IF L < 0 THEN L = 0
               IF Inv.Tde + L > 100 * VAL(Inv.Prc) THEN L = 100 * VAL(Inv.Prc) - Inv.Tde
          CASE 9 TO 14
               L = 100 * VAL(Inv.Prc) - Inv.Tde
          CASE 15
               L = 0
          END SELECT
                            Inv.Mde = Inv.Mde + L  'Depreciate
                            Inv.Yde = Inv.Yde + L  '
                            Inv.Tde = Inv.Tde + L  '----------
                            Lt = Lt + L            'accumulate

          SELECT CASE Inv.Sta                      '----------
                 CASE 0:    Inv.Mdi = Inv.Mdi + d  'Idle
                            Inv.Ydi = Inv.Ydi + d  '
                            Inv.Tdi = Inv.Tdi + d  '----------
                 CASE 1, 4: Inv.Mds = Inv.Mds + d  'In-Service
                            Inv.Yds = Inv.Yds + d  'On-Rent/IS
                            Inv.Tds = Inv.Tds + d  '----------
                 CASE 2:    Inv.Mdr = Inv.Mdr + d  'On-Rent
                            Inv.Ydr = Inv.Ydr + d  '
                            Inv.Tdr = Inv.Tdr + d  '----------
                 CASE 3:    Inv.Mas = Inv.Mas + d  'On-Rent/AS
                            Inv.Yas = Inv.Yas + d  '
                            Inv.Tas = Inv.Tas + d  '----------
                 CASE 5:    Inv.Mar = Inv.Mar + d  'On-Rent/AR
                            Inv.Yar = Inv.Yar + d  '
                            Inv.Tar = Inv.Tar + d  '----------
                 CASE 8:    Inv.Mdr = Inv.Mdr + d  'Loaner
                            Inv.Ydr = Inv.Ydr + d  '
                            Inv.Tdr = Inv.Tdr + d  '----------
          END SELECT                               '
          PUT #10, x, Inv                          'save
          NEXT                                     '
          
          GET #22, 1, Act                          '
          FOR z = 1 TO 5                           '
          Act.Dep(z) = Act.Dep(z) + Lt             'update depreciation
          NEXT                                     '
          PUT #22, 1, Act                          '

         'Reorts------------------------------------------------------
          DO
          mTelRct                                  'telephone receipts
          mCshRpt 0                                'print cash journal
          mVewOpt 1                                'activity journal
          mDueOpt 1                                'delinquency sheet
          TranLog                                  'transaction log
          LOOP WHILE Response("Resp-20") = 0       'repeat if desired

          Today$ = LEFT$(DayWeek$(DATE$), 3)       'today's initials
          SHELL "fSpool Office." + Today$ + " >NUL"'print to file
          Laser = 1                                'set laser printer
          mCshRpt 0                                'print cash journal
          mVewOpt 1                                'activity journal
          mDueOpt 1                                'delinquency sheet
          TranLog                                  'transaction log
          IF Today$ = RptDay THEN mOrdRpt 0        'inventory orders
          Laser = 0                                'reset laser printer
          SHELL "fSpool /U >NUL"                   'reset to printer
          
          ZeroAct 1                                'clear day activity
          EndDay = DATE$                           'update config date
          CLOSE : KILL "..\DataFile\Transact.LOG"  'clear trasaction log
          CLOSE : KILL "..\DataFile\Transact.HST"  'clear trasaction file
       END IF
    ELSE
       WinMssg "Day has already been closed"
    END IF
END SUB

DEFINT L
SUB mDueOpt (a)
    PastDue
    Statlin "View Store Delinquency"
    CALL MhDisplayByName(0, 0, "PastDues", e)
    
    FOR x = 1 TO LOF(11) \ LEN(Inx)
        GET #11, x, Inx
        GET #10, Inx.Num, Inv
        IF Inv.Sta < 9 THEN L0 = L0 + 1
        IF Inv.Sta = 0 THEN L1 = L1 + 1
    NEXT
       COLOR 16, 3
       LOCATE 6, 33: PRINT USING "######"; L0;
       LOCATE 7, 33: PRINT USING "######"; L1;
       LOCATE 8, 33: PRINT USING "##.##%"; 100 * L1 / L0;

    FOR r = 0 TO 1
    IF Act.Dui(r, 7) > 0 THEN
       COLOR 16, 3
       LOCATE 6, r * 40 + 10: PRINT USING "#####"; r;
       LOCATE 7, r * 40 + 10: PRINT USING "#####"; Act.Dui(r, 7);
       LOCATE 8, r * 40 + 10: PRINT USING "#####"; Act.Dui(r, 8) / Act.Dui(r, 7);
       COLOR 17, 7
       FOR x = 1 TO 6
       LOCATE 2 * x + 10, r * 40 + 12: PRINT USING "#####"; Act.Dua(r, x);
       LOCATE 2 * x + 10, r * 40 + 18: PRINT USING "###.##"; 100 * (Act.Dua(r, x) / Act.Dua(r, 7));
       LOCATE 2 * x + 10, r * 40 + 27: PRINT USING "#####"; Act.Dui(r, x);
       LOCATE 2 * x + 10, r * 40 + 33: PRINT USING "###.##"; 100 * (Act.Dui(r, x) / Act.Dui(r, 7));
       NEXT
    END IF
    NEXT
    FOR r = 0 TO 1
    IF Act.Dui(r + 2, 7) > 0 THEN
       COLOR 16, 3
       LOCATE 25, r * 40 + 10: PRINT USING "#####"; r + 2;
       LOCATE 26, r * 40 + 10: PRINT USING "#####"; Act.Dui(r + 2, 7);
       LOCATE 27, r * 40 + 10: PRINT USING "#####"; Act.Dui(r + 2, 8) / Act.Dui(r + 2, 7);
       COLOR 17, 7
       FOR x = 1 TO 6
       LOCATE 2 * x + 29, r * 40 + 12: PRINT USING "#####"; Act.Dua(r + 2, x);
       LOCATE 2 * x + 29, r * 40 + 18: PRINT USING "###.##"; 100 * (Act.Dua(r + 2, x) / Act.Dua(r + 2, 7));
       LOCATE 2 * x + 29, r * 40 + 27: PRINT USING "#####"; Act.Dui(r + 2, x);
       LOCATE 2 * x + 29, r * 40 + 33: PRINT USING "###.##"; 100 * (Act.Dui(r + 2, x) / Act.Dui(r + 2, 7));
       NEXT
    END IF
    NEXT
    IF a = 0 THEN
       KeyPres f$
    ELSE
       Heading
       IF Laser THEN
          LPRINT CHR$(27); "(s4099T"; 'Courier
          LPRINT CHR$(27); "(s0B";    'Medium bold
          LPRINT CHR$(27); "(s10H";   'Pitch 10 cpi
          LPRINT CHR$(27); "&a80M"    'Right margin
       ELSE
          LPRINT CHR$(27); "!"; CHR$(0)
       END IF
       FOR L = 2 TO 42
       FOR c = 1 TO 80
       LPRINT CHR$(SCREEN(L, c));
       NEXT
       NEXT
       LPRINT CHR$(12);
    END IF
END SUB

DEFLNG L
SUB mExpOpt ()
    Statlin "Petty Cash"
    PCOPY 0, 2: CALL MhDisplayByName(0, 0, "Expense", e)

    t = LOF(28) \ LEN(Pet)
    r = 1
    FOR n = 1 TO t
        GET #28, n, Pet
        qPrint Pet.Amt, r + 10, 39, 0, 1
        qPrint Pet.Det, r + 10, 50, 0, 1
        r = r + 1
        IF r > 12 THEN
           CALL Scroll(39, 11, 47, r + 9, 1)
           CALL Scroll(50, 11, 75, r + 9, 1)
           r = 12
        END IF
    NEXT

    GET #22, 1, Act                                              'get activity
    COLOR 11, 1
    LOCATE 24, 39, 0: PRINT USING "#####.##"; Act.Pet(1) / 100;  'display amount
    Picture 24, 39, 8, p$                                        'get picture
    L = Act.Pet(1)
    FOR x = 1 TO 5                                               'reverse old
        Act.Pet(x) = Act.Pet(x) - 100 * VAL(p$)                  'day value
    NEXT                                                         'and get ready
    PUT #22, 1, Act                                              'for new value

140 GET #28, n, Pet
    qPrint Pet.Det, r + 10, 50, 0, 1

141 L = L - 100 * VAL(Pet.Amt)
    EntryN r + 10, 39, 8, 2, e, b, Pet.Amt: IF e THEN 143
    L = L + 100 * VAL(Pet.Amt)
    COLOR 11, 1: LOCATE 24, 39: PRINT USING "#####.##"; L / 100;

    IF b THEN
       IF Pet.Amt > "       0" THEN PUT #28, n, Pet
       n = n - 1: IF n < 1 THEN n = 1: GOTO 141
       r = r - 1
       IF r < 1 THEN
          r = 1
          CALL BkScroll(39, 11, 47, 22, 1)
          CALL BkScroll(50, 11, 75, 22, 1)
          qPrint Pet.Det, r + 10, 50, 0, 1
       END IF
       GOTO 140
    END IF
    IF Pet.Amt < "       0" THEN 141
    PUT #28, n, Pet
142 EntryS r + 10, 50, 26, e, b, Pet.Det:   IF e THEN 143
    PUT #28, n, Pet
    IF b THEN 141
    n = n + 1
    r = r + 1
    IF r > 12 THEN
       CALL Scroll(39, 11, 47, r + 9, 1)
       CALL Scroll(50, 11, 75, r + 9, 1)
       r = 12
    END IF
    GOTO 140

143 Picture 24, 39, 8, p$
    FOR x = 1 TO 5
        Act.Pet(x) = Act.Pet(x) + 100 * VAL(p$)
    NEXT
    PUT #22, 1, Act
END SUB

DEFSNG A-Z
SUB mFmtOpt ()
    CLS
    PRINT "Use -Ctrl C- to exit format."
    SHELL "Format A:/q/u/v:transfers/f:1.44"
END SUB

DEFINT A-Z
SUB mMonOpt ()
    Statlin "End of MONTH Closing"
    d = DateDif&(EndDay, DATE$)
    m = DateDif&(EndMon, DATE$)
    IF d <> 0 THEN WinMssg "Must close DAY first":  EXIT SUB
    IF w <> 0 THEN WinMssg "Must close WEEK first": EXIT SUB
    IF m <> 0 THEN
       IF Confirm(0, "close the MONTH") THEN
          mStsRpt 0                                           'status report
          Today$ = LEFT$(DayWeek$(DATE$), 3)                  'today's initials
          LOCATE 1: SHELL "fSpool Office." + Today$ + " >NUL" 'print status report
          Laser = 1                                           'set laser printer
          mStsRpt 0                                           'for office
          Laser = 0                                           'reset laser printer
          LOCATE 1: SHELL "fSpool /U >NUL"                    'reset printer

          FOR r = 1 TO LOF(10) \ LEN(Inv)
              GET #10, r, Inv
              Inv.Mdi = 0'D-Idle
              Inv.Mdr = 0'D-Rented
              Inv.Mas = 0'D-Awaiting Service
              Inv.Mds = 0'D-In Service
              Inv.Mar = 0'D-Awaiting Return
              Inv.Min = 0'$-Revenues
              Inv.Mde = 0'$-Depreciation
              Inv.Mre = 0'$-Repairs
              PUT #10, r, Inv
          NEXT
          FOR r = 1 TO LOF(16) \ LEN(Agr)
              GET #16, r, Agr
              Agr.Mri = 0'Rental income
              Agr.Mds = 0'Discount
              Agr.Mtx = 0'Sales tax
              Agr.Mlc = 0'Late charge
              Agr.Mtc = 0'Trip charge
              Agr.Mot = 0'Other charges
              Agr.Min = 0'Waiver
              Agr.Mrf = 0'Refferal
              PUT #16, r, Agr
          NEXT
          ZeroAct 3
          EndMon = DATE$
       END IF
    ELSE
       WinMssg "Month has already been closed"
    END IF
END SUB

SUB mOrdOpt ()
    Statlin "Recap Report Weekday"
    CALL MhDisplayByName(0, 0, "List-Day", e)
    REDIM a$(1 TO 7)
    a$(1) = "Monday   "
    a$(2) = "Tuesday  "
    a$(3) = "Wednesday"
    a$(4) = "Thursday "
    a$(5) = "Friday   "
    a$(6) = "Saturday "
    a$(7) = "Sunday   "
    FOR x = 1 TO 7
    IF RptDay = LEFT$(a$(x), 3) THEN EXIT FOR
    NEXT
    r = 15 + x: n = x
    COLOR 27, 0: LOCATE r, 54: PRINT "û";
O1: COLOR 27, 0: LOCATE r, 56: PRINT a$(n);
    KeyPres n$
    COLOR 31, 0: LOCATE r, 56: PRINT a$(n);
    SELECT CASE n$
    CASE CHR$(27)
        EXIT SUB
    CASE CHR$(13)
        COLOR 27, 0: LOCATE r, 54: PRINT "û";
        RptDay = a$(n): EXIT SUB
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > 7 GOTO O1 ELSE n = n + 1: r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 GOTO O1 ELSE n = n - 1: r = r - 1
    END SELECT
    GOTO O1
END SUB

SUB mPagOpt ()
    Statlin "Pager Communications"
    Tra.Typ = "OG": PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini
   '$INCLUDE: '..\Include\Terminat.DEC'
    CALL PrtSc(1)
    CHDIR "\PROCOMMP"
    SHELL "PCPLUS /fPAGER"
    CHDIR "\RENTALS\SYSTEM"
    CALL MhBlinkOff
    CALL MhShadowOn(0, 8)
'   CALL Clock(1)
    OpenFiles
    Tra.Typ = "OF": PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini
END SUB

DEFLNG L
SUB mPurOpt ()
    IF Response("Resp-14") THEN EXIT SUB

    Statlin "Purging Agreements"
    CLOSE 27: OPEN "..\DataFile\Agreemnt.M" FOR RANDOM AS #27 LEN = LEN(Agr)
    CLOSE 28: OPEN "..\DataFile\Agreemnt.I" FOR RANDOM AS #28 LEN = LEN(Agx)
    r = 0: t = LOF(16) / LEN(Agr)
    FOR x = 1 TO t                                  'purge agreements
    GET #16, x, Agr                                 '----------------------
    LOCATE 43, 55: PRINT USING "###%"; 100 * (x / t);
    L = DateDif(Agr.Dta, DATE$)                     'days since last activity
    IF Agr.Bad > 0 THEN L = 1                       'no purge unmarked bad chk
    SELECT CASE Agr.Sta                             'purge according to status
      CASE "A"                                      'keep all active
      CASE "V":           GOTO pu                   'Void purge all
      CASE "N": IF FindAgr(Agr.Ref) = 0 GOTO pu     'Note
      CASE "P": IF L > 40 GOTO pu                   'Paid-outs, 40 days
      CASE "C": IF L > 40 GOTO pu                   'Charge-offs, 40 days
      CASE "T": IF L > 190 GOTO pu                  'Terminated, >190 = all
                IF L > 100 AND Percent < 80 GOTO pu '      purge >100 < 80%
                IF L > 40 AND Percent < 60 GOTO pu  '      purge >40  < 60%
      CASE ELSE                                     '
    END SELECT                                      '
    r = r + 1                                       'increment number
    Agx.Cus = Agr.Cus                               'build new index
    Agx.Agr = Agr.Num                               'agreement file
    Agx.Num = r                                     'new index number
    PUT #27, r, Agr                                 'save agreement record
    PUT #28, r, Agx                                 'save index record
    IF INKEY$ = CHR$(27) THEN EXIT SUB              'check for ESCAPE
pu: NEXT
    a = t - r: LOCATE 43, 55: PRINT USING "####"; a;
    CLOSE : LOCATE 1
    KILL "..\DataFile\Agreemnt.MAS"
    KILL "..\DataFile\Agreemnt.INX"
    KILL "..\DataFile\Agreemnt.CUS"
    NAME "..\DataFile\Agreemnt.M" AS "..\DataFile\Agreemnt.MAS"
    NAME "..\DataFile\Agreemnt.I" AS "..\DataFile\Agreemnt.INX"
    AgrSort

    OpenFiles
    Statlin "Purging Receipts"
    CLOSE 27: OPEN "..\DataFile\Receipts.M" FOR RANDOM AS #27 LEN = LEN(Rct)
    CLOSE 28: OPEN "..\DataFile\Receipts.I" FOR RANDOM AS #28 LEN = LEN(Rcx)
    r = 0: t = LOF(19) / LEN(Rct)                'Purge receipts
    FOR x = 1 TO t                               '-----------------------
    GET #19, x, Rct                              'retrieve receipt
    LOCATE 43, 55: PRINT USING "###%"; 100 * (x / t);
    IF FindAgr(Rct.Agr) THEN                     'if agreement exists
       r = r + 1                                 'increment index number
       Rcx.Agr = VAL(Rct.Agr)                    'and build new receipt
       Rcx.Rct = VAL(Rct.Num)                    'index record
       Rcx.Num = r                               'and new index number
       PUT #27, r, Rct                           'save receipt record
       PUT #28, r, Rcx                           'save index record
    END IF                                       '
    NEXT
    c = t - r: LOCATE 43, 55: PRINT USING "####"; c;
    CLOSE : LOCATE 1
    KILL "..\DataFile\Receipts.MAS"
    KILL "..\DataFile\Receipts.INX"
    KILL "..\DataFile\Receipts.AGR"
    NAME "..\DataFile\Receipts.M" AS "..\DataFile\Receipts.MAS"
    NAME "..\DataFile\Receipts.I" AS "..\DataFile\Receipts.INX"
    RctSort

    OpenFiles
    Statlin "Purging Merchandise"
    CLOSE 27: OPEN "..\DataFile\Inventor.M" FOR RANDOM AS #27 LEN = LEN(Inv)
    CLOSE 28: OPEN "..\DataFile\Inventor.I" FOR RANDOM AS #28 LEN = LEN(Inx)
    r = 0: t = LOF(10) / LEN(Inv)                     'Purge merchandise
    FOR x = 1 TO t                                    '----------------------
    GET #11, x, Inx: GET #10, Inx.Num, Inv            'retrieve inventory

    LOCATE 43, 55: PRINT USING "###%"; 100 * (x / t); 'percentage completed
    L = DateDif(Inv.Dta, DATE$)                       'days since last activity
    IF L > 190 AND Inv.Sta > 8 THEN                   'elapsed days exceeds
       IF Inv.Sta <> 15 THEN                          'skip transferred
          Ld = 100 * VAL(Inv.Prc) - Inv.Tde           'check depreciation
          IF Ld > 0 THEN Lt = Lt + Ld                 'accumulate if any left
       END IF
    ELSE                                              '190 days & extingueshed
       r = r + 1                                      'increment index number
       Inx.Num = r                                    'set record number
       PUT #27, r, Inv                                'save inventory record
       PUT #28, r, Inx                                'save inventory index
    END IF                                            '
    NEXT

    GET #22, 1, Act                                   'retrieve activity
    FOR z = 1 TO 5                                    '
    Act.Dep(z) = Act.Dep(z) + Lt                      'update depreciation
    NEXT                                              '
    PUT #22, 1, Act                                   '

    i = t - r: LOCATE 43, 55: PRINT USING "####"; i;
    Tra.Typ = "OP"
    PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini; a; c; i
    CLOSE : LOCATE 1
    KILL "..\DataFile\Inventor.MAS"
    KILL "..\DataFile\Inventor.INX"
    KILL "..\DataFile\Inventor.SER"
    SHELL "Sortf ..\DataFile\Inventor.I   ..\DataFile\Inventor.Inx /L33 /+1,27 /Q"
    SHELL "Sortf ..\DataFile\Inventor.Inx ..\DataFile\Inventor.Ser /L33 /+18,10 /Q"
    NAME "..\DataFile\Inventor.M" AS "..\DataFile\Inventor.MAS"
    KILL "..\DataFile\Inventor.I"
END SUB

DEFSNG A-Z
SUB mRptOpt ()
    CLOSE
    Statlin "View Reports Activities"
    SHELL "List ..\DataFile\Printing.LOG"
END SUB

DEFINT A-Z
SUB mTerOpt ()
    b = DateDif&(EndBak, DATE$)
    d = DateDif&(EndDay, DATE$)
    w = DateDif&(EndWek, DATE$)
    m = DateDif&(EndMon, DATE$) - Ndm(VAL(MID$(DATE$, 1, 2)))
    y = DateDif&(EndYer, DATE$)
    IF b > 0 THEN WinMssg "Warning: did not backup data"
    IF d > 0 THEN WinMssg "Warning: day was not closed"
    IF w > 6 THEN WinMssg "Warning: week was not closed"
    IF m > 0 THEN WinMssg "Warning: month was not closed"
    IF y > 365 THEN WinMssg "Warning: year was not closed"
    Tra.Typ = "OO": PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini
   '$INCLUDE: '..\Include\Terminat.DEC'
    CALL PrtSc(1)
    IF Pas.Lvl > "7" THEN
       END
    ELSE
       CHDIR "C:\anyWhere"
       RUN "awHOST -m=w"
    END IF
END SUB

DEFSNG A-Z
SUB mTraOpt ()
    CLOSE
    Statlin "View Daily Transactions"
    SHELL "List ..\DataFile\Transact.LOG"
END SUB

DEFINT A-Z
SUB mVewOpt (a)
    Statlin "View Store Activity"
    d$ = "#######.##"
    w$ = "#######.  "
    p$ = "##### ###%"
    COLOR 17, 7
    GET #22, 1, Act
    
    SELECT CASE VAL(Pas.Lvl)
           CASE 7 TO 9: t = 5                          'view all cloumns
           CASE 5 TO 6: t = 3                          'view day, week & month
           CASE 1 TO 4: IF a THEN t = 3 ELSE t = 1     'view day only
           CASE ELSE:   t = 1
    END SELECT

    CALL MhDisplayByName(0, 0, "Activity0", e)
    FOR x = 1 TO t
    LOCATE 5, 15 + x * 11:  PRINT USING w$; Act.Bor(1, x);
    LOCATE 6, 15 + x * 11:  PRINT USING w$; Act.Bor(0, x);
    LOCATE 7, 15 + x * 11:  PRINT USING w$; Act.Bor(2, x);
    LOCATE 8, 15 + x * 11:  PRINT USING w$; Act.Bor(3, x);
    LOCATE 10, 15 + x * 11: PRINT USING w$; Act.Bor(1, x) - Act.Bor(0, x) - Act.Bor(2, x) - Act.Bor(3, x);
    LOCATE 11, 15 + x * 11: PRINT USING w$; Act.Bor(4, x);
    LOCATE 12, 15 + x * 11: PRINT USING w$; Act.Bor(4, x) + Act.Bor(1, x) - Act.Bor(0, x) - Act.Bor(2, x) - Act.Bor(3, x);
    
    FOR z = 0 TO 14
    LOCATE z + 14, 15 + x * 11: PRINT USING w$; Act.Sta(z, x);
    NEXT

    LOCATE 30, 15 + x * 11: PRINT USING w$; Act.New(x);
    LOCATE 31, 15 + x * 11: PRINT USING w$; Act.Tro(x);
    LOCATE 32, 15 + x * 11: PRINT USING w$; Act.Tri(x);
    LOCATE 33, 15 + x * 11: PRINT USING w$; Act.Sro(x);
    LOCATE 34, 15 + x * 11: PRINT USING w$; Act.Sri(x);

    LOCATE 36, 15 + x * 11: PRINT USING d$; Act.Inn(x) / 100;
    LOCATE 37, 15 + x * 11: PRINT USING d$; Act.Inu(x) / 100;
    LOCATE 38, 15 + x * 11: PRINT USING d$; Act.Dep(x) / 100;
    NEXT
    IF a = 0 THEN
       KeyPres f$
       PCOPY 0, 1
    ELSE
       Heading
       IF Laser THEN
          LPRINT CHR$(27); "(s4099T"; 'Courier
          LPRINT CHR$(27); "(s0B";    'Medium bold
          LPRINT CHR$(27); "(s10H";   'Pitch 10 cpi
          LPRINT CHR$(27); "&a80M"    'Right margin
       ELSE
          LPRINT CHR$(27); "!"; CHR$(0)
       END IF
       FOR L = 2 TO 39
       FOR c = 1 TO 80
       LPRINT CHR$(SCREEN(L, c));
       NEXT
       NEXT
       LPRINT CHR$(12);
    END IF
    '-------------------------------------------------------------------------
    CALL MhDisplayByName(0, 0, "Activity1", e)
    FOR x = 1 TO t

    LOCATE 5, 15 + x * 11:  PRINT USING d$; Act.Sal(x) / 100;
    LOCATE 6, 15 + x * 11:  PRINT USING d$; Act.Pay(x) / 100;
    LOCATE 7, 15 + x * 11:  PRINT USING d$; Act.Rnt(x) / 100;
    LOCATE 8, 15 + x * 11:  PRINT USING d$; Act.Dis(x) / 100;
    LOCATE 9, 15 + x * 11:  PRINT USING d$; Act.Tax(x) / 100;
    LOCATE 10, 15 + x * 11: PRINT USING d$; Act.Lat(x) / 100;
    LOCATE 11, 15 + x * 11: PRINT USING d$; Act.Trp(x) / 100;
    LOCATE 12, 15 + x * 11: PRINT USING d$; Act.Oth(x) / 100;
    LOCATE 13, 15 + x * 11: PRINT USING d$; Act.Wav(x) / 100;
    LOCATE 14, 15 + x * 11: PRINT USING d$; Act.Ref(x) / 100;
    LOCATE 15, 15 + x * 11: PRINT USING d$; Act.Crd(x) / 100;
    LOCATE 16, 15 + x * 11: PRINT USING d$; Act.Una(x) / 100;
    LOCATE 18, 15 + x * 11: PRINT USING d$; Act.Amt(x) / 100;
    LOCATE 19, 15 + x * 11: PRINT USING d$; Act.Tel(x) / 100;
    LOCATE 20, 15 + x * 11: PRINT USING d$; Act.Cel(x) / 100;
    LOCATE 21, 15 + x * 11: PRINT USING d$; Act.Pet(x) / 100;
    LOCATE 23, 15 + x * 11: PRINT USING d$; (Act.Amt(x) + Act.Tel(x) + Act.Cel(x) - Act.Pet(x)) / 100;
    LOCATE 24, 15 + x * 11: PRINT USING d$; (Act.Amt(x) - Act.Pet(x) - Act.Tax(x)) / 100;
    
    LOCATE 26, 15 + x * 11: PRINT USING w$; Act.Agn(x);
    LOCATE 27, 15 + x * 11: PRINT USING w$; Act.Agv(x);
    LOCATE 28, 15 + x * 11: PRINT USING w$; Act.Agt(x);
    LOCATE 29, 15 + x * 11: PRINT USING w$; Act.Agc(x);
    LOCATE 30, 15 + x * 11: PRINT USING w$; Act.Agp(x);
    LOCATE 31, 15 + x * 11: PRINT USING w$; Act.Agr(x);
                 
    LOCATE 33, 15 + x * 11: PRINT USING w$; Act.Rcn(x);
    LOCATE 34, 15 + x * 11: PRINT USING w$; Act.Rcv(x);
    LOCATE 35, 15 + x * 11: PRINT USING w$; Act.Rcb(x);
    LOCATE 36, 15 + x * 11: PRINT USING w$; Act.Rcc(x);

    LOCATE 38, 15 + x * 11: PRINT USING w$; Act.Eza(x);
    LOCATE 39, 15 + x * 11: PRINT USING w$; Act.FRd(x);
    LOCATE 40, 15 + x * 11: PRINT USING w$; Act.Ltd(x);
    NEXT

    IF a = 0 THEN
       p = 1
       DO
       KeyPres f$
       SCREEN , , , p
       SWAP p, a
       LOOP WHILE f$ <> CHR$(27)
       SCREEN , , , 0
    ELSE
       Heading
       IF Laser THEN
          LPRINT CHR$(27); "(s4099T"; 'Courier
          LPRINT CHR$(27); "(s0B";    'Medium bold
          LPRINT CHR$(27); "(s10H";   'Pitch 10 cpi
          LPRINT CHR$(27); "&a80M"    'Right margin
       ELSE
          LPRINT CHR$(27); "!"; CHR$(0)
       END IF
       FOR L = 2 TO 41
       FOR c = 1 TO 80
       LPRINT CHR$(SCREEN(L, c));
       NEXT
       NEXT
       LPRINT CHR$(12);
    END IF
END SUB

SUB mWekOpt ()
    Statlin "End of WEEK Closing"
    d = DateDif&(EndDay, DATE$)
    w = DateDif&(EndWek, DATE$)
    IF d <> 0 THEN WinMssg "Must close the DAY first": EXIT SUB
    IF w <> 0 THEN
       IF Confirm(0, "close the WEEK") THEN
          LOCATE 43, 45: PRINT "Working.....";
          ZeroAct 2
          EndWek = DATE$
       END IF
    ELSE
       WinMssg "Week has already been closed"
    END IF
END SUB

SUB mYerOpt ()
    Statlin "End of YEAR Closing"
    d = DateDif&(EndDay, DATE$)
    w = DateDif&(EndWek, DATE$)
    m = DateDif&(EndMon, DATE$)
    y = DateDif&(EndYer, DATE$)
    IF d <> 0 THEN WinMssg "Must close the DAY first":   EXIT SUB
    IF m <> 0 THEN WinMssg "Must close the MONTH first": EXIT SUB
    IF y <> 0 THEN
       IF Confirm(0, "close the YEAR") THEN
          Today$ = LEFT$(DayWeek$(DATE$), 3)                  'today's initials
          LOCATE 1: SHELL "fSpool Office." + Today$ + " >NUL" 'print status report
          Laser = 1                                           'set laser printer
          mPriRpt 0                                           'for office
          Laser = 0                                           'reset laser printer
          LOCATE 1: SHELL "fSpool /U >NUL"                    'reset printer

          LOCATE 43, 45: PRINT "Working.....";
          FOR r = 1 TO LOF(10) \ LEN(Inv)
              GET #10, r, Inv
              Inv.Ydi = 0'D-Idle
              Inv.Ydr = 0'D-Rented
              Inv.Yas = 0'D-Awaiting Service
              Inv.Yds = 0'D-In Service
              Inv.Yar = 0'D-Awaiting Return
              Inv.Yin = 0'$-Revenues
              Inv.Yde = 0'$-Depreciation
              Inv.Yre = 0'$-Repairs
              PUT #10, r, Inv
          NEXT
          FOR r = 1 TO LOF(16) \ LEN(Agr)
              GET #16, r, Agr
              Agr.Yri = 0'Rental income
              Agr.Yds = 0'Discount
              Agr.Ytx = 0'Sales tax
              Agr.Ylc = 0'Late charge
              Agr.Ytc = 0'Trip charge
              Agr.Yot = 0'Other charges
              Agr.Yin = 0'Waiver
              Agr.Yrf = 0'Refferal
              PUT #16, r, Agr
          NEXT
          ZeroAct 4
          EndYer = DATE$
       END IF
    ELSE
       WinMssg "Year is already closed"
    END IF
END SUB

SUB PastDue ()
    Statlin "Computing Delinquency"
    GET #22, 1, Act
    ERASE Act.Dui, Act.Dua
    t = LOF(16) \ LEN(Agr)
    FOR x = 1 TO t
    LOCATE 43, 51: PRINT USING "###%"; 100 * (x / t);
    GET #16, x, Agr
    IF Agr.Sta = "A" THEN
       i = 0
       IF Agr.It1 > "          " THEN i = i + 1
       IF Agr.It2 > "          " THEN i = i + 1
       IF Agr.It3 > "          " THEN i = i + 1
       IF Agr.It4 > "          " THEN i = i + 1
       d = DateDif&(Agr.Nxt, DATE$)
       r = VAL(Agr.Rte)
       Act.Dui(0, 7) = Act.Dui(0, 7) + i 'store BOR inventory
       Act.Dua(0, 7) = Act.Dua(0, 7) + 1 'store BOR accounts
       Act.Dui(0, 8) = Act.Dui(0, 8) + d 'store DAY days index
       Act.Dui(r, 7) = Act.Dui(r, 7) + i 'route BOR inventory
       Act.Dua(r, 7) = Act.Dua(r, 7) + 1 'route BOR accounts
       Act.Dui(r, 8) = Act.Dui(r, 8) + d 'route DAY days index
       SELECT CASE d
           CASE 3 TO 4:   p = 1
           CASE 5 TO 7:   p = 2
           CASE 8 TO 13:  p = 3
           CASE 14 TO 30: p = 4
           CASE IS > 30:  p = 5
           CASE ELSE:     p = 0
       END SELECT
       IF p > 0 THEN
          Act.Dui(0, p) = Act.Dui(0, p) + i
          Act.Dua(0, p) = Act.Dua(0, p) + 1
          Act.Dui(0, 6) = Act.Dui(0, 6) + i
          Act.Dua(0, 6) = Act.Dua(0, 6) + 1
          Act.Dui(r, p) = Act.Dui(r, p) + i
          Act.Dua(r, p) = Act.Dua(r, p) + 1
          Act.Dui(r, 6) = Act.Dui(r, 6) + i
          Act.Dua(r, 6) = Act.Dua(r, 6) + 1
       END IF
    END IF
    NEXT
    PUT #22, 1, Act
END SUB

DEFSNG A-C, E-Z
SUB ZeroAct (d)
    GET #22, 1, Act
    Act.Pet(d) = 0
    Act.Pay(d) = 0
    Act.Tel(d) = 0
    Act.Cel(d) = 0
    Act.Sal(d) = 0
    Act.Rnt(d) = 0
    Act.Dis(d) = 0
    Act.Tax(d) = 0
    Act.Lat(d) = 0
    Act.Trp(d) = 0
    Act.Oth(d) = 0
    Act.Wav(d) = 0
    Act.Ref(d) = 0
    Act.Crd(d) = 0
    Act.Una(d) = 0
    Act.Amt(d) = 0

    Act.Agn(d) = 0
    Act.Agv(d) = 0
    Act.Agt(d) = 0
    Act.Agc(d) = 0
    Act.Agp(d) = 0
    Act.Agr(d) = 0

    Act.Rcn(d) = 0
    Act.Rcv(d) = 0
    Act.Rcb(d) = 0
    Act.Rcc(d) = 0

    Act.Eza(d) = 0
    Act.FRd(d) = 0
    Act.Ltd(d) = 0

    Act.Sta(0, d) = 0
    Act.Sta(1, d) = 0
    Act.Sta(2, d) = 0
    Act.Sta(3, d) = 0
    Act.Sta(4, d) = 0
    Act.Sta(5, d) = 0
    Act.Sta(6, d) = 0
    Act.Sta(7, d) = 0
    Act.Sta(8, d) = 0
    Act.Sta(9, d) = 0
    Act.Sta(10, d) = 0
    Act.Sta(11, d) = 0
    Act.Sta(12, d) = 0
    Act.Sta(13, d) = 0
    Act.Sta(14, d) = 0
    Act.Sta(15, d) = 0

    Act.Bor(4, d) = Act.Bor(4, d) + Act.Bor(1, d) - Act.Bor(0, d) - Act.Bor(2, d) - Act.Bor(3, d)
    Act.Bor(0, d) = 0
    Act.Bor(1, d) = 0
    Act.Bor(2, d) = 0
    Act.Bor(3, d) = 0
    
    Act.New(d) = 0
    Act.Tro(d) = 0
    Act.Tri(d) = 0
    Act.Sro(d) = 0
    Act.Sri(d) = 0

    Act.Inn(d) = 0
    Act.Inu(d) = 0
    Act.Dep(d) = 0

    PUT #22, 1, Act
END SUB

