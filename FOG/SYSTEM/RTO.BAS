DEFINT A-Z: DEFLNG L

DECLARE SUB mNewInv ()
DECLARE SUB mTroInv ()
DECLARE SUB mTriInv ()
DECLARE SUB mTrmInv ()
DECLARE SUB mVewInv ()
DECLARE SUB mSroInv ()
DECLARE SUB mSriInv ()
DECLARE SUB mSrsInv ()
DECLARE SUB mGivInv ()
DECLARE SUB mScrInv ()
DECLARE SUB mChkInv ()
DECLARE SUB mMasInv ()

DECLARE SUB mNewAgr ()
DECLARE SUB mDspAgr ()
DECLARE SUB mPrtAgr ()
DECLARE SUB mVoiAgr ()
DECLARE SUB mChgAgr ()
DECLARE SUB mProAgr ()
DECLARE SUB mSwpAgr ()
DECLARE SUB mTerAgr ()
DECLARE SUB mLonAgr ()
DECLARE SUB mEzaAgr ()
DECLARE SUB mFreAgr ()
DECLARE SUB mReiAgr ()
DECLARE SUB mWavAgr ()
DECLARE SUB mRteAgr ()

DECLARE SUB mPrtRct ()
DECLARE SUB mVoiRct ()
DECLARE SUB mNewRct ()
DECLARE SUB mDspRct ()
DECLARE SUB mBadRct ()
DECLARE SUB mMrkRct ()
DECLARE SUB mSalRct ()
DECLARE SUB mTelRct ()

DECLARE SUB mAudRpt ()
DECLARE SUB mTimRpt ()
DECLARE SUB mMonRpt ()
DECLARE SUB mSrvRpt ()
DECLARE SUB mStaRpt ()
DECLARE SUB mStsRpt (s)
DECLARE SUB mIdeRpt ()
DECLARE SUB mBorRpt ()
DECLARE SUB mExtRpt ()
DECLARE SUB mSerRpt ()
DECLARE SUB mRatRpt ()
DECLARE SUB mAgrRpt ()
DECLARE SUB mCshRpt (s)
DECLARE SUB mPriRpt (s)
DECLARE SUB mOrdRpt (s)
DECLARE SUB mDueRpt (s)
DECLARE SUB mDuqRpt ()
DECLARE SUB mDelRpt ()
DECLARE SUB mActRpt ()
DECLARE SUB mTraRpt ()
DECLARE SUB mBadRpt ()
DECLARE SUB mLblRpt ()

DECLARE SUB mExpOpt ()

DECLARE SUB mDayOpt ()
DECLARE SUB mWekOpt ()
DECLARE SUB mMonOpt ()
DECLARE SUB mYerOpt ()
DECLARE SUB mTraOpt ()
DECLARE SUB mComOpt ()
DECLARE SUB mPagOpt ()
DECLARE SUB mVewOpt (s)
DECLARE SUB mDueOpt (s)
DECLARE SUB mRptOpt ()
DECLARE SUB mOrdOpt ()
DECLARE SUB mDatOpt ()
DECLARE SUB mFmtOpt ()
DECLARE SUB mPurOpt ()
DECLARE SUB mBakOpt (s)
DECLARE SUB mTerOpt ()

DECLARE SUB CustAgr (f$, f, e)
DECLARE SUB RctAgrt (f$, f, e)
DECLARE SUB InvRate (r, c)
DECLARE SUB RctSort ()
DECLARE SUB AgrSort ()


DECLARE FUNCTION InvRtrv ()
DECLARE FUNCTION Percent ()

'$INCLUDE: '..\Include\Config.dec'

    ON ERROR GOTO Errors
    IF COMMAND$ = "S" THEN Setup = 1
    IF COMMAND$ = "L" THEN Laser = 1

    y = SETMEM(-16384)   ' SETMEM in MS-DOS session under
                         ' Windows
    IR.ax = &H6700
    IR.bx = 36
    CALL INTERRUPT(&H21, IR, OU)

    SCREEN 0, 0, 0, 0: WIDTH 80, 43
    SCREEN 0, 0, 1, 1: PCOPY 0, 1
    SCREEN 0, 0, 0, 0

    CALL MhOpenScreenLib("RTO.SLB" + CHR$(0), e)
    CALL MhBlinkOff
    CALL MhShadowOn(0, 8)
'   CALL ClockSet(0, 1, 8, 1, 70, 141, 1)
    CALL PrtSc(0)

    DIM Menu(6, 2)     AS STRING
    DIM Loct(6, 2)     AS INTEGER
    DIM Subm(6, 23, 2) AS STRING
    DIM HiLi(6, 23)    AS INTEGER

    FOR m = 1 TO 6
    READ Menu(m, 1), Menu(m, 2), Loct(m, 1), Loct(m, 2)
    FOR c = 1 TO Loct(m, 2)
    READ Subm(m, c, 1), Subm(m, c, 2), HiLi(m, c)
    NEXT
    NEXT

   '$INCLUDE: '..\Include\LoadCfg.DEC'
    OpenFiles
    DO: LOOP WHILE PassWord("0") = 0
    Tra.Typ = "OI": PRINT #26, USING TraType; Tra.Typ; TIME$; Pas.Ini
    
    IF LOF(16) \ LEN(Agr) > 0 THEN
       GET #16, LOF(16) \ LEN(Agr), Agr
       IF VAL(Agr.Num) > AgrNum THEN
          PCOPY 0, 3: CALL MhDisplayByName(0, 0, "ErrMessage", e)
          qPrint STR$(ERR), 32, 38, 0, 1
          qPrint "Agreement number error ", 33, 38, 0, 1
          qPrint "Config.MAS", 35, 38, 0, 1
          SOUND 50, 3: KeyPres f$
          PCOPY 3, 0
          AgrNum = VAL(Agr.Num)
         '$INCLUDE: '..\Include\Terminat.DEC'
        END IF
    END IF
    OpenFiles
    IF LOF(19) \ LEN(Rct) > 0 THEN
       GET #19, LOF(19) \ LEN(Rct), Rct
       IF VAL(Rct.Num) > RctNum THEN
          PCOPY 0, 3: CALL MhDisplayByName(0, 0, "ErrMessage", e)
          qPrint STR$(ERR), 32, 38, 0, 1
          qPrint "Receipt number error ", 33, 38, 0, 1
          qPrint "Config.MAS", 35, 38, 0, 1
          SOUND 50, 3: KeyPres f$
          PCOPY 3, 0
          RctNum = VAL(Rct.Num)
         '$INCLUDE: '..\Include\Terminat.DEC'
       END IF
    END IF

'   CALL Clock(1)
    OpenFiles
    GET #6, VAL(StoNum), Sto
    
    m = 1: c = 1
10  IF c > Loct(m, 2) THEN c = Loct(m, 2)
    CALL MhDisplayByName(0, 0, "Logo", e)
    CALL MhDisplayByName(0, 0, Menu(m, 1), e)
    qPrint SPACE$(80), 43, 1, 0, 1
    qPrint Sto.Nam + " " + DATE$, 43, 2, 0, 1
20  COLOR 27, 0: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
    COLOR 28, 0: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
    CLOSE : KeyPres f$
    COLOR 17, 6: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
    COLOR 20, 6: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
30  SELECT CASE f$
    CASE CHR$(0) + "M"
         m = m + 1: IF m > 6 THEN m = 1
         GOTO 10
    CASE CHR$(0) + "K"
         m = m - 1: IF m < 1 THEN m = 6
         GOTO 10
    CASE CHR$(0) + "P"
         c = c + 1: IF c > Loct(m, 2) THEN c = 1
    CASE CHR$(0) + "H"
         c = c - 1: IF c < 1 THEN c = Loct(m, 2)
    CASE CHR$(13)
         COLOR 27, 0: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
         COLOR 28, 0: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
         CALL MhDisplayByName(0, 0, Menu(m, 2), e)
         OpenFiles
         SELECT CASE m
              CASE 1
                   SELECT CASE c
                        CASE 1:  IF PassWord("3") THEN CALL mNewAgr
                        CASE 2:  IF PassWord("3") THEN CALL mDspAgr
                        CASE 3:  IF PassWord("3") THEN CALL mPrtAgr
                        CASE 4:  IF PassWord("3") THEN CALL mVoiAgr
                        CASE 5:  IF PassWord("3") THEN CALL mTerAgr
                        CASE 6:  IF PassWord("8") THEN CALL mChgAgr
                        CASE 7:  IF PassWord("3") THEN CALL mLonAgr
                        CASE 8:  IF PassWord("8") THEN CALL mSwpAgr
                        CASE 9:  IF PassWord("8") THEN CALL mProAgr
                        CASE 10: IF PassWord("5") THEN CALL mReiAgr
                        CASE 11: IF PassWord("3") THEN CALL mEzaAgr
                        CASE 12: IF PassWord("8") THEN CALL mFreAgr
                        CASE 13: IF PassWord("3") THEN CALL mWavAgr
                        CASE 14: IF PassWord("5") THEN CALL mRteAgr
                   END SELECT
              CASE 2
                   SELECT CASE c
                        CASE 1:  IF PassWord("3") THEN CALL mNewRct
                        CASE 2:  IF PassWord("3") THEN CALL mDspRct
                        CASE 3:  IF PassWord("3") THEN CALL mPrtRct
                        CASE 4:  IF PassWord("3") THEN CALL mVoiRct
                        CASE 5:  IF PassWord("3") THEN CALL mBadRct
                        CASE 6:  IF PassWord("8") THEN CALL mMrkRct
                        CASE 7:  IF PassWord("3") THEN CALL mSalRct
                        CASE 8:  IF PassWord("3") THEN CALL mTelRct
                   END SELECT
              CASE 3
                   SELECT CASE c
                        CASE 1:  IF PassWord("3") THEN CALL mNewInv
                        CASE 2:  IF PassWord("3") THEN CALL mTroInv
                        CASE 3:  IF PassWord("3") THEN CALL mTriInv
                        CASE 4:  IF PassWord("7") THEN CALL mTrmInv
                        CASE 5:  IF PassWord("3") THEN CALL mVewInv
                        CASE 6:  IF PassWord("3") THEN CALL mSroInv
                        CASE 7:  IF PassWord("3") THEN CALL mSriInv
                        CASE 8:  IF PassWord("3") THEN CALL mSrsInv
                        CASE 9:  IF PassWord("8") THEN CALL mGivInv
                        CASE 10: IF PassWord("8") THEN CALL mScrInv
                        CASE 11: IF PassWord("3") THEN CALL mChkInv
                        CASE 12: IF PassWord("3") THEN CALL mMasInv
                   END SELECT
              CASE 4
                   SELECT CASE c
                        CASE 1:  IF PassWord("5") THEN CALL mExpOpt
                       'CASE 2:  IF PassWord("3") THEN CALL mExpOpt
                   END SELECT
              CASE 5
                   SELECT CASE c
                        CASE 1:  IF PassWord("5") THEN CALL mAudRpt
                        CASE 2:  IF PassWord("5") THEN CALL mTimRpt
                        CASE 3:  IF PassWord("7") THEN CALL mMonRpt
                        CASE 4:  IF PassWord("4") THEN CALL mSrvRpt
                        CASE 5:  IF PassWord("5") THEN CALL mStaRpt
                        CASE 6:  IF PassWord("5") THEN CALL mStsRpt(1)
                        CASE 7:  IF PassWord("5") THEN CALL mBorRpt
                        CASE 8:  IF PassWord("5") THEN CALL mIdeRpt
                        CASE 9:  IF PassWord("5") THEN CALL mExtRpt
                        CASE 10: IF PassWord("4") THEN CALL mSerRpt
                        CASE 11: IF PassWord("4") THEN CALL mRatRpt
                        CASE 12: IF PassWord("8") THEN CALL mPriRpt(1)
                        CASE 13: IF PassWord("5") THEN CALL mOrdRpt(1)
                        CASE 14: IF PassWord("4") THEN CALL mAgrRpt
                        CASE 15: IF PassWord("5") THEN CALL mCshRpt(1)
                        CASE 16: IF PassWord("4") THEN CALL mDuqRpt
                        CASE 17: IF PassWord("5") THEN CALL mDueRpt(0)
                        CASE 18: IF PassWord("5") THEN CALL mDueRpt(1)
                        CASE 19: IF PassWord("5") THEN CALL mDelRpt
                        CASE 20: IF PassWord("8") THEN CALL mActRpt
                        CASE 21: IF PassWord("8") THEN CALL mTraRpt
                        CASE 22: IF PassWord("5") THEN CALL mBadRpt
                        CASE 23: IF PassWord("5") THEN CALL mLblRpt
                   END SELECT
              CASE 6
                   SELECT CASE c
                        CASE 1:  IF PassWord("4") THEN CALL mDayOpt
                        CASE 2:  IF PassWord("4") THEN CALL mWekOpt
                        CASE 3:  IF PassWord("4") THEN CALL mMonOpt
                        CASE 4:  IF PassWord("5") THEN CALL mYerOpt
                        CASE 5:  IF PassWord("4") THEN CALL mTraOpt
                        CASE 6:  IF PassWord("1") THEN CALL mVewOpt(0)
                        CASE 7:  IF PassWord("4") THEN CALL mDueOpt(0)
                        CASE 8:  IF PassWord("5") THEN CALL mComOpt
                        CASE 9:  IF PassWord("4") THEN CALL mPagOpt
                        CASE 10: IF PassWord("8") THEN CALL mRptOpt
                        CASE 11: IF PassWord("5") THEN CALL mOrdOpt
                        CASE 12: IF PassWord("8") THEN CALL mDatOpt
                        CASE 13: IF PassWord("4") THEN CALL mFmtOpt
                        CASE 14: IF PassWord("7") THEN CALL mPurOpt
                        CASE 15: IF PassWord("1") THEN CALL mBakOpt(1)
                        CASE 16: IF PassWord("1") THEN CALL mBakOpt(2)
                        CASE 17: IF PassWord("1") THEN CALL mBakOpt(3)
                        CASE 18: IF PassWord("8") THEN CALL mBakOpt(4)
                        CASE 19: IF PassWord("0") THEN CALL mTerOpt
                   END SELECT
         END SELECT
         GOTO 10
    CASE ELSE: FOR x = 1 TO Loct(m, 2)
         IF f$ = Subm(m, x, 2) THEN c = x: f$ = CHR$(13): GOTO 30
         NEXT
    END SELECT
    GOTO 20
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    PCOPY 0, 3: CALL MhDisplayByName(0, 0, "ErrMessage", e)
    SELECT CASE ERR
           CASE 200:  Er$ = "Unable to find code master"
           CASE 201:  Er$ = "Unable to find user password"
           CASE 203:  Er$ = "Receipt rent exceeds $999.99"
           CASE 204:  Er$ = "Late charges exceeds $99.99"
           CASE 205:  Er$ = "Not eligible for reinstatment"
           CASE 206:  Er$ = "Invalid agreement number "
           CASE 207:  Er$ = "Store has item as active"
           CASE 208:  Er$ = "Printer {1} is not ready"
           CASE 209:  Er$ = "Printer {2} is not ready"
           CASE 210:  Er$ = "Invalid status, not 3,4,or 5"
           CASE ELSE: Er$ = ERROR$(ERR)
    END SELECT
    qPrint STR$(ERR) + STR$(ERDEV AND &HFF) + STR$((ERDEV AND &HFF00) \ 256), 32, 38, 0, 1
    qPrint Er$, 33, 38, 0, 1
    qPrint ERDEV$, 35, 38, 0, 1
    SOUND 50, 3: KeyPres f$
    PCOPY 3, 0
    OpenFiles
    GET #6, VAL(StoNum), Sto
    IF SortR THEN RctSort
    IF SortA THEN AgrSort
    RESUME 10
MenuData: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    DATA "Menu-Agr","Agreement",2,14
    DATA " Issue      ","I",3
    DATA " Display    ","D",3
    DATA " Print      ","P",3
    DATA " Void       ","V",3
    DATA " Terminate  ","T",3
    DATA " Charge-off ","C",3
    DATA " Loaner     ","L",3
    DATA " Swap       ","S",3
    DATA " Note       ","N",3
    DATA " Reinstate  ","R",3
    DATA " EZA Days   ","E",3
    DATA " Free Days  ","F",3
    DATA " Waiver     ","W",3
    DATA " ReroUte    ","U",7

    DATA "Menu-Rct","",13,8
    DATA " Issue      ","I",14
    DATA " Display    ","D",14
    DATA " Print      ","P",14
    DATA " Void       ","V",14
    DATA " Bad Check  ","B",14
    DATA " Mark Check ","M",14
    DATA " Cash Sale  ","C",14
    DATA " Telephone  ","T",14
    
    DATA "Menu-Inv","",22,12
    DATA " Receive New         ","N",31
    DATA " Transfer Out        ","T",23
    DATA " Transfer In         ","I",32
    DATA " Transfer Manual     ","M",32
    DATA " View Transfer File  ","F",37
    DATA " Service Send        ","S",31
    DATA " Service Receive     ","R",31
    DATA " Service OnSite      ","O",31
    DATA " Charge off Giveaway ","G",34
    DATA " Charge off Scrap    ","C",23
    DATA " Validate Codes      ","V",23
    DATA " Master Listing      ","L",30
                                    
    DATA "Menu-Pty","PettyCash",35,2
    DATA " Expenditure   ","E",36
    DATA " Replenishment ","R",36

    DATA "Menu-Rpt", "",46,23
    DATA " Audit Report      ","A",47
    DATA " Time Detail       ","T",47
    DATA " Money Detail      ","M",47
    DATA " Service Detail    ","S",47
    DATA " Code Status       ","C",47
    DATA " Type Status       ","T",47
    DATA " BOR Listing       ","B",47
    DATA " Idle Listing      ","I",47
    DATA " Extinguished      ","E",47
    DATA " Loaner & Service  ","L",47
    DATA " Rates and Terms   ","R",47
    DATA " Inventory Cost    ","I",47
    DATA " Inventory ReOrder ","O",59
    DATA " Agreement Detail  ","A",47
    DATA " Cash Journal      ","C",47
    DATA " Past Due Quick    ","P",47
    DATA " Past Due Summary  ","P",47
    DATA " Past Due Detail   ","P",47
    DATA " Store Delinquency ","S",47
    DATA " Store Activities  ","S",47
    DATA " Daily Transaction ","D",47
    DATA " Bad Checks Report ","B",47
    DATA " Address Labels    ","A",47

    DATA "Menu-End","",55,19
    DATA " Day Closing   ","D",56
    DATA " Week Closing  ","W",56
    DATA " Month Closing ","M",56
    DATA " Year Closing  ","Y",56
    DATA " Transactions  ","T",56
    DATA " View Activity ","A",61
    DATA " DelinQuency   ","Q",61
    DATA " Communication ","C",56
    DATA " Pager Service ","P",56
    DATA " Special Menu  ","M",64
    DATA " Recap Weekday ","R",56
    DATA " System Date   ","S",56
    DATA " Format Disk   ","F",56
    DATA " Purge Records ","P",56
    DATA " Backup Data   ","B",56
    DATA " Test Backup   ","T",56
    DATA " View Backup   ","V",56
    DATA " Restore Data  ","R",56
    DATA " Terminate     ","T",56

DEFINT L
SUB AgrSort ()
    SortA = 0
    CLOSE
    SHELL "SortF ..\DataFile\Agreemnt.Inx ..\DataFile\Agreemnt.Cus /L12 /+1,10 /Q"
END SUB

SUB CompNam (n$)
    n$ = UCASE$(n$)
    DO: l = LEN(n$)
        n = INSTR(n$, ",")
        IF n = 0 THEN EXIT DO
        n$ = LEFT$(n$, n - 1) + RIGHT$(n$, l - n)
    LOOP
    DO: l = LEN(n$)
        n = INSTR(n$, ".")
        IF n = 0 THEN EXIT DO
        n$ = LEFT$(n$, n - 1) + RIGHT$(n$, l - n)
    LOOP
    DO: l = LEN(n$)
        n = INSTR(n$, " ")
        IF n = 0 THEN EXIT DO
        n$ = LEFT$(n$, n - 1) + RIGHT$(n$, l - n)
    LOOP
END SUB

FUNCTION Confirm (g, f$)
    IF g AND DateDif&(EndDay, DATE$) < 1 THEN
       WinMssg "Confirm not allowed after end of day"
    ELSE
       r = 30
       PCOPY 0, 1
       COLOR 17, 6: LOCATE r + 1, 40: PRINT STRING$(15 + LEN(f$), " ");
       COLOR 17, 6: LOCATE r + 2, 40: PRINT "  Press * to "; f$; "  ";
       COLOR 8, 0: Picture r + 2, POS(0), 1, c$: PRINT c$;
       COLOR 17, 6: LOCATE r + 3, 40: PRINT STRING$(15 + LEN(f$), " ");
       COLOR 8, 0: Picture r + 3, POS(0), 1, c$: PRINT c$;
       COLOR 8, 0: Picture r + 4, 41, 15 + LEN(f$), c$: LOCATE r + 4, 41: PRINT c$;
       SOUND 350, .5: KeyPres f$
       IF f$ = "*" THEN Confirm = 1: SOUND 1200, .3: SOUND 1000, .2
       PCOPY 1, 0
    END IF
END FUNCTION

FUNCTION FindAgr (f$)
   b = 1: t = LOF(17) / LEN(Agx)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #17, m, Agx
      IF f$ = Agx.Agr THEN FindAgr = m: EXIT DO
      IF f$ > Agx.Agr THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindCmb (f$)
   b = 1: t = LOF(2) \ LEN(Cmb)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #2, m, Cmb
      IF f$ = Cmb.Cod THEN FindCmb = 1: EXIT DO
      IF f$ > Cmb.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindCod (f$)
   b = 1: t = LOF(1) \ LEN(Cod)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #1, m, Cod
      IF f$ = Cod.Cod THEN FindCod = 1: EXIT DO
      IF f$ > Cod.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindCus (f$)
   f$ = UCASE$(f$): b = 1: t = LOF(15) \ LEN(Cnx)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #15, m, Cnx
      t$ = UCASE$(Cnx.Nam)
      IF f$ = t$ THEN FindCus = m: GET #13, Cnx.Num, Cus: EXIT DO
      IF f$ > t$ THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

SUB FindGrp (f$)
   IF f$ > "  " THEN
      b = 1: t = LOF(8) \ LEN(Cod)
      DO UNTIL (t < b)
         m = (t + b) \ 2: GET #8, m, Cod
         IF f$ > Cod.Cmb THEN b = m + 1 ELSE t = m - 1
      LOOP
      DO WHILE f$ > Cod.Cmb AND NOT EOF(8)
         GET #8, , Cod
      LOOP
      
      qPrint f$, CSRLIN, 9, 0, 1
      DO WHILE f$ = Cod.Cmb
         qPrint Cod.Cod, CSRLIN, 12, 0, 1
         qPrint Cod.Des, CSRLIN, 17, 0, 1
         LOCATE CSRLIN + 1: IF CSRLIN > 36 THEN KeyPres n$
         GET #8, , Cod
         IF f$ <> Cod.Cmb THEN EXIT DO
      LOOP
   END IF
END SUB

FUNCTION FindIni (f$)
   f$ = RTRIM$(f$)
   FOR x = 1 TO LOF(5) \ LEN(Pas)
   GET #5, x, Pas
   IF f$ = Pas.Ini THEN FindIni = 1: EXIT FOR
   NEXT
END FUNCTION

FUNCTION FindPas (f$)
   b = 1: t = LOF(5) \ LEN(Pas)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #5, m, Pas
      IF f$ = Pas.Pas THEN FindPas = m: EXIT DO
      IF f$ > Pas.Pas THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindPrc (f$)
   b = 1: t = LOF(3) \ LEN(Prc)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #3, m, Prc
      IF f$ = Prc.Dis + Prc.Mdl + Prc.Cod THEN FindPrc = m: EXIT DO
      IF f$ > Prc.Dis + Prc.Mdl + Prc.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindSer (f$)
   b = 1: t = LOF(12) \ LEN(Inx)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #12, m, Inx
      IF f$ = Inx.Ser THEN FindSer = m: EXIT DO
      IF f$ > Inx.Ser THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

FUNCTION FindZip (f$)
   b = 1: t = LOF(27) \ LEN(Zip)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #27, m, Zip
      IF f$ = Zip.Cod THEN FindZip = m: EXIT DO
      IF f$ > Zip.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

SUB InvRate (r, c)
    FOR z = 1 TO 3
        qPrint Rat.Mtr(z), r + z, c + 1, 0, 1
        qPrint Rat.Mrt(z), r + z, c + 5, 0, 1
        qPrint Rat.Wtr(z), r + z, c + 13, 0, 1
        qPrint Rat.Wrt(z), r + z, c + 17, 0, 1
    NEXT
END SUB

DEFSNG A-Z
SUB KeyPres (f$)
    DO: f$ = INKEY$: LOOP WHILE f$ = ""
    f$ = UCASE$(f$)
END SUB

SUB OpenFiles ()
    CLOSE 1:  OPEN "..\Master\Codes   .Mas" FOR RANDOM AS #1 LEN = LEN(Cod)
    CLOSE 2:  OPEN "..\Master\Combos  .Mas" FOR RANDOM AS #2 LEN = LEN(Cmb)
    CLOSE 3:  OPEN "..\Master\Prices  .Mas" FOR RANDOM AS #3 LEN = LEN(Prc)
    CLOSE 4:  OPEN "..\Master\Rates   .Mas" FOR RANDOM AS #4 LEN = LEN(Rat)
    CLOSE 5:  OPEN "..\Master\PassWord.Mas" FOR RANDOM AS #5 LEN = LEN(Pas)
    CLOSE 6:  OPEN "..\Master\Stores  .Mas" FOR RANDOM AS #6 LEN = LEN(Sto)
    CLOSE 7:  OPEN "..\Master\Centers .Mas" FOR RANDOM AS #7 LEN = LEN(Ctr)
    CLOSE 8:  OPEN "..\Master\Codes   .Cmb" FOR RANDOM AS #8 LEN = LEN(Cod)
   
    CLOSE 9:  OPEN "..\DataFile\Transact.Hst" FOR RANDOM AS #9 LEN = LEN(Tra)
 
    CLOSE 10: OPEN "..\DataFile\Inventor.Mas" FOR RANDOM AS #10 LEN = LEN(Inv)
    CLOSE 11: OPEN "..\DataFile\Inventor.Inx" FOR RANDOM AS #11 LEN = LEN(Inx)
    CLOSE 12: OPEN "..\DataFile\Inventor.Ser" FOR RANDOM AS #12 LEN = LEN(Inx)
    
    CLOSE 13: OPEN "..\DataFile\Customer.Mas" FOR RANDOM AS #13 LEN = LEN(Cus)
    CLOSE 14: OPEN "..\DataFile\Customer.Inx" FOR RANDOM AS #14 LEN = LEN(Cnx)
    CLOSE 15: OPEN "..\DataFile\Customer.Nam" FOR RANDOM AS #15 LEN = LEN(Cnx)
   
    CLOSE 16: OPEN "..\DataFile\Agreemnt.Mas" FOR RANDOM AS #16 LEN = LEN(Agr)
    CLOSE 17: OPEN "..\DataFile\Agreemnt.Inx" FOR RANDOM AS #17 LEN = LEN(Agx)
    CLOSE 18: OPEN "..\DataFile\Agreemnt.Cus" FOR RANDOM AS #18 LEN = LEN(Agx)

    CLOSE 19: OPEN "..\DataFile\Receipts.Mas" FOR RANDOM AS #19 LEN = LEN(Rct)
    CLOSE 20: OPEN "..\DataFile\Receipts.Inx" FOR RANDOM AS #20 LEN = LEN(Rcx)
    CLOSE 21: OPEN "..\DataFile\Receipts.Agr" FOR RANDOM AS #21 LEN = LEN(Rcx)

    CLOSE 22: OPEN "..\DataFile\Activity.Mas" FOR RANDOM AS #22 LEN = LEN(Act)
    CLOSE 23: OPEN "..\DataFile\Printing.Log" FOR APPEND AS #23

    CLOSE 24: OPEN "LPT2:" FOR OUTPUT AS #24
   'CLOSE 25: OPEN "drive A: Service and transfer RANDOM AS #25
    CLOSE 26: OPEN "..\DataFile\Transact.Log" FOR APPEND AS #26
    CLOSE 27: OPEN "..\DataFile\ZipCodes.Mas" FOR RANDOM AS #27 LEN = LEN(Zip)
    CLOSE 28: OPEN "..\DataFile\Expenses.Mas" FOR RANDOM AS #28 LEN = LEN(Pet)
END SUB

DEFINT A-Z
FUNCTION PassWord (p$)
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "User-ID", e)
    COLOR 28: LOCATE 20, 47
    SOUND 860, .3
    FOR x = 1 TO 4
    DO: s$ = INKEY$: LOOP WHILE s$ = ""
    PRINT "þ";
    IF s$ = CHR$(27) THEN PCOPY 1, 0: EXIT FUNCTION
    f$ = f$ + s$
    NEXT
    PCOPY 1, 0
    f$ = CHR$(ASC(MID$(f$, 1, 1)) + 34) + CHR$(ASC(MID$(f$, 2, 1)) + 70) + CHR$(ASC(MID$(f$, 3, 1)) + 81) + CHR$(ASC(MID$(f$, 4, 1)) + 63)
    IF FindPas(f$) THEN
       IF Pas.Lvl >= p$ THEN PassWord = 1 ELSE WinMssg "Permission denied"
    ELSE
       WinMssg "Invalid Password"
    END IF
END FUNCTION

FUNCTION Percent ()
         Percent = Agr.Tri / (VAL(Agr.Mtr) * VAL(Agr.Mrt))
END FUNCTION

SUB RctSort ()
    PCOPY 0, 2
    SortR = 0
    CLOSE
    'SHELL "SortF ..\DataFile\Receipts.Inx ..\DataFile\Receipts.AGR /L10 /+4,1 /+3,1 /+2,1 /+1,1 /+8,1 /+7,1 /+6,1 /+5,1 /Q"
    SHELL "RPSORT /Q ..\DataFile\Receipts.Inx ..\DataFile\Receipts.AGR /F10 /+1:4F /+5:4F"
    PCOPY 2, 0
END SUB

DEFSNG A-Q, S-Z
FUNCTION Response (f$)
    SOUND 800, .2
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, f$, e)
    KeyPres f$
    PCOPY 1, 0
    IF f$ = CHR$(27) THEN Response = 1
END FUNCTION

DEFSNG R
SUB Statlin (f$)
    qPrint SPACE$(25 - LEN(f$)) + f$, 43, 55, 0, 1
END SUB

DEFSTR T
FUNCTION TraStat (f$)
     SELECT CASE f$
            CASE "AA": TraStat = "A. Paid-out[R] "
            CASE "AB": TraStat = "A. Loaner Back "
            CASE "AC": TraStat = "A. Charge-off  "
            CASE "AE": TraStat = "A. EZA days    "
            CASE "AF": TraStat = "A. Free Days   "
            CASE "AK": TraStat = "A. Cash Sale   "
            CASE "AL": TraStat = "A. Loaner      "
            CASE "AM": TraStat = "A. Reroute     "
            CASE "AN": TraStat = "A. New Issue   "
            CASE "AO": TraStat = "A. Paid-out[A] "
            CASE "AP": TraStat = "A. Promissary  "
            CASE "AR": TraStat = "A. Reinstate   "
            CASE "AS": TraStat = "A. Swap        "
            CASE "AT": TraStat = "A. Terminate   "
            CASE "AV": TraStat = "A. Void        "
            CASE "AW": TraStat = "A. Waiver      "
            
            CASE "MC": TraStat = "M. Override    "
            CASE "ME": TraStat = "M. Erase Item  "
            CASE "MG": TraStat = "M. Give-away   "
            CASE "MI": TraStat = "M. Service in  "
            CASE "MN": TraStat = "M. Receive new "
            CASE "MM": TraStat = "M. Transfer man"
            CASE "MO": TraStat = "M. Service out "
            CASE "MP": TraStat = "M. Scrap       "
            CASE "MR": TraStat = "M. Transfer in "
            CASE "MS": TraStat = "M. Service site"
            CASE "MT": TraStat = "M. Transfer out"
            CASE "MU": TraStat = "M. Modify Item "

            CASE "PC": TraStat = "P. Petty Cash  "

            CASE "RB": TraStat = "R. Bad Check   "
            CASE "RN": TraStat = "R. New Issue   "
            CASE "RP": TraStat = "R. Paid Out    "
            CASE "RV": TraStat = "R. Void        "
            CASE "RR": TraStat = "R. Reprint     "
            CASE "RT": TraStat = "R. Telephone   "
            CASE "RC": TraStat = "R. Cell Phone  "

            CASE "CN": TraStat = "C. New         "
            CASE "CM": TraStat = "C. Edit        "
            CASE ELSE: TraStat = "?. Activity    "
     END SELECT
END FUNCTION

FUNCTION TraType ()
   SELECT CASE Tra.Typ
    CASE "AA": TraType = "& & & Paid-out[R] agreement & for & route & {   paid-out: &}"
    CASE "AB": TraType = "& & & Loaner Back "
    CASE "AC": TraType = "& & & Charged-off agreement & for & route & {charged-off: &}{in-service: &}{idled: &}"
    CASE "AE": TraType = "& & & & EZA  days on agreement & for & route &"
    CASE "AF": TraType = "& & & & Free days on agreement & for & route &"
    CASE "AK": TraType = "& & & Issued Cash agreement & for & route & {  cash sale: & & & &}"
    CASE "AL": TraType = "& & & Loaner   on agreement & for & route & {&}"
    CASE "AM": TraType = "& & & Rerouted    agreement & for & route & to route &"
    CASE "AN": TraType = "& & & Issued  new agreement & for & route & {put on-rent: & & & &}"
    CASE "AO": TraType = "& & & Paid-out[F] agreement & for & route & {   paid-out: &}"
    CASE "AP": TraType = "& & & Issue promissary note & for & route & {became idle: & & & &}"
    CASE "AR": TraType = "& & & Reinstated  agreement & for & route & {put on-rent: & & & &}"
    CASE "AS": TraType = "& & & Swapped  on agreement & for & route & {    swapped: &}"
    CASE "AT": TraType = "& & & Terminated  agreement & for & route & {became idle: &}{in-service: &}"
    CASE "AU": TraType = "& & & Unpaid-out  agreement & for & route & {put on-rent: &}"
    CASE "AV": TraType = "& & & Voided      agreement & for & route & {became idle: & & & &}"
    CASE "AW": TraType = "& & & Waiver  on  agreement & for & route & was &"

    CASE "MC": TraType = "& & & Rate code & override on & & & Model: & Dist: &"
    CASE "MG": TraType = "& & & Charged-off as  giveaway & & & Model: & Dist: &"
    CASE "MI": TraType = "& & & Returned  from  service  & & & Model: & Dist: & Center: &"
    CASE "MN": TraType = "& & & Received new merchandise & & & Model: & Dist: &"
    CASE "MM": TraType = "& & & Transferred-in manually  & & & Model: & Dist: &   from: &"
    CASE "MO": TraType = "& & & Sent to service          & & & Model: & Dist: & Center: &"
    CASE "MP": TraType = "& & & Charged-off as scrap     & & & Model: & Dist: &"
    CASE "MR": TraType = "& & & Transferred-in           & & & Model: & from: & &"
    CASE "MS": TraType = "& & & Serviced on-site         & & & Model: & Dist: &     by: &"
    CASE "MT": TraType = "& & & Transferred-out          & & & Model: & Dist: &     to: &"
    CASE "MU": TraType = "& & & Modified inventory item  & & & Model: & Dist: & status: ##"

    CASE "PC": TraType = "& & & Petty Cash & Amount $&"

    CASE "RB": TraType = "& & & Badcheck  $& receipt & agreement & for &"
    CASE "RC": TraType = "& & & Cash Sale $& receipt & agreement & for &"
    CASE "RN": TraType = "& & & Issued    $& receipt & agreement & for &"
    CASE "RV": TraType = "& & & Voided    $& receipt & agreement & for &"
    CASE "RR": TraType = "& & & Reprint   $& receipt & agreement & for &"
    CASE "RT": TraType = "& & & Telephone Receipts $&"

    CASE "CN": TraType = "& & & Added new customer number & &"
    CASE "CM": TraType = "& & & Modified  customer number & &"

    CASE "OP": TraType = "& & & Purged:#### agreements, #### receipts, and #### inventory items"
    CASE "OB": TraType = "& & & Backup data files"
    CASE "OI": TraType = "& & & Logged  in RTO"
    CASE "OO": TraType = "& & & Logged out RTO"
    CASE "OC": TraType = "& & & Inter-Store communications"
    CASE "OE": TraType = "& & & Finished communications"
    CASE "OG": TraType = "& & & Started Pager Service"
    CASE "OF": TraType = "& & & Finished Pager Service"
    CASE ELSE: TraType = "& & & Unknown activity"
   END SELECT
END FUNCTION

DEFINT A-Z
FUNCTION WinAgrt ()
    CustAgr Cus.Num, f, t
    IF f = 0 THEN
       WinMssg "No agreement for this customer"
       WinAgrt = 1
       EXIT FUNCTION
    END IF
    r = 12: b = f: n = f
    PCOPY 0, 2: CALL MhDisplayByName(0, 0, "List-AGR", e)
    GOSUB a2
a1: GET #18, n, Agx: GET #16, Agx.Num, Agr
    COLOR 15, 5: LOCATE r, 32: PRINT USING "&ณ & ณ&ณ&ณ& "; Agr.Num; Agr.Sta; Agr.Dat; Agr.Cod; Agr.Cmb
    KeyPres f$
    COLOR 17, 5: LOCATE r, 32: PRINT USING "&ณ & ณ&ณ&ณ& "; Agr.Num; Agr.Sta; Agr.Dat; Agr.Cod; Agr.Cmb
    SELECT CASE f$
    CASE CHR$(0) + "P"
         IF n + 1 > t GOTO a1 ELSE n = n + 1
         IF r + 1 > 26 THEN CALL Scroll(32, 12, 62, 26, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"
         IF n - 1 < f GOTO a1 ELSE n = n - 1
         IF r - 1 < 12 THEN CALL BkScroll(32, 12, 62, 26, 1) ELSE r = r - 1
    CASE CHR$(13)
         IF Yes = 2 THEN IF WinRcts GOTO a1
         PCOPY 2, 0
         EXIT FUNCTION
    CASE ELSE
         WinAgrt = 1
         PCOPY 2, 0
         EXIT FUNCTION
    END SELECT
    GOTO a1

a2: COLOR 17, 5: LOCATE 12
    FOR x = b TO t
    GET #18, x, Agx: GET #16, Agx.Num, Agr
    LOCATE , 32: PRINT USING "&ณ & ณ&ณ&ณ& "; Agr.Num; Agr.Sta; Agr.Dat; Agr.Cod; Agr.Cmb
    IF CSRLIN > 26 THEN EXIT FOR
    NEXT
    RETURN
END FUNCTION

FUNCTION WinCntr ()
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-CTR", e)
    b = 1: n = b: r = 5: GOSUB T2
T1: GET #7, n, Ctr
    COLOR 27, 0: LOCATE r, 41: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam;
    KeyPres n$
    COLOR 8, 3:  LOCATE r, 41: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam;
    SELECT CASE n$
    CASE CHR$(13)
        IF Ctr.Nam > "!" THEN
           PCOPY 1, 0
           EXIT FUNCTION
        END IF
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > 99 GOTO T1 ELSE n = n + 1
        IF r + 1 > 19 THEN CALL Scroll(41, 5, 69, 19, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 GOTO T1 ELSE n = n - 1
        IF r - 1 < 5 THEN CALL BkScroll(41, 5, 69, 19, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        b = n - r - 10: IF b < 1 THEN b = 1
        GOSUB T2: n = b + r - 5
    CASE CHR$(0) + "Q" 'PgDn
        b = 20 - r + n
        IF b > 85 THEN b = 85: n = 80 + r ELSE n = n + 15
        GOSUB T2
    CASE CHR$(0) + "G" 'Home
        b = 1: GOSUB T2
        n = r - 4
    CASE CHR$(0) + "O" 'End
        b = 85: GOSUB T2
        n = 80 + r
    CASE ELSE
        WinCntr = 1
        PCOPY 1, 0
        EXIT FUNCTION
    END SELECT
    GOTO T1
  
T2: COLOR 8, 3: LOCATE 5
    FOR x = b TO b + 14: GET #7, x, Ctr
    LOCATE , 41: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam
    NEXT
    RETURN
END FUNCTION

FUNCTION WinComb ()
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-CMB", e)
    b = 1: t = LOF(2) \ LEN(Cmb): r = 7: n = 1: GOSUB C2
c1: GET #2, n, Cmb
    COLOR 27, 1: LOCATE r, 43: PRINT USING "&ณ&"; Cmb.Cod; Cmb.Des;
    KeyPres n$
    COLOR 17, 3: LOCATE r, 43: PRINT USING "&ณ&"; Cmb.Cod; Cmb.Des;
    SELECT CASE n$
    CASE CHR$(0) + "P", "2"'--Dn
        IF n + 1 > t GOTO c1 ELSE n = n + 1
        IF r + 1 > 21 THEN CALL Scroll(43, 7, 71, 21, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", "8"'--Up
        IF n - 1 < 1 GOTO c1 ELSE n = n - 1
        IF r - 1 < 7 THEN CALL BkScroll(43, 7, 71, 21, 1) ELSE r = r - 1
    CASE CHR$(0) + "I", "9"'--PgUp
        b = n - r - 8
        IF b < 1 THEN b = 1
        GOSUB C2: n = x + r - 21
    CASE CHR$(0) + "Q", "3"'--PgDn
        b = n - r + 22
        IF b + 14 > t THEN b = t - 14
        GOSUB C2: n = x + r - 21
    CASE CHR$(0) + "G", "7"'--Home
        b = 1: GOSUB C2
        n = r - 6
    CASE CHR$(0) + "O", "1"'--End
        b = t - 14: GOSUB C2
        n = t - 21 + r
    CASE CHR$(27), "5"
        WinComb = 1
        GOTO C3
    CASE CHR$(13)
        Agr.Cmb = Cmb.Cod
        GOTO C3
    CASE CHR$(0) + "K", CHR$(0) + "M", "4", "6"'--left
        PCOPY 0, 2: CALL MhDisplayByName(0, 0, "Combo", e)
        GET #4, VAL(Cmb.Rat), Rat
        qPrint Cmb.Cod + " " + Cmb.Des, 12, 10, 0, 1
        InvRate 17, 42
        LOCATE 16
        FOR s = 1 TO 4: FindGrp (Cmb.Cde(s)): NEXT
        KeyPres f$
        PCOPY 2, 0
    CASE ELSE
    END SELECT
    GOTO c1

C2: COLOR 17, 3: LOCATE 7
    FOR x = b TO t: GET #2, x, Cmb
    LOCATE , 43: PRINT USING "&ณ&"; Cmb.Cod; Cmb.Des
    IF CSRLIN > 21 THEN EXIT FOR
    NEXT
    RETURN
C3: PCOPY 1, 0
END FUNCTION

FUNCTION WinCust (h$)
    x = FindCus(Cus.Nam)
    n = LOC(15)
    t = LOF(15) \ LEN(Cnx)
    b = n
    r = 4
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-CUS", e)
    GOSUB w2
w1: GET #15, n, Cnx
    COLOR 15, 1: LOCATE r, 47: PRINT USING "&"; Cnx.Nam;
    KeyPres f$
    COLOR 17, 7: LOCATE r, 47: PRINT USING "&"; Cnx.Nam;
    SELECT CASE f$
    CASE CHR$(0) + "P", "2"
         IF n + 1 > t GOTO w1 ELSE n = n + 1
         IF r + 1 > 18 THEN CALL Scroll(47, 4, 72, 18, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", "8"
         IF n - 1 < 1 GOTO w1 ELSE n = n - 1
         IF r - 1 < 4 THEN CALL BkScroll(47, 4, 72, 18, 1) ELSE r = r - 1
    CASE CHR$(0) + "I", "9"'PgUp
         IF t > 15 THEN
            b = n - r - 11
            IF b < 1 THEN b = 1
            GOSUB w2: n = x + r - 18
         END IF
    CASE CHR$(0) + "Q", "3"'PgDn
         IF t > 15 THEN
            b = 19 - r + n
            IF b + 14 > t THEN b = t - 14
            GOSUB w2: n = x + r - 18
         END IF
    CASE CHR$(0) + "G", "7"'Home
         b = 1: GOSUB w2: n = r - 3
    CASE CHR$(0) + "O", "1"'End
         b = t - 14: IF b < 1 THEN b = 1
         GOSUB w2: n = b + r - 4
    CASE CHR$(13)
         GET #13, Cnx.Num, Cus
         IF Yes THEN IF WinAgrt GOTO w1
         GOTO w3
    CASE "N"
         Cnx.Num = 1 + LOF(15) \ LEN(Cnx)
         Tra.Typ = "CN"
         CusEdit "9"
    CASE "M"
         Tra.Typ = "CM"
         CusEdit h$
    CASE ELSE
         GET #13, Cnx.Num, Cus
         WinCust = 1: GOTO w3
    END SELECT
    GOTO w1

w2: COLOR 17, 7: LOCATE 4
    FOR x = b TO t
    GET #15, x, Cnx
    LOCATE , 47: PRINT USING "&"; Cnx.Nam
    IF CSRLIN > 18 THEN EXIT FOR
    NEXT
    RETURN
w3: PCOPY 1, 0
END FUNCTION

FUNCTION WinItem (c, i)
    n = LOC(12)
    t = LOF(12) \ LEN(Inx)
    b = n
    r = 7
    s = 12
    f = 11
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-ITM", e)
    GOSUB i2
I1: GET #f, n, Inx: GET #10, Inx.Num, Inv
    COLOR 16, 3: LOCATE r, 45: PRINT USING "&ณ&ณ&ณ&"; Inx.Typ + Inx.Grp; Inx.Cod; Inx.Mdl; Inx.Ser;
    KeyPres n$
    COLOR 16 + Inv.Sta, 7
    LOCATE r, 45: PRINT Inx.Typ + Inx.Grp;
    LOCATE r, 49: PRINT Inx.Cod;
    LOCATE r, 54: PRINT Inx.Mdl;
    LOCATE r, 65: PRINT Inx.Ser;
    SELECT CASE n$
    CASE CHR$(0) + "P", "2"
        IF n + 1 > t GOTO I1 ELSE n = n + 1
        IF r + 1 > 21 THEN CALL Scroll(45, 7, 74, 21, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", "8"
        IF n - 1 < 1 GOTO I1 ELSE n = n - 1
        IF r - 1 < 7 THEN CALL BkScroll(45, 7, 74, 21, 1) ELSE r = r - 1
    CASE CHR$(0) + "I", "9"
        b = n - r - 8
        IF b < 1 THEN b = 1
        GOSUB i2: n = x + r - 21
    CASE CHR$(0) + "Q", "3"
        b = n - r + 22
        IF b + 14 > t THEN b = t - 14
        GOSUB i2: n = x + r - 21
    CASE CHR$(0) + "G", "7"
        b = 1: GOSUB i2: n = r - 6
    CASE CHR$(0) + "O", "1"
        b = t - 14: GOSUB i2: n = t - 21 + r
    CASE CHR$(13)
        GET #10, Inx.Num, Inv
        WinItem = 0: GOTO I3
    CASE CHR$(27), "5"
        WinItem = 1: GOTO I3
    CASE "I"
        SWAP f, s
        b = n + 7 - r: GOSUB i2
    CASE CHR$(0) + "K", CHR$(0) + "M", "4", "6"'--left
        PCOPY 0, 2: CALL MhDisplayByName(0, 0, "Inventory", e)
        IF InvRtrv THEN WinMssg "Code not found": GOTO I1
        COLOR 11, 5
        qPrint Cod.Typ + "ณ" + Inv.Cod + "ณ" + Cod.Des, 13, 4, 0, 1
        qPrint Status(Inv.Sta), 15, 23, 0, 1
        qPrint Inv.Mdl, 17, 23, 0, 1
        qPrint Inv.Ser, 18, 23, 0, 1
        IF Pas.Lvl > "6" THEN
           LOCATE 21, 23: PRINT USING "#######"; Inv.Tdr;
           LOCATE 22, 23: PRINT USING "####.##"; Inv.Tin / 100;
           qPrint Inv.Prc, 24, 23, 0, 1
           qPrint Cod.Fmv, 25, 26, 0, 1
           qPrint Cod.Dfm, 26, 26, 0, 1
           qPrint Cod.Dtr, 27, 26, 0, 1
           qPrint Cod.Dep, 28, 25, 0, 1
        END IF
        qPrint Inv.Par, 30, 24, 0, 1
        qPrint Inv.Lab, 31, 24, 0, 1
        qPrint Inv.Dtp, 35, 3, 0, 1
        qPrint Inv.Dtr, 35, 14, 0, 1
        qPrint Inv.Dta, 35, 25, 0, 1
        InvRate 16, 36
        KeyPres f$
        PCOPY 2, 0
    CASE "F"
        FOR x = n + 1 TO t
        GET #f, x, Inx
        GET #10, Inx.Num, Inv
        IF Inv.Sta = 0 THEN
           SELECT CASE c
           CASE 0 'any idle items
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR
           CASE 1 'for loaners, check group
                IF Inx.Grp = Cob(i) THEN
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR
                END IF
           CASE 2 'for reinstatement, same value
                IF Inx.Grp = Cob(i) AND ABS(Inv.Tdr - Cst(i)) / Cst(i) <= .2 THEN
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR
                END IF
           CASE 3 'for a combo item
                IF FindCod(Inv.Cod) = 0 THEN ERROR 200
                IF Cod.Cmb = Cmb.Cde(i) THEN
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR
                END IF
           END SELECT
        END IF
        NEXT
    END SELECT
    GOTO I1
i2: COLOR 16, 3: LOCATE 7
    FOR x = b TO t
    GET #f, x, Inx: GET #10, Inx.Num, Inv
    COLOR 16 + Inv.Sta, 7
    LOCATE , 45: PRINT Inx.Typ + Inx.Grp;
    LOCATE , 49: PRINT Inx.Cod;
    LOCATE , 54: PRINT Inx.Mdl;
    LOCATE , 65: PRINT Inx.Ser
    IF CSRLIN > 21 THEN EXIT FOR
    NEXT
    RETURN
I3: PCOPY 1, 0
END FUNCTION

DEFSNG A-Q, S-Z
SUB WinMssg (f$)
    r = 16
    PCOPY 0, 3
    COLOR 17, 7
    LOCATE r + 1, 31: PRINT STRING$(4 + LEN(f$), " ");
    LOCATE r + 2, 31: PRINT "  "; f$; "  ";
    LOCATE r + 3, 31: PRINT STRING$(4 + LEN(f$), " ");
    COLOR 8, 0
    Picture r + 2, 30, 1, b$: LOCATE r + 2, 30: PRINT b$;
    Picture r + 3, 30, 1, b$: LOCATE r + 3, 30: PRINT b$;
    Picture r + 4, 30, 4 + LEN(f$), b$: LOCATE r + 4, 30: PRINT b$;
    SOUND 350, 3: KeyPres f$
    PCOPY 3, 0
END SUB

DEFINT A-Q, S-Z
FUNCTION WinPric ()
    n = FindPrc(Inv.Dis + Inv.Mdl + Inv.Cod)
    IF n = 0 THEN n = 1
    t = LOF(3) \ LEN(Prc)
    b = n
    r = 7
    IF b > t - 13 THEN b = t - 13: r = 20 - (t - n)
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-PRC", e)
    GOSUB p2
p1: GET #3, n, Prc
    COLOR 15, 3: LOCATE r, 50: PRINT USING "& ณ&ณ&"; Prc.Dis; Prc.Mdl; Prc.Cod;
    KeyPres n$
    COLOR 17, 3: LOCATE r, 50: PRINT USING "& ณ&ณ&"; Prc.Dis; Prc.Mdl; Prc.Cod;
    SELECT CASE n$
    CASE CHR$(13)
        IF Yes AND Prc.Dis <> Inv.Dis GOTO p1 ELSE PCOPY 1, 0: EXIT FUNCTION
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > t GOTO p1 ELSE n = n + 1
        IF r + 1 > 20 THEN CALL Scroll(50, 7, 76, 20, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 GOTO p1 ELSE n = n - 1
        IF r - 1 < 7 THEN CALL BkScroll(50, 7, 76, 20, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        b = n - r - 7: IF b < 1 THEN b = 1
        GOSUB p2: n = b + r - 7
    CASE CHR$(0) + "Q" 'PgDn
        b = 21 - r + n
        IF b > t - 13 THEN b = t - 13: n = t - 20 + r ELSE n = n + 14
        GOSUB p2
    CASE CHR$(0) + "G" 'Home
        b = 1: GOSUB p2
        n = r - 6
    CASE CHR$(0) + "O" 'End
        b = t - 13: GOSUB p2
        n = t - 20 + r
    CASE ELSE
        WinPric = 1
        PCOPY 1, 0
        EXIT FUNCTION
    END SELECT
    GOTO p1
  
p2: COLOR 17, 3: LOCATE 7
    FOR x = b TO t: GET #3, x, Prc
    LOCATE , 50: PRINT USING "& ณ&ณ&"; Prc.Dis; Prc.Mdl; Prc.Cod
    IF CSRLIN > 20 THEN EXIT FOR
    NEXT
    RETURN
END FUNCTION

FUNCTION WinRate ()
    PCOPY 0, 1
    COLOR 1, 7: LOCATE 7
    LOCATE , 20: PRINT "ษอออออออออออัอออออออออออป"
    LOCATE , 20: PRINT "บ  MONTHLY  ณ  WEEKLY   บ"
    LOCATE , 20: PRINT "วฤฤฤฤยฤฤฤฤฤฤลฤฤฤฤยฤฤฤฤฤฤถ"
    LOCATE , 20: PRINT "บTermณ Rate ณTermณ Rate บ"
    LOCATE , 20: PRINT "วฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤลฤฤฤฤฤฤถ"
    m = (Trm(1) * Dis(1) + Trm(2) * Dis(2) + Trm(3) * Dis(3) + Trm(4) * Dis(4)) / 100
    w = m * DaysAvg \ 7
    FOR z = 1 TO 3                                         'rates and load
        IF Rat.Mtr(z) < "  1" THEN EXIT FOR                'droping empty
        Rats(1, z) = VAL(Rat.Mtr(z)) - m                   'modified terms
        Rats(2, z) = VAL(Rat.Wtr(z)) - w                   'modified terms
        LOCATE , 20
        PRINT USING "บ ###ณ&ณ ###ณ&บ"; Rats(1, z); Rat.Mrt(z); Rats(2, z); Rat.Wrt(z)
    NEXT
    LOCATE , 20: PRINT "ศออออฯออออออฯออออฯออออออผ"
    t = z - 1: z = 1
r1: COLOR 7, 0: LOCATE 11 + z, 21: PRINT USING " ###ณ&ณ ###ณ&"; Rats(1, z); Rat.Mrt(z); Rats(2, z); Rat.Wrt(z);
    KeyPres f$
    COLOR 1, 7: LOCATE 11 + z, 21: PRINT USING " ###ณ&ณ ###ณ&"; Rats(1, z); Rat.Mrt(z); Rats(2, z); Rat.Wrt(z);
    SELECT CASE f$
    CASE CHR$(0) + "P"
        IF z + 1 > t GOTO r1 ELSE z = z + 1
    CASE CHR$(0) + "H"
        IF z - 1 < 1 GOTO r1 ELSE z = z - 1
    CASE CHR$(13)
        WinRate = z: GOTO r2
    CASE ELSE
        WinRate = 0: GOTO r2
    END SELECT
    GOTO r1
r2: PCOPY 1, 0
END FUNCTION

FUNCTION WinRcts ()
    RctAgrt Agx.Agr, f, t
    IF f = 0 THEN
       WinMssg "No receipts for this agreement"
       WinRcts = 1
       EXIT FUNCTION
    END IF
    r = 20: b = f: n = f
    PCOPY 0, 3: CALL MhDisplayByName(0, 0, "List-RCT", e)
    GOSUB m2
m1: GET #21, n, Rcx: GET #19, Rcx.Num, Rct
    COLOR 15, 1: LOCATE r, 23: PRINT USING "&ณ&ณ&ณ&"; Rct.Num; Rct.Ddt; Rct.Dat; Rct.Amt;
    KeyPres f$
    COLOR 27, 1: LOCATE r, 23: PRINT USING "&ณ&ณ&ณ&"; Rct.Num; Rct.Ddt; Rct.Dat; Rct.Amt;
    SELECT CASE f$
    CASE CHR$(0) + "P"
         IF n + 1 > t GOTO m1 ELSE n = n + 1
         IF r + 1 > 34 THEN CALL Scroll(23, 20, 58, 34, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"
         IF n - 1 < f GOTO m1 ELSE n = n - 1
         IF r - 1 < 20 THEN CALL BkScroll(23, 20, 58, 34, 1) ELSE r = r - 1
    CASE CHR$(13)
         PCOPY 3, 0
         EXIT FUNCTION
    CASE ELSE
         WinRcts = 1
         PCOPY 3, 0
         EXIT FUNCTION
    END SELECT
    GOTO m1

m2: COLOR 27, 1: LOCATE 20
    FOR x = b TO t
    GET #21, x, Rcx: GET #19, Rcx.Num, Rct
    LOCATE , 23: PRINT USING "&ณ&ณ&ณ&"; Rct.Num; Rct.Ddt; Rct.Dat; Rct.Amt
    IF CSRLIN > 34 THEN EXIT FOR
    NEXT
    RETURN
END FUNCTION

FUNCTION WinStor ()
    PCOPY 0, 1: : CALL MhDisplayByName(0, 0, "List-STO", e)
    Bot = 1: Top = 99: r = 5: n = 1: GOSUB S2
s1: GET #6, n, Sto
    COLOR 15, 3: LOCATE r, 41: PRINT USING "&ณ&"; Sto.Num; Sto.Nam;
    KeyPres n$
    COLOR 0, 7: LOCATE r, 41:  PRINT USING "&ณ&"; Sto.Num; Sto.Nam;
    SELECT CASE n$
    CASE CHR$(13)
        IF Sto.Nam > "!" THEN
           PCOPY 1, 0
           EXIT FUNCTION
        END IF
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > 99 GOTO s1 ELSE n = n + 1
        IF r + 1 > 19 THEN CALL Scroll(41, 5, 69, 19, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 GOTO s1 ELSE n = n - 1
        IF r - 1 < 5 THEN CALL BkScroll(41, 5, 69, 19, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        Bot = n - r - 10: IF Bot < 1 THEN Bot = 1
        GOSUB S2: n = Bot + r - 5
    CASE CHR$(0) + "Q" 'PgDn
        Bot = 20 - r + n
        IF Bot > 85 THEN Bot = 85: n = 80 + r ELSE n = n + 15
        GOSUB S2
    CASE CHR$(0) + "G" 'Home
        Bot = 1: GOSUB S2
        n = r - 4
    CASE CHR$(0) + "O" 'End
        Bot = 85: GOSUB S2
        n = 80 + r
    CASE ELSE
        WinStor = 1
        PCOPY 1, 0
        EXIT FUNCTION
    END SELECT
    GOTO s1
   
S2: COLOR 0, 7: LOCATE 5
    FOR x = Bot TO Bot + 14
    GET #6, x, Sto
    LOCATE , 41: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
    NEXT
    RETURN
END FUNCTION

FUNCTION WinZipC ()
    t = LOF(27) \ LEN(Zip)
    b = 1
    n = 1
    r = 13
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-ZIP", e)
    GOSUB z2
z1: GET #27, n, Zip
    COLOR 16, 3: LOCATE r, 51: PRINT USING "  & ณ  &  ณ #####"; Zip.Mrk; Zip.Cod; Zip.Num;
    KeyPres n$
    COLOR 16, 7: LOCATE r, 51: PRINT USING "  & ณ  &  ณ #####"; Zip.Mrk; Zip.Cod; Zip.Num;
    SELECT CASE n$
    CASE CHR$(0) + "P", "2"
        IF n + 1 > t GOTO z1 ELSE n = n + 1
        IF r + 1 > 27 THEN CALL Scroll(51, 13, 71, 27, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", "8"
        IF n - 1 < 1 GOTO z1 ELSE n = n - 1
        IF r - 1 < 13 THEN CALL BkScroll(51, 13, 71, 27, 1) ELSE r = r - 1
    CASE CHR$(0) + "I", "9"
        b = n - r - 2
        IF b < 1 THEN b = 1
        GOSUB z2: n = x + r - 27
    CASE CHR$(0) + "Q", "3"
        b = n - r + 28
        IF b + 14 > t THEN b = t - 14
        GOSUB z2: n = x + r - 27
    CASE CHR$(0) + "G", "7"
        b = 1: GOSUB z2: n = r - 12
    CASE CHR$(0) + "O", "1"
        b = t - 14: GOSUB z2: n = t - 27 + r
    CASE CHR$(0) + "K", "4"
        Zip.Mrk = " "
        PUT #27, n, Zip
    CASE CHR$(0) + "M", "6"
        Zip.Mrk = "*"
        PUT #27, n, Zip
    CASE CHR$(13)
        WinZipC = 0: GOTO z3
    CASE CHR$(27), "5"
        WinZipC = 1: GOTO z3
    END SELECT
    GOTO z1
z2: COLOR 16, 3: LOCATE 13
    FOR x = b TO t
    GET #27, x, Zip
    COLOR 16, 7:  LOCATE , 51: PRINT USING "  & ณ  &  ณ #####"; Zip.Mrk; Zip.Cod; Zip.Num
    IF CSRLIN > 27 THEN EXIT FOR
    NEXT
    RETURN
z3: PCOPY 1, 0
END FUNCTION

