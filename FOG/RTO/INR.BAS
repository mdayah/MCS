DEFINT A-Z: DEFLNG L
DECLARE SUB HBor ()
DECLARE SUB HIdle ()
DECLARE SUB HCash ()
DECLARE SUB HAudit ()
DECLARE SUB HRates ()
DECLARE SUB HOrder ()
DECLARE SUB HPrice ()
DECLARE SUB HExting ()
DECLARE SUB HLoaner ()
DECLARE SUB Heading ()
DECLARE SUB HMonDet ()
DECLARE SUB HTimDet ()
DECLARE SUB HStatus ()
DECLARE SUB HServic ()
DECLARE SUB HTraLog ()
DECLARE SUB HBadChk ()
DECLARE SUB HDueDet (r$)
DECLARE SUB HDueQik (r$)
DECLARE SUB AmntDue ()
DECLARE SUB RctLate ()
DECLARE SUB RctTotl ()

DECLARE SUB TranLog ()
DECLARE SUB PyoAmnt (l)
DECLARE SUB mVewOpt (a)
DECLARE SUB mDueOpt (a)
DECLARE SUB AgrDetl (u$)
DECLARE SUB RctAgrt (f$, f, e)
DECLARE SUB CustAgr (f$, f, e)

DECLARE FUNCTION InvRtrv ()

REM $INCLUDE: '..\Include\Config.Dec'

    DIM SHARED Total(2, 20) AS INTEGER

DEFSNG A-K, M-W, Y-Z
SUB ActLine (x, l)
    SELECT CASE x
           CASE 1: FOR z = 1 TO 5: LPRINT USING " #######.##"; l(z) / 100; : NEXT
           CASE 2: LPRINT USING "#######   "; l
           CASE 3: LPRINT USING "###### ##%"; l
    END SELECT
END SUB

DEFINT A-K, M-O, Q-W, Y-Z
DEFLNG P
DEFDBL L
SUB AgrDetl (u$)
    RctAgrt Agx.Agr, f, t
    IF f = 0 THEN WinMssg "No receipts for this agreement": EXIT SUB
    IF Response("Resp-12") THEN EXIT SUB
    Statlin "Agreement Detail"
    PageNum = 0: Heading
    LPRINT TAB(72); "AGREEMENT DETAIL": LPRINT
    PyoAmnt p
    f$ = "&       &: \        \       &: &  &  ####.##        &: ######.##  ######.##  ######.##"
    LPRINT "============================   ===============================   =========================================   =================================================="
    LPRINT "Customer                            Agreement Information                            Term   Rate    Total                         Month      Year       Total  "
    LPRINT "============================   ===============================   ==================  ====  ======  =======   =================  =========  =========  ========="
    LPRINT USING f$; Cus.Nam; " Agreement Num"; Agr.Num; "Standared  Monthly"; Agr.Mst; Agr.Msr; VAL(Agr.Mst) * VAL(Agr.Msr); "        Rent"; Agr.Mri / 100; Agr.Yri / 100; Agr.Tri / 100
    LPRINT USING f$; Cus.Nm1; "     Refere to"; Agr.Ref; "            Weekly"; Agr.Wst; Agr.Wsr; VAL(Agr.Wst) * VAL(Agr.Wsr); "    Discount"; Agr.Mds / 100; Agr.Yds / 100; Agr.Tds / 100
    LPRINT USING f$; Cus.Add; "         Route"; Agr.Rte; "Reduced    Monthly"; Agr.Mtr; Agr.Mrt; VAL(Agr.Mtr) * VAL(Agr.Mrt); "   Sales Tax"; Agr.Mtx / 100; Agr.Ytx / 100; Agr.Ttx / 100
    LPRINT USING f$; Cus.Str; "        Status"; Agr.Sta; "            Weekly"; Agr.Wtr; Agr.Wrt; VAL(Agr.Wtr) * VAL(Agr.Wrt); " Late Charge"; Agr.Mlc / 100; Agr.Ylc / 100; Agr.Tlc / 100
    LPRINT USING f$; Cus.Cty + " " + Cus.Sta + " " + Cus.Zip; "     Frequency"; Agr.Frq; "          EZA Days"; "   "; "      "; Agr.Eza; " Trip Charge"; Agr.Mtc / 100; Agr.Ytc / 100; Agr.Ttc / 100
    LPRINT USING f$; "                          "; "    Issue Date"; Agr.Dat; "         Free Days"; "   "; "      "; Agr.Fre; "Other Charge"; Agr.Mot / 100; Agr.Yot / 100; Agr.Tot / 100
    LPRINT USING f$; "Phone (Home): " + Cus.Tel; "First Due Date"; Agr.Due; "         Late Days"; "   "; "      "; Agr.Ndl; "      Waiver"; Agr.Min / 100; Agr.Yin / 100; Agr.Tin / 100
    LPRINT USING f$; "Phone (Work): " + Cus.Wor; " Next Due Date"; Agr.Nxt; "Number of Receipts"; "   "; "      "; Agr.Rct; "   Referrals"; Agr.Mrf / 100; Agr.Yrf / 100; Agr.Trf / 100
    LPRINT USING f$; "Phone (Work): " + Cus.Wt1; "   Payout Date"; Agr.Pod; "     Payout Amount"; "   "; "      "; p / 100; "   Unapplied"; Agr.Una / 100; Agr.Una / 100; Agr.Una / 100
    LPRINT
    LPRINT USING "Agreement last activity & & "; Agr.Dta; TraStat(Agr.Act);
    LPRINT USING "### EZA day(s) issued by &on &.  "; Agr.EZn; Agr.EZi; Agr.EZd;
    LPRINT USING "### Free day(s) issued by &on &."; Agr.FRn; Agr.FRi; Agr.FRd
    
    LPRINT

    f$ = "&   &   &   &   &  ###   ####.##&"
    LPRINT "=Original=====================================================================   ==Loaner======================================================================"
    LPRINT "Serial Num   Model Num    Code   Bra   Description                %      ICV     Serial Num   Model Num    Code   Bra   Description                %      ICV  "
    LPRINT "==========   ==========   ====   ===   =======================   ===   =======   ==========   ==========   ====   ===   =======================   ===   ======="
    Ser(1) = Agr.It1: Lon(1) = Agr.Lo1: Dis(1) = Agr.Ds1: Mkt(1) = Agr.Fm1
    Ser(2) = Agr.It2: Lon(2) = Agr.Lo2: Dis(2) = Agr.Ds2: Mkt(2) = Agr.Fm2
    Ser(3) = Agr.It3: Lon(3) = Agr.Lo3: Dis(3) = Agr.Ds3: Mkt(3) = Agr.Fm3
    Ser(4) = Agr.It4: Lon(4) = Agr.Lo4: Dis(4) = Agr.Ds4: Mkt(4) = Agr.Fm4
    FOR r = 1 TO 4
    IF FindSer(Ser(r)) THEN
       GET #10, Inx.Num, Inv
       IF FindCod(Inv.Cod) = 0 THEN ERROR 200
       LPRINT USING f$; Inv.Ser; Inv.Mdl; Inv.Cod; Inv.Bra; Cod.Des; Dis(r); Mkt(r) / 100; "   ";
       IF FindSer(Lon(r)) THEN
          GET #10, Inx.Num, Inv
          IF FindCod(Inv.Cod) = 0 THEN ERROR 200
          LPRINT USING f$; Inv.Ser; Inv.Mdl; Inv.Cod; Inv.Bra; Cod.Des; Dis(r); Mkt(r) / 100; ""
       ELSE
          LPRINT
       END IF
    END IF
    NEXT
    LPRINT USING "Accessories: &"; Agr.Acc
    LineNum = LineNum + 23: GOSUB 400
    FOR n = f TO t
    GET #21, n, Rcx
    GET #19, Rcx.Num, Rct
    LPRINT USING "& "; Rct.Dat; Rct.Ddt; Rct.Nxt; Rct.Num;
    LPRINT USING "& "; Rct.Rnt; Rct.Dis; Rct.Tax; Rct.Lat; Rct.Trp; Rct.Oth; Rct.Wav; Rct.Ref; Rct.Crd; Rct.Due; Rct.Amt;
    LPRINT USING "###.##"; VAL(Rct.Amt) - VAL(Rct.Due);
    LPRINT USING " &"; Rct.Typ + Rct.Day + Rct.Cod; Rct.Chk; Rct.Sta
    LOCATE 43, 51: PRINT USING "###%"; 100 * (1 + n - f) / (1 + t - f);
    IF INKEY$ = CHR$(27) THEN EXIT FOR
    LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : Heading: LPRINT TAB(72); "AGREEMENT DETAIL": LPRINT : GOSUB 400
    La = La + VAL(Rct.Rnt)
    Lb = Lb + VAL(Rct.Dis)
    Lc = Lc + VAL(Rct.Tax)
    Ld = Ld + VAL(Rct.Lat)
    Le = Le + VAL(Rct.Trp)
    Lf = Lf + VAL(Rct.Oth)
    Lg = Lg + VAL(Rct.Wav)
    Lh = Lh + VAL(Rct.Ref)
    Li = Li + VAL(Rct.Crd)
    Lj = Lj + VAL(Rct.Due)
    Lk = Lk + VAL(Rct.Amt)
    NEXT
    LPRINT : LPRINT TAB(40);
    LPRINT USING "#####.##"; La; Lb; Lc; Ld; Le; Lf; Lg; Lh; Li; Lj; Lk
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; u$; "Agreement Detail       "; PageNum; "Page(s)."
EXIT SUB
400 LPRINT "==============================================================================================================================================================="
    LPRINT "  Receipt                Next     Rct                    Sales    Late    Trip   Other                          Amount   Amount                                "
    LPRINT "   Date     Due Date   Due Date  Number  Rent   Discnt    Tax     Fees   Charge  Charge  Waiver Reffer   Credit   Due     Paid  Unappl   Period    Payment Type"
    LPRINT "========== ========== ========== ====== ======= ======= ======= ======= ======= ======= ======= ======= ======= ======= ======= ====== ========== ============="
    RETURN
END SUB

DEFINT P
DEFLNG L
SUB AmntDue ()
    Rct.Dat = DATE$
    Rct.Ddt = Agr.Nxt
    Lm = 100 * VAL(Agr.Mrt)                                'monthly rate
    Lw = 100 * VAL(Agr.Wrt)                                'weekly rate
    Ld = Lw / 7                                            'daily rate
    d = DateDif(Rct.Ddt, Rct.Dat)                          'elapsed days
    m = VAL(LEFT$(Rct.Ddt, 2))                             'month number
    y = VAL(RIGHT$(Rct.Ddt, 4))                            'year number
    f$ = "*"                                               'standered marker
    IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29         'check leap year
    WHILE d >= Ndm(m)                                      'repeat until
       IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29      'elapsed number
       d = d - Ndm(m)                                      'of whole months
       nm = nm + 1                                         'are determined
       m = m + 1: IF m > 12 THEN m = 1: y = y + 1          'then determine
    WEND                                                   'the elapsed
    IF Agr.Rct = 0 THEN                                    'first receipt
       Rct.Nxt = Agr.Due                                   'next due date
       RSET Rct.Oth = Agr.Pro                              'add processing fee
       Lr = 100 * VAL(Agr.Pay)                             'agreement rent
       nd = DateDif(Rct.Ddt, Rct.Nxt)                      'elapsed days
    ELSE
       nw = (7 + d) \ 7                                    '   payments
       IF nw > 4 THEN nm = nm + 1: d = d - Ndm(m): nw = (7 + d) \ 7
       nl = nm + (6 + d) \ 7                               'weeks late
       Lr = Lr + nm * Lm + nw * Lw                         'compute rent
       Rct.Oth = "   0.00"
       Rct.Nxt = MonMath$(Rct.Ddt, nm)                     'compute next due
       Rct.Nxt = DayMath$(Rct.Nxt, nw * 7)                 'compute next due
       PyoAmnt Lp                                          'compute payout
       IF DateDif(Agr.Pod, Rct.Nxt) >= 0 OR Lr >= Lp THEN  'if last payment
          Lr = Lp                                          'rent is payout
          Rct.Nxt = Agr.Pod                                'next due date
          f$ = "P"                                         'payout marker
       END IF                                              '-----------------
    END IF
    IF Agr.Wav = "Y" THEN
       IF Lm > 8000 THEN Lv = Lm * nm * .05 ELSE Lv = nm * 400
       IF Lw > 2000 THEN Lv = Lv + (Lw * nw * .05) ELSE Lv = Lv + (nw * 100)
       IF Ld > 2000 THEN Lv = Lv + (Ld * nd * .05) ELSE Lv = Lv + (nd / .07)
    END IF
    
    lt = Lr + Lv + (Lr * VAL(Sto.Stx) / 100) + (100 * nl * VAL(Agr.Lat)) + 100 * (VAL(Agr.Trp) + VAL(Rct.Oth)) - Agr.Una
    LPRINT USING " &"; Rct.Dat; Rct.Ddt; Rct.Nxt;
    LPRINT USING "####.##"; Lr / 100; 0;
    LPRINT USING "####.##"; (Lr * VAL(Sto.Stx)) / 10000;
    LPRINT USING "####.##"; nl * VAL(Agr.Lat);
    LPRINT USING "####.##"; VAL(Agr.Trp); VAL(Rct.Oth); Lv / 100; 0;
    LPRINT USING "####.##"; Agr.Una / 100;
    LPRINT USING "####.##"; lt / 100; 0;
    LPRINT USING "##m#w####d &"; nm; nw; nd; f$;
END SUB

DEFSNG A-Z
SUB HAudit ()
    Heading
    LPRINT TAB(69); "INVENTORY AUDIT REPORT": LPRINT
    LPRINT "                                                            Model       Serial                              Date         Date        Last                    "
    LPRINT "Type  Code  Brand  Distributor        Description           Number      Number      Status     Agreement  Purcahsed    Received    Activity    Activity Type "
    LPRINT "====  ====  =====  ===========  ========================= ==========  ==========  ===========  =========  ==========  ==========  ==========  ==============="
END SUB

SUB HBadChk ()
    Heading
    LPRINT TAB(74); "Bad Checks": LPRINT
    LPRINT "  Receipt   Rct    Agr                                                     Sales     Late     Trip    Other                               Amount   Amount"
    LPRINT "   Date     Num    Num   Mgr  Customer                    Rent   Discount   Tax     Charge   Charge   Charge   Waiver  Referral  Credit    Due      Paid   Code"
    LPRINT "========== ====== ====== === ========================== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ===="
END SUB

SUB HBor ()
    Heading
    LPRINT TAB(74); "BOR LISTING": LPRINT
    LPRINT "                            Agr    Contract                                     Amount         Payout   Payout  Agr. ===ONE==== ===TWO==== ==THREE=== ===FOUR==="
    LPRINT "Customer Name              Number    Date     Date Due      Terms & Rates        Due  Waiver    Date    Amount  Code Serial Num Serial Num Serial Num Serial Num"
    LPRINT "========================== ====== ========== ========== ======================= ============ ========== ======= ==== ========== ========== ========== =========="
END SUB

SUB HCash ()
    Heading
    LPRINT TAB(73); "Cash Journal": LPRINT
    LPRINT " Rct     Agr                                                       Sales     Late     Trip    Other                               Amount   Amount  Payment"
    LPRINT " Num     Num    Mgr   Customer                    Rent   Discount   Tax     Charge   Charge   Charge   Waiver  Referral  Credit    Due      Paid     Code "
    LPRINT "======  ======  ===  ========================== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======="
END SUB

SUB HDueDet (r$)
    Heading
    LPRINT TAB(65); "DETAILED PAST DUE FOR ROUTE "; r$: LPRINT
    LPRINT "                           Days  Rct                            Next                  Sales   Late   Trip  Other                       Amount Amount  Period    "
    LPRINT "Customer Name              Late Number  Rct Date   Due Date   Due Date   Rent   Dsct   Tax   Charge Charge Charge Waiver Referr Credit  Due    Paid   Covered  S"
    LPRINT "========================== ==== ====== ========== ========== ========== ====== ====== ====== ====== ====== ====== ====== ====== ====== ====== ====== ========= ="
END SUB

SUB HDueQik (r$)
    Heading
    LPRINT TAB(66); "QUICK PAST DUE FOR ROUTE "; r$: LPRINT
    LPRINT "                            Agr   Days                          Next                  Sales   Late   Trip  Other                       Amount Amount  Period    "
    LPRINT "Customer Name              Number Late  Rct Date   Due Date   Due Date   Rent   Dsct   Tax   Charge Charge Charge Waiver Referr Credit  Due    Paid   Covered  S"
    LPRINT "========================== ====== ==== ========== ========== ========== ====== ====== ====== ====== ====== ====== ====== ====== ====== ====== ====== ========= ="
END SUB

SUB Heading ()
    IF PrinterReady%(1) = 0 THEN ERROR 208
    WIDTH LPRINT 255            'Supress Line Feed
    Laser = 1
    SELECT CASE Laser
    CASE 0 '=====================EPSON PRINTER
         LPRINT CHR$(27); "@";  'Reset Printer
         LPRINT CHR$(27); "M";  'Elite Pitch
         LPRINT CHR$(15)        'Condensed
    CASE 1 '=====================HP LaserJET
         LPRINT CHR$(27); "E";       'Reset Printer
         LPRINT CHR$(27); "(10U";    'PC-8 character set
         LPRINT CHR$(27); "(s0P";    'Fixed font
         LPRINT CHR$(27); "(s4102T"; 'Letter Gothic
         LPRINT CHR$(27); "(s2B";    'Demi bold
         LPRINT CHR$(27); "(s20H";   'Pitch 20 cpi
         LPRINT CHR$(27); "&l2A";    '8.5" x 11"
         LPRINT CHR$(27); "&l0E";    'Top margin 0 lines
         LPRINT CHR$(27); "&a0L";    'Left margin 0
         LPRINT CHR$(27); "&a164M";  'Right margin
         LPRINT CHR$(27); "&l66F";   'Text length 66 lines
         LPRINT CHR$(27); "&s0C"     'Line wrap
    END SELECT
    PageNum = PageNum + 1: LineNum = 8
    LPRINT "Time: "; TIME$; TAB(64); "F. O. G.   E N T E R P R I S E"; TAB(152); "Page:"; PageNum
    LPRINT "Date: "; DATE$; TAB(79 - LEN(RTRIM$(Sto.Nam)) \ 2); Sto.Nam
    LPRINT
END SUB

SUB HExting ()
    Heading
    LPRINT TAB(65); " EXTINGUISHED INVENTORY LISTING": LPRINT
    LPRINT "                                                            Model       Serial                              Date         Date        Last                    "
    LPRINT "Type  Code  Brand  Distributor        Description           Number      Number      Status     Agreement  Purcahsed    Received    Activity    Activity Type "
    LPRINT "====  ====  =====  ===========  ========================= ==========  ==========  ===========  =========  ==========  ==========  ==========  ==============="
END SUB

SUB HIdle ()
    Heading
    LPRINT TAB(69); "IDLE INVENTORY LISTING": LPRINT
    LPRINT "                                                            Model       Serial                   Idle     Date         Date        Last                    "
    LPRINT "Type  Code  Brand  Distributor        Description           Number      Number       Status      Days   Purchased    Received    Activity    Activity Type "
    LPRINT "====  ====  =====  ===========  ========================  ==========  ==========  ============  ======  ==========  ==========  ==========  ==============="
END SUB

SUB HLoaner ()
    Heading
    LPRINT TAB(68); "LOANER & SERVICE DETAIL": LPRINT
    LPRINT
    LPRINT " Agr                                           Serial                 Activity     Activity               Serial                   Activity      Activity    "
    LPRINT "Number Route       Customer Name        Code   Number      Status       Date         Type         Code    Number       Status        Date          Type      "
    LPRINT "====== ===== ========================== ==== ========== ============ ========== ===============   ====  ==========  ============  ==========  ==============="
END SUB

SUB HMonDet ()
    Heading
    LPRINT TAB(68); "INVENTORY DETAIL - MONEY": LPRINT
    LPRINT "                                       Serial     ===========RENTAL INCOME=========== ===========DEPRECIATION============ ==========SERVICE EXPENSE=========="
    LPRINT "Typ  Code       Description            Number        Month       Year        Total       Month       Year        Total       Month       Year        Total   "
    LPRINT "===  ====  ========================  ==========   =========== =========== =========== =========== =========== =========== =========== =========== ==========="
END SUB

SUB HOrder ()
    Heading
    LPRINT TAB(69); "INVENTORY ORDER REPORT": LPRINT
    LPRINT "                                                             Model                           In                       ------------------TOTAL-------------------"
    LPRINT "Type  Code  Brand  Distributor        Description            Number        Idle   Loaner   Service  On Rent  On Hand  On Hand  On Rent    Idle   Loaner  Service"
    LPRINT "====  ====  =====  ===========  ========================  =============  =======  =======  =======  =======  =======  =======  =======  =======  ======= ======="
END SUB

SUB HPrice ()
    Heading
    LPRINT TAB(70); "INVENTORY COST REPORT": LPRINT
    LPRINT "                                                            Model       Serial                             Date         Date        Last                    "
    LPRINT "Type  Code  Brand  Distributor        Description           Number      Number      Status       Cost    Purcahsed    Received    Activity    Activity Type "
    LPRINT "====  ====  =====  ===========  ========================= ==========  ==========  ===========  ========  ==========  ==========  ==========  ==============="
END SUB

SUB HRates ()
    Heading
    LPRINT TAB(69); "INVENTORY TERMS & RATES": LPRINT
    LPRINT "                                                                 Model        Serial       Date       =====M O N T H L Y=====   =====W E E K L Y======"
    LPRINT "Type   Code   Brand   Distibutor         Description             Number       Number     Purchased    Standard      Reduced     Standard     Reduced     Group"
    LPRINT "====   ====   =====   ==========   =========================   ==========   ==========   ==========   =======================   ======================   ====="
END SUB

SUB HServic ()
    Heading
    LPRINT TAB(69); "SERVICE DETAIL REPORT": LPRINT
    LPRINT "                                                  Model      Serial                        Date        Date       Last "
    LPRINT "Code Brand Distributor       Description          Number     Number      Status    Days  Purchased   Received   Activity       Service Center         Telephone"
    LPRINT "==== ===== =========== ======================== ========== ========== ============ ==== ========== ========== ========== ========================== ============"
END SUB

SUB HStatus ()
    Heading
    LPRINT TAB(68); "INVENTORY STATUS REPORT": LPRINT
    LPRINT
    LPRINT "                                                In    =================ON-RENT================         Cash    Paid  ========CHARGE-OFF========= Transf       "
    LPRINT "Type Code         Description            Idle Service OnRent  (AS)   (IS)   (AR)   (IC)  Legal Loaner  Sale     Out   Skip  Legal  Scrap   Away   Outs  Total "
    LPRINT "==== ====  ==========================  ======================================================================================================================="
END SUB

SUB HTimDet ()
    Heading
    LPRINT TAB(68); "INVENTORY DETAIL - TIME": LPRINT
    LPRINT "                                       Serial   =======AGE=======  ======IDLE=======  =====ON-RENT=====  ===ON-RENT/AS====  ===ON-RENT/IS====  ===ON-RENT/AR===="
    LPRINT "Typ  Code       Description            Number   Month Year  Total  Month Year  Total  Month Year  Total  Month Year  Total  Month Year  Total  Month Year  Total"
    LPRINT "===  ====  ========================  ========== =================  =================  =================  =================  =================  ================="
END SUB

SUB HTraLog ()
    Heading
    LPRINT TAB(69); "Daily Transaction Log": LPRINT
    LPRINT
    LPRINT "TC   TIME   INI TRANSACTION"
    LPRINT "== ======== === =============================================================================================================================================="
END SUB

SUB mActRpt ()
    IF Response("Resp-17") THEN EXIT SUB
    mVewOpt 1
END SUB

DEFINT A-Z
SUB mAgrRpt ()
    Yes = 1
551 IF WinCust(Pas.Lvl) THEN EXIT SUB
    AgrDetl Pas.Nam
END SUB

DEFLNG L
SUB mAudRpt ()
    Statlin "Inventory Audit Report"
    IF Response("Resp-09") THEN EXIT SUB
    Top = LOF(11) \ LEN(Inx): PageNum = 0: HAudit
    GET #11, 1, Inx: v$ = Inx.Grp
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #11, x, Inx
        GET #10, Inx.Num, Inv
        IF v$ <> Inx.Grp THEN GOSUB 510
        IF Inv.Sta < 9 THEN
           LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HAudit
           IF FindCod(Inv.Cod) = 0 THEN ERROR 200
           LPRINT USING "&   "; Cod.Typ + Cod.Grp; Inv.Cod; Inv.Bra; Inv.Dis;
           LPRINT USING "&  "; Cod.Des; Inv.Mdl; Inv.Ser; Status(Inv.Sta);
           LPRINT USING "  &"; Inv.Agr; Inv.Dtp; Inv.Dtr; Inv.Dta; TraStat(Inv.Act)
           IF Inv.Sta = 2 OR Inv.Sta = 8 THEN L1 = L1 + 1 ELSE L2 = L2 + 1
        END IF
    NEXT
    GOSUB 510
    LPRINT USING "      Store Total=####    BOR=####   Idle=####"; L3 + L4; L3; L4
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Inventory Audit        "; PageNum; "Page(s)."
EXIT SUB
510 LPRINT
    LPRINT USING "      Group Total=####    BOR=####   Idle=####"; L1 + L2; L1; L2
    LPRINT
    v$ = Inx.Grp
    LineNum = LineNum + 3
    L3 = L3 + L1: L1 = 0
    L4 = L4 + L2: L2 = 0
    RETURN
END SUB

DEFINT L
DEFDBL D
SUB mBadRpt ()
    Statlin "Bad Check Report"
    IF Response("Resp-19") THEN EXIT SUB
    Top = LOF(19) \ LEN(Rct): PageNum = 0: HBadChk
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #19, x, Rct
        IF Rct.Sta = "B" AND Rct.Mrk <> "û" THEN
           IF VAL(Rct.Amt) < 0 THEN
              GET #13, VAL(Rct.Cus), Cus
              LPRINT USING "& "; Rct.Dat; Rct.Num; Rct.Agr; Rct.Mgr; Cus.Nam;
              LPRINT USING " & "; Rct.Rnt; Rct.Dis; Rct.Tax; Rct.Lat; Rct.Trp;
              LPRINT USING " & "; Rct.Oth; Rct.Wav; Rct.Ref; Rct.Crd; Rct.Due; Rct.Amt;
              LPRINT USING " &"; Rct.Cod
              LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HBadChk
              d0 = d0 + VAL(Rct.Rnt)
              d1 = d1 + VAL(Rct.Dis)
              d2 = d2 + VAL(Rct.Tax)
              d3 = d3 + VAL(Rct.Lat)
              d4 = d4 + VAL(Rct.Trp)
              d5 = d5 + VAL(Rct.Oth)
              d6 = d6 + VAL(Rct.Wav)
              d7 = d7 + VAL(Rct.Ref)
              d8 = d8 + VAL(Rct.Crd)
              d9 = d9 + VAL(Rct.Due)
              da = da + VAL(Rct.Amt)
           END IF
        END IF
    NEXT
    LPRINT
    LPRINT TAB(56);
    LPRINT USING "######.##"; d0; d1; d2; d3; d4; d5; d6; d7; d8; d9; da
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Bad Check Report       "; PageNum; "Page(s)."
END SUB

DEFINT D
DEFLNG L
SUB mBorRpt ()
    Statlin "BOR Listing"
    IF Response("Resp-09") THEN EXIT SUB
    Top = LOF(15) \ LEN(Cnx): PageNum = 0: HBor
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #15, x, Cnx
        GET #13, Cnx.Num, Cus
        CustAgr Cus.Num, f, t
        IF f > 0 THEN
           FOR y = f TO t
           GET #18, y, Agx
           GET #16, Agx.Num, Agr
           IF Agr.Sta = "A" THEN
              LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HBor
              PyoAmnt Lp
              Lw = 0
              IF Agr.Frq = "W" THEN
                 Lr = 100 * VAL(Agr.Wrt)
                 IF Agr.Wav = "Y" THEN
                    IF Lr > 2000 THEN Lw = Lr * .05 ELSE Lw = 100
                 END IF
              ELSE
                 Lr = 100 * VAL(Agr.Mrt)
                 IF Agr.Wav = "Y" THEN
                    IF Lr > 8000 THEN Lw = Lr * .05 ELSE Lw = 400
                 END IF
              END IF
              LPRINT USING "& "; Cus.Nam; Agr.Num; Agr.Dat; Agr.Nxt; Agr.Frq; Agr.Wtr; Agr.Wrt; Agr.Mtr; Agr.Mrt;
              LPRINT USING "###.##"; Lr / 100; Lw / 100;
              LPRINT USING " & ####.##"; Agr.Pod; Lp / 100;
              IF Agr.Cmb > "    " THEN
                 LPRINT USING " &"; Agr.Cmb;
              ELSE
                 LPRINT USING " &"; Agr.Cod;
              END IF
              IF Agr.It1 > "          " THEN i = i + 1
              IF Agr.It2 > "          " THEN i = i + 1
              IF Agr.It3 > "          " THEN i = i + 1
              IF Agr.It4 > "          " THEN i = i + 1
              LPRINT USING " &"; Agr.It1; Agr.It2; Agr.It3; Agr.It4
           END IF
           NEXT
        END IF
    NEXT
    LPRINT
    LPRINT USING "          Total BOR: ##### pieces"; i
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "BOR Listing            "; PageNum; "Page(s)."
END SUB

DEFINT L
DEFDBL D
SUB mCshRpt (w)
    Statlin "Cash Journal"
    IF w THEN IF Response("Resp-10") THEN EXIT SUB
    Top = LOF(19) \ LEN(Rct): f$ = DATE$: PageNum = 0: HCash
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF w THEN IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #19, x, Rct
        IF Rct.Dat = f$ THEN
           GET #13, VAL(Rct.Cus), Cus
           IF FindAgr(Rct.Agr) THEN GET #16, Agx.Num, Agr
           IF Agr.Frq = "C" THEN LPRINT Rct.Num + "c ";  ELSE LPRINT Rct.Num + "  ";
           LPRINT USING "&  "; Rct.Agr; Rct.Mgr; Cus.Nam; Rct.Rnt; Rct.Dis;
           LPRINT USING "&  "; Rct.Tax; Rct.Lat; Rct.Trp; Rct.Oth; Rct.Wav;
           LPRINT USING "&  "; Rct.Ref; Rct.Crd; Rct.Due; Rct.Amt;
           LPRINT USING " &"; Rct.Cod; Rct.Sta
           LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HCash
           d0 = d0 + VAL(Rct.Rnt)
           d1 = d1 + VAL(Rct.Dis)
           d2 = d2 + VAL(Rct.Tax)
           d3 = d3 + VAL(Rct.Lat)
           d4 = d4 + VAL(Rct.Trp)
           d5 = d5 + VAL(Rct.Oth)
           d6 = d6 + VAL(Rct.Wav)
           d7 = d7 + VAL(Rct.Ref)
           d8 = d8 + VAL(Rct.Crd)
           d9 = d9 + VAL(Rct.Due)
           da = da + VAL(Rct.Amt)
        END IF
    NEXT
    GET #22, 1, Act                                              'get activity
    LPRINT
    LPRINT "   Rental Receipts"; TAB(48);
    LPRINT USING "######.##"; d0; d1; d2; d3; d4; d5; d6; d7; d8; d9; da

    LPRINT "Telephone Receipts"; TAB(138);
    LPRINT USING "######.##"; Act.Tel(1) / 100

    LPRINT "        Petty Cash"; TAB(138);
    LPRINT USING "######.##"; Act.Pet(1) / 100

    LPRINT "     Total Deposit"; TAB(138);
    LPRINT USING "######.##"; da + (Act.Tel(1) / 100) - (Act.Pet(1) / 100)

    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Cash Journal           "; PageNum; "Page(s)."
END SUB

DEFSNG A-Z
SUB mDelRpt ()
    IF Response("Resp-18") THEN EXIT SUB
    mDueOpt 1
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB mDueRpt (s)
    PCOPY 0, 2: CALL MhDisplayByName(0, 0, "Limit", e)
76  EntryU 22, 65, 1, e, b, r$:     IF e THEN EXIT SUB ELSE IF b THEN 76
77  EntryN 26, 60, 3, 0, e, b, d0$: IF e THEN EXIT SUB ELSE IF b THEN 76
    d0 = VAL(d0$)
    IF d0 < -3 AND Pas.Lvl < "8" THEN WinMssg "Low Level Password, max -3 days": GOTO 77
78  EntryN 26, 68, 3, 0, e, b, d1$: IF e THEN EXIT SUB ELSE IF b THEN 77
    d1 = VAL(d1$)
    IF d1 < d0 THEN WinMssg "Upper limit must be higher": GOTO 78
    Statlin "Past Due Detail"
    IF Response("Resp-13") THEN EXIT SUB
    f$ = "&   &  &   &   ####.##  &"
    Top = LOF(15) \ LEN(Cnx): PageNum = 0: HDueDet r$
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #15, x, Cnx
        GET #13, Cnx.Num, Cus
        CustAgr Cus.Num, f, t
        IF f > 0 THEN
           FOR y = f TO t
           GET #18, y, Agx
           GET #16, Agx.Num, Agr
           IF Agr.Sta = "A" AND Agr.Rte = r$ THEN
              n = DateDif&(Agr.Nxt, DATE$)
              IF n >= d0 AND n <= d1 THEN
                 RctAgrt Agx.Agr, b, e
                 f = e - 2: IF f < b THEN f = b
                 REDIM a$(2): a$ = "    "
                 IF s AND f > 0 THEN
                    FOR z = f TO e
                    GET #21, z, Rcx: GET #19, Rcx.Num, Rct
                    RSET a$ = RIGHT$(STR$(DateDif(Rct.Ddt, Rct.Dat)), 5)
                    a$(z - f) = a$ + " " + Rct.Num + " " + Rct.Dat + " " + Rct.Ddt + " " + Rct.Nxt
                    a$(z - f) = a$(z - f) + Rct.Rnt + Rct.Dis + Rct.Tax + Rct.Lat + Rct.Trp + Rct.Oth + Rct.Wav + Rct.Ref + Rct.Crd + Rct.Due + Rct.Amt + Rct.Typ + Rct.Day + " *"
                    NEXT
                 END IF
                 LPRINT "Agreement No: "; Agr.Num; "       ";
                 IF s THEN
                    LPRINT a$(0)
                    LPRINT Cus.Nam; " "; a$(1)
                    LPRINT Cus.Nm1; " "; a$(2)
                    LPRINT Cus.Add; " ";
                 END IF
                 LPRINT CHR$(27); "G";
                 LPRINT USING "####  due  "; n;
                 AmntDue
                 LPRINT CHR$(27); "H";
                 REDIM a$(1 TO 4): a$(4) = SPACE$(87)
                 Ser(1) = Agr.It1: Lon(1) = Agr.Lo1
                 Ser(2) = Agr.It2: Lon(2) = Agr.Lo2
                 Ser(3) = Agr.It3: Lon(3) = Agr.Lo3
                 Ser(4) = Agr.It4: Lon(4) = Agr.Lo4
                 FOR r = 1 TO 4
                 IF FindSer(Ser(r)) THEN
                    GET #10, Inx.Num, Inv
                    IF FindCod(Inv.Cod) = 0 THEN ERROR 200
                    a$(r) = "   Original:  " + Inv.Ser + " " + Inv.Mdl + " " + Inv.Cod + " " + Inv.Bra + " " + Cod.Des
                    IF FindSer(Lon(r)) THEN
                       GET #10, Inx.Num, Inv
                       IF FindCod(Inv.Cod) = 0 THEN ERROR 200
                       a$(r) = "     Loaner:  " + Inv.Ser + " " + Inv.Mdl + " " + Inv.Cod + " " + Inv.Bra + " " + Cod.Des
                    END IF
                 END IF
                 NEXT
                 IF s THEN
                    b$ = Cus.Str
                    c$ = Cus.Cty + " " + Cus.Sta + " " + Cus.Zip
                    d$ = "Phone (Home): " + Cus.Tel
                    e$ = "Phone (Work): " + Cus.Wor + "   Accessories: " + Agr.Acc + SPACE$(7)
                 ELSE
                    b$ = Cus.Nam
                    c$ = Cus.Add
                    d$ = Cus.Cty + " " + Cus.Sta + " " + Cus.Zip
                    e$ = "Phone (Home): " + Cus.Tel + "   Accessories: " + Agr.Acc + SPACE$(7)
                 END IF
                 LPRINT USING f$; b$; "Monthly Term/Rate:"; Agr.Mtr; Agr.Mrt; VAL(Agr.Mtr) * VAL(Agr.Mrt); a$(1)
                 LPRINT USING f$; c$; "Weekly  Term/Rate:"; Agr.Wtr; Agr.Wrt; VAL(Agr.Wtr) * VAL(Agr.Wrt); a$(2)
                 LPRINT USING f$; d$; "Note: " + Cus.Rmk; a$(3)
                 IF s = 0 THEN LPRINT CHR$(27); "-"; "1";
                 LPRINT USING f$; e$; a$(4)
                 IF s THEN
                    l = 11
                    LPRINT : LPRINT
                    LPRINT CHR$(27); "-"; "1"; SPACE$(160);
                 ELSE
                    l = 5
                 END IF
                 LPRINT CHR$(27); "-"; "0";
                 LineNum = LineNum + l: IF LineNum > 61 THEN LPRINT CHR$(12); : HDueDet r$
              END IF
           END IF
           NEXT
        END IF
    NEXT
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Past Due Detailed      "; PageNum; "Page(s)."
END SUB

SUB mDuqRpt ()
    PCOPY 0, 2: CALL MhDisplayByName(0, 0, "Limit", e)
86  EntryU 22, 65, 1, e, b, r$:     IF e THEN EXIT SUB ELSE IF b THEN 86
87  EntryN 26, 60, 3, 0, e, b, d0$: IF e THEN EXIT SUB ELSE IF b THEN 86
    d0 = VAL(d0$)
    IF d0 < -3 AND Pas.Lvl < "8" THEN WinMssg "Low Level Password, max -3 days": GOTO 87
88  EntryN 26, 68, 3, 0, e, b, d1$: IF e THEN EXIT SUB ELSE IF b THEN 87
    d1 = VAL(d1$)
    IF d1 < d0 THEN WinMssg "Upper limit must be higher": GOTO 88
    Statlin "Past Due Quick"
    IF Response("Resp-13") THEN EXIT SUB
    Top = LOF(15) \ LEN(Cnx): PageNum = 0: HDueQik r$
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #15, x, Cnx
        GET #13, Cnx.Num, Cus
        CustAgr Cus.Num, f, t
        IF f > 0 THEN
           FOR y = f TO t
           GET #18, y, Agx
           GET #16, Agx.Num, Agr
           IF Agr.Sta = "A" AND Agr.Rte = r$ THEN
              n = DateDif&(Agr.Nxt, DATE$)
              IF n >= d0 AND n <= d1 THEN
                 LineNum = LineNum + 1: IF LineNum > 61 THEN LPRINT CHR$(12); : HDueQik r$
                 LPRINT USING "& & ####"; Cus.Nam; Agr.Num; n;
                 AmntDue
              END IF
           END IF
           NEXT
        END IF
    NEXT
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Past Due Quick         "; PageNum; "Page(s)."
END SUB

SUB mExtRpt ()
    Statlin "Extinguished Inventory"
    IF Response("Resp-09") THEN EXIT SUB
    Top = LOF(11) \ LEN(Inx): PageNum = 0: HExting
    GET #11, 1, Inx: v$ = Inx.Grp
    FOR x = 1 TO Top
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        GET #11, x, Inx
        GET #10, Inx.Num, Inv
        IF Inv.Sta > 9 AND Inv.Sta < 15 THEN
           LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HExting
           IF FindCod(Inv.Cod) = 0 THEN ERROR 200
           LPRINT USING "&   "; Cod.Typ + Cod.Grp; Inv.Cod; Inv.Bra; Inv.Dis;
           LPRINT USING "&  "; Cod.Des; Inv.Mdl; Inv.Ser; Status(Inv.Sta);
           LPRINT USING "  &"; Inv.Agr; Inv.Dtp; Inv.Dtr; Inv.Dta; TraStat(Inv.Act)
           L1 = L1 + 1
        END IF
    NEXT
    LPRINT
    LPRINT USING "      Store Total=####"; L1
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Extingueshed Inventory "; PageNum; "Page(s)."
END SUB

SUB mIdeRpt ()
    Statlin "Idle Inventory Listing"
    IF Response("Resp-09") THEN EXIT SUB
    Top = LOF(11) \ LEN(Inx): PageNum = 0: HIdle
    GET #11, 1, Inx: v$ = Inx.Grp
    FOR x = 1 TO Top
        GET #11, x, Inx
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        IF v$ <> Inx.Grp AND L1 > 0 THEN GOSUB 300
        GET #10, Inx.Num, Inv
        IF Inv.Sta < 9 THEN L0 = L0 + 1
        IF Inv.Sta = 0 THEN
           LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HIdle
           IF FindCod(Inv.Cod) = 0 THEN ERROR 200
           LPRINT USING "&   "; Cod.Typ + Cod.Grp; Inv.Cod; Inv.Bra; Inv.Dis;
           LPRINT USING "&  "; Cod.Des; Inv.Mdl; Inv.Ser; Status(Inv.Sta);
           LPRINT USING " #####"; DateDif&(Inv.Dta, DATE$);
           LPRINT USING "  &"; Inv.Dtp; Inv.Dtr; Inv.Dta; TraStat(Inv.Act)
           L1 = L1 + 1: L2 = L2 + DateDif&(Inv.Dta, DATE$)
        END IF
    NEXT
    IF L1 > 0 THEN GOSUB 300
    LPRINT USING "       Store Total=#### (##%) Idle of #### Total & Avarage Idle=##### & ###### Idle Days"; L3; 100 * (L3 / L0); L0; SPACE$(8); L4 \ L3; SPACE$(18); L4
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Idle Inventory Listing "; PageNum; "Page(s)."
    EXIT SUB
300 LPRINT
    LPRINT USING "       Group Total=#### Items & Avarage Idle=##### & ###### Idle Days"; L1; SPACE$(27); L2 \ L1; SPACE$(18); L2
    LPRINT
    LineNum = LineNum + 3
    v$ = Inx.Grp
    L3 = L3 + L1: L1 = 0
    L4 = L4 + L2: L2 = 0
    RETURN
END SUB

DEFINT L
SUB mLblRpt ()
    Statlin "Address Lables"
    IF WinZipC THEN EXIT SUB
    IF Response("Resp-21") THEN EXIT SUB
    LPRINT CHR$(27); "@"; CHR$(27); "C"; "!";
    Top = LOF(15) \ LEN(Cnx)
    FOR x = 1 TO Top
    LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
    IF INKEY$ = CHR$(27) THEN EXIT FOR
    GET #15, x, Cnx
    GET #13, Cnx.Num, Cus
    IF FindZip(Cus.Zip) AND Zip.Mrk = "*" THEN
       n = INSTR(Cus.Nam, ",")
       IF n THEN
           f$ = RTRIM$(LTRIM$(MID$(Cus.Nam, n + 1)))
           l$ = RTRIM$(LTRIM$(MID$(Cus.Nam, 1, n - 1)))
       ELSE
           f$ = Cus.Nam
           l$ = ""
       END IF
       LPRINT TAB(4); f$; " "; l$
       LPRINT TAB(4); "or Current Resident"
       LPRINT TAB(4); Cus.Add
       LPRINT TAB(4); Cus.Str
       LPRINT TAB(4); RTRIM$(Cus.Cty); "  "; Cus.Sta; "  "; Cus.Zip
       LPRINT
    END IF
    NEXT
END SUB

DEFLNG L
SUB mMonRpt ()
    IF Response("Resp-09") THEN EXIT SUB
    Statlin "Inventory Detail - Money"
    Top = LOF(11) \ LEN(Inx): PageNum = 0: HMonDet
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #11, x, Inx
        GET #10, Inx.Num, Inv
        IF Inv.Sta < 9 THEN
           LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HMonDet
           IF FindCod(Inv.Cod) = 0 THEN ERROR 200
           LPRINT USING "&  "; Cod.Typ + Cod.Grp; Inv.Cod; Cod.Des; Inv.Ser;
           LPRINT USING " #######,.##"; Inv.Min / 100; Inv.Yin / 100; Inv.Tin / 100;
           LPRINT USING " #######,.##"; Inv.Mde / 100; Inv.Yde / 100; Inv.Tde / 100;
           LPRINT USING " #######,.##"; Inv.Mre / 100; Inv.Yre / 100; Inv.Tre / 100
           La = La + Inv.Min: Lb = Lb + Inv.Yin: Lc = Lc + Inv.Tin
           Ld = Ld + Inv.Mde: Le = Le + Inv.Yde: Lf = Lf + Inv.Tde
           Lg = Lg + Inv.Mre: Lh = Lh + Inv.Yre: Li = Li + Inv.Tre
        END IF
    NEXT
    LPRINT : LPRINT TAB(50);
    LPRINT USING "########,.##"; La / 100; Lb / 100; Lc / 100; Ld / 100; Le / 100; Lf / 100; Lg / 100; Lh / 100; Li / 100
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Detail Inventory-Dollar"; PageNum; "Page(s)."
END SUB

DEFINT L
SUB mOrdRpt (w)
    Statlin "Inventory Order Report"

    IF w THEN IF Response("Resp-09") THEN EXIT SUB
    Top = LOF(11) \ LEN(Inx): PageNum = 0: HOrder
    GET #11, 1, Inx
    m$ = Inx.Mdl
    t$ = Inx.Typ + Inx.Grp
    FOR x = 1 TO Top
        GET #11, x, Inx
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF w THEN IF INKEY$ = CHR$(27) THEN EXIT FOR
        IF m$ <> Inx.Mdl THEN GOSUB ItmTxt
        IF t$ <> Inx.Typ + Inx.Grp THEN GOSUB ItmTot
        GET #10, Inx.Num, Inv
        IF Inv.Sta < 9 THEN n = n + 1
        SELECT CASE Inv.Sta
               CASE 0: i = i + 1
               CASE 1: s = s + 1
               CASE 2: r = r + 1
               CASE 8: l = l + 1
        END SELECT
    NEXT
    GOSUB ItmTxt
    GOSUB ItmTot
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Inventory Order Report "; PageNum; "Page(s)."
    LOCATE 1
    EXIT SUB

ItmTxt: 'if not same modle number print previous item total
    IF i + l + s > 0 THEN
       LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HOrder
       IF FindCod(Inv.Cod) = 0 THEN Cod.Des = "ERROR:CODE-NOT-IN-MASTER"
       IF LPOS(1) > 1 THEN LPRINT
       LPRINT USING "&   "; Cod.Typ + Cod.Grp; Inv.Cod; Inv.Bra; Inv.Dis;
       LPRINT USING "&  "; Cod.Des; Inv.Mdl;
       LPRINT USING "    #####"; i; l; s; r; n;
    END IF
    nt = nt + n: n = 0
    rt = rt + r: r = 0
    it = it + i: i = 0
    lt = lt + l: l = 0
    st = st + s: s = 0
    m$ = Inx.Mdl
    RETURN
ItmTot: 'if not same type print total line for inventory type
    IF it + lt + st > 0 THEN
       LPRINT TAB(116); : LPRINT USING "    #####"; nt; rt; it; lt; st
    END IF
    nt = 0
    rt = 0
    it = 0
    lt = 0
    st = 0
    t$ = Inx.Typ + Inx.Grp
    RETURN
END SUB

DEFLNG L
SUB mPriRpt (w)
    Statlin "Inventory Cost Report"
    IF w THEN IF Response("Resp-09") THEN EXIT SUB

    CLOSE 11
    SHELL "Sortf ..\DataFile\Inventor.Inx ..\DataFile\Inventor.Tmp /L33 /+31,1 /+30,1 /+29,1 /+28,1 /+1,27 /Q"
    OPEN "..\DataFile\Inventor.Tmp" FOR RANDOM AS #11 LEN = LEN(Inx)
    
    Top = LOF(11) \ LEN(Inx): PageNum = 0: HPrice
    GET #11, 1, Inx
    GET #10, Inx.Num, Inv
    y$ = RIGHT$(Inv.Dtp, 4)
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #11, x, Inx
        GET #10, Inx.Num, Inv
        IF y$ <> RIGHT$(Inv.Dtp, 4) THEN GOSUB 511
        IF Inv.Sta < 9 THEN
           LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HPrice
           IF FindCod(Inv.Cod) = 0 THEN ERROR 200
           LPRINT USING "&   "; Cod.Typ + Cod.Grp; Inv.Cod; Inv.Bra; Inv.Dis;
           LPRINT USING "&  "; Cod.Des; Inv.Mdl; Inv.Ser; Status(Inv.Sta);
           LPRINT USING "&  "; Inv.Prc; Inv.Dtp; Inv.Dtr; Inv.Dta;
           LPRINT USING "&"; TraStat(Inv.Act)
           IF Inv.Sta = 2 OR Inv.Sta = 8 THEN L1 = L1 + 1 ELSE L2 = L2 + 1
           L3 = L3 + 100 * VAL(Inv.Prc)
        END IF
    NEXT
    GOSUB 511
    LPRINT USING "      Store Total=####    BOR=####   Idle=####"; L4 + L5; L4; L5;
    LPRINT TAB(94); : LPRINT USING "#######.##"; L6 / 100
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Inventory Audit        "; PageNum; "Page(s)."
    CLOSE 11
    KILL "..\DataFile\Inventor.Tmp"
    OPEN "..\DataFile\Inventor.Inx" FOR RANDOM AS #11 LEN = LEN(Inx)
EXIT SUB
511 LPRINT
    LPRINT USING "      Year  Total=####    BOR=####   Idle=####"; L1 + L2; L1; L2;
    LPRINT TAB(94); : LPRINT USING "#######.##"; L3 / 100
    LPRINT
    y$ = RIGHT$(Inv.Dtp, 4)
    LineNum = LineNum + 3
    L4 = L4 + L1: L1 = 0
    L5 = L5 + L2: L2 = 0
    L6 = L6 + L3: L3 = 0
    RETURN
END SUB

DEFLNG A-K, M-W, Y-Z
SUB mRatRpt ()
    IF Response("Resp-11") THEN EXIT SUB
    Statlin "Merchandise Rates"
    Top = LOF(11) \ LEN(Inx): PageNum = 0: HRates
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #11, x, Inx
        GET #10, Inx.Num, Inv
        IF Inv.Sta = 0 THEN
           IF InvRtrv THEN WinMssg "Code not found": EXIT SUB

           LPRINT USING " &  "; Cod.Typ + Cod.Grp; Inv.Cod; " " + Inv.Bra + "  "; Inv.Dis; Cod.Des; Inv.Mdl; Inv.Ser; Inv.Dtp;

           Trm = VAL(Rat.Mtr(1))
           Rte = VAL(Rat.Mrt(1)) * 100
           Cst = 100 * VAL(Inv.Prc)
           Mkt = Cst * VAL(Cod.Fmv) - (Inv.Tin / VAL(Cod.Dfm))
           IF Mkt < Cst THEN Mkt = Cst
           m = Inv.Tdr \ (DaysAvg * VAL(Cod.Dtr))
           p = Rte * (Trm - m)
           IF p < Mkt THEN r = CINT(Mkt / Rte): m = Trm - r
           IF m > .33 * Trm THEN m = .33 * Trm
           IF Inv.Ovr = "O" THEN m = 0
           w = m * DaysAvg \ 7
           
           FOR z = 1 TO 3
               IF Rat.Mtr(z) < "  1" THEN EXIT FOR
               LPRINT TAB(104);
               LPRINT USING "  &   "; Rat.Mtr(z);
               LPRINT USING "  ### "; VAL(Rat.Mtr(z)) - m;
               LPRINT USING "  &   "; Rat.Mrt(z);
               LPRINT USING "  &   "; Rat.Wtr(z);
               LPRINT USING "  ### "; VAL(Rat.Wtr(z)) - w;
               LPRINT USING "  &   "; Rat.Wrt(z); Cod.Cmb
               LineNum = LineNum + 1
           NEXT
           IF LineNum > 60 THEN LPRINT CHR$(12); : HRates
        END IF
    NEXT
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Merchandise Rates      "; PageNum; "Page(s)."
END SUB

DEFINT A-K, M-W, Y-Z
SUB mSerRpt ()
    Statlin "Loaner & Service Detail"
    IF Response("Resp-09") THEN EXIT SUB
    Top = LOF(16) \ LEN(Agr): PageNum = 0: HLoaner
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #16, x, Agr
        IF Agr.Sta = "A" THEN
           Ser(1) = Agr.It1: Lon(1) = Agr.Lo1
           Ser(2) = Agr.It2: Lon(2) = Agr.Lo2
           Ser(3) = Agr.It3: Lon(3) = Agr.Lo3
           Ser(4) = Agr.It4: Lon(4) = Agr.Lo4
           FOR y = 1 TO 4
               IF Lon(y) > "         0" THEN
               IF FindSer(Lon(y)) THEN
                  GET #10, Inx.Num, Inv
                  GET #13, VAL(Agr.Cus), Cus
                  LPRINT USING "& "; Agr.Num; "  " + Agr.Rte + "  "; Cus.Nam; Inv.Cod; Inv.Ser; Status(Inv.Sta); Inv.Dta; TraStat(Inv.Act);
                  IF FindSer(Ser(y)) THEN
                     GET #10, Inx.Num, Inv
                     LPRINT USING "  &"; Inv.Cod; Inv.Ser; Status(Inv.Sta); Inv.Dta; TraStat(Inv.Act)
                  END IF
               END IF
               END IF
           NEXT
        END IF
    NEXT
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Loaner & Service Detail"; PageNum; "Page(s)."
END SUB

DEFINT L
SUB mSrvRpt ()
    Statlin "Service Detail Report"
    IF Response("Resp-09") THEN EXIT SUB
    t = LOF(11) \ LEN(Inx): PageNum = 0: HServic
    GET #11, 1, Inx
    FOR x = 1 TO t
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / t);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #11, x, Inx
        GET #10, Inx.Num, Inv
        SELECT CASE Inv.Sta
        CASE 1, 3, 4, 5
            LineNum = LineNum + 3: IF LineNum > 60 THEN LPRINT CHR$(12); : HServic
            IF FindCod(Inv.Cod) = 0 THEN ERROR 200
            LPRINT USING "&  "; Inv.Cod; Inv.Bra; Inv.Dis;
            LPRINT USING "& "; Cod.Des; Inv.Mdl; Inv.Ser; Status(Inv.Sta);
            LPRINT USING "#### "; DateDif&(Inv.Dta, DATE$);
            LPRINT USING "& "; Inv.Dtp; Inv.Dtr; Inv.Dta;
            IF Inv.Ctr > " 0" THEN
               GET #7, VAL(Inv.Ctr), Ctr
               LPRINT USING "& &"; Ctr.Nam; Ctr.Tel
            ELSE
               LPRINT
            END IF
            LPRINT TAB(49); Inv.Rmk
            LPRINT
        END SELECT
    NEXT
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Service Detail         "; PageNum; "Page(s)."
END SUB

SUB mStaRpt ()
    Statlin "Detail Inventory Status"
    IF Response("Resp-09") THEN EXIT SUB
    ERASE Total
    Top = LOF(11) \ LEN(Inx): PageNum = 0: HStatus
    GET #11, 1, Inx: v$ = Inx.Cod
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #11, x, Inx
        IF v$ <> Inx.Cod THEN
           LineNum = LineNum + 1: IF LineNum > 61 THEN LPRINT CHR$(12); : HStatus
           IF FindCod(Inv.Cod) = 0 THEN ERROR 200
           LPRINT USING "&  "; Cod.Typ + Cod.Grp; Cod.Cod; Cod.Des;
           FOR y = 0 TO 16
               LPRINT USING " ######"; Total(1, y);
               Total(2, y) = Total(2, y) + Total(1, y)
               Total(1, y) = 0
           NEXT
           LPRINT
           v$ = Inx.Cod
        END IF
        GET #10, Inx.Num, Inv
        Total(1, Inv.Sta) = Total(1, Inv.Sta) + 1
        Total(1, 16) = Total(1, 16) + 1
    NEXT
    IF FindCod(Inv.Cod) = 0 THEN ERROR 200
    LPRINT USING "&  "; Cod.Typ + Cod.Grp; Cod.Cod; Cod.Des;
    FOR y = 0 TO 16
        LPRINT USING " ######"; Total(1, y);
        Total(2, y) = Total(2, y) + Total(1, y)
    NEXT
    LPRINT
    LPRINT
    LPRINT TAB(38);
    FOR y = 0 TO 16: LPRINT USING " ######"; Total(2, y); : NEXT
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Inventory Status-Detail"; PageNum; "Page(s)."
END SUB

SUB mStsRpt (a)
    Statlin "Inventory Status Report"
    IF a THEN IF Response("Resp-09") THEN EXIT SUB
    ERASE Total
    Top = LOF(11) \ LEN(Inx): PageNum = 0: HStatus
    GET #11, 1, Inx: v$ = Inx.Typ + Inx.Grp
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        GET #11, x, Inx
        IF v$ <> Inx.Typ + Inx.Grp THEN
           LineNum = LineNum + 1: IF LineNum > 61 THEN LPRINT CHR$(12); : HStatus
           IF FindCod(Inv.Cod) = 0 THEN ERROR 200
           LPRINT USING "&  "; Cod.Typ + Cod.Grp; Cod.Cod; Cod.Des;
           FOR y = 0 TO 16
               LPRINT USING " ######"; Total(1, y);
               Total(2, y) = Total(2, y) + Total(1, y)
               Total(1, y) = 0
           NEXT
           LPRINT
           v$ = Inx.Typ + Inx.Grp
        END IF
        GET #10, Inx.Num, Inv
        Total(1, Inv.Sta) = Total(1, Inv.Sta) + 1
        Total(1, 16) = Total(1, 16) + 1
    NEXT
    IF FindCod(Inv.Cod) = 0 THEN ERROR 200
    LPRINT USING "&  "; Cod.Typ + Cod.Grp; Cod.Cod; Cod.Des;
    FOR y = 0 TO 16
        LPRINT USING " ######"; Total(1, y);
        Total(2, y) = Total(2, y) + Total(1, y)
    NEXT
    LPRINT
    LPRINT
    LPRINT TAB(38);
    FOR y = 0 TO 16: LPRINT USING " ######"; Total(2, y); : NEXT
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Inventory Status       "; PageNum; "Page(s)."
END SUB

DEFLNG L
SUB mTimRpt ()
    IF Response("Resp-09") THEN EXIT SUB
    Statlin "Inventory Detail - Days"
    Top = LOF(11) \ LEN(Inx): PageNum = 0: HTimDet
    FOR x = 1 TO Top
        LOCATE 43, 51: PRINT USING "###%"; 100 * (x / Top);
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        GET #11, x, Inx
        GET #10, Inx.Num, Inv
        IF Inv.Sta < 9 THEN
           LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HTimDet
           IF FindCod(Inv.Cod) = 0 THEN ERROR 200
           IF Inv.Sta > 8 THEN d$ = Inv.Dta ELSE d$ = DATE$
           t = DateDif&(Inv.Dtp, d$)
           y = DateDif&("01-01" + RIGHT$(DATE$, 5), d$)
           m = DateDif&(LEFT$(DATE$, 3) + "01" + RIGHT$(DATE$, 5), d$)
           IF y < 0 THEN y = 0 ELSE IF y > t THEN y = t
           IF m < 0 THEN m = 0 ELSE IF m > y THEN m = y
           LPRINT USING "&  "; Cod.Typ + Cod.Grp; Inv.Cod; Cod.Des;
           LPRINT Inv.Ser;
           LPRINT USING " ##### ##### #####"; m; y; t;
           LPRINT USING "  ##### ##### #####"; Inv.Mdi; Inv.Ydi; Inv.Tdi;
           LPRINT USING "  ##### ##### #####"; Inv.Mdr; Inv.Ydr; Inv.Tdr;
           LPRINT USING "  ##### ##### #####"; Inv.Mas; Inv.Yas; Inv.Tas;
           LPRINT USING "  ##### ##### #####"; Inv.Mds; Inv.Yds; Inv.Tds;
           LPRINT USING "  ##### ##### #####"; Inv.Mar; Inv.Yar; Inv.Tar
           La = La + m: Lb = Lb + y: Lc = Lc + t
           Ld = Ld + Inv.Mdi: Le = Le + Inv.Ydi: Lf = Lf + Inv.Tdi
           Lg = Lg + Inv.Mdr: Lh = Lh + Inv.Ydr: Li = Li + Inv.Tdr
           Lj = Lj + Inv.Mas: Lk = Lk + Inv.Yas: Ll = Ll + Inv.Tas
           Lm = Lm + Inv.Mds: Ln = Ln + Inv.Yds: Lo = Lo + Inv.Tds
           Lp = Lp + Inv.Mar: Lq = Lq + Inv.Yar: Lr = Lr + Inv.Tar
        END IF
    NEXT
    LPRINT : LPRINT TAB(47);
    LPRINT USING "  ##### ##### #####"; La; Lb; Lc; Ld; Le; Lf; Lg; Lh; Li; Lj; Lk; Ll; Lm; Ln; Lo; Lp; Lq; Lr
    LPRINT CHR$(12);
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Detail Inventory - Time"; PageNum; "Page(s)."
END SUB

DEFINT L
SUB mTraRpt ()
    Statlin "Todays Transactions"
    IF Response("Resp-09") THEN EXIT SUB
    TranLog
    PRINT #23, USING "& & & & ### &"; DATE$; TIME$; Pas.Nam; "Daily Transaction Log  "; PageNum; "Page(s)."
END SUB

DEFSNG A-Z
SUB TranLog ()
    CLOSE
    LOCATE 1
    SHELL "SortF ..\DataFile\Transact.Log ..\DataFile\Transact.Tmp /+1,11 /Q"
    KILL "..\DataFile\Transact.Log"
    NAME "..\DataFile\Transact.Tmp" AS "..\DataFile\Transact.Log"
    OPEN "..\DataFile\Transact.Log" FOR INPUT AS #26
    PageNum = 0: HTraLog
    WHILE NOT EOF(26)
      LINE INPUT #26, s$
      a$ = LEFT$(s$, 2)
      IF p$ <> a$ THEN
         LPRINT
         p$ = a$
         LineNum = LineNum + 1
      END IF
      LPRINT s$
      LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HTraLog
    WEND
    LPRINT CHR$(12);
    OpenFiles
END SUB

