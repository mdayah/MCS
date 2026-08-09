DEFINT A-Z
DECLARE FUNCTION WinStor% ()
DECLARE FUNCTION TraStat$ (f$)
DECLARE FUNCTION StoPick% (s$)

DECLARE SUB Statlin (f$)
DECLARE SUB HAudit ()
DECLARE SUB HAging ()
DECLARE SUB HBor ()
DECLARE SUB HCosts ()
DECLARE SUB Heading ()
DECLARE SUB HExting ()
DECLARE SUB HStatus ()

REM $INCLUDE: '..\Include\Config.off'

DIM SHARED Total(2, 20) AS INTEGER

DEFSNG A-Z
SUB HAging ()
    Heading
    LPRINT TAB(64); "    MERCHANDISE AGING REPORT   ": LPRINT
    LPRINT "                                                               Model       Serial                              Date         Date        Last                    "
    LPRINT "Store Group Code Brand Distributor       Description           Number      Number      Status     Agreement  Purcahsed    Received    Activity    Activity Type "
    LPRINT "===== ===== ==== ===== =========== ========================  ==========  ==========  ===========  =========  ==========  ==========  ==========  ==============="
END SUB

SUB HAudit ()
    Heading
    LPRINT TAB(69); "INVENTORY AUDIT REPORT": LPRINT
    LPRINT "                                                               Model       Serial                              Date         Date        Last                    "
    LPRINT "Store Group Code Brand Distributor       Description           Number      Number      Status     Agreement  Purcahsed    Received    Activity    Activity Type "
    LPRINT "===== ===== ==== ===== =========== ========================  ==========  ==========  ===========  =========  ==========  ==========  ==========  ==============="
END SUB

DEFINT A-Z
SUB HCosts ()
    Heading
    LPRINT TAB(64); "   MERCHANDISE PRICE LISTING   ": LPRINT
    LPRINT "                                                  Model             Rate  Drop    =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY== =MONTHLY== ==WEEKLY=="
    LPRINT "Group Code       Description        Distributor   Number     Cost   Code TRM FMV  Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate Term  Rate"
    LPRINT "===== ==== ======================== =========== ========== ======== ==== =======  ========== ========== ========== ========== ========== =========="
END SUB

DEFSNG A-Z
SUB HExting ()
    Heading
    LPRINT TAB(64); " EXTINGUISHED INVENTORY LISTING": LPRINT
    LPRINT "                                                               Model       Serial                              Date         Date        Last                    "
    LPRINT "Store Group Code Brand Distributor       Description           Number      Number      Status     Agreement  Purcahsed    Received    Activity    Activity Type "
    LPRINT "===== ===== ==== ===== =========== ========================  ==========  ==========  ===========  =========  ==========  ==========  ==========  ==============="
END SUB

SUB HStatus ()
    Heading
    LPRINT TAB(68); "INVENTORY STATUS REPORT": LPRINT
    LPRINT
    LPRINT "                                                   In    =================ON-RENT================         Cash    Paid  ========CHARGE-OFF========= Transf       "
    LPRINT "Store Grp Code        Description           Idle Service OnRent  (AS)   (IS)   (AR)   (IC)  Legal Loaner  Sale     Out   Skip  Legal  Scrap   Away   Outs  Total "
    LPRINT "===== === ==== ========================== ======================================================================================================================="
END SUB

DEFINT A-Z
SUB mAgeRpt (s)
    Statlin "Active Invetory Aging"

    IF s THEN
    CALL MhDisplayByName(0, 0, "Aging", e)
A1: EntryU 9, 50, 2, e, b, s$: IF e THEN EXIT SUB
    IF s$ < "01" THEN IF WinStor GOTO A1 ELSE s$ = Sto.Num
    GET #6, VAL(s$), Sto
    n$ = RTRIM$(Sto.Nam)
    qPrint SPACE$(30), 11, 25, 0, 1
    qPrint n$, 11, 39 - LEN(n$) \ 2, 0, 1
    s$ = Sto.Num
A2: EntryD 13, 40, e, b, b$: IF e THEN EXIT SUB ELSE IF b GOTO A1
    d$ = RIGHT$(b$, 4) + LEFT$(b$, 2) + MID$(b$, 4, 2)
    IF d$ < "0" GOTO A2
    KeyPres p$: IF p$ = "P" THEN p = 1
    END IF

    IF Response("Resp-09") THEN EXIT SUB

    SETINDEX 10, "Sto"
    IF s THEN SEEKGE #10, s$ ELSE MOVEFIRST #10
    
    DO: RETRIEVE #10, Inv
        IF s AND Inv.Sto <> s$ THEN EXIT DO
        IF Inv.Dtp > "0" THEN
           Inv.Fac = RIGHT$(Inv.Dtp, 4) + LEFT$(Inv.Dtp, 2) + MID$(Inv.Dtp, 4, 2)
        ELSE
           Inv.Fac = RIGHT$(Inv.Dtr, 4) + LEFT$(Inv.Dtr, 2) + MID$(Inv.Dtr, 4, 2)
        END IF
        UPDATE #10, Inv
        MOVENEXT #10
    LOOP UNTIL EOF(10)

    DELETEINDEX #10, "Age"
    IF s THEN
       CREATEINDEX #10, "Age", 0, "Sto", "Fac", "Grp", "Cod", "Dis", "Mdl"
    ELSE
       CREATEINDEX #10, "Age", 0, "Fac", "Sto", "Grp", "Cod", "Dis", "Mdl"
    END IF
    SETINDEX 10, "Age"

    IF p THEN PageNum = 0: HAging
    IF s THEN SEEKGE #10, s$ ELSE MOVEFIRST #10
    
    DO: IF INKEY$ = CHR$(27) THEN EXIT DO
        RETRIEVE #10, Inv
        IF s AND Inv.Sto <> s$ THEN EXIT DO
        IF Inv.Sta < 10 THEN
           L0 = L0 + 1
           IF Inv.Fac <= d$ THEN
              IF p THEN LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HAging
              IF FindCod(Inv.Cod) = 0 THEN ERROR 200
              IF p THEN LPRINT USING "  &   &  &  &  &  "; Inv.Sto; Cod.Typ + Cod.Grp; Inv.Cod; Inv.Bra; Inv.Dis;
              IF p THEN LPRINT USING "&  "; Cod.Des; Inv.Mdl; Inv.Ser; Status(Inv.Sta); Inv.Agr;
              IF p THEN LPRINT USING "  &"; Inv.Dtp; Inv.Dtr; Inv.Dta; TraStat(Inv.Act)
              L1 = L1 + 1
           END IF
        END IF
        MOVENEXT #10
    LOOP UNTIL EOF(10)
    IF p THEN
       LPRINT : LPRINT USING "Inventory Total #######,  Inventory Purchased on or Prior to & #######  Percentage of Old Inventory to Total ##.##%"; L0; b$; L1; 100 * (L1 / L0)
    ELSE
       COLOR 16, 1
       LOCATE 17, 47: PRINT USING "######"; L0;
       LOCATE 18, 47: PRINT USING "######"; L1;
       LOCATE 19, 47: PRINT USING "###.##"; 100 * (L1 / L0);
       KeyPres f$
    END IF
END SUB

DEFLNG L
SUB mAudRpt (s)
    Statlin "Inventory Audit Report"
    IF s THEN IF StoPick(s$) THEN EXIT SUB
    IF Response("Resp-09") THEN EXIT SUB
    PageNum = 0: HAudit

    SETINDEX 10, "Sto"
    IF s THEN SEEKGE #10, s$ ELSE MOVEFIRST #10
    RETRIEVE #10, Inv
    v$ = Inv.Grp

    DO: IF INKEY$ = CHR$(27) THEN EXIT DO
        RETRIEVE #10, Inv
        IF s AND Inv.Sto <> s$ THEN EXIT DO
        IF v$ <> Inv.Grp THEN GOSUB 510
        IF Inv.Sta < 9 THEN
'           LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HAudit
           IF FindCod(Inv.Cod) = 0 THEN ERROR 200
           LPRINT USING "  &   &  &  &  &  "; Inv.Sto; Cod.Typ + Cod.Grp; Inv.Cod; Inv.Bra; Inv.Dis;
           LPRINT USING "&  "; Cod.Des; Inv.Mdl; Inv.Ser; Status(Inv.Sta); Inv.Agr;
           LPRINT USING "  &"; Inv.Dtp; Inv.Dtr; Inv.Dta; TraStat(Inv.Act); Inv.Prc
           IF Inv.Sta = 2 OR Inv.Sta = 8 THEN L1 = L1 + 1 ELSE L2 = L2 + 1
        END IF
        MOVENEXT #10
    LOOP UNTIL EOF(10)
    
    GOSUB 510
    LPRINT USING "      Store Total=####    BOR=####   Idle=####"; L3 + L4; L3; L4
    LPRINT CHR$(12);
    EXIT SUB

510 LPRINT
    LPRINT USING "      Group Total=####    BOR=####   Idle=####"; L1 + L2; L1; L2
    LPRINT
    v$ = Inv.Grp
    LineNum = LineNum + 3
    L3 = L3 + L1: L1 = 0
    L4 = L4 + L2: L2 = 0
    RETURN
END SUB

SUB mExtRpt (s)
    Statlin "Extinguished Inventory"
    IF s THEN IF StoPick(s$) THEN EXIT SUB
    IF Response("Resp-09") THEN EXIT SUB
    PageNum = 0: HExting
    SETINDEX 10, "Sto"
    IF s THEN SEEKGE #10, s$ ELSE MOVEFIRST #10
    RETRIEVE #10, Inv
    v$ = Inv.Grp

    DO: IF INKEY$ = CHR$(27) THEN EXIT DO
        RETRIEVE #10, Inv
        IF Inv.Sto <> s$ THEN EXIT DO
        IF l > 0 AND v$ <> Inv.Grp THEN GOSUB 512
        IF Inv.Sta > 9 AND Inv.Sta < 15 THEN
'           LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HExting
           IF FindCod(Inv.Cod) = 0 THEN ERROR 200
           LPRINT USING "  &   &  &  &  &  "; Inv.Sto; Cod.Typ + Cod.Grp; Inv.Cod; Inv.Bra; Inv.Dis;
           LPRINT USING "&  "; Cod.Des; Inv.Mdl; Inv.Ser; Status(Inv.Sta); Inv.Agr;
           LPRINT USING "  &"; Inv.Dtp; Inv.Dtr; Inv.Dta; TraStat(Inv.Act); Inv.Prc
           L1 = L1 + 1
        END IF
        MOVENEXT #10
    LOOP UNTIL EOF(10)

    GOSUB 512
    LPRINT USING "      Store Total=####"; L2
    LPRINT CHR$(12);
    EXIT SUB

512 LPRINT
    LPRINT USING "      Group Total=####"; L1
    LPRINT
    v$ = Inv.Grp
    LineNum = LineNum + 3
    L2 = L2 + L1: L1 = 0
    RETURN
END SUB

DEFINT L
SUB mInvPrc ()
    Statlin "Merchandise Prices"
    IF Response("Resp-09") THEN EXIT SUB
    
    CREATEINDEX #10, "Cst", 0, "Grp", "Cod", "Dis", "Mdl", "Prc"
    SETINDEX 10, "Cst"
    PageNum = 0: HCosts
    MOVEFIRST #10
    DO: IF INKEY$ = CHR$(27) THEN EXIT DO
       RETRIEVE #10, Inv
       IF Inv.Grp + Inv.Cod + Inv.Dis + Inv.Mdl + Inv.Prc <> p$ THEN
          LineNum = LineNum + 1: IF LineNum > 60 THEN LPRINT CHR$(12); : HCosts
          IF FindCod(Inv.Cod) = 0 THEN Cod.Des = "***Code not found***"
          LPRINT USING "  &  & & &"; Inv.Grp; Inv.Cod; Cod.Des; Inv.Dis;
          LPRINT USING "  &  &  &   &   & "; Inv.Mdl; Inv.Prc; Inv.Rat; Cod.Dtr; Cod.Dfm;
          IF Inv.Rat > "   " THEN
             GET #4, VAL(Inv.Rat), Rat
             FOR z = 1 TO 3
                 LPRINT USING " & &"; Rat.Mtr(z); Rat.Mrt(z); Rat.Wtr(z); Rat.Wrt(z);
             NEXT
          END IF
          LPRINT
          p$ = Inv.Grp + Inv.Cod + Inv.Dis + Inv.Mdl + Inv.Prc
       END IF
       MOVENEXT #10
    LOOP WHILE NOT EOF(10)
    DELETEINDEX #10, "Cst"
END SUB

SUB mStaRpt (s)
    Statlin "Detai Inventory Status"
    IF s THEN IF StoPick(s$) THEN EXIT SUB
    IF Response("Resp-09") THEN EXIT SUB

    ERASE Total
    PageNum = 0: HStatus
    
    SETINDEX 10, "Sto"
    IF s THEN SEEKGE #10, s$ ELSE MOVEFIRST #10
    RETRIEVE #10, Inv
    v$ = Inv.Cod

    DO: IF INKEY$ = CHR$(27) THEN EXIT DO
        RETRIEVE #10, Inv
        IF Inv.Sto <> s$ THEN EXIT DO
        IF v$ <> Inv.Cod THEN
           LineNum = LineNum + 1: IF LineNum > 61 THEN LPRINT CHR$(12); : HStatus
           IF FindCod(v$) = 0 THEN ERROR 200
           LPRINT USING " &   & & &"; Inv.Sto; Cod.Typ + Cod.Grp; Cod.Cod; Cod.Des;
           FOR y = 0 TO 16
               LPRINT USING " ######"; Total(1, y);
               Total(2, y) = Total(2, y) + Total(1, y)
               Total(1, y) = 0
           NEXT
           LPRINT
           v$ = Inv.Cod
        END IF
        Total(1, Inv.Sta) = Total(1, Inv.Sta) + 1
        Total(1, 16) = Total(1, 16) + 1
        MOVENEXT #10
    LOOP UNTIL EOF(10)
    
    IF FindCod(Inv.Cod) = 0 THEN ERROR 200
    LPRINT USING " &   & & &"; Inv.Sto; Cod.Typ + Cod.Grp; Cod.Cod; Cod.Des;
    FOR y = 0 TO 16
        LPRINT USING " ######"; Total(1, y);
        Total(2, y) = Total(2, y) + Total(1, y)
    NEXT
    LPRINT
    LPRINT
    LPRINT TAB(40);
    FOR y = 0 TO 16: LPRINT USING " ######"; Total(2, y); : NEXT
    LPRINT CHR$(12);
END SUB

FUNCTION StoPick (s$)
    CALL MhDisplayByName(0, 0, "Sto-Pick", e)
509 EntryU 9, 50, 2, e, b, s$: IF e THEN StoPick = 1: EXIT FUNCTION ELSE IF b THEN 509
    IF s$ < "01" THEN IF WinStor THEN 509 ELSE s$ = Sto.Num
    GET #6, VAL(s$), Sto
    qPrint Sto.Nam, 11, 26, 0, 1
    s$ = Sto.Num
END FUNCTION

