DECLARE SUB cInventory ()
DECLARE SUB cCustomers ()
DECLARE SUB cReceipts ()
DECLARE SUB cAgreement ()
DEFINT A-Z

REM $INCLUDE: '..\Include\Inventor.REC'
REM $INCLUDE: '..\Include\Inventor.OFF'
REM $INCLUDE: '..\Include\Agreemnt.REC'
REM $INCLUDE: '..\Include\Agreemnt.OFF'
REM $INCLUDE: '..\Include\Customer.REC'
REM $INCLUDE: '..\Include\Customer.OFF'
REM $INCLUDE: '..\Include\Receipts.REC'
REM $INCLUDE: '..\Include\Receipts.OFF'

    DIM SHARED Inv  AS Inventory
    DIM SHARED Agr  AS Agreement
    DIM SHARED Cus  AS Customer
    DIM SHARED Rct  AS Receipts
    DIM SHARED AgrN AS AgreementO
    DIM SHARED InvN AS InventoryO
    DIM SHARED CusN AS CustomerO
    DIM SHARED RctN AS ReceiptsO

    WIDTH 80, 50
    CLS

    SHELL "Del ..\OfData\Inventor.MAS"
    SHELL "Del ..\OfData\Agreemnt.MAS"
    SHELL "Del ..\OfData\Receipts.MAS"
    SHELL "Del ..\OfData\Customer.MAS"

    OPEN "..\DataFile\Inventor.MAS" FOR RANDOM AS #1 LEN = LEN(Inv)
    OPEN "..\OfData\Inventor.MAS" FOR ISAM InventoryO "Invent" AS #2
    OPEN "..\DataFile\Agreemnt.MAS" FOR RANDOM AS #3 LEN = LEN(Agr)
    OPEN "..\OfData\Agreemnt.MAS" FOR ISAM AgreementO "Agreem" AS #4
    OPEN "..\DataFile\Customer.MAS" FOR RANDOM AS #5 LEN = LEN(Cus)
    OPEN "..\OfData\Customer.MAS" FOR ISAM CustomerO "Custom" AS #6
    OPEN "..\DataFile\Receipts.MAS" FOR RANDOM AS #7 LEN = LEN(Rct)
    OPEN "..\OfData\Receipts.MAS" FOR ISAM ReceiptsO "Receip" AS #8

    cAgreement
         g$ = INPUT$(1)
    cInventory
         g$ = INPUT$(1)
    cCustomers
         g$ = INPUT$(1)
    cReceipts
         g$ = INPUT$(1)
    

    
10  CLS
    SETINDEX 2, "Sto"
    PRINT "Index in use: "; GETINDEX$(2);
    MOVEFIRST #2
    VIEW PRINT
    PRINT "Press (S) for screen or (P) for printer"
    DO: k$ = INKEY$: LOOP WHILE k$ = ""
    k$ = UCASE$(k$)
    IF k$ <> "S" AND k$ <> "P" THEN 10

    IF k$ = "S" THEN
       LOCATE 1, 1: PRINT "                                                 Model            Rate Drop Drop"
       LOCATE 2, 1: PRINT "Group Code       Description        Distribut    Number     Cost  Code Term FMV "
       LOCATE 3, 1: PRINT "===== ==== ======================== ========== ========== ======= ==== ==== ===="
       VIEW PRINT 4 TO 50
    END IF
    
20  RETRIEVE #2, InvN
    IF INKEY$ = CHR$(27) THEN CLOSE : END
        l = l + 1: IF l > 46 THEN l = 1: DO: LOOP WHILE INKEY$ = ""
        PRINT USING " &"; InvN.Sto; InvN.Typ; InvN.Grp; InvN.Cod; InvN.Dis; InvN.Mdl; InvN.Ser; InvN.Prc; InvN.Rat
    MOVENEXT #2
    IF NOT EOF(2) THEN GOTO 20
    CLOSE
    END

ErrTrap:
    IF ERR = 86 THEN RESUME 10
    BEEP: PRINT ERR
    CLOSE
    END

SUB cAgreement ()
    CREATEINDEX 4, "Sto", 1, "Sto", "Num"
    CREATEINDEX 4, "Cus", 0, "Cus"

    SETINDEX 4, "Sto"

    LOCATE 7, 33: PRINT LOF(3) \ LEN(Agr);
    FOR x = 3 TO LOF(3) \ LEN(Agr)
    IF INKEY$ = CHR$(27) THEN CLOSE : END
    GET #3, x, Agr

     AgrN.Sto = "06"
     AgrN.Num = Agr.Num
     AgrN.Dat = Agr.Dat
     AgrN.Mgr = Agr.Mgr
     AgrN.Rte = Agr.Rte
     AgrN.Cus = Agr.Cus

     AgrN.Cmb = Agr.Cmb
     AgrN.Cod = Agr.Cod
     AgrN.Due = Agr.Due
     AgrN.Nxt = Agr.Nxt
     AgrN.Pod = Agr.Pod
     AgrN.Dta = Agr.Dta
     AgrN.Sta = Agr.Sta
     AgrN.Frq = Agr.Frq
     AgrN.Lat = Agr.Lat
     AgrN.Trp = Agr.Trp
     AgrN.Dlv = Agr.Dlv
     AgrN.Ref = Agr.Ref
     AgrN.Pay = Agr.Pay
     AgrN.Act = Agr.Act
     AgrN.Wav = Agr.Wav

     AgrN.Mst = Agr.Mst: AgrN.Msr = Agr.Msr
     AgrN.Wst = Agr.Wst: AgrN.Wsr = Agr.Wsr
     AgrN.Mtr = Agr.Mtr: AgrN.Mrt = Agr.Mrt
     AgrN.Wtr = Agr.Wtr: AgrN.Wrt = Agr.Wrt

     AgrN.Una = Agr.Una
     AgrN.Npd = Agr.Npd
     AgrN.Npw = Agr.Npw
     AgrN.Npm = Agr.Npm
     AgrN.Rct = Agr.Rct
     AgrN.Ndl = Agr.Ndl
     AgrN.Eza = Agr.Eza
     AgrN.Fre = Agr.Fre

     AgrN.Mri = Agr.Mri: AgrN.Yri = Agr.Yri: AgrN.Tri = Agr.Tri
     AgrN.Mds = Agr.Mds: AgrN.Yds = Agr.Yds: AgrN.Tds = Agr.Tds
     AgrN.Mtx = Agr.Mtx: AgrN.Ytx = Agr.Ytx: AgrN.Ttx = Agr.Ttx
     AgrN.Mlc = Agr.Mlc: AgrN.Ylc = Agr.Ylc: AgrN.Tlc = Agr.Tlc
     AgrN.Mtc = Agr.Mtc: AgrN.Ytc = Agr.Ytc: AgrN.Ttc = Agr.Ttc
     AgrN.Mot = Agr.Mot: AgrN.Yot = Agr.Yot: AgrN.Tot = Agr.Tot
     AgrN.Min = Agr.Min: AgrN.Yin = Agr.Yin: AgrN.Tin = Agr.Tin
     AgrN.Mrf = Agr.Mrf: AgrN.Yrf = Agr.Yrf: AgrN.Trf = Agr.Trf

     AgrN.It1 = Agr.It1: AgrN.Lo1 = Agr.Lo1: AgrN.Ds1 = Agr.Ds1: AgrN.Fm1 = Agr.Fm1
     AgrN.It2 = Agr.It2: AgrN.Lo2 = Agr.Lo2: AgrN.Ds2 = Agr.Ds2: AgrN.Fm2 = Agr.Fm2
     AgrN.It3 = Agr.It3: AgrN.Lo3 = Agr.Lo3: AgrN.Ds3 = Agr.Ds3: AgrN.Fm3 = Agr.Fm3
     AgrN.It4 = Agr.It4: AgrN.Lo4 = Agr.Lo4: AgrN.Ds4 = Agr.Ds4: AgrN.Fm4 = Agr.Fm4

    LOCATE 7, 43: PRINT x;
    INSERT #4, AgrN
    NEXT

END SUB

SUB cCustomers ()
    CREATEINDEX 6, "Cus", 0, "Nam"
    CREATEINDEX 6, "Num", 1, "Num"

    SETINDEX 6, "Num"

    LOCATE 7, 33: PRINT LOF(5) \ LEN(Cus);
    FOR x = 1 TO LOF(5) \ LEN(Cus)
    IF INKEY$ = CHR$(27) THEN CLOSE : END
    GET #5, x, Cus
    
     CusN.Sto = "06"
     CusN.Num = Cus.Num
     CusN.Sal = Cus.Sal
     CusN.Nam = Cus.Nam
     CusN.Add = Cus.Add
     CusN.Str = Cus.Str
     CusN.Cty = Cus.Cty
     CusN.Sta = Cus.Sta
     CusN.Zip = Cus.Zip
     CusN.Tel = Cus.Tel
     CusN.Rmk = Cus.Rmk

     CusN.SSn = Cus.SSn
     CusN.Dln = Cus.Dln
     CusN.Dob = Cus.Dob
     CusN.Mar = Cus.Mar
     CusN.Rac = Cus.Rac
     CusN.Sex = Cus.Sex

     CusN.Emp = Cus.Emp
     CusN.Wor = Cus.Wor
     CusN.Lon = Cus.Lon
     CusN.Inc = Cus.Inc
     CusN.Oth = Cus.Oth

     CusN.Dis = Cus.Dis
     CusN.Ref = Cus.Ref
     CusN.Typ = Cus.Typ
     CusN.Del = Cus.Del

     CusN.Nm1 = Cus.Nm1

     CusN.SS1 = Cus.SS1
     CusN.Dl1 = Cus.Dl1
     CusN.Bd1 = Cus.Bd1
     CusN.St1 = Cus.St1
     CusN.Rc1 = Cus.Rc1
     CusN.Sx1 = Cus.Sx1

     CusN.Em1 = Cus.Em1
     CusN.Wt1 = Cus.Wt1
     CusN.Ln1 = Cus.Ln1
     CusN.In1 = Cus.In1
     CusN.Ot1 = Cus.Ot1
     
    
    'LOCATE 7, 43
    PRINT x; CusN.Nam
    INSERT #6, CusN
    NEXT
END SUB

SUB cInventory ()
    CREATEINDEX 2, "Sto", 1, "Sto", "Typ", "Grp", "Cod", "Mdl", "Ser"
    CREATEINDEX 2, "Ser", 1, "Ser"

    SETINDEX 2, "Sto"

    LOCATE 7, 33: PRINT LOF(1) \ LEN(Inv);
    FOR x = 1 TO LOF(1) \ LEN(Inv)
    IF INKEY$ = CHR$(27) THEN CLOSE : END
    GET #1, x, Inv

     InvN.Sto = "06"
     InvN.Typ = Inv.Typ
     InvN.Grp = Inv.Grp
     InvN.Cod = Inv.Cod
     InvN.Bra = Inv.Bra
     InvN.Sta = Inv.Sta
     InvN.Tim = Inv.Tim
     InvN.Ctr = Inv.Ctr
     InvN.Mdl = Inv.Mdl
     InvN.Ser = Inv.Ser
     InvN.Dis = Inv.Dis
     InvN.Dtp = Inv.Dtp
     InvN.Dtr = Inv.Dtr
     InvN.Dta = Inv.Dta
     InvN.Prc = Inv.Prc
     InvN.Agr = Inv.Agr
     InvN.Del = Inv.Del
     InvN.Tfr = Inv.Tfr
     InvN.Tto = Inv.Tto
     InvN.Pon = Inv.Pon
     InvN.Act = Inv.Act
     InvN.Par = Inv.Par
     InvN.Lab = Inv.Lab
     InvN.Rat = Inv.Rat
     InvN.Ovr = Inv.Ovr

     InvN.Rmk = Inv.Rmk

     InvN.Mdi = Inv.Mdi: InvN.Ydi = Inv.Ydi: InvN.Tdi = Inv.Tdi
     InvN.Mdr = Inv.Mdr: InvN.Ydr = Inv.Ydr: InvN.Tdr = Inv.Tdr
     InvN.Mas = Inv.Mas: InvN.Yas = Inv.Yas: InvN.Tas = Inv.Tas
     InvN.Mds = Inv.Mds: InvN.Yds = Inv.Yds: InvN.Tds = Inv.Tds
     InvN.Mar = Inv.Mar: InvN.Yar = Inv.Yar: InvN.Tar = Inv.Tar
     InvN.Min = Inv.Min: InvN.Yin = Inv.Yin: InvN.Tin = Inv.Tin
     InvN.Mde = Inv.Mde: InvN.Yde = Inv.Yde: InvN.Tde = Inv.Tde
     InvN.Mre = Inv.Mre: InvN.Yre = Inv.Yre: InvN.Tre = Inv.Tre

    LOCATE 7, 43: PRINT x;
    INSERT #2, InvN
    NEXT
END SUB

SUB cReceipts ()
    CREATEINDEX 8, "Sto", 1, "Sto", "Agr", "Num"
    SETINDEX 8, "Sto"

    LOCATE 7, 33: PRINT LOF(7) \ LEN(Rct);
    FOR x = 1 TO LOF(7) \ LEN(Rct)
    IF INKEY$ = CHR$(27) THEN CLOSE : END
    GET #7, x, Rct

     RctN.Sto = "06"
     RctN.Cus = Rct.Cus
     RctN.Agr = Rct.Agr
     RctN.Num = Rct.Num
     RctN.Mgr = Rct.Mgr
     RctN.Mrk = Rct.Mrk
     RctN.Cod = Rct.Cod
     RctN.Sta = Rct.Sta
     RctN.Chk = Rct.Chk
     RctN.Dat = Rct.Dat
     RctN.Ddt = Rct.Ddt
     RctN.Nxt = Rct.Nxt
     RctN.Rnt = Rct.Rnt
     RctN.Dis = Rct.Dis
     RctN.Tax = Rct.Tax
     RctN.Lat = Rct.Lat
     RctN.Trp = Rct.Trp
     RctN.Oth = Rct.Oth
     RctN.Wav = Rct.Wav
     RctN.Ref = Rct.Ref
     RctN.Crd = Rct.Crd
     RctN.Due = Rct.Due
     RctN.Amt = Rct.Amt
     RctN.Typ = Rct.Typ
     RctN.Day = Rct.Day
    
    PRINT RctN.Sto; RctN.Agr; RctN.Num
    INSERT #8, RctN
    NEXT
END SUB

