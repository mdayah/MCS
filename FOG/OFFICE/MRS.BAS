DECLARE FUNCTION PicRate% ()
DECLARE FUNCTION TraStat$ (f$)
DECLARE FUNCTION TraType$ ()
DECLARE FUNCTION WinAgrt% ()
DECLARE FUNCTION WinCust% ()
DECLARE FUNCTION WinRcts% ()
DEFINT A-Z: DEFLNG L
DECLARE FUNCTION Percent% ()
DECLARE SUB mMasCol ()
DECLARE SUB mMasRat ()
DECLARE SUB mMasPas ()
DECLARE SUB mMasSto ()
DECLARE SUB mMasCtr ()
DECLARE SUB mMasCmb ()
DECLARE SUB mMasPrc ()
DECLARE SUB mMasCod ()
DECLARE SUB mMasCus ()
DECLARE SUB mMasInv ()
DECLARE SUB InvRate (r, c)
DECLARE SUB CusEdit ()
DECLARE SUB InvRtrv ()
DECLARE SUB Statlin (f$)

'$INCLUDE: '..\Include\Config.off'

'   ON ERROR GOTO Errors
    
    IR.ax = &H6700
    IR.bx = 26
    CALL INTERRUPT(&H21, IR, OU)

    SCREEN 0, 0, 0, 0: WIDTH 80, 50
    SCREEN 0, 0, 1, 1: PCOPY 0, 1
    SCREEN 0, 0, 0, 0

    CALL MhOpenScreenLib("MRS.SLB" + CHR$(0), e)
    CALL MhBlinkOff
    CALL MhShadowOn(0, 8)

    DIM Menu(6, 2)     AS STRING
    DIM Loct(6, 2)     AS INTEGER
    DIM Subm(6, 21, 2) AS STRING
    DIM HiLi(6, 21)    AS INTEGER

    FOR m = 1 TO 6
    READ Menu(m, 1), Menu(m, 2), Loct(m, 1), Loct(m, 2)
    FOR c = 1 TO Loct(m, 2)
    READ Subm(m, c, 1), Subm(m, c, 2), HiLi(m, c)
    NEXT
    NEXT

    OpenFiles
    
    m = 1: c = 1
10  IF c > Loct(m, 2) THEN c = Loct(m, 2)
    CALL MhDisplayByName(0, 0, "Logo", e)
    CALL MhDisplayByName(0, 0, Menu(m, 1), e)
20  COLOR 27, 0: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
    COLOR 28, 0: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
    KeyPres f$
    
30  SELECT CASE f$
    CASE CHR$(27): END
    CASE CHR$(0) + "M"
         m = m + 1: IF m > 6 THEN m = 1
         GOTO 10
    CASE CHR$(0) + "K"
         m = m - 1: IF m < 1 THEN m = 6
         GOTO 10
    CASE CHR$(0) + "P"
         COLOR 17, 6: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
         COLOR 20, 6: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
         c = c + 1: IF c > Loct(m, 2) THEN c = 1
    CASE CHR$(0) + "H"
         COLOR 17, 6: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
         COLOR 20, 6: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
         c = c - 1: IF c < 1 THEN c = Loct(m, 2)
    CASE CHR$(13)
         COLOR 17, 6: LOCATE c + 3, Loct(m, 1): PRINT Subm(m, c, 1);
         COLOR 20, 6: LOCATE c + 3, HiLi(m, c): PRINT Subm(m, c, 2);
         CALL MhDisplayByName(0, 0, Menu(m, 2), e)
         SELECT CASE m
              CASE 1
                   SELECT CASE c
                        CASE 1:  CALL mMasCod
                        CASE 2:  CALL mMasCmb
                        CASE 3:  CALL mMasPrc
                        CASE 4:  CALL mMasRat
                        CASE 5:  CALL mMasPas
                        CASE 6:  CALL mMasSto
                        CASE 7:  CALL mMasCtr
                   END SELECT
              CASE 2
                   SELECT CASE c
                   END SELECT
              CASE 3
                   SELECT CASE c
                        CASE 9:  CALL mMasCol
                        CASE 10: CALL mMasCus
                        CASE 11: CALL mMasInv
                   END SELECT
              CASE 4
                   SELECT CASE c
                   '    CASE 1:  CALL mExpOpt
                   '    CASE 2:  CALL mExpOpt
                   END SELECT
              CASE 5
                   SELECT CASE c
                   END SELECT
              CASE 6
                   SELECT CASE c
                   END SELECT
         END SELECT
         GOTO 10
    CASE ELSE: FOR x = 1 TO Loct(m, 2)
         IF f$ = Subm(m, x, 2) THEN c = x: f$ = CHR$(13): GOTO 30
         NEXT
    END SELECT
    GOTO 20
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
SELECT CASE ERR
       CASE 7:    Er$ = "Out of memory "
       CASE 9:    Er$ = "Subscript is out of range "
       CASE 24:   Er$ = "Device time out "
       CASE 25:   Er$ = "Device fault "
       CASE 27:   Er$ = "Printer out of paper "
       CASE 51:   Er$ = "Internal error"
       CASE 57:   Er$ = "Device I/O error "
       CASE 58:   Er$ = "File already exists "
       CASE 61:   Er$ = "Disk is full "
       CASE 68:   Er$ = "Device unavailable "
       CASE 70:   Er$ = "Disk is write protect "
       CASE 71:   Er$ = "Disk is not ready "
       CASE 72:   Er$ = "Disk media error "
       CASE 76:   Er$ = "Data path not found "
       CASE 81:   Er$ = "ISAM - Invalid name"
       CASE 82:   Er$ = "ISAM - Table not found"
       CASE 83:   Er$ = "ISAM - Index not found"
       CASE 84:   Er$ = "ISAM - Invalid column"
       CASE 85:   Er$ = "ISAM - No current record"
       CASE 86:   Er$ = "ISAM - Duplicate value for unique index"
       CASE 87:   Er$ = "ISAM - Invalid operation on NULL index"
       CASE 88:   Er$ = "ISAM - Database inconsistent"
       CASE 89:   Er$ = "ISAM - Insufficient ISAM buffers"

       CASE 200:  Er$ = "Unable to find code master  "
       CASE 201:  Er$ = "Unable to find user password "
       CASE 203:  Er$ = "Receipt rent exceeds $999.99 "
       CASE 204:  Er$ = "Late charges exceeds $99.99  "
       CASE 206:  Er$ = "Invalid agreement number "
       CASE 207:  Er$ = "Store has item as active"
       CASE 210:  Er$ = "Invalid status, not 3,4,or 5 "
       CASE 211:  Er$ = "Agreement number error "
       CASE 212:  Er$ = "Receipt number error "
       CASE ELSE: Er$ = "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
END SELECT
       PCOPY 0, 3: CALL MhDisplayByName(0, 0, "ErrMessage", e)
       COLOR 22, 4
       LOCATE 32, 38: PRINT ERR;
       LOCATE 33, 38: PRINT Er$;
       LOCATE 35, 38: PRINT ERDEV$;
       SOUND 50, 3: KeyPres f$
       PCOPY 3, 0
       RESUME 10
MenuData: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    DATA "Menu-Mas","",2,7
    DATA " Codes     ","C",3
    DATA " Combos    ","B",6
    DATA " Prices    ","P",3
    DATA " Rates     ","R",3
    DATA " Passwords ","W",7
    DATA " Stores    ","S",3
    DATA " Centers   ","T",6

    DATA "Menu-Rct","Receipt",13,7
    DATA " Issue      ","I",14
    DATA " Display    ","D",14
    DATA " Print      ","P",14
    DATA " Void       ","V",14
    DATA " Bad Check  ","B",14
    DATA " Mark Check ","M",14
    DATA " Cash Sale  ","C",14
    
    DATA "Menu-Inv","",22,11
    DATA " Receive New         ","N",31
    DATA " Transfer Out        ","O",32
    DATA " Transfer In         ","I",32
    DATA " Transfer Manual     ","M",32
    DATA " View Transfer File  ","V",23
    DATA " Service Send        ","S",31
    DATA " Service Receive     ","R",31
    DATA " Service OnSite      ","S",33
    DATA " Charge off Giveaway ","G",34
    DATA " Charge off Scrap    ","C",23
    DATA " Master Listing      ","L",30
                                    
    DATA "Menu-Pty","PettyCash",35,2
    DATA " Replenishment ","R",36
    DATA " Expenditure   ","E",36
                              
    DATA "Menu-Rpt", "",46,21
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
    
    DATA "Menu-End","",55,15
    DATA " Day closing   ","D",56
    DATA " Week closing  ","W",56
    DATA " Month closing ","M",56
    DATA " Year closing  ","Y",56
    DATA " Transactions  ","T",56
    DATA " View activity ","V",56
    DATA " Delinquency   ","D",56
    DATA " Communication ","C",56
    DATA " Special menu  ","S",56
    DATA " Reset date    ","R",56
    DATA " Format disk   ","F",56
    DATA " Purge records ","P",56
    DATA " Backup data   ","B",56
    DATA " Restore data  ","R",56
    DATA " Terminate     ","T",56

DEFINT L
FUNCTION Confirm (g, f$)
    IF g AND DateDif&("1", DATE$) < 1 THEN
       WinMssg "Confirm not allowed after end of day"
    ELSE
       PCOPY 0, 1: CALL MhDisplayByName(0, 0, "Confirm", e)
       SOUND 350, .5: KeyPres f$
       IF f$ = "*" THEN Confirm = 1: SOUND 1200, .3: SOUND 1000, .2
       PCOPY 1, 0
    END IF
END FUNCTION

FUNCTION FindCmb (f$)
   b = 1: t = LOF(3) \ LEN(Cmb)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #3, m, Cmb
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

SUB FindGrp (f$)
   IF f$ > "  " THEN
      b = 1: t = LOF(2) \ LEN(Cod)
      DO UNTIL (t < b)
         m = (t + b) \ 2: GET #2, m, Cod
         IF f$ > Cod.Cmb THEN b = m + 1 ELSE t = m - 1
      LOOP
      DO WHILE f$ > Cod.Cmb AND NOT EOF(2)
         GET #2, , Cod
      LOOP
      qPrint f$, CSRLIN, 9, 0, 1
      DO WHILE f$ = Cod.Cmb
         qPrint Cod.Cod + "ณ" + Cod.Des, CSRLIN, 12, 0, 1
         LOCATE CSRLIN + 1
         IF CSRLIN > 46 THEN KeyPres n$
         GET #2, , Cod
         IF f$ <> Cod.Cmb THEN EXIT DO
      LOOP
   END IF
END SUB

FUNCTION FindIni (f$)
   FOR x = 1 TO LOF(7) \ LEN(Pas)
       GET #7, x, Pas
       IF f$ = Pas.Ini THEN FindIni = x: EXIT FOR
   NEXT
END FUNCTION

FUNCTION FindPas (f$)
   FOR x = 1 TO LOF(7) \ LEN(Pas)
       GET #7, x, Pas
       IF f$ = Pas.Pas THEN FindPas = x: EXIT FOR
   NEXT
END FUNCTION

FUNCTION FindPrc (f$)
   b = 1: t = LOF(4) \ LEN(Prc)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #4, m, Prc
      IF f$ = Prc.Dis + Prc.Mdl + Prc.Cod THEN FindPrc = m: EXIT DO
      IF f$ > Prc.Dis + Prc.Mdl + Prc.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

DEFSNG A-Z
SUB KeyPres (f$)
    DO: LOOP WHILE INKEY$ > ""
    DO: f$ = INKEY$: LOOP WHILE f$ = ""
    f$ = UCASE$(f$)
END SUB

DEFINT A-Z
STATIC SUB mMasCol ()
    x = x + 1
    IF x > 63 THEN x = 1: p = p + 1
    PALETTE p, x
END SUB

DEFSNG A-Z
SUB OpenFiles ()
    CLOSE 1:  OPEN "..\Master\Codes   .Mas" FOR RANDOM AS #1 LEN = LEN(Cod)
    CLOSE 2:  OPEN "..\Master\Codes   .Cmb" FOR RANDOM AS #2 LEN = LEN(Cod)
    CLOSE 3:  OPEN "..\Master\Combos  .Mas" FOR RANDOM AS #3 LEN = LEN(Cmb)
    CLOSE 4:  OPEN "..\Master\Prices  .Mas" FOR RANDOM AS #4 LEN = LEN(Prc)
    CLOSE 5:  OPEN "..\Master\Rates   .Inx" FOR RANDOM AS #5 LEN = LEN(Rat)
    CLOSE 6:  OPEN "..\Master\Rates   .Mas" FOR RANDOM AS #6 LEN = LEN(Rat)
    CLOSE 7:  OPEN "..\Master\PassWord.Nam" FOR RANDOM AS #7 LEN = LEN(Pas)
    CLOSE 8:  OPEN "..\Master\Stores  .Mas" FOR RANDOM AS #8 LEN = LEN(Sto)
    CLOSE 9:  OPEN "..\Master\Centers .Mas" FOR RANDOM AS #9 LEN = LEN(Ctr)

    CLOSE 10: OPEN "..\OfData\Inventor.MAS" FOR ISAM Inventory "Invent" AS #10
    CLOSE 11: OPEN "..\OfData\Customer.MAS" FOR ISAM Customer "Custom" AS #11
    CLOSE 12: OPEN "..\OfData\Agreemnt.MAS" FOR ISAM Agreement "Agreem" AS #12
    CLOSE 13: OPEN "..\OfData\Receipts.MAS" FOR ISAM Receipts "Receip" AS #13

'    CLOSE 14: OPEN "..\OfData\Activity.Mas" FOR RANDOM AS #14 LEN = LEN(Act)
'    CLOSE 15: OPEN "..\OfData\Transact.Hst" FOR RANDOM AS #15 LEN = LEN(Tra)
'    CLOSE 16: OPEN "..\OfData\Transact.Log" FOR APPEND AS #16
'    CLOSE 17: OPEN "LPT2:" FOR OUTPUT AS #17
END SUB

DEFINT A-Z
FUNCTION PassWord (p$)
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "User-ID", e)
    COLOR 28: LOCATE 20, 47
    SOUND 860, .3
    FOR x = 1 TO 4
    KeyPres s$
    PRINT "þ";
    IF s$ = CHR$(27) THEN PCOPY 1, 0: EXIT FUNCTION
    IF s$ = CHR$(13) THEN EXIT FOR
    f$ = f$ + s$
    NEXT
    PCOPY 1, 0
    IF FindPas(f$) THEN
       IF Pas.Lvl >= p$ THEN PassWord = 1 ELSE WinMssg "Permission denied"
    ELSE
       WinMssg "Invalid Password"
    END IF
END FUNCTION

FUNCTION Percent ()
         Percent = Agr.Tri / (VAL(Agr.Mtr) * VAL(Agr.Mrt))
END FUNCTION

FUNCTION PicRate ()
    PCOPY 0, 1
    COLOR 1, 7: LOCATE 12
'    m = (Trm(1) * Dis(1) + Trm(2) * Dis(2) + Trm(3) * Dis(3) + Trm(4) * Dis(4)) / 100
'    w = m * DaysAvg \ 7
    FOR z = 1 TO 3                                         'rates and load
        IF Rat.Mtr(z) < "  1" THEN EXIT FOR                'droping empty
        Rats(1, z) = VAL(Rat.Mtr(z)) - m                   'modified terms
        Rats(2, z) = VAL(Rat.Wtr(z)) - w                   'modified terms
        LOCATE , 20
        PRINT USING "บ ###ณ&ณ ###ณ&บ"; Rats(1, z); Rat.Mrt(z); Rats(2, z); Rat.Wrt(z)
    NEXT

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
        PicRate = z: GOTO r2
    CASE ELSE
        PicRate = 0: GOTO r2
    END SELECT
    GOTO r1
r2: PCOPY 1, 0
END FUNCTION

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
    qPrint SPACE$(25 - LEN(f$)) + f$, 50, 55, 0, 1
END SUB

DEFINT A-Z
STATIC SUB SubMenu (s, n$)
    IF s THEN
       REDIM a$(10), a(10)
       Picture 49, 1, 80, m$
       b = 1
       FOR x = 1 TO 9
           e = INSTR(b, m$, "  ")
           IF e = b THEN EXIT FOR
           a$(x) = MID$(m$, b, 1 + e - b)
           a(x) = b
           b = e + 1
       NEXT
       REDIM PRESERVE a$(x - 1)
       REDIM PRESERVE a(x - 1)
       b = 1
    ELSE
       LOCATE 49, a(b)
       COLOR 10, 0: PRINT MID$(a$(b), 1, 2);
       COLOR 13, 0: PRINT MID$(a$(b), 3);
       SELECT CASE n$
       CASE CHR$(0) + "M"  '===============Right=
            b = b + 1: IF b > UBOUND(a, 1) THEN b = 1
       CASE CHR$(0) + "K"  '===============Left==
            b = b - 1: IF b < 1 THEN b = UBOUND(a, 1)
       CASE CHR$(13)
            n$ = MID$(a$(b), 2, 1): EXIT SUB
       CASE ELSE
            FOR x = 1 TO UBOUND(a, 1)
            IF n$ = MID$(a$(x), 2, 1) THEN b = x: EXIT FOR
            NEXT
       END SELECT
       LOCATE 49, a(b)
       COLOR 18, 14: PRINT MID$(a$(b), 1, 2);
       COLOR 21, 14: PRINT MID$(a$(b), 3);
    END IF
END SUB

DEFSNG A-S, U-Z
DEFSTR T
FUNCTION TraStat (f$)
     SELECT CASE f$
            CASE "AB": TraStat = "A. Loaner Back "
            CASE "AC": TraStat = "A. Charge-off  "
            CASE "AE": TraStat = "A. EZA days    "
            CASE "AF": TraStat = "A. Free Days   "
            CASE "AL": TraStat = "A. Loaner      "
            CASE "AM": TraStat = "A. Reroute     "
            CASE "AN": TraStat = "A. New Issue   "
            CASE "AP": TraStat = "A. Promissary  "
            CASE "AR": TraStat = "A. Reinstate   "
            CASE "AS": TraStat = "A. Swap        "
            CASE "AT": TraStat = "A. Terminate   "
            CASE "AV": TraStat = "A. Void        "
            CASE "AW": TraStat = "A. Wavier      "
            
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
            CASE "MU": TraStat = "M. Unerase Item"
            

            CASE "RB": TraStat = "R. Bad Check   "
            CASE "RN": TraStat = "R. New Issue   "
            CASE "RP": TraStat = "R. Paid Out    "
            CASE "RV": TraStat = "R. Void        "

            CASE "CN": TraStat = "C. New         "
            CASE "CM": TraStat = "C. Edit        "
            CASE ELSE: TraStat = "?????? Activity"
     END SELECT
END FUNCTION

FUNCTION TraType ()
   SELECT CASE Tra.Typ
    CASE "AA": TraType = "& & & Paid-out[R] agreement & for & route & {   paid-out: &}"
    CASE "AC": TraType = "& & & Charged-off agreement & for & route & {charged-off: &}{in-service: &}{idled: &}"
    CASE "AE": TraType = "& & & & EZA  days on agreement & for & route &"
    CASE "AF": TraType = "& & & & Free days on agreement & for & route &"
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
    CASE "AW": TraType = "& & & Wavier  on  agreement & for & route & was &"

    CASE "MC": TraType = "& & & Rate code & override on & & & Model: & Dist: &"
    CASE "MG": TraType = "& & & Charged-off as  giveaway & & & Model: & Dist: &"
    CASE "MI": TraType = "& & & Returned  from  service  & & & Model: & Dist: & Center: &"
    CASE "MN": TraType = "& & & Received new merchandise & & & Model: & Dist: &"
    CASE "MM": TraType = "& & & Transferred-in manually  & & & Model: & Dist: &   from: &"
    CASE "MO": TraType = "& & & Sent to service          & & & Model: & Dist: & Center: &"
    CASE "MP": TraType = "& & & Charged-off as scrap     & & & Model: & Dist: &"
    CASE "MR": TraType = "& & & Transferred-in           & & & Model: & Dist: &   from: &"
    CASE "MS": TraType = "& & & Serviced on-site         & & & Model: & Dist: &     by: &"
    CASE "MT": TraType = "& & & Transferred-out          & & & Model: & Dist: &     to: &"
    CASE "MU": TraType = "& & & Modified inventory item  & & & Model: & Dist: & status: ##"

    CASE "RB": TraType = "& & & Badcheck $& receipt & agreement & for &"
    CASE "RN": TraType = "& & & Issued   $& receipt & agreement & for &"
    CASE "RV": TraType = "& & & Voided   $& receipt & agreement & for &"
    
    CASE "CN": TraType = "& & & Added new customer number & &"
    CASE "CM": TraType = "& & & Modified  customer number & &"

    CASE "OP": TraType = "& & & Purged:#### agreements, #### receipts, and #### items"
    CASE "OI": TraType = "& & & Logged  in RTO"
    CASE "OO": TraType = "& & & Logged out RTO"
    CASE "OC": TraType = "& & & Started communications"

    CASE ELSE: TraType = "& & & Unknown activity"
   END SELECT
END FUNCTION

DEFINT A-Z
FUNCTION WinAgrt ()
'   CustAgr Cus.Num, f, t
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
T1: GET #8, n, Ctr
    COLOR 27, 0: LOCATE r, 41: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam
    KeyPres n$
    COLOR 8, 3: LOCATE r, 41: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam
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
    FOR x = b TO b + 14: GET #8, x, Ctr
    LOCATE , 41: PRINT USING "&ณ&"; Ctr.Num; Ctr.Nam
    NEXT
    RETURN
END FUNCTION

SUB WinCode (t$)
    PCOPY 0, 1
    b = 1: t = LOF(2) / LEN(Cod): r = 7: n = 1: GOSUB C2
C1: GET #2, n, Cod
    COLOR 15, 2: LOCATE r, 46: PRINT USING "&ณ&ณ&"; Cod.Cmb; Cod.Cod; Cod.Des;
    KeyPres n$
    COLOR 11, 1: LOCATE r, 46: PRINT USING "&ณ&ณ&"; Cod.Cmb; Cod.Cod; Cod.Des;
    SELECT CASE n$
    CASE CHR$(13)
        t$ = Cod.Cmb
        PCOPY 1, 0
        EXIT SUB
    CASE CHR$(27)
        t$ = ""
        PCOPY 1, 0
        EXIT SUB
    CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
        IF n + 1 > t GOTO C1 ELSE n = n + 1
        IF r + 1 > 20 THEN CALL Scroll(46, 7, 77, 20, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
        IF n - 1 < 1 GOTO C1 ELSE n = n - 1
        IF r - 1 < 7 THEN CALL BkScroll(46, 7, 77, 20, 1) ELSE r = r - 1
    CASE CHR$(0) + "I" 'PgUp
        b = n - r - 7: IF b < 1 THEN b = 1
        GOSUB C2: n = b + r - 7
    CASE CHR$(0) + "Q" 'PgDn
        b = 21 - r + n
        IF b > t - 13 THEN b = t - 13: n = t - 20 + r ELSE n = n + 14
        GOSUB C2
    CASE CHR$(0) + "G" 'Home
        b = 1: GOSUB C2
        n = r - 6
    CASE CHR$(0) + "O" 'End
        b = t - 13: GOSUB C2
        n = t - 20 + r
    CASE ELSE
    END SELECT
    GOTO C1
 
C2: COLOR 11, 1: LOCATE 6, 45: PRINT "ษออัออออัออออออออออออออออออออออออป"
    FOR x = b TO t: GET #2, x, Cod
    LOCATE , 45: PRINT USING "บ&ณ&ณ&บ"; Cod.Cmb; Cod.Cod; Cod.Des
    IF CSRLIN > 20 THEN EXIT FOR
    NEXT
    LOCATE , 45: PRINT "ศออฯออออฯออออออออออออออออออออออออผ"
    RETURN
END SUB

FUNCTION WinComb ()
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-CMB", e)
    b = 1: t = LOF(3) \ LEN(Cmb): r = 7: n = 1: GOSUB B2
B1: GET #3, n, Cmb
    COLOR 27, 1: LOCATE r, 43: PRINT USING "&ณ&"; Cmb.Cod; Cmb.Des;
    KeyPres n$
    COLOR 17, 3: LOCATE r, 43: PRINT USING "&ณ&"; Cmb.Cod; Cmb.Des;
    SELECT CASE n$
    CASE CHR$(0) + "P", "2"'--Dn
        IF n + 1 > t GOTO B1 ELSE n = n + 1
        IF r + 1 > 21 THEN CALL Scroll(43, 7, 71, 21, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", "8"'--Up
        IF n - 1 < 1 GOTO B1 ELSE n = n - 1
        IF r - 1 < 7 THEN CALL BkScroll(43, 7, 71, 21, 1) ELSE r = r - 1
    CASE CHR$(0) + "I", "9"'--PgUp
        b = n - r - 8
        IF b < 1 THEN b = 1
        GOSUB B2: n = x + r - 21
    CASE CHR$(0) + "Q", "3"'--PgDn
        b = n - r + 22
        IF b + 14 > t THEN b = t - 14
        GOSUB B2: n = x + r - 21
    CASE CHR$(0) + "G", "7"'--Home
        b = 1: GOSUB B2
        n = r - 6
    CASE CHR$(0) + "O", "1"'--End
        b = t - 14: GOSUB B2
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
        COLOR 10, 0: LOCATE 12, 10: PRINT Cmb.Cod; " "; Cmb.Des
 '      InvRate 18, 42
        LOCATE 16
        FOR s = 1 TO 4: FindGrp (Cmb.Cde(s)): NEXT
        KeyPres f$
        PCOPY 2, 0
    CASE ELSE
    END SELECT
    GOTO B1

B2: COLOR 17, 3: LOCATE 7
    FOR x = b TO t: GET #3, x, Cmb
    LOCATE , 43: PRINT USING "&ณ&"; Cmb.Cod; Cmb.Des
    IF CSRLIN > 21 THEN EXIT FOR
    NEXT
    RETURN
C3: PCOPY 1, 0
END FUNCTION

FUNCTION WinCust ()
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
'        Tra.Typ = "CN"
'        CusEdit
    CASE "M"
'        Tra.Typ = "CM"
'        CusEdit
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
i1: GET #f, n, Inx
    COLOR 11, 1: LOCATE r, 43: PRINT USING " & ณ&ณ&ณ& "; Inx.Typ + Inx.Grp; Inx.Cod; Inx.Mdl; Inx.Ser: KeyPres n$
    COLOR 16, 3: LOCATE r, 43: PRINT USING " & ณ&ณ&ณ& "; Inx.Typ + Inx.Grp; Inx.Cod; Inx.Mdl; Inx.Ser
    SELECT CASE n$
    CASE CHR$(0) + "P", "2"
        IF n + 1 > t GOTO i1 ELSE n = n + 1
        IF r + 1 > 21 THEN CALL Scroll(44, 7, 75, 21, 1) ELSE r = r + 1
    CASE CHR$(0) + "H", "8"
        IF n - 1 < 1 GOTO i1 ELSE n = n - 1
        IF r - 1 < 7 THEN CALL BkScroll(44, 7, 75, 21, 1) ELSE r = r - 1
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
'       InvRtrv
        COLOR 11, 5
        LOCATE 13, 4:  PRINT USING "&ณ&ณ&"; Cod.Typ; Inv.Cod; Cod.Des;
        LOCATE 15, 23: PRINT Status(Inv.Sta);
        LOCATE 16, 23: PRINT Cod.Grp;
        LOCATE 17, 23: PRINT Inv.Mdl;
        LOCATE 18, 23: PRINT Inv.Ser;
        IF Pas.Lvl > "5" THEN
           LOCATE 21, 23: PRINT USING "#######"; Inv.Tdr;
           LOCATE 22, 23: PRINT USING "####.##"; Inv.Tin / 100;
           LOCATE 24, 23: PRINT Inv.Prc;
           LOCATE 25, 26: PRINT Cod.Fmv;
           LOCATE 26, 26: PRINT Cod.Dfm;
           LOCATE 27, 26: PRINT Cod.Dtr;
           LOCATE 28, 25: PRINT Cod.Dep;
        END IF
        LOCATE 30, 24: PRINT Inv.Par;
        LOCATE 31, 24: PRINT Inv.Lab;
        LOCATE 35, 3:  PRINT USING "&ณ&ณ&"; Inv.Dtp; Inv.Dtr; Inv.Dta;
'       InvRate 17, 36
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
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR

           CASE 2 'for reinstatement, same value
                
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR
                
           CASE 3 'for a combo item
                IF FindCod(Inv.Cod) = 0 THEN ERROR 200
                
                   n = x: b = n - r + 7
                   IF b + 14 > t THEN b = t - 14: r = n - b + 7
                   GOSUB i2
                   EXIT FOR
                
           END SELECT
        END IF
        NEXT
    END SELECT
    GOTO i1
i2: COLOR 16, 3: LOCATE 7
    FOR x = b TO t
    GET #f, x, Inx
    LOCATE , 44
    PRINT USING "& ณ&ณ&ณ&"; Inx.Typ + Inx.Grp; Inx.Cod; Inx.Mdl; Inx.Ser
    IF CSRLIN > 21 THEN EXIT FOR
    NEXT
    RETURN
I3: PCOPY 1, 0
END FUNCTION

DEFSNG A-Q, S-Z
SUB WinMssg (f$)
    r = 16
    PCOPY 0, 3
    COLOR 4, 3
    LOCATE r + 1, 31: PRINT STRING$(4 + LEN(f$), " ");
    LOCATE r + 2, 31: PRINT "  "; f$; "  ";
    LOCATE r + 3, 31: PRINT STRING$(4 + LEN(f$), " ");
    COLOR 11
    LOCATE r + 2, 30: PRINT "Û";
    LOCATE r + 3, 30: PRINT "Û";
    LOCATE r + 4, 30: PRINT "ÛÛÛÛ"; STRING$(LEN(f$), "Û");
    SOUND 50, 3: KeyPres f$
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

SUB WinRate ()
    PCOPY 0, 1: CALL MhDisplayByName(0, 0, "List-RAT", e)
    FOR z = 1 TO 3
        r = 19 + z
        qPrint Rat.Mtr(z), r, 29, 0, 1
        qPrint Rat.Mrt(z), r, 33, 0, 1
        qPrint Rat.Wtr(z), r, 41, 0, 1
        qPrint Rat.Wrt(z), r, 45, 0, 1
    NEXT
    KeyPres f$
    PCOPY 1, 0
END SUB

FUNCTION WinRcts ()
'   RctAgrt Agx.Agr, f, t
    IF f = 0 THEN
       WinMssg "No receipts for this agreement"
       WinRcts = 1
       EXIT FUNCTION
    END IF
    r = 20: b = f: n = f
    PCOPY 0, 3: CALL MhDisplayByName(0, 0, "List-RCT", e)
    GOSUB m2
M1: GET #21, n, Rcx: GET #19, Rcx.Num, Rct
    COLOR 15, 1: LOCATE r, 23: PRINT USING "&ณ&ณ&ณ&"; Rct.Agr; Rct.Num; Rct.Dat; Rct.Amt
    KeyPres f$
    COLOR 27, 1: LOCATE r, 23: PRINT USING "&ณ&ณ&ณ&"; Rct.Agr; Rct.Num; Rct.Dat; Rct.Amt
    SELECT CASE f$
    CASE CHR$(0) + "P"
         IF n + 1 > t GOTO M1 ELSE n = n + 1
         IF r + 1 > 34 THEN CALL Scroll(23, 20, 54, 34, 1) ELSE r = r + 1
    CASE CHR$(0) + "H"
         IF n - 1 < f GOTO M1 ELSE n = n - 1
         IF r - 1 < 20 THEN CALL BkScroll(23, 20, 54, 34, 1) ELSE r = r - 1
    CASE CHR$(13)
         PCOPY 3, 0
         EXIT FUNCTION
    CASE ELSE
         WinRcts = 1
         PCOPY 3, 0
         EXIT FUNCTION
    END SELECT
    GOTO M1

m2: COLOR 27, 1: LOCATE 20
    FOR x = b TO t
    GET #21, x, Rcx: GET #19, Rcx.Num, Rct
    LOCATE , 23: PRINT USING "&ณ&ณ&ณ&"; Rct.Agr; Rct.Num; Rct.Dat; Rct.Amt
    IF CSRLIN > 34 THEN EXIT FOR
    NEXT
    RETURN
END FUNCTION

FUNCTION WinStor ()
    PCOPY 0, 1: : CALL MhDisplayByName(0, 0, "List-STO", e)
    Bot = 1: Top = 99: r = 5: n = 1: GOSUB S2
s1: GET #6, n, Sto
    COLOR 15, 3: LOCATE r, 41: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
    KeyPres n$
    COLOR 0, 7: LOCATE r, 41: PRINT USING "&ณ&"; Sto.Num; Sto.Nam
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

