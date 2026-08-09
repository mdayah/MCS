' INVSCRUB.BAS - Anonymize the FOG office Inventor.MAS (a VBDOS ISAM database).
' ===========================================================================
' The office masters are ISAM, not flat records, so the python scrubber can't
' touch them. This walks the ISAM the same way his own RE-BUILD.BAS does
' (SETINDEX / MOVEFIRST / RETRIEVE / UPDATE / MOVENEXT) and overwrites the only
' PII in each record: the free-text Rmk (remarks) and the 3-char Ini (user
' initials). Every business field (model, serial, dates, depreciation) is kept,
' and because Rmk/Ini are not part of any index the UPDATE is in place and the
' indexes stay valid.
'
' Run (headless in DOSBox, PROISAM loaded):
'   PROISAM
'   VBDOS.EXE /RUN INVSCRUB.BAS
' Writes INVDONE.TXT ("OK <count>" or "ERR <n> at <count>") and exits to DOS.
DEFINT A-Z
REM $INCLUDE: 'INVENTOR.OFF'
DIM Inv AS Inventory
ON ERROR GOTO Oops
OPEN "INVENTOR.MAS" FOR ISAM Inventory "Inventory" AS #10
SETINDEX 10, "Sto"
MOVEFIRST #10
DO
    RETRIEVE #10, Inv
    Inv.Rmk = "SYNTHETIC DEMO REMARK"
    Inv.Ini = "DMO"
    UPDATE #10, Inv
    n& = n& + 1
    MOVENEXT #10
LOOP UNTIL EOF(10)
CLOSE
OPEN "INVDONE.TXT" FOR OUTPUT AS #2
PRINT #2, "OK"; n&
CLOSE #2
SYSTEM
Oops:
CLOSE
OPEN "INVDONE.TXT" FOR OUTPUT AS #2
PRINT #2, "ERR"; ERR; "at"; n&
CLOSE #2
SYSTEM
