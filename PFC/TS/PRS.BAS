DEF SEG = 0 'determine type of monitor
IF (PEEK(&H410) AND &H30) <> &H30 THEN sg = &HB800 ELSE sg = &HB000
DEF SEG
CLS : COLOR 0, 3
PRINT : PRINT : PRINT
LOCATE , 20: PRINT "ษอออออออออออออออออออออออออออออออออออออออป"
LOCATE , 20: PRINT "บ                                       บ"
LOCATE , 20: PRINT "บ                                       บ"
LOCATE , 20: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
LOCATE , 20: PRINT "บ                                       บ"
LOCATE , 20: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
LOCATE , 20: PRINT "บ  Michel                     A. Dayah  บ"
LOCATE , 20: PRINT "ฬออออออัออออออออออออออออออออออออออออออออน"
LOCATE , 20: PRINT "บ  F1  ณ  Enter or Update Timecards.    บ"
LOCATE , 20: PRINT "บ  F2  ณ  Print ProTemp Time Sheets.    บ"
LOCATE , 20: PRINT "บ  F3  ณ  Maintain ProTemp Employees.   บ"
LOCATE , 20: PRINT "บ  F4  ณ  Print ProTemp Employees.      บ"
LOCATE , 20: PRINT "บ  F5  ณ                                บ"
LOCATE , 20: PRINT "บ  F6  ณ                                บ"
LOCATE , 20: PRINT "บ  F7  ณ                                บ"
LOCATE , 20: PRINT "บ  F8  ณ                                บ"
LOCATE , 20: PRINT "บ  F9  ณ                                บ"
LOCATE , 20: PRINT "บ  F0  ณ  Exit ProTemp Program.         บ"
LOCATE , 20: PRINT "ศออออออฯออออออออออออออออออออออออออออออออผ"
COLOR 14, 1: LOCATE 5, 22: PRINT "     Processed Foods Corporation     "
             LOCATE 6, 22: PRINT "707 Willow Avenue, Knoxville TN 37915"
COLOR 11, 4: LOCATE 8, 22: PRINT "  C O B B L E   T I M E S H E E T S  "
COLOR 15, 0: LOCATE 10, 35: PRINT " MAIN MENU "
DEF SEG = sg: BSAVE "Screen.Mnu", 0, &H1000: DEF SEG


COLOR 7, 1
LOCATE , 18: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออป"
LOCATE , 18: PRINT "บÛÛÛÛÛÛÛÛÛÛÛÛÛ T I M E C A R D ÛÛÛÛÛÛÛÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออผ": COLOR 2, 0
LOCATE , 18: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออป"
LOCATE , 18: PRINT "บ Employee:                                 บ"
LOCATE , 18: PRINT "ฬออออออออออออัอัออออัออออัออออัออออัอัออออออน"
LOCATE , 18: PRINT "บ Monday     ณ ณ    ณ    ณ    ณ    ณ ณÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤด รฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤด รฤฤฤฤฤฤบ"
LOCATE , 18: PRINT "บ Tuesday    ณ ณ    ณ    ณ    ณ    ณ ณÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤด รฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤด รฤฤฤฤฤฤถ"
LOCATE , 18: PRINT "บ Wednesday  ณ ณ    ณ    ณ    ณ    ณ ณÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤด รฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤด รฤฤฤฤฤฤถ"
LOCATE , 18: PRINT "บ Thursday   ณ ณ    ณ    ณ    ณ    ณ ณÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤด รฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤด รฤฤฤฤฤฤถ"
LOCATE , 18: PRINT "บ Friday     ณ ณ    ณ    ณ    ณ    ณ ณÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤด รฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤด รฤฤฤฤฤฤถ"
LOCATE , 18: PRINT "บ Saturday   ณ ณ    ณ    ณ    ณ    ณ ณÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤด รฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤด รฤฤฤฤฤฤถ"
LOCATE , 18: PRINT "บ Sunday     ณ ณ    ณ    ณ    ณ    ณ ณÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "ฬออออออออออออฯอฯออออฯออออฯออออฯออออฯอุออออออน"
LOCATE , 18: PRINT "บ Total hours                        ณÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "บ Regular hours                      ณÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "บ Overtime hours                     ณÛÛÛÛÛÛบ"
LOCATE , 18: PRINT "ศออออออออออออออออออออออออออออออออออออฯออออออผ"; : COLOR 1, 7: LOCATE 25, 1
LOCATE , 18: PRINT " Hit [*] to save     [Esc] to return to menu ";
COLOR 14, 1: LOCATE 2, 20: PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛ T I M E C A R D ÛÛÛÛÛÛÛÛÛÛÛÛ";
COLOR 11, 0
LOCATE 7, 20: PRINT "Monday"
LOCATE 9, 20: PRINT "Tuesday"
LOCATE 11, 20: PRINT "Wednesday"
LOCATE 13, 20: PRINT "Thursday"
LOCATE 15, 20: PRINT "Friday"
LOCATE 17, 20: PRINT "Saturday"
LOCATE 19, 20: PRINT "Sunday"
DEF SEG = sg: BSAVE "Screen.Tim", 0, &H1000: DEF SEG
PRINT "ฺยยยลยยยยืยยยยลยยยยืยยยยลยยยยืยยยยลยยยยืยยยยลยยยยืยยยยลยยยยืยยยยลยยยยืยยยยลยยยยถ"
CLS : COLOR 14, 4: PRINT : PRINT
PRINT
LOCATE , 17: PRINT "ษออออออออออออออออออออออออออออออออออออออออออออป"
LOCATE , 17: PRINT "บ ÛÛÛÛÛ E M P L O Y E E    M A S T E R ÛÛÛÛÛ บ"
LOCATE , 17: PRINT "ศออออออออออออออออออออออออออออออออออออออออออออผ": COLOR 1, 3
LOCATE , 17: PRINT "ษอออออออออออออออออัออออออออออออออออออออออออออป"
LOCATE , 17: PRINT "บ Employee Number ณ __                       บ"
LOCATE , 17: PRINT "บ   Employee Name ณ ________________________ บ"
LOCATE , 17: PRINT "บ Social Security ณ ___-__-____              บ"
LOCATE , 17: PRINT "บ        Pay Rate ณ __.__                    บ"
LOCATE , 17: PRINT "บ    Billing Rate ณ __.__                    บ"
LOCATE , 17: PRINT "ศอออออออออออออออออฯออออออออออออออออออออออออออผ": COLOR 14, 4
LOCATE , 17: PRINT "         Hit [Esc] to return to menu          "
DEF SEG = sg: BSAVE "Screen.EMP", 0, &H1000: DEF SEG

