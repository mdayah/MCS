DEF SEG = 0
IF (PEEK(&H410) AND &H30) <> &H30 THEN sg = &HB800 ELSE sg = &HB000
DEF SEG = 0

CLS : COLOR 11, 1
PRINT : PRINT : PRINT
LOCATE , 20: PRINT "ษอออออออออออออออออออออออออออออออออออออออป"
LOCATE , 20: PRINT "บ                                       บ"
LOCATE , 20: PRINT "บ                                       บ"
LOCATE , 20: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
LOCATE , 20: PRINT "บ                                       บ"
LOCATE , 20: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
LOCATE , 20: PRINT "บ  Michel                     A. Dayah  บ"
LOCATE , 20: PRINT "ฬออออออัออออออออออออออออออออออออออออออออน"
LOCATE , 20: PRINT "บ  F1  ณ  Enter New Sales Invoices.     บ"
LOCATE , 20: PRINT "บ  F2  ณ  Update Old Sales Invoices.    บ"
LOCATE , 20: PRINT "บ  F3  ณ  Enter Check Receipts.         บ"
LOCATE , 20: PRINT "บ  F4  ณ  Daily Receivables Reports.    บ"
LOCATE , 20: PRINT "บ  F5  ณ  Receivables Report Menu.      บ"
LOCATE , 20: PRINT "บ  F6  ณ  Materials Inventory Master.   บ"
LOCATE , 20: PRINT "บ  F7  ณ  Customer Master File.         บ"
LOCATE , 20: PRINT "บ  F8  ณ  Utilities Menu.               บ"
LOCATE , 20: PRINT "บ  F9  ณ  End - of - Month Closing.     บ"
LOCATE , 20: PRINT "บ  F0  ณ  End - of - Year Closing.      บ"
LOCATE , 20: PRINT "ศออออออฯออออออออออออออออออออออออออออออออผ"
COLOR 12, 0: LOCATE 5, 22: PRINT "    McKenry Produce Company, Inc.    "
			 LOCATE 6, 22: PRINT "717 Willow Avenue, Knoxville TN 37915"
COLOR 14, 4: LOCATE 8, 22: PRINT "A C C O U N T S   R E C E I V A B L E"
COLOR 15, 0: LOCATE 10, 35: PRINT " MAIN MENU "
DEF SEG = sg: BSAVE "Menu.SCR", 0, 4000: DEF SEG

LOCATE 10, 36: PRINT " REPORTS ": COLOR 10, 1
LOCATE 12, 30: PRINT "Sales Invoices Register      "
LOCATE 13, 30: PRINT "Detail Inventory Sales       "
LOCATE 14, 30: PRINT "Summary Sales Register       "
LOCATE 15, 30: PRINT "Check Receipts Register      "
LOCATE 16, 30: PRINT "Print Address Lablels        "
LOCATE 17, 30: PRINT "Sales Totals Between Dates   "
LOCATE 18, 30: PRINT "                             "
LOCATE 19, 30: PRINT "                             "
LOCATE 20, 30: PRINT "                             "
LOCATE 21, 30: PRINT "                             "
DEF SEG = sg: BSAVE "DailyRpt.SCR", 0, 4000: DEF SEG

LOCATE 10, 36: PRINT " REPORTS ": COLOR 10, 1
LOCATE 12, 30: PRINT "Aging of Receivables Report  "
LOCATE 13, 30: PRINT "Customer's Open Invoices     "
LOCATE 14, 30: PRINT "Open Invoices by Type        "
LOCATE 15, 30: PRINT "Customer's Listing [Month]   "
LOCATE 16, 30: PRINT "Customer's Listing [Year ]   "
LOCATE 17, 30: PRINT "Reprint Selected Statement   "
LOCATE 18, 30: PRINT "Print Customer's Statement   "
LOCATE 19, 30: PRINT "Print Statements by TYPE     "
LOCATE 20, 30: PRINT "                             "
LOCATE 21, 30: PRINT "                             "
DEF SEG = sg: BSAVE "Reports.SCR", 0, 4000: DEF SEG
STOP
LOCATE 10, 36: PRINT " UTILITY ": COLOR 10, 1
LOCATE 12, 30: PRINT "Cancel a Check Payment       "
LOCATE 13, 30: PRINT "Purge Old Paid Invoices      "
LOCATE 14, 30: PRINT "Purge Old Check Receipts     "
LOCATE 15, 30: PRINT "Recost Month's Invoices      "
LOCATE 16, 30: PRINT "                             "
LOCATE 17, 30: PRINT "                             "
LOCATE 18, 30: PRINT "                             "
LOCATE 19, 30: PRINT "                             "
LOCATE 20, 30: PRINT "                             "
LOCATE 21, 30: PRINT "                             "
DEF SEG = sg: BSAVE "Utility.SCR", 0, 4000: DEF SEG
STOP
CLS : COLOR 11, 0
WIDTH 80, 50
PRINT "ษออออัออออออออออออออออออออออออออออปษออออออออออออออัออออออออออออออออออออออออออออป"
PRINT "บ    ณ                            บบ Customer:    ณ                            บ"
PRINT "บ    ณ                            บบ              รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT "บ    ณ                            บบ              ณ      BILLING ADDRESS       บ"
PRINT "บ    ณ                            บบ              รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT "บ    ณ                            บบ       Contactณ                            บ"
PRINT "บ    ณ                            บบ     Telephoneณ                            บ"
PRINT "บ    ณ                            บบ       Addressณ                            บ"
PRINT "บ    ณ                            บบ        Streetณ                            บ"
PRINT "บ    ณ                            บบCity State Zipณ                            บ"
PRINT "บ    ณ                            บบ              รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT "บ    ณ                            บบ              ณ      SHIPPING ADDRESS      บ"
PRINT "บ    ณ                            บบ              รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT "บ    ณ                            บบ       Contactณ                            บ"
PRINT "บ    ณ                            บบ     Telephoneณ                            บ"
PRINT "บ    ณ                            บบ       Addressณ                            บ"
PRINT "บ    ณ                            บบ        Streetณ                            บ"
PRINT "บ    ณ                            บบCity State Zipณ                            บ"
PRINT "บ    ณ                            บบ              รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT "บ    ณ                            บบ              ณ     CREDIT INFORMATION     บ"
PRINT "บ    ณ                            บบ              รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT "บ    ณ                            บบStatement Typeณ                            บ"
PRINT "บ    ณ                            บบ Starting Dateณ                            บ"
PRINT "บ    ณ                            บบ     Bank Nameณ                            บ"
PRINT "บ    ณ                            บบAccount Numberณ                            บ"
PRINT "บ    ณ                            บบ Sales Tax Numณ                            บ"
PRINT "บ    ณ                            บฬออออออออออออออุออออออออออออออออออออออออออออน"
PRINT "บ    ณ                            บบ         Termsณ   % charge net in    days  บ"
PRINT "บ    ณ                            บฬออออออออออออออุอออออออออออออัออออออออออออออน"
PRINT "บ    ณ                            บบ              ณ  M O N T H  ณ   Y E A R    บ"
PRINT "บ    ณ                            บบSales Revenuesณ             ณ              บ"
PRINT "บ    ณ                            บบCredits.......ณ             ณ              บ"
PRINT "บ    ณ                            บบSales Tax.....ณ             ณ              บ"
PRINT "บ    ณ                            บบFrieght Chargeณ             ณ              บ"
PRINT "บ    ณ                            บบ              รฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT "บ    ณ                            บบNet Amount....ณ             ณ              บ"
PRINT "บ    ณ                            บบBeg. Balance..ณ             ณ              บ"
PRINT "บ    ณ                            บบService Chargeณ             ณ              บ"
PRINT "บ    ณ                            บบ              รฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT "บ    ณ                            บบAmount Due....ณ             ณ              บ"
PRINT "บ    ณ                            บบAmount Paid...ณ             ณ              บ"
PRINT "บ    ณ                            บบ              รฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT "บ    ณ                            บบBalance Due...ณ             ณ              บ"
PRINT "บ    ณ                            บศออออออออออออออฯอออออออออออออฯออออออออออออออผ"
PRINT "บ    ณ                            บษอออออัอออออออออออัออออออออออออัออออออออออออป"
PRINT "บ    ณ                            บบ     ณ   Sales   ณ    Cost    ณGross Profitบ"
PRINT "บ    ณ                            บบMonthณ           ณ            ณ            บ"
PRINT "บ    ณ                            บบ Yearณ           ณ            ณ            บ"
PRINT "ศออออฯออออออออออออออออออออออออออออผศอออออฯอออออออออออฯออออออออออออฯออออออออออออผ"; : LOCATE 50, 1: COLOR 11, 1
PRINT " x x x x xx PgUp  PgDn  Home  End  ÿAdd  Modify  Erase  Display  List  Invoices ";
COLOR 15, 6
LOCATE 4, 52:  PRINT "      BILLING ADDRESS       "
LOCATE 12, 52: PRINT "      SHIPPING ADDRESS      "
LOCATE 20, 52: PRINT "     CREDIT INFORMATION     "
LOCATE 30, 52: PRINT "  M O N T H  "; : LOCATE , 66: PRINT "   Y E A R    "
LOCATE 46, 43: PRINT "   Sales   ";
LOCATE 46, 55: PRINT "    Cost    ";
LOCATE 46, 68: PRINT "Gross Profit";

COLOR 14, 1
LOCATE 50, 2: PRINT CHR$(24); " "; CHR$(25); " "; CHR$(27); " "; CHR$(26); " "; CHR$(17); "ู ";
PRINT "PgUp  PgDn  Home  End";
LOCATE , 37: PRINT "A"; : LOCATE , 42: PRINT "M"; : LOCATE , 50: PRINT "E";
LOCATE , 57: PRINT "D"; : LOCATE , 66: PRINT "L"; : LOCATE , 72: PRINT "I";
COLOR 1, 3
FOR x = 2 TO 48: LOCATE x, 2
PRINT "    ณ                            ";
NEXT
DEF SEG = sg: BSAVE "Customer.SCR", 0, 8000

INVOICE:
CLS
COLOR 0, 3
PRINT
PRINT "                         McKENRY PRODUCE COMPANY, INC.                          "
PRINT "                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                          "
PRINT "                               717 Willow Avenue                                "
PRINT "                          Knoxville,  Tennessee 37915                           "
PRINT "                                                                                "
PRINT "                                                                                ": COLOR 3, 0
PRINT "ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ ษออออออออออออัออออออออออัออออออออออออออัออออออออป"
PRINT "Ý          Þ  Invoice Number Þ บ   Referenceณ          ณ  Sales Amountณ        บ"
PRINT "Ý฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿Þ วฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤด       Returnsณ        บ"
PRINT "ÝCustomer:                   Þ บInvoice Dateณ          รฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤถ"
PRINT "Ý                            Þ บ    Due Dateณ          ณ     Net SalesณÛÛÛÛÛÛÛÛบ"
PRINT "Ý                            Þ วฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤด     Sales Taxณ        บ"
PRINT "Ý                            Þ บ   Date Paidณ          ณFreight Chargeณ        บ"
PRINT "Ý                            Þ บCheck Numberณ          รฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤถ"
PRINT "Ý                            Þ บCheck Amountณ          ณ    Net AmountณÛÛÛÛÛÛÛÛบ"
PRINT "฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿ ศออออออออออออฯออออออออออฯออออออออออออออฯออออออออผ"
PRINT
PRINT "ษอออัออออออออออออออออออออออออออออัออออออออัออัออออออออัอออออออัอออออออัออออออออป"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "วฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤลฤฤลฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤลฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤถ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "บ   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        บ"
PRINT "วฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤมฤฤลฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤมฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤถ"
PRINT "บ                               Cost of Goodsณ        ณ    Total SalesณÛÛÛÛÛÛÛÛบ"
PRINT "ศออออออออออออออออออออออออออออออออออออออออออออฯออออออออฯอออออออออออออออฯออออออออผ"
COLOR 12, 0
LOCATE 11, 2: PRINT "Customer:"
LOCATE 39, 33: PRINT "Cost of Goods";
COLOR 11, 1
LOCATE 20, 2: PRINT "Itm"
LOCATE 20, 6: PRINT "                           "
LOCATE 20, 35: PRINT "        "
LOCATE 20, 44: PRINT "  "
LOCATE 20, 47: PRINT "  Cost  "
LOCATE 20, 56: PRINT "Standrd"
LOCATE 20, 64: PRINT " Unit  "
LOCATE 20, 72: PRINT "        "
LOCATE 21, 2: PRINT "Num"
LOCATE 21, 6: PRINT "        Description        "
LOCATE 21, 35: PRINT "Quantity"
LOCATE 21, 44: PRINT "Ut"
LOCATE 21, 47: PRINT "Of Goods"
LOCATE 21, 56: PRINT " Price "
LOCATE 21, 64: PRINT " Price "
LOCATE 21, 72: PRINT " Amount "
COLOR 6, 0
LOCATE 9, 33:  PRINT "   Reference"
LOCATE 11, 33: PRINT "Invoice Date"
LOCATE 12, 33: PRINT "    Due Date"
COLOR 13, 0
LOCATE 14, 33: PRINT "   Date Paid"
LOCATE , 33:   PRINT "Check Number"
LOCATE , 33:   PRINT "Check Amount"
COLOR 10, 0
LOCATE 9, 57:  PRINT "  Sales Amount"
LOCATE , 57:   PRINT "       Returns"
LOCATE 13, 57: PRINT "     Sales Tax"
LOCATE , 57:   PRINT "  Other Charge"
LOCATE 16, 57: PRINT "    Net Amount"
COLOR 0, 7
LOCATE 12, 57: PRINT "     Net Sales"
LOCATE 39, 56: PRINT "    Total Sales";
DEF SEG = sg: BSAVE "Invoices.SCR", 0, 8000

COLOR 0, 3
CLS
LOCATE , 12: PRINT
LOCATE , 12: PRINT "            ACCOUNTS RECEIVABLE CHECK RECEIPTS            "
LOCATE , 12: PRINT "            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~            ": COLOR 7, 0
LOCATE , 13: PRINT "                                                        "
LOCATE , 12: PRINT " ษออออออออออออออัอออออออออออออป ษออออออออออออัออออออออออป "
LOCATE , 12: PRINT " บ Customer Codeณ             บ บ Beg.Balanceณ          บ "
LOCATE , 12: PRINT " วฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤถ บ     Chargesณ          บ "
LOCATE , 12: PRINT " บ                            บ วฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤถ "
LOCATE , 12: PRINT " บ                            บ บ  Amount Dueณ          บ "
LOCATE , 12: PRINT " บ                            บ บ Amount Paidณ          บ "
LOCATE , 12: PRINT " บ                            บ วฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤถ "
LOCATE , 12: PRINT " บ                            บ บ Balance Dueณ          บ "
LOCATE , 12: PRINT " ศออออออออออออออออออออออออออออผ ศออออออออออออฯออออออออออผ "
LOCATE , 13: PRINT "                                                        "
PRINT
LOCATE , 13: PRINT "                                                        "
LOCATE , 12: PRINT " ษออออออออออัออออออออออัออออออออออัออออออออออัออออออออออป "
LOCATE , 12: PRINT " บ  Invoice ณ Invoice  ณ          ณ          ณ  Amount  บ "
LOCATE , 12: PRINT " บ  Number  ณ   Date   ณ Due Date ณNet Amountณ   Paid   บ "
LOCATE , 12: PRINT " วฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤถ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " บ          ณ          ณ          ณ          ณ          บ "
LOCATE , 12: PRINT " วฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤถ "
LOCATE , 12: PRINT " บ                           Totalณ          ณ          บ "
LOCATE , 12: PRINT " ฬออออออออออออออออัออออออออออัออออฯออออออออออุออออออออออน "
LOCATE , 12: PRINT " บ       Date Paidณ          ณ   Check Amountณ          บ "
LOCATE , 12: PRINT " บ    Check Numberณ          ณ               ณ          บ "
LOCATE , 12: PRINT " ศออออออออออออออออฯออออออออออฯอออออออออออออออฯออออออออออผ "
LOCATE , 13: PRINT "                                                        "

COLOR 5, 0
LOCATE 6, 15:  PRINT "Customer Code"
COLOR 6
LOCATE 6, 46:  PRINT "Beg.Balance"
LOCATE 7, 46:  PRINT "    Charges"
LOCATE 9, 46:  PRINT " Amount Due"
LOCATE 10, 46: PRINT "Amount Paid"
LOCATE 12, 46: PRINT "Balance Due"
COLOR 11, 1
LOCATE 18, 14: PRINT " Invoice  "
LOCATE 18, 25: PRINT " Invoice  "
LOCATE 18, 36: PRINT "          "
LOCATE 18, 47: PRINT "          "
LOCATE 18, 58: PRINT "  Amount  "
LOCATE 19, 14: PRINT " Number   "
LOCATE 19, 25: PRINT "   Date   "
LOCATE 19, 36: PRINT " Date Due "
LOCATE 19, 47: PRINT "Amount Due"
LOCATE 19, 58: PRINT "   Paid   "
LOCATE 42, 14: PRINT "                           Total"
COLOR 10, 0
LOCATE 44, 21: PRINT "Date Paid"
LOCATE 45, 18: PRINT "Check Number"
LOCATE 44, 45: PRINT "Check Amount"
LOCATE 45, 51: PRINT "      "
DEF SEG = sg: BSAVE "Receipts.Scr", 0, 8000

WIDTH 80, 43
CLS : COLOR 7, 4
PRINT
PRINT
PRINT
PRINT
PRINT
LOCATE , 13: PRINT "                                                        "
LOCATE , 13: PRINT "                CANCEL A CHECK RECEIPT                  "
LOCATE , 13: PRINT "                ~~~~~~~~~~~~~~~~~~~~~~                  "
LOCATE , 13: PRINT "                                                        "
LOCATE , 13: PRINT " ษออออออออออออออัอออออออออออออป ษออออออออออัออออออออออป "
LOCATE , 13: PRINT " บ  Check Numberณ             บ บ  Invoice ณ  Amount  บ "
LOCATE , 13: PRINT " วฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤถ บ  Number  ณ   Paid   บ "
LOCATE , 13: PRINT " บ     Date Paidณ             บ วฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤถ "
LOCATE , 13: PRINT " บ  Check Amountณ             บ บ          ณ          บ "
LOCATE , 13: PRINT " ศออออออออออออออฯอออออออออออออผ บ          ณ          บ "
LOCATE , 13: PRINT "                                บ          ณ          บ "
LOCATE , 13: PRINT "                                บ          ณ          บ "
LOCATE , 13: PRINT " ษออออออออออออออัอออออออออออออป บ          ณ          บ "
LOCATE , 13: PRINT " บ Customer Codeณ             บ บ          ณ          บ "
LOCATE , 13: PRINT " วฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤถ บ          ณ          บ "
LOCATE , 13: PRINT " บ                            บ บ          ณ          บ "
LOCATE , 13: PRINT " บ                            บ บ          ณ          บ "
LOCATE , 13: PRINT " บ                            บ บ          ณ          บ "
LOCATE , 13: PRINT " บ                            บ บ          ณ          บ "
LOCATE , 13: PRINT " บ                            บ บ          ณ          บ "
LOCATE , 13: PRINT " ศออออออออออออออออออออออออออออผ บ          ณ          บ "
LOCATE , 13: PRINT "                                บ          ณ          บ "
LOCATE , 13: PRINT "                                บ          ณ          บ "
LOCATE , 13: PRINT "                                บ          ณ          บ "
LOCATE , 13: PRINT "                                บ          ณ          บ "
LOCATE , 13: PRINT "                                บ          ณ          บ "
LOCATE , 13: PRINT "                                บ          ณ          บ "
LOCATE , 13: PRINT "                                บ          ณ          บ "
LOCATE , 13: PRINT "                                ศออออออออออฯออออออออออผ "
LOCATE , 13: PRINT "                                                        "
COLOR 15, 4
LOCATE 19, 15:  PRINT " Customer Code"
COLOR 11, 1
LOCATE 11, 46: PRINT "  Invoice "
LOCATE 11, 57: PRINT "  Amount  "
LOCATE 12, 46: PRINT "  Number  "
LOCATE 12, 57: PRINT "   Paid   "
COLOR 10, 0
LOCATE 11, 15: PRINT "  Check Number"
COLOR 11, 4
LOCATE 13, 15: PRINT "     Date Paid"
LOCATE 14, 15: PRINT "  Check Amount"
DEF SEG = sg: BSAVE "Cancel.Scr", 0, 6880
STOP

