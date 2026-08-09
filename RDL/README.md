# RDL - Dental Laboratory Billing System

## Overview

RDL is a specialized **accounts receivable and invoicing system** built for **Rogers' Dental Laboratory** of Athens, Tennessee. It manages customer billing, invoice tracking, and payment processing for dental lab work — crowns, bridges, dentures, and the like.

**This is a distinct program** - focused on dental industry billing rather than general accounting.

## System Classification

| Attribute | Value |
|-----------|-------|
| Type | Industry-Specific AR System |
| Language | Visual Basic for MS-DOS |
| Industry | Dental Laboratory |
| Status | Standalone AR application |

## Directory Structure

```
RDL/
├── AR/                     # Accounts Receivable Application (49MB)
│   ├── AR.BAS             # Main source (56K)
│   ├── ARS.BAS            # Additional AR code (21K)
│   ├── AR.EXE             # Compiled executable (115K)
│   ├── AR.QLB / AR.LIB    # Libraries (contains screen() functions)
│   ├── CUSTOMER.DTA       # Customer database (16.68K)
│   ├── INVOICES.DTA       # Invoice records (3.1MB)
│   ├── PROCEDUR.DTA       # Procedure catalog (7.44K)
│   ├── INVOICES.INX       # Invoice index (160K)
│   └── SCREEN.*           # 27 screen definitions
│
├── VBASIC/                 # Development environment (9.8MB)
├── EDIT_AR.BAT             # Editor batch file (loads AR.BAS in VB)
└── dosbox.sh               # DOSBox launcher
```

## Key Features

### Customer Management
- Customer master file (138 records)
- Contact information
- Credit limits
- Monthly/yearly statistics

### Invoice Processing
- Invoice creation and editing
- Line item details
- Tax calculations
- Due date tracking
- ~3,800 invoice records

### Procedure Catalog
- Dental procedure definitions
- Pricing per procedure
- Tax flags
- Monthly/yearly metrics

### Payment Processing
- Payment entry
- Check tracking
- Account aging

### Reporting
- Invoice lists
- AR statements (aging)
- Customer reports
- Payment reports

## Data Structures

### Customer Record
```basic
TYPE Customer
  ID AS STRING * 2
  Name AS STRING * 30
  Address AS STRING * 30
  City AS STRING * 20
  State AS STRING * 2
  Zip AS STRING * 5
  Phone AS STRING * 14
  CreditLimit AS SINGLE
  ' Monthly/Yearly statistics
END TYPE
```

### Procedure Record
```basic
TYPE Procedure
  ID AS STRING * 3
  Name AS STRING * 30
  Quantity AS INTEGER
  SetInfo AS INTEGER
  Price AS SINGLE
  TaxFlag AS INTEGER
  ' Monthly/Yearly metrics
END TYPE
```

### Invoice Record
```basic
TYPE Invoice
  CustomerID
  BillingDate, DueDate, PaymentDate
  CheckNumber
  Gross, Tax, Net amounts
  LineItems (details)
END TYPE
```

## Screen Definitions (27 Screens)

| Screen | Purpose |
|--------|---------|
| SCREEN.INV | Invoice entry |
| SCREEN.CUS | Customer master |
| SCREEN.PRO | Procedure master |
| SCREEN.MAT | Materials/Inventory |
| SCREEN.PAY | Payment entry |
| SCREEN.LST | Invoice list |
| SCREEN.LOG | AR statement |
| SCREEN.MNU | Main menu |
| SCREEN.RPT | Reports menu |
| SCREEN.UTI | Utilities menu |

## Data Files

| File | Size | Records |
|------|------|---------|
| CUSTOMER.DTA | 16.68K | 138 customers |
| INVOICES.DTA | 3.1MB | ~3,800 invoices |
| PROCEDUR.DTA | 7.44K | Procedure catalog |
| INVOICES.INX | 160K | Date/customer index |
| INVOICES.CUS | 160K | Customer index |
| INVOICES.DUE | 160K | Due amount index |

## Development

- **Edit:** Use `EDIT_AR.BAT` to load AR.BAS in Visual Basic
- **Screens:** *.SLB files editable with `NOVA\NSEDIT.EXE`
- **Libraries:** AR.QLB contains screen() functions

## Comparison with Other Systems

| Feature | RDL | PFC/MPC | FOG |
|---------|-----|---------|-----|
| **Focus** | Dental AR | General Acct | Rent-to-Own |
| Accounts Receivable | ✓ | ✓ | - |
| Accounts Payable | - | ✓ | ✓ |
| General Ledger | - | ✓ | ✓ |
| Payroll | - | ✓ | ✓ |
| **Industry-Specific** | Dental | Food/Poultry | Rent-to-Own |
| Procedure Catalog | ✓ | - | - |

**RDL is a specialized vertical application** for dental laboratories, focusing on billing and AR rather than full accounting.

## Running the Application

```bash
# Start DOSBox-X
./dosbox.sh

# Run AR system
CD AR
AR.EXE
```

## Developer

Michel A. Dayah
2420 Congress Pk, Athens, TN 37303
