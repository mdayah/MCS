# PFC - Processed Foods Corporation ERP System

## Overview

PFC is the **most complete and feature-rich** business management system in this repository. It provides a full Enterprise Resource Planning (ERP) solution including accounting, inventory, order entry, payroll, and financial reporting modules.

**Company:** Processed Foods Corporation, 707 Willow Avenue, Knoxville, TN 37915

## System Classification

| Attribute | Value |
|-----------|-------|
| Type | Complete ERP System |
| Language | Visual Basic for MS-DOS |
| Era | 1988-1999 |
| Status | Most feature-complete |
| Modules | 12 integrated modules |

## Module Overview

### Core Accounting
| Module | Directory | Purpose | Lines |
|--------|-----------|---------|-------|
| **GL** | GL/ | General Ledger | ~500 |
| **AP** | AP/ | Accounts Payable | 1,766 |
| **AR** | AR/ | Accounts Receivable | 1,766 |
| **FS** | FS/ | Financial Statements | ~200 |

### Operations
| Module | Directory | Purpose | Lines |
|--------|-----------|---------|-------|
| **OE** | OE/ | Order Entry | 232 |
| **IN** | IN/ | Inventory Management | ~1,500 |
| **PR** | PR/ | Payroll | 1,102 |
| **TS** | TS/ | Time Sheets | ~120 |

### Support
| Module | Directory | Purpose |
|--------|-----------|---------|
| **BI** | BI/ | Business Intelligence (Data Definitions) |
| **LIB** | LIB/ | Custom Libraries |
| **VBASIC** | VBASIC/ | Development Environment |
| **NOVA** | NOVA/ | Screen Editor |

## Directory Structure

```
PFC/
├── AP/                 # Accounts Payable (240K)
│   ├── AP.BAS         # Main AP source (1,766 lines)
│   ├── AP.EXE         # Compiled executable
│   ├── AP.SLB         # Screen library
│   ├── VENDORS.MAS    # Vendor master file
│   ├── INVOICES.MAS   # Invoice records
│   └── ACCOUNTS.DTA   # Account data
│
├── AR/                 # Accounts Receivable (272K)
│   ├── ARP.BAS        # Main AR source (1,766 lines)
│   ├── ARP.EXE        # Compiled executable
│   ├── ARS.SLB        # Screen library
│   ├── CUSTOMER.MAS   # Customer master
│   └── RECEIPTS.MAS   # Receipt records
│
├── GL/                 # General Ledger (148K)
│   ├── GLP.BAS        # GL source
│   ├── GLP.EXE        # Compiled executable
│   ├── JOURNAL.MAS    # Journal entries
│   └── SCREEN.*       # Screen definitions
│
├── FS/                 # Financial Statements (108K)
│   ├── FSP.BAS        # FS source
│   ├── FSP.EXE        # Compiled executable
│   ├── INCOME.PIC     # Income statement format
│   └── SHEET.PIC      # Balance sheet format
│
├── OE/                 # Order Entry (176K)
│   ├── OEP.BAS        # OE source (232 lines)
│   ├── OEP.EXE        # Compiled executable
│   ├── OES.SLB        # Screen library
│   ├── ORDERS.MAS     # Order master
│   └── ORDERS.OPN     # Open orders
│
├── IN/                 # Inventory (420K)
│   ├── INJ.BAS        # Job costing (517 lines)
│   ├── PUP.BAS        # Product master (653 lines)
│   ├── INJ.EXE        # Job costing executable
│   ├── PUP.EXE        # Product executable
│   ├── MATERIAL.MAS   # Material master
│   ├── PRODUCTS.MAS   # Product master
│   └── RECIPES.MAS    # Recipe/formula master
│
├── PR/                 # Payroll (172K)
│   ├── PRP.BAS        # Payroll source (1,102 lines)
│   ├── PRP.EXE        # Compiled executable
│   ├── PRS.SLB        # Screen library
│   ├── EMPLOYEE.MAS   # Employee master
│   └── TIMECARD.MAS   # Timecard records
│
├── TS/                 # Time Sheets (36K)
│   ├── PRP.BAS        # Time sheet source
│   └── PRS.BAS        # Time sheet support
│
├── BI/                 # Data Definitions (104K)
│   ├── LIBRARY.DEC    # Shared declarations
│   ├── ACCOUNTS.REC   # Account structure
│   ├── CUSTOMER.REC   # Customer structure
│   ├── VENDORS.REC    # Vendor structure
│   ├── EMPLOYEE.REC   # Employee structure
│   ├── PRODUCTS.REC   # Product structure
│   ├── MATERIAL.REC   # Material structure
│   ├── INVOICES.REC   # Invoice structure
│   ├── ORDERS.REC     # Order structure
│   ├── RECEIPTS.REC   # Receipt structure
│   ├── TIMECARD.REC   # Timecard structure
│   ├── PURCHASE.REC   # Purchase order structure
│   ├── RECIPES.REC    # Recipe structure
│   └── JOBCOST.REC    # Job costing structure
│
├── LIB/                # Libraries (896K)
├── VBASIC/             # Development Environment (9.8M)
└── NOVA/               # Screen Editor (428K)
```

## Key Features

### Accounts Payable (AP)
- Vendor management
- Invoice entry and editing
- Check register and printing
- Payment tracking
- Month/year-end processing

### Accounts Receivable (AR)
- Customer management
- Sales invoice creation
- Receipt recording
- Aging reports
- Customer summaries

### General Ledger (GL)
- Chart of accounts
- Journal entry
- Trial balance
- Account reconciliation

### Financial Statements (FS)
- Income statement generation
- Balance sheet creation
- Custom report formatting

### Order Entry (OE)
- Sales order entry
- Order tracking
- Open order management
- Customer order history

### Inventory (IN)
- Material management
- Product definition
- Recipe/formula management
- Job costing
- Inventory tracking
- Purchase tracking

### Payroll (PR)
- Employee management
- Timecard entry
- Payroll processing
- Tax calculations
- Check printing

## Data Model

### Master Files
| File | Purpose |
|------|---------|
| CUSTOMER.MAS | Customer records |
| VENDORS.MAS | Vendor records |
| EMPLOYEE.MAS | Employee records |
| PRODUCTS.MAS | Product catalog |
| MATERIAL.MAS | Raw materials |
| ACCOUNTS.MAS | Chart of accounts |
| RECIPES.MAS | Manufacturing formulas |

### Transaction Files
| File | Purpose |
|------|---------|
| INVOICES.MAS | AP/AR invoices |
| JOURNAL.MAS | GL entries |
| ORDERS.MAS | Sales orders |
| RECEIPTS.MAS | Cash receipts |
| TIMECARD.MAS | Employee timecards |

## Comparison with Other Systems

| Feature | PFC | MPC | MCS | BAJ/HBS/IMM |
|---------|-----|-----|-----|-------------|
| General Ledger | ✓ | ✓ | ✓ | - |
| Accounts Payable | ✓ | ✓ | - | - |
| Accounts Receivable | ✓ | ✓ | - | - |
| Financial Statements | ✓ | ✓ | - | - |
| Order Entry | ✓ | - | - | - |
| Inventory | ✓ | ✓ | - | - |
| Payroll | ✓ | ✓ | ✓ | ✓ |
| Job Costing | ✓ | - | - | - |
| Recipe Management | ✓ | - | - | - |
| Time Sheets | ✓ | - | - | - |

**PFC is the most complete system**, containing all modules found in other projects plus unique features like:
- Order Entry (OE)
- Recipe/Formula Management
- Job Costing
- Dedicated Time Sheet module

## Running the Application

```bash
# Start DOSBox-X
./dosbox.sh

# In DOSBox, navigate to module
CD AP
AP.EXE
```

## Development

Edit batch files are provided for each module:
```
EDIT_AP.BAT, EDIT_ARP.BAT, EDIT_FS.BAT, EDIT_GL.BAT,
EDIT_INJ.BAT, EDIT_OEP.BAT, EDIT_PUP.BAT, EDIT_PRP.BAT
```

## Version History

| Date | Description |
|------|-------------|
| Jul 1988 | Initial development |
| 1988-1999 | Active development |

## Related Systems

PFC contains the superset of features. Other systems are subsets:
- **MPC** - Similar full system for McKenry Produce
- **MCS** - Accounting/Payroll subset
- **BAJ/HBS/IMM** - Payroll-focused subsets
