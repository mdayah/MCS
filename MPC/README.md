# MPC - McKenry Produce Company Accounting System

## Overview

MPC is a **comprehensive accounting and business management system** for McKenry Produce Company, Inc. — a Knoxville **poultry** business ("Produce Company" in the old egg-and-poultry sense, not fruit and vegetables). It provides integrated accounting, accounts receivable, accounts payable, inventory, and payroll functionality.

**Company:** McKenry Produce Company, Inc., 717 Willow Avenue, Knoxville, TN 37915

## System Classification

| Attribute | Value |
|-----------|-------|
| Type | Full Accounting/ERP System |
| Language | Visual Basic for MS-DOS |
| Era | 1990-2001 |
| Status | Production system |
| Modules | 9 integrated modules |

## Module Overview

| Module | File | Size | Purpose |
|--------|------|------|---------|
| **AC** | AC.BAS | 21K | Main Menu/Accounting |
| **AP** | AP.BAS | 62K | Accounts Payable |
| **AR** | ARP/ARS/ARD/ART/ARR/ARI.BAS | 220K | Accounts Receivable |
| **GL** | GL.BAS | 20K | General Ledger |
| **PR** | PR.BAS | 68K | Payroll |
| **IN** | IN.BAS | 26K | Inventory |
| **FS** | FS.BAS | 12K | Financial Statements |
| **OP** | OP.BAS | - | Operations/Reports |
| **IC** | IC.BAS | - | Initial Configuration |

## Directory Structure

```
MPC/
├── SYSTEM/             # Main application (executables & source)
│   ├── AC.BAS         # Main accounting menu
│   ├── AP.BAS         # Accounts Payable
│   ├── ARP.BAS        # AR - Primary module
│   ├── ARS.BAS        # AR - Sales
│   ├── ARD.BAS        # AR - Details
│   ├── ART.BAS        # AR - Transactions
│   ├── ARR.BAS        # AR - Reports
│   ├── ARI.BAS        # AR - Initialization
│   ├── GL.BAS         # General Ledger
│   ├── PR.BAS         # Payroll
│   ├── IN.BAS         # Inventory
│   ├── FS.BAS         # Financial Statements
│   ├── MPC.EXE        # Main executable (257K)
│   ├── MPC.SLB        # Screen library (42K)
│   └── SCREEN.*       # Screen definitions
│
├── DATA/               # Business database files
│   ├── ACCOUNTS.MAS   # Chart of accounts (5.6K)
│   ├── CUSTOMER.MAS   # Customer master (233K)
│   ├── EMPLOYEE.MAS   # Employee master (64K)
│   ├── INVOICES.MAS   # Sales invoices (6.4M)
│   ├── JOURNAL.MAS    # Journal entries (54K)
│   ├── MATERIAL.MAS   # Inventory materials (195K)
│   ├── VENDORS.MAS    # Vendor master (90K)
│   ├── PINVOICE.MAS   # Purchase invoices (754K)
│   └── *.INX          # Index files
│
├── INCLUDE/            # Data structure definitions
│   ├── LIBRARY.REC    # Shared declarations
│   ├── ACCOUNTS.REC   # Account structure
│   ├── CUSTOMER.REC   # Customer structure
│   ├── EMPLOYEE.REC   # Employee structure
│   ├── MATERIAL.REC   # Material structure
│   ├── PAYABLES.REC   # AP structure
│   ├── JOURNAL.REC    # Journal structure
│   ├── PURCHASE.REC   # Purchase order structure
│   ├── SALES.REC      # Sales structure
│   └── RECEIPTS.REC   # Receipt structure
│
├── LIB/                # Utility libraries (submodule)
├── VBASIC/             # Development environment (submodule)
└── page*.png           # Documentation screenshots
```

## Key Features

### Accounts Receivable (AR) - Most Developed
The AR module is the most comprehensive, split into 6 sub-modules:
- **ARP.BAS** - Primary AR operations
- **ARS.BAS** - Sales processing
- **ARD.BAS** - Detailed transactions
- **ART.BAS** - Transaction history
- **ARR.BAS** - AR reporting
- **ARI.BAS** - AR initialization

### Accounts Payable (AP)
- Vendor management
- Invoice entry
- Check writing
- Payment tracking
- Aging reports

### General Ledger (GL)
- Chart of accounts
- Journal entries
- Trial balance
- Account reconciliation

### Payroll (PR)
- Employee management
- Payroll processing
- Tax calculations (FIT, FICA, Medicare)
- Check generation
- W-2 reporting

### Inventory (IN)
- Material master
- Purchase tracking
- Inventory valuation

### Financial Statements (FS)
- Balance sheet
- Income statement
- Trial balance

## Data Files

### Master Files
| File | Size | Records |
|------|------|---------|
| CUSTOMER.MAS | 233K | Customer database |
| VENDORS.MAS | 90K | Vendor database |
| EMPLOYEE.MAS | 64K | Employee records |
| MATERIAL.MAS | 195K | Inventory items |
| ACCOUNTS.MAS | 5.6K | Chart of accounts |

### Transaction Files
| File | Size | Purpose |
|------|------|---------|
| INVOICES.MAS | 6.4M | Sales invoices (largest) |
| PINVOICE.MAS | 754K | Purchase invoices |
| JOURNAL.MAS | 54K | GL journal entries |

## Compiled Executables

| File | Size | Purpose |
|------|------|---------|
| MPC.EXE | 257K | Main application |
| ARP.EXE | 116K | AR primary |
| ARS.EXE | 93K | AR sales |
| ARD.EXE | 86K | AR details |
| ART.EXE | 84K | AR transactions |
| ARR.EXE | 93K | AR reports |
| ARI.EXE | 70K | AR initialization |
| IC.EXE | 55K | Initial configuration |
| TIMECARD.EXE | 51K | Payroll timecard |

## Comparison with PFC

| Feature | MPC | PFC |
|---------|-----|-----|
| General Ledger | ✓ | ✓ |
| Accounts Payable | ✓ | ✓ |
| Accounts Receivable | ✓ (6 modules) | ✓ (1 module) |
| Financial Statements | ✓ | ✓ |
| Inventory | ✓ | ✓ |
| Payroll | ✓ | ✓ |
| Order Entry | - | ✓ |
| Job Costing | - | ✓ |
| Recipe Management | - | ✓ |

**MPC has more mature AR functionality** with 6 dedicated sub-modules, while PFC has more total modules including Order Entry and Manufacturing features.

## Running the Application

```bash
# Start DOSBox-X
./dosbox.sh

# Run main application
MPC.EXE
```

## Source Code Statistics

- **Total Source Lines:** ~10,873 lines
- **Modules:** 9 main programs
- **Executables:** 9 compiled programs
- **Database Files:** 11+ master files

## Version History

| Date | Description |
|------|-------------|
| 1990 | Initial development |
| 1999-2001 | Active production use |
| 2024 | Git repository created |

## Related Systems

- **PFC** - More modules (Order Entry, Job Costing)
- **MCS** - Payroll-focused subset for same company
- **BAJ/HBS/IMM** - Simpler payroll systems
