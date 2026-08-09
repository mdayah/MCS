# MCS - Accounting & Payroll Subset (Micro Computer Services)

## Overview

MCS is a lighter accounting-and-payroll build (AC, GL, PR, OP) than the full MPC system, configured in this archive for **McKenry Produce Company**. "MCS" are the initials of the developer's own company, **Micro Computer Services** — the product line this repository is named for — not "McKenry Company Subset," as an earlier note guessed.

**Company:** McKenry Produce Company, 717 Willow Avenue, Knoxville, TN 37917

## System Classification

| Attribute | Value |
|-----------|-------|
| Type | Accounting/Payroll Subset |
| Language | Visual Basic for MS-DOS |
| Era | 1991-1999 |
| Status | Subset of MPC |
| Modules | 4 modules |

## Module Overview

| Module | File | Size | Purpose | Lines |
|--------|------|------|---------|-------|
| **AC** | AC.BAS | 12K | Main Menu/Accounting | 377 |
| **GL** | GL.BAS | 14K | General Ledger | 368 |
| **PR** | PR.BAS | 68K | Payroll | 1,968 |
| **OP** | OP.BAS | 2K | Operations | 66 |

## Directory Structure

```
MCS/
├── AC/                 # Main Accounting Application (140K)
│   ├── AC.BAS         # Main menu (377 lines)
│   ├── GL.BAS         # General Ledger (368 lines)
│   ├── PR.BAS         # Payroll (1,968 lines)
│   ├── OP.BAS         # Operations (66 lines)
│   ├── AC.EXE         # Compiled executable
│   ├── AC.SLB         # Screen library (19K)
│   ├── AC.MAS         # Master configuration (5.6K)
│   └── 386.QLB/LIB    # Processor libraries
│
├── BI/                 # Data Definitions (16K)
│   ├── LIBRARY.REC    # Shared declarations
│   ├── ACCOUNTS.REC   # Account structure
│   ├── EMPLOYEE.REC   # Employee structure
│   └── JOURNAL.REC    # Journal structure
│
├── MPC/                # Company Data (28K)
│   ├── CONFIG.MAS     # Company configuration
│   ├── ACCOUNTS.MAS   # Chart of accounts (99 accounts)
│   ├── EMPLOYEE.MAS   # Employee master
│   └── EMPLOYEE.*     # Employee indexes
│
├── LIB/                # Utility libraries (896K, submodule)
└── VBASIC/             # Development environment (9.8M, submodule)
```

## Key Features

### Main Menu (AC)
- 7 menu categories: GL, PR, AP, AR, IN, FS, OP
- Multi-company support
- Database access
- Error handling

### General Ledger (GL)
- Account management
- Journal entry processing
- Balance calculations
- Debit/credit tracking

### Payroll (PR) - Primary Focus
The payroll module is the most developed (1,968 lines):
- Employee check processing
- Payroll calculations (regular/overtime)
- Tax calculations (FIT, FICA, Medicare)
- Insurance deductions
- Wage and salary management
- Check printing

### Operations (OP)
- Company configuration
- Employee master maintenance
- Configuration file I/O

## Data Structures

### Accounts
```basic
TYPE Accounts
  Type, Number, Name, Credit limit
  Monthly: Beginning Balance, Debit, Credit
  Yearly: Beginning Balance, Debit, Credit
END TYPE
```

### Employee
```basic
TYPE Employee
  Personal: Name, Address, SSN, Phone, Department
  Hire/Release dates
  Type (Salary/Wages)
  Pay: Regular, Overtime, Bonus
  Taxes: FIT, FICA, Medicare, Insurance
  Time tracking: Hours in/out
  Totals: Monthly, Quarterly, Yearly
END TYPE
```

## Company Configuration

```
Code: MPC
Name: McKenry Produce Company
Address: 717 Willow Avenue, Knoxville TN 37917
Payroll: Bi-weekly
Tax ID: 1010101010
```

## Comparison with Related Systems

| Feature | MCS | MPC | PFC |
|---------|-----|-----|-----|
| General Ledger | ✓ | ✓ | ✓ |
| Payroll | ✓ | ✓ | ✓ |
| Accounts Payable | - | ✓ | ✓ |
| Accounts Receivable | - | ✓ | ✓ |
| Financial Statements | - | ✓ | ✓ |
| Inventory | - | ✓ | ✓ |
| Order Entry | - | - | ✓ |

**MCS is a payroll-focused subset** - similar functionality to BAJ, HBS, and IMM.

## Source Code Statistics

- **Total Lines:** 2,779 lines
- **Primary Module:** PR.BAS (1,968 lines - 71% of code)
- **Chart of Accounts:** 99 accounts defined

## Running the Application

```bash
# Start DOSBox-X
./dosbox.sh

# Run application
CD AC
AC.EXE
```

## Version History

| Date | Description |
|------|-------------|
| Sep 1991 | Initial development |
| 1993-1999 | Active maintenance |
| 2024 | Git repository (7 commits) |

## Related Systems

- **MPC** - Full system for same company (adds AP, AR, IN, FS)
- **BAJ, HBS, IMM** - Similar payroll subsets for other companies
- **PFC** - Complete ERP with all modules
