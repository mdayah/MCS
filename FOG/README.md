# FOG - Rent-to-Own & Inventory Management System

## Overview

FOG is a **comprehensive rent-to-own and inventory management system** with integrated accounting capabilities. Unlike the other accounting-focused projects in this repository, FOG is designed specifically for **rent-to-own home-furnishings businesses** managing rental-purchase agreements, inventory tracking, and customer accounts. Its books in this archive are for a family store, "Home Rentals," of Greeneville, Tennessee.

**This is a distinct program** - not a subset of the accounting systems.

## System Classification

| Attribute | Value |
|-----------|-------|
| Type | Rent-to-Own Management System |
| Language | Microsoft BASIC (compiled) |
| Era | Mid-1990s |
| Status | Complete standalone system |
| Total Size | 98 MB |
| Source Files | 36 BASIC files (~10,314 lines) |

## Directory Structure

```
FOG/
├── SYSTEM/             # Main rental transaction modules (12 EXE)
├── OFFICE/             # Master data & office functions (7 programs)
├── ACCOUNTG/           # Accounting subsystem
│   ├── AC/            # Core Accounting (AP, GL, PR, FS)
│   ├── IN/            # Inventory/Purchasing
│   └── BI/            # Business definitions
├── DATAFILE/           # Active business data (1.3MB)
├── MASTER/             # Reference data tables
├── OFDATA/             # Office data archives (40MB)
└── INCLUDE/            # Record definitions (23 files)
```

## Core Modules

### SYSTEM/ - Rental Operations (12 Executables)

| Module | Size | Purpose |
|--------|------|---------|
| **RTO.EXE** | 362K | Main Rental Transaction Operations |
| **MOP.EXE** | 118K | Manager Operations |
| **INV-PUR.EXE** | 106K | Inventory Purchase |
| **INV-DEP.EXE** | 63K | Inventory Depreciation |
| **RECODE.EXE** | 55K | Item Recoding |
| **BOR-FIX.EXE** | 54K | Beginning of Revenue Fix |
| **INV-INX.EXE** | 43K | Inventory Indexing |
| **ACT-NEW.EXE** | 33K | Activity Statistics |
| **PHONE.EXE** | 33K | Contact Management |
| **LABEL.EXE** | 50K | Label Printing |
| **ZIP-INX.EXE** | 27K | Zip Code Indexing |
| **REMOTE.EXE** | 40K | Remote Operations |

### OFFICE/ - Master Data (6 Programs)

| Module | Size | Purpose |
|--------|------|---------|
| **RTS.EXE** | 282K | Rental Transaction System (main UI) |
| **REINDEX.EXE** | 97K | Database Reindexing |
| **INDEX.EXE** | 95K | Index Utilities |

### ACCOUNTG/ - Accounting Integration

| Module | Size | Purpose |
|--------|------|---------|
| **AC.EXE** | 263K | Accounting (GL, AP, Payroll) |
| **STORES.EXE** | 58K | Store/Location Accounting |
| **PU.EXE** | 93K | Purchase Order Processing |

## Key Features

### Rental Management
- Agreement creation, modification, termination
- Multiple items per agreement (up to 4 + loaners)
- Flexible payment frequency (weekly/monthly)
- Automatic rate calculations
- Fair Market Value (FMV) tracking
- Late fees, trip charges, waivers

### Inventory Tracking
- Serial number tracking
- Equipment status management:
  - Idle, In-service, On-rent
  - Awaiting service, Loaner
- Distributor/model management
- Depreciation calculation
- Transfer between locations
- Activity history per item

### Payment Processing
- Receipt generation & posting
- Check writing & reconciliation
- Bad check handling
- Discount & tax management
- Payment application to agreements

### Financial Reporting
- General Ledger with account balances
- Trial balance reports
- Income statements
- Balance sheets
- Journal entry review

### Customer Management
- Customer master file
- Employment/income verification
- Multiple contacts
- Credit scoring data

### Administrative
- Password-protected access with levels
- Multi-store/center support
- Backup/restore procedures
- Report printing

## Data Model

### Master Files
| File | Size | Purpose |
|------|------|---------|
| CUSTOMER.MAS | 1.4M | Customer records |
| INVENTOR.MAS | 327K | Active inventory |
| RECEIPTS.MAS | 1.3M | Payment history |
| AGREEMNT.MAS | 232K | Rental agreements |
| CODES.MAS | 77K | Item classifications |
| PRICES.MAS | 428K | Pricing tables |
| VENDORS.MAS | 507K | Vendor master |
| ACCOUNTS.MAS | 236K | GL accounts |

### Record Definitions (INCLUDE/)
| File | Purpose |
|------|---------|
| CUSTOMER.REC | Customer structure (415 bytes) |
| INVENTOR.REC | Inventory items (293 bytes) |
| AGREEMNT.REC | Rental agreements (415 bytes) |
| RECEIPTS.REC | Payment receipts (150 bytes) |
| CODES.REC | Item classification codes |
| PRICES.REC | Pricing & warranty |
| RATES.REC | Rental rate definitions |
| ACTIVITY.REC | Activity tracking |
| TRANSACT.REC | Transaction logging |

## Source Code Statistics

| Category | Count |
|----------|-------|
| Source Files (.BAS) | 36 |
| Lines of Code | ~10,314 |
| Compiled Executables | 18 |
| Shared Libraries (.SLB) | 7 |
| Record Definitions | 16 |
| Total Data Files | 170 |

## Technical Stack

- **Language:** Microsoft BASIC (compiled)
- **Compiler:** BC.exe, Link.exe
- **Libraries:** VBDRT10E.lib, VBDCL10E.lib, 386.LIB
- **UI:** MhOpenScreenLib (menu-driven)
- **Database:** Random-access binary files with indexes
- **File Handles:** 35+ simultaneous files

## Comparison with Other Systems

| Feature | FOG | PFC | MPC |
|---------|-----|-----|-----|
| **Primary Purpose** | Rent-to-Own | Food Mfg | Poultry |
| General Ledger | ✓ | ✓ | ✓ |
| Accounts Payable | ✓ | ✓ | ✓ |
| Payroll | ✓ | ✓ | ✓ |
| **Rental Agreements** | ✓ | - | - |
| **Equipment Tracking** | ✓ | - | - |
| **Multi-Store** | ✓ | - | - |
| **Depreciation** | ✓ | - | - |
| Order Entry | - | ✓ | - |
| Manufacturing | - | ✓ | - |

**FOG is a completely different application** focused on rent-to-own business operations rather than manufacturing or distribution.

## Running the Application

```bash
# In DOSBox
CD OFFICE
RTS.EXE    # Main rental transaction system

# Or for accounting
CD ACCOUNTG\AC
AC.EXE
```

## Target Industry

- Rent-to-own home-furnishings stores (furniture, appliances, electronics)
- Multi-location rent-to-own operations
