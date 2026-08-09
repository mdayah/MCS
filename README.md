# Legacy BASIC and Visual Basic for DOS Business Software Collection

## Try the Preserved Demo

The repository includes prebuilt js-dos bundles. From the repository root:

```bash
python3 web/server.py
```

Then open <http://localhost:8080/web/>. The page loads the js-dos runtime from a CDN, so the first launch requires network access.

For the fastest route into the programmer's work, read [CODE_TOUR.md](CODE_TOUR.md) beside the running demo. For a forensic comparison with the neighboring Ptable codebase, read [PTABLE_COMPARISON.md](PTABLE_COMPARISON.md).

## Executive Summary

This is a working archive of **business software built by a single developer beginning in 1988**, first in Microsoft BASIC/QuickBASIC-era tools and later maintained with Visual Basic for MS-DOS. The collection spans multiple industries and contains roughly 46,000 lines in a curated, de-duplicated application count; a raw count is higher because FOG contains parallel SYSTEM, RTO, OFFICE, and accounting trees.

- **PFC**: Complete 12-module ERP for food manufacturing (job costing, recipe management)
- **MPC**: Full accounting system for McKenry Produce Company, a Knoxville **poultry** business (523 customer slots / 410 populated; 99 employee slots / 51 populated in the preserved masters)
- **FOG**: **Rent-to-own** home-furnishings management system with multi-store support (3,479 populated customer-master records)
- **RDL**: Dental laboratory AR system with a 120-record procedure catalog, 60 populated customer records, and 11,638 invoice records
- **Payroll Suite**: 4 per-client deployments of one shared payroll engine (2,600-3,000 lines each)

The applications use fixed-length random-access records, companion indexes, multi-module designs, and extensive reporting. Some documentation calls this "ISAM"; the source inspected here primarily shows native BASIC `OPEN ... FOR RANDOM` access plus application-managed index files, not use of the VBDOS Professional ISAM API. The archive includes historical source and executables alongside later portability, demo, and anonymization work; it is not a byte-for-byte untouched original.

---

## Historical Context: Why This Matters

### The Era (1988-2006)

When this work began in 1988, the DOS BASIC compilers available were **QuickBASIC 4.5** and **Turbo BASIC**. VB DOS wouldn't be released until September 1992. Enterprise accounting software was dominated by:
- **COBOL** on mainframes (IBM, Burroughs)
- **SAP R/2** (mainframe-based, then SAP R/3 launches 1992)
- **dBASE/Clipper** (Clipper dominated xBase market 1985-1992, valued at $190 million when sold to Computer Associates)
- **Accpac** (accounting module package, $495+ per version)
- **QuickBooks** (launched 1992 for DOS, consumer/SMB focus)

**What made this unusual:** A single developer chose QuickBASIC as the platform starting in 1988 and built multiple, complete business systems that competed functionally with commercial software costing 10-100x more. The codebase shows:

- **1988-1992**: Built PFC and MPC in QuickBASIC—professional, compiled, but unconventional for serious ERP work (most used COBOL or Clipper).
- **1992+**: Later projects migrated to VB DOS, taking advantage of the new platform as it became available.
- **1990-2001**: Actively maintained production systems handling real payroll, AR/AP, GL, and manufacturing logic.
- **Multiple verticals**: Not a toolkit—completely separate systems built for food manufacturing, poultry wholesaling, rent-to-own retail, and dental-lab billing.
- **A product lineage in source**: The payroll system was copied, adapted, and maintained across client deployments. The versions share the same routine structure but diverge in details, making the archive unusually good evidence of one programmer evolving a product family before modern version-control workflows.

### The Challenge of Preservation

These systems held real customer and employee data. The browser demos use deterministic synthetic replacements, including the packed patient-name fields in RDL invoices, while preserving record layouts and business behavior.

---

## Project Timeline & Breakdown

### Phase 1: Foundation (1988-1990)

**PFC - Processed Foods Corporation** (Started July 1988)
- Most ambitious: Complete 12-module ERP
- Modules: GL, AP, AR, Financial Statements, Payroll, Inventory, Order Entry, Time Sheets, Job Costing, Recipe Management
- ~6,800 lines of application code
- **Significance**: Demonstrates manufacturing-specific logic (job costing, recipes) in a DOS system

**MPC - McKenry Produce Company** (Started 1990, active 1990-2001)
- Full accounting system: GL, AP, AR (6 sub-modules), Financial Statements, Payroll, Inventory
- ~10,900 lines of application code
- **Preserved data metrics**: 523 customer slots (410 populated) and 99 employee slots (51 populated)
- **Significance**: Most mature AR implementation—6 dedicated sub-modules (ARP, ARS, ARD, ART, ARR, ARI) show deep specialization

### Phase 2: Payroll Product Line (1991-1995)

Starting September 1991, the payroll module was extracted and deployed as a standalone product to a series of bookkeeping and payroll clients. This strategy demonstrates product-based thinking—one core module, customized deployments:

**MCS - Accounting Subset** (1991-1999)
- Base: Payroll + General Ledger
- 2,779 lines
- Context: a lighter accounting-and-payroll build configured for McKenry; "MCS" is the developer's own company, Micro Computer Services (not "McKenry Subset")

**BAJ** (1991-1999)
- Standard payroll system: Payroll + Menu + Configuration
- 2,561 lines
- First standalone payroll deployment

**HBS** (1991-1995)
- A per-client payroll install; the specific client is not identified in this public archive
- ~2,700 lines
- Added journal records for manual GL entries

**IMM** (1991-1995)
- A per-client payroll install; the specific client is not identified in this public archive
- ~2,970 lines (largest payroll module)
- Refined after the earlier payroll installs

**Key insight**: All four use nearly identical payroll code (~2,000 lines), but the architecture allowed menu systems (AC.BAS) and configuration (OP.BAS) to be customized per client. This is sophisticated product management for a solo developer.

### Phase 3: New Verticals (Mid-1990s)

**FOG - Rent-to-Own System** (Mid-1990s)
- Completely different domain from accounting
- ~10,300 lines across 36 source files, 98 MB
- Modules: Rent-to-Own (RTO.EXE, 362K), Office Master (RTS.EXE, 282K), Accounting integration
- Features: Rental-purchase agreements, serial-numbered inventory, multi-store support, depreciation
- **Significance**: Shows the developer could pivot to entirely new business domains

**RDL - Dental Lab Billing** (1990s)
- Industry-specific AR for dental procedures
- Procedure catalog (120 fixed records), invoicing
- 60 populated customer records, 120 procedure slots, and 11,638 invoice records in the preserved files
- **Significance**: Demonstrates ability to understand specialized vertical requirements

### Phase 4: Modernization (1999+)

**CNB - File Converter Utility** (1999)
- Single-purpose utility: converts payroll data between bank file formats
- ~600 lines
- Context: As banking formats evolved, created a bridge tool
- **Significance**: Final piece before preservation in 2024

---

## Technical Achievements

### 1. Fixed-Record Data Architecture

The applications use an **ISAM-like fixed-record design** implemented with BASIC random files and companion indexes:
- Fixed-size record structures
- Random access by record number
- Index files for fast lookups
- Multiple data files per application (EMPLOYEE.MAS, CUSTOMER.MAS, VENDORS.MAS, etc.)

This was cutting-edge for DOS and shows understanding of database design beyond simple sequential files.

### 2. Multi-Module Integration

Rather than monolithic applications, systems were built as cooperating modules:
- **PFC**: 12 distinct modules that share common data structures
- **MPC**: 9 modules with sophisticated AR decomposition
- Modules communicate through standardized record types (EMPLOYEE.REC, CUSTOMER.REC, etc.)

This modular approach kept large programs within DOS memory limits, separated operational areas into independently compiled executables, and made client-specific deployments practical. Reuse was mostly copy-and-evolve rather than a shared source dependency.

### 3. Sophisticated Business Logic

**Payroll** (~2,000 lines per system):
- W-2 generation
- Tax calculations: Federal Income Tax (FIT), Social Security (FICA), Medicare
- Check printing and void processing
- Employee master maintenance and sorting
- Multiple report types (register, monthly, W-2s)

**Accounts Receivable** (6 modules in MPC):
- Customer master management (ARI)
- Sales processing (ARS)
- Detail transaction entry (ARD)
- Transaction history (ART)
- Reporting (ARR)
- Primary operations (ARP)

**Manufacturing/Job Costing** (PFC unique):
- Job tracking across recipe/formula manufacturing
- Cost allocation
- Bill of materials

### 4. Text-Mode UI with Screen Libraries

Systems used **NOVA screen editor** to define .SLB (screen library) files:
- Menu-driven interfaces
- Data entry forms with validation
- Report layout definitions
- Saved in binary format, interpreted at runtime

This is equivalent to modern Form Designers, but in DOS.

---

## Architecture Overview



## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                    VISUAL BASIC DOS SOFTWARE COLLECTION                         │
└─────────────────────────────────────────────────────────────────────────────────┘

═══════════════════════════════════════════════════════════════════════════════════
                              SHARED COMPONENTS
═══════════════════════════════════════════════════════════════════════════════════

    ┌─────────────┐      ┌─────────────┐      ┌─────────────┐
    │   VBASIC    │      │     LIB     │      │    NOVA     │
    │  ─────────  │      │  ─────────  │      │  ─────────  │
    │ VB DOS IDE  │      │  PBClone    │      │ Screen/SLB  │
    │ BC.EXE      │      │  600+ funcs │      │   Editor    │
    │ LINK.EXE    │      │  286/386    │      │             │
    │ CV.EXE      │      │  libs       │      │             │
    └──────┬──────┘      └──────┬──────┘      └──────┬──────┘
           │                    │                    │
           └────────────────────┼────────────────────┘
                                │
                    ┌───────────┴───────────┐
                    │  Used by ALL projects │
                    └───────────────────────┘

═══════════════════════════════════════════════════════════════════════════════════
                           FULL ERP SYSTEMS
═══════════════════════════════════════════════════════════════════════════════════

┌─────────────────────────────────────┐    ┌─────────────────────────────────────┐
│  PFC (Processed Foods Corp)         │    │  MPC (McKenry Produce Co)           │
│  ═══════════════════════════════    │    │  ═══════════════════════════════    │
│  MOST COMPLETE - ALL FEATURES       │    │  MOST MATURE AR (6 sub-modules)     │
│                                     │    │                                     │
│  ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐    │    │  ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐    │
│  │ GL  │ │ AP  │ │ AR  │ │ FS  │    │    │  │ GL  │ │ AP  │ │ AR* │ │ FS  │    │
│  └─────┘ └─────┘ └─────┘ └─────┘    │    │  └─────┘ └─────┘ └─────┘ └─────┘    │
│  ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐    │    │  ┌─────┐ ┌─────┐                    │
│  │ PR  │ │ IN  │ │ OE  │ │ TS  │    │    │  │ PR  │ │ IN  │  *AR has 6 parts:  │
│  └─────┘ └─────┘ └─────┘ └─────┘    │    │  └─────┘ └─────┘   ARP,ARS,ARD,     │
│  ┌─────────────┐ ┌─────────────┐    │    │                    ART,ARR,ARI      │
│  │ Job Costing │ │   Recipes   │    │    │                                     │
│  └─────────────┘ └─────────────┘    │    │                                     │
│         ▲              ▲            │    │                                     │
│         └──── UNIQUE ──┘            │    │                                     │
└─────────────────────────────────────┘    └─────────────────────────────────────┘

═══════════════════════════════════════════════════════════════════════════════════
                    PAYROLL SUBSETS (NEARLY IDENTICAL CODE)
═══════════════════════════════════════════════════════════════════════════════════

        ┌───────────────────────────────────────────────────────────┐
        │              IDENTICAL PAYROLL MODULE (PR.BAS)            │
        │         ~2,000-2,100 lines - SAME STRUCTURE               │
        │                                                           │
        │  • EmpEdit, EmpSort, EmpShow, EmpWind                     │
        │  • ChkFind, ChkPost, ChkPrnt, ChkVoid, ChkShow            │
        │  • Tax calcs: FIT, FICA, Medicare                         │
        │  • Reports: mprRegister, mprW2s, mprMonth, etc.           │
        └───────────────────────────────────────────────────────────┘
                    │           │           │           │
        ┌───────────┴───┐ ┌─────┴─────┐ ┌───┴───────┐ ┌─┴───────────┐
        │               │ │           │ │           │ │             │
        ▼               ▼ ▼           ▼ ▼           ▼ ▼             ▼
┌───────────────┐ ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
│      MCS      │ │      BAJ      │ │      HBS      │ │      IMM      │
│  ───────────  │ │  ───────────  │ │  ───────────  │ │  ───────────  │
│  McKenry      │ │               │ │ Home Business │ │  Mechanical   │
│               │ │               │ │               │ │               │
│ ┌────┐ ┌────┐ │ │ ┌────┐ ┌────┐ │ │ ┌────┐ ┌────┐ │ │ ┌────┐ ┌────┐ │
│ │ AC │ │ PR │ │ │ │ AC │ │ PR │ │ │ │ AC │ │ PR │ │ │ │ AC │ │ PR │ │
│ └────┘ └────┘ │ │ └────┘ └────┘ │ │ └────┘ └────┘ │ │ └────┘ └────┘ │
│ ┌────┐ ┌────┐ │ │ ┌────┐        │ │ ┌────┐        │ │ ┌────┐        │
│ │ GL │ │ OP │ │ │ │ OP │        │ │ │ OP │        │ │ │ OP │        │
│ └────┘ └────┘ │ │ └────┘        │ │ └────┘        │ │ └────┘        │
│    ▲          │ │               │ │               │ │    ▲          │
│    │          │ │               │ │               │ │    │          │
│ +GL module    │ │  Standard     │ │  Standard     │ │ Largest PR    │
│               │ │               │ │               │ │  (71K)        │
└───────────────┘ └───────────────┘ └───────────────┘ └───────────────┘
    2,779 loc         2,561 loc        ~2,700 loc        ~2,970 loc

═══════════════════════════════════════════════════════════════════════════════════
                        DISTINCT PROGRAMS (DIFFERENT DOMAINS)
═══════════════════════════════════════════════════════════════════════════════════

┌─────────────────────────┐  ┌─────────────────────────┐  ┌─────────────────────────┐
│          FOG            │  │          RDL            │  │          CNB            │
│  ═══════════════════    │  │  ═══════════════════    │  │  ═══════════════════    │
│  RENT-TO-OWN            │  │  DENTAL LAB BILLING     │  │  FILE CONVERTER         │
│                         │  │                         │  │                         │
│  ┌───────────────────┐  │  │  ┌───────────────────┐  │  │  ┌───────────────────┐  │
│  │ SYSTEM (Rent-Own) │  │  │  │ AR (Dental)       │  │  │  │ CNB.BAS           │  │
│  │ • RTO.EXE (362K)  │  │  │  │ • Customers       │  │  │  │ • Payroll format  │  │
│  │ • Agreements      │  │  │  │ • Procedures      │  │  │  │ • Bank formats    │  │
│  │ • Inventory       │  │  │  │ • Invoices        │  │  │  │ • Conversion      │  │
│  │ • Receipts        │  │  │  │ • Payments        │  │  │  └───────────────────┘  │
│  └───────────────────┘  │  │  └───────────────────┘  │  │                         │
│  ┌───────────────────┐  │  │                         │  │  Single utility         │
│  │ OFFICE (Master)   │  │  │  Industry-specific      │  │  226 lines              │
│  │ • RTS.EXE (282K)  │  │  │  1,400 lines            │  │                         │
│  └───────────────────┘  │  │                         │  │  Converts:              │
│  ┌───────────────────┐  │  │  Features:              │  │  • PAYROLL.TXT          │
│  │ ACCOUNTG          │  │  │  • Procedure catalog    │  │  • 1STNAT.TXT           │
│  │ • GL, AP, PR      │  │  │  • Dental invoicing     │  │  • LEAGUE.TXT           │
│  └───────────────────┘  │  │  • 60 customers         │  │         ↓               │
│                         │  │  • 11,638 invoices      │  │  Standardized .OUT      │
│  ~10,300 main lines     │  │                         │  │                         │
│  36 source files        │  │                         │  │                         │
└─────────────────────────┘  └─────────────────────────┘  └─────────────────────────┘

═══════════════════════════════════════════════════════════════════════════════════
                              MODULE LEGEND
═══════════════════════════════════════════════════════════════════════════════════

  GL = General Ledger       AP = Accounts Payable      AR = Accounts Receivable
  FS = Financial Stmts      PR = Payroll               IN = Inventory
  OE = Order Entry          TS = Time Sheets           OP = Operations/Config
  AC = Main Menu/Accounting

═══════════════════════════════════════════════════════════════════════════════════
                            RELATED FILES MATRIX
═══════════════════════════════════════════════════════════════════════════════════

                    MCS     BAJ     HBS     IMM
                   ─────   ─────   ─────   ─────
  AC.BAS            ≈       ≈       ≈       ≈      (~375 lines, menu system)
  PR.BAS            ≈       ≈       ≈       ≈      (~2,000 lines, payroll)
  OP.BAS            ≈       ≈       ≈       ≈      (~160 lines, config)
  EMPLOYEE.REC      ≈       ≈       ≈       ≈      (same structure)
  ACCOUNTS.REC      ≈       ≈       ≈       ≈      (same structure)
  LIBRARY.REC       ≈       ≈       ≈       ≈      (same declarations)

  ≈ = Same product lineage with installation-specific differences
```

---

## Project Classification

### Distinct Programs (Different Application Domains)

| Project | Type | Purpose | Unique Features |
|---------|------|---------|-----------------|
| **FOG** | Rent-to-Own | Rent-to-own home furnishings | Rental-purchase agreements, serial-numbered inventory, multi-store |
| **RDL** | Dental Billing | Rogers' Dental Laboratory, Athens TN | Procedure catalog, dental-specific invoicing |
| **CNB** | Utility | Payroll file converter | Banking data interchange |

### Full ERP/Accounting Systems

| Project | Company | Modules | Status |
|---------|---------|---------|--------|
| **PFC** | Processed Foods Corp | 12 modules | Most complete (all features) |
| **MPC** | McKenry Produce Co (poultry) | 9 modules | Full system (mature AR) |

### Payroll-Focused Subsets

| Project | Based On | Modules | Differences |
|---------|----------|---------|-------------|
| **MCS** | MPC | AC, GL, PR, OP | Micro Computer Services build; adds GL |
| **BAJ** | Base payroll | AC, PR, OP | Standard payroll |
| **HBS** | Base payroll | AC, PR, OP | Journal records |
| **IMM** | Base payroll | AC, PR, OP | Largest PR module |

### Shared Components (Not Applications)

| Project | Type | Purpose |
|---------|------|---------|
| **VBASIC** | Development Environment | Microsoft VB for MS-DOS IDE |
| **LIB** | Utility Library | PBClone 600+ functions |
| **NOVA** | Editor Tool | Screen library (.SLB) editor |
| **VBTOO** | Component Library | MicroHelp VBTools 3 (Windows) |

---

## Feature Comparison Matrix

### Accounting Modules

| Module | PFC | MPC | MCS | BAJ | HBS | IMM | FOG | RDL |
|--------|-----|-----|-----|-----|-----|-----|-----|-----|
| General Ledger | ✓ | ✓ | ✓ | - | - | - | ✓ | - |
| Accounts Payable | ✓ | ✓ | - | - | - | - | ✓ | - |
| Accounts Receivable | ✓ | ✓ | - | - | - | - | - | ✓ |
| Financial Statements | ✓ | ✓ | - | - | - | - | ✓ | - |
| Payroll | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | - |
| Inventory | ✓ | ✓ | - | - | - | - | - | - |
| Order Entry | ✓ | - | - | - | - | - | - | - |
| Job Costing | ✓ | - | - | - | - | - | - | - |
| Recipe Management | ✓ | - | - | - | - | - | - | - |

### Industry-Specific Features

| Feature | FOG | RDL | PFC |
|---------|-----|-----|-----|
| Rental Agreements | ✓ | - | - |
| Equipment Tracking | ✓ | - | - |
| Multi-Store Support | ✓ | - | - |
| Depreciation | ✓ | - | - |
| Procedure Catalog | - | ✓ | - |
| Dental Invoicing | - | ✓ | - |
| Recipe/Formula Mgmt | - | - | ✓ |
| Manufacturing | - | - | ✓ |

---

## Code Relationship Analysis

### Most Complete System: **PFC**

PFC (Processed Foods Corporation) has the most modules:
- All accounting modules (GL, AP, AR, FS)
- Operations (OE, IN, PR, TS)
- Manufacturing (Job Costing, Recipe Management)
- **~6,800 lines of application code**

### Most Mature AR Module: **MPC**

MPC has the most developed Accounts Receivable with 6 sub-modules:
- ARP.BAS - Primary operations
- ARS.BAS - Sales processing
- ARD.BAS - Detail transactions
- ART.BAS - Transaction history
- ARR.BAS - Reporting
- ARI.BAS - Initialization

### Identical/Near-Identical Code

The following projects share nearly identical payroll code:

```
BAJ/AC/PR.BAS  ≈  HBS/AC/PR.BAS  ≈  IMM/AC/PR.BAS  ≈  MCS/AC/PR.BAS
(~2,000-2,100 lines each)
```

**Key similarities:**
- Same subroutine structure (EmpEdit, EmpSort, ChkPost, ChkPrnt, etc.)
- Same data structures (Employee, EmpIndex, EmpCheck)
- Same tax calculations (FIT, FICA, Medicare)
- Same reporting functions (mprRegister, mprW2s, etc.)

**Minor differences:**
- IMM has largest PR.BAS (71K vs 68K)
- MCS adds GL.BAS module
- HBS includes Journal records in BI/

### Shared Submodule Dependencies

All projects use these git submodules:

```
Project/
├── VBASIC/  → github.com/mdayah/VBASIC
├── LIB/     → github.com/mdayah/LIB
└── NOVA/    → github.com/mdayah/NOVA (most projects)
```

---

## Directory Structure Summary

```
dad/
├── PFC/          # Complete ERP (Processed Foods)
│   ├── AP/       # Accounts Payable
│   ├── AR/       # Accounts Receivable
│   ├── GL/       # General Ledger
│   ├── FS/       # Financial Statements
│   ├── OE/       # Order Entry
│   ├── IN/       # Inventory
│   ├── PR/       # Payroll
│   ├── TS/       # Time Sheets
│   ├── BI/       # Data Definitions
│   ├── LIB/      # Libraries
│   ├── VBASIC/   # Development Environment
│   └── NOVA/     # Screen Editor
│
├── MPC/          # Full Accounting (McKenry Produce)
│   ├── SYSTEM/   # Main executables
│   ├── DATA/     # Business data
│   ├── INCLUDE/  # Record definitions
│   ├── LIB/      # Libraries
│   └── VBASIC/   # Development Environment
│
├── MCS/          # Accounting Subset (McKenry)
├── BAJ/          # Payroll System
├── HBS/          # Payroll System (client install)
├── IMM/          # Payroll System (client install)
│
├── FOG/          # Rent-to-Own (Distinct)
│   ├── SYSTEM/   # Rent-to-own operations
│   ├── OFFICE/   # Master data
│   ├── ACCOUNTG/ # Accounting integration
│   ├── DATAFILE/ # Business data
│   └── INCLUDE/  # Record definitions
│
├── RDL/          # Dental Billing (Distinct)
│   ├── AR/       # Accounts Receivable
│   └── VBASIC/   # Development Environment
│
├── CNB/          # File Converter (Utility)
│   ├── floppy/   # Sample data
│   ├── LIB/      # Libraries
│   ├── NOVA/     # Screen Editor
│   └── VBASIC/   # Development Environment
│
├── VBASIC/       # Shared: VB for MS-DOS IDE
├── LIB/          # Shared: PBClone Utilities
├── NOVA/         # Shared: Screen Editor
└── VBTOO/        # Archived: Windows VB Components
```

---

## Source Code Statistics

| Project | Type | Lines | Modules | Era |
|---------|------|-------|---------|-----|
| PFC | Full ERP | ~6,800 | 12 | 1988-1999 |
| MPC | Full Acct | ~10,900 | 9 | 1990-2001 |
| FOG | Rent-to-Own | ~10,300 | 36 files | Mid-1990s |
| MCS | Subset | ~2,800 | 4 | 1991-1999 |
| BAJ | Payroll | ~2,600 | 3 | 1991-1999 |
| HBS | Payroll | ~2,700 | 3 | 1991-1995 |
| IMM | Payroll | ~3,000 | 3 | 1991-1995 |
| RDL | Dental AR | 1,400 | 2 | 1990s |
| CNB | Converter | 226 | 1 | 1999 |

---

## Technology Stack

| Component | Technology |
|-----------|------------|
| Language | Visual Basic for MS-DOS v1.00 |
| Compiler | BC.EXE (Microsoft BASIC Compiler) |
| Linker | LINK.EXE (Microsoft Linker) |
| Debugger | CV.EXE (CodeView) |
| Database | Fixed-length random files with companion indexes |
| UI | Text-mode with screen libraries |
| Libraries | PBClone (600+ utility functions) |
| Screen Editor | NOVA (NSEDIT.EXE) |

## Running on Modern Systems

For the browser demo, use the command at the top of this README. For local DOS development on the currently documented WSL2/Windows setup, use DOSBox-X:

```bash
# From a project directory
./dosbox.sh
```

## Version History

| Era | Development |
|-----|-------------|
| 1988-1992 | Initial PFC development |
| 1991-1995 | BAJ, HBS, IMM payroll systems |
| 1991-1999 | MCS accounting subset |
| 1990-2001 | MPC full system |
| Mid-1990s | FOG rent-to-own system |
| 1999 | CNB converter utility |
| 2024 | Git repository creation |

## Archived Components

These applications were developed for specific companies:
- Processed Foods Corporation, a Knoxville food manufacturer (PFC)
- McKenry Produce Company, a Knoxville poultry business (MPC, MCS)
- Rogers' Dental Laboratory, Athens TN (RDL)
- A rent-to-own home-furnishings store in Greeneville (FOG)
- Payroll-bureau clients, not individually identified here (BAJ, HBS, IMM)

The VBASIC development environment is:
- Microsoft Visual Basic for MS-DOS Professional Edition
- Copyright (c) Microsoft Corporation 1982-1992

The LIB utilities are:
- PBClone Library (Third-party)

The VBTOO components are:
- MicroHelp VBTools 3 (Commercial, 1993)
