# Portfolio: Enterprise Software (1988-2006)

A single developer's comprehensive business software suite built in Visual Basic for MS-DOS.

---

## Full ERP Systems

### PFC - Processed Foods Corporation
**Era:** 1988-1999 | **Status:** Most complete
**Size:** ~6,800 lines | **Modules:** 12

**Features:**
- General Ledger, Accounts Payable, Accounts Receivable
- Financial Statements, Payroll, Inventory
- **Manufacturing-specific**: Job Costing, Recipe/Formula Management
- Order Entry, Time Sheets

**Significance:** Earliest major system (started July 1988); demonstrates manufacturing ERP sophistication in VB DOS.

**Try it:** [Run in Browser](web/index.html) | Browse source: [PFC/](PFC/)

---

### MPC - McKenry Produce Company
**Era:** 1990-2001 | **Status:** Production system
**Size:** ~10,900 lines | **Modules:** 9
**Preserved data:** 523 customer slots / 410 populated; 99 employee slots / 51 populated

**Features:**
- General Ledger, Accounts Payable
- **Accounts Receivable** (6 specialized sub-modules):
  - ARP: Primary operations
  - ARS: Sales processing
  - ARD: Detail transactions
  - ART: Transaction history
  - ARR: Reporting
  - ARI: Initialization
- Financial Statements, Payroll, Inventory

**Significance:** Longest-lived system (actively used 1990-2001); most mature AR implementation with 6 dedicated modules.

**Try it:** [Run in Browser](web/index.html) | Browse source: [MPC/](MPC/)

---

## Specialized Vertical Systems

### FOG - Rent-to-Own Management
**Era:** Mid-1990s | **Status:** Complete
**Size:** ~10,300 lines, 36 source files, 98 MB

**Features:**
- **Rent-to-Own System** (RTO.EXE, 362K): Rental-purchase agreements, serial-numbered inventory, receipts
- **Office/Master Data** (RTS.EXE, 282K): Multi-store support
- Accounting Integration: GL, AP, Payroll
- Multi-store depreciation tracking

**Significance:** Completely different domain from accounting; shows capability to build specialized systems for new verticals.

**Try it:** [Run in Browser](web/index.html) | Browse source: [FOG/](FOG/)

---

### RDL - Dental Laboratory AR
**Era:** 1990s | **Status:** Complete
**Size:** 1,400 source lines across 2 files

**Features:**
- Procedure Catalog (120 fixed records)
- Customer Management
- Invoicing and Payments
- Reports

**Preserved data:** 120 customer slots / 60 populated; 11,638 invoice records

**Significance:** Industry-specific application showing domain expertise in healthcare billing.

**Try it:** [Run in Browser](web/index.html) | Browse source: [RDL/](RDL/)

---

## Payroll Product Suite (1991-1995)

Four deployments of a refined, reusable payroll core:

### MCS - Accounting Subset (Micro Computer Services)
**Size:** 2,779 lines
**Modules:** Payroll + General Ledger + Menu + Config
**Context:** a lighter accounting-and-payroll build configured for McKenry; "MCS" is the developer's own company, Micro Computer Services.

[Browse source](MCS/)

---

### BAJ - Standard Payroll System
**Size:** 2,561 lines
**Modules:** Payroll + Menu + Config
**Timeline:** 1991-1999
**Significance:** First standalone payroll product extraction.

[Browse source](BAJ/) | [Run in Browser](web/index.html)

---

### HBS
**Size:** ~2,700 lines
**Modules:** Payroll + Menu + Config + Journal Records
**Timeline:** 1991-1995
**Focus:** A per-client payroll install; the client is not identified in this public archive.

[Browse source](HBS/) | [Run in Browser](web/index.html)

---

### IMM
**Size:** ~2,970 lines (largest payroll module)
**Modules:** Payroll + Menu + Config
**Timeline:** 1991-1995
**Focus:** A per-client payroll install; the client is not identified in this public archive.

[Browse source](IMM/) | [Run in Browser](web/index.html)

---

### Payroll Core Features (all four systems)
**~2,000-line family of related implementations:**
- Employee master maintenance and sorting
- Check finding, posting, printing, void processing
- Tax calculations: FIT (Federal Income Tax), FICA (Social Security), Medicare
- W-2 generation
- Reports: Register, Monthly, W-2 forms

**Design insight:** A proven payroll implementation was copied and adapted with customized menus and configuration for different installations. The shared routine structure and local divergence make the family useful code-archeology material.

---

## Utilities

### CNB - File Format Converter
**Era:** 1999
**Size:** ~600 lines
**Purpose:** Convert payroll data between bank file formats (PAYROLL.TXT, 1STNAT.TXT, LEAGUE.TXT → standardized .OUT format)

**Significance:** Late-stage system adapting to evolving banking standards; final project before preservation.

[Browse source](CNB/) | [Run in Browser](web/index.html)

---

## Development Tools (Included)

### VBASIC - Visual Basic for MS-DOS IDE
**Microsoft Visual Basic for MS-DOS Professional Edition v1.0**

Included in archive for reference and compilation. Contains:
- BC.EXE (BASIC Compiler)
- LINK.EXE (Linker)
- CV.EXE (CodeView Debugger)
- 600+ library functions (PBClone)
- Graphics and charting toolkits

[Browse source](VBASIC/)

---

### NOVA - Screen Library Editor
Tool for designing .SLB (screen library) files used by all applications for UI layout and forms.

[Browse source](NOVA/)

---

### LIB - PBClone Utility Library
600+ reusable functions for:
- String manipulation
- File I/O
- Fixed-record and index-file operations
- Screen drawing
- Graphics

Shared across all projects.

[Browse source](LIB/)

---

## Statistics

| Metric | Value |
|--------|-------|
| **Total Application Code** | ~46,000 lines |
| **Number of Complete Systems** | 9 (2 full ERP, 1 rent-to-own, 1 dental, 4 payroll, 1 utility) |
| **Different Business Verticals** | 5 (food manufacturing, poultry wholesaling, rent-to-own retail, dental lab, payroll services) |
| **Development Span** | 1988-2006 (18 years) |
| **Preserved customer masters** | MPC: 410 populated records; FOG: 3,479; RDL: 60 |
| **Single Developer** | Yes, entire portfolio |
| **Platform** | Visual Basic for MS-DOS Professional Edition v1.0 |
| **Database Design** | Fixed-length random files with application-managed companion indexes |
| **Demo data** | Deterministic synthetic identities in the original fixed-record layouts |

---

## How to Use This Archive

### Option 1: Run in Browser
Run `python3 web/server.py`, then open <http://localhost:8080/web/>. Opening the HTML directly is insufficient because the emulator fetches bundle assets over HTTP and loads js-dos from a CDN.

### Option 2: Run Locally with DOSBox-X
```bash
cd /home/lucent/dad/MPC
./dosbox.sh
```

See [SETUP.md](SETUP.md) for installation instructions.

### Option 3: Browse Source Code
All `.BAS` files are human-readable BASIC source. Start with:
- `MPC/SYSTEM/MPC.BAS` - Main menu
- `BAJ/AC/PR.BAS` - Payroll module (~2,000 lines, typical structure)
- `PFC/AR/ARP.BAS` - Accounts Receivable (core accounting logic)

---

## Historical Context

Built during the microcomputer ERP transition (1988-2006):
- **1988**: PFC begins in the pre-VBDOS Microsoft BASIC/QuickBASIC era
- **1990s**: Payroll module refined and deployed to 4 clients
- **Mid-1990s**: Ventured into new vertical (rent-to-own retail)
- **1999-2001**: Active maintenance period, final systems built
- **2024**: Preserved with source code and anonymized production data

See [HISTORY.md](HISTORY.md) for analysis of the competitive landscape at the time.

---

## Further Reading

- **[README.md](README.md)** - Complete technical overview and architecture
- **[CODE_TOUR.md](CODE_TOUR.md)** - The most revealing routines and what they show
- **[HISTORY.md](HISTORY.md)** - Historical context and comparative analysis
- **[AGENTS.md](AGENTS.md)** - Data anonymization techniques used for preservation
- **[SETUP.md](SETUP.md)** - Running systems on modern computers
- **[Web Interface](web/index.html)** - Browser-based DOS emulator
