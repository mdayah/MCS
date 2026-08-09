# VBASIC Edit Batch File Management

## Overview

This directory contains tools to manage VBASIC editor integration batch files and DOSBox configuration files for the multi-project development environment.

## Scripts

### `create_edit_batches.py`

Generates `edit_*.bat` files that launch the VBASIC IDE to edit specific project modules.

**Pattern:**
```batch
\VBASIC\SYSTEM\VBDOS.EXE /l \LIB\386.QLB [MAKEFILE].MAK
```

**Usage:**
```bash
# Preview what would be created
python3 create_edit_batches.py --check

# Create all missing batch files
python3 create_edit_batches.py

# Check/create for a specific project
python3 create_edit_batches.py --project PFC
```

**Coverage:**
- **19 modules** across 9 projects
- **16 files created** on first run
- **3 files already existed** (AP, AR-PFC, IMM/AC)

### `enhance_dosbox_configs.py`

Updates `dosbox-x.conf` files in each project with helpful entry point documentation.

When you launch a project in DOSBox, you see:
- Project name and description
- Available modules with what they do
- How to run executables (if they exist)
- How to edit in VBASIC

**Example output in DOSBox:**
```
======================================================
MPC - McKenry Produce - Accounts Receivable & Payroll
======================================================

AR: AR (multiple modes)
   Run:  ARP.EXE
   Run:  ARD.EXE
   Edit: SYSTEM/edit_arp.bat

MAIN: Main menu system
   Run:  MPC.EXE
   Edit: SYSTEM/edit_mpc.bat

UTIL: Utilities
   Run:  IC.EXE
   Run:  TIMECARD.EXE
```

**Usage:**
```bash
# Preview changes
python3 enhance_dosbox_configs.py --check

# Apply to all projects
python3 enhance_dosbox_configs.py
```

## Coverage by Project

| Project | Modules | Status | Notes |
|---------|---------|--------|-------|
| **PFC** | AP, AR, OE, IN (2), GL, PR, TS, FS | Complete | 9 edit batch files |
| **MPC** | SYSTEM | Complete | 1 edit batch + 5 runtime EXEs |
| **MCS** | AC | Complete | 1 edit batch file |
| **BAJ** | AC | Complete | 1 edit batch file |
| **HBS** | AC | Complete | 1 edit batch file (pre-existing) |
| **IMM** | AC | Complete | 1 edit batch file (pre-existing) |
| **FOG** | SYSTEM, OFFICE, ACCOUNTG | Complete | 3 edit batch files + 1 main EXE + 4 utilities |
| **RDL** | AR | Complete | 1 edit batch file |
| **CNB** | ROOT | Complete | 1 edit batch file |

## Batch File Locations

### Created Files (16 new)
```
BAJ/AC/edit_ac.bat                    # Accounting module
CNB/edit_cnb.bat                      # Root converter
FOG/SYSTEM/edit_rto.bat               # Rental system
FOG/OFFICE/edit_rts.bat               # Office management
FOG/ACCOUNTG/edit_ac.bat              # Accounting
HBS/AC/edit_ac.bat                    # Accounting
MCS/AC/edit_ac.bat                    # Accounting
MPC/SYSTEM/edit_mpc.bat               # Main system
PFC/OE/edit_oep.bat                   # Order entry
PFC/IN/edit_inj.bat                   # Inventory jobs
PFC/IN/edit_pup.bat                   # Product master
PFC/GL/edit_gl.bat                    # General ledger
PFC/PR/edit_pr.bat                    # Payroll
PFC/TS/edit_ts.bat                    # Time sheets
PFC/FS/edit_fs.bat                    # Financial statements
RDL/AR/edit_arp.bat                   # Accounts receivable
```

### Pre-existing Files (3)
```
PFC/AP/edit_ap.bat                    # Accounts payable
PFC/AR/edit_arp.bat                   # Accounts receivable
IMM/AC/edit_ac.bat                    # Accounting
```

## DOSBox Configs

All 9 `dosbox-x.conf` files have been enhanced with:
- Clear project identification
- Module descriptions
- Entry point information (executables to run, batch files to edit)
- Navigation hints

**Projects updated:**
- BAJ, CNB, FOG, HBS, IMM, MCS, MPC, PFC, RDL

## Running from DOSBox

When you launch a project with DOSBox:

1. **See available options** - Helpful ECHO statements display what you can do
2. **Run a program** - Type the .EXE filename directly:
   ```
   C:\> MPC.EXE
   C:\> RTO.EXE
   ```
3. **Edit in VBASIC** - Run the appropriate batch file:
   ```
   C:\> SYSTEM\edit_mpc.bat
   C:\> SYSTEM\edit_rto.bat
   C:\> AC\edit_ac.bat
   ```

## PII Protection (.gitignore)

A comprehensive `.gitignore` has been created to protect sensitive data:

**Protected file types:**
- `*.MAS` - Binary data files containing employee/customer information with PII
  - SSNs, addresses, phone numbers
  - Bank account numbers
  - Driver's license numbers
  - Date of birth records
  - Login credentials

**Not ignored (safely committed):**
- `*.BAS` - BASIC source code
- `*.MAK` - Makefiles
- `*.BAT` / `*.CMD` - Batch files
- `*.SCR` - Screen definitions
- `*.SLB` / `*.QLB` - Compiled libraries
- Documentation (*.md, *.json)
- Python scripts

**Additional protection:**
- Python virtual environments (`.venv/`)
- Build artifacts (compiled .EXE, .COM, .OBJ, .LIB)
- IDE configuration (.vscode, .idea)
- OS/temp files

## Verification

To verify everything is in place:

```bash
# Count created batch files
find . -name "edit_*.bat" | wc -l      # Should show 19

# Check that PII is protected in gitignore
grep "*.MAS" .gitignore

# Test a batch file
cat BAJ/AC/edit_ac.bat                 # Should contain VBASIC command

# Test a dosbox config
head -20 MPC/dosbox-x.conf             # Should show helpful info
```

## Future Maintenance

If you add new modules or projects:

1. **Update `PROJECTS` dict** in `create_edit_batches.py`:
   ```python
   'NEWPROJECT': {
       'MODNAME': {'makefile': 'MOD.MAK', 'source': 'MOD.BAS'},
   }
   ```

2. **Update `ENTRY_POINTS` dict** in `enhance_dosbox_configs.py`:
   ```python
   'NEWPROJECT': {
       'description': 'What the project does',
       'modules': {
           'MODNAME': {'exe': 'MOD.EXE', 'edit': 'edit_mod.bat', 'desc': 'What it does'},
       },
   }
   ```

3. **Re-run the generators:**
   ```bash
   python3 scripts/create_edit_batches.py
   python3 scripts/enhance_dosbox_configs.py
   ```

## Notes

- All batch files use the **dos path syntax** (`\VBASIC\SYSTEM\VBDOS.EXE`) which works in DOSBox
- Makefiles determine what gets loaded/compiled
- The `/l` flag loads the 386.QLB screen library
- DOSBox configs mount the current directory as `C:` drive
