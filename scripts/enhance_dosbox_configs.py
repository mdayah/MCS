#!/usr/bin/env python3
"""
Enhance dosbox-x.conf files with entry point information.

Adds echo statements showing:
- Main executable entry points (what you can run)
- How to edit modules in VBASIC
- Available utilities

Usage:
    python3 enhance_dosbox_configs.py [--check]
"""

import os
import sys
from pathlib import Path

# Define entry points for each project
ENTRY_POINTS = {
    'PFC': {
        'description': 'Processed Foods ERP - Complete multi-module system',
        'modules': {
            'AP': {'exe': None, 'edit': 'edit_ap.bat', 'desc': 'Accounts Payable'},
            'AR': {'exe': None, 'edit': 'edit_arp.bat', 'desc': 'Accounts Receivable'},
            'OE': {'exe': None, 'edit': 'edit_oep.bat', 'desc': 'Order Entry'},
            'IN': {'exe': None, 'edit': ['edit_inj.bat', 'edit_pup.bat'], 'desc': 'Inventory (Job/Product)'},
            'GL': {'exe': None, 'edit': 'edit_gl.bat', 'desc': 'General Ledger'},
            'PR': {'exe': None, 'edit': 'edit_pr.bat', 'desc': 'Payroll'},
            'TS': {'exe': None, 'edit': 'edit_ts.bat', 'desc': 'Time Sheet'},
            'FS': {'exe': None, 'edit': 'edit_fs.bat', 'desc': 'Financial Statements'},
        },
    },
    'MPC': {
        'description': 'McKenry Produce - Accounts Receivable & Payroll System',
        'modules': {
            'AR': {'exe': ['ARP.EXE', 'ARD.EXE', 'ARI.EXE', 'ARR.EXE', 'ART.EXE'], 'edit': 'SYSTEM/edit_arp.bat', 'desc': 'AR (multiple modes)'},
            'MAIN': {'exe': 'MPC.EXE', 'edit': 'SYSTEM/edit_mpc.bat', 'desc': 'Main menu system'},
            'UTIL': {'exe': ['IC.EXE', 'TIMECARD.EXE'], 'edit': None, 'desc': 'Utilities'},
        },
    },
    'MCS': {
        'description': 'McKenry Accounting (AR + GL)',
        'modules': {
            'AC': {'exe': None, 'edit': 'AC/edit_ac.bat', 'desc': 'Accounting'},
        },
    },
    'BAJ': {
        'description': 'Basic AR + Payroll',
        'modules': {
            'AC': {'exe': None, 'edit': 'AC/edit_ac.bat', 'desc': 'Accounting'},
        },
    },
    'HBS': {
        'description': 'Home Business System',
        'modules': {
            'AC': {'exe': None, 'edit': 'AC/edit_ac.bat', 'desc': 'Accounting'},
        },
    },
    'IMM': {
        'description': 'Mechanical Business System',
        'modules': {
            'AC': {'exe': None, 'edit': 'AC/edit_ac.bat', 'desc': 'Accounting'},
        },
    },
    'FOG': {
        'description': 'Rental Management System',
        'modules': {
            'MAIN': {'exe': 'SYSTEM/RTO.EXE', 'edit': 'SYSTEM/edit_rto.bat', 'desc': 'Rental System (main)'},
            'OFFICE': {'exe': None, 'edit': 'OFFICE/edit_rts.bat', 'desc': 'Office Management'},
            'AC': {'exe': None, 'edit': 'ACCOUNTG/edit_ac.bat', 'desc': 'Accounting'},
            'UTIL': {'exe': ['SYSTEM/MOP.EXE', 'SYSTEM/PHONE.EXE', 'SYSTEM/LABEL.EXE', 'SYSTEM/REMOTE.EXE'], 'edit': None, 'desc': 'Utilities'},
        },
    },
    'RDL': {
        'description': 'Dental Lab Billing',
        'modules': {
            'AR': {'exe': None, 'edit': 'AR/edit_arp.bat', 'desc': 'Accounts Receivable'},
        },
    },
    'CNB': {
        'description': 'Data File Converter',
        'modules': {
            'ROOT': {'exe': None, 'edit': 'edit_cnb.bat', 'desc': 'File converter'},
        },
    },
}

DOSBOX_HEADER = r"""[autoexec]
MOUNT C .
C:
ECHO.
ECHO ======================================================
ECHO {project} - {desc}
ECHO ======================================================
ECHO.
"""

MODULE_INFO = "ECHO {module}: {desc}\n"
EDIT_INFO = "ECHO   Edit: {edit}\n"
RUN_INFO = "ECHO   Run:  {exe}\n"
BLANK_LINE = "ECHO.\n"
FOOTER = """ECHO.
ECHO ======================================================
ECHO Type a command name to run or edit batch file to modify
ECHO ======================================================
ECHO.
"""


def generate_dosbox_config(project_name, info):
    """Generate dosbox config content."""
    lines = []

    # Header
    lines.append(DOSBOX_HEADER.format(
        project=project_name,
        desc=info['description']
    ))

    # Module info
    for module_name, module_info in sorted(info['modules'].items()):
        lines.append(MODULE_INFO.format(
            module=module_name,
            desc=module_info['desc']
        ))

        if module_info['exe']:
            exes = module_info['exe'] if isinstance(module_info['exe'], list) else [module_info['exe']]
            for exe in exes:
                lines.append(RUN_INFO.format(exe=exe))

        if module_info['edit']:
            edits = module_info['edit'] if isinstance(module_info['edit'], list) else [module_info['edit']]
            for edit in edits:
                lines.append(EDIT_INFO.format(edit=edit))

        lines.append(BLANK_LINE)

    # Footer
    lines.append(FOOTER)

    return ''.join(lines)


def main():
    check_only = '--check' in sys.argv

    base_dir = Path(__file__).parent.parent
    os.chdir(base_dir)

    print("=" * 60)
    print("DOSBox Configuration Generator")
    print("=" * 60)
    print()

    if check_only:
        print("CHECK MODE (no files will be written)\n")

    updated = 0

    for project_name in sorted(ENTRY_POINTS.keys()):
        conf_file = base_dir / project_name / 'dosbox-x.conf'

        if not conf_file.exists():
            print(f"SKIP  {project_name:5} (dosbox-x.conf not found)")
            continue

        content = generate_dosbox_config(project_name, ENTRY_POINTS[project_name])

        if check_only:
            print(f"SHOW  {project_name:5} dosbox-x.conf")
            print("-" * 60)
            print(content)
            print()
        else:
            conf_file.write_text(content)
            print(f"UPDATED {project_name:5} dosbox-x.conf")
            updated += 1

    print()
    print("=" * 60)
    print(f"Updated: {updated} files")
    print("=" * 60)

    if check_only:
        print("\nRun without --check to apply these changes")


if __name__ == '__main__':
    main()
