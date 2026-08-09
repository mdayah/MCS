#!/usr/bin/env python3
"""
Generate VBASIC editor batch files for DOS development projects.

This script creates edit_*.bat files that launch the VBASIC IDE with the
appropriate makefile or source file for editing.

Usage:
    python3 create_edit_batches.py [--check] [--project PROJECT_NAME]

Options:
    --check        Only show what would be created, don't write files
    --project NAME Only process specific project (PFC, MPC, FOG, etc.)
"""

import os
import sys
from pathlib import Path

# Define all projects and their modules requiring VBASIC integration
PROJECTS = {
    'PFC': {
        'AP': {'makefile': 'AP.MAK', 'source': 'AP.BAS'},
        'AR': {'makefile': 'ARP.MAK', 'source': 'ARP.BAS'},
        'OE': {'makefile': 'OEP.MAK', 'source': 'OEP.BAS'},
        'IN': [
            {'makefile': 'INJ.MAK', 'source': 'INJ.BAS'},
            {'makefile': 'PUP.MAK', 'source': 'PUP.BAS'},
        ],
        'GL': {'makefile': 'GL.MAK', 'source': 'GL.BAS'},
        'PR': {'makefile': 'PR.MAK', 'source': 'PR.BAS'},
        'TS': {'makefile': 'TS.MAK', 'source': 'TS.BAS'},
        'FS': {'makefile': 'FS.MAK', 'source': 'FS.BAS'},
    },
    'MPC': {
        'SYSTEM': {'makefile': 'MPC.MAK', 'source': 'MPC.BAS'},
    },
    'MCS': {
        'AC': {'makefile': 'AC.MAK', 'source': 'AC.BAS'},
    },
    'BAJ': {
        'AC': {'makefile': 'AC.MAK', 'source': 'AC.BAS'},
    },
    'HBS': {
        'AC': {'makefile': 'AC.MAK', 'source': 'AC.BAS'},
    },
    'IMM': {
        'AC': {'makefile': 'AC.MAK', 'source': 'AC.BAS'},
    },
    'FOG': {
        'SYSTEM': {'makefile': 'RTO.MAK', 'source': 'RTO.BAS'},
        'OFFICE': {'makefile': 'RTS.MAK', 'source': 'RTS.BAS'},
        'ACCOUNTG': {'makefile': 'AC.MAK', 'source': 'AC.BAS'},
    },
    'RDL': {
        'AR': {'makefile': 'ARP.MAK', 'source': 'ARP.BAS'},
    },
    'CNB': {
        'ROOT': {'makefile': 'CNB.MAK', 'source': 'CNB.BAS'},
    },
}

VBASIC_TEMPLATE = r'\VBASIC\SYSTEM\VBDOS.EXE /l \LIB\386.QLB {target}'


def get_batch_filename(module_path, makefile_name):
    """Generate batch filename based on makefile name."""
    # Extract base name from makefile (e.g., ARP.MAK -> arp)
    base_name = makefile_name.split('.')[0].lower()

    if module_path.name == module_path.parent.name:
        # Root-level module like CNB
        return 'edit.bat'
    else:
        return f'edit_{base_name}.bat'


def create_edit_batch(project_root, module_path, target_name):
    """Create an edit batch file for a module."""
    batch_file = module_path / get_batch_filename(module_path, target_name)
    content = VBASIC_TEMPLATE.format(target=target_name) + '\n'

    return {
        'path': batch_file,
        'content': content,
        'exists': batch_file.exists(),
    }


def process_project(project_name, modules, project_root, check_only=False):
    """Process all modules in a project."""
    results = []

    for module_name, module_config in modules.items():
        if isinstance(module_config, list):
            # Multiple files in this module (e.g., INJ and PUP in IN)
            for config in module_config:
                if module_name == 'ROOT':
                    module_path = project_root
                else:
                    module_path = project_root / module_name

                target = config['makefile']
                batch_info = create_edit_batch(project_root, module_path, target)

                results.append({
                    'project': project_name,
                    'module': module_name,
                    'file': batch_info['path'],
                    'content': batch_info['content'],
                    'exists': batch_info['exists'],
                })
        else:
            # Single file per module
            if module_name == 'ROOT':
                module_path = project_root
            else:
                module_path = project_root / module_name

            target = module_config['makefile']
            batch_info = create_edit_batch(project_root, module_path, target)

            results.append({
                'project': project_name,
                'module': module_name,
                'file': batch_info['path'],
                'content': batch_info['content'],
                'exists': batch_info['exists'],
            })

    # Write or report
    for result in results:
        if result['exists']:
            status = 'EXISTS (skip)'
        else:
            status = 'CREATE'
            if not check_only:
                result['file'].parent.mkdir(parents=True, exist_ok=True)
                result['file'].write_text(result['content'])

        print(f"{status:15} {result['project']:4} {result['module']:10} {result['file'].name}")

    return results


def main():
    check_only = '--check' in sys.argv
    project_filter = None

    if '--project' in sys.argv:
        idx = sys.argv.index('--project')
        if idx + 1 < len(sys.argv):
            project_filter = sys.argv[idx + 1].upper()

    base_dir = Path(__file__).parent.parent
    os.chdir(base_dir)

    print("=" * 60)
    print("VBASIC Edit Batch File Generator")
    print("=" * 60)
    print()

    if check_only:
        print("CHECK MODE (no files will be written)\n")

    total = 0
    created = 0

    for project_name in sorted(PROJECTS.keys()):
        if project_filter and project_name != project_filter:
            continue

        project_root = base_dir / project_name
        if not project_root.exists():
            print(f"SKIP        {project_name} (directory not found)")
            continue

        print(f"\n[{project_name}]")
        results = process_project(
            project_name,
            PROJECTS[project_name],
            project_root,
            check_only
        )

        total += len(results)
        created += sum(1 for r in results if not r['exists'])

    print()
    print("=" * 60)
    print(f"Total files: {total}")
    print(f"To create:   {created}")
    print(f"Already exist: {total - created}")
    print("=" * 60)

    if check_only and created > 0:
        print("\nRun without --check to create these files")


if __name__ == '__main__':
    main()
