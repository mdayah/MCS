#!/usr/bin/env python3
"""
Generate .jsdos bundles for the Legacy VB DOS applications.

Each bundle is a ZIP file containing:
- .jsdos/dosbox.conf - DOSBox configuration with autoexec
- The application directory and its dependencies (VBASIC IDE + LIB included
  for source-launched apps)

Usage:
    python3 web/create_bundles.py           # Create all bundles
    python3 web/create_bundles.py MPC       # Create specific bundle
    python3 web/create_bundles.py --list    # List available apps
"""

import os
import sys
import zipfile
from pathlib import Path

BASE_DIR = Path(__file__).parent.parent.resolve()
BUNDLES_DIR = Path(__file__).parent / "bundles"

# Each app declares the autoexec lines that run after `mount c .` and `c:`.
# Most apps launch through VBDOS.EXE /RUN against a .MAK so that any source
# edits in this repo (e.g., the relative-INCLUDE paths in PFC/BAJ/HBS) take
# effect at run time. NOVA and the IDE itself ship as plain EXE launches.
APPS = {
    "MPC": {
        # Launch the compiled EXE (like FOG/RDL) instead of interpreting the
        # source through the VBDOS IDE — the IDE /RUN path is CPU-bound parsing
        # that hangs for minutes in the browser worker. MPC.EXE bakes in the
        # absolute screen-library path "C:\MPC.SLB", so stage a copy there or
        # MhOpenScreenLib fails silently and no background paints.
        "name": "MPC - McKenry Produce",
        "dirs": ["MPC"],
        "autoexec": [
            "cd MPC\\SYSTEM",
            "copy MPC.SLB C:\\ > NUL",
            "MPC.EXE",
        ],
    },
    "MPC_AR": {
        # The AR side of MPC was never launched from the main menu (its
        # dispatch is commented out in AC.BAS) — in production the six AR
        # programs (ARP/ARS/ARD/ART/ARR/ARI) ran standalone, per the
        # developer's own dosbox-x.conf notes. ARP is the primary one.
        # Compiled ARP.EXE, run from MPC\SYSTEM (its data and MPC.SLB are here).
        "name": "MPC - Receivables (ARP)",
        "dirs": ["MPC"],
        "autoexec": [
            "cd MPC\\SYSTEM",
            "copy MPC.SLB C:\\ > NUL",
            "ARP.EXE",
        ],
    },
    "FOG": {
        # RTODEMO.EXE is RTO recompiled with PassWord() bypassed (a single
        # `PassWord = 1: EXIT FUNCTION`), built with his own RUN.BAT flags so
        # visitors aren't stopped by a login plus a password on every one of
        # RTO's 78 gated actions. The authentic RTO.EXE ships unchanged.
        # (Compiled, not /RUN: the 6-module program overflows the VBDOS IDE
        # with "Out of memory" regardless of memsize — EMS/XMS don't help the
        # IDE heap. This is also exactly how he ran it: BC + LINK, per RUN.BAT.)
        "name": "FOG - Equipment Rental",
        "dirs": ["FOG"],
        "autoexec": [
            "cd FOG\\SYSTEM",
            "RTODEMO.EXE",
        ],
    },
    "FOG_AC": {
        # FOG/ACCOUNTG is the evolved MPC accounting engine (GL/AP/FS/OP/PR
        # all module-for-module descendants, each larger than MPC's copy) —
        # SUB-level analysis 2026-07: FOG = rental vertical + full accounting.
        "name": "FOG - Accounting (MPC engine, evolved)",
        "dirs": ["FOG"],
        "autoexec": [
            # Compiled EXE (AC.SLB opened relative, source unmodified) — the
            # 6-module program overflows the VBDOS IDE like FOG/RTO does.
            "cd FOG\\ACCOUNTG\\AC",
            "AC.EXE",
        ],
    },
    "FOG_OFFICE": {
        # The rent-to-own OFFICE program (RTS.EXE) over the VBDOS ISAM inventory.
        # OFDATA is excluded wholesale (real PII); the office bundle pulls in ONLY
        # the scrubbed Inventor.MAS via "files". That file is NOT in git — CI
        # regenerates it from the private repo with scripts/isam_scrub.js before
        # this runs. PROISAM is the ISAM engine, loaded before RTS.
        "name": "FOG - Rent-to-Own Office (RTS)",
        "dirs": ["FOG/OFFICE", "FOG/MASTER"],
        "files": [
            "FOG/OFDATA/INVENTOR.MAS",
            "VBASIC/SYSTEM/PROISAM.EXE",
            "VBASIC/SYSTEM/PROISAMD.EXE",
            "VBASIC/SYSTEM/ISAMIO.EXE",
        ],
        "autoexec": [
            "cd FOG\\OFFICE",
            "C:\\VBASIC\\SYSTEM\\PROISAM.EXE",
            "RTS.EXE",
        ],
    },
    "RDL": {
        # The compiled application runs without redistributing the VBDOS IDE.
        "name": "RDL - Dental Lab Billing",
        "dirs": ["RDL"],
        "autoexec": [
            "cd RDL\\AR",
            "AR.EXE",
        ],
    },
    "PFC": {
        # Compiled ARP.EXE. Its screen libs are baked relative ("..\AR\ARS.SLB",
        # "..\IN\INS.SLB"), which resolve when run from PFC\AR.
        "name": "PFC - Processed Foods Corp",
        "dirs": ["PFC"],
        "autoexec": [
            "cd PFC\\AR",
            "ARP.EXE",
        ],
    },
    "BAJ": {
        # Compiled AC.EXE bakes in the absolute "C:\Payroll\AC\AC.SLB"; stage
        # the screen library there so the background paints.
        "name": "BAJ - Payroll System",
        "dirs": ["BAJ"],
        "autoexec": [
            "md C:\\Payroll",
            "md C:\\Payroll\\AC",
            "copy BAJ\\AC\\AC.SLB C:\\Payroll\\AC\\ > NUL",
            "cd BAJ\\AC",
            "AC.EXE",
        ],
    },
    "HBS": {
        # Compiled AC.EXE with a relative "..\AC\AC.SLB" — resolves from HBS\AC.
        "name": "HBS - Home Business",
        "dirs": ["HBS"],
        "autoexec": [
            "cd HBS\\AC",
            "AC.EXE",
        ],
    },
    "IMM": {
        # Compiled AC.EXE with a relative "..\AC\AC.SLB" — resolves from IMM\AC.
        "name": "IMM - Mechanical",
        "dirs": ["IMM"],
        "autoexec": [
            "cd IMM\\AC",
            "AC.EXE",
        ],
    },
    "MCS": {
        # MCS ships no compiled EXE, only source — so it stays on the VBDOS /RUN
        # path. It is a subset of the payroll engine already runnable via BAJ.
        "name": "MCS - McKenry Subset",
        "dirs": ["MCS", "VBASIC", "LIB"],
        "autoexec": [
            "cd MCS\\AC",
            "\\VBASIC\\SYSTEM\\VBDOS.EXE /L \\LIB\\386.QLB /RUN AC.MAK",
        ],
    },
    "CNB": {
        # Compiled CNB.EXE with a relative "CNB.SLB" — resolves from CNB.
        "name": "CNB - File Converter",
        "dirs": ["CNB"],
        "autoexec": [
            "cd CNB",
            "CNB.EXE",
        ],
    },
    "VBASIC": {
        # The IDE itself — no source to /RUN, just launch it.
        "name": "VB DOS IDE",
        "dirs": ["VBASIC", "LIB"],
        "autoexec": [
            "cd VBASIC\\SYSTEM",
            "VBDOS.EXE",
        ],
    },
    "NOVA": {
        # Screen library editor — small standalone utility, no .BAS to /RUN.
        "name": "NOVA Screen Editor",
        "dirs": ["NOVA"],
        "autoexec": [
            "cd NOVA",
            "NSEDIT.EXE",
        ],
    },
}

# Directory names whose entire subtree is excluded — checked against every
# ancestor of each file path, not just the leaf name.
# OFDATA holds FOG's page-structured "office" data files — format not yet
# decoded, still contains REAL PII. Excluded until a scrubber exists for it.
EXCLUDE_DIRS = {".git", "__pycache__", "_REAL_BACKUP", "OFDATA"}

# Filename / suffix patterns excluded individually.
EXCLUDE_NAMES = {
    ".gitmodules",
    ".gitignore",
    "README.md",
    "dosbox.sh",
    "dosbox-x.conf",
}
EXCLUDE_SUFFIXES = {".pyc", ".png", ".original", ".CPY", ".cpy"}


def should_exclude(path: Path) -> bool:
    if any(part in EXCLUDE_DIRS for part in path.parts):
        return True
    if path.name.startswith("WARNING_REAL_DATA"):
        return True
    if path.name in EXCLUDE_NAMES:
        return True
    if path.suffix in EXCLUDE_SUFFIXES:
        return True
    return False


def generate_dosbox_conf(app: dict) -> str:
    # DOS env vars VBDOS uses to find its INCLUDE/LIB/HELP. The bundled
    # VBDOS.INI is the developer's saved settings with paths from his
    # original drive letters; even though we patched it, env vars are the
    # documented override and are read on every launch regardless of INI
    # state. Set them only when the bundle ships VBASIC.
    env_setup = ""
    if any(d == "VBASIC" for d in app.get("dirs", [])):
        env_setup = (
            "set INCLUDE=C:\\VBASIC\\INCLUDE\n"
            "set LIB=C:\\VBASIC\\LIBRARY\n"
            "set PATH=C:\\VBASIC\\SYSTEM;%PATH%\n"
        )
    autoexec_body = env_setup + "\n".join(app["autoexec"])
    return f"""[sdl]
fullscreen=false
output=surface

[dosbox]
machine=svga_s3
memsize=16

[dos]
# VBDOS's interpreter swaps module code to EMS; without it the larger
# multi-module .MAKs (FOG's RTO is ~7,900 lines across 6 modules) die
# with "Out of memory" at load. UMB frees conventional memory too.
xms=true
ems=true
umb=true

[cpu]
core=auto
cputype=auto
cycles=max

[mixer]
rate=44100

[midi]
mpu401=intelligent

[parallel]
# Each port gets its OWN capture file — pointing all three at one file makes
# LPT1 hold it and the app's OPEN "LPT2:" fail with "Device unavailable"
# (FOG/RTO opens LPT2 on startup). The printer view merges LPT1..3.
parallel1=file append:LPT1.TXT timeout:500
parallel2=file append:LPT2.TXT timeout:500
parallel3=file append:LPT3.TXT timeout:500

[serial]
serial1=dummy
serial2=dummy

[autoexec]
@echo off
mount c .
rem A: floppy — FOG's inter-store transfer (Merchandise ▸ Transfer Out) reads
rem and writes A:\\Transfer.Mas; without a floppy it fails "Device unavailable".
mount a . -t floppy > NUL
c:
rem VBDOS's SHELL (INT 21h EXEC) can't launch COMMAND.COM from the Z: drive
rem in this emulator (report/sort steps like GL's `SHELL "AC.COM ..."` failed
rem with "Path not found"); a copy on C: with COMSPEC pointed at it works.
copy Z:\\COMMAND.COM C:\\ > NUL
set COMSPEC=C:\\COMMAND.COM
echo R > \\READY.RDY
{autoexec_body}
"""


def create_bundle(app_id: str, app: dict) -> Path:
    bundle_path = BUNDLES_DIR / f"{app_id.lower()}.jsdos"

    print(f"Creating {bundle_path.name}...")

    # Each project (MPC, FOG, etc.) has nested git submodules pointing at the
    # same top-level VBASIC and LIB repos we're bundling separately. Walking
    # them would duplicate every VBASIC/LIB file under MPC/VBASIC/, FOG/LIB/,
    # etc., bloating the bundle and breaking emscripten extraction (libzip's
    # mkdir isn't recursive across the duplicated paths).
    submodule_names_to_skip = {"VBASIC", "LIB", "NOVA"}

    # Collect the file list first; emit directory entries before files so
    # emscripten's libzip extractor (which doesn't mkdir-p) can land each
    # file under an already-created parent.
    entries: list[Path] = []
    for dir_name in app["dirs"]:
        src_dir = BASE_DIR / dir_name
        if not src_dir.exists():
            print(f"  Warning: {dir_name} directory not found, skipping")
            continue
        added_here = 0
        for file_path in src_dir.rglob("*"):
            if not file_path.is_file() or should_exclude(file_path):
                continue
            rel = file_path.relative_to(BASE_DIR)
            if any(part in submodule_names_to_skip for part in rel.parts[1:-1]):
                continue
            entries.append(file_path)
            added_here += 1
        print(f"  Added {added_here} files from {dir_name}/")

    # Explicit single-file includes, bypassing should_exclude() — used to admit
    # one regenerated file (the scrubbed OFDATA/INVENTOR.MAS) past the OFDATA
    # exclusion, and to pull the PROISAM ISAM engine out of VBASIC.
    for rel_file in app.get("files", []):
        fp = BASE_DIR / rel_file
        if not fp.is_file():
            print(f"  Warning: file {rel_file} not found, skipping")
            continue
        # Never let a real (unscrubbed) office inventory into a published bundle.
        if fp.name.upper() == "INVENTOR.MAS" and "OFDATA" in fp.parts:
            if b"SYNTHETIC DEMO REMARK" not in fp.read_bytes():
                raise SystemExit(
                    f"REFUSING: {rel_file} is not scrubbed — run scripts/isam_scrub.js first")
        entries.append(fp)
        print(f"  Added file {rel_file}")

    # Every unique parent directory of every file we're shipping.
    dirs_needed: set[str] = set()
    for f in entries:
        rel = f.relative_to(BASE_DIR)
        for i in range(1, len(rel.parts)):
            dirs_needed.add("/".join(rel.parts[:i]) + "/")

    with zipfile.ZipFile(bundle_path, 'w', zipfile.ZIP_DEFLATED) as zf:
        zf.writestr(".jsdos/dosbox.conf", generate_dosbox_conf(app))

        # Sort so parents come before children (shorter paths first).
        for d in sorted(dirs_needed):
            zi = zipfile.ZipInfo(d)
            zi.external_attr = 0o40755 << 16  # directory bit + 0755
            zf.writestr(zi, b"")

        for f in entries:
            zf.write(f, f.relative_to(BASE_DIR))

    size_mb = bundle_path.stat().st_size / (1024 * 1024)
    print(f"  Created: {bundle_path.name} ({size_mb:.1f} MB)")

    return bundle_path


def main():
    if len(sys.argv) > 1:
        arg = sys.argv[1]

        if arg == "--list":
            print("Available applications:")
            for app_id, app in APPS.items():
                print(f"  {app_id:8} - {app['name']}")
            return

        if arg == "--help" or arg == "-h":
            print(__doc__)
            return

        app_id = arg.upper()
        if app_id not in APPS:
            print(f"Error: Unknown app '{arg}'")
            print("Use --list to see available apps")
            sys.exit(1)

        apps_to_build = {app_id: APPS[app_id]}
    else:
        apps_to_build = APPS

    BUNDLES_DIR.mkdir(exist_ok=True)

    print(f"Building {len(apps_to_build)} bundle(s)...\n")

    for app_id, app in apps_to_build.items():
        create_bundle(app_id, app)
        print()

    print("Done!")
    print(f"Bundles saved to: {BUNDLES_DIR}")


if __name__ == "__main__":
    main()
