#!/usr/bin/env python3
"""
Re-synchronize .NAM index files with their (scrubbed) master data files.

Each .NAM is a fixed-width index of {name, pointer} entries (layouts from
the *Index TYPEs in the .REC includes). After generate_synthetic.py rewrites
the masters, the indexes still hold the previous (real) names — this script
replaces each entry's name with the name now in the master record its
pointer targets, keeping entry positions unless the file was verifiably
sorted by name, in which case it re-sorts by the same key.

Before touching a file it validates pointer semantics: the existing entry
names must match the .original master's names (>=70%). A <file>.original of
the index itself is kept, and mtimes are restored.

Usage: python3 scripts/sync_companions.py [--dry-run]
"""
import json
import os
import shutil
import sys
from pathlib import Path

BASE = Path(__file__).parent.parent.resolve()
SCHEMAS = json.load(open(BASE / "scripts/schemas.json"))

SZ = {"INTEGER": 2, "LONG": 4, "SINGLE": 4, "DOUBLE": 8, "CURRENCY": 8}


def field_offsets(proj, table):
    """{field name -> (offset, size)} from schemas.json."""
    fields = SCHEMAS[proj]["tables"][table]["fields"]
    out, off = {}, 0
    for f in fields:
        size = f.get("size") or SZ[f["type"].upper()]
        out[f["name"]] = (off, size)
        off += size
    return out, SCHEMAS[proj]["tables"][table]["record_size"]


def last_first_upper(name: bytes, width: int) -> bytes:
    """'Angela Adams' -> 'ADAMS, ANGELA' (FOG customer index style)."""
    parts = name.decode("ascii", "replace").strip().split()
    if len(parts) >= 2:
        s = parts[-1] + ", " + " ".join(parts[:-1])
    else:
        s = " ".join(parts)
    return s.upper().encode("ascii", "replace")[:width].ljust(width)


def identity(name: bytes, width: int) -> bytes:
    return name[:width].ljust(width)


# (project, nam_path, master_table, entry_size,
#  name_off, name_len, ptr_spec, transform)
# ptr_spec: ("int", off)        pointer is a 1-based record number
#           ("str", off, len, key_field)  pointer matches master field value
#           ("copy-sorted",)    file is a full-record copy of the master,
#                               sorted by the master's Nam field
SYNCS = [
    ("MPC", "DATA/EMPLOYEE.NAM", "DATA/EMPLOYEE.MAS", 28, 1, 24,
     ("str", 25, 3, "Num"), identity),
    ("MPC", "DATA/CUSTOMER.NAM", "DATA/CUSTOMER.MAS", 34, 4, 28,
     ("int", 32), identity),
    ("MPC", "DATA/VENDORS.NAM", "DATA/VENDORS.MAS", 30, 0, 28,
     ("int", 28), identity),
    # force=True: the index predates the previous scrub generation, so
    # names can't validate against .original — layout confirmed by hexdump.
    ("FOG", "DATAFILE/CUSTOMER.NAM", "DATAFILE/CUSTOMER.MAS", 28, 0, 26,
     ("int", 26), identity, True),
    ("FOG", "SYSTEM/CUSTOMER.NAM", "SYSTEM/CUSTOMER.MAS", 28, 0, 26,
     ("int", 26), identity, True),
    ("FOG", "ACCOUNTG/AC/EMPLOYEE.NAM", "ACCOUNTG/AC/EMPLOYEE.MAS", 30, 3, 24,
     ("str", 27, 3, "Num"), identity),
    ("FOG", "ACCOUNTG/AC/VENDORS.NAM", "ACCOUNTG/AC/VENDORS.MAS", 30, 0, 28,
     ("int", 28), identity),
    ("FOG", "MASTER/PASSWORD.NAM", "MASTER/PASSWORD.MAS", 27, 4, 19,
     ("copy-sorted",), identity),
    ("PFC", "AP/VENDORS.NAM", "AP/VENDORS.MAS", 30, 0, 28,
     ("int", 28), identity, True),
    ("MCS", "MPC/EMPLOYEE.NAM", "MPC/EMPLOYEE.MAS", 28, 1, 24,
     ("str", 25, 3, "Num"), identity),
]


def read_records(path, rec_size):
    data = path.read_bytes()
    n = len(data) // rec_size
    return [data[i * rec_size:(i + 1) * rec_size] for i in range(n)], \
        data[n * rec_size:]


def master_name(rec, offs):
    off, size = offs["Nam"]
    return rec[off:off + size]


def sync(proj, nam_rel, master_rel, esize, noff, nlen, ptr, transform, *rest):
    force = len(rest) == 2 and rest[0]
    dry = rest[-1]
    nam_path = BASE / proj / nam_rel
    master_path = BASE / proj / master_rel
    orig_master = master_path.with_name(master_path.name + ".original")
    if not nam_path.exists() or nam_path.stat().st_size == 0:
        return f"  skip {proj}/{nam_rel} (missing/empty)"
    offs, rec_size = field_offsets(proj, master_rel)
    masters, _ = read_records(master_path, rec_size)
    originals = (read_records(orig_master, rec_size)[0]
                 if orig_master.exists() else None)
    entries, tail = read_records(nam_path, esize)

    if ptr[0] == "copy-sorted":
        # full-record copy of the master, sorted by name
        noff_m, nlen_m = offs["Nam"]
        live = [r for r in masters if r.strip(b"\x00 ")]
        new = sorted(live, key=lambda r: r[noff_m:noff_m + nlen_m].upper())
        out = b"".join(new)
        note = f"regenerated as sorted copy ({len(new)} records)"
        return write(nam_path, out + tail, note, dry)

    def target(entry):
        if ptr[0] == "int":
            recno = int.from_bytes(entry[ptr[1]:ptr[1] + 2], "little")
            if 1 <= recno <= len(masters):
                return masters[recno - 1], (originals[recno - 1]
                                            if originals else None)
        else:
            _, off, ln, key = ptr
            koff, ksz = offs[key]
            want = entry[off:off + ln].strip()
            for i, m in enumerate(masters):
                if m[koff:koff + ksz].strip() == want:
                    return m, (originals[i] if originals else None)
        return None, None

    # validate pointer semantics against the pre-scrub originals; on re-runs
    # the current entries are already synthetic, so validate the index's own
    # .original (the pre-sync state) instead
    nam_orig = nam_path.with_name(nam_path.name + ".original")
    val_entries = (read_records(nam_orig, esize)[0]
                   if nam_orig.exists() else entries)
    if originals and not force:
        hits = total = 0
        for e in val_entries:
            if not e[noff:noff + nlen].strip(b"\x00 "):
                continue
            _, o = target(e)
            if o is None:
                continue
            total += 1
            want = transform(master_name(o, offs), nlen).strip().upper()
            have = e[noff:noff + nlen].strip().upper()
            if want[:10] == have[:10]:
                hits += 1
        if total and hits / total < 0.7:
            return (f"  !! {proj}/{nam_rel}: only {hits}/{total} entries "
                    f"match originals — layout/pointer wrong, NOT touched")

    # detect whether the file is currently sorted by its leading bytes
    keys = [e[:noff + nlen] for e in entries]
    was_sorted = all(keys[i] <= keys[i + 1] for i in range(len(keys) - 1))

    new_entries = []
    for e in entries:
        m, _ = target(e)
        if m is not None:
            e = (e[:noff] + transform(master_name(m, offs), nlen)
                 + e[noff + nlen:])
        new_entries.append(e)
    if was_sorted:
        new_entries.sort(key=lambda e: e[:noff + nlen])
    out = b"".join(new_entries) + tail
    note = (f"{len(new_entries)} entries re-pointed"
            + (", re-sorted" if was_sorted else ", order preserved"))
    return write(nam_path, out, note, dry)


def write(path, data, note, dry):
    old = path.read_bytes()
    if len(data) != len(old):
        return f"  !! {path}: size would change {len(old)}->{len(data)}, NOT touched"
    if dry:
        return f"  would sync {path}: {note}"
    bak = path.with_name(path.name + ".original")
    if not bak.exists():
        shutil.copy2(path, bak)
    mtime = path.stat().st_mtime
    path.write_bytes(data)
    os.utime(path, (mtime, mtime))
    return f"  OK {path}: {note}"


def main():
    dry = "--dry-run" in sys.argv
    for cfg in SYNCS:
        print(sync(*cfg, dry))


if __name__ == "__main__":
    main()
