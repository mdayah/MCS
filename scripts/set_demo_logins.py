#!/usr/bin/env python3
"""
Make FOG's login usable after scrubbing.

FOG's PassWord routine encodes each typed character before comparing it to the
stored value:  stored[i] == (typed[i] + OFFSET[i]) mod 256, with
OFFSET = (34, 70, 81, 63) (from RTO.BAS / RTS.BAS). The PII scrub replaces the
stored 4 bytes with a random password, which then decodes to un-typeable
extended characters — nobody can log in.

Worse, FindPas() looks the password up by BINARY SEARCH, so PASSWORD.MAS must be
sorted ascending by the encoded Pas field. Scrubbing randomized the passwords
and destroyed that order, breaking every login.

This restores a working login by (1) storing the ENCODING of a simple plaintext
in one record so typing that plaintext authenticates as a level-0 user, then
(2) re-sorting the whole file by Pas so the binary search finds it. Idempotent;
preserves mtime; keeps a .original.

Plaintext is DEMO_PASSWORD below — keep it in sync with the web page hint and
the auto-login keystrokes in web/index.html.
"""
import os
import shutil
from pathlib import Path

BASE = Path(__file__).parent.parent.resolve()
OFFSETS = (34, 70, 81, 63)
DEMO_PASSWORD = "demo"          # what a visitor types
REC_SIZE = 27                    # Password record: Pas4 Nam19 Ini3 Lvl1

# Every FOG password file that RTO/RTS authenticate against.
TARGETS = ["FOG/MASTER/PASSWORD.MAS"]


def encode(plain: str) -> bytes:
    return bytes((ord(c) + off) % 256 for c, off in zip(plain, OFFSETS))


def patch(rel: str):
    p = BASE / rel
    if not p.exists() or p.stat().st_size < REC_SIZE:
        print(f"  skip {rel} (missing/short)")
        return
    raw = p.read_bytes()
    n = len(raw) // REC_SIZE
    recs = [bytearray(raw[i * REC_SIZE:(i + 1) * REC_SIZE]) for i in range(n)]
    enc = encode(DEMO_PASSWORD)

    # set the demo password on the first active (non-empty-name) record,
    # and raise its level to '9' so PassWord("3".."8") permission checks on
    # every menu action pass (FOG re-prompts per action, gated by level)
    demo_idx = next((i for i, r in enumerate(recs)
                     if bytes(r[4:23]).strip(b"\x00 ")), 0)
    demo_name = bytes(recs[demo_idx][4:23]).decode("latin1").strip()
    recs[demo_idx][0:4] = enc
    recs[demo_idx][26:27] = b"9"

    # binary search in FindPas() requires ascending order by the Pas field
    recs.sort(key=lambda r: bytes(r[0:4]))
    out = b"".join(bytes(r) for r in recs)

    if out == raw:
        print(f"  {rel}: already set")
        return
    bak = p.with_name(p.name + ".original")
    if not bak.exists():
        shutil.copy2(p, bak)
    mtime = p.stat().st_mtime
    p.write_bytes(out)
    os.utime(p, (mtime, mtime))
    print(f"  {rel}: {n} records sorted by password; "
          f"'{demo_name}' -> type '{DEMO_PASSWORD}'")


def main():
    print(f"Setting demo login '{DEMO_PASSWORD}' (encodes to "
          f"{encode(DEMO_PASSWORD).hex()}):")
    for t in TARGETS:
        patch(t)


if __name__ == "__main__":
    main()
