#!/usr/bin/env node
// Regenerate a PII-free FOG office Inventor.MAS from the real ISAM database.
//
// The office masters are VBDOS ISAM (paged), so they can't be scrubbed by the
// flat-record python generator. Instead we run his own idiom (FOG/OFFICE/
// INVSCRUB.BAS: SETINDEX/RETRIEVE/UPDATE/MOVENEXT) inside a real DOSBox-X via
// the `emulators` package + PROISAM, then read the rewritten ISAM back out.
// Rmk (remarks) and Ini (user initials) are the only PII in the record; every
// business field (models, serials, dates, depreciation) is preserved, and the
// ISAM indexes stay valid because those two fields are not indexed.
//
// Usage: node scripts/isam_scrub.js <real-inventor.mas> [out.mas=in place]
// Exits non-zero unless the scrub completed and the output is verifiably scrubbed.
const fs = require("fs");
const path = require("path");
const JSZip = require("jszip");
global.self = global;
global.ImageData = class { constructor(d, w, h) { this.data = d; this.width = w; this.height = h; } };
require(path.join(__dirname, "..", "node_modules", "emulators", "dist", "emulators.js"));
const em = self.emulators;
em.pathPrefix = path.join(__dirname, "..", "node_modules", "emulators", "dist") + "/";
const ROOT = path.join(__dirname, "..");
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const PLACEHOLDER = "SYNTHETIC DEMO REMARK";

// The bundle autoexec loads the ISAM engine then runs the scrubber; INVSCRUB.BAS
// writes WORK\INVDONE.TXT ("OK <n>" / "ERR ...") and exits to DOS via SYSTEM.
const CONF = `[dosbox]
machine=svga_s3
memsize=32
[dos]
xms=true
ems=true
umb=true
[cpu]
core=auto
cputype=auto
cycles=max
[autoexec]
@echo off
mount c .
c:
copy Z:\\COMMAND.COM C:\\ > NUL
set COMSPEC=C:\\COMMAND.COM
cd WORK
C:\\VBASIC\\SYSTEM\\PROISAM.EXE
C:\\VBASIC\\SYSTEM\\VBDOS.EXE /RUN INVSCRUB.BAS
`;

function addDir(zip, absDir, arcPrefix) {
  for (const name of fs.readdirSync(absDir)) {
    if (name.endsWith(".original")) continue;
    const abs = path.join(absDir, name);
    const arc = arcPrefix + name;
    if (fs.statSync(abs).isDirectory()) addDir(zip, abs, arc + "/");
    else zip.file(arc, fs.readFileSync(abs));
  }
}

function findFile(node, suffix, acc) {
  const here = (node.name || "");
  const p = acc + here;
  if (!node.nodes) return p.toUpperCase().endsWith(suffix.toUpperCase()) ? p : null;
  for (const c of node.nodes) {
    const r = findFile(c, suffix, p + "/");
    if (r) return r;
  }
  return null;
}

(async () => {
  const realInv = process.argv[2];
  const outInv = process.argv[3] || realInv;
  if (!realInv || !fs.existsSync(realInv)) throw new Error(`real inventory not found: ${realInv}`);

  const zip = new JSZip();
  zip.file(".jsdos/dosbox.conf", CONF);
  zip.file("WORK/INVSCRUB.BAS", fs.readFileSync(path.join(ROOT, "FOG/OFFICE/INVSCRUB.BAS")));
  zip.file("WORK/INVENTOR.OFF", fs.readFileSync(path.join(ROOT, "FOG/INCLUDE/INVENTOR.OFF")));
  zip.file("WORK/INVENTOR.MAS", fs.readFileSync(realInv));
  addDir(zip, path.join(ROOT, "VBASIC"), "VBASIC/");   // VBDOS runtime + PROISAM (public submodule)
  const bundle = await zip.generateAsync({ type: "uint8array", compression: "DEFLATE" });
  console.log(`scrub bundle: ${(bundle.length / 1e6).toFixed(1)} MB`);

  const ci = await em.dosboxXNode(bundle, {});
  let done = null;
  for (let i = 0; i < 200 && !done; i++) {          // up to ~10 min
    await sleep(3000);
    try {
      const marker = findFile(await ci.fsTree(), "INVDONE.TXT", "");
      if (marker) {
        const b = await ci.fsReadFile(marker);
        if (b && b.length) done = Buffer.from(b).toString().trim();
      }
    } catch (e) { /* fs not ready */ }
  }
  if (!done) throw new Error("scrubber never wrote INVDONE.TXT");
  console.log("scrubber:", done);
  if (!done.startsWith("OK")) throw new Error("scrubber error: " + done);

  const invPath = findFile(await ci.fsTree(), "WORK/INVENTOR.MAS", "");
  const scrubbed = Buffer.from(await ci.fsReadFile(invPath));
  if (!scrubbed.includes(Buffer.from(PLACEHOLDER))) throw new Error("output has no placeholder — refusing");
  fs.writeFileSync(outInv, scrubbed);
  console.log(`wrote ${outInv}: ${scrubbed.length.toLocaleString()} bytes, ${done.split(/\s+/)[1]} records scrubbed`);
  try { await ci.exit(); } catch (e) { /* ignore */ }
  process.exit(0);
})().catch((e) => { console.error("isam_scrub FAILED:", e.message); process.exit(1); });
