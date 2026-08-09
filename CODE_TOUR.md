# Code Tour: What Makes This Work Worth Preserving

This archive is most interesting when treated as evidence of how one programmer turned BASIC into a durable business-software platform. The impressive part is not that it resembles SAP or that it contains a large line count. It is the density of operational knowledge in ordinary source files: accounting invariants, rental policy, payroll law, printer control, indexing, recovery, and deployment all live close together and can still be followed line by line.

## 1. FOG's End-of-Day Close Is the Centerpiece

Start with [`mDayOpt`](FOG/SYSTEM/OPT.BAS#L112). It is a complete operational close, not a date rollover. Before closing, it validates every inventory code. It posts petty cash, computes depreciation asset by asset, advances month/year/lifetime counters for idle, rented, awaiting-service, in-service, awaiting-return, and loaner states, updates store activity totals, prints the cash journal, activity journal, delinquency sheet, and transaction log, then clears the day's transaction files.

The depreciation expression at [FOG/SYSTEM/OPT.BAS:140](FOG/SYSTEM/OPT.BAS#L140) is especially revealing. It implements a declining allocation across an asset's configured life, caps accumulated depreciation at purchase price, and immediately feeds the result into both inventory history and store activity totals. This is the kind of business rule that generic software rarely captures without customization.

Opinionated verdict: FOG is the work's best memorial. It is not merely a rental screen in front of accounting tables. It models the daily physics of a rent-to-own business.

## 2. The Rental Domain Is a State Machine

The 293-byte [`Inventory`](FOG/INCLUDE/INVENTOR.REC#L1) record tracks serial number, model, distributor, purchase and activity dates, agreement, transfer state, warranty rates, free-form remarks, and 24 separate month/year/lifetime counters. [`mDayOpt`](FOG/SYSTEM/OPT.BAS#L153) updates different counters according to inventory status; [`mPurOpt`](FOG/SYSTEM/OPT.BAS#L455) applies different retention rules to active, void, paid-out, charged-off, and terminated agreements; and the agreement module stamps terse activity codes such as `AN`, `AT`, `AV`, and `AP` throughout [FOG/SYSTEM/AGR.BAS](FOG/SYSTEM/AGR.BAS).

Those two-letter codes are compact event history. The source shows an implicit state machine distributed across agreement, receipt, inventory, report, and close routines. A future preservation project should extract that state machine into a diagram generated from assignments to `Agr.Act`, `Agr.Sta`, `Inv.Act`, and `Inv.Sta`; that would explain the system better than another feature checklist.

## 3. It Built Its Own Indexed Record Discipline

The applications do not appear to use Microsoft's Professional ISAM API. They use BASIC random files plus explicit companion indexes. FOG opens master, customer, agreement, receipt, inventory, serial-number, and activity files on fixed channel numbers in [`OpenFiles`](FOG/SYSTEM/RTO.BAS#L572). Lookups such as [`FindPas`](FOG/SYSTEM/RTO.BAS#L521), `FindPrc`, `FindSer`, and `FindZip` are handwritten binary searches over sorted fixed-width records. Rebuild operations invoke `SORTF` or `RPSORT`, then replace old index files.

MPC adds record-level locking around accounting posts. The sales path in [MPC/SYSTEM/ARP.BAS:978](MPC/SYSTEM/ARP.BAS#L978) locks and updates cash, receivables, inventory, cost of goods, sales revenue, other revenue, and sales-tax accounts one record at a time. The payroll path does the same for payroll expense, tax liabilities, insurance, deductions, and cash in [MPC/SYSTEM/PR.BAS:79](MPC/SYSTEM/PR.BAS#L79).

Opinionated verdict: call this an application-managed indexed record system, not a custom database engine. That wording is both more accurate and more impressive because it points at the actual engineering work.

## 4. Payroll Is a Product Family, Not One Shared Module

The BAJ, HBS, IMM, MCS, MPC, and FOG payroll files all expose the same recognizable spine: `ChkPost`, `ChkVoid`, `IncomeTax`, `SocSecTax`, `Medicare`, employee and check windows, monthly/quarterly/year reports, and W-2 printing. BAJ's implementation begins at [BAJ/AC/PR.BAS:136](BAJ/AC/PR.BAS#L136); equivalent routines occur at different offsets in every descendant.

The files are not identical and are not linked to one shared source file. They are copied variants that evolved separately. For example, a normalized Git diff between BAJ and HBS reports 128 added and 180 removed lines. That is historically useful: the variants preserve the programmer's practical product strategy, where a proven payroll core was cloned and adjusted to fit each installation.

The posting routines are also real double-entry integrations. A payroll check does not merely print a stub; it updates employee month/quarter/year totals and posts both employee and employer tax effects to named ledger slots. Voiding reverses those entries explicitly. The report code then emits checks, registers, timecards, quarterly summaries, year-end summaries, and W-2 forms with direct printer-control sequences.

## 5. MPC's Six Receivables Programs Reflect DOS Constraints and Workflow

MPC's receivables system occupies 4,482 lines across `ARP`, `ARS`, `ARD`, `ART`, `ARR`, and `ARI`. The split is not evidence of six abstract services. It is a pragmatic decomposition into executable-sized operational tools: primary posting, sales, detail, transaction/history work, reporting, and master maintenance.

The most valuable reading is [`ARP.BAS`](MPC/SYSTEM/ARP.BAS), where a single invoice or receipt crosses customer, invoice, receipt, material, index, and general-ledger files. The code makes the accounting consequences visible. It is repetitive by modern standards, but the repetition acts as an audit trail: each debit and credit is named in comments beside the locked record update.

## 6. RDL Shows Extreme Data Compression Without Obscurity

RDL stores an invoice in 273 bytes. Its 216-byte `Dst` field contains eight 27-byte lines, each composed of a 13-byte patient name, 3-byte procedure number, 2-byte quantity, 5-byte price, and 4-byte tax. The packing happens at [RDL/AR/AR.BAS:228](RDL/AR/AR.BAS#L228), unpacking at [RDL/AR/AR.BAS:316](RDL/AR/AR.BAS#L316), and invoice printing at [RDL/AR/AR.BAS:362](RDL/AR/AR.BAS#L362).

This is worth showing in the demo because it makes the constraints tangible: eight billable procedure rows, patient attribution, prices, and tax fit in less space than a small modern JSON object. The earlier preservation tooling missed those embedded patient fields and also mistook procedure descriptions for personal names; the schema now understands the packed layout and preserves the catalog.

## 7. The Build Is Native, Modular, and Reproducible

FOG's [`RTO.MAK`](FOG/SYSTEM/RTO.MAK) names six modules: `RTO`, `AGR`, `RCT`, `INV`, `INR`, and `OPT`. [`RUN.BAT`](FOG/SYSTEM/RUN.BAT) compiles each with `BC` and links them with the VBDOS runtime and `386.LIB` into one executable. The module boundaries correspond to menu/control, agreements, receipts, inventory, reports, and operations rather than arbitrary file splitting.

The screen libraries are a second compiled resource layer. Menus and forms are loaded by name from `.SLB` files through PBClone calls such as `MhOpenScreenLib` and `MhDisplayByName`. That gives the programs a consistent form system even though the business logic remains plain BASIC.

## 8. The Rough Edges Belong in the Story

The code is full of fixed file numbers, magic ledger record numbers, printer escape sequences, hard-coded screen coordinates, external sort commands, floppy-disk backup, PCPlus pager invocation, and direct deletion/rename recovery sequences. Those are not embarrassments to edit out of the memorial. They are the deployment environment made visible.

The accurate claim is strong enough: one programmer repeatedly converted the procedures of small and mid-sized businesses into software that combined data entry, accounting, reports, security, maintenance, backup, and hardware control. The archive matters because the full stack survives, not because it needs to be compared upward to the largest enterprise packages of its era.
