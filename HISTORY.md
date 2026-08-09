# Reference: Competitive Landscape, Production History, and Build Detail

Supporting detail behind the overview on the project site and in the README: the competitors these systems stood against, a feature-by-feature comparison, the documented record of their production use, and the specifics of how they were built and run. The narrative account of the software itself lives on the site.

---

## The Commercial Landscape

The market had three tiers with wide gaps between them, plus the developer-tool platforms that competed for the same business applications.

### Enterprise

**SAP R/3** (launched July 6, 1992) brought client-server ERP with real-time integration across finance (FICO: GL, AR, AP), human resources and payroll (HR), materials management (MM: inventory, procurement, purchase orders), production planning (PP: bills of material, MRP, product costing), and sales and distribution (SD: order entry, pricing, shipping). Software licensing ran over $100K, implementation $500K to $2M, and Fortune 500 deployments $50M and up, with 20-to-50-person implementation teams and 12-to-36-month timelines. Oracle Financials and Baan occupied the same tier. Its predecessor R/2 was mainframe-only.

### Mid-Market

Integrated accounting packages for companies of roughly 50 to 500 employees, priced at $2,000 to $5,000 plus internal implementation:

- **Peachtree Complete Accounting**: market leader at about 17% share in 1990. Modules covered general ledger, receivables, payables, bank reconciliation, inventory (with assembly tracking and LIFO/FIFO/average costing), payroll, sales and purchase orders, job costing, fixed assets, time tracking, and financial reporting. A standalone edition ran $2,000 to $2,500 and the complete edition $3,000 to $5,000, with tax updates by yearly subscription. A Windows version (CA-Accpac/2000) arrived October 1994. Acquired by Sage in 1998 and rebranded Sage 50.
- **MAS 90**: about 12% share, mid-market accounting with manufacturing support, priced in the same $2,000-to-$5,000 tier.
- **Accpac** (Computer Associates, acquired 1985 from Easy Business Systems): about 8% share, module-priced at $495 and up per module, covering GL, AR, AP, inventory, payroll, and job costing, with a multi-window interface added in 1987.

### Small Business

**QuickBooks** (Intuit, launched 1992) entered on DOS at $99 with simple invoicing, basic AR/AP, a general ledger, a chart of accounts, financial reports, and check writing. Its early versions had no inventory, no payroll, no job costing, no purchase orders, and no multi-user capability; accountants dismissed it as too simple. Payroll arrived around 2000 with QuickBooks Pro ($199). Through continuous feature expansion and strong marketing it reached over 85% of the small-business market by 2013.

### Developer Tools

**Clipper** (Nantucket, then Computer Associates from 1992) was the dominant xBase compiler from 1985 to 1992, generating standalone DOS executables that needed no runtime and building millions of accounting, inventory, banking, and insurance applications. Nantucket's sale of Clipper to Computer Associates for $190 million indicates an xBase market worth hundreds of millions. **dBASE** III (1984) and IV (1988) were the pioneering database platform of the 1980s, later ceding compiled applications to Clipper and relational work to SQL systems.

---

## Feature Comparison

**Core accounting**

| Feature | SAP R/3 | Peachtree | Accpac | QuickBooks 1992 | PFC | MPC | Payroll Suite |
|---|---|---|---|---|---|---|---|
| General Ledger | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | – |
| Accounts Receivable | ✓ | ✓ | ✓ | basic | ✓ | ✓ (6 modules) | – |
| Accounts Payable | ✓ | ✓ | ✓ | basic | ✓ | ✓ | – |
| Bank Reconciliation | – | ✓ | – | ✓ | – | – | – |
| Fixed Assets | – | ✓ | – | – | – | – | – |

**Operations and manufacturing**

| Feature | SAP R/3 | Peachtree | Accpac | QuickBooks 1992 | PFC | MPC | FOG |
|---|---|---|---|---|---|---|---|
| Inventory | ✓ | ✓ | ✓ | ✗ | ✓ | ✓ | ✓ (equipment) |
| Manufacturing / MRP | ✓ | – | – | ✗ | job costing | – | – |
| Job Costing | ✓ | ✓ | ✓ | ✗ | ✓ | – | – |
| Order Entry | ✓ | – | – | ✗ | ✓ | – | – |
| Sales Orders | ✓ | ✓ | – | ✗ | ✓ | – | – |

**Payroll**

| Feature | SAP R/3 | Peachtree | Accpac | QuickBooks 1992 | PFC | MPC | Payroll Suite |
|---|---|---|---|---|---|---|---|
| Payroll | ✓ | ✓ | ✓ | ✗ | ✓ | ✓ | ✓ |
| W-2 Generation | ✓ | ✓ | ✓ | ✗ | ✓ | ✓ | ✓ |
| Tax (FIT/FICA/Medicare) | ✓ | ✓ | ✓ | ✗ | ✓ | ✓ | ✓ |
| Check Printing | – | – | – | – | ✓ | ✓ | ✓ |

**Specialized**

| Feature | PFC | MPC | FOG | RDL |
|---|---|---|---|---|
| Recipe / Formula Management | ✓ | – | – | – |
| Equipment Tracking | – | – | ✓ | – |
| Rental Agreements | – | – | ✓ | – |
| Procedure Catalog (180+) | – | – | – | ✓ |
| Specialized AR | – | ✓ (6 modules) | – | ✓ |

---

## Production History

The systems were commercial software in continuous use, supported directly by their author, and the support record documents their deployment.

**MPC** was under active support at McKenry Produce (717 Willow Avenue, Knoxville) from at least April 2000, with on-site and remote work billed at $30/hour. By August 2001 its billing had moved from hourly support to monthly web-hosting subscriptions, one sign of the business modernizing around the aging DOS application.

**FOG** ran across multiple rent-to-own stores. E-Z Rentals generated on the order of 97 support invoices; A+ Rentals (A-Plus Rent to Own) and ACE of Tennessee were separate deployments at the same premises (P.O. Box 1499, Harriman). Support records document 9-to-30-store operations, with work including data-file repair across stores, month-end closing corrections, label generation and distribution, custom programming (correcting accounting totals, adjusting access levels), hardware replacement (power supplies, drives, video cards, network adapters), and dedicated Year 2000 date work in December 1999 ("rebuild date library for 2000 dates"). Stores were supported by modem via pcAnywhere.

Accounting modules were recompiled as late as 2005, and rental books posted journal entries into 2006. The support practice extended over a broader base of East Tennessee businesses across medical, legal, funeral, restaurant, and other services, several hundred invoices spanning 2000 to 2012, with the software systems above forming its technical core.

Billing evolved from hourly on-site and remote support at $30/hour (1999–2000), to a mix of hourly work and monthly subscriptions such as web hosting (2001–2002), to hardware sales and consulting alongside software support (2002 onward).

---

## Build and Runtime Detail

The programs were compiled to native DOS executables, not interpreted. Each module was compiled with Microsoft's `BC` compiler and joined with `LINK` into a single standalone `.EXE`. The rent-to-own system's own build script compiles six modules (`RTO`, `AGR`, `RCT`, `INV`, `INR`, `OPT`) and links them against the VB DOS runtime library (`VBDRT10E.LIB`) and a shared object library (`386.LIB`). A six-module program compiled and linked in seconds and ran as one executable.

The runtime environment used expanded memory. A representative `CONFIG.SYS` loaded `HIMEM.SYS` and `EMM386.EXE` with EMS enabled, placed DOS high with upper memory blocks (`DOS=HIGH,UMB`), and raised the file-handle limit (`FILES=50`). The larger multi-module programs depend on EMS to hold their code; without it they exhaust conventional memory at load.

The shared object library is PBClone, roughly 600 assembly routines covering screen handling, keyboard input, sorting, and file access. Screens are binary `.SLB` libraries produced in a dedicated editor (NOVA) and painted at runtime by writing directly to video memory.

Data is stored in ISAM (indexed sequential access) files: fixed-size binary records, random access by record number, and separate index files for lookups, with record locking for multi-user access.

---

## Related Documents

- [README.md](README.md): overview and technical details
- [AGENTS.md](AGENTS.md): how the production data was anonymized for public release
- [SETUP.md](SETUP.md): running the systems under DOSBox-X
