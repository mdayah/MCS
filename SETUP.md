# Development Environment Setup

## Windows Native DOSBox-X Integration

This project uses DOSBox-X running natively on Windows (accessed via WSL2 mount) rather than flatpak. This provides better performance and direct Windows integration.

### Prerequisites

1. **DOSBox-X installed on Windows** at `C:\DOSBox-X\`
   - Download from: https://dosbox-x.com/
   - Place executable at: `C:\DOSBox-X\dosbox-x.exe`

2. **WSL2 mounted Windows drive** (standard WSL2 setup)
   - Windows `C:\` drive available as `/mnt/c/` in WSL2

### Setup Instructions

#### 1. Add DOSBox-X to Your PATH

Add this line to your `~/.bashrc`:

```bash
# Add DOSBox-X to PATH (Windows native executable via WSL2)
export PATH="/mnt/c/DOSBox-X:$PATH"
```

**Or**, run this to append it:

```bash
cat >> ~/.bashrc << 'EOF'

# Add DOSBox-X to PATH (Windows native executable via WSL2)
export PATH="/mnt/c/DOSBox-X:$PATH"
EOF
```

Then reload:
```bash
source ~/.bashrc
```

#### 2. Verify Installation

```bash
which dosbox-x.exe
dosbox-x.exe --version
```

### Running Projects

Once setup is complete, you can launch any project:

```bash
cd /home/lucent/dad/PFC
./dosbox.sh
```

Or directly:
```bash
cd /home/lucent/dad/MPC
dosbox-x.exe -conf dosbox-x.conf
```

### What Happens When You Launch

1. DOSBox-X starts with the project's `dosbox-x.conf` configuration
2. You see helpful ECHO text showing available modules and how to edit/run them
3. You're in the project directory (mounted as `C:` in DOSBox)
4. Type `edit_mpc.bat` to edit in VBASIC, or `MPC.EXE` to run

**Example for MPC:**
```
C:\> MPC.EXE                  # Run main system
C:\> SYSTEM\edit_mpc.bat      # Edit in VBASIC
C:\> SYSTEM\ARP.EXE           # Run AR module
```

### Windows Integration

Since DOSBox-X is a native Windows executable:

- **File paths** work with Windows paths
- **Performance** is better than emulated/containerized approaches
- **Clipboard** integration works seamlessly
- **Network** access if needed (via Windows)
- **Direct access** to Windows file system

### Alternative: Direct Windows Launch (Optional)

You can also launch projects directly from Windows:

1. Open Windows File Explorer
2. Navigate to the project folder (e.g., `C:\Users\lucent\dad\PFC`)
3. Double-click `dosbox-x.conf`

Or from Windows command line:
```cmd
cd C:\Users\lucent\dad\MPC
dosbox-x.exe -conf dosbox-x.conf
```

### Script Reference

All projects have updated launcher scripts:
- `*/dosbox.sh` - Launches DOSBox-X with project config (uses native exe)
- Build scripts remain as shell scripts for compilation tasks

**No more flatpak dependency required.**

### Troubleshooting

**"dosbox-x.exe: command not found"**
- Verify `C:\DOSBox-X\dosbox-x.exe` exists
- Check that bashrc addition is loaded: `echo $PATH | grep DOSBox-X`
- Restart WSL terminal

**Path resolution issues**
- WSL2 mounts Windows drives as `/mnt/[letter]/`
- Verify with: `ls /mnt/c/DOSBox-X/`

**Config file not found**
- Make sure you're in the project directory before running `./dosbox.sh`
- Or explicitly: `dosbox-x.exe -conf /path/to/dosbox-x.conf`

---

## Project Structure Reference

```
/home/lucent/dad/
├── PFC/                   # Processed Foods ERP
│   ├── dosbox-x.conf     # Configuration with entry points
│   ├── dosbox.sh         # Launcher script
│   ├── */edit_*.bat      # VBASIC editor launchers
│   ├── VBASIC/           # IDE & tools
│   ├── LIB/              # Runtime libraries
│   └── ...modules...
│
├── MPC/                   # McKenry Produce
│   ├── dosbox-x.conf     # Configuration
│   ├── dosbox.sh         # Launcher
│   ├── SYSTEM/           # Compiled EXEs & edit batches
│   └── ...modules...
│
├── ... (7 more projects)
│
├── scripts/              # Management tools
│   ├── create_edit_batches.py
│   ├── enhance_dosbox_configs.py
│   └── BATCH_MANAGEMENT.md
│
├── .bashrc_dosbox_snippet        # Add to ~/.bashrc
├── .gitignore            # Protects *.MAS PII files
└── SETUP.md             # This file
```

## Notes

- All `.bat` files are Windows batch scripts (compatible via DOSBox)
- `.sh` files are used only for launching DOSBox or build operations
- The project runs exclusively on Windows (via native DOSBox-X exe)
- WSL2 provides a convenient bridge for command-line access
