"""Command-line interface for packmol."""

import os
import subprocess
import sys
from pathlib import Path


def get_binary_path() -> Path:
    """Get the path to the packmol binary.

    Returns:
        Path to the compiled packmol executable.

    Raises:
        FileNotFoundError: If the binary is not found.
    """
    # Binary is installed in the package's binaries directory
    package_dir = Path(__file__).parent
    binaries_dir = package_dir / "binaries"

    # Check for platform-specific binary names
    binary_candidates = [
        binaries_dir / "packmol",      # Unix/Linux/macOS
        binaries_dir / "packmol.exe",  # Windows
    ]

    for binary_path in binary_candidates:
        if binary_path.exists():
            return binary_path

    raise FileNotFoundError(
        f"Packmol binary not found in {binaries_dir}. "
        "This may indicate a packaging issue."
    )


def main() -> int:
    """Main entry point for the packmol CLI.

    Executes the packmol binary with all provided arguments.

    Returns:
        Exit code from the packmol process.
    """
    try:
        binary_path = get_binary_path()
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    # Execute packmol with all command-line arguments
    # Use os.execv to replace the current process (Unix-like behavior)
    # On Windows, use subprocess for compatibility
    if os.name == "nt":  # Windows
        result = subprocess.run(
            [str(binary_path)] + sys.argv[1:],
            check=False,
        )
        return result.returncode
    else:  # Unix/Linux/macOS
        os.execv(str(binary_path), [str(binary_path)] + sys.argv[1:])
        # execv never returns if successful
        return 1


if __name__ == "__main__":
    sys.exit(main())
