"""Custom Hatchling build hook to compile packmol binary."""

import os
import shutil
import subprocess
from pathlib import Path
from typing import Any

from hatchling.builders.hooks.plugin.interface import BuildHookInterface


class CustomBuildHook(BuildHookInterface):
    """Build hook to compile packmol from Fortran source."""

    def initialize(self, version: str, build_data: dict[str, Any]) -> None:
        """Compile packmol and copy binary to package.

        Args:
            version: The version of the package being built.
            build_data: Dictionary of build configuration data.
        """
        # Find gfortran compiler
        gfortran = self._find_gfortran()

        # Verify source files exist
        src_dir = Path("src")
        if not src_dir.exists():
            raise RuntimeError(
                f"Source directory not found: {src_dir}. "
                "Ensure packmol Fortran source files are in the src/ directory."
            )

        # Create binaries output directory
        binaries_dir = Path("python/packmol/binaries")
        binaries_dir.mkdir(parents=True, exist_ok=True)

        # Clean previous builds
        print("Cleaning previous build artifacts...")
        subprocess.run(["make", "clean"], check=False, cwd=".")

        # Compile packmol
        print(f"Compiling packmol with {gfortran}...")
        result = subprocess.run(
            ["make", f"FORTRAN={gfortran}"],
            check=False,
            capture_output=True,
            text=True,
        )

        if result.returncode != 0:
            raise RuntimeError(
                f"Failed to compile packmol:\n{result.stderr}\n{result.stdout}"
            )

        # Find compiled binary
        binary_name = "packmol.exe" if os.name == "nt" else "packmol"
        binary_path = Path(binary_name)

        if not binary_path.exists():
            raise RuntimeError(
                f"Compiled binary not found: {binary_path}. "
                "Compilation may have failed."
            )

        # Copy binary to package
        target_path = binaries_dir / binary_name
        print(f"Copying {binary_path} to {target_path}...")
        shutil.copy2(binary_path, target_path)

        # Ensure binary is executable
        target_path.chmod(0o755)

        print(f"Successfully built packmol binary: {target_path}")

        # Mark wheel as platform-specific
        build_data["pure_python"] = False
        build_data["infer_tag"] = True

    def _find_gfortran(self) -> str:
        """Find gfortran compiler.

        Returns:
            Path to gfortran executable.

        Raises:
            RuntimeError: If gfortran is not found.
        """
        # Try plain gfortran first
        if shutil.which("gfortran"):
            return "gfortran"

        # Try versioned gfortran (e.g., gfortran-13, gfortran-14)
        for version in range(20, 10, -1):
            gfortran_versioned = f"gfortran-{version}"
            if shutil.which(gfortran_versioned):
                return gfortran_versioned

        # Check common Homebrew paths on macOS
        homebrew_paths = [
            "/opt/homebrew/bin/gfortran",
            "/usr/local/bin/gfortran",
        ]
        for path in homebrew_paths:
            if Path(path).exists():
                return path

        # Not found
        raise RuntimeError(
            "gfortran compiler not found. Please install gfortran:\n"
            "  macOS: brew install gcc\n"
            "  Ubuntu/Debian: sudo apt-get install gfortran\n"
            "  RHEL/CentOS: sudo yum install gcc-gfortran"
        )
