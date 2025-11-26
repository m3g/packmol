"""Integration test for the packmol package."""

import subprocess
import tempfile
from pathlib import Path

import pytest

WATER_XYZ = """\
3
water molecule
O   0.000   0.000   0.000
H   0.957   0.000   0.000
H  -0.240   0.927   0.000
"""


@pytest.fixture
def tmp_workdir():
    """Create a temporary working directory with a water structure file."""
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)
        water_file = tmpdir / "water.xyz"
        water_file.write_text(WATER_XYZ)
        yield tmpdir


def test_pack_water_box(tmp_workdir):
    """Test that packmol can pack water molecules into a box."""
    output_file = tmp_workdir / "output.xyz"
    input_file = tmp_workdir / "input.inp"
    input_file.write_text(f"""\
tolerance 2.0
filetype xyz
output {output_file}

structure {tmp_workdir / "water.xyz"}
  number 10
  inside box 0. 0. 0. 20. 20. 20.
end structure
""")

    result = subprocess.run(
        ["packmol"],
        stdin=open(input_file),
        capture_output=True,
        cwd=tmp_workdir,
    )

    assert result.returncode == 0
    assert output_file.exists()
