"""Integration test for the packmol package."""

import subprocess

WATER_XYZ = """\
3
water molecule
O   0.000   0.000   0.000
H   0.957   0.000   0.000
H  -0.240   0.927   0.000
"""


def test_pack_water_box(tmp_path):
    """Test that packmol can pack water molecules into a box."""
    (tmp_path / "water.xyz").write_text(WATER_XYZ)

    (tmp_path / "input.inp").write_text("""\
tolerance 2.0
filetype xyz
output output.xyz

structure water.xyz
  number 10
  inside box 0. 0. 0. 20. 20. 20.
end structure
""")

    result = subprocess.run(
        ["packmol"],
        stdin=open(tmp_path / "input.inp"),
        capture_output=True,
        cwd=tmp_path,
    )

    assert result.returncode == 0
    assert (tmp_path / "output.xyz").exists()
