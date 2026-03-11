#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_DIR="${ROOT_DIR}/testing"
TMP_DIR="${TEST_DIR}/tmp_profile_check"

BASELINE_BIN="${ROOT_DIR}/packmol-baseline"
PERF_BIN="${ROOT_DIR}/packmol-perf-native"
INPUT_FILE="${TEST_DIR}/input_files/benzene2.inp"
INPUT_CHKGRAD="${TMP_DIR}/benzene2_chkgrad.inp"

OBJ_TOL="${OBJ_TOL:-1e-8}"
GRAD_TOL="${GRAD_TOL:-1e-3}"

extract_function_value() {
    awk '/Function Value =/ {print $4}' "$1"
}

extract_max_grad_error() {
    awk '/Maximum difference =/ {print $7}' "$1"
}

run_profile() {
    local profile="$1"
    local binary="$2"
    local outdir="${TMP_DIR}/${profile}"

    rm -rf "${outdir}"
    mkdir -p "${outdir}"
    cp -r "${TEST_DIR}/structure_files" "${outdir}/"

    (
        cd "${ROOT_DIR}"
        make clean >/dev/null
        make "${profile}" >/dev/null
        cp packmol "${binary}"
    )

    (
        cd "${outdir}"
        "${binary}" < "${INPUT_CHKGRAD}" > profile.log
    )

    cp "${outdir}/chkgrad.log" "${TMP_DIR}/chkgrad-${profile}.log"
}

mkdir -p "${TMP_DIR}"
cp "${INPUT_FILE}" "${INPUT_CHKGRAD}"
echo "chkgrad" >> "${INPUT_CHKGRAD}"

echo "[1/2] Running baseline profile"
run_profile baseline "${BASELINE_BIN}"

echo "[2/2] Running perf-native profile"
run_profile perf-native "${PERF_BIN}"

base_obj="$(extract_function_value "${TMP_DIR}/chkgrad-baseline.log")"
perf_obj="$(extract_function_value "${TMP_DIR}/chkgrad-perf-native.log")"
base_grad_err="$(extract_max_grad_error "${TMP_DIR}/chkgrad-baseline.log")"
perf_grad_err="$(extract_max_grad_error "${TMP_DIR}/chkgrad-perf-native.log")"

python - "$base_obj" "$perf_obj" "$base_grad_err" "$perf_grad_err" "$OBJ_TOL" "$GRAD_TOL" <<'PY'
import math
import sys

base_obj = float(sys.argv[1])
perf_obj = float(sys.argv[2])
base_grad_err = float(sys.argv[3])
perf_grad_err = float(sys.argv[4])
obj_tol = float(sys.argv[5])
grad_tol = float(sys.argv[6])

obj_delta = abs(base_obj - perf_obj)
grad_delta = abs(base_grad_err - perf_grad_err)

print(f"Baseline objective: {base_obj:.16e}")
print(f"Perf-native objective: {perf_obj:.16e}")
print(f"|Δ objective| = {obj_delta:.6e} (tol={obj_tol:.6e})")
print(f"Baseline max grad check error: {base_grad_err:.6e}")
print(f"Perf-native max grad check error: {perf_grad_err:.6e}")
print(f"|Δ grad error| = {grad_delta:.6e} (tol={grad_tol:.6e})")

if obj_delta > obj_tol:
    raise SystemExit("Objective delta exceeds tolerance")
if grad_delta > grad_tol:
    raise SystemExit("Gradient delta exceeds tolerance")
PY

echo "Numerics comparison passed."
