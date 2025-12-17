# This file is part of CERE.
#
# Copyright (c) 2013-2016, Universite de Versailles St-Quentin-en-Yvelines
#
# CERE is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# CERE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with CERE.  If not, see <http://www.gnu.org/licenses/>.

"""
CERE Python package.

Upstream CERE generates `cere/vars.py` at build time (via autotools). When
running directly from the git tree (e.g. `PYTHONPATH=src` in a Docker-mounted
workspace), that generated file may be missing, which breaks `import cere`.

To keep local runs working, we provide a small runtime fallback for `cere.vars`
when the generated module is not present. The fallback derives tool paths from
the current environment (`llvm-config`, `CERE_LIBDIR`, etc.).
"""

import importlib
import os
import shutil
import subprocess
import sys
import sysconfig
import types
from pathlib import Path


def _is_truthy(value):
    if value is None:
        return False
    return value.strip().lower() in {"1", "true", "yes", "y", "on"}


def _detect_llvm_bindir():
    env_value = os.environ.get("LLVM_BINDIR") or os.environ.get("CERE_LLVM_BINDIR")
    if env_value:
        return env_value

    for llvm_config in ("llvm-config", "llvm-config-16", "llvm-config-15"):
        llvm_config_path = shutil.which(llvm_config)
        if not llvm_config_path:
            continue
        try:
            bindir = subprocess.check_output(
                [llvm_config_path, "--bindir"], universal_newlines=True
            ).strip()
        except Exception:
            continue
        if bindir and os.path.isdir(bindir):
            return bindir

    for candidate in (
        "/usr/lib/llvm-16/bin",
        "/usr/lib/llvm-15/bin",
        "/usr/lib/llvm-14/bin",
        "/usr/bin",
    ):
        if os.path.isdir(candidate):
            return candidate

    return "/usr/bin"


def _detect_executable(bindir, name):
    candidate = os.path.join(bindir, name) if bindir else ""
    if candidate and os.path.isfile(candidate) and os.access(candidate, os.X_OK):
        return candidate
    return shutil.which(name) or name


def _detect_pprof():
    env_value = os.environ.get("CERE_PPROF") or os.environ.get("PPROF")
    if env_value:
        return env_value
    for candidate in ("pprof", "google-pprof"):
        resolved = shutil.which(candidate)
        if resolved:
            return resolved
    return "pprof"


def _libdir_from_site_packages():
    package_file = Path(__file__).resolve()
    for parent in package_file.parents:
        if parent.name in ("site-packages", "dist-packages"):
            lib_dir = parent.parent.parent
            return str(lib_dir)
    return None


def _detect_cere_libdir():
    env_value = os.environ.get("CERE_LIBDIR")
    if env_value:
        return env_value

    candidates = []

    site_lib = _libdir_from_site_packages()
    if site_lib:
        candidates.append(site_lib)
        multiarch = sysconfig.get_config_var("MULTIARCH")
        if multiarch:
            candidates.append(os.path.join(site_lib, multiarch))

    multiarch = sysconfig.get_config_var("MULTIARCH")
    if multiarch:
        candidates.extend((f"/usr/local/lib/{multiarch}", f"/usr/lib/{multiarch}"))

    candidates.extend(("/usr/local/lib", "/usr/local/lib64", "/usr/lib", "/usr/lib64"))

    sentinels = (
        "libcere_regionreplay.so",
        "libcere_regiondump.so",
        "libcere_regioninstrumentation.so",
    )
    for libdir in candidates:
        try:
            if any(os.path.exists(os.path.join(libdir, s)) for s in sentinels):
                return libdir
        except Exception:
            continue

    return site_lib or "/usr/local/lib"


def _create_fallback_vars_module():
    module = types.ModuleType(__name__ + ".vars")

    module.VERSION = os.environ.get("CERE_VERSION", "0.3.1")

    module.LLVM_BINDIR = _detect_llvm_bindir()
    module.CLANG = _detect_executable(module.LLVM_BINDIR, "clang")
    module.CLANGPP = _detect_executable(module.LLVM_BINDIR, "clang++")
    module.FLANG = _detect_executable(module.LLVM_BINDIR, "flang")
    module.PPROF = _detect_pprof()
    module.GCC = os.environ.get("CC") or shutil.which("gcc") or "gcc"

    module.LIBDIR = _detect_cere_libdir()

    env_fortran = os.environ.get("CERE_FORTRAN_SUPPORT")
    module.FORTRAN_SUPPORT = (
        _is_truthy(env_fortran)
        if env_fortran is not None
        else bool(shutil.which(module.FLANG))
    )

    module.C_EXTENSIONS = [".c", ".C"]
    module.CXX_EXTENSIONS = [".cpp", ".cc", ".cxx", ".CPP", ".CC", ".CXX"]
    module.FORTRAN_EXTENSIONS = [".f", ".f90", ".f77", ".F", ".F90", ".F77"]
    module.SOURCE_EXTENSIONS = (
        module.C_EXTENSIONS + module.CXX_EXTENSIONS + module.FORTRAN_EXTENSIONS
    )

    module.ROOT = os.path.dirname(os.path.realpath(__file__))
    module.PROJECT_ROOT = os.path.dirname(os.path.realpath(__file__ + "/.."))
    module.PROFILE_LIB = "-lprofiler"
    module.OBJCOPY = os.environ.get("OBJCOPY", "objcopy")

    module.LOOP_REPLAY = module.LIBDIR + "/libcere_regionreplay.so"
    module.LOOP_INSTR = module.LIBDIR + "/libcere_regioninstrumentation.so"
    module.LOOP_EXT = module.LIBDIR + "/libcere_regionoutliner.so"
    module.LOOP_DUMP = module.LIBDIR + "/libcere_regiondump.so"
    module.GLOB_RENAME = module.LIBDIR + "/libcere_globalrename.so"

    module.LIBDIR_FLAGS = "-L {libdir} -Wl,--rpath={libdir}".format(
        libdir=module.LIBDIR
    )
    module.RDTSC_WRAPPER = "-lcere_rdtsc"
    module.LIKWID = "-llikwid"
    module.DUMPS_DIR = ".cere/dumps/"

    module.INVALID_REGION_FILE = "invalid_regions"

    module.CERE_DIRECTORIES = []

    module.CERE_MAIN_DIR = ".cere"
    module.CERE_REPLAYS = "replays"
    module.CERE_PROFILE = "profile"
    module.CERE_TRACES = "traces"
    module.CERE_DUMPS = "dumps"
    module.CERE_PLOTS = "plots"
    module.CERE_REPORT = "report"
    module.CERE_FLAGS = "flags"

    def create_path(top_dir, sub_dir):
        new_path = os.path.join(top_dir, sub_dir)
        module.CERE_DIRECTORIES.append(new_path)
        return new_path

    module.create_path = create_path

    module.CERE_REPLAY_PATH = create_path(module.CERE_MAIN_DIR, module.CERE_REPLAYS)
    module.CERE_PROFILE_PATH = create_path(module.CERE_MAIN_DIR, module.CERE_PROFILE)
    module.CERE_TRACES_PATH = create_path(module.CERE_MAIN_DIR, module.CERE_TRACES)
    module.CERE_DUMPS_PATH = create_path(module.CERE_MAIN_DIR, module.CERE_DUMPS)
    module.CERE_PLOTS_PATH = create_path(module.CERE_MAIN_DIR, module.CERE_PLOTS)
    module.CERE_REPORT_PATH = create_path(module.CERE_MAIN_DIR, module.CERE_REPORT)
    module.CERE_FLAGS_PATH = create_path(module.CERE_MAIN_DIR, module.CERE_FLAGS)

    module.CERE_WORKING_PATH = os.environ.get("CERE_WORKING_PATH", os.getcwd())

    for i in range(len(module.CERE_DIRECTORIES)):
        module.CERE_DIRECTORIES[i] = os.path.join(
            module.CERE_WORKING_PATH, module.CERE_DIRECTORIES[i]
        )

    module.CERE_REPLAY_PATH = module.CERE_DIRECTORIES[0]
    module.CERE_PROFILE_PATH = module.CERE_DIRECTORIES[1]
    module.CERE_TRACES_PATH = module.CERE_DIRECTORIES[2]
    module.CERE_DUMPS_PATH = module.CERE_DIRECTORIES[3]
    module.CERE_PLOTS_PATH = module.CERE_DIRECTORIES[4]
    module.CERE_REPORT_PATH = module.CERE_DIRECTORIES[5]
    module.CERE_FLAGS_PATH = module.CERE_DIRECTORIES[6]

    module.CERE_BUILD_PATH = os.environ.get("CERE_BUILD_PATH", module.CERE_WORKING_PATH)
    module.CERE_RUN_PATH = os.environ.get("CERE_RUN_PATH", module.CERE_WORKING_PATH)

    return module


try:
    vars = importlib.import_module(__name__ + ".vars")
except ModuleNotFoundError as exc:
    if exc.name != __name__ + ".vars":
        raise
    vars = _create_fallback_vars_module()
    sys.modules[vars.__name__] = vars
