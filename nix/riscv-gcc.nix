{ # Chosen toolchain subarchitecture, e.g. 'rv32imc'
  riscv-arch

  # Package set
, pkgs
}:

# --------------------------
# RISC-V GCC Toolchain Setup

let
  # risc-v toolchain source code. TODO FIXME: this should be replaced with
  # upstream versions of GCC. in the future we could also include LLVM (the
  # upstream nixpkgs LLVM expression should be built with it in time)
  riscv-toolchain-ver = "8.2.0";
  riscv-src = pkgs.fetchFromGitHub {
    owner  = "riscv";
    repo   = "riscv-gnu-toolchain";
    rev    = "c3ad5556197e374c25bc475ffc9285b831f869f8";
    sha256 = "1j9y3ai42xzzph9rm116sxfzhdlrjrk4z0v4yrk197j72isqyxbc";
    fetchSubmodules = true;
  };
  #
  # given an architecture like 'rv32i', this will generate the given
  # toolchain derivation based on the above source code.
  make-riscv-toolchain = arch:
    pkgs.stdenv.mkDerivation rec {
      name    = "riscv-${arch}-toolchain-${version}";
      version = "${riscv-toolchain-ver}-${builtins.substring 0 7 src.rev}";
      src     = riscv-src;

      configureFlags   = [ "--with-arch=${arch}" ];
      installPhase     = ":"; # 'make' installs on its own
      hardeningDisable = [ "all" ];
      enableParallelBuilding = true;

      # Stripping/fixups break the resulting libgcc.a archives, somehow.
      # Maybe something in stdenv that does this...
      dontStrip = true;
      dontFixup = true;

      nativeBuildInputs = with pkgs; [ curl gawk texinfo bison flex gperf ];
      buildInputs = with pkgs; [ libmpc mpfr gmp expat ];
    };

in make-riscv-toolchain riscv-arch
