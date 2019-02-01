{ # Package set
  pkgs
}:

let
  gmp =(pkgs.gmp.override {
    stdenv = pkgs.emscriptenStdenv;
  }).overrideDerivation
    (old: rec {
      buildInputs = old.buildInputs ++ [ pkgs.pkgconfig ];
      # we need to reset this setting!
      NIX_CFLAGS_COMPILE="";
      configurePhase = ''
        # FIXME: Some tests require writing at $HOME
        HOME=$TMPDIR
        runHook preConfigure

        emconfigure ./configure --disable-static --enable-shared --host none --prefix=$out --disable-assembly

        runHook postConfigure
      '';
      dontStrip = true;
      outputs = [ "out" ];
      buildPhase = ''
        emmake make
      '';
      installPhase = ''
        emmake make install
        mkdir -p $out/lib/pkgconfig
        cat > "$out/lib/pkgconfig/gmp.pc" <<EOF
        prefix=$out
        exec_prefix=$out
        libdir=$out/lib
        includedir=$out/include

        Name: gmp
        Version: 6.1.3
        Description: GNU Multiple Precision Arithmetic Library
        URL: http://gmplib.org
        Libs: -L$out/lib -lgmp
        Cflags: -I$out/include
        EOF
      '';
      checkPhase = "true";
    });
in { inherit gmp; }
