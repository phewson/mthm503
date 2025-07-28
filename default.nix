let
  pkgs = import (fetchTarball "https://github.com/rstats-on-nix/nixpkgs/archive/2024-04-29.tar.gz") {};
  # Define a path where Miniconda will be installed in the user's home directory

  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages)
      RPostgres;
  };


  system_packages = with pkgs; [
    glibcLocales nix igraph glpk libxml2 curl openssl icu fontconfig freetype
    harfbuzz libpng libjpeg zlib fribidi pkg-config postgresql pandoc texliveFull
    emacs R
    fontconfig.dev freetype.dev icu.dev curl bash coreutils
  ];

in pkgs.mkShell {
  LANG = "en_US.UTF-8";
  LC_ALL = "en_US.UTF-8";

  buildInputs = system_packages ++ rpkgs;

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.libxml2.out}/lib:${pkgs.postgresql.lib}/lib:${pkgs.glpk}/lib:${pkgs.icu.out}/lib:${pkgs.curl.out}/lib:${pkgs.openssl.out}/lib:${pkgs.fontconfig.out}/lib:${pkgs.freetype.out}/lib:${pkgs.harfbuzz.out}/lib:${pkgs.libpng.out}/lib:${pkgs.libjpeg.out}/lib:${pkgs.zlib.out}/lib:${pkgs.fribidi.out}/lib:$LD_LIBRARY_PATH"

    export PKG_CONFIG_PATH="${pkgs.icu.dev}/lib/pkgconfig:${pkgs.fontconfig.dev}/lib/pkgconfig:${pkgs.freetype.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"

    export INCLUDE_PATH="${pkgs.icu.dev}/include:${pkgs.fontconfig.dev}/include:${pkgs.freetype.dev}/include:$INCLUDE_PATH"
    export LIBRARY_PATH="${pkgs.icu.out}/lib:${pkgs.fontconfig.out}/lib:${pkgs.freetype.out}/lib:$LIBRARY_PATH"

    export DATASTORE="$HOME/DATA"
    export OFFLOC="$HOME/official_stats"

    echo "🔧 Checking pkg-config availability..."
    pkg-config --cflags fontconfig || echo "fontconfig not found"
    pkg-config --cflags freetype2 || echo "freetype2 not found"
    if [ ! -d "${minicondaPath}" ]; then
      echo "🔧 Installing Miniconda..."
      curl -L -o /tmp/miniconda.sh https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
      bash /tmp/miniconda.sh -b -p ${minicondaPath}
      rm /tmp/miniconda.sh
    fi

    export PATH="${minicondaPath}/bin:$PATH"
    echo "✅ Miniconda available. Run 'conda init' if needed."

  '';

}
