{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ git zlib x11 gmp ] ++ (with xorg; [ libX11 libXcursor libXext libXfixes libXi libXinerama libXrandr libXxf86vm libGL libGLU ]);
}
