self: super:
{
  stork = super.stork.overrideAttrs (old: {
    patches = (old.patches or []) ++ (if old.version == "1.4.0" then [
      ./patches/0001-Fix-regression-when-reading-from-stdin.patch
    ] else []);
  });
}
