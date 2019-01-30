## Issue description



### Steps to reproduce



## Technical details

Please run:

```bash
echo $(git rev-parse HEAD) && nix run nixpkgs.nix-info -c nix-info -m
```

from the root of the project directory, and paste the results here.
