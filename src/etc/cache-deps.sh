# simple hack script to get all dependencies and print them out for moving
# binary caches around

nix-store -qR $(nix-build --no-link -Q release.nix)
