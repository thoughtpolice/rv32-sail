workflow "Build and Publish" {
  on = "push"
  resolves = ["Docker Run Tests"]
}

action "Docker Lint" {
  uses = "docker://replicated/dockerfilelint"
  args = [".github/actions/nix-build/Dockerfile"]
}

action "nix-build" {
  uses = "./.github/actions/nix-build"
  needs = ["Docker Lint"]
  args = "-Q release.nix -A docker"
}

action "Docker Load" {
  uses = "actions/docker/cli@master"
  needs = ["nix-build"]
  args = "load -i ./result"
}

action "Docker Run Tests" {
  uses = "actions/docker/cli@master"
  needs = ["Docker Load"]
  args = "run rv32-sail cruise -e smoke.elf"
}
