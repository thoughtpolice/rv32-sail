workflow "Build and Publish" {
  on = "push"
  resolves = ["Nix Build"]
}

action "Docker Lint" {
  uses = "docker://replicated/dockerfilelint"
  args = [".github/actions/nix-build/Dockerfile"]
}

action "Nix Build" {
  uses = "./.github/actions/nix-build"
  needs = ["Docker Lint"]
  args = "-Q release.nix -A docker"
}
