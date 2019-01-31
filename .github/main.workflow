workflow "Build and Publish" {
  on = "push"
  resolves = ["Nix Docker Build"]
}

action "Shell Lint" {
  uses = "actions/bin/shellcheck@master"
  args = ".github/actions/nix-build/entrypoint.sh .github/actions/skopeo/entrypoint.sh .github/actions/cachix/entrypoint.sh"
}

action "Docker Lint" {
  uses = "docker://replicated/dockerfilelint"
  args = [".github/actions/nix-build/Dockerfile", ".github/actions/skopeo/Dockerfile", ".github/actions/cachix/Dockerfile"]
}

action "Nix Docker Build" {
  uses = "./.github/actions/nix-build"
  needs = ["Shell Lint", "Docker Lint"]
  args = "release.nix docker image.tar.gz"
}
