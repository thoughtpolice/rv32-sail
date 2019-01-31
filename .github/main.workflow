workflow "Build and Publish" {
  on = "push"
  resolves = ["Docker Load"]
}

action "Shell Lint" {
  uses = "actions/bin/shellcheck@master"
  args = ".github/actions/nix-build/entrypoint.sh"
}

action "Docker Lint" {
  uses = "docker://replicated/dockerfilelint"
  args = [".github/actions/nix-build/Dockerfile"]
}

action "Nix Docker Build" {
  uses = "./.github/actions/nix-build"
  needs = ["Shell Lint", "Docker Lint"]
  args = "release.nix docker image.tar.gz"
}

action "Docker Load" {
  uses = "actions/docker/cli@master"
  needs = ["Nix Docker Build"]
  args = "load -i image.tar.gz"
}
