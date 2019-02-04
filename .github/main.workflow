workflow "Build and Publish" {
  on = "push"
  resolves = ["Push to Docker Hub"]
}

action "Shell Lint" {
  uses = "actions/bin/shellcheck@master"
  args = ".github/actions/nix-build/entrypoint.sh .github/actions/skopeo/entrypoint.sh .github/actions/cachix/entrypoint.sh .github/actions/b2-sync/entrypoint.sh"
}

action "Docker Lint" {
  uses = "docker://replicated/dockerfilelint"
  args = [".github/actions/nix-build/Dockerfile", ".github/actions/skopeo/Dockerfile", ".github/actions/cachix/Dockerfile", ".github/actions/b2-sync/Dockerfile" ]
}

action "Nix Docker Build" {
  uses = "./.github/actions/nix-build"
  needs = ["Shell Lint", "Docker Lint"]
  args = "release.nix"
}

action "Publish Filter" {
  uses = "actions/bin/filter@master"
  needs = ["Nix Docker Build"]
  args = "branch master"
}

action "Docker Login" {
  uses = "actions/docker/login@master"
  needs = ["Publish Filter"]
  secrets = ["DOCKER_USERNAME", "DOCKER_PASSWORD"]
}

action "Push to Docker Hub" {
  uses = "./.github/actions/skopeo"
  needs = ["Docker Login"]
  args = "docker.tar.gz docker://thoughtpolice/rv32-sail --no-latest"
}
