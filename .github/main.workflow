workflow "Build and Publish on Push" {
  on = "push"
  resolves = ["Docker Lint"]
}

action "Docker Lint" {
  uses = "docker://replicated/dockerfilelint"
  args = [".github/actions/nix-build/Dockerfile"]
}
