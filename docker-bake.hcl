variable "IMAGE_REGISTRY" {
  default = "docker.io"
}

variable "IMAGE_REPOSITORY" {
  default = "mattocci/afec-bayes"
}

variable "VERSION" {
  default = "latest"
}

target "base" {
  context    = "."
  dockerfile = "docker/base/Dockerfile"
  tags = [
    "${IMAGE_REGISTRY}/${IMAGE_REPOSITORY}:base",
    "${IMAGE_REGISTRY}/${IMAGE_REPOSITORY}:base-${VERSION}"
  ]
  platforms = ["linux/amd64"]
}

target "dev" {
  context    = "."
  dockerfile = "docker/dev/Dockerfile"
  depends_on = ["base"]
  tags = [
    "${IMAGE_REGISTRY}/${IMAGE_REPOSITORY}:dev",
    "${IMAGE_REGISTRY}/${IMAGE_REPOSITORY}:dev-${VERSION}"
  ]
  platforms = ["linux/amd64"]
}

target "prod" {
  context    = "."
  dockerfile = "docker/prod/Dockerfile"
  depends_on = ["base"]
  tags = [
    "${IMAGE_REGISTRY}/${IMAGE_REPOSITORY}:prod",
    "${IMAGE_REGISTRY}/${IMAGE_REPOSITORY}:prod-${VERSION}"
  ]
  platforms = ["linux/amd64"]
}

group "default" {
  targets = ["base", "dev", "prod"]
}

