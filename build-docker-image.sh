#!/bin/sh

BIN_ROOT=$(/home/tf/.local/bin/stack path --local-install-root)
BIN_NAME=md-to-blog
BIN_PATH="$BIN_ROOT/bin/$BIN_NAME"

TAG_NAME=md-to-blog

cp "$BIN_PATH" "docker/$BIN_NAME"
docker build -t "$TAG_NAME" ./docker
