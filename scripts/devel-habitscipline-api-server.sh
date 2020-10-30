#!/usr/bin/env bash

stack install habitscipline-api-server \
  --file-watch \
  --exec='./scripts/restart-habitscipline-api-server.sh'
