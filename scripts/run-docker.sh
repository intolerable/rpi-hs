#!/usr/bin/env sh
podman build --tag libgpiod-testing . && \
podman run -it -v $(pwd):/workdir -v /dev/:/dev/ -v /sys/kernel/config:/sys/kernel/config --privileged libgpiod-testing bash
