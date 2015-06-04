#!/usr/bin/env sh
sed -i 's#"directory": ".*"#"directory": "'$(realpath `dirname $0`)'"#' compile_commands.json
