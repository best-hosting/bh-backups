#!/bin/sh

set -euf

if [ -z "${SERVER:-}" -o -z "${INSTALL_PATH:-}" ]; then
    echo "Either \$SERVER ('${SERVER:-}') or \$INSTALL_PATH ('${INSTALL_PATH:-}') is unset." 1>&2
    exit 1
fi

file="rsnapshot.${SERVER}.conf"
cp rsnapshot.SERVER.conf "$file"
sed -i -e "s/SERVER/$SERVER/g" "$file"
sed -i -e "s%INSTALL_PATH%$INSTALL_PATH%g" "$file"
cp "$file" "$INSTALL_PATH/$file"

dir="$INSTALL_PATH/rsnapshot.d"
cp -rT rsnapshot.d "$dir"
find "$dir" -type f -exec sed -i -e "s%INSTALL_PATH%$INSTALL_PATH%g" \{\} \;
