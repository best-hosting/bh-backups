#!/bin/sh

set -euf

if [ -n "${SERVER:-}" -a -n "${INSTALL_PATH:-}" ]; then
    echo "Installing config for server '$SERVER' with filter directory '$INSTALL_PATH'" 1>&2
    file="rsnapshot.${SERVER}.conf"
    cp rsnapshot.SERVER.conf "$file"
    sed -i -e "s/SERVER/$SERVER/g" "$file"
    sed -i -e "s%INSTALL_PATH%$INSTALL_PATH%g" "$file"
    cp "$file" "$INSTALL_PATH/$file"
fi

if [ -n "${INSTALL_PATH:-}" ]; then
    echo "Installing filters to directory '$INSTALL_PATH'" 1>&2
    dir="$INSTALL_PATH/rsnapshot.d"
    cp -rT rsnapshot.d "$dir"
    find "$dir" -type f -exec sed -i -e "s%INSTALL_PATH%$INSTALL_PATH%g" \{\} \;
fi
