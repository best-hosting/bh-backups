#!/bin/sh

set -euf

nl='
'
OIFS="$IFS"

# Find unused local port.
# Args:
# 1 - port number to start from.
# 2 - maximum port number.
find_unused_port()
{
    local lport="$1"        # Local port to start search from.
    local lport_max="$2"    # Maximum socat port number.
    local port_inc=1        # Increment for port number.

    # FIXME: sanity check numbers..
    # Find unused local port.
    while [ $lport -le $lport_max ]; do
        echo "Trying local port $lport.." 1>&2
        # It seems, that without '-u' option i can't reuse port immediately. I.e.
        # unused port, which i'd found here, becomes "already in use", when i'll
        # try to bind to it later.
        socat -u TCP-LISTEN:$lport,bind=127.0.0.1 OPEN:/dev/null 2>/dev/null &
        sleep 1
        nc -z 127.0.0.1 $lport || true
        if wait "$!"; then
            break
        fi
        lport=$(($lport + $port_inc))
    done
    if [ $lport -gt $lport_max ]; then
        echo "Error: Can't find unused local port." 1>&2
        return 1
    fi
    echo "$lport"
}

readonly name="$(basename "$0" .sh)"
# Set timestamp far in the past, so if i don't reach the end of script, nagios
# will notice that.
readonly timestamp="$name.timestamp"
echo 1 > "$timestamp"

echo "Using SSH hostname '$name'." 1>&2
readonly ssh_sock="$HOME/.ssh/$name.socket"
if ssh -S "$ssh_sock" -O check none 2>/dev/null; then
    echo "SSH tunnel has already opened with socket '$ssh_sock'." 1>&2
    exit 1
fi

readonly lport=$(find_unused_port 6446 6476)
readonly laddr=127.0.0.1
readonly raddr=127.0.0.1
echo "Forward port $lport" 1>&2
ssh -M -S "$ssh_sock" -fnNT -L $laddr:$lport:$raddr:5432 $name
if ! ssh -T -S "$ssh_sock" -O check none; then
    echo "Failed to open SSH tunnel with socket '$ssh_sock'." 1>&2
    exit 1
fi

trap "ssh -T -S \"$ssh_sock\" -O exit none" INT QUIT EXIT

args_psql="-h${nl}${laddr}${nl}-p$nl$lport"

readonly args_psql passfile
readonly cmd_psql="psql$nl$args_psql"
readonly cmd_psqldump="pg_dump$nl$args_psql"
readonly cmd_psqldumpall="pg_dumpall$nl$args_psql"

mkdir -p databases

IFS="$nl"
# Backup each database to separate file
ds="$($cmd_psql -t -A -c 'SELECT datname FROM pg_database WHERE datistemplate = false;' postgres)"
for database in $ds; do
	echo -n "Database: '$database' -> " 1>&2
	database="$(echo "$database" | sed -e's/[/?*]/_/g')"
	echo " '$database'" 1>&2
        p="databases/${database}.sql"
        $cmd_psqldump --create --clean "$database" > "$p"
        gzip --fast < "$p" > "$p".gz
        rm "$p"
done
$cmd_psqldumpall -g | gzip -5 > databases/allroles.sql.gz

# Now i may update timestamp to current time.
echo "$(date +%s)" > "$timestamp"

