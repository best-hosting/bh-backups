#!/bin/sh

set -euf

nl='
'
OIFS="$IFS"

mkdir -p databases users

# Set timestamp far in the past, so if i don't reach the end of script, nagios
# will notice that.
echo 1 > "$(basename "$0" .sh).timestamp"

def_file=""
# mysql and mysqldump place options from "extra" default file before options
# from standard files. Thus, making overwriting options from "extra" default
# file not possible. Moreover, when '--defaults-file' is specified, "extra"
# won't be read at all.
f="$(dirname "$0")/$(basename "$0" .sh).cnf"
if [ -f "$f" ]; then
    def_file="${def_file:+$def_file$nl}--defaults-file=$f"
else
    f="$(dirname "$0")/$(basename "$0" .sh).extra.cnf"
    if [ -f "$f" ]; then
	def_file="${def_file:+$def_file$nl}--defaults-extra-file=$f"
    fi
fi
cmd_mysql="mysql$nl$def_file"
cmd_mysqldump="mysqldump$nl$def_file"

IFS="$nl"
# Backup each database to separate file
$cmd_mysql -BNe 'SHOW databases WHERE `database` != "information_schema" AND `database` != "performance_schema";' | while read database
do
	echo -n "Database: '$database' -> " 1>&2
	database="$(echo "$database" | sed -e's/[/?*]/_/g')"
	echo " '$database'" 1>&2
        $cmd_mysqldump --events --force --opt --databases "$database" | gzip -5 > databases/"$database".sql.gz
done

# Store user grants and passwords in separate files for convinience
$cmd_mysql -BNe 'SELECT DISTINCT user FROM mysql.user;' | while read user
do
        $cmd_mysql -BNe "SELECT DISTINCT CONCAT('SHOW GRANTS FOR ''',user,'''@''',host,''';') FROM mysql.user WHERE user = '${user}';" | $cmd_mysql -N | sed 's/\\\\/\\/g;s/$/;/' > users/"$user".grants.sql
done

# Now i may update timestamp to current time.
echo "$(date +%s)" > "$(basename "$0" .sh).timestamp"

