Install.
========

rsync filters for rsnapshot setup.
----------------------------------

1. Install rsync filters using `install.sh` script.

        # ( export SERVER=srv; export INSTALL_PATH=/etc; ./install.sh )

    This script will:

    - copy rsync filters into `INSTALL_PATH/rsnapshot.d` directory and
      create corresponding rsnapshot config for server `SERVER`.
    - *overwrite* any changes made there. So, if you're updating existing
      installation and want to only update rsync filters, specify some
      dummy name in `SERVER` to not overwrite rsnapshot config.

2. Create symlinks to corresponding library filters for each `SERVER`'s backup
   point, e.g.:

        # ln -s save-root.rsync-filter srv-root.rsync-filter

    or define your own rsync filter, probably, including library filters.

mysql backup script setup.
--------------------------

1. Requirments: `socat` and `nc` for searching for unused local port, `mysql`
   and `mysqldump` for backup.
2. Create symlink pointing to installed `mysql.sh`, with some name `NAME`
   (usually, denoting backed up server name):

        # ln -sv /usr/local/bin/mysql.sh /etc/rsnapshot.d/mysql-SERVER.sh
        `/etc/rsnapshot.d/mysql-SERVER.sh' -> `/usr/local/bin/mysql.sh'

3. Create my.cnf style config with login, password, host and other options
   with *matched* `NAME`:
    - if i want to create config for using with '--defaults-file' mysql
      option, name it `NAME.cnf`, e.g.:

            /etc/rsnapshot.d/mysql-SERVER.cnf

    - if i want to create config for using with '--defaults-extra-file' mysql
      option, name it `NAME.extra.cnf`, e.g.:

            /etc/rsnapshot.d/mysql-SERVER.extra.cnf

    If config `NAME.cnf` was found, script won't look for `NAME.extra.cnf` .
    When `NAME.cnf` is used, this will be the only config, which `mysql` will
    read (i.e. options from `/root/.my.cnf` won't be used). But when
    `NAME.extra.cnf` is used, its options will be merged with content of
    `/root/.my.cnf` (the latter will overwrite the former).
4. Add an entry in '~/.ssh/config' for connecting to server with *matched*
   `NAME`:

        Host NAME
            HostName SERVER
            User root

5. Create user in mysql for backup server. It will connect from `localhost`
   (through ssh tunnel) and needs following privileges: `SELECT, LOCK TABLES,
   SHOW VIEW, EVENT, TRIGGER`. And, if i want to dump mysql users's passwords
   (`IDENTIFIED BY PASSWORD <secret>` in `GRANT`), `SUPER` privilege also
   necessary e.g.:

        GRANT SELECT, SUPER, LOCK TABLES, SHOW VIEW, EVENT, TRIGGER ON *.* TO 'backup'@'localhost';

6. Specify backup point with `backup_script` in `rsnapshot.conf`:

        backup_script   /root/mysql-SERVER.sh   mysql/
