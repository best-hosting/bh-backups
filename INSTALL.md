Install.
========

rsync filters for rsnapshot setup.
----------------------------------

1. Install common rsync filters using `install.sh` script

        # ( export INSTALL_PATH=/etc; ./install.sh )

    or install both common rsync filters and default rsnapshot config
    instantiated for server `srv`:

        # ( export SERVER=srv; export INSTALL_PATH=/etc; ./install.sh )

    Note, that this script will always *overwrite* any changes made to
    installed files. Thus, usually, i only need to install rsnapshot server
    config once (the first time) and later only update common rsync filters.

2. Create symlinks to corresponding library filters for each backup point of
   server `srv`, e.g.:

        # ln -s save-root.rsync-filter srv-root.rsync-filter

    or define your own rsync filter, probably, including library filters.

3. Add an entry to `~/.ssh/config` for connecting to server named `srv`:

        Host srv
            HostName some-server
            User root

4. Install mysql backup script `mysql.sh` (see below).

mysql backup script setup.
--------------------------

1. Requirments: `socat` and `nc` for searching for unused local port, `mysql`
   and `mysqldump` for backup. Install dependencies and copy `mysql.sh`
   somewhere:

        # cp mysql.sh /usr/local/bin/mysql.sh

2. Create symlink pointing to installed `mysql.sh` for server `srv`:

        # ln -sv /usr/local/bin/mysql.sh /etc/rsnapshot.d/srv-mysql.sh
        `/etc/rsnapshot.d/srv-mysql.sh' -> `/usr/local/bin/mysql.sh'

3. Create `my.cnf` style config with login, password, host and other options
   with *the same* name (except extension) as symlink to `mysql.sh`:
    - if i want to create config for using with `--defaults-file` option of
      `mysql` client, i should use `.cnf` extension:

            /etc/rsnapshot.d/srv-mysql.cnf

        In that case, `mysql` client won't read any other configs, i.e.
        `srv-mysql.cnf` will be the only config used.

    - if i want to create config for using with `--defaults-extra-file` option
      of `mysql` client, i should use `.extra.cnf` extension:

            /etc/rsnapshot.d/srv-mysql.extra.cnf

        In that case, content of `srv-mysql.extra.cnf` will be merged with
        `/root/.my.cnf` (the latter will overwrite the former).

    `mysql.sh` will first check `name.cnf` (where `name` is the name with
    which it was invoked), and won't look further for `name.extra.cnf`, if
    found.

4. Add an entry in `~/.ssh/config` for connecting to server `srv-mysql` (note,
   here is the name with which `mysql.sh` was invoked without `.sh`
   extension):

        Host srv-mysql
            HostName some-server
            User root

5. Create user in mysql for backup server. It will connect from `localhost`
   (through ssh tunnel) and need following privileges: `SELECT, LOCK TABLES,
   SHOW VIEW, EVENT, TRIGGER`. And, if i want to dump mysql users's passwords
   (`IDENTIFIED BY PASSWORD <secret>` in `GRANT`), `SUPER` privilege also
   necessary e.g.:

        GRANT SELECT, SUPER, LOCK TABLES, SHOW VIEW, EVENT, TRIGGER ON *.* TO 'backup'@'localhost';

6. Specify backup point with `backup_script` in `rsnapshot.conf` (it's already
   there, if i have installed config using `install.sh`):

        backup_script   /etc/rsnapshot.d/srv-mysql.sh   mysql/
