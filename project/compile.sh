#! /bin/sh

psql -f recreate.sql
psql pkgmanager -f tables.sql
psql pkgmanager -f constraints.sql
psql pkgmanager -f realdata_users.sql
psql pkgmanager -f realdata_ssh.sql
psql pkgmanager -f realdata_gpg.sql
