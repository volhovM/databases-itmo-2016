#! /bin/sh

psql -f recreate.sql
psql pkgmanager -f tables.sql
psql pkgmanager -f realdata_users.sql
psql pkgmanager -f realdata_ssh.sql
psql pkgmanager -f realdata_gpg.sql
psql pkgmanager -f realdata_packages.sql
psql pkgmanager -f realdata_maintainers.sql
psql pkgmanager -f realdata_versions.sql
psql pkgmanager -f realdata_deps.sql
psql pkgmanager -f realdata_snapshots.sql
psql pkgmanager -f realdata_snapshot_versions.sql
psql pkgmanager -f realdata_downloads.sql
psql pkgmanager -f constraints.sql
