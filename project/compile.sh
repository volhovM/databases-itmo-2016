#! /bin/sh

psql -f recreate.sql
psql pkgmanager -f tables.sql
psql pkgmanager -f constraints.sql
