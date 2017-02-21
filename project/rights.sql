-- First of all let's define which information is publicly available:
-- Also share all users to public.
CREATE VIEW UsersPublic
  AS SELECT UserId, UserLogin, UserName FROM RegisteredUser;
GRANT SELECT ON TABLE UsersPublic TO PUBLIC;

-- Also there are just public tables (everything except users, in
-- fact), we don't want to bother.
GRANT SELECT
  ON TABLE Package, Maintainers, Version, Dependencies, Snapshot, SnapshotVersions, Build, Downloads
  TO PUBLIC;

-- Applications that can query information about versions, create
-- snapshots and build them.
CREATE ROLE builders;
-- Applications that can query packages, add versions, upload,
-- etc. Also responsible for regsitering users.
CREATE ROLE frontends;

-- Frontends can access everything except for builds/snapshots.
GRANT SELECT, INSERT, UPDATE
  ON TABLE RegisteredUser
         , GpgKey
         , SshKey
         , Package
         , Maintainers
         , Version
         , Dependencies
         , Downloads
  TO GROUP frontends;
GRANT ALL
  ON SEQUENCE registereduser_userid_seq
            , sshkey_sshid_seq
            , package_packageid_seq
            , version_versionid_seq
            , downloads_downloadid_seq
  TO GROUP frontends;

-- Almost inverse for builders (they shouldn't know all users' info).
GRANT SELECT, INSERT, UPDATE
  ON TABLE Snapshot, SnapshotVersions, Build
  TO GROUP builders;
GRANT ALL
  ON SEQUENCE snapshot_snapshotid_seq, build_buildid_seq
  TO GROUP builders;

-- Restricted (but more permissive) view to users (w/o password hash
-- to prevent attacks). So build servers can send emails about failed
-- builds.
CREATE VIEW UsersBuilders
  AS SELECT UserId, UserLogin, UserName, Email FROM RegisteredUser;
GRANT SELECT
  ON TABLE UsersBuilders
  TO GROUP builders;

-- Appeared to be the only read-only procedure, because read-only re
-- usually trivial.
GRANT EXECUTE
  ON FUNCTION list_maintainers(VARCHAR)
  TO PUBLIC;

-- Frontenders are able to modify things.
GRANT EXECUTE
  ON FUNCTION upload_version (INTEGER, VARCHAR, TEXT, INTEGER, HASH256, INTEGER[])
            , upload_package (VARCHAR, VARCHAR, VARCHAR, TEXT, VARCHAR, TEXT, INTEGER, HASH256, INTEGER[])
            , register_user (VARCHAR, VARCHAR, VARCHAR, HASH256)
            , add_maintainer (VARCHAR, VARCHAR)
            , remove_maintainer (VARCHAR, VARCHAR)
  TO GROUP frontends;
