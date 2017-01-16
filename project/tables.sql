CREATE DOMAIN HASH256 AS BYTEA CHECK (octet_length(VALUE) = 32);
CREATE DOMAIN HASH160 AS BYTEA CHECK (octet_length(VALUE) = 20);
CREATE DOMAIN SIG256 AS BYTEA CHECK (octet_length(VALUE) = 32);
CREATE TYPE BUILDSTATUS AS ENUM ('PENDING','IN PROGRESS', 'SUCCESS', 'FAIL');

CREATE TABLE RegisteredUser (
  UserId SERIAL NOT NULL PRIMARY KEY,
  UserLogin VARCHAR(30) NOT NULL,
  UserName VARCHAR(45) NOT NULL,
  Email VARCHAR(50) NOT NULL,
  PassHash HASH256 NOT NULL
);

CREATE TABLE SshKey (
  SshId SERIAL NOT NULL PRIMARY KEY,
  SshPubKey TEXT NOT NULL,
  SshOwner SERIAL NOT NULL
);

CREATE TABLE GpgKey (
  GpgOwner SERIAL NOT NULL PRIMARY KEY,
  GpgPublicKeyId HASH160 NOT NULL, -- Gpg PublicKey id ~ fingerprint
  GpgSignature SIG256 NOT NULL
);

CREATE TABLE Package (
  -- Main info
  PackageId SERIAL NOT NULL PRIMARY KEY,
  PName VARCHAR(20) NOT NULL,
  PSite VARCHAR(70) NOT NULL,
  PLicense VARCHAR(20) NOT NULL,
  Author SERIAL NOT NULL,
  LastVersion SERIAL NOT NULL,

  -- Options zone
  NotifyAuthor BOOL NOT NULL DEFAULT FALSE,
  NotifyMaintainers BOOL NOT NULL DEFAULT TRUE,
  NotifyUploaders BOOL NOT NULL DEFAULT TRUE,
  PDepricated BOOL NOT NULL DEFAULT FALSE,
  PPrivate BOOL NOT NULL DEFAULT FALSE
);

CREATE TABLE Version (
  -- General descriptions
  VersionId SERIAL NOT NULL PRIMARY KEY,
  VersionL VARCHAR(15) NOT NULL, -- Version literal
  Category VARCHAR(50) NOT NULL, -- Category list: a,b,c,d
  VSource TEXT NOT NULL, -- Source URL
  VPackage SERIAL NOT NULL,

  -- Upload-related
  Uploader SERIAL NOT NULL,
  UploadSign HASH256,
  UploadTime TIMESTAMP NOT NULL
);

CREATE TABLE Dependencies (
  DepChild SERIAL NOT NULL,
  DepParent SERIAL NOT NULL,

  PRIMARY KEY (DepChild, DepParent)
);

CREATE TABLE Snapshot (
  SnapshotId SERIAL NOT NULL PRIMARY KEY,
  SnapshotName VARCHAR(20) NOT NULL, -- mnemonic (lts-5.9, nigthly-2017.01.06)
  SnapshotSuccess BOOL NOT NULL DEFAULT FALSE
);

CREATE TABLE SnapshotVersions (
  SVSnapshot SERIAL NOT NULL,
  SVVersion SERIAL NOT NULL,

  PRIMARY KEY (SVSnapshot, SVVersion)
);

CREATE TABLE Build (
  BuildId SERIAL NOT NULL PRIMARY KEY,
  BuildStatus BUILDSTATUS NOT NULL,
  TimeStarted TIMESTAMP NOT NULL, -- means "planned time to start" if "planned"
  TimeFinished TIMESTAMP,
  WorkDirectory TEXT,
  BuildVersion SERIAL NOT NULL
);

CREATE TABLE Downloads (
  DownloadId SERIAL NOT NULL PRIMARY KEY,
  DTime TIMESTAMP NOT NULL,
  DIp VARCHAR(15),
  DBrowser VARCHAR(4),
  DVersion SERIAL NOT NULL,
  DBinaryBuild SERIAL
);
