CREATE DOMAIN HASH256 AS BYTEA CHECK (octet_length(VALUE) = 32);
CREATE DOMAIN HASH160 AS BYTEA CHECK (octet_length(VALUE) = 20);
CREATE DOMAIN SIG256 AS BYTEA CHECK (octet_length(VALUE) = 32);

CREATE TABLE RegisteredUser (
  UserId SERIAL NOT NULL PRIMARY KEY,
  UserLogin VARCHAR(20) NOT NULL,
  UserName VARCHAR(40) NOT NULL,
  Email VARCHAR(20) NOT NULL,
  PassHash HASH256 NOT NULL
);

CREATE TABLE SshKey (
  SshId SERIAL NOT NULL PRIMARY KEY,
  SshPubKey TEXT NOT NULL,
  SshOwner SERIAL NOT NULL
);

CREATE TABLE GpgKey (
  GpgOwner SERIAL NOT NULL PRIMARY KEY,
  GpgSignature SIG256 NOT NULL,
  GpgPublicKeyId HASH256 NOT NULL -- Gpg PublicKey id ~ fingerprint
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
  VersionId SERIAL NOT NULL PRIMARY KEY,
  -- Version literal
  VersionL VARCHAR(15) NOT NULL,
  -- Category list: a,b,c,d
  Category VARCHAR(50) NOT NULL,
  -- Source URL
  VSource TEXT NOT NULL,
  VPackage SERIAL NOT NULL
);

-- CREATE TABLE SshKey (
--     SshPubKey text NOT NULL PRIMARY KEY,
--     SeatNum int NOT NULL
-- );
--
-- CREATE TABLE Flights(
--   FlightId int NOT NULL PRIMARY KEY,
--   FlightTime timestamp NOT NULL,
--   PlaneId int NOT NULL,
--   SoldOut boolean NOT NULL,
--
--   FOREIGN KEY (PlaneId) REFERENCES Seats(PlaneId) ON DELETE CASCADE
-- );
--
-- CREATE TABLE Tickets(
--     UserId int NOT NULL,
--     FlightId int NOT NULL,
--     SeatId int NOT NULL,
--     Expiration timestamp,
--     IsPaid boolean NOT NULL DEFAULT False,
--
--     PRIMARY KEY (UserId, FlightId, SeatId),
--     FOREIGN KEY (FlightId) REFERENCES Flights(FlightId) ON DELETE CASCADE,
--     UNIQUE(FlightId, SeatId) -- место не может быть продано/забронировано дважды
-- );
