ALTER TABLE RegisteredUser ADD CONSTRAINT UN_RegisteredUser_UserLogin UNIQUE(UserLogin);
ALTER TABLE SshKey ADD CONSTRAINT FK_SshKey_UserId
  FOREIGN KEY (SshOwner) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE GpgKey ADD CONSTRAINT FK_GpgKey_UserId
  FOREIGN KEY (GpgOwner) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE GpgKey ADD CONSTRAINT UN_GpgKey_GpgPublicKeyId UNIQUE(GpgPublicKeyId);
ALTER TABLE Package ADD CONSTRAINT FK_LastVersion_VersionId
  FOREIGN KEY (LastVersion) REFERENCES Version(VersionId) ON DELETE CASCADE;
ALTER TABLE Package ADD CONSTRAINT UN_Package_PackageName UNIQUE(PackageName);
ALTER TABLE Maintainers ADD CONSTRAINT FK_MaintUser_UserId
  FOREIGN KEY (MaintUser) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE Maintainers ADD CONSTRAINT FK_MaintPackage_PackageId
  FOREIGN KEY (MaintPackage) REFERENCES Package(PackageId) ON DELETE CASCADE;
ALTER TABLE Version ADD CONSTRAINT FK_VPackage_PackageId
  FOREIGN KEY (VPackage) REFERENCES Package(PackageId) ON DELETE CASCADE;
ALTER TABLE Version ADD CONSTRAINT FK_Uploader_UserId
  FOREIGN KEY (Uploader) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE Dependencies ADD CONSTRAINT FK_DepChild_VersionId
  FOREIGN KEY (DepChild) REFERENCES Version(VersionId) ON DELETE CASCADE;
ALTER TABLE Dependencies ADD CONSTRAINT FK_DepParent_VersionId
  FOREIGN KEY (DepParent) REFERENCES Version(VersionId) ON DELETE CASCADE;
ALTER TABLE SnapshotVersions ADD CONSTRAINT FK_SVSnapshot_SnapshotId
  FOREIGN KEY (SVSnapshot) REFERENCES Snapshot(SnapshotId) ON DELETE CASCADE;
ALTER TABLE SnapshotVersions ADD CONSTRAINT FK_SVVersion_VersionId
  FOREIGN KEY (SVVersion) REFERENCES Version(VersionId) ON DELETE CASCADE;
ALTER TABLE Build ADD CONSTRAINT FK_BuildVersion_VersionId
  FOREIGN KEY (BuildVersion) REFERENCES Version(VersionId) ON DELETE CASCADE;
ALTER TABLE Build ADD CONSTRAINT BuildStatus_Matches_Nulls CHECK
  ((BuildStatus = 'PENDING' AND TimeFinished IS NULL) OR
   (BuildStatus = 'IN PROGRESS' AND TimeFinished IS NULL AND WorkDirectory IS NOT NULL) OR
   ((BuildStatus = 'SUCCESS' OR BuildStatus = 'FAIL') AND TimeFinished IS NOT NULL AND WorkDirectory IS NOT NULL));
ALTER TABLE Downloads ADD CONSTRAINT FK_DVersion_VersionId
  FOREIGN KEY (DVersion) REFERENCES Version(VersionId) ON DELETE CASCADE;
ALTER TABLE Downloads ADD CONSTRAINT FK_DBinaryBuild_BuildId
  FOREIGN KEY (DBinaryBuild) REFERENCES Build(BuildId) ON DELETE CASCADE;

---- For searching packages by name/regex.
CREATE INDEX ON Package USING btree (PName);
---- For quickly finding the package related to VersionId
CREATE INDEX ON Version USING btree (VersionId, VPackage);
---- For getting all versions of the package
CREATE INDEX ON Version USING hash (VPackage);
---- Doesn't optimize anything if there's not much users
-- CREATE INDEX ON RegisteredUser USING hash (Name);
---- For stats
CREATE INDEX ON Downloads USING hash (DVersion);
CREATE INDEX ON Downloads USING hash (DBinaryBuild);
