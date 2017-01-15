ALTER TABLE RegisteredUser ADD CONSTRAINT
  UN_RegisteredUser_UserLogin UNIQUE(UserLogin);
ALTER TABLE SshKey ADD CONSTRAINT FK_SshKey_UserId
  FOREIGN KEY (SshOwner) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE GpgKey ADD CONSTRAINT FK_GpgKey_UserId
  FOREIGN KEY (GpgOwner) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE Package ADD CONSTRAINT FK_Author_UserId
  FOREIGN KEY (Author) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE Package ADD CONSTRAINT FK_LastVersion_VersionId
  FOREIGN KEY (LastVersion) REFERENCES Version(VersionId) ON DELETE CASCADE;
ALTER TABLE Version ADD CONSTRAINT FK_VPackage_PackageId
  FOREIGN KEY (VPackage) REFERENCES Package(PackageId) ON DELETE CASCADE;
ALTER TABLE Version ADD CONSTRAINT FK_Uploader_UserId
  FOREIGN KEY (Uploader) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE Dependencies ADD CONSTRAINT FK_DepChild_VersionId
  FOREIGN KEY (DepChild) REFERENCES Version(VersionId);
ALTER TABLE Dependencies ADD CONSTRAINT FK_DepParent_VersionId
  FOREIGN KEY (DepParent) REFERENCES Version(VersionId);
ALTER TABLE SnapshotVersions ADD CONSTRAINT FK_SVSnapshot_SnapshotId
  FOREIGN KEY (SVSnapshot) REFERENCES Snapshot(SnapshotId);
ALTER TABLE SnapshotVersions ADD CONSTRAINT FK_SVVersion_VersionId
  FOREIGN KEY (SVVersion) REFERENCES Version(VersionId);
ALTER TABLE Build ADD CONSTRAINT FK_BuildVersion_VersionId
  FOREIGN KEY (BuildVersion) REFERENCES Version(VersionId);
ALTER TABLE Downloads ADD CONSTRAINT FK_DVersion_VersionId
  FOREIGN KEY (DVersion) REFERENCES Version(VersionId);
ALTER TABLE Downloads ADD CONSTRAINT FK_DBinaryBuild_BuildId
  FOREIGN KEY (DBinaryBuild) REFERENCES Build(BuildId);
