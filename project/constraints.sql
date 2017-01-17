------ CONSTRAINTS ------

ALTER TABLE RegisteredUser ADD CONSTRAINT UN_RegisteredUser_UserLogin UNIQUE(UserLogin);
ALTER TABLE RegisteredUser ADD CONSTRAINT UN_RegisteredUser_Email UNIQUE(Email);
ALTER TABLE SshKey ADD CONSTRAINT FK_SshKey_UserId
  FOREIGN KEY (SshOwner) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE GpgKey ADD CONSTRAINT FK_GpgKey_UserId
  FOREIGN KEY (GpgOwner) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE GpgKey ADD CONSTRAINT UN_GpgKey_GpgPublicKeyId UNIQUE(GpgPublicKeyId);
ALTER TABLE Package ADD CONSTRAINT FK_LastVersion_VersionId
  FOREIGN KEY (LastVersion) REFERENCES Version(VersionId) DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE Package ADD CONSTRAINT FK_MainMaintainer_MaintUser
  FOREIGN KEY (PackageId,MainMaintainer)
  REFERENCES Maintainers(MaintPackage,MaintUser) DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE Package ADD CONSTRAINT UN_Package_PackageName UNIQUE(PackageName);
ALTER TABLE Maintainers ADD CONSTRAINT FK_MaintUser_UserId
  FOREIGN KEY (MaintUser) REFERENCES RegisteredUser(UserId) ON DELETE CASCADE;
ALTER TABLE Maintainers ADD CONSTRAINT FK_MaintPackage_PackageId
  FOREIGN KEY (MaintPackage) REFERENCES Package(PackageId) DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE Version ADD CONSTRAINT FK_VPackage_PackageId
  FOREIGN KEY (VPackage) REFERENCES Package(PackageId) DEFERRABLE INITIALLY DEFERRED;
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
   ((BuildStatus = 'SUCCESS' OR BuildStatus = 'FAILURE') AND TimeFinished IS NOT NULL AND WorkDirectory IS NOT NULL));
ALTER TABLE Build ADD CONSTRAINT Build_Correct_Timestamps CHECK
  (TimeFinished IS NULL OR (TimeFinished >= TimeStarted));
ALTER TABLE Build ADD CONSTRAINT UN_Build_WorkDirectory UNIQUE(WorkDirectory);
ALTER TABLE Downloads ADD CONSTRAINT FK_DVersion_VersionId
  FOREIGN KEY (DVersion) REFERENCES Version(VersionId) ON DELETE CASCADE;
ALTER TABLE Downloads ADD CONSTRAINT FK_DBinaryBuild_BuildId
  FOREIGN KEY (DBinaryBuild) REFERENCES Build(BuildId) ON DELETE CASCADE;


------ INDICES ------

---- For searching packages by name/regex.
CREATE INDEX ON Package USING btree (PackageName);
---- For quickly finding the package related to VersionId
-- CREATE INDEX ON Version USING btree (VersionId, VPackage);
---- For getting all versions of the package
CREATE INDEX ON Version USING hash (VPackage);
---- Search user by login/email for quick log in
CREATE INDEX ON RegisteredUser USING hash (UserLogin);
CREATE INDEX ON RegisteredUser USING hash (Email);
---- For stats -- getting downloads by version/binary build/time range
CREATE INDEX ON Downloads USING hash (DVersion); -- 0.4 ms w/ vs 33.3 ms w/o
CREATE INDEX ON Downloads USING hash (DBinaryBuild);
CREATE INDEX ON Downloads USING btree (DTime);


------ PROCEDURES AND TRIGGERS ------

CREATE FUNCTION check_build_state() RETURNS TRIGGER AS $$
DECLARE
  bstatus BUILDSTATUS;
  bversion INTEGER;
BEGIN
  SELECT BuildStatus,BuildVersion INTO bstatus,bversion
      FROM Build WHERE BuildId = NEW.DBinaryBuild;

  IF (NEW.DBinaryBuild IS NOT NULL) AND (bstatus != 'SUCCESS') THEN
     RAISE EXCEPTION 'Downloads row % references build % that is not SUCCESS',
         NEW.DownloadId, NEW.DBinaryBuild;
  END IF;

  IF (NEW.DBinaryBuild IS NOT NULL) AND (bversion != NEW.DVersion) THEN
     RAISE EXCEPTION 'Download version % doesnt match related binary build version %',
         NEW.DVersion, bversion;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_build_state AFTER INSERT OR UPDATE ON Downloads
  FOR EACH ROW EXECUTE PROCEDURE check_build_state();

CREATE FUNCTION check_uploader_is_maintainer() RETURNS TRIGGER AS $$
BEGIN
  IF NOT EXISTS (SELECT FROM Maintainers
                 WHERE MaintPackage = NEW.VPackage AND
                       MaintUser = NEW.Uploader) THEN
      RAISE EXCEPTION 'Uploader of package is not in maintainer group';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_uploader_is_maintainer AFTER INSERT OR UPDATE ON Version
  FOR EACH ROW EXECUTE PROCEDURE check_uploader_is_maintainer();

CREATE FUNCTION auto_update_package_version() RETURNS TRIGGER AS $$
BEGIN
  UPDATE Package SET LastVersion = NEW.VersionId WHERE PackageId = NEW.VPackage;
  RAISE NOTICE 'Last version of package was automatically bumped.';
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER auto_update_package_version AFTER INSERT ON Version
  FOR EACH ROW EXECUTE PROCEDURE auto_update_package_version();

CREATE FUNCTION set_snapshot_to_succeeded() RETURNS TRIGGER AS $$
DECLARE
  sid INTEGER;
BEGIN
  FOR sid IN
     SELECT DISTINCT SVSnapshot
     FROM SnapshotVersions
     WHERE SVVersion = NEW.BuildVersion
  LOOP
      IF sid NOT IN (
           SELECT SVSnapshot FROM SnapshotVersions
           WHERE SVVersion NOT IN (
             SELECT BuildVersion FROM Build WHERE BuildStatus = 'SUCCESS')) THEN
        RAISE NOTICE 'Snapshot % is now successfully built', sid;
        UPDATE Snapshot SET PassesBuild = True Where SnapshotId = sid;
      END IF;
  END LOOP;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_snapshot_to_succeeded AFTER INSERT OR UPDATE ON Build
  FOR EACH ROW EXECUTE PROCEDURE set_snapshot_to_succeeded();

CREATE FUNCTION snapshot_contains_no_version_duplicates() RETURNS TRIGGER AS $$
DECLARE
  pId INTEGER;
BEGIN
  SELECT VPackage INTO pId FROM Version WHERE VersionId = NEW.SVVersion;
  IF (pId,NEW.SVSnapshot) IN (
       SELECT VPackage, SVSnapshot
       FROM SnapshotVersions s
       INNER JOIN Version v
       ON (s.SVVersion = v.VersionId)) THEN
      RAISE EXCEPTION 'This package (%) is already in snapshot %',
        pId, NEW.SVSnapshot;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER snapshot_contains_no_version_duplicates BEFORE INSERT OR UPDATE ON SnapshotVersions
  FOR EACH ROW EXECUTE PROCEDURE snapshot_contains_no_version_duplicates();
