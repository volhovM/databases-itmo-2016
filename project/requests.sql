-- | Uploading version only, returns Version ID
CREATE FUNCTION upload_version ( pId INTEGER
                               , versionL VARCHAR
                               , category TEXT
                               , uploader INTEGER
                               , sign HASH256
                               , deps INTEGER[]
                               ) RETURNS INTEGER AS $$
DECLARE
  dep INTEGER;
  vId INTEGER;
BEGIN
  RAISE INFO 'Uploading version';
  INSERT INTO Version (VersionL,Category,VPackage,Uploader,UploadSign,UploadTime) VALUES
      (versionL,category,pId,uploader,sign,now());
  SELECT currval('version_versionid_seq') INTO vId;
  FOREACH dep IN ARRAY deps
  LOOP
    INSERT INTO Dependencies VALUES (vId, dep);
  END LOOP;
  RETURN vId;
END;
$$ LANGUAGE plpgsql;

-- | Uploading package
CREATE FUNCTION upload_package ( pname VARCHAR
                               , psite VARCHAR
                               , plicense VARCHAR
                               , pauthor TEXT
                               , versionL VARCHAR
                               , category TEXT
                               , uploader INTEGER
                               , sign HASH256
                               , deps INTEGER[]
                               ) RETURNS INTEGER AS $$
DECLARE
  pId INTEGER;
  vId INTEGER;
BEGIN
  RAISE INFO 'Uploading package';
  SELECT last_value INTO vId FROM version_versionid_seq;
  INSERT INTO Package (PackageName,Site,License,Author,MainMaintainer,LastVersion) VALUES
      (pname,psite,plicense,pauthor,uploader,vId+1);
  SELECT currval('package_packageid_seq') INTO pId;
  INSERT INTO Maintainers VALUES (uploader, pId);
  PERFORM upload_version(pId,versionL,category,uploader,sign,deps);
  RETURN vId;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION register_user
    (realname VARCHAR, email VARCHAR, login VARCHAR, passHash HASH256) RETURNS INTEGER AS $$
DECLARE
  userId INTEGER;
BEGIN
  RAISE INFO 'Registering user';
  INSERT INTO RegisteredUser (UserLogin, UserName, Email, PassHash) VALUES
      (login, realname, email, passHash);
  SELECT currval('registereduser_userid_seq') INTO userId;
  RETURN userId;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION list_maintainers (pname VARCHAR)
  RETURNS TABLE (userId INTEGER, userLogin VARCHAR) AS $$
BEGIN
    RETURN QUERY (
        SELECT u.UserId, u.UserLogin
        FROM (SELECT PackageId FROM Package WHERE PackageName = pname) p
             INNER JOIN Maintainers m ON (p.PackageId = m.MaintPackage)
             INNER JOIN RegisteredUser u ON (m.MaintUser = u.UserId)
    );
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION add_maintainer (login VARCHAR, pname VARCHAR) RETURNS VOID AS $$
DECLARE
  pId INTEGER;
  uId INTEGER;
BEGIN
  SELECT PackageId INTO pId FROM Package WHERE PackageName = pname;
  SELECT UserId INTO uId FROM RegisteredUser WHERE UserLogin = login;
  INSERT INTO Maintainers VALUES (uId, pId);
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION remove_maintainer (login VARCHAR, pname VARCHAR) RETURNS VOID AS $$
DECLARE
  pId INTEGER;
  pMainM INTEGER;
  uId INTEGER;
  newUId INTEGER;
  mn INTEGER;
BEGIN
  SELECT PackageId,MainMaintainer INTO pId,pMainM FROM Package WHERE PackageName = pname;
  SELECT count(*) INTO mn FROM Maintainers WHERE MaintPackage = pId;
  IF mn = 1 THEN
    RAISE EXCEPTION 'Cant delete the last maintainer of the package';
  END IF;

  SELECT UserId INTO uId FROM RegisteredUser WHERE UserLogin = login;
  DELETE FROM Maintainers WHERE MaintPackage = pId AND MaintUser = uId;
  SELECT MaintUser INTO newUId FROM Maintainers WHERE MaintPackage = pId LIMIT 1;

  -- need to replace main maintainer
  IF pMainM = uId THEN
    UPDATE Package SET MainMaintainer = newUId WHERE PackageId = pId;
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION packages_downloads()
  RETURNS TABLE (pId INTEGER, packName VARCHAR, downloadCount NUMERIC) AS $$
BEGIN
  RETURN QUERY (
    SELECT p.PackageId, p.PackageName, sum(DS) as downloadCount
    FROM Package p INNER JOIN (
      SELECT VPackage, count(*) as DS
      FROM Version v INNER JOIN Downloads d on (v.VersionId = d.DVersion)
      GROUP BY VPackage) v1 on (p.PackageId = v1.VPackage)
    GROUP BY p.PackageId ORDER BY downloadCount DESC
  );
END;
$$ LANGUAGE plpgsql;
