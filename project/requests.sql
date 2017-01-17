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
  INSERT INTO Package (PackageName,Site,License,Author,LastVersion) VALUES
      (pname,psite,plicense,pauthor,vId+1);
  SELECT currval('package_packageid_seq') INTO pId;
  INSERT INTO Maintainers VALUES (uploader, pId);
  PERFORM upload_version(pId,versionL,category,uploader,sign,deps);
  RETURN vId;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION register_user
    (username VARCHAR, email VARCHAR, login VARCHAR, passHash HASH256) RETURNS INTEGER AS $$
DECLARE
  userId INTEGER;
BEGIN
  RAISE INFO 'Registering user';
  INSERT INTO RegisteredUser (UserLogin, UserName, Email, PassHash) VALUES
      (login, username, email, passHash);
  SELECT currval('registereduser_userid_seq') INTO userId;
  RETURN userId;
END;
$$ LANGUAGE plpgsql;
