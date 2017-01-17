--- Sort all packages by downloaded
SELECT p.PackageId, p.PackageName, sum(DS) as downloadCount
FROM Package p INNER JOIN (
  SELECT VPackage, count(*) as DS
  FROM Version v INNER JOIN Downloads d on (v.VersionId = d.DVersion)
  GROUP BY VPackage) v1 on (p.PackageId = v1.VPackage)
GROUP BY p.PackageId ORDER BY downloadCount DESC;

--- Show users and their uploaded packages
SELECT Userlogin, PackageName
FROM RegisteredUser u INNER JOIN Version v ON (u.userId = v.uploader)
                      INNER JOIN Package p ON (p.PackageId = v.VPackage)
GROUP BY PackageName,UserLogin ORDER BY UserLogin ASC;

--- Compare users by amount of uploaded packages
SELECT Userlogin, count(PackageName) as UploadedN
FROM RegisteredUser u INNER JOIN Version v ON (u.userId = v.uploader)
                      INNER JOIN Package p ON (p.PackageId = v.VPackage)
GROUP BY UserLogin ORDER BY UploadedN DESC;

--- Same for maintaining
--- <user,maintained package>
SELECT Userlogin, PackageName
FROM RegisteredUser u INNER JOIN Maintainers m ON (u.UserId = m.MaintUser)
                      INNER JOIN Package p ON (p.PackageId = m.MaintPackage)
GROUP BY UserLogin,PackageName ORDER BY UserLogin ASC;

--- <user,number of maintained packagas>
SELECT Userlogin, count(PackageName) as MaintainedN
FROM RegisteredUser u INNER JOIN Maintainers m ON (u.UserId = m.MaintUser)
                      INNER JOIN Package p ON (p.PackageId = m.MaintPackage)
GROUP BY UserLogin ORDER BY MaintainedN DESC;

--- Sort package versions by number of dependencies
SELECT PackageName,VersionL,count(d.DepChild) as depsN
FROM Version v INNER JOIN Package p ON (p.PackageId = v.VPackage)
               INNER JOIN Dependencies d ON (d.DepParent = v.VersionId)
GROUP BY PackageName,VersionL ORDER BY depsN DESC;
