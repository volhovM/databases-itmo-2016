INSERT INTO Snapshot (SnapshotId, SnapshotName) VALUES
    (0,'nightly-0'),
    (1,'nightly-1'),
    (2,'nightly-2'),
    (3,'nightly-3'),
    (4,'nightly-4'),
    (5,'nightly-5'),
    (6,'nightly-6');
ALTER SEQUENCE snapshot_snapshotid_seq RESTART WITH 8;