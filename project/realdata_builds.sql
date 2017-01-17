INSERT INTO Build (BuildStatus, TimeStarted, TimeFinished, WorkDirectory, BuildVersion) VALUES
    ('SUCCESS', now() - interval '1 hour', now() - interval '10 minute', 'dir1', 123),
    ('SUCCESS', now() - interval '1 hour 5 m', now() - interval '5 minute', 'dir2', 242),
    ('FAILURE', now() - interval '1 hour 4 m', now() - interval '1 minute', 'dir3', 463),
    ('SUCCESS', now() - interval '1 hour 10m', now() - interval '3 minute', 'dir4', 264),
    ('SUCCESS', now() - interval '2 hour', now() - interval '2 minute', 'dir5', 15),
    ('IN PROGRESS', now() - interval '20 minute', null, 'dir6', 16),
    ('IN PROGRESS', now() - interval '15 minute', null, 'dir7', 17),
    ('IN PROGRESS', now() - interval '10 minute', null, 'dir8', 18),
    ('IN PROGRESS', now() - interval '5 minute', null, 'dir9', 19),
    ('PENDING', now() + interval '30 minute', null, 'dir10', 333),
    ('PENDING', now() + interval '35 minute', null, 'dir11', 334),
    ('PENDING', now() + interval '40 minute', null, 'dir12', 335);
