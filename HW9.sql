DROP SCHEMA PUBLIC CASCADE;
CREATE SCHEMA PUBLIC;

CREATE TABLE Seats(
    PlaneId int NOT NULL PRIMARY KEY,
    SeatNum int NOT NULL
);

CREATE TABLE Flights(
  FlightId int NOT NULL PRIMARY KEY,
  FlightTime timestamp NOT NULL,
  PlaneId int NOT NULL,
  SoldOut boolean NOT NULL,

  FOREIGN KEY (PlaneId) REFERENCES Seats(PlaneId) ON DELETE CASCADE
);

CREATE TABLE Tickets(
    UserId int NOT NULL,
    FlightId int NOT NULL,
    SeatId int NOT NULL,
    Expiration timestamp,
    IsPaid boolean NOT NULL DEFAULT False,

    PRIMARY KEY (UserId, FlightId, SeatId),
    FOREIGN KEY (FlightId) REFERENCES Flights(FlightId) ON DELETE CASCADE,
    UNIQUE(FlightId, SeatId) -- место не может быть продано/забронировано дважды
);

DELETE FROM Seats WHERE True;
INSERT INTO Seats VALUES
  (111, 10),
  (222, 15),
  (333, 20);

DELETE FROM Flights WHERE True;
INSERT INTO Flights VALUES
  (0, '2016-12-06 12:00:00', 111, False),
  (1, '2016-12-07 22:00:00', 222, False),
  (2, '2016-12-08 06:30:00', 333, False),
  (3, '2016-12-07 06:30:00', 222, True);

DELETE FROM Tickets WHERE True;
INSERT INTO Tickets VALUES
  (0,0,2,null,True),
  (1,0,3,'2016-12-06 04:00:00',False),
  (2,2,5,'2016-12-06 12:00:00',False);

-- Flights that we can manipulate tickets for.
CREATE VIEW ValidFlights AS
SELECT FlightId, PlaneId FROM Flights
WHERE NOT SoldOut AND
      FlightTime - now() > INTERVAL '2 hour';

-- (planeId, seatId) for every accessible seat in the plane
-- let's suppose there can't be more than 100 seats in plane
-- (we can use 1000 instead)
CREATE VIEW AllSeats AS
SELECT S.PlaneId, n AS SeatId
FROM generate_series(1,100) AS a(n), Seats AS S
WHERE n <= S.SeatNum
ORDER BY S.PlaneId;

-- Valid reservations + sold out seats.
CREATE VIEW NonFreeSeats AS
SELECT FlightId, SeatId
FROM ValidFlights NATURAL JOIN Flights NATURAL JOIN Tickets
WHERE IsPaid OR                         -- reedemed tickets
      (Expiration IS NOT NULL AND       -- valid reservations
       now() < Expiration AND
       Expiration < FlightTime AND
       FlightTime - now() > INTERVAL '1 day');

-- Seats that we can buy (only).
CREATE VIEW PurchaseAvailable AS
SELECT FlightId, SeatId
FROM ValidFlights NATURAL JOIN AllSeats
WHERE (FlightId,SeatId) NOT IN (SELECT * from NonFreeSeats);

-- Seats that can be bought _and_ reserved. PurchaseAvailable \ those
-- that can't be booked.
CREATE VIEW ReservationAvailable AS
SELECT FlightId, SeatId
FROM ValidFlights NATURAL JOIN Flights NATURAL JOIN AllSeats
WHERE FlightTime - now() > INTERVAL '1 day' AND
      (FlightId,SeatId) NOT IN (SELECT * FROM NonFreeSeats);
--        SELECT FlightId, SeatId
--        FROM ValidFlights NATURAL JOIN Flights NATURAL JOIN Tickets
--        WHERE IsPaid OR                               -- reserved
--              (Expiration IS NOT NULL AND             -- not-expired-ok-bookings
--               Expiration < FlightTime AND
--               now() < Expiration) AND                -- that are expired
--              FlightTime - now() > INTERVAL '1 day'); -- and we still can reserve

---------------------------
-- Here's the fun begins --
---------------------------

-- ambiguous
CREATE FUNCTION FreeSeatsAll(fId INT)
  RETURNS TABLE (chairId INT) AS
$func$
BEGIN
    RETURN QUERY SELECT SeatId FROM PurchaseAvailable WHERE FlightId = fId;
END
$func$ LANGUAGE plpgsql;


CREATE FUNCTION Reserve(uId INT, fId INT, sId INT)
  RETURNS boolean AS
$func$
BEGIN
   INSERT INTO Tickets (UserId, FlightId, SeatId, Expiration, IsPaid)
     SELECT uId,fId,sId,now() + INTERVAL '1 day',FALSE
     ON CONFLICT (FlightId,SeatId) DO
       UPDATE SET Expiration = now() + INTERVAL '1 day',
                  UserId = uId
     WHERE (fId,sId) IN (SELECT * FROM ReservationAvailable);

   IF FOUND THEN RETURN TRUE;
   ELSE RETURN FALSE;
   END IF;
END
$func$ LANGUAGE plpgsql;

CREATE FUNCTION ExtendReservation(fId INT, sId INT)
  RETURNS boolean AS
$func$
BEGIN
   UPDATE Tickets
   SET Expiration = now() + INTERVAL '1 day'
   WHERE
       FlightId = fId AND
       SeatId = sId AND
       IsPaid = FALSE AND
       (fId, sId) in (SELECT * FROM NonFreeSeats);

   IF FOUND THEN RETURN TRUE;
   ELSE RETURN FALSE;
   END IF;
END
$func$ LANGUAGE plpgsql;

CREATE FUNCTION BuyFree(uId INT, fId INT, sId INT)
  RETURNS boolean AS
$func$
BEGIN
   INSERT INTO Tickets (UserId, FlightId, SeatId, IsPaid)
     SELECT uId,fId,sId,TRUE
     ON CONFLICT (FlightId,SeatId) DO
       UPDATE SET Expiration = null
                , UserId = uId
                , IsPaid = TRUE
     WHERE (fId,sId) IN (SELECT * FROM FreeSeats);

   IF FOUND THEN RETURN TRUE;
   ELSE RETURN FALSE;
   END IF;
END
$func$ LANGUAGE plpgsql;

CREATE FUNCTION BuyReserved(uId INT, fId INT, sId INT)
  RETURNS boolean AS
$func$
BEGIN
   UPDATE Tickets
   SET Expiration = NULL, IsPaid = TRUE, UserId = uId
   WHERE
       UserId = uId AND
       FlightId = fId AND
       SeatId = sId AND
       IsPaid = FALSE AND
       (fId, sId) in (SELECT * FROM NonFreeSeats);

   IF FOUND THEN RETURN TRUE;
   ELSE RETURN FALSE;
   END IF;
END
$func$ LANGUAGE plpgsql;
