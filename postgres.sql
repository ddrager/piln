CREATE TABLE payments (
  order_id text PRIMARY KEY,
  cid text NOT NULL,
  note text,
  paid_at timestamp NOT NULL DEFAULT now(),
  amount int NOT NULL,
  processed boolean NOT NULL DEFAULT false,
  tries int NOT NULL DEFAULT 0,
  given_up boolean NOT NULL DEFAULT false
);

CREATE TABLE objects (
  cid text PRIMARY KEY,
  sizegb real NOT NULL,
  pinned_at timestamp,
  lifespan interval
);

CREATE OR REPLACE FUNCTION notes(objects) RETURNS text[] AS $$
  SELECT coalesce(array_remove(array_agg(DISTINCT note), ''), '{}') FROM payments
  WHERE payments.cid = $1.cid
    AND note IS NOT NULL
    AND processed AND NOT given_up;
$$ LANGUAGE SQL;

select * from objects;
select * from payments order by paid_at;
select * from payments where given_up;
select * from payments where not processed;
