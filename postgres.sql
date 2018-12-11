CREATE TYPE status AS ENUM ('trying', 'pinned', 'given_up', 'repurposed');

CREATE TABLE payments (
  order_id text PRIMARY KEY,
  cid text NOT NULL,
  note text,
  paid_at timestamp NOT NULL DEFAULT now(),
  amount int NOT NULL,
  status status NOT NULL DEFAULT 'trying',
  tries int NOT NULL DEFAULT 0,
  recycling text[] NOT NULL DEFAULT '{}'
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
    AND status = 'pinned';
$$ LANGUAGE SQL;

select * from objects;
select * from payments order by paid_at;
select * from payments where status != 'pinned';
