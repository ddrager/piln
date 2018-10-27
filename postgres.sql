CREATE TABLE payments (
  order_id text PRIMARY KEY,
  cid text NOT NULL,
  note text,
  paid_at timestamp NOT NULL DEFAULT now(),
  amount int NOT NULL,
  processed boolean NOT NULL DEFAULT false
);

CREATE TABLE objects (
  cid text PRIMARY KEY,
  sizegb real NOT NULL,
  pinned_at timestamp,
  lifespan interval
);
