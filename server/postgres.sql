CREATE TABLE payments (
  opennode_id text UNIQUE,
  cid text NOT NULL,
  amount int NOT NULL,
  time_bought interval NOT NULL,
  bought_at timestamp NOT NULL DEFAULT now(),
  actual_start timestamp NOT NULL,
  ended boolean NOT NULL DEFAULT false,
  note text
);

CREATE VIEW objects AS
  SELECT
    cid,
    array_agg(note) AS notes,
    min(actual_start) + sum(time_bought) AS ending_at,
    sum(amount) AS paid
  FROM payments
  WHERE NOT ended
  GROUP BY cid;

table payments;
table objects;
