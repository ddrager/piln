CREATE TABLE payments (
  order_id text PRIMARY KEY,
  cid text NOT NULL,
  amount int NOT NULL,
  time_bought interval NOT NULL,
  paid_at timestamp NOT NULL DEFAULT now(),
  actual_start timestamp NOT NULL,
  ended boolean NOT NULL DEFAULT false,
  note text NOT NULL
);

CREATE VIEW objects AS
  SELECT
    cid,
    array_remove(array_agg(note), '') AS notes,
    min(actual_start) + sum(time_bought) AS ending_at,
    sum(amount) AS paid
  FROM payments
  WHERE NOT ended
  GROUP BY cid;

table payments;
table objects;
