@0xc6a6f5682379876c;

interface Callback {
  log @0 (msg :Text) -> ();
}

interface Register {
  ping      @0 (msg :Text) -> (reply :Text);
  heartbeat @1 (msg :Text, callback :Callback) -> ();
  logger    @2 () -> (callback :Callback);
}

interface Log {
  init @0 (label :Text) -> (id: Int64);
  send @1 (id :Int64, msg: Text) -> ();
  close @2 (id :Int64);
}