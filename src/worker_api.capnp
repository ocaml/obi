@0xc6a6f5682379876c;

interface Callback {
  log @0 (msg :Text) -> ();
}

interface Register {
  ping      @0 (msg :Text) -> (reply :Text);
  heartbeat @1 (msg :Text, callback :Callback) -> ();
  logger    @2 () -> (callback :Callback);
}
