@0xc6a6f5682379876c;

interface Log {
  init @0 (label :Text) -> (id: Int64);
  send @1 (id :Int64, msg: Text) -> ();
  close @2 (id :Int64);
}

interface BuildLog {
  stdout @0 (msg: Text) -> ();
  stderr @1 (msg: Text) -> ();
  close  @2 (exitCode :Int32) -> ();
}

interface Build {
  shell @0 (cmd :Text, log :BuildLog) -> ();
}

interface Register {
  struct LogEntry {
    id @0 :Int64;
    label @1:Text;
  }
  ping      @0 (msg :Text) -> (reply :Text);
  worker    @1 (hostname :Text, arch :Text, ncpus: UInt32, exec :Build) -> (logger :Log);
  listLogs  @2 () -> (logs: List(LogEntry));
}
