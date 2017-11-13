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
    stdout @2: Text;
    stderr @3: Text;
  }
  struct NodeEntry {
    hostname @0 :Text;
    arch @1 :Text;
    ncpus @2 :UInt32;
  }
  ping      @0 (msg :Text) -> (reply :Text);
  worker    @1 (node: NodeEntry, exec :Build) -> (logger :Log);
  listWorkers @2 () -> (nodes: List(NodeEntry));
  listLogs  @3 () -> (logs: List(LogEntry));
  listLog   @4 (id :Int64) -> LogEntry;
}
