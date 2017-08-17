@0xc6a6f5682379876c;

interface Log {
  init @0 (label :Text) -> (id: Int64);
  send @1 (id :Int64, msg: Text) -> ();
  close @2 (id :Int64);
}

interface Build {
  struct ProcessOutput {
     stdout @0 :Data;
     stderr @1 :Data;
     exitCode @2 :Int32;
  }
  shell     @0 (cmd :Text) -> (result :ProcessOutput);
}

interface Register {
  ping      @0 (msg :Text) -> (reply :Text);
  worker    @1 (hostname :Text, arch :Text, ncpus: UInt32, ) -> (logger :Log);
}
