module OpamJsonActions : sig
  val installs : Ezjsonm.value -> (string * string * [> `Fail | `Ok | `Skipped ]) list
end

