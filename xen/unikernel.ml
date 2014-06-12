open Lwt
open V1_LWT

module Main (C : CONSOLE) = struct
  let start c =
    Server_xen.main (module C : CONSOLE) c
end

