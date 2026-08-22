val write : kind:string -> path:string -> 'a -> unit
val read : kind:string -> path:string -> ('a, string) result
val remove_if_present : string -> unit
