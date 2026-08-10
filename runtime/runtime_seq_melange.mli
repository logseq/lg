val defer : (unit -> unit -> 'a) -> unit -> 'a

val transformer_sequence :
  ((unit -> unit) *
   (('complete_result -> 'complete_result) *
    (('step_accumulator -> 'output -> 'step_accumulator Runtime_reduced.t) * unit)) ->
   'unused_init *
   ((unit -> unit) *
    ((unit -> 'input -> unit Runtime_reduced.t) * 'unused_metadata))) ->
  'input Seq.t -> 'output Seq.t

val unfold : ('state -> ('value * 'state) option) -> 'state -> 'value Seq.t

val unfold_memoized :
  ('state -> ('value * 'state) option) -> 'state -> 'value Seq.t

val unfold_chunks :
  ('state -> ('value array * int * int * (unit -> 'state)) option) ->
  'state ->
  'value Seq.t
