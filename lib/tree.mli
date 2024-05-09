
type builder

val init_builder : string -> builder
val next_node : builder -> builder * Node.t option
