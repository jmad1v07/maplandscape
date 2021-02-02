# waiting screen spinners

map_screen <- tagList(
  waiter::spin_flower(),
  h4("Drawing map...")
)

join_screen <- tagList(
  waiter::spin_flower(),
  h4("Joining tables...")
)

sync_screen <- tagList(
  waiter::spin_flower(),
  h4("Syncing forms...")
)

resize_screen <- tagList(
  waiter::spin_flower(),
  h4("Resizing...")
)