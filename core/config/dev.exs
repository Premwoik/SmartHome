import Config

config :logger, backends: [RingLogger]

# Set the number of messages to hold in the circular buffer
config :logger, RingLogger, max_size: 1024

# You can also configure RingLogger.Client options to be used
# with every client by default
config :ring_logger,
  application_levels: %{my_app: :error},
  color: [debug: :yellow],
  level: :debug
